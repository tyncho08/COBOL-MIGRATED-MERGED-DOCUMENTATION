"""Sales Ledger Payment Processing API endpoints"""

from fastapi import APIRouter, Depends, HTTPException, status, Query, Body
from sqlalchemy.orm import Session
from typing import List, Optional, Dict, Any
from datetime import date, datetime
from decimal import Decimal

from app.api import deps
from app.services.sl import cash_receipt as payment_service
from app.schemas.sl import (
    Payment, PaymentCreate, PaymentListResponse,
    PaymentAllocation, BatchPaymentAllocation,
    PaymentMethod
)

router = APIRouter()


@router.get("", response_model=PaymentListResponse)
async def list_payments(
    customer_code: Optional[str] = Query(None, description="Filter by customer"),
    start_date: Optional[date] = Query(None, description="Start date"),
    end_date: Optional[date] = Query(None, description="End date"),
    payment_method: Optional[PaymentMethod] = Query(None, description="Payment method"),
    unallocated_only: bool = Query(False, description="Only show payments with unallocated amounts"),
    skip: int = 0,
    limit: int = 100,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    List customer payments with optional filters.
    
    Returns payments with allocation details.
    """
    payments = payment_service.get_payments(
        db,
        customer_code=customer_code,
        start_date=start_date,
        end_date=end_date,
        payment_method=payment_method,
        unallocated_only=unallocated_only,
        skip=skip,
        limit=limit
    )
    
    total = payment_service.count_payments(
        db,
        customer_code=customer_code,
        start_date=start_date,
        end_date=end_date,
        payment_method=payment_method,
        unallocated_only=unallocated_only
    )
    
    return PaymentListResponse(
        payments=payments,
        total=total,
        skip=skip,
        limit=limit
    )


@router.get("/{payment_id}", response_model=Payment)
async def get_payment(
    payment_id: int,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Get specific payment by ID."""
    payment = payment_service.get_payment(db, payment_id=payment_id)
    if not payment:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Payment {payment_id} not found"
        )
    return payment


@router.post("", response_model=Payment, status_code=status.HTTP_201_CREATED)
async def create_payment(
    payment_in: PaymentCreate,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Create new customer payment.
    
    Can auto-allocate to oldest invoices or manually allocate.
    """
    # Validate customer exists
    customer = payment_service.validate_customer(db, customer_code=payment_in.customer_code)
    if not customer:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Customer {payment_in.customer_code} not found"
        )
    
    # Validate allocations if provided
    if payment_in.allocations:
        total_allocated = sum(a.amount_applied + a.discount_taken for a in payment_in.allocations)
        if total_allocated > payment_in.amount:
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail=f"Total allocation ({total_allocated}) exceeds payment amount ({payment_in.amount})"
            )
    
    payment = payment_service.create_payment(
        db,
        payment_in=payment_in,
        created_by=current_user.id
    )
    
    return payment


@router.post("/{payment_id}/allocate")
async def allocate_payment(
    payment_id: int,
    allocations: List[PaymentAllocation],
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Allocate payment to invoices.
    
    Can be used to reallocate an existing payment.
    """
    payment = payment_service.get_payment(db, payment_id=payment_id)
    if not payment:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Payment {payment_id} not found"
        )
    
    # Calculate total to allocate
    total_to_allocate = sum(a.amount_applied + a.discount_taken for a in allocations)
    
    if total_to_allocate > payment.unallocated_amount:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Total allocation ({total_to_allocate}) exceeds unallocated amount ({payment.unallocated_amount})"
        )
    
    result = payment_service.allocate_payment(
        db,
        payment_id=payment_id,
        allocations=allocations,
        user_id=current_user.id
    )
    
    return {
        "payment_id": payment_id,
        "allocations_created": len(result["allocations"]),
        "total_allocated": result["total_allocated"],
        "remaining_unallocated": result["remaining_unallocated"]
    }


@router.post("/{payment_id}/deallocate")
async def deallocate_payment(
    payment_id: int,
    allocation_ids: Optional[List[int]] = None,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Remove payment allocations.
    
    If no allocation_ids provided, removes all allocations.
    """
    payment = payment_service.get_payment(db, payment_id=payment_id)
    if not payment:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Payment {payment_id} not found"
        )
    
    if payment.gl_posted:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Cannot deallocate posted payment"
        )
    
    result = payment_service.deallocate_payment(
        db,
        payment_id=payment_id,
        allocation_ids=allocation_ids
    )
    
    return {
        "payment_id": payment_id,
        "allocations_removed": result["allocations_removed"],
        "amount_deallocated": result["amount_deallocated"]
    }


@router.post("/{payment_id}/reverse")
async def reverse_payment(
    payment_id: int,
    reversal_date: date,
    reason: str = Body(..., min_length=10),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Reverse a posted payment.
    
    Creates reversal entries and deallocates invoices.
    """
    payment = payment_service.get_payment(db, payment_id=payment_id)
    if not payment:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Payment {payment_id} not found"
        )
    
    reversal = payment_service.reverse_payment(
        db,
        payment_id=payment_id,
        reversal_date=reversal_date,
        reason=reason,
        reversed_by=current_user.id
    )
    
    return {
        "original_payment_id": payment_id,
        "reversal_payment_id": reversal.id,
        "reversal_receipt_number": reversal.receipt_number,
        "gl_entries_created": len(reversal.gl_entries)
    }


@router.get("/unallocated-summary")
async def get_unallocated_summary(
    customer_code: Optional[str] = None,
    min_amount: Decimal = Query(0, description="Minimum unallocated amount"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Get summary of unallocated payments."""
    summary = payment_service.get_unallocated_summary(
        db,
        customer_code=customer_code,
        min_amount=min_amount
    )
    
    return {
        "total_unallocated": summary["total_unallocated"],
        "payment_count": summary["payment_count"],
        "by_customer": summary["by_customer"],
        "oldest_unallocated_date": summary["oldest_unallocated_date"]
    }


@router.post("/batch-allocate")
async def batch_allocate_payments(
    batch_request: BatchPaymentAllocation,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Allocate multiple payments using FIFO/LIFO method.
    """
    results = []
    success_count = 0
    error_count = 0
    
    for payment_id in batch_request.payment_ids:
        try:
            result = payment_service.auto_allocate_payment(
                db,
                payment_id=payment_id,
                method=batch_request.allocation_method,
                user_id=current_user.id
            )
            
            success_count += 1
            results.append({
                "payment_id": payment_id,
                "success": True,
                "allocated_amount": result["allocated_amount"],
                "invoices_allocated": len(result["allocations"])
            })
        except Exception as e:
            error_count += 1
            results.append({
                "payment_id": payment_id,
                "success": False,
                "error": str(e)
            })
    
    return {
        "total_processed": len(batch_request.payment_ids),
        "success_count": success_count,
        "error_count": error_count,
        "results": results
    }


@router.post("/deposit-slip")
async def create_deposit_slip(
    payment_date: date,
    bank_account: str,
    payment_method: Optional[PaymentMethod] = None,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Create deposit slip for payments.
    
    Groups payments by date and payment method.
    """
    deposit = payment_service.create_deposit_slip(
        db,
        payment_date=payment_date,
        bank_account=bank_account,
        payment_method=payment_method,
        created_by=current_user.id
    )
    
    return {
        "deposit_number": deposit["deposit_number"],
        "deposit_date": deposit["deposit_date"],
        "payment_count": deposit["payment_count"],
        "total_amount": deposit["total_amount"],
        "by_payment_method": deposit["by_payment_method"]
    }


@router.get("/cash-forecast")
async def get_cash_forecast(
    days_ahead: int = Query(30, ge=1, le=90),
    include_overdue: bool = Query(True),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Get cash collection forecast.
    
    Based on invoice due dates and payment history.
    """
    forecast = payment_service.generate_cash_forecast(
        db,
        days_ahead=days_ahead,
        include_overdue=include_overdue
    )
    
    return forecast


@router.post("/payment-run")
async def create_payment_run(
    run_date: date,
    customer_codes: Optional[List[str]] = None,
    min_balance: Decimal = Query(0, description="Minimum balance to include"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Create automated payment collection run.
    
    Generates payment requests for customers with outstanding balances.
    """
    run_result = payment_service.create_payment_run(
        db,
        run_date=run_date,
        customer_codes=customer_codes,
        min_balance=min_balance,
        created_by=current_user.id
    )
    
    return {
        "run_id": run_result["run_id"],
        "run_date": run_date,
        "customers_processed": run_result["customers_processed"],
        "total_outstanding": run_result["total_outstanding"],
        "payment_requests_created": run_result["payment_requests_created"]
    }