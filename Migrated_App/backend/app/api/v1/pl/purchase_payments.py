"""Purchase Ledger Payment Processing API endpoints"""

from fastapi import APIRouter, Depends, HTTPException, status, Query, Body
from sqlalchemy.orm import Session
from typing import List, Optional, Dict, Any
from datetime import date, datetime
from decimal import Decimal

from app.api import deps
from app.services.pl import supplier_master as payment_service
from app.schemas.pl import (
    PurchasePayment, PurchasePaymentCreate, PaymentListResponse,
    PaymentAllocation, BatchPaymentRun, RemittanceAdvice
)

router = APIRouter()


@router.get("", response_model=PaymentListResponse)
async def list_payments(
    supplier_code: Optional[str] = Query(None, description="Filter by supplier"),
    start_date: Optional[date] = Query(None, description="Payment date from"),
    end_date: Optional[date] = Query(None, description="Payment date to"),
    payment_method: Optional[str] = Query(None, description="Payment method"),
    check_number: Optional[str] = Query(None, description="Check number search"),
    unprinted_checks: bool = Query(False, description="Only unprinted checks"),
    skip: int = 0,
    limit: int = 100,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    List purchase payments with optional filters.
    """
    payments = payment_service.get_payments(
        db,
        supplier_code=supplier_code,
        start_date=start_date,
        end_date=end_date,
        payment_method=payment_method,
        check_number=check_number,
        unprinted_checks=unprinted_checks,
        skip=skip,
        limit=limit
    )
    
    total = payment_service.count_payments(
        db,
        supplier_code=supplier_code,
        start_date=start_date,
        end_date=end_date,
        payment_method=payment_method,
        check_number=check_number,
        unprinted_checks=unprinted_checks
    )
    
    return PaymentListResponse(
        payments=payments,
        total=total,
        skip=skip,
        limit=limit
    )


@router.get("/{payment_id}", response_model=PurchasePayment)
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


@router.post("", response_model=PurchasePayment, status_code=status.HTTP_201_CREATED)
async def create_payment(
    payment_in: PurchasePaymentCreate,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Create new payment to supplier.
    
    Allocates to specific invoices.
    """
    # Validate supplier
    supplier = payment_service.validate_supplier(db, supplier_code=payment_in.supplier_code)
    if not supplier:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Supplier {payment_in.supplier_code} not found"
        )
    
    # Validate bank account
    bank_account = payment_service.validate_bank_account(db, account_code=payment_in.bank_account)
    if not bank_account:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Bank account {payment_in.bank_account} not found"
        )
    
    # Validate allocations
    if payment_in.allocations:
        total_allocated = sum(a.amount_applied + a.discount_taken for a in payment_in.allocations)
        if total_allocated > payment_in.amount:
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail=f"Total allocation ({total_allocated}) exceeds payment amount ({payment_in.amount})"
            )
        
        # Validate each invoice
        for allocation in payment_in.allocations:
            invoice = payment_service.get_invoice(db, invoice_id=allocation.invoice_id)
            if not invoice:
                raise HTTPException(
                    status_code=status.HTTP_404_NOT_FOUND,
                    detail=f"Invoice {allocation.invoice_id} not found"
                )
            if invoice.supplier_code != payment_in.supplier_code:
                raise HTTPException(
                    status_code=status.HTTP_400_BAD_REQUEST,
                    detail=f"Invoice {allocation.invoice_number} belongs to different supplier"
                )
            if allocation.amount_applied > invoice.balance_due:
                raise HTTPException(
                    status_code=status.HTTP_400_BAD_REQUEST,
                    detail=f"Allocation exceeds invoice balance for {allocation.invoice_number}"
                )
    
    payment = payment_service.create_payment(
        db,
        payment_in=payment_in,
        created_by=current_user.id
    )
    
    return payment


@router.post("/{payment_id}/void")
async def void_payment(
    payment_id: int,
    void_date: date = Body(...),
    reason: str = Body(..., min_length=10),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Void a payment.
    
    Reverses allocations and GL entries.
    """
    payment = payment_service.get_payment(db, payment_id=payment_id)
    if not payment:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Payment {payment_id} not found"
        )
    
    if payment.void_status:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Payment is already voided"
        )
    
    # Check if check has been cleared
    if payment.payment_method == "CHECK" and payment_service.is_check_cleared(db, payment_id=payment_id):
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Cannot void cleared check"
        )
    
    result = payment_service.void_payment(
        db,
        payment_id=payment_id,
        void_date=void_date,
        reason=reason,
        voided_by=current_user.id
    )
    
    return {
        "payment_id": payment_id,
        "payment_number": payment.payment_number,
        "voided_at": datetime.now(),
        "reversal_entries": result["reversal_entries"]
    }


@router.post("/{payment_id}/print-check")
async def print_check(
    payment_id: int,
    check_number: Optional[str] = Body(None, description="Manual check number"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Print check for payment.
    
    Assigns check number if not provided.
    """
    payment = payment_service.get_payment(db, payment_id=payment_id)
    if not payment:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Payment {payment_id} not found"
        )
    
    if payment.payment_method != "CHECK":
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Payment method is not CHECK"
        )
    
    if payment.check_printed:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Check already printed"
        )
    
    # Generate check document
    result = payment_service.print_check(
        db,
        payment_id=payment_id,
        check_number=check_number,
        printed_by=current_user.id
    )
    
    import base64
    return {
        "payment_id": payment_id,
        "check_number": result["check_number"],
        "document_type": "PDF",
        "document_data": base64.b64encode(result["document"]).decode(),
        "filename": f"check_{result['check_number']}.pdf"
    }


@router.post("/payment-run")
async def create_payment_run(
    run_params: BatchPaymentRun,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Create batch payment run.
    
    Selects invoices for payment based on criteria.
    """
    # Validate bank account
    bank_account = payment_service.validate_bank_account(db, account_code=run_params.bank_account)
    if not bank_account:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Bank account {run_params.bank_account} not found"
        )
    
    # Get invoices for payment
    invoices = payment_service.select_invoices_for_payment(
        db,
        supplier_codes=run_params.supplier_codes,
        payment_date=run_params.payment_date,
        include_overdue_only=run_params.include_overdue_only,
        min_payment_amount=run_params.min_payment_amount
    )
    
    if not invoices:
        return {
            "message": "No invoices found matching criteria",
            "invoices_selected": 0,
            "total_amount": 0
        }
    
    # Create payment run
    run_result = payment_service.create_payment_run(
        db,
        run_params=run_params,
        invoices=invoices,
        created_by=current_user.id
    )
    
    return {
        "run_id": run_result["run_id"],
        "payment_date": run_params.payment_date,
        "payments_created": run_result["payments_created"],
        "total_amount": run_result["total_amount"],
        "suppliers_paid": run_result["suppliers_paid"]
    }


@router.get("/payment-run/{run_id}")
async def get_payment_run_details(
    run_id: int,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Get payment run details.
    """
    run_details = payment_service.get_payment_run(db, run_id=run_id)
    if not run_details:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Payment run {run_id} not found"
        )
    
    return run_details


@router.post("/payment-run/{run_id}/approve")
async def approve_payment_run(
    run_id: int,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Approve payment run.
    
    Releases payments for processing.
    """
    run_details = payment_service.get_payment_run(db, run_id=run_id)
    if not run_details:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Payment run {run_id} not found"
        )
    
    if run_details["status"] != "PENDING":
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Payment run status is {run_details['status']}"
        )
    
    result = payment_service.approve_payment_run(
        db,
        run_id=run_id,
        approved_by=current_user.id
    )
    
    return {
        "run_id": run_id,
        "status": result["status"],
        "approved_at": result["approved_at"],
        "payments_released": result["payments_released"]
    }


@router.post("/remittance/{payment_id}")
async def send_remittance_advice(
    payment_id: int,
    remittance: RemittanceAdvice,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Send remittance advice to supplier.
    """
    payment = payment_service.get_payment(db, payment_id=payment_id)
    if not payment:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Payment {payment_id} not found"
        )
    
    # Validate supplier email
    if remittance.send_email:
        supplier = payment_service.get_supplier(db, supplier_code=payment.supplier_code)
        email_address = remittance.email_override or supplier.email
        if not email_address:
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail="No email address available for supplier"
            )
    
    result = payment_service.send_remittance_advice(
        db,
        payment_id=payment_id,
        remittance=remittance,
        sent_by=current_user.id
    )
    
    return {
        "payment_id": payment_id,
        "sent_to": result["sent_to"],
        "sent_at": result["sent_at"],
        "method": result["method"]
    }


@router.get("/check-register")
async def get_check_register(
    bank_account: str = Query(..., description="Bank account code"),
    start_date: Optional[date] = None,
    end_date: Optional[date] = None,
    check_number_from: Optional[str] = None,
    check_number_to: Optional[str] = None,
    include_voided: bool = Query(False),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Get check register report.
    """
    register = payment_service.get_check_register(
        db,
        bank_account=bank_account,
        start_date=start_date,
        end_date=end_date,
        check_number_from=check_number_from,
        check_number_to=check_number_to,
        include_voided=include_voided
    )
    
    return register


@router.post("/eft-file")
async def generate_eft_file(
    payment_ids: List[int] = Body(...),
    file_format: str = Body("ACH", regex="^(ACH|WIRE|CUSTOM)$"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Generate EFT file for electronic payments.
    """
    # Validate payments
    payments = []
    for payment_id in payment_ids:
        payment = payment_service.get_payment(db, payment_id=payment_id)
        if not payment:
            raise HTTPException(
                status_code=status.HTTP_404_NOT_FOUND,
                detail=f"Payment {payment_id} not found"
            )
        if payment.payment_method not in ["ACH", "WIRE"]:
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail=f"Payment {payment_id} is not electronic"
            )
        payments.append(payment)
    
    # Generate file
    file_data = payment_service.generate_eft_file(
        db,
        payments=payments,
        file_format=file_format
    )
    
    import base64
    return {
        "file_format": file_format,
        "payment_count": len(payments),
        "total_amount": sum(p.amount for p in payments),
        "file_data": base64.b64encode(file_data).decode(),
        "filename": f"EFT_{datetime.now().strftime('%Y%m%d_%H%M%S')}.txt"
    }


@router.get("/cash-requirements")
async def get_cash_requirements(
    as_of_date: date = Query(...),
    days_ahead: int = Query(30, ge=1, le=90),
    bank_account: Optional[str] = None,
    include_approved_only: bool = Query(True),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Get cash requirements forecast.
    """
    forecast = payment_service.calculate_cash_requirements(
        db,
        as_of_date=as_of_date,
        days_ahead=days_ahead,
        bank_account=bank_account,
        include_approved_only=include_approved_only
    )
    
    return forecast


@router.post("/positive-pay")
async def generate_positive_pay_file(
    bank_account: str = Body(...),
    payment_date: date = Body(...),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Generate positive pay file for bank.
    
    Fraud prevention for check payments.
    """
    # Get payments for date
    payments = payment_service.get_payments_for_positive_pay(
        db,
        bank_account=bank_account,
        payment_date=payment_date
    )
    
    if not payments:
        return {
            "message": "No check payments found for date",
            "payment_count": 0
        }
    
    # Generate file
    file_data = payment_service.generate_positive_pay_file(
        db,
        bank_account=bank_account,
        payments=payments
    )
    
    import base64
    return {
        "bank_account": bank_account,
        "payment_date": payment_date,
        "payment_count": len(payments),
        "total_amount": sum(p.amount for p in payments),
        "file_data": base64.b64encode(file_data).decode(),
        "filename": f"PositivePay_{bank_account}_{payment_date}.txt"
    }