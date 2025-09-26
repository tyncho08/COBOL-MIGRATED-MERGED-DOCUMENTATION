"""Sales Ledger Credit Control API endpoints"""

from fastapi import APIRouter, Depends, HTTPException, status, Query, Body
from sqlalchemy.orm import Session
from typing import List, Optional, Dict, Any
from datetime import date, datetime
from decimal import Decimal

from app.api import deps
from app.services.sl import customer_master as credit_service
from app.schemas.sl import (
    CustomerSummary, CreditStatus, CreditHoldRequest,
    CreditReleaseRequest, CollectionNote, DunningLetter
)

router = APIRouter()


@router.get("/{customer_code}/status", response_model=CustomerSummary)
async def get_credit_status(
    customer_code: str,
    include_aging: bool = Query(True, description="Include aging analysis"),
    include_transactions: bool = Query(False, description="Include recent transactions"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Get comprehensive customer credit status.
    
    Includes credit limit, balance, aging, and credit analysis.
    """
    customer = credit_service.get_customer_with_summary(
        db,
        customer_code=customer_code,
        include_aging=include_aging,
        include_transactions=include_transactions
    )
    
    if not customer:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Customer {customer_code} not found"
        )
    
    return customer


@router.get("/credit-review")
async def get_credit_review_list(
    review_type: str = Query("all", regex="^(all|overlimit|overdue|warning)$"),
    min_exposure: Decimal = Query(0, description="Minimum credit exposure"),
    days_overdue: int = Query(0, ge=0),
    territory_code: Optional[str] = None,
    salesperson_code: Optional[str] = None,
    skip: int = 0,
    limit: int = 100,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Get customers requiring credit review.
    
    Filters by various credit risk criteria.
    """
    customers = credit_service.get_credit_review_list(
        db,
        review_type=review_type,
        min_exposure=min_exposure,
        days_overdue=days_overdue,
        territory_code=territory_code,
        salesperson_code=salesperson_code,
        skip=skip,
        limit=limit
    )
    
    return {
        "review_date": date.today(),
        "review_type": review_type,
        "customer_count": len(customers),
        "total_exposure": sum(c.current_balance for c in customers),
        "customers": customers
    }


@router.post("/{customer_code}/hold")
async def place_credit_hold(
    customer_code: str,
    hold_request: CreditHoldRequest,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Place customer on credit hold.
    
    Blocks new orders and/or shipments.
    """
    customer = credit_service.get_customer(db, customer_code=customer_code)
    if not customer:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Customer {customer_code} not found"
        )
    
    if customer.credit_status == CreditStatus.BLOCKED:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Customer is already on credit hold"
        )
    
    result = credit_service.place_credit_hold(
        db,
        customer_code=customer_code,
        reason=hold_request.reason,
        hold_orders=hold_request.hold_orders,
        hold_shipments=hold_request.hold_shipments,
        notes=hold_request.notes,
        placed_by=current_user.id
    )
    
    return {
        "customer_code": customer_code,
        "credit_status": result.credit_status,
        "hold_orders": result.hold_orders,
        "hold_shipments": result.hold_shipments,
        "hold_placed_at": datetime.now(),
        "open_orders_affected": result.get("open_orders_affected", 0)
    }


@router.post("/{customer_code}/release")
async def release_credit_hold(
    customer_code: str,
    release_request: CreditReleaseRequest,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Release customer from credit hold.
    
    Allows orders and shipments to proceed.
    """
    customer = credit_service.get_customer(db, customer_code=customer_code)
    if not customer:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Customer {customer_code} not found"
        )
    
    if customer.credit_status != CreditStatus.BLOCKED:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Customer is not on credit hold"
        )
    
    result = credit_service.release_credit_hold(
        db,
        customer_code=customer_code,
        reason=release_request.reason,
        release_orders=release_request.release_orders,
        release_shipments=release_request.release_shipments,
        notes=release_request.notes,
        released_by=current_user.id
    )
    
    return {
        "customer_code": customer_code,
        "credit_status": result.credit_status,
        "hold_released_at": datetime.now(),
        "orders_released": result.get("orders_released", 0)
    }


@router.put("/{customer_code}/credit-limit")
async def update_credit_limit(
    customer_code: str,
    new_limit: Decimal = Body(..., ge=0),
    reason: str = Body(..., min_length=10),
    temporary: bool = Body(False),
    expiry_date: Optional[date] = Body(None),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Update customer credit limit.
    
    Can set temporary or permanent limits.
    """
    customer = credit_service.get_customer(db, customer_code=customer_code)
    if not customer:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Customer {customer_code} not found"
        )
    
    updated = credit_service.update_credit_limit(
        db,
        customer_code=customer_code,
        new_limit=new_limit,
        reason=reason,
        temporary=temporary,
        expiry_date=expiry_date,
        updated_by=current_user.id
    )
    
    return {
        "customer_code": customer_code,
        "previous_limit": customer.credit_limit,
        "new_limit": new_limit,
        "temporary": temporary,
        "expiry_date": expiry_date,
        "credit_available": new_limit - customer.current_balance
    }


@router.post("/{customer_code}/collection-note")
async def add_collection_note(
    customer_code: str,
    note: CollectionNote,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Add collection activity note.
    
    Records customer contact and promises.
    """
    customer = credit_service.get_customer(db, customer_code=customer_code)
    if not customer:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Customer {customer_code} not found"
        )
    
    note_id = credit_service.add_collection_note(
        db,
        customer_code=customer_code,
        note=note,
        created_by=current_user.id
    )
    
    # Update promise to pay if provided
    if note.promise_date and note.promise_amount:
        credit_service.record_promise_to_pay(
            db,
            customer_code=customer_code,
            promise_date=note.promise_date,
            promise_amount=note.promise_amount
        )
    
    return {
        "note_id": note_id,
        "customer_code": customer_code,
        "contact_date": note.contact_date,
        "follow_up_scheduled": note.follow_up_date is not None
    }


@router.get("/{customer_code}/collection-history")
async def get_collection_history(
    customer_code: str,
    days_back: int = Query(90, ge=1, le=365),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Get collection activity history.
    
    Shows all collection notes and contacts.
    """
    history = credit_service.get_collection_history(
        db,
        customer_code=customer_code,
        days_back=days_back
    )
    
    if not history:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Customer {customer_code} not found"
        )
    
    return history


@router.post("/dunning-letters")
async def generate_dunning_letters(
    min_days_overdue: int = Query(30, ge=1),
    letter_level: int = Query(1, ge=1, le=5),
    customer_codes: Optional[List[str]] = None,
    preview_only: bool = Query(False),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Generate dunning letters for overdue accounts.
    
    Progressive letter levels from gentle reminder to final notice.
    """
    # Get eligible customers
    if customer_codes:
        customers = credit_service.get_customers_by_codes(db, customer_codes)
    else:
        customers = credit_service.get_overdue_customers(
            db,
            min_days_overdue=min_days_overdue,
            letter_level=letter_level
        )
    
    results = []
    letters_generated = 0
    
    for customer in customers:
        try:
            # Check if eligible for this letter level
            if not credit_service.is_eligible_for_dunning(
                db,
                customer_code=customer.customer_code,
                letter_level=letter_level
            ):
                continue
            
            if preview_only:
                results.append({
                    "customer_code": customer.customer_code,
                    "customer_name": customer.customer_name,
                    "days_overdue": customer.max_days_overdue,
                    "overdue_amount": customer.overdue_amount,
                    "letter_level": letter_level,
                    "would_generate": True
                })
            else:
                letter = credit_service.generate_dunning_letter(
                    db,
                    customer_code=customer.customer_code,
                    letter_level=letter_level,
                    created_by=current_user.id
                )
                
                letters_generated += 1
                results.append({
                    "customer_code": customer.customer_code,
                    "letter_id": letter.id,
                    "letter_level": letter_level,
                    "generated": True
                })
                
        except Exception as e:
            results.append({
                "customer_code": customer.customer_code,
                "error": str(e),
                "generated": False
            })
    
    return {
        "preview_only": preview_only,
        "customers_reviewed": len(customers),
        "letters_generated": letters_generated,
        "letter_level": letter_level,
        "results": results
    }


@router.get("/promises-to-pay")
async def get_promises_to_pay(
    status: str = Query("pending", regex="^(pending|kept|broken|all)$"),
    from_date: Optional[date] = None,
    to_date: Optional[date] = None,
    customer_code: Optional[str] = None,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Get promise to pay tracking.
    
    Monitor customer payment promises.
    """
    promises = credit_service.get_promises_to_pay(
        db,
        status=status,
        from_date=from_date,
        to_date=to_date,
        customer_code=customer_code
    )
    
    summary = {
        "total_promises": len(promises),
        "total_amount": sum(p.promise_amount for p in promises),
        "pending_amount": sum(p.promise_amount for p in promises if p.status == "pending"),
        "kept_amount": sum(p.amount_received or 0 for p in promises if p.status == "kept"),
        "broken_count": sum(1 for p in promises if p.status == "broken")
    }
    
    return {
        "summary": summary,
        "promises": promises
    }


@router.post("/credit-insurance-claim")
async def create_credit_insurance_claim(
    customer_code: str,
    claim_amount: Decimal = Body(..., gt=0),
    invoice_numbers: List[str] = Body(...),
    reason: str = Body(...),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Create credit insurance claim.
    
    For bad debt write-offs.
    """
    claim = credit_service.create_insurance_claim(
        db,
        customer_code=customer_code,
        claim_amount=claim_amount,
        invoice_numbers=invoice_numbers,
        reason=reason,
        created_by=current_user.id
    )
    
    return {
        "claim_id": claim.id,
        "claim_number": claim.claim_number,
        "customer_code": customer_code,
        "claim_amount": claim_amount,
        "status": "submitted",
        "invoices_included": len(invoice_numbers)
    }