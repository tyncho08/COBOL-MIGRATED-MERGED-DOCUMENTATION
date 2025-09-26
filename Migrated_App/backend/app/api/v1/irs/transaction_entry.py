"""Transaction Entry API endpoints for IRS module"""

from fastapi import APIRouter, Depends, HTTPException, status, Query
from sqlalchemy.orm import Session
from typing import List, Optional
from datetime import date, datetime
from decimal import Decimal

from app.api import deps
from app.services.irs import transaction_entry as transaction_service
from app.schemas.irs import (
    IRSTransaction, IRSTransactionCreate, IRSTransactionUpdate,
    TransactionSummary, BatchTransaction, TransactionValidation
)

router = APIRouter()


@router.post("", response_model=IRSTransaction, status_code=status.HTTP_201_CREATED)
async def create_transaction(
    transaction_in: IRSTransactionCreate,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Create a new IRS transaction.
    
    Transaction types:
    - INCOME: Income transaction
    - EXPENSE: Expense transaction
    - ASSET: Asset purchase/sale
    - LIABILITY: Liability transaction
    - EQUITY: Equity transaction
    """
    # Validate transaction before creation
    validation = transaction_service.validate_transaction(db, transaction_in)
    if not validation.is_valid:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=validation.errors[0] if validation.errors else "Transaction validation failed"
        )
    
    transaction = transaction_service.create_transaction(
        db, transaction_in=transaction_in, created_by=current_user.id
    )
    return transaction


@router.post("/batch", response_model=List[IRSTransaction])
async def create_batch_transactions(
    batch_in: BatchTransaction,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Create multiple transactions in a single batch.
    
    All transactions must be valid for the batch to be processed.
    """
    # Validate all transactions
    for idx, trans in enumerate(batch_in.transactions):
        validation = transaction_service.validate_transaction(db, trans)
        if not validation.is_valid:
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail=f"Transaction {idx + 1} validation failed: {validation.errors[0]}"
            )
    
    transactions = transaction_service.create_batch_transactions(
        db, batch_in=batch_in, created_by=current_user.id
    )
    return transactions


@router.get("", response_model=List[IRSTransaction])
async def list_transactions(
    company_id: Optional[int] = Query(None, description="Filter by company ID"),
    transaction_type: Optional[str] = Query(None, description="Filter by transaction type"),
    start_date: Optional[date] = Query(None, description="Start date for date range"),
    end_date: Optional[date] = Query(None, description="End date for date range"),
    min_amount: Optional[Decimal] = Query(None, description="Minimum transaction amount"),
    max_amount: Optional[Decimal] = Query(None, description="Maximum transaction amount"),
    account_code: Optional[str] = Query(None, description="Filter by account code"),
    skip: int = 0,
    limit: int = 100,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    List IRS transactions with optional filters.
    """
    filters = {
        "company_id": company_id,
        "transaction_type": transaction_type,
        "start_date": start_date,
        "end_date": end_date,
        "min_amount": min_amount,
        "max_amount": max_amount,
        "account_code": account_code
    }
    # Remove None values
    filters = {k: v for k, v in filters.items() if v is not None}
    
    transactions = transaction_service.get_transactions(
        db, skip=skip, limit=limit, **filters
    )
    return transactions


@router.get("/summary", response_model=TransactionSummary)
async def get_transaction_summary(
    company_id: int,
    start_date: date,
    end_date: date,
    group_by: str = Query("type", description="Group by: type, account, category"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Get summarized transaction data for reporting.
    """
    summary = transaction_service.get_transaction_summary(
        db,
        company_id=company_id,
        start_date=start_date,
        end_date=end_date,
        group_by=group_by
    )
    return summary


@router.get("/{transaction_id}", response_model=IRSTransaction)
async def get_transaction(
    transaction_id: int,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Get specific transaction by ID."""
    transaction = transaction_service.get_transaction(db, transaction_id=transaction_id)
    if not transaction:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Transaction with id {transaction_id} not found"
        )
    return transaction


@router.put("/{transaction_id}", response_model=IRSTransaction)
async def update_transaction(
    transaction_id: int,
    transaction_in: IRSTransactionUpdate,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Update an existing transaction.
    
    Note: Some fields may not be editable after posting.
    """
    transaction = transaction_service.get_transaction(db, transaction_id=transaction_id)
    if not transaction:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Transaction with id {transaction_id} not found"
        )
    
    if transaction.posted:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Cannot update posted transaction"
        )
    
    # Validate updated transaction
    validation = transaction_service.validate_transaction_update(db, transaction_id, transaction_in)
    if not validation.is_valid:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=validation.errors[0] if validation.errors else "Transaction validation failed"
        )
    
    transaction = transaction_service.update_transaction(
        db, db_transaction=transaction, transaction_in=transaction_in
    )
    return transaction


@router.delete("/{transaction_id}")
async def delete_transaction(
    transaction_id: int,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Delete a transaction.
    
    Note: Posted transactions cannot be deleted.
    """
    transaction = transaction_service.get_transaction(db, transaction_id=transaction_id)
    if not transaction:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Transaction with id {transaction_id} not found"
        )
    
    if transaction.posted:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Cannot delete posted transaction. Consider creating a reversal entry."
        )
    
    transaction_service.delete_transaction(db, transaction_id=transaction_id)
    return {"message": f"Transaction {transaction_id} deleted successfully"}


@router.post("/{transaction_id}/post")
async def post_transaction(
    transaction_id: int,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Post a transaction to make it permanent.
    
    Posted transactions cannot be edited or deleted.
    """
    transaction = transaction_service.get_transaction(db, transaction_id=transaction_id)
    if not transaction:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Transaction with id {transaction_id} not found"
        )
    
    if transaction.posted:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Transaction is already posted"
        )
    
    posted_transaction = transaction_service.post_transaction(
        db, transaction_id=transaction_id, posted_by=current_user.id
    )
    return {
        "message": f"Transaction {transaction_id} posted successfully",
        "posted_at": posted_transaction.posted_at
    }


@router.post("/{transaction_id}/void")
async def void_transaction(
    transaction_id: int,
    reason: str = Query(..., description="Reason for voiding the transaction"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Void a posted transaction by creating a reversal entry.
    """
    transaction = transaction_service.get_transaction(db, transaction_id=transaction_id)
    if not transaction:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Transaction with id {transaction_id} not found"
        )
    
    if not transaction.posted:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Only posted transactions can be voided"
        )
    
    if transaction.voided:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Transaction is already voided"
        )
    
    reversal = transaction_service.void_transaction(
        db, transaction_id=transaction_id, reason=reason, voided_by=current_user.id
    )
    return {
        "message": f"Transaction {transaction_id} voided successfully",
        "reversal_id": reversal.id
    }


@router.post("/validate", response_model=TransactionValidation)
async def validate_transaction(
    transaction_in: IRSTransactionCreate,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Validate a transaction without creating it.
    
    Useful for pre-validation in forms.
    """
    validation = transaction_service.validate_transaction(db, transaction_in)
    return validation


@router.get("/{transaction_id}/audit-trail")
async def get_transaction_audit_trail(
    transaction_id: int,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Get complete audit trail for a transaction."""
    audit_trail = transaction_service.get_transaction_audit_trail(db, transaction_id=transaction_id)
    if not audit_trail:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"No audit trail found for transaction {transaction_id}"
        )
    return audit_trail