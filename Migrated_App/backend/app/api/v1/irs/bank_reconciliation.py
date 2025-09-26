"""Bank Reconciliation API endpoints for IRS module"""

from fastapi import APIRouter, Depends, HTTPException, status, Query, UploadFile, File
from sqlalchemy.orm import Session
from typing import List, Optional
from datetime import date
from decimal import Decimal

from app.api import deps
from app.services.irs import bank_reconciliation as bank_service
from app.schemas.irs import (
    BankReconciliation, BankReconciliationCreate, BankReconciliationUpdate,
    BankStatement, BankTransaction, ReconciliationItem, ReconciliationSummary
)

router = APIRouter()


@router.post("/statements", response_model=BankStatement, status_code=status.HTTP_201_CREATED)
async def upload_bank_statement(
    company_id: int = Query(..., description="Company ID"),
    bank_account_id: int = Query(..., description="Bank Account ID"),
    statement_date: date = Query(..., description="Statement Date"),
    ending_balance: Decimal = Query(..., description="Statement Ending Balance"),
    statement_file: Optional[UploadFile] = File(None),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Upload a bank statement for reconciliation.
    
    Can optionally include a PDF/CSV file of the statement.
    """
    statement_data = {
        "company_id": company_id,
        "bank_account_id": bank_account_id,
        "statement_date": statement_date,
        "ending_balance": ending_balance
    }
    
    if statement_file:
        # Process uploaded file
        file_content = await statement_file.read()
        statement_data["file_content"] = file_content
        statement_data["file_name"] = statement_file.filename
        statement_data["file_type"] = statement_file.content_type
    
    statement = bank_service.create_bank_statement(
        db, statement_data=statement_data, uploaded_by=current_user.id
    )
    return statement


@router.get("/statements", response_model=List[BankStatement])
async def list_bank_statements(
    company_id: int = Query(..., description="Company ID"),
    bank_account_id: Optional[int] = Query(None, description="Filter by Bank Account"),
    start_date: Optional[date] = Query(None, description="Start date"),
    end_date: Optional[date] = Query(None, description="End date"),
    skip: int = 0,
    limit: int = 100,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """List bank statements with optional filters."""
    statements = bank_service.get_bank_statements(
        db,
        company_id=company_id,
        bank_account_id=bank_account_id,
        start_date=start_date,
        end_date=end_date,
        skip=skip,
        limit=limit
    )
    return statements


@router.post("/import-transactions", response_model=List[BankTransaction])
async def import_bank_transactions(
    statement_id: int = Query(..., description="Bank Statement ID"),
    transactions_file: UploadFile = File(..., description="CSV/OFX file with transactions"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Import bank transactions from a file (CSV/OFX format).
    """
    # Get statement
    statement = bank_service.get_bank_statement(db, statement_id=statement_id)
    if not statement:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Bank statement {statement_id} not found"
        )
    
    # Process file
    file_content = await transactions_file.read()
    file_type = transactions_file.filename.split('.')[-1].lower()
    
    try:
        transactions = bank_service.import_bank_transactions(
            db,
            statement_id=statement_id,
            file_content=file_content,
            file_type=file_type,
            imported_by=current_user.id
        )
        return transactions
    except ValueError as e:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=str(e)
        )


@router.post("", response_model=BankReconciliation, status_code=status.HTTP_201_CREATED)
async def create_reconciliation(
    reconciliation_in: BankReconciliationCreate,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Create a new bank reconciliation.
    """
    # Validate bank statement exists
    statement = bank_service.get_bank_statement(db, statement_id=reconciliation_in.statement_id)
    if not statement:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Bank statement {reconciliation_in.statement_id} not found"
        )
    
    reconciliation = bank_service.create_reconciliation(
        db, reconciliation_in=reconciliation_in, created_by=current_user.id
    )
    return reconciliation


@router.get("", response_model=List[BankReconciliation])
async def list_reconciliations(
    company_id: int = Query(..., description="Company ID"),
    bank_account_id: Optional[int] = Query(None, description="Filter by Bank Account"),
    status: Optional[str] = Query(None, description="Filter by status: draft, completed, approved"),
    skip: int = 0,
    limit: int = 100,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """List bank reconciliations with optional filters."""
    reconciliations = bank_service.get_reconciliations(
        db,
        company_id=company_id,
        bank_account_id=bank_account_id,
        status=status,
        skip=skip,
        limit=limit
    )
    return reconciliations


@router.get("/{reconciliation_id}", response_model=BankReconciliation)
async def get_reconciliation(
    reconciliation_id: int,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Get specific bank reconciliation by ID."""
    reconciliation = bank_service.get_reconciliation(db, reconciliation_id=reconciliation_id)
    if not reconciliation:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Reconciliation {reconciliation_id} not found"
        )
    return reconciliation


@router.put("/{reconciliation_id}", response_model=BankReconciliation)
async def update_reconciliation(
    reconciliation_id: int,
    reconciliation_in: BankReconciliationUpdate,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Update a bank reconciliation."""
    reconciliation = bank_service.get_reconciliation(db, reconciliation_id=reconciliation_id)
    if not reconciliation:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Reconciliation {reconciliation_id} not found"
        )
    
    if reconciliation.status == "approved":
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Cannot update approved reconciliation"
        )
    
    reconciliation = bank_service.update_reconciliation(
        db, db_reconciliation=reconciliation, reconciliation_in=reconciliation_in
    )
    return reconciliation


@router.post("/{reconciliation_id}/items", response_model=ReconciliationItem)
async def add_reconciliation_item(
    reconciliation_id: int,
    item_data: ReconciliationItem,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Add an item to the reconciliation."""
    reconciliation = bank_service.get_reconciliation(db, reconciliation_id=reconciliation_id)
    if not reconciliation:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Reconciliation {reconciliation_id} not found"
        )
    
    if reconciliation.status == "approved":
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Cannot add items to approved reconciliation"
        )
    
    item = bank_service.add_reconciliation_item(
        db, reconciliation_id=reconciliation_id, item_data=item_data
    )
    return item


@router.delete("/{reconciliation_id}/items/{item_id}")
async def remove_reconciliation_item(
    reconciliation_id: int,
    item_id: int,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Remove an item from the reconciliation."""
    reconciliation = bank_service.get_reconciliation(db, reconciliation_id=reconciliation_id)
    if not reconciliation:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Reconciliation {reconciliation_id} not found"
        )
    
    if reconciliation.status == "approved":
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Cannot remove items from approved reconciliation"
        )
    
    bank_service.remove_reconciliation_item(db, item_id=item_id)
    return {"message": f"Item {item_id} removed successfully"}


@router.post("/{reconciliation_id}/auto-match")
async def auto_match_transactions(
    reconciliation_id: int,
    tolerance_amount: Decimal = Query(0.01, description="Amount tolerance for matching"),
    tolerance_days: int = Query(3, description="Date tolerance in days"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Automatically match bank transactions with book transactions.
    """
    reconciliation = bank_service.get_reconciliation(db, reconciliation_id=reconciliation_id)
    if not reconciliation:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Reconciliation {reconciliation_id} not found"
        )
    
    match_result = bank_service.auto_match_transactions(
        db,
        reconciliation_id=reconciliation_id,
        tolerance_amount=tolerance_amount,
        tolerance_days=tolerance_days
    )
    
    return {
        "matched_count": match_result["matched_count"],
        "unmatched_bank": match_result["unmatched_bank"],
        "unmatched_book": match_result["unmatched_book"],
        "message": f"Matched {match_result['matched_count']} transactions"
    }


@router.get("/{reconciliation_id}/summary", response_model=ReconciliationSummary)
async def get_reconciliation_summary(
    reconciliation_id: int,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Get reconciliation summary with balances and differences."""
    summary = bank_service.get_reconciliation_summary(db, reconciliation_id=reconciliation_id)
    if not summary:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Reconciliation {reconciliation_id} not found"
        )
    return summary


@router.post("/{reconciliation_id}/complete")
async def complete_reconciliation(
    reconciliation_id: int,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Mark reconciliation as complete and ready for approval.
    """
    reconciliation = bank_service.get_reconciliation(db, reconciliation_id=reconciliation_id)
    if not reconciliation:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Reconciliation {reconciliation_id} not found"
        )
    
    # Check if reconciliation is balanced
    summary = bank_service.get_reconciliation_summary(db, reconciliation_id=reconciliation_id)
    if summary.difference != 0:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Reconciliation is not balanced. Difference: {summary.difference}"
        )
    
    completed = bank_service.complete_reconciliation(
        db, reconciliation_id=reconciliation_id, completed_by=current_user.id
    )
    return {
        "message": "Reconciliation completed successfully",
        "status": completed.status
    }


@router.post("/{reconciliation_id}/approve")
async def approve_reconciliation(
    reconciliation_id: int,
    approval_notes: Optional[str] = Query(None, description="Approval notes"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Approve a completed reconciliation.
    
    Only administrators can approve reconciliations.
    """
    reconciliation = bank_service.get_reconciliation(db, reconciliation_id=reconciliation_id)
    if not reconciliation:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Reconciliation {reconciliation_id} not found"
        )
    
    if reconciliation.status != "completed":
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Only completed reconciliations can be approved"
        )
    
    approved = bank_service.approve_reconciliation(
        db,
        reconciliation_id=reconciliation_id,
        approved_by=current_user.id,
        approval_notes=approval_notes
    )
    return {
        "message": "Reconciliation approved successfully",
        "status": approved.status,
        "approved_at": approved.approved_at
    }


@router.get("/{reconciliation_id}/report")
async def generate_reconciliation_report(
    reconciliation_id: int,
    format: str = Query("pdf", description="Report format: pdf, excel, csv"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Generate bank reconciliation report."""
    reconciliation = bank_service.get_reconciliation(db, reconciliation_id=reconciliation_id)
    if not reconciliation:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Reconciliation {reconciliation_id} not found"
        )
    
    report_data = bank_service.generate_reconciliation_report(
        db, reconciliation_id=reconciliation_id, format=format
    )
    
    return {
        "report_url": report_data["url"],
        "format": format,
        "generated_at": datetime.now()
    }