"""General Ledger Postings API endpoints"""

from fastapi import APIRouter, Depends, HTTPException, status, Query
from sqlalchemy.orm import Session
from typing import List, Optional
from datetime import date, datetime
from decimal import Decimal

from app.api import deps
from app.services.gl import journal_entry as posting_service
from app.schemas.gl import (
    GLPosting, GLPostingCreate, TrialBalance,
    AccountBalance, PostingStatus
)

router = APIRouter()


@router.get("", response_model=List[GLPosting])
async def list_postings(
    account_number: Optional[str] = Query(None, description="Filter by account number"),
    start_date: Optional[date] = Query(None, description="Start date"),
    end_date: Optional[date] = Query(None, description="End date"),
    status: Optional[PostingStatus] = Query(None, description="Filter by posting status"),
    source_module: Optional[str] = Query(None, description="Filter by source module"),
    reference: Optional[str] = Query(None, description="Search by reference"),
    skip: int = 0,
    limit: int = 100,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    List GL postings with optional filters.
    
    Returns individual debit/credit postings to GL accounts.
    """
    postings = posting_service.get_gl_postings(
        db,
        account_number=account_number,
        start_date=start_date,
        end_date=end_date,
        status=status,
        source_module=source_module,
        reference=reference,
        skip=skip,
        limit=limit
    )
    return postings


@router.get("/{posting_id}", response_model=GLPosting)
async def get_posting(
    posting_id: int,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Get specific GL posting by ID."""
    posting = posting_service.get_gl_posting(db, posting_id=posting_id)
    if not posting:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Posting {posting_id} not found"
        )
    return posting


@router.post("", response_model=GLPosting, status_code=status.HTTP_201_CREATED)
async def create_posting(
    posting_in: GLPostingCreate,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Create a direct GL posting.
    
    Note: Most postings are created automatically through journal entries.
    This endpoint is for direct GL adjustments only.
    """
    # Validate account exists and allows manual entry
    account = posting_service.validate_account_for_posting(
        db,
        account_number=posting_in.account_number
    )
    
    if not account:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Account {posting_in.account_number} not found"
        )
    
    if not account.allow_manual_entry:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Account {posting_in.account_number} does not allow manual entries"
        )
    
    # Validate either debit or credit amount is provided
    if not posting_in.debit_amount and not posting_in.credit_amount:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Either debit_amount or credit_amount must be provided"
        )
    
    if posting_in.debit_amount and posting_in.credit_amount:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Cannot have both debit and credit amounts"
        )
    
    posting = posting_service.create_gl_posting(
        db,
        posting_in=posting_in,
        created_by=current_user.id
    )
    return posting


@router.post("/reverse/{posting_id}", response_model=GLPosting)
async def reverse_posting(
    posting_id: int,
    reversal_date: Optional[date] = None,
    reversal_reason: str = Query(..., description="Reason for reversal"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Create a reversal posting for an existing GL posting.
    
    The reversal will have opposite debit/credit amounts.
    """
    original_posting = posting_service.get_gl_posting(db, posting_id=posting_id)
    if not original_posting:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Posting {posting_id} not found"
        )
    
    if original_posting.status == PostingStatus.REVERSED:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Posting has already been reversed"
        )
    
    reversal = posting_service.create_reversal_posting(
        db,
        original_posting_id=posting_id,
        reversal_date=reversal_date or date.today(),
        reversal_reason=reversal_reason,
        reversed_by=current_user.id
    )
    
    return reversal


@router.get("/account/{account_number}/activity")
async def get_account_activity(
    account_number: str,
    start_date: Optional[date] = None,
    end_date: Optional[date] = None,
    include_beginning_balance: bool = Query(True, description="Include beginning balance"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Get detailed activity for a specific GL account.
    
    Returns all postings with running balance calculation.
    """
    activity = posting_service.get_account_activity(
        db,
        account_number=account_number,
        start_date=start_date,
        end_date=end_date,
        include_beginning_balance=include_beginning_balance
    )
    
    if not activity:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Account {account_number} not found"
        )
    
    return activity


@router.get("/trial-balance", response_model=TrialBalance)
async def get_trial_balance(
    as_of_date: Optional[date] = Query(None, description="As of date (defaults to today)"),
    period_id: Optional[int] = Query(None, description="Specific period ID"),
    include_zero_balances: bool = Query(False, description="Include accounts with zero balance"),
    level_of_detail: int = Query(3, ge=1, le=9, description="Account level detail (1-9)"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Generate trial balance report.
    
    Shows all GL accounts with their debit/credit balances.
    """
    if not as_of_date and not period_id:
        as_of_date = date.today()
    
    trial_balance = posting_service.generate_trial_balance(
        db,
        as_of_date=as_of_date,
        period_id=period_id,
        include_zero_balances=include_zero_balances,
        level_of_detail=level_of_detail
    )
    
    return trial_balance


@router.get("/account-balances", response_model=List[AccountBalance])
async def get_all_account_balances(
    as_of_date: Optional[date] = Query(None, description="As of date"),
    account_type: Optional[str] = Query(None, description="Filter by account type"),
    only_active: bool = Query(True, description="Only active accounts"),
    non_zero_only: bool = Query(True, description="Only accounts with non-zero balance"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Get balances for multiple GL accounts."""
    balances = posting_service.get_all_account_balances(
        db,
        as_of_date=as_of_date or date.today(),
        account_type=account_type,
        only_active=only_active,
        non_zero_only=non_zero_only
    )
    
    return balances


@router.post("/verify-balance/{account_number}")
async def verify_account_balance(
    account_number: str,
    expected_balance: Decimal,
    as_of_date: Optional[date] = None,
    tolerance: Decimal = Query(Decimal("0.01"), description="Acceptable difference"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Verify if an account balance matches expected value.
    
    Useful for reconciliation and audit purposes.
    """
    actual_balance = posting_service.get_account_balance_amount(
        db,
        account_number=account_number,
        as_of_date=as_of_date or date.today()
    )
    
    if actual_balance is None:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Account {account_number} not found"
        )
    
    difference = abs(actual_balance - expected_balance)
    is_balanced = difference <= tolerance
    
    return {
        "account_number": account_number,
        "expected_balance": expected_balance,
        "actual_balance": actual_balance,
        "difference": difference,
        "is_balanced": is_balanced,
        "as_of_date": as_of_date or date.today()
    }


@router.get("/unbalanced-entries")
async def find_unbalanced_entries(
    start_date: Optional[date] = None,
    end_date: Optional[date] = None,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Find journal entries that are not balanced.
    
    Admin only - used for data integrity checks.
    """
    unbalanced = posting_service.find_unbalanced_entries(
        db,
        start_date=start_date,
        end_date=end_date
    )
    
    if not unbalanced:
        return {
            "message": "All entries are balanced",
            "count": 0,
            "entries": []
        }
    
    return {
        "message": f"Found {len(unbalanced)} unbalanced entries",
        "count": len(unbalanced),
        "entries": unbalanced
    }


@router.post("/reconcile")
async def reconcile_accounts(
    account_numbers: List[str],
    as_of_date: Optional[date] = None,
    auto_create_adjustments: bool = Query(False, description="Auto-create adjustment entries"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Reconcile multiple GL accounts.
    
    Checks for imbalances and optionally creates adjustments.
    """
    reconciliation_date = as_of_date or date.today()
    results = []
    
    for account_number in account_numbers:
        result = posting_service.reconcile_account(
            db,
            account_number=account_number,
            as_of_date=reconciliation_date,
            auto_create_adjustments=auto_create_adjustments,
            user_id=current_user.id
        )
        results.append(result)
    
    return {
        "reconciliation_date": reconciliation_date,
        "accounts_processed": len(account_numbers),
        "results": results
    }


@router.get("/posting-summary")
async def get_posting_summary(
    period_id: Optional[int] = None,
    start_date: Optional[date] = None,
    end_date: Optional[date] = None,
    group_by: str = Query("source_module", description="Group by: source_module, account_type, daily"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Get summary of GL postings.
    
    Useful for high-level analysis and dashboards.
    """
    if not period_id and not start_date:
        # Default to current month
        today = date.today()
        start_date = date(today.year, today.month, 1)
        end_date = today
    
    summary = posting_service.get_posting_summary(
        db,
        period_id=period_id,
        start_date=start_date,
        end_date=end_date,
        group_by=group_by
    )
    
    return summary


@router.post("/batch-import")
async def import_postings_batch(
    postings: List[GLPostingCreate],
    validate_only: bool = Query(False, description="Only validate without creating"),
    force_balance: bool = Query(False, description="Force balance by creating adjustment"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Import multiple GL postings in batch.
    
    Admin only - validates and optionally forces balance.
    """
    # Calculate totals
    total_debits = sum(p.debit_amount or 0 for p in postings)
    total_credits = sum(p.credit_amount or 0 for p in postings)
    
    if total_debits != total_credits:
        if not force_balance:
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail=f"Batch is not balanced. Debits: {total_debits}, Credits: {total_credits}"
            )
    
    if validate_only:
        validation_results = []
        for idx, posting in enumerate(postings):
            try:
                # Validate account
                account = posting_service.validate_account_for_posting(
                    db,
                    account_number=posting.account_number
                )
                if not account:
                    validation_results.append({
                        "index": idx,
                        "valid": False,
                        "error": f"Account {posting.account_number} not found"
                    })
                elif not account.allow_manual_entry:
                    validation_results.append({
                        "index": idx,
                        "valid": False,
                        "error": f"Account {posting.account_number} does not allow manual entries"
                    })
                else:
                    validation_results.append({
                        "index": idx,
                        "valid": True
                    })
            except Exception as e:
                validation_results.append({
                    "index": idx,
                    "valid": False,
                    "error": str(e)
                })
        
        return {
            "validate_only": True,
            "total_postings": len(postings),
            "valid_postings": sum(1 for r in validation_results if r["valid"]),
            "invalid_postings": sum(1 for r in validation_results if not r["valid"]),
            "is_balanced": total_debits == total_credits,
            "validation_results": validation_results
        }
    
    # Create postings
    created_postings = []
    errors = []
    
    for idx, posting in enumerate(postings):
        try:
            new_posting = posting_service.create_gl_posting(
                db,
                posting_in=posting,
                created_by=current_user.id
            )
            created_postings.append(new_posting)
        except Exception as e:
            errors.append({
                "index": idx,
                "error": str(e)
            })
    
    # Force balance if needed
    if force_balance and total_debits != total_credits:
        difference = total_debits - total_credits
        adjustment_account = "9999"  # Suspense account
        
        adjustment_posting = GLPostingCreate(
            posting_date=date.today(),
            account_number=adjustment_account,
            debit_amount=difference if difference < 0 else None,
            credit_amount=difference if difference > 0 else None,
            description="Automatic balancing adjustment",
            reference="BATCH_BALANCE",
            source_module="GL",
            source_id=0
        )
        
        try:
            adjustment = posting_service.create_gl_posting(
                db,
                posting_in=adjustment_posting,
                created_by=current_user.id
            )
            created_postings.append(adjustment)
        except Exception as e:
            errors.append({
                "index": -1,
                "error": f"Failed to create balancing adjustment: {str(e)}"
            })
    
    return {
        "total_submitted": len(postings),
        "successfully_created": len(created_postings),
        "errors": errors,
        "forced_balance": force_balance,
        "posting_ids": [p.id for p in created_postings]
    }