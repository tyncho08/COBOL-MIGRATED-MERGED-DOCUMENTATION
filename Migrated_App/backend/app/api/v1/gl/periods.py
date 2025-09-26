"""General Ledger Period Management API endpoints"""

from fastapi import APIRouter, Depends, HTTPException, status, Query
from sqlalchemy.orm import Session
from typing import List, Optional
from datetime import date, datetime

from app.api import deps
from app.services.gl import period_close as period_service
from app.schemas.gl import (
    AccountingPeriod, AccountingPeriodCreate,
    TrialBalance, IncomeStatement, BalanceSheet
)

router = APIRouter()


@router.get("", response_model=List[AccountingPeriod])
async def list_periods(
    fiscal_year: Optional[int] = Query(None, description="Filter by fiscal year"),
    is_open: Optional[bool] = Query(None, description="Filter by open/closed status"),
    include_adjustment: bool = Query(True, description="Include adjustment periods"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    List accounting periods.
    
    Returns all periods or filtered by criteria.
    """
    periods = period_service.get_periods(
        db,
        fiscal_year=fiscal_year,
        is_open=is_open,
        include_adjustment=include_adjustment
    )
    return periods


@router.get("/current", response_model=AccountingPeriod)
async def get_current_period(
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Get the current open accounting period."""
    period = period_service.get_current_period(db)
    if not period:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail="No current period found. All periods may be closed."
        )
    return period


@router.get("/{period_id}", response_model=AccountingPeriod)
async def get_period(
    period_id: int,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Get specific accounting period by ID."""
    period = period_service.get_period(db, period_id=period_id)
    if not period:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Period {period_id} not found"
        )
    return period


@router.post("", response_model=AccountingPeriod, status_code=status.HTTP_201_CREATED)
async def create_period(
    period_in: AccountingPeriodCreate,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Create new accounting period.
    
    Requires admin privileges.
    """
    # Validate dates don't overlap with existing periods
    overlapping = period_service.check_period_overlap(
        db,
        start_date=period_in.start_date,
        end_date=period_in.end_date,
        fiscal_year=period_in.fiscal_year
    )
    
    if overlapping:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Period dates overlap with existing period: {overlapping.period_name}"
        )
    
    # Validate period number is sequential
    if not period_service.is_period_number_valid(
        db,
        fiscal_year=period_in.fiscal_year,
        period_number=period_in.period_number
    ):
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Invalid period number {period_in.period_number} for fiscal year {period_in.fiscal_year}"
        )
    
    period = period_service.create_period(
        db,
        period_in=period_in,
        created_by=current_user.id
    )
    return period


@router.post("/{period_id}/close")
async def close_period(
    period_id: int,
    force_close: bool = Query(False, description="Force close even with warnings"),
    create_closing_entries: bool = Query(True, description="Create automatic closing entries"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Close an accounting period.
    
    Performs validation checks and creates closing entries.
    """
    period = period_service.get_period(db, period_id=period_id)
    if not period:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Period {period_id} not found"
        )
    
    if not period.is_open:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Period {period.period_name} is already closed"
        )
    
    # Run pre-closing validation
    validation = period_service.validate_period_close(db, period_id=period_id)
    
    if not validation["can_close"] and not force_close:
        return {
            "can_close": False,
            "errors": validation["errors"],
            "warnings": validation["warnings"],
            "message": "Period cannot be closed due to errors. Use force_close=true to override."
        }
    
    # Close the period
    closing_result = period_service.close_period(
        db,
        period_id=period_id,
        closed_by=current_user.id,
        create_closing_entries=create_closing_entries,
        force=force_close
    )
    
    return {
        "period_id": period_id,
        "period_name": period.period_name,
        "closed_at": closing_result["closed_at"],
        "closing_entries_created": closing_result["closing_entries_created"],
        "retained_earnings_updated": closing_result["retained_earnings_updated"],
        "warnings": validation["warnings"] if force_close else []
    }


@router.post("/{period_id}/reopen")
async def reopen_period(
    period_id: int,
    reason: str = Query(..., description="Reason for reopening period"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Reopen a closed accounting period.
    
    Admin only - should be used sparingly.
    """
    period = period_service.get_period(db, period_id=period_id)
    if not period:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Period {period_id} not found"
        )
    
    if period.is_open:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Period {period.period_name} is already open"
        )
    
    # Check if we can reopen (no subsequent periods closed)
    if not period_service.can_reopen_period(db, period_id=period_id):
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Cannot reopen period - subsequent periods are already closed"
        )
    
    reopened = period_service.reopen_period(
        db,
        period_id=period_id,
        reopened_by=current_user.id,
        reason=reason
    )
    
    return {
        "period_id": period_id,
        "period_name": period.period_name,
        "reopened_at": datetime.now(),
        "reason": reason,
        "message": "Period reopened successfully. Closing entries have been reversed."
    }


@router.get("/{period_id}/trial-balance", response_model=TrialBalance)
async def get_period_trial_balance(
    period_id: int,
    level_of_detail: int = Query(3, ge=1, le=9, description="Account level detail"),
    include_zero_balances: bool = Query(False, description="Include zero balance accounts"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Get trial balance for a specific period."""
    period = period_service.get_period(db, period_id=period_id)
    if not period:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Period {period_id} not found"
        )
    
    trial_balance = period_service.generate_period_trial_balance(
        db,
        period_id=period_id,
        level_of_detail=level_of_detail,
        include_zero_balances=include_zero_balances
    )
    
    return trial_balance


@router.get("/{period_id}/income-statement", response_model=IncomeStatement)
async def get_period_income_statement(
    period_id: int,
    comparison_periods: int = Query(0, description="Number of comparison periods"),
    show_percentages: bool = Query(True, description="Show percentage of revenue"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Generate income statement for a specific period."""
    period = period_service.get_period(db, period_id=period_id)
    if not period:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Period {period_id} not found"
        )
    
    income_statement = period_service.generate_income_statement(
        db,
        period_id=period_id,
        comparison_periods=comparison_periods,
        show_percentages=show_percentages
    )
    
    return income_statement


@router.get("/{period_id}/balance-sheet", response_model=BalanceSheet)
async def get_period_balance_sheet(
    period_id: int,
    comparison_period_id: Optional[int] = None,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Generate balance sheet as of period end date."""
    period = period_service.get_period(db, period_id=period_id)
    if not period:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Period {period_id} not found"
        )
    
    balance_sheet = period_service.generate_balance_sheet(
        db,
        period_id=period_id,
        comparison_period_id=comparison_period_id
    )
    
    return balance_sheet


@router.post("/{period_id}/validate-close")
async def validate_period_close(
    period_id: int,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Validate if a period can be closed.
    
    Checks for unposted entries, unbalanced accounts, etc.
    """
    period = period_service.get_period(db, period_id=period_id)
    if not period:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Period {period_id} not found"
        )
    
    validation = period_service.validate_period_close(db, period_id=period_id)
    
    return {
        "period_id": period_id,
        "period_name": period.period_name,
        "can_close": validation["can_close"],
        "validation_date": datetime.now(),
        "checks_performed": validation["checks_performed"],
        "errors": validation["errors"],
        "warnings": validation["warnings"]
    }


@router.get("/{period_id}/activity-summary")
async def get_period_activity_summary(
    period_id: int,
    group_by: str = Query("account_type", description="Group by: account_type, source_module"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Get activity summary for the period."""
    period = period_service.get_period(db, period_id=period_id)
    if not period:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Period {period_id} not found"
        )
    
    summary = period_service.get_period_activity_summary(
        db,
        period_id=period_id,
        group_by=group_by
    )
    
    return {
        "period_id": period_id,
        "period_name": period.period_name,
        "start_date": period.start_date,
        "end_date": period.end_date,
        "total_debits": summary["total_debits"],
        "total_credits": summary["total_credits"],
        "transaction_count": summary["transaction_count"],
        "activity_by_group": summary["activity_by_group"]
    }


@router.post("/initialize-year/{fiscal_year}")
async def initialize_fiscal_year(
    fiscal_year: int,
    periods_per_year: int = Query(12, ge=12, le=13, description="Number of periods (12 or 13)"),
    first_period_start: date = Query(..., description="Start date of first period"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Initialize all periods for a fiscal year.
    
    Creates 12 or 13 periods automatically.
    """
    # Check if year already has periods
    existing = period_service.get_periods(db, fiscal_year=fiscal_year)
    if existing:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Fiscal year {fiscal_year} already has {len(existing)} periods"
        )
    
    created_periods = period_service.initialize_fiscal_year(
        db,
        fiscal_year=fiscal_year,
        periods_per_year=periods_per_year,
        first_period_start=first_period_start,
        created_by=current_user.id
    )
    
    return {
        "fiscal_year": fiscal_year,
        "periods_created": len(created_periods),
        "first_period": created_periods[0].period_name,
        "last_period": created_periods[-1].period_name,
        "message": f"Successfully created {len(created_periods)} periods for fiscal year {fiscal_year}"
    }


@router.post("/year-end-close/{fiscal_year}")
async def perform_year_end_close(
    fiscal_year: int,
    retained_earnings_account: str = Query(..., description="GL account for retained earnings"),
    create_opening_balances: bool = Query(True, description="Create opening balances for next year"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Perform year-end closing process.
    
    Closes all P&L accounts to retained earnings.
    """
    # Validate all periods are closed
    open_periods = period_service.get_open_periods_for_year(db, fiscal_year=fiscal_year)
    if open_periods:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Cannot perform year-end close - {len(open_periods)} periods are still open"
        )
    
    # Validate retained earnings account exists
    if not period_service.validate_account_exists(db, account_number=retained_earnings_account):
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Retained earnings account {retained_earnings_account} not found"
        )
    
    result = period_service.perform_year_end_close(
        db,
        fiscal_year=fiscal_year,
        retained_earnings_account=retained_earnings_account,
        create_opening_balances=create_opening_balances,
        performed_by=current_user.id
    )
    
    return {
        "fiscal_year": fiscal_year,
        "net_income": result["net_income"],
        "retained_earnings_updated": result["retained_earnings_updated"],
        "closing_entries_created": result["closing_entries_created"],
        "opening_balances_created": result["opening_balances_created"],
        "next_year_initialized": result["next_year_initialized"],
        "message": f"Year-end close completed successfully. Net income of {result['net_income']} transferred to retained earnings."
    }


@router.get("/closing-checklist/{period_id}")
async def get_closing_checklist(
    period_id: int,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Get period closing checklist with status of each item.
    """
    period = period_service.get_period(db, period_id=period_id)
    if not period:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Period {period_id} not found"
        )
    
    checklist = period_service.get_closing_checklist(db, period_id=period_id)
    
    return {
        "period_id": period_id,
        "period_name": period.period_name,
        "checklist": checklist,
        "completed_items": sum(1 for item in checklist if item["completed"]),
        "total_items": len(checklist),
        "ready_to_close": all(item["completed"] or not item["required"] for item in checklist)
    }