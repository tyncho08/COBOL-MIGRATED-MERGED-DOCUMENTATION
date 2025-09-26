"""General Ledger Budget Management API endpoints"""

from fastapi import APIRouter, Depends, HTTPException, status, Query
from sqlalchemy.orm import Session
from typing import List, Optional, Dict, Any
from datetime import date
from decimal import Decimal

from app.api import deps
from app.services.gl import budget_actual as budget_service
from app.schemas.gl import (
    Budget, BudgetCreate, BudgetLineBase,
    AccountBalance
)

router = APIRouter()


@router.get("", response_model=List[Budget])
async def list_budgets(
    fiscal_year: Optional[int] = Query(None, description="Filter by fiscal year"),
    budget_type: Optional[str] = Query(None, description="Filter by type: annual, quarterly, monthly"),
    is_active: Optional[bool] = Query(True, description="Filter by active status"),
    skip: int = 0,
    limit: int = 100,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    List budgets with optional filters.
    """
    budgets = budget_service.get_budgets(
        db,
        fiscal_year=fiscal_year,
        budget_type=budget_type,
        is_active=is_active,
        skip=skip,
        limit=limit
    )
    return budgets


@router.get("/{budget_id}", response_model=Budget)
async def get_budget(
    budget_id: int,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Get specific budget by ID."""
    budget = budget_service.get_budget(db, budget_id=budget_id)
    if not budget:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Budget {budget_id} not found"
        )
    return budget


@router.post("", response_model=Budget, status_code=status.HTTP_201_CREATED)
async def create_budget(
    budget_in: BudgetCreate,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Create new budget.
    
    Requires admin privileges.
    """
    # Check if budget already exists for the year
    existing = budget_service.get_budget_by_name_and_year(
        db,
        budget_name=budget_in.budget_name,
        fiscal_year=budget_in.fiscal_year
    )
    if existing:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Budget '{budget_in.budget_name}' already exists for fiscal year {budget_in.fiscal_year}"
        )
    
    # Validate all account numbers exist
    invalid_accounts = budget_service.validate_budget_accounts(
        db,
        account_numbers=[line.account_number for line in budget_in.lines]
    )
    if invalid_accounts:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Invalid account numbers: {', '.join(invalid_accounts)}"
        )
    
    budget = budget_service.create_budget(
        db,
        budget_in=budget_in,
        created_by=current_user.id
    )
    return budget


@router.put("/{budget_id}", response_model=Budget)
async def update_budget(
    budget_id: int,
    budget_update: Dict[str, Any],
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Update budget.
    
    Can update budget lines and metadata.
    """
    budget = budget_service.get_budget(db, budget_id=budget_id)
    if not budget:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Budget {budget_id} not found"
        )
    
    if budget.approved_at:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Cannot update approved budget. Create a revision instead."
        )
    
    updated_budget = budget_service.update_budget(
        db,
        budget_id=budget_id,
        budget_update=budget_update,
        updated_by=current_user.id
    )
    return updated_budget


@router.delete("/{budget_id}")
async def delete_budget(
    budget_id: int,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Delete budget.
    
    Can only delete non-approved budgets.
    """
    budget = budget_service.get_budget(db, budget_id=budget_id)
    if not budget:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Budget {budget_id} not found"
        )
    
    if budget.approved_at:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Cannot delete approved budget"
        )
    
    budget_service.delete_budget(db, budget_id=budget_id)
    return {"message": f"Budget {budget_id} deleted successfully"}


@router.post("/{budget_id}/approve")
async def approve_budget(
    budget_id: int,
    approval_notes: Optional[str] = None,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Approve budget for use.
    
    Only one budget per type/year can be active.
    """
    budget = budget_service.get_budget(db, budget_id=budget_id)
    if not budget:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Budget {budget_id} not found"
        )
    
    if budget.approved_at:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Budget is already approved"
        )
    
    approved = budget_service.approve_budget(
        db,
        budget_id=budget_id,
        approved_by=current_user.id,
        approval_notes=approval_notes
    )
    
    return {
        "budget_id": budget_id,
        "budget_name": approved.budget_name,
        "approved_at": approved.approved_at,
        "message": "Budget approved and activated successfully"
    }


@router.post("/{budget_id}/copy", response_model=Budget)
async def copy_budget(
    budget_id: int,
    new_name: str,
    new_fiscal_year: int,
    adjustment_percentage: float = Query(0, description="Percentage to adjust all amounts by"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Create a copy of existing budget.
    
    Useful for creating next year's budget based on current.
    """
    source_budget = budget_service.get_budget(db, budget_id=budget_id)
    if not source_budget:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Budget {budget_id} not found"
        )
    
    # Check if target already exists
    existing = budget_service.get_budget_by_name_and_year(
        db,
        budget_name=new_name,
        fiscal_year=new_fiscal_year
    )
    if existing:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Budget '{new_name}' already exists for fiscal year {new_fiscal_year}"
        )
    
    new_budget = budget_service.copy_budget(
        db,
        source_budget_id=budget_id,
        new_name=new_name,
        new_fiscal_year=new_fiscal_year,
        adjustment_percentage=adjustment_percentage,
        created_by=current_user.id
    )
    
    return new_budget


@router.get("/{budget_id}/comparison")
async def get_budget_vs_actual(
    budget_id: int,
    period_id: Optional[int] = Query(None, description="Specific period to compare"),
    as_of_date: Optional[date] = Query(None, description="Compare as of specific date"),
    variance_threshold: float = Query(10.0, description="Highlight variances above this percentage"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Get budget vs actual comparison report.
    """
    budget = budget_service.get_budget(db, budget_id=budget_id)
    if not budget:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Budget {budget_id} not found"
        )
    
    comparison = budget_service.get_budget_vs_actual(
        db,
        budget_id=budget_id,
        period_id=period_id,
        as_of_date=as_of_date or date.today()
    )
    
    # Add variance analysis
    for item in comparison["details"]:
        if item["budget_amount"] > 0:
            item["variance_percentage"] = (
                (item["actual_amount"] - item["budget_amount"]) / item["budget_amount"] * 100
            )
            item["over_threshold"] = abs(item["variance_percentage"]) > variance_threshold
        else:
            item["variance_percentage"] = 0
            item["over_threshold"] = item["actual_amount"] != 0
    
    return comparison


@router.get("/{budget_id}/line/{account_number}")
async def get_budget_line(
    budget_id: int,
    account_number: str,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Get budget details for specific account."""
    budget = budget_service.get_budget(db, budget_id=budget_id)
    if not budget:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Budget {budget_id} not found"
        )
    
    budget_line = budget_service.get_budget_line(
        db,
        budget_id=budget_id,
        account_number=account_number
    )
    
    if not budget_line:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"No budget found for account {account_number}"
        )
    
    return budget_line


@router.post("/{budget_id}/lines", response_model=Budget)
async def update_budget_lines(
    budget_id: int,
    lines: List[BudgetLineBase],
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Update multiple budget lines at once.
    """
    budget = budget_service.get_budget(db, budget_id=budget_id)
    if not budget:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Budget {budget_id} not found"
        )
    
    if budget.approved_at:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Cannot update lines of approved budget"
        )
    
    # Validate accounts
    invalid_accounts = budget_service.validate_budget_accounts(
        db,
        account_numbers=[line.account_number for line in lines]
    )
    if invalid_accounts:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Invalid account numbers: {', '.join(invalid_accounts)}"
        )
    
    updated_budget = budget_service.update_budget_lines(
        db,
        budget_id=budget_id,
        lines=lines,
        updated_by=current_user.id
    )
    
    return updated_budget


@router.get("/variance-report")
async def get_variance_report(
    fiscal_year: int = Query(..., description="Fiscal year"),
    period_id: Optional[int] = Query(None, description="Specific period"),
    department: Optional[str] = Query(None, description="Filter by department"),
    variance_type: str = Query("all", description="Type: all, favorable, unfavorable"),
    min_variance_amount: Decimal = Query(0, description="Minimum variance amount to show"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Get comprehensive variance report across all budgeted accounts.
    """
    # Get active budget for the year
    active_budget = budget_service.get_active_budget_for_year(
        db,
        fiscal_year=fiscal_year
    )
    
    if not active_budget:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"No active budget found for fiscal year {fiscal_year}"
        )
    
    variance_report = budget_service.generate_variance_report(
        db,
        budget_id=active_budget.id,
        period_id=period_id,
        department=department,
        variance_type=variance_type,
        min_variance_amount=min_variance_amount
    )
    
    return variance_report


@router.post("/upload")
async def upload_budget_from_file(
    budget_name: str,
    fiscal_year: int,
    file_format: str = Query("csv", description="File format: csv, excel"),
    file_content: str = Query(..., description="Base64 encoded file content"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Upload budget from CSV or Excel file.
    
    Expected columns: account_number, period_1 through period_12/13, annual_total
    """
    try:
        result = budget_service.import_budget_from_file(
            db,
            budget_name=budget_name,
            fiscal_year=fiscal_year,
            file_format=file_format,
            file_content=file_content,
            created_by=current_user.id
        )
        
        return {
            "budget_id": result["budget_id"],
            "lines_imported": result["lines_imported"],
            "total_amount": result["total_amount"],
            "warnings": result["warnings"]
        }
    except ValueError as e:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=str(e)
        )


@router.get("/templates")
async def get_budget_templates(
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Get available budget templates.
    """
    return {
        "templates": [
            {
                "id": "zero_based",
                "name": "Zero-Based Budget",
                "description": "Start with zero amounts for all accounts"
            },
            {
                "id": "prior_year",
                "name": "Prior Year Based",
                "description": "Copy from previous year with optional adjustment"
            },
            {
                "id": "actual_based",
                "name": "Actual Based",
                "description": "Based on actual spending from previous periods"
            },
            {
                "id": "incremental",
                "name": "Incremental Budget",
                "description": "Adjust existing budget by percentage"
            }
        ]
    }


@router.post("/create-from-template")
async def create_budget_from_template(
    template_id: str,
    budget_name: str,
    fiscal_year: int,
    template_params: Dict[str, Any],
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Create budget using a template.
    
    Template parameters vary by template type.
    """
    try:
        budget = budget_service.create_budget_from_template(
            db,
            template_id=template_id,
            budget_name=budget_name,
            fiscal_year=fiscal_year,
            template_params=template_params,
            created_by=current_user.id
        )
        
        return {
            "budget_id": budget.id,
            "budget_name": budget.budget_name,
            "total_amount": budget.total_amount,
            "message": f"Budget created successfully using {template_id} template"
        }
    except ValueError as e:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=str(e)
        )


@router.get("/forecast/{budget_id}")
async def get_budget_forecast(
    budget_id: int,
    periods_ahead: int = Query(3, ge=1, le=12, description="Number of periods to forecast"),
    include_seasonality: bool = Query(True, description="Include seasonal adjustments"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Get budget forecast based on current trends.
    
    Uses actual data to project future periods.
    """
    budget = budget_service.get_budget(db, budget_id=budget_id)
    if not budget:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Budget {budget_id} not found"
        )
    
    forecast = budget_service.generate_budget_forecast(
        db,
        budget_id=budget_id,
        periods_ahead=periods_ahead,
        include_seasonality=include_seasonality
    )
    
    return forecast