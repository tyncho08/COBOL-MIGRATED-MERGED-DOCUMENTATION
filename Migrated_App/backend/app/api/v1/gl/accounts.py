"""
General Ledger Accounts API endpoints
"""

from typing import List, Optional
from fastapi import APIRouter, Depends, HTTPException, Query
from sqlalchemy.orm import Session

from app.api import deps
from app.models.gl import GLAccount
from app.schemas.gl import (
    GLAccountCreate,
    GLAccountUpdate,
    GLAccountResponse,
    GLAccountListResponse
)
from app.services.gl.chart_of_accounts import ChartOfAccountsService
from app.core.security import get_current_active_user
from app.models.auth import User

router = APIRouter()


@router.get("/", response_model=GLAccountListResponse)
def list_accounts(
    db: Session = Depends(deps.get_db),
    skip: int = Query(0, ge=0),
    limit: int = Query(100, ge=1, le=1000),
    account_type: Optional[str] = None,
    search: Optional[str] = None,
    active_only: bool = True,
    current_user: User = Depends(get_current_active_user)
) -> GLAccountListResponse:
    """
    Retrieve list of GL accounts with optional filtering.
    """
    service = ChartOfAccountsService(db)
    
    filters = {}
    if account_type:
        filters['account_type'] = account_type
    if active_only:
        filters['is_active'] = True
        
    accounts = service.search_accounts(
        search_term=search,
        filters=filters,
        skip=skip,
        limit=limit
    )
    
    total = service.count_accounts(filters)
    
    return GLAccountListResponse(
        accounts=accounts,
        total=total,
        skip=skip,
        limit=limit
    )


@router.post("/", response_model=GLAccountResponse)
def create_account(
    account: GLAccountCreate,
    db: Session = Depends(deps.get_db),
    current_user: User = Depends(get_current_active_user)
) -> GLAccountResponse:
    """
    Create a new GL account.
    """
    service = ChartOfAccountsService(db)
    
    # Check if account code already exists
    existing = service.get_account_by_code(account.account_code)
    if existing:
        raise HTTPException(
            status_code=400,
            detail=f"Account code {account.account_code} already exists"
        )
    
    try:
        new_account = service.create_account(
            account_data=account.dict(),
            user_id=current_user.id
        )
        return GLAccountResponse.from_orm(new_account)
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))


@router.get("/{account_id}", response_model=GLAccountResponse)
def get_account(
    account_id: int,
    db: Session = Depends(deps.get_db),
    current_user: User = Depends(get_current_active_user)
) -> GLAccountResponse:
    """
    Get a specific GL account by ID.
    """
    service = ChartOfAccountsService(db)
    account = service.get_account(account_id)
    
    if not account:
        raise HTTPException(status_code=404, detail="Account not found")
    
    return GLAccountResponse.from_orm(account)


@router.get("/code/{account_code}", response_model=GLAccountResponse)
def get_account_by_code(
    account_code: str,
    db: Session = Depends(deps.get_db),
    current_user: User = Depends(get_current_active_user)
) -> GLAccountResponse:
    """
    Get a specific GL account by code.
    """
    service = ChartOfAccountsService(db)
    account = service.get_account_by_code(account_code)
    
    if not account:
        raise HTTPException(status_code=404, detail="Account not found")
    
    return GLAccountResponse.from_orm(account)


@router.put("/{account_id}", response_model=GLAccountResponse)
def update_account(
    account_id: int,
    account_update: GLAccountUpdate,
    db: Session = Depends(deps.get_db),
    current_user: User = Depends(get_current_active_user)
) -> GLAccountResponse:
    """
    Update a GL account.
    """
    service = ChartOfAccountsService(db)
    
    try:
        updated_account = service.update_account(
            account_id=account_id,
            update_data=account_update.dict(exclude_unset=True),
            user_id=current_user.id
        )
        
        if not updated_account:
            raise HTTPException(status_code=404, detail="Account not found")
        
        return GLAccountResponse.from_orm(updated_account)
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))


@router.delete("/{account_id}")
def delete_account(
    account_id: int,
    db: Session = Depends(deps.get_db),
    current_user: User = Depends(get_current_active_user)
):
    """
    Deactivate a GL account (soft delete).
    """
    service = ChartOfAccountsService(db)
    
    # Check if account has transactions
    if service.has_transactions(account_id):
        raise HTTPException(
            status_code=400,
            detail="Cannot delete account with existing transactions"
        )
    
    success = service.deactivate_account(account_id, current_user.id)
    
    if not success:
        raise HTTPException(status_code=404, detail="Account not found")
    
    return {"message": "Account deactivated successfully"}


@router.get("/{account_id}/balance")
def get_account_balance(
    account_id: int,
    period_id: Optional[int] = None,
    as_of_date: Optional[str] = None,
    db: Session = Depends(deps.get_db),
    current_user: User = Depends(get_current_active_user)
):
    """
    Get account balance for a specific period or date.
    """
    service = ChartOfAccountsService(db)
    
    try:
        balance_info = service.get_account_balance(
            account_id=account_id,
            period_id=period_id,
            as_of_date=as_of_date
        )
        
        if balance_info is None:
            raise HTTPException(status_code=404, detail="Account not found")
        
        return balance_info
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))


@router.get("/{account_id}/transactions")
def get_account_transactions(
    account_id: int,
    start_date: Optional[str] = None,
    end_date: Optional[str] = None,
    skip: int = Query(0, ge=0),
    limit: int = Query(100, ge=1, le=1000),
    db: Session = Depends(deps.get_db),
    current_user: User = Depends(get_current_active_user)
):
    """
    Get transactions for a specific account.
    """
    service = ChartOfAccountsService(db)
    
    try:
        transactions = service.get_account_transactions(
            account_id=account_id,
            start_date=start_date,
            end_date=end_date,
            skip=skip,
            limit=limit
        )
        
        return transactions
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))


@router.post("/validate")
def validate_chart_of_accounts(
    db: Session = Depends(deps.get_db),
    current_user: User = Depends(get_current_active_user)
):
    """
    Validate the entire chart of accounts structure.
    """
    service = ChartOfAccountsService(db)
    
    validation_result = service.validate_chart_structure()
    
    if validation_result['is_valid']:
        return {
            "status": "valid",
            "message": "Chart of accounts is properly structured"
        }
    else:
        return {
            "status": "invalid",
            "errors": validation_result['errors'],
            "warnings": validation_result['warnings']
        }