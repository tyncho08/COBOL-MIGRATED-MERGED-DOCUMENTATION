"""Company Configuration API endpoints for IRS module"""

from fastapi import APIRouter, Depends, HTTPException, status
from sqlalchemy.orm import Session
from typing import List, Optional
from datetime import datetime

from app.api import deps
from app.services.irs import company_config as company_service
from app.schemas.irs import CompanyConfig, CompanyConfigCreate, CompanyConfigUpdate

router = APIRouter()


@router.get("", response_model=List[CompanyConfig])
async def list_companies(
    skip: int = 0,
    limit: int = 100,
    active_only: bool = True,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Retrieve list of company configurations.
    
    - **skip**: Number of records to skip
    - **limit**: Maximum number of records to return
    - **active_only**: If true, only return active companies
    """
    companies = company_service.get_companies(db, skip=skip, limit=limit, active_only=active_only)
    return companies


@router.get("/{company_id}", response_model=CompanyConfig)
async def get_company(
    company_id: int,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Get specific company configuration by ID."""
    company = company_service.get_company(db, company_id=company_id)
    if not company:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Company with id {company_id} not found"
        )
    return company


@router.post("", response_model=CompanyConfig, status_code=status.HTTP_201_CREATED)
async def create_company(
    company_in: CompanyConfigCreate,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Create new company configuration.
    
    Only administrators can create new companies.
    """
    # Check if company with same EIN already exists
    existing = company_service.get_company_by_ein(db, ein=company_in.employer_id_number)
    if existing:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Company with EIN {company_in.employer_id_number} already exists"
        )
    
    company = company_service.create_company(db, company_in=company_in)
    return company


@router.put("/{company_id}", response_model=CompanyConfig)
async def update_company(
    company_id: int,
    company_in: CompanyConfigUpdate,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Update company configuration.
    
    Only administrators can update company configurations.
    """
    company = company_service.get_company(db, company_id=company_id)
    if not company:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Company with id {company_id} not found"
        )
    
    company = company_service.update_company(db, db_company=company, company_in=company_in)
    return company


@router.delete("/{company_id}")
async def delete_company(
    company_id: int,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Soft delete company configuration.
    
    Only administrators can delete companies.
    """
    company = company_service.get_company(db, company_id=company_id)
    if not company:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Company with id {company_id} not found"
        )
    
    company_service.delete_company(db, company_id=company_id)
    return {"message": f"Company {company_id} deleted successfully"}


@router.get("/{company_id}/tax-settings")
async def get_company_tax_settings(
    company_id: int,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Get tax settings for a specific company."""
    settings = company_service.get_tax_settings(db, company_id=company_id)
    if not settings:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Tax settings for company {company_id} not found"
        )
    return settings


@router.put("/{company_id}/tax-settings")
async def update_company_tax_settings(
    company_id: int,
    settings_data: dict,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """Update tax settings for a company."""
    company = company_service.get_company(db, company_id=company_id)
    if not company:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Company with id {company_id} not found"
        )
    
    updated_settings = company_service.update_tax_settings(
        db, company_id=company_id, settings_data=settings_data
    )
    return updated_settings


@router.get("/{company_id}/filing-requirements")
async def get_filing_requirements(
    company_id: int,
    tax_year: Optional[int] = None,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Get filing requirements for a company."""
    if not tax_year:
        tax_year = datetime.now().year
    
    requirements = company_service.get_filing_requirements(
        db, company_id=company_id, tax_year=tax_year
    )
    return requirements


@router.post("/{company_id}/validate")
async def validate_company_config(
    company_id: int,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Validate company configuration for completeness and accuracy."""
    validation_result = company_service.validate_company_config(db, company_id=company_id)
    
    if not validation_result["is_valid"]:
        return {
            "is_valid": False,
            "errors": validation_result["errors"],
            "warnings": validation_result["warnings"]
        }
    
    return {
        "is_valid": True,
        "message": "Company configuration is valid",
        "warnings": validation_result["warnings"]
    }


@router.post("/{company_id}/activate")
async def activate_company(
    company_id: int,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """Activate a company for use in the system."""
    company = company_service.get_company(db, company_id=company_id)
    if not company:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Company with id {company_id} not found"
        )
    
    # Validate before activation
    validation_result = company_service.validate_company_config(db, company_id=company_id)
    if not validation_result["is_valid"]:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Cannot activate company with invalid configuration",
            headers={"errors": str(validation_result["errors"])}
        )
    
    activated_company = company_service.activate_company(db, company_id=company_id)
    return {
        "message": f"Company {activated_company.company_name} activated successfully",
        "company_id": activated_company.id,
        "status": "active"
    }