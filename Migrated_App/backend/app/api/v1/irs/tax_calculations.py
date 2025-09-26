"""Tax Calculations API endpoints for IRS module"""

from fastapi import APIRouter, Depends, HTTPException, status, Query
from sqlalchemy.orm import Session
from typing import List, Optional, Dict, Any
from datetime import date
from decimal import Decimal

from app.api import deps
from app.services.irs import tax_calculations as tax_service
from app.schemas.irs import (
    TaxCalculationRequest, TaxCalculationResponse, TaxBracket, TaxDeduction, 
    TaxCredit, EstimatedTax, QuarterlyPayment, TaxLiability
)

router = APIRouter()


@router.post("/calculate", response_model=TaxCalculationResponse)
async def calculate_taxes(
    calculation_request: TaxCalculationRequest,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Calculate taxes based on income, deductions, and credits.
    
    Supports both individual and business tax calculations.
    """
    try:
        calculation_result = tax_service.calculate_taxes(
            db,
            company_id=calculation_request.company_id,
            tax_year=calculation_request.tax_year,
            income_data=calculation_request.income_data,
            deductions=calculation_request.deductions,
            credits=calculation_request.credits,
            filing_status=calculation_request.filing_status,
            entity_type=calculation_request.entity_type
        )
        
        # Store calculation for audit trail
        tax_service.store_calculation(
            db,
            calculation_request=calculation_request,
            calculation_result=calculation_result,
            calculated_by=current_user.id
        )
        
        return calculation_result
    except ValueError as e:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=str(e)
        )


@router.get("/brackets/{tax_year}", response_model=List[TaxBracket])
async def get_tax_brackets(
    tax_year: int,
    filing_status: str = Query(..., description="Filing status: single, married_joint, married_separate, head_of_household"),
    entity_type: str = Query("individual", description="Entity type: individual, corporation, partnership"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Get tax brackets for a specific year and filing status."""
    brackets = tax_service.get_tax_brackets(
        db,
        tax_year=tax_year,
        filing_status=filing_status,
        entity_type=entity_type
    )
    
    if not brackets:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Tax brackets not found for {tax_year} {filing_status}"
        )
    
    return brackets


@router.get("/deductions/{tax_year}", response_model=List[TaxDeduction])
async def get_standard_deductions(
    tax_year: int,
    filing_status: str = Query(..., description="Filing status"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Get standard deductions for a tax year."""
    deductions = tax_service.get_standard_deductions(
        db,
        tax_year=tax_year,
        filing_status=filing_status
    )
    return deductions


@router.post("/deductions/validate", response_model=Dict[str, Any])
async def validate_deductions(
    deductions: List[TaxDeduction],
    tax_year: int = Query(..., description="Tax year"),
    entity_type: str = Query("individual", description="Entity type"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Validate deductions for compliance and limits.
    """
    validation_result = tax_service.validate_deductions(
        db,
        deductions=deductions,
        tax_year=tax_year,
        entity_type=entity_type
    )
    
    return {
        "is_valid": validation_result["is_valid"],
        "total_deductions": validation_result["total_deductions"],
        "warnings": validation_result["warnings"],
        "errors": validation_result["errors"]
    }


@router.get("/credits/{tax_year}", response_model=List[TaxCredit])
async def get_available_credits(
    tax_year: int,
    entity_type: str = Query("individual", description="Entity type"),
    income_level: Optional[Decimal] = Query(None, description="Income level for eligibility check"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Get available tax credits for a tax year."""
    credits = tax_service.get_available_credits(
        db,
        tax_year=tax_year,
        entity_type=entity_type,
        income_level=income_level
    )
    return credits


@router.post("/credits/calculate", response_model=Dict[str, Decimal])
async def calculate_credits(
    credits: List[TaxCredit],
    taxpayer_info: Dict[str, Any],
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Calculate eligible credit amounts based on taxpayer information.
    """
    credit_calculations = tax_service.calculate_credits(
        db,
        credits=credits,
        taxpayer_info=taxpayer_info
    )
    return credit_calculations


@router.post("/estimated-tax", response_model=EstimatedTax)
async def calculate_estimated_tax(
    company_id: int,
    tax_year: int,
    projected_income: Decimal,
    projected_deductions: Optional[Decimal] = None,
    prior_year_tax: Optional[Decimal] = None,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Calculate estimated tax payments for the year.
    """
    estimated_tax = tax_service.calculate_estimated_tax(
        db,
        company_id=company_id,
        tax_year=tax_year,
        projected_income=projected_income,
        projected_deductions=projected_deductions,
        prior_year_tax=prior_year_tax
    )
    return estimated_tax


@router.get("/quarterly-payments/{company_id}", response_model=List[QuarterlyPayment])
async def get_quarterly_payments(
    company_id: int,
    tax_year: int = Query(..., description="Tax year"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Get quarterly estimated tax payment schedule."""
    payments = tax_service.get_quarterly_payments(
        db,
        company_id=company_id,
        tax_year=tax_year
    )
    return payments


@router.post("/quarterly-payments", response_model=QuarterlyPayment)
async def record_quarterly_payment(
    payment_data: QuarterlyPayment,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Record a quarterly estimated tax payment."""
    payment = tax_service.record_quarterly_payment(
        db,
        payment_data=payment_data,
        recorded_by=current_user.id
    )
    return payment


@router.get("/liability/{company_id}", response_model=TaxLiability)
async def get_tax_liability(
    company_id: int,
    tax_year: int = Query(..., description="Tax year"),
    as_of_date: Optional[date] = Query(None, description="Calculate liability as of specific date"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Get current tax liability for a company.
    """
    liability = tax_service.get_tax_liability(
        db,
        company_id=company_id,
        tax_year=tax_year,
        as_of_date=as_of_date
    )
    
    if not liability:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"No tax liability found for company {company_id} in {tax_year}"
        )
    
    return liability


@router.post("/withholding/calculate")
async def calculate_withholding(
    gross_income: Decimal,
    pay_period: str = Query(..., description="Pay period: weekly, biweekly, semimonthly, monthly"),
    allowances: int = Query(0, description="Number of allowances"),
    additional_withholding: Decimal = Query(0, description="Additional withholding amount"),
    filing_status: str = Query("single", description="Filing status"),
    tax_year: Optional[int] = None,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Calculate federal tax withholding from paycheck.
    """
    if not tax_year:
        tax_year = date.today().year
    
    withholding = tax_service.calculate_withholding(
        db,
        gross_income=gross_income,
        pay_period=pay_period,
        allowances=allowances,
        additional_withholding=additional_withholding,
        filing_status=filing_status,
        tax_year=tax_year
    )
    
    return {
        "gross_income": gross_income,
        "federal_withholding": withholding["federal_withholding"],
        "social_security": withholding["social_security"],
        "medicare": withholding["medicare"],
        "additional_medicare": withholding["additional_medicare"],
        "total_withholding": withholding["total_withholding"],
        "net_income": gross_income - withholding["total_withholding"]
    }


@router.get("/effective-rate/{company_id}")
async def calculate_effective_tax_rate(
    company_id: int,
    tax_year: int = Query(..., description="Tax year"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Calculate effective tax rate for a company."""
    rate_info = tax_service.calculate_effective_rate(
        db,
        company_id=company_id,
        tax_year=tax_year
    )
    
    return {
        "company_id": company_id,
        "tax_year": tax_year,
        "gross_income": rate_info["gross_income"],
        "taxable_income": rate_info["taxable_income"],
        "total_tax": rate_info["total_tax"],
        "effective_rate": rate_info["effective_rate"],
        "marginal_rate": rate_info["marginal_rate"]
    }


@router.post("/tax-planning/scenarios")
async def analyze_tax_scenarios(
    company_id: int,
    scenarios: List[Dict[str, Any]],
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Analyze different tax planning scenarios.
    
    Compare tax outcomes under different strategies.
    """
    analysis = tax_service.analyze_tax_scenarios(
        db,
        company_id=company_id,
        scenarios=scenarios
    )
    
    return {
        "scenarios": analysis["scenarios"],
        "recommendations": analysis["recommendations"],
        "potential_savings": analysis["potential_savings"]
    }


@router.get("/history/{company_id}")
async def get_tax_calculation_history(
    company_id: int,
    tax_year: Optional[int] = None,
    skip: int = 0,
    limit: int = 100,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Get history of tax calculations for a company."""
    history = tax_service.get_calculation_history(
        db,
        company_id=company_id,
        tax_year=tax_year,
        skip=skip,
        limit=limit
    )
    
    return {
        "total": len(history),
        "calculations": history
    }