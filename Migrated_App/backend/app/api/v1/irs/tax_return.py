"""Tax Return API endpoints for IRS module"""

from fastapi import APIRouter, Depends, HTTPException, status, Query, UploadFile, File
from fastapi.responses import FileResponse
from sqlalchemy.orm import Session
from typing import List, Optional, Dict, Any
from datetime import date, datetime
from decimal import Decimal

from app.api import deps
from app.services.irs import tax_return as return_service
from app.schemas.irs import (
    TaxReturn, TaxReturnCreate, TaxReturnUpdate,
    TaxReturnForm, TaxReturnSchedule, TaxReturnStatus,
    FilingConfirmation, TaxReturnValidation
)

router = APIRouter()


@router.post("", response_model=TaxReturn, status_code=status.HTTP_201_CREATED)
async def create_tax_return(
    return_in: TaxReturnCreate,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Create a new tax return for filing.
    
    This initializes the return with basic information.
    """
    # Check if return already exists for company/year
    existing = return_service.get_tax_return_by_year(
        db,
        company_id=return_in.company_id,
        tax_year=return_in.tax_year
    )
    if existing:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Tax return already exists for company {return_in.company_id} year {return_in.tax_year}"
        )
    
    tax_return = return_service.create_tax_return(
        db,
        return_in=return_in,
        created_by=current_user.id
    )
    return tax_return


@router.get("", response_model=List[TaxReturn])
async def list_tax_returns(
    company_id: Optional[int] = Query(None, description="Filter by company"),
    tax_year: Optional[int] = Query(None, description="Filter by tax year"),
    status: Optional[str] = Query(None, description="Filter by status"),
    filing_type: Optional[str] = Query(None, description="Filter by filing type"),
    skip: int = 0,
    limit: int = 100,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """List tax returns with optional filters."""
    returns = return_service.get_tax_returns(
        db,
        company_id=company_id,
        tax_year=tax_year,
        status=status,
        filing_type=filing_type,
        skip=skip,
        limit=limit
    )
    return returns


@router.get("/{return_id}", response_model=TaxReturn)
async def get_tax_return(
    return_id: int,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Get specific tax return by ID."""
    tax_return = return_service.get_tax_return(db, return_id=return_id)
    if not tax_return:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Tax return {return_id} not found"
        )
    return tax_return


@router.put("/{return_id}", response_model=TaxReturn)
async def update_tax_return(
    return_id: int,
    return_in: TaxReturnUpdate,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Update tax return information.
    
    Cannot update filed returns.
    """
    tax_return = return_service.get_tax_return(db, return_id=return_id)
    if not tax_return:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Tax return {return_id} not found"
        )
    
    if tax_return.status in ["filed", "accepted"]:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Cannot update filed tax return"
        )
    
    tax_return = return_service.update_tax_return(
        db,
        db_return=tax_return,
        return_in=return_in,
        updated_by=current_user.id
    )
    return tax_return


@router.post("/{return_id}/forms", response_model=TaxReturnForm)
async def add_tax_form(
    return_id: int,
    form_data: TaxReturnForm,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Add a tax form to the return.
    
    Common forms: 1040, 1065, 1120, W-2, 1099, etc.
    """
    tax_return = return_service.get_tax_return(db, return_id=return_id)
    if not tax_return:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Tax return {return_id} not found"
        )
    
    if tax_return.status in ["filed", "accepted"]:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Cannot add forms to filed tax return"
        )
    
    form = return_service.add_tax_form(
        db,
        return_id=return_id,
        form_data=form_data,
        added_by=current_user.id
    )
    return form


@router.get("/{return_id}/forms", response_model=List[TaxReturnForm])
async def get_tax_forms(
    return_id: int,
    form_type: Optional[str] = Query(None, description="Filter by form type"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Get all forms associated with a tax return."""
    forms = return_service.get_tax_forms(
        db,
        return_id=return_id,
        form_type=form_type
    )
    return forms


@router.post("/{return_id}/schedules", response_model=TaxReturnSchedule)
async def add_schedule(
    return_id: int,
    schedule_data: TaxReturnSchedule,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Add a schedule to the tax return.
    
    Common schedules: Schedule A (Itemized Deductions), Schedule C (Business Income), etc.
    """
    tax_return = return_service.get_tax_return(db, return_id=return_id)
    if not tax_return:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Tax return {return_id} not found"
        )
    
    schedule = return_service.add_schedule(
        db,
        return_id=return_id,
        schedule_data=schedule_data,
        added_by=current_user.id
    )
    return schedule


@router.post("/{return_id}/calculate")
async def calculate_tax_return(
    return_id: int,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Calculate all amounts on the tax return.
    
    This performs all tax calculations based on forms and schedules.
    """
    tax_return = return_service.get_tax_return(db, return_id=return_id)
    if not tax_return:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Tax return {return_id} not found"
        )
    
    calculation_result = return_service.calculate_tax_return(
        db,
        return_id=return_id
    )
    
    return {
        "return_id": return_id,
        "gross_income": calculation_result["gross_income"],
        "adjusted_gross_income": calculation_result["adjusted_gross_income"],
        "taxable_income": calculation_result["taxable_income"],
        "total_tax": calculation_result["total_tax"],
        "total_payments": calculation_result["total_payments"],
        "refund_amount": calculation_result["refund_amount"],
        "amount_due": calculation_result["amount_due"],
        "effective_rate": calculation_result["effective_rate"]
    }


@router.post("/{return_id}/validate", response_model=TaxReturnValidation)
async def validate_tax_return(
    return_id: int,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Validate tax return for completeness and accuracy.
    
    Checks for missing forms, calculation errors, and compliance issues.
    """
    validation_result = return_service.validate_tax_return(
        db,
        return_id=return_id
    )
    return validation_result


@router.post("/{return_id}/generate-pdf")
async def generate_tax_return_pdf(
    return_id: int,
    include_worksheets: bool = Query(False, description="Include calculation worksheets"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Generate PDF version of the tax return."""
    tax_return = return_service.get_tax_return(db, return_id=return_id)
    if not tax_return:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Tax return {return_id} not found"
        )
    
    pdf_path = return_service.generate_tax_return_pdf(
        db,
        return_id=return_id,
        include_worksheets=include_worksheets
    )
    
    return FileResponse(
        pdf_path,
        media_type="application/pdf",
        filename=f"tax_return_{tax_return.company_id}_{tax_return.tax_year}.pdf"
    )


@router.post("/{return_id}/review")
async def submit_for_review(
    return_id: int,
    reviewer_notes: Optional[str] = None,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Submit tax return for review before filing."""
    tax_return = return_service.get_tax_return(db, return_id=return_id)
    if not tax_return:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Tax return {return_id} not found"
        )
    
    # Validate before review
    validation = return_service.validate_tax_return(db, return_id=return_id)
    if not validation.is_valid:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Tax return has validation errors",
            headers={"errors": str(validation.errors)}
        )
    
    review_result = return_service.submit_for_review(
        db,
        return_id=return_id,
        submitted_by=current_user.id,
        reviewer_notes=reviewer_notes
    )
    
    return {
        "status": "under_review",
        "submitted_at": review_result["submitted_at"],
        "reviewer_assigned": review_result["reviewer_assigned"]
    }


@router.post("/{return_id}/approve")
async def approve_tax_return(
    return_id: int,
    approval_notes: Optional[str] = None,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Approve tax return for filing.
    
    Only authorized users can approve returns.
    """
    tax_return = return_service.get_tax_return(db, return_id=return_id)
    if not tax_return:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Tax return {return_id} not found"
        )
    
    if tax_return.status != "under_review":
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Only returns under review can be approved"
        )
    
    approval_result = return_service.approve_tax_return(
        db,
        return_id=return_id,
        approved_by=current_user.id,
        approval_notes=approval_notes
    )
    
    return {
        "status": "approved",
        "approved_at": approval_result["approved_at"],
        "ready_for_filing": True
    }


@router.post("/{return_id}/file", response_model=FilingConfirmation)
async def file_tax_return(
    return_id: int,
    filing_method: str = Query(..., description="Filing method: e-file, paper"),
    test_mode: bool = Query(False, description="File in test mode"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    File the tax return with IRS.
    
    Supports both e-filing and paper filing preparation.
    """
    tax_return = return_service.get_tax_return(db, return_id=return_id)
    if not tax_return:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Tax return {return_id} not found"
        )
    
    if tax_return.status != "approved":
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Tax return must be approved before filing"
        )
    
    try:
        filing_result = return_service.file_tax_return(
            db,
            return_id=return_id,
            filing_method=filing_method,
            test_mode=test_mode,
            filed_by=current_user.id
        )
        return filing_result
    except Exception as e:
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"Filing failed: {str(e)}"
        )


@router.get("/{return_id}/status", response_model=TaxReturnStatus)
async def get_filing_status(
    return_id: int,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Get current status of tax return filing."""
    status = return_service.get_filing_status(db, return_id=return_id)
    if not status:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Status not found for return {return_id}"
        )
    return status


@router.post("/{return_id}/amend")
async def create_amended_return(
    return_id: int,
    amendment_reason: str,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Create an amended tax return."""
    original_return = return_service.get_tax_return(db, return_id=return_id)
    if not original_return:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Tax return {return_id} not found"
        )
    
    if original_return.status not in ["filed", "accepted"]:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Can only amend filed returns"
        )
    
    amended_return = return_service.create_amended_return(
        db,
        original_return_id=return_id,
        amendment_reason=amendment_reason,
        created_by=current_user.id
    )
    
    return {
        "amended_return_id": amended_return.id,
        "original_return_id": return_id,
        "status": "draft",
        "message": "Amended return created successfully"
    }


@router.get("/{return_id}/documents")
async def get_supporting_documents(
    return_id: int,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Get list of supporting documents for a tax return."""
    documents = return_service.get_supporting_documents(db, return_id=return_id)
    return {
        "return_id": return_id,
        "total_documents": len(documents),
        "documents": documents
    }


@router.post("/{return_id}/documents")
async def upload_supporting_document(
    return_id: int,
    document_type: str = Query(..., description="Document type: W2, 1099, receipt, etc."),
    description: Optional[str] = None,
    file: UploadFile = File(...),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Upload a supporting document for the tax return."""
    tax_return = return_service.get_tax_return(db, return_id=return_id)
    if not tax_return:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Tax return {return_id} not found"
        )
    
    # Save document
    document = return_service.save_supporting_document(
        db,
        return_id=return_id,
        document_type=document_type,
        file_content=await file.read(),
        file_name=file.filename,
        description=description,
        uploaded_by=current_user.id
    )
    
    return {
        "document_id": document.id,
        "filename": document.filename,
        "document_type": document_type,
        "uploaded_at": document.uploaded_at
    }