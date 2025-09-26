"""
IRS Electronic Filing API endpoints
"""

from typing import List, Dict, Any, Optional
from fastapi import APIRouter, Depends, HTTPException, Query, Body
from sqlalchemy.orm import Session

from app.api import deps
from app.services.irs.electronic_filing_service import ElectronicFilingService
from app.core.security import get_current_active_user
from app.models.auth import User

router = APIRouter()


@router.post("/prepare")
def prepare_electronic_file(
    tax_return_id: int = Body(..., description="ID of the tax return to file"),
    preparer_info: Dict[str, Any] = Body(..., description="Preparer information including PTIN, EIN, name, firm"),
    db: Session = Depends(deps.get_db),
    current_user: User = Depends(get_current_active_user)
) -> Dict[str, Any]:
    """
    Prepare a tax return for electronic filing.
    """
    service = ElectronicFilingService(db)
    
    # Validate preparer info
    required_fields = ['ptin', 'ein', 'name']
    missing_fields = [field for field in required_fields if field not in preparer_info]
    
    if missing_fields:
        raise HTTPException(
            status_code=400,
            detail=f"Missing required preparer fields: {', '.join(missing_fields)}"
        )
    
    try:
        efile = service.prepare_electronic_file(
            tax_return_id=tax_return_id,
            preparer_info=preparer_info,
            user_id=current_user.id
        )
        
        return {
            "success": True,
            "efile_id": efile.id,
            "submission_id": efile.submission_id,
            "filing_status": efile.filing_status,
            "created_at": efile.created_at.isoformat()
        }
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/{efile_id}/validate")
def validate_electronic_file(
    efile_id: int,
    db: Session = Depends(deps.get_db),
    current_user: User = Depends(get_current_active_user)
) -> Dict[str, Any]:
    """
    Validate an electronic file before transmission.
    """
    service = ElectronicFilingService(db)
    
    try:
        is_valid, issues = service.validate_electronic_file(
            efile_id=efile_id,
            user_id=current_user.id
        )
        
        errors = [issue for issue in issues if issue.get('code', '').startswith(('PREP', 'COMP', 'TAX'))]
        warnings = [issue for issue in issues if issue.get('code', '').startswith(('SCHED', 'DEAD', 'PRIOR'))]
        
        return {
            "is_valid": is_valid,
            "errors": errors,
            "warnings": warnings,
            "error_count": len(errors),
            "warning_count": len(warnings)
        }
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/{efile_id}/transmit")
def transmit_electronic_file(
    efile_id: int,
    test_mode: bool = Query(True, description="Use test transmission mode"),
    db: Session = Depends(deps.get_db),
    current_user: User = Depends(get_current_active_user)
) -> Dict[str, Any]:
    """
    Transmit an electronic file to the IRS.
    """
    service = ElectronicFilingService(db)
    
    try:
        result = service.transmit_electronic_file(
            efile_id=efile_id,
            user_id=current_user.id,
            test_mode=test_mode
        )
        
        return {
            "success": result['status'] == 'ACCEPTED',
            "status": result['status'],
            "message": result.get('message', ''),
            "irs_submission_id": result.get('irs_submission_id'),
            "errors": result.get('errors', []),
            "test_mode": test_mode
        }
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/{efile_id}/status")
def check_transmission_status(
    efile_id: int,
    db: Session = Depends(deps.get_db),
    current_user: User = Depends(get_current_active_user)
) -> Dict[str, Any]:
    """
    Check the status of a transmitted file with the IRS.
    """
    service = ElectronicFilingService(db)
    
    try:
        status = service.check_transmission_status(
            efile_id=efile_id,
            user_id=current_user.id
        )
        
        return status
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/history/{company_id}")
def get_filing_history(
    company_id: int,
    tax_year: Optional[int] = Query(None, description="Filter by specific tax year"),
    db: Session = Depends(deps.get_db),
    current_user: User = Depends(get_current_active_user)
) -> List[Dict[str, Any]]:
    """
    Get electronic filing history for a company.
    """
    service = ElectronicFilingService(db)
    
    try:
        history = service.get_filing_history(
            company_id=company_id,
            tax_year=tax_year
        )
        
        return history
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/{efile_id}")
def get_electronic_file(
    efile_id: int,
    db: Session = Depends(deps.get_db),
    current_user: User = Depends(get_current_active_user)
) -> Dict[str, Any]:
    """
    Get details of a specific electronic file.
    """
    service = ElectronicFilingService(db)
    
    try:
        efile = service.db.get(service.db.query(
            service.db.models.irs.IrsElectronicFileRec
        ).filter_by(id=efile_id).first())
        
        if not efile:
            raise HTTPException(status_code=404, detail="Electronic file not found")
        
        return {
            "efile_id": efile.id,
            "tax_return_id": efile.tax_return_id,
            "submission_id": efile.submission_id,
            "filing_status": efile.filing_status,
            "transmission_status": efile.transmission_status,
            "preparer_ptin": efile.preparer_ptin,
            "preparer_name": efile.preparer_name,
            "preparer_firm": efile.preparer_firm,
            "created_at": efile.created_at.isoformat() if efile.created_at else None,
            "transmitted_at": efile.transmitted_at.isoformat() if efile.transmitted_at else None,
            "accepted_at": efile.accepted_at.isoformat() if efile.accepted_at else None,
            "validation_errors": efile.validation_errors,
            "rejection_codes": efile.rejection_codes
        }
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/{efile_id}/resubmit")
def resubmit_electronic_file(
    efile_id: int,
    corrections: Dict[str, Any] = Body(..., description="Corrections made to address rejection"),
    db: Session = Depends(deps.get_db),
    current_user: User = Depends(get_current_active_user)
) -> Dict[str, Any]:
    """
    Resubmit a rejected electronic file after corrections.
    """
    service = ElectronicFilingService(db)
    
    try:
        # Get the electronic file
        efile = service.db.get(service.db.models.irs.IrsElectronicFileRec, efile_id)
        
        if not efile:
            raise HTTPException(status_code=404, detail="Electronic file not found")
        
        if efile.filing_status != 'REJECTED':
            raise HTTPException(
                status_code=400,
                detail="Only rejected files can be resubmitted"
            )
        
        # Apply corrections to the associated tax return
        # (This would involve updating the tax return data based on corrections)
        
        # Re-validate
        is_valid, issues = service.validate_electronic_file(
            efile_id=efile_id,
            user_id=current_user.id
        )
        
        if not is_valid:
            return {
                "success": False,
                "message": "Validation failed after corrections",
                "errors": [issue for issue in issues if issue.get('code', '').startswith(('PREP', 'COMP', 'TAX'))]
            }
        
        # Re-transmit
        result = service.transmit_electronic_file(
            efile_id=efile_id,
            user_id=current_user.id,
            test_mode=False
        )
        
        return {
            "success": result['status'] == 'ACCEPTED',
            "status": result['status'],
            "message": "File resubmitted successfully" if result['status'] == 'ACCEPTED' else "Resubmission failed",
            "irs_submission_id": result.get('irs_submission_id')
        }
        
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))