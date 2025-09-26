"""
IRS Audit Trail API endpoints
"""

from typing import List, Optional, Dict, Any
from datetime import datetime
from fastapi import APIRouter, Depends, HTTPException, Query
from sqlalchemy.orm import Session

from app.api import deps
from app.services.irs.audit_trail_service import AuditTrailService
from app.core.security import get_current_active_user
from app.models.auth import User

router = APIRouter()


@router.post("/log")
def log_audit_trail(
    entity_type: str = Query(..., description="Type of entity (e.g., transaction, tax_return, company_config)"),
    entity_id: str = Query(..., description="ID of the entity being audited"),
    action: str = Query(..., description="Action performed (CREATE, UPDATE, DELETE, VIEW, EXPORT)"),
    field_name: Optional[str] = Query(None, description="Name of the field changed"),
    old_value: Optional[str] = Query(None, description="Previous value"),
    new_value: Optional[str] = Query(None, description="New value"),
    description: Optional[str] = Query(None, description="Additional description"),
    db: Session = Depends(deps.get_db),
    current_user: User = Depends(get_current_active_user)
) -> Dict[str, Any]:
    """
    Log an audit trail entry.
    """
    service = AuditTrailService(db)
    
    # Get client IP from request context (would be injected by middleware)
    ip_address = "127.0.0.1"  # Placeholder - would get from request
    
    try:
        audit_record = service.log_change(
            user_id=current_user.id,
            entity_type=entity_type,
            entity_id=entity_id,
            action=action,
            field_name=field_name,
            old_value=old_value,
            new_value=new_value,
            description=description,
            ip_address=ip_address
        )
        
        return {
            "success": True,
            "audit_id": audit_record.id,
            "timestamp": audit_record.timestamp.isoformat()
        }
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/entity/{entity_type}/{entity_id}")
def get_entity_history(
    entity_type: str,
    entity_id: str,
    start_date: Optional[str] = Query(None, regex=r'^\d{4}-\d{2}-\d{2}$'),
    end_date: Optional[str] = Query(None, regex=r'^\d{4}-\d{2}-\d{2}$'),
    limit: int = Query(100, ge=1, le=1000),
    db: Session = Depends(deps.get_db),
    current_user: User = Depends(get_current_active_user)
) -> List[Dict[str, Any]]:
    """
    Get audit history for a specific entity.
    """
    service = AuditTrailService(db)
    
    start_dt = datetime.strptime(start_date, '%Y-%m-%d') if start_date else None
    end_dt = datetime.strptime(end_date, '%Y-%m-%d') if end_date else None
    
    try:
        history = service.get_entity_history(
            entity_type=entity_type,
            entity_id=entity_id,
            start_date=start_dt,
            end_date=end_dt,
            limit=limit
        )
        
        return history
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/user/{user_id}")
def get_user_activity(
    user_id: int,
    start_date: Optional[str] = Query(None, regex=r'^\d{4}-\d{2}-\d{2}$'),
    end_date: Optional[str] = Query(None, regex=r'^\d{4}-\d{2}-\d{2}$'),
    entity_type: Optional[str] = None,
    limit: int = Query(100, ge=1, le=1000),
    db: Session = Depends(deps.get_db),
    current_user: User = Depends(get_current_active_user)
) -> List[Dict[str, Any]]:
    """
    Get all audit trail records for a specific user.
    Admin only or user viewing their own activity.
    """
    # Check permissions - admin or self
    if current_user.id != user_id and not current_user.is_admin:
        raise HTTPException(
            status_code=403,
            detail="Not authorized to view other users' activity"
        )
    
    service = AuditTrailService(db)
    
    start_dt = datetime.strptime(start_date, '%Y-%m-%d') if start_date else None
    end_dt = datetime.strptime(end_date, '%Y-%m-%d') if end_date else None
    
    try:
        activity = service.get_user_activity(
            user_id=user_id,
            start_date=start_dt,
            end_date=end_dt,
            entity_type=entity_type,
            limit=limit
        )
        
        return activity
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/search")
def search_audit_trail(
    search_criteria: Dict[str, Any],
    start_date: Optional[str] = Query(None, regex=r'^\d{4}-\d{2}-\d{2}$'),
    end_date: Optional[str] = Query(None, regex=r'^\d{4}-\d{2}-\d{2}$'),
    limit: int = Query(100, ge=1, le=1000),
    db: Session = Depends(deps.get_db),
    current_user: User = Depends(get_current_active_user)
) -> List[Dict[str, Any]]:
    """
    Search audit trail records based on various criteria.
    Admin only.
    """
    if not current_user.is_admin:
        raise HTTPException(
            status_code=403,
            detail="Admin access required"
        )
    
    service = AuditTrailService(db)
    
    start_dt = datetime.strptime(start_date, '%Y-%m-%d') if start_date else None
    end_dt = datetime.strptime(end_date, '%Y-%m-%d') if end_date else None
    
    try:
        results = service.search_audit_trail(
            search_criteria=search_criteria,
            start_date=start_dt,
            end_date=end_dt,
            limit=limit
        )
        
        return results
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/reports/compliance")
def generate_compliance_report(
    start_date: str = Query(..., regex=r'^\d{4}-\d{2}-\d{2}$'),
    end_date: str = Query(..., regex=r'^\d{4}-\d{2}-\d{2}$'),
    report_type: str = Query('summary', regex='^(summary|detailed|user_activity|entity_changes)$'),
    db: Session = Depends(deps.get_db),
    current_user: User = Depends(get_current_active_user)
) -> Dict[str, Any]:
    """
    Generate compliance reports from audit trail data.
    Admin only.
    """
    if not current_user.is_admin:
        raise HTTPException(
            status_code=403,
            detail="Admin access required"
        )
    
    service = AuditTrailService(db)
    
    start_dt = datetime.strptime(start_date, '%Y-%m-%d')
    end_dt = datetime.strptime(end_date, '%Y-%m-%d')
    
    try:
        report = service.generate_compliance_report(
            start_date=start_dt,
            end_date=end_dt,
            report_type=report_type
        )
        
        return report
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/cleanup")
def cleanup_old_records(
    retention_days: int = Query(2555, ge=365, description="Number of days to retain (min 365)"),
    db: Session = Depends(deps.get_db),
    current_user: User = Depends(get_current_active_user)
) -> Dict[str, Any]:
    """
    Clean up old audit trail records based on retention policy.
    Admin only. Default retention is 7 years (2555 days) for IRS compliance.
    """
    if not current_user.is_admin:
        raise HTTPException(
            status_code=403,
            detail="Admin access required"
        )
    
    service = AuditTrailService(db)
    
    try:
        deleted_count = service.cleanup_old_records(retention_days)
        
        return {
            "success": True,
            "deleted_count": deleted_count,
            "retention_days": retention_days
        }
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))