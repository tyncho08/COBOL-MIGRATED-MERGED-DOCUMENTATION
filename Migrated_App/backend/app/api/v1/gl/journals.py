"""
General Ledger Journal Entries API endpoints
"""

from typing import List, Optional
from datetime import date, datetime
from fastapi import APIRouter, Depends, HTTPException, Query, Body
from sqlalchemy.orm import Session

from app.api import deps
from app.schemas.gl import (
    JournalEntryCreate,
    JournalEntryUpdate,
    JournalEntryResponse,
    JournalEntryListResponse,
    JournalLineCreate
)
from app.services.gl.journal_entry import JournalEntryService
from app.core.security import get_current_active_user
from app.models.auth import User

router = APIRouter()


@router.get("/", response_model=JournalEntryListResponse)
def list_journal_entries(
    db: Session = Depends(deps.get_db),
    skip: int = Query(0, ge=0),
    limit: int = Query(100, ge=1, le=1000),
    start_date: Optional[str] = None,
    end_date: Optional[str] = None,
    status: Optional[str] = None,
    journal_type: Optional[str] = None,
    current_user: User = Depends(get_current_active_user)
) -> JournalEntryListResponse:
    """
    Retrieve list of journal entries with optional filtering.
    """
    service = JournalEntryService(db)
    
    filters = {}
    if status:
        filters['status'] = status
    if journal_type:
        filters['journal_type'] = journal_type
    if start_date:
        filters['start_date'] = datetime.strptime(start_date, '%Y-%m-%d').date()
    if end_date:
        filters['end_date'] = datetime.strptime(end_date, '%Y-%m-%d').date()
    
    entries = service.list_journal_entries(
        filters=filters,
        skip=skip,
        limit=limit
    )
    
    total = service.count_journal_entries(filters)
    
    return JournalEntryListResponse(
        entries=entries,
        total=total,
        skip=skip,
        limit=limit
    )


@router.post("/", response_model=JournalEntryResponse)
def create_journal_entry(
    entry: JournalEntryCreate,
    db: Session = Depends(deps.get_db),
    current_user: User = Depends(get_current_active_user)
) -> JournalEntryResponse:
    """
    Create a new journal entry.
    """
    service = JournalEntryService(db)
    
    # Validate that debits equal credits
    total_debits = sum(line.debit_amount or 0 for line in entry.lines)
    total_credits = sum(line.credit_amount or 0 for line in entry.lines)
    
    if total_debits != total_credits:
        raise HTTPException(
            status_code=400,
            detail=f"Journal entry is not balanced. Debits: {total_debits}, Credits: {total_credits}"
        )
    
    try:
        new_entry = service.create_journal_entry(
            entry_data=entry.dict(),
            user_id=current_user.id
        )
        return JournalEntryResponse.from_orm(new_entry)
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))


@router.get("/{entry_id}", response_model=JournalEntryResponse)
def get_journal_entry(
    entry_id: int,
    db: Session = Depends(deps.get_db),
    current_user: User = Depends(get_current_active_user)
) -> JournalEntryResponse:
    """
    Get a specific journal entry by ID.
    """
    service = JournalEntryService(db)
    entry = service.get_journal_entry(entry_id)
    
    if not entry:
        raise HTTPException(status_code=404, detail="Journal entry not found")
    
    return JournalEntryResponse.from_orm(entry)


@router.put("/{entry_id}", response_model=JournalEntryResponse)
def update_journal_entry(
    entry_id: int,
    entry_update: JournalEntryUpdate,
    db: Session = Depends(deps.get_db),
    current_user: User = Depends(get_current_active_user)
) -> JournalEntryResponse:
    """
    Update a journal entry (only if not posted).
    """
    service = JournalEntryService(db)
    
    # Get existing entry
    existing_entry = service.get_journal_entry(entry_id)
    if not existing_entry:
        raise HTTPException(status_code=404, detail="Journal entry not found")
    
    if existing_entry.status == 'POSTED':
        raise HTTPException(
            status_code=400,
            detail="Cannot update a posted journal entry"
        )
    
    try:
        updated_entry = service.update_journal_entry(
            entry_id=entry_id,
            update_data=entry_update.dict(exclude_unset=True),
            user_id=current_user.id
        )
        return JournalEntryResponse.from_orm(updated_entry)
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))


@router.delete("/{entry_id}")
def delete_journal_entry(
    entry_id: int,
    db: Session = Depends(deps.get_db),
    current_user: User = Depends(get_current_active_user)
):
    """
    Delete a journal entry (only if not posted).
    """
    service = JournalEntryService(db)
    
    # Get existing entry
    existing_entry = service.get_journal_entry(entry_id)
    if not existing_entry:
        raise HTTPException(status_code=404, detail="Journal entry not found")
    
    if existing_entry.status == 'POSTED':
        raise HTTPException(
            status_code=400,
            detail="Cannot delete a posted journal entry"
        )
    
    success = service.delete_journal_entry(entry_id, current_user.id)
    
    if not success:
        raise HTTPException(status_code=500, detail="Failed to delete journal entry")
    
    return {"message": "Journal entry deleted successfully"}


@router.post("/{entry_id}/post")
def post_journal_entry(
    entry_id: int,
    db: Session = Depends(deps.get_db),
    current_user: User = Depends(get_current_active_user)
):
    """
    Post a journal entry to the general ledger.
    """
    service = JournalEntryService(db)
    
    try:
        result = service.post_journal_entry(
            entry_id=entry_id,
            user_id=current_user.id
        )
        
        if result['success']:
            return {
                "message": "Journal entry posted successfully",
                "posting_reference": result['posting_reference']
            }
        else:
            raise HTTPException(status_code=400, detail=result['error'])
            
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))


@router.post("/{entry_id}/reverse")
def reverse_journal_entry(
    entry_id: int,
    reversal_date: date = Body(...),
    reversal_reason: str = Body(...),
    db: Session = Depends(deps.get_db),
    current_user: User = Depends(get_current_active_user)
):
    """
    Create a reversal entry for an existing journal entry.
    """
    service = JournalEntryService(db)
    
    try:
        reversal_entry = service.create_reversal_entry(
            original_entry_id=entry_id,
            reversal_date=reversal_date,
            reason=reversal_reason,
            user_id=current_user.id
        )
        
        return {
            "message": "Reversal entry created successfully",
            "reversal_entry_id": reversal_entry.id,
            "reversal_entry_number": reversal_entry.entry_number
        }
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))


@router.post("/recurring")
def create_recurring_journal(
    template: JournalEntryCreate,
    frequency: str = Body(..., regex="^(DAILY|WEEKLY|MONTHLY|QUARTERLY|ANNUALLY)$"),
    start_date: date = Body(...),
    end_date: Optional[date] = Body(None),
    occurrences: Optional[int] = Body(None),
    db: Session = Depends(deps.get_db),
    current_user: User = Depends(get_current_active_user)
):
    """
    Create a recurring journal entry template.
    """
    service = JournalEntryService(db)
    
    if not end_date and not occurrences:
        raise HTTPException(
            status_code=400,
            detail="Either end_date or occurrences must be provided"
        )
    
    try:
        recurring_id = service.create_recurring_template(
            template_data=template.dict(),
            frequency=frequency,
            start_date=start_date,
            end_date=end_date,
            occurrences=occurrences,
            user_id=current_user.id
        )
        
        return {
            "message": "Recurring journal template created successfully",
            "recurring_template_id": recurring_id
        }
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))


@router.post("/import")
def import_journal_entries(
    file_format: str = Body(..., regex="^(CSV|EXCEL|JSON)$"),
    file_data: str = Body(...),
    validate_only: bool = Body(False),
    db: Session = Depends(deps.get_db),
    current_user: User = Depends(get_current_active_user)
):
    """
    Import journal entries from file.
    """
    service = JournalEntryService(db)
    
    try:
        result = service.import_journal_entries(
            file_format=file_format,
            file_data=file_data,
            validate_only=validate_only,
            user_id=current_user.id
        )
        
        return {
            "success": result['success'],
            "imported_count": result.get('imported_count', 0),
            "validation_errors": result.get('errors', []),
            "warnings": result.get('warnings', [])
        }
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))