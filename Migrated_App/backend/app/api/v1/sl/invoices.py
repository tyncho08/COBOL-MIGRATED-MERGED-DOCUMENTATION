"""Sales Ledger Invoice Management API endpoints"""

from fastapi import APIRouter, Depends, HTTPException, status, Query, Body
from sqlalchemy.orm import Session
from typing import List, Optional, Dict, Any
from datetime import date, datetime
from decimal import Decimal

from app.api import deps
from app.services.sl import sales_invoice as invoice_service
from app.schemas.sl import (
    Invoice, InvoiceCreate, InvoiceUpdate, InvoiceListResponse,
    InvoiceStatus, InvoicePost, CreditNoteCreate
)

router = APIRouter()


@router.get("", response_model=InvoiceListResponse)
async def list_invoices(
    customer_code: Optional[str] = Query(None, description="Filter by customer"),
    status: Optional[InvoiceStatus] = Query(None, description="Filter by status"),
    start_date: Optional[date] = Query(None, description="Start date"),
    end_date: Optional[date] = Query(None, description="End date"),
    overdue_only: bool = Query(False, description="Only overdue invoices"),
    unpaid_only: bool = Query(False, description="Only unpaid invoices"),
    skip: int = 0,
    limit: int = 100,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    List sales invoices with optional filters.
    
    Returns invoice headers with summary information.
    """
    invoices = invoice_service.get_invoices(
        db,
        customer_code=customer_code,
        status=status,
        start_date=start_date,
        end_date=end_date,
        overdue_only=overdue_only,
        unpaid_only=unpaid_only,
        skip=skip,
        limit=limit
    )
    
    total = invoice_service.count_invoices(
        db,
        customer_code=customer_code,
        status=status,
        start_date=start_date,
        end_date=end_date,
        overdue_only=overdue_only,
        unpaid_only=unpaid_only
    )
    
    return InvoiceListResponse(
        invoices=invoices,
        total=total,
        skip=skip,
        limit=limit
    )


@router.post("", response_model=Invoice, status_code=status.HTTP_201_CREATED)
async def create_invoice(
    invoice_in: InvoiceCreate,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Create new sales invoice.
    
    Validates customer, items, pricing and tax calculations.
    """
    # Validate customer
    customer = invoice_service.validate_customer(db, customer_code=invoice_in.customer_code)
    if not customer:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Customer {invoice_in.customer_code} not found"
        )
    
    # Check credit status
    if customer.credit_status == "BLOCKED":
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Customer is on credit hold"
        )
    
    # Validate invoice lines
    if not invoice_in.lines:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Invoice must have at least one line item"
        )
    
    # Create invoice
    invoice = invoice_service.create_invoice(
        db,
        invoice_in=invoice_in,
        created_by=current_user.id
    )
    
    return invoice


@router.get("/{invoice_id}", response_model=Invoice)
async def get_invoice(
    invoice_id: int,
    include_gl_entries: bool = Query(False, description="Include GL posting details"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Get specific invoice by ID."""
    invoice = invoice_service.get_invoice(
        db, 
        invoice_id=invoice_id,
        include_gl_entries=include_gl_entries
    )
    if not invoice:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Invoice {invoice_id} not found"
        )
    return invoice


@router.put("/{invoice_id}", response_model=Invoice)
async def update_invoice(
    invoice_id: int,
    invoice_update: InvoiceUpdate,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Update invoice (draft only).
    
    Cannot update posted invoices.
    """
    invoice = invoice_service.get_invoice(db, invoice_id=invoice_id)
    if not invoice:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Invoice {invoice_id} not found"
        )
    
    if invoice.status != InvoiceStatus.DRAFT:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Can only update draft invoices"
        )
    
    updated_invoice = invoice_service.update_invoice(
        db,
        invoice_id=invoice_id,
        invoice_update=invoice_update,
        updated_by=current_user.id
    )
    
    return updated_invoice


@router.post("/{invoice_id}/post")
async def post_invoice(
    invoice_id: int,
    post_request: InvoicePost = Body(InvoicePost()),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Post invoice to General Ledger.
    
    Creates GL entries and updates inventory.
    """
    invoice = invoice_service.get_invoice(db, invoice_id=invoice_id)
    if not invoice:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Invoice {invoice_id} not found"
        )
    
    if invoice.status != InvoiceStatus.DRAFT:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Invoice is already {invoice.status}"
        )
    
    # Post invoice
    result = invoice_service.post_invoice(
        db,
        invoice_id=invoice_id,
        posting_date=post_request.posting_date or date.today(),
        create_gl_entries=post_request.create_gl_entries,
        posted_by=current_user.id
    )
    
    return {
        "invoice_id": invoice_id,
        "invoice_number": invoice.invoice_number,
        "posted_at": result["posted_at"],
        "gl_entries": result["gl_entries"],
        "inventory_updated": result["inventory_updated"]
    }


@router.post("/{invoice_id}/void")
async def void_invoice(
    invoice_id: int,
    reason: str = Body(..., min_length=10, description="Reason for voiding"),
    void_date: Optional[date] = Body(None),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Void posted invoice.
    
    Creates reversal GL entries.
    """
    invoice = invoice_service.get_invoice(db, invoice_id=invoice_id)
    if not invoice:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Invoice {invoice_id} not found"
        )
    
    if invoice.status != InvoiceStatus.POSTED:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Can only void posted invoices"
        )
    
    result = invoice_service.void_invoice(
        db,
        invoice_id=invoice_id,
        reason=reason,
        void_date=void_date or date.today(),
        voided_by=current_user.id
    )
    
    return {
        "invoice_id": invoice_id,
        "voided_at": result["voided_at"],
        "reversal_entries": result["reversal_entries"],
        "credit_note_created": result.get("credit_note_id") is not None
    }


@router.post("/{invoice_id}/credit-note")
async def create_credit_note(
    invoice_id: int,
    credit_note_in: CreditNoteCreate,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Create credit note for invoice.
    
    Can be partial or full credit.
    """
    invoice = invoice_service.get_invoice(db, invoice_id=invoice_id)
    if not invoice:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Invoice {invoice_id} not found"
        )
    
    if invoice.status not in [InvoiceStatus.POSTED, InvoiceStatus.PAID, InvoiceStatus.PARTIAL]:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Can only create credit notes for posted invoices"
        )
    
    credit_note = invoice_service.create_credit_note(
        db,
        original_invoice_id=invoice_id,
        credit_note_in=credit_note_in,
        created_by=current_user.id
    )
    
    return {
        "credit_note_id": credit_note.id,
        "credit_note_number": credit_note.credit_note_number,
        "total_amount": credit_note.total_amount,
        "auto_allocated": credit_note_in.auto_allocate,
        "unallocated_amount": credit_note.unallocated_amount
    }


@router.get("/{invoice_id}/print")
async def print_invoice(
    invoice_id: int,
    format: str = Query("PDF", regex="^(PDF|HTML|CSV|XML)$"),
    include_details: bool = Query(True),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Generate printable invoice.
    
    Multiple formats supported.
    """
    invoice = invoice_service.get_invoice(db, invoice_id=invoice_id)
    if not invoice:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Invoice {invoice_id} not found"
        )
    
    document = invoice_service.generate_invoice_document(
        db,
        invoice_id=invoice_id,
        format=format,
        include_details=include_details
    )
    
    if format == "PDF":
        import base64
        return {
            "document_type": "PDF",
            "document_data": base64.b64encode(document).decode(),
            "filename": f"invoice_{invoice.invoice_number}.pdf"
        }
    else:
        return {
            "document_type": format,
            "document_data": document,
            "filename": f"invoice_{invoice.invoice_number}.{format.lower()}"
        }


@router.delete("/{invoice_id}")
async def delete_invoice(
    invoice_id: int,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Delete draft invoice.
    
    Only draft invoices can be deleted.
    """
    invoice = invoice_service.get_invoice(db, invoice_id=invoice_id)
    if not invoice:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Invoice {invoice_id} not found"
        )
    
    if invoice.status != InvoiceStatus.DRAFT:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Can only delete draft invoices"
        )
    
    invoice_service.delete_invoice(db, invoice_id=invoice_id)
    
    return {"message": f"Invoice {invoice_id} deleted successfully"}


@router.post("/batch-post")
async def batch_post_invoices(
    invoice_ids: List[int] = Body(..., description="List of invoice IDs to post"),
    posting_date: Optional[date] = Body(None),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Post multiple invoices in batch.
    """
    results = []
    success_count = 0
    error_count = 0
    
    for invoice_id in invoice_ids:
        try:
            invoice = invoice_service.get_invoice(db, invoice_id=invoice_id)
            if not invoice:
                results.append({
                    "invoice_id": invoice_id,
                    "success": False,
                    "error": "Invoice not found"
                })
                error_count += 1
                continue
            
            if invoice.status != InvoiceStatus.DRAFT:
                results.append({
                    "invoice_id": invoice_id,
                    "success": False,
                    "error": f"Invoice already {invoice.status}"
                })
                error_count += 1
                continue
            
            result = invoice_service.post_invoice(
                db,
                invoice_id=invoice_id,
                posting_date=posting_date or date.today(),
                posted_by=current_user.id
            )
            
            success_count += 1
            results.append({
                "invoice_id": invoice_id,
                "invoice_number": invoice.invoice_number,
                "success": True,
                "gl_entries_count": len(result["gl_entries"])
            })
            
        except Exception as e:
            error_count += 1
            results.append({
                "invoice_id": invoice_id,
                "success": False,
                "error": str(e)
            })
    
    return {
        "total_processed": len(invoice_ids),
        "success_count": success_count,
        "error_count": error_count,
        "results": results
    }


@router.get("/outstanding")
async def get_outstanding_invoices(
    customer_code: Optional[str] = None,
    as_of_date: Optional[date] = None,
    include_partial: bool = Query(True, description="Include partially paid invoices"),
    min_amount: Decimal = Query(0, description="Minimum outstanding amount"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Get outstanding (unpaid) invoices.
    
    Used for payment allocation and aging.
    """
    outstanding = invoice_service.get_outstanding_invoices(
        db,
        customer_code=customer_code,
        as_of_date=as_of_date or date.today(),
        include_partial=include_partial,
        min_amount=min_amount
    )
    
    summary = {
        "total_outstanding": sum(inv.balance_due for inv in outstanding),
        "invoice_count": len(outstanding),
        "overdue_amount": sum(inv.balance_due for inv in outstanding if inv.days_overdue > 0),
        "current_amount": sum(inv.balance_due for inv in outstanding if inv.days_overdue <= 0)
    }
    
    return {
        "summary": summary,
        "invoices": outstanding
    }


@router.post("/recurring")
async def create_recurring_invoice_template(
    template_name: str = Body(...),
    customer_code: str = Body(...),
    frequency: str = Body(..., regex="^(monthly|quarterly|annually)$"),
    next_date: date = Body(...),
    invoice_template: InvoiceCreate = Body(...),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Create recurring invoice template.
    
    Automatically generates invoices on schedule.
    """
    template = invoice_service.create_recurring_template(
        db,
        template_name=template_name,
        customer_code=customer_code,
        frequency=frequency,
        next_date=next_date,
        invoice_template=invoice_template,
        created_by=current_user.id
    )
    
    return {
        "template_id": template.id,
        "template_name": template_name,
        "frequency": frequency,
        "next_generation_date": next_date
    }


@router.post("/generate-recurring")
async def generate_recurring_invoices(
    as_of_date: date = Body(..., description="Generate invoices due by this date"),
    preview_only: bool = Body(False, description="Preview without creating"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Generate invoices from recurring templates.
    
    Run periodically (e.g., daily) to create scheduled invoices.
    """
    result = invoice_service.generate_recurring_invoices(
        db,
        as_of_date=as_of_date,
        preview_only=preview_only,
        created_by=current_user.id if not preview_only else None
    )
    
    return {
        "preview_only": preview_only,
        "templates_processed": result["templates_processed"],
        "invoices_generated": result["invoices_generated"],
        "next_run_date": result["next_run_date"],
        "generated_invoices": result["invoices"] if preview_only else []
    }