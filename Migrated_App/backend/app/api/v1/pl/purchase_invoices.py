"""Purchase Ledger Invoice Management API endpoints"""

from fastapi import APIRouter, Depends, HTTPException, status, Query, Body
from sqlalchemy.orm import Session
from typing import List, Optional, Dict, Any
from datetime import date, datetime
from decimal import Decimal

from app.api import deps
from app.services.pl import purchase_invoice as invoice_service
from app.schemas.pl import (
    PurchaseInvoice, PurchaseInvoiceCreate, PurchaseInvoiceUpdate,
    PurchaseInvoiceListResponse, PurchaseInvoiceStatus, ApprovalStatus,
    BatchInvoiceApproval, DebitNote, DebitNoteCreate
)

router = APIRouter()


@router.get("", response_model=PurchaseInvoiceListResponse)
async def list_purchase_invoices(
    supplier_code: Optional[str] = Query(None, description="Filter by supplier"),
    status: Optional[PurchaseInvoiceStatus] = Query(None, description="Filter by status"),
    start_date: Optional[date] = Query(None, description="Invoice date from"),
    end_date: Optional[date] = Query(None, description="Invoice date to"),
    po_number: Optional[str] = Query(None, description="Filter by PO number"),
    pending_approval: bool = Query(False, description="Only pending approval"),
    skip: int = 0,
    limit: int = 100,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    List purchase invoices with optional filters.
    
    Returns invoice headers with summary information.
    """
    invoices = invoice_service.get_purchase_invoices(
        db,
        supplier_code=supplier_code,
        status=status,
        start_date=start_date,
        end_date=end_date,
        po_number=po_number,
        pending_approval=pending_approval,
        skip=skip,
        limit=limit
    )
    
    total = invoice_service.count_purchase_invoices(
        db,
        supplier_code=supplier_code,
        status=status,
        start_date=start_date,
        end_date=end_date,
        po_number=po_number,
        pending_approval=pending_approval
    )
    
    return PurchaseInvoiceListResponse(
        invoices=invoices,
        total=total,
        skip=skip,
        limit=limit
    )


@router.get("/{invoice_id}", response_model=PurchaseInvoice)
async def get_purchase_invoice(
    invoice_id: int,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Get specific purchase invoice by ID."""
    invoice = invoice_service.get_purchase_invoice(db, invoice_id=invoice_id)
    if not invoice:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Purchase invoice {invoice_id} not found"
        )
    return invoice


@router.post("", response_model=PurchaseInvoice, status_code=status.HTTP_201_CREATED)
async def create_purchase_invoice(
    invoice_in: PurchaseInvoiceCreate,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Create new purchase invoice.
    
    Can match to PO or create standalone.
    """
    # Validate supplier
    supplier = invoice_service.validate_supplier(db, supplier_code=invoice_in.supplier_code)
    if not supplier:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Supplier {invoice_in.supplier_code} not found"
        )
    
    if supplier.status != "ACTIVE":
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Supplier {invoice_in.supplier_code} is not active"
        )
    
    # Check for duplicate invoice
    duplicate = invoice_service.check_duplicate_invoice(
        db,
        supplier_code=invoice_in.supplier_code,
        invoice_number=invoice_in.invoice_number
    )
    if duplicate:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Invoice {invoice_in.invoice_number} already exists for this supplier"
        )
    
    # Match to PO if requested
    if invoice_in.match_to_po and invoice_in.po_number:
        po = invoice_service.get_purchase_order(db, po_number=invoice_in.po_number)
        if not po:
            raise HTTPException(
                status_code=status.HTTP_404_NOT_FOUND,
                detail=f"Purchase order {invoice_in.po_number} not found"
            )
        
        # Perform 3-way match
        match_result = invoice_service.three_way_match(
            db,
            invoice_in=invoice_in,
            po=po
        )
        if not match_result["match_successful"]:
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail=f"3-way match failed: {match_result['errors']}"
            )
    
    invoice = invoice_service.create_purchase_invoice(
        db,
        invoice_in=invoice_in,
        created_by=current_user.id
    )
    
    return invoice


@router.put("/{invoice_id}", response_model=PurchaseInvoice)
async def update_purchase_invoice(
    invoice_id: int,
    invoice_update: PurchaseInvoiceUpdate,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Update purchase invoice.
    
    Only draft invoices can be updated.
    """
    invoice = invoice_service.get_purchase_invoice(db, invoice_id=invoice_id)
    if not invoice:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Purchase invoice {invoice_id} not found"
        )
    
    if invoice.status != PurchaseInvoiceStatus.DRAFT:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Can only update draft invoices"
        )
    
    updated = invoice_service.update_purchase_invoice(
        db,
        invoice_id=invoice_id,
        invoice_update=invoice_update,
        updated_by=current_user.id
    )
    
    return updated


@router.delete("/{invoice_id}")
async def delete_purchase_invoice(
    invoice_id: int,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Delete draft purchase invoice.
    """
    invoice = invoice_service.get_purchase_invoice(db, invoice_id=invoice_id)
    if not invoice:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Purchase invoice {invoice_id} not found"
        )
    
    if invoice.status != PurchaseInvoiceStatus.DRAFT:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Can only delete draft invoices"
        )
    
    invoice_service.delete_purchase_invoice(db, invoice_id=invoice_id)
    
    return {"message": f"Purchase invoice {invoice_id} deleted successfully"}


@router.post("/{invoice_id}/submit-approval")
async def submit_for_approval(
    invoice_id: int,
    urgency: str = Body("normal", regex="^(low|normal|high|urgent)$"),
    notes: Optional[str] = Body(None),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Submit invoice for approval.
    
    Routes based on amount and approval hierarchy.
    """
    invoice = invoice_service.get_purchase_invoice(db, invoice_id=invoice_id)
    if not invoice:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Purchase invoice {invoice_id} not found"
        )
    
    if invoice.status != PurchaseInvoiceStatus.DRAFT:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Invoice status is {invoice.status}"
        )
    
    result = invoice_service.submit_for_approval(
        db,
        invoice_id=invoice_id,
        requested_by=current_user.id,
        urgency=urgency,
        notes=notes
    )
    
    return {
        "invoice_id": invoice_id,
        "status": result.status,
        "approval_status": result.approval_status,
        "routed_to": result.get("routed_to")
    }


@router.post("/{invoice_id}/approve")
async def approve_purchase_invoice(
    invoice_id: int,
    comments: Optional[str] = Body(None),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Approve purchase invoice.
    """
    invoice = invoice_service.get_purchase_invoice(db, invoice_id=invoice_id)
    if not invoice:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Purchase invoice {invoice_id} not found"
        )
    
    if invoice.approval_status != ApprovalStatus.PENDING:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Approval status is {invoice.approval_status}"
        )
    
    # Check approval authority
    if not invoice_service.has_approval_authority(
        db,
        user_id=current_user.id,
        amount=invoice.total_amount
    ):
        raise HTTPException(
            status_code=status.HTTP_403_FORBIDDEN,
            detail="Insufficient approval authority"
        )
    
    result = invoice_service.approve_invoice(
        db,
        invoice_id=invoice_id,
        approved_by=current_user.id,
        comments=comments
    )
    
    return {
        "invoice_id": invoice_id,
        "status": result.status,
        "approval_status": result.approval_status,
        "approved_at": result.approved_at
    }


@router.post("/{invoice_id}/reject")
async def reject_purchase_invoice(
    invoice_id: int,
    reason: str = Body(..., min_length=10),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Reject purchase invoice.
    """
    invoice = invoice_service.get_purchase_invoice(db, invoice_id=invoice_id)
    if not invoice:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Purchase invoice {invoice_id} not found"
        )
    
    if invoice.approval_status != ApprovalStatus.PENDING:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Approval status is {invoice.approval_status}"
        )
    
    result = invoice_service.reject_invoice(
        db,
        invoice_id=invoice_id,
        rejected_by=current_user.id,
        reason=reason
    )
    
    return {
        "invoice_id": invoice_id,
        "approval_status": result.approval_status,
        "rejected_at": datetime.now()
    }


@router.post("/{invoice_id}/post")
async def post_purchase_invoice(
    invoice_id: int,
    posting_date: Optional[date] = Body(None),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Post invoice to General Ledger.
    
    Must be approved first.
    """
    invoice = invoice_service.get_purchase_invoice(db, invoice_id=invoice_id)
    if not invoice:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Purchase invoice {invoice_id} not found"
        )
    
    if invoice.status == PurchaseInvoiceStatus.POSTED:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Invoice is already posted"
        )
    
    if invoice.approval_status != ApprovalStatus.APPROVED:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Invoice must be approved before posting"
        )
    
    result = invoice_service.post_invoice(
        db,
        invoice_id=invoice_id,
        posting_date=posting_date or date.today(),
        posted_by=current_user.id
    )
    
    return {
        "invoice_id": invoice_id,
        "status": result["status"],
        "posted_at": result["posted_at"],
        "gl_entries": result["gl_entries"]
    }


@router.post("/{invoice_id}/void")
async def void_purchase_invoice(
    invoice_id: int,
    reason: str = Body(..., min_length=10),
    void_date: Optional[date] = Body(None),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Void posted invoice.
    
    Creates reversal entries.
    """
    invoice = invoice_service.get_purchase_invoice(db, invoice_id=invoice_id)
    if not invoice:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Purchase invoice {invoice_id} not found"
        )
    
    if invoice.status != PurchaseInvoiceStatus.POSTED:
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
        "reversal_entries": result["reversal_entries"]
    }


@router.post("/{invoice_id}/debit-note")
async def create_debit_note(
    invoice_id: int,
    debit_note_in: DebitNoteCreate,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Create debit note against invoice.
    """
    invoice = invoice_service.get_purchase_invoice(db, invoice_id=invoice_id)
    if not invoice:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Purchase invoice {invoice_id} not found"
        )
    
    if invoice.status not in [PurchaseInvoiceStatus.POSTED, PurchaseInvoiceStatus.PAID, PurchaseInvoiceStatus.PARTIAL]:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Can only create debit notes for posted invoices"
        )
    
    debit_note = invoice_service.create_debit_note(
        db,
        original_invoice_id=invoice_id,
        debit_note_in=debit_note_in,
        created_by=current_user.id
    )
    
    return {
        "debit_note_id": debit_note.id,
        "debit_note_number": debit_note.debit_note_number,
        "total_amount": debit_note.total_amount
    }


@router.get("/{invoice_id}/match-status")
async def get_match_status(
    invoice_id: int,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Get 3-way match status for invoice.
    """
    invoice = invoice_service.get_purchase_invoice(db, invoice_id=invoice_id)
    if not invoice:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Purchase invoice {invoice_id} not found"
        )
    
    if not invoice.matched_to_po:
        return {
            "invoice_id": invoice_id,
            "matched_to_po": False,
            "match_status": "Not matched to PO"
        }
    
    match_details = invoice_service.get_match_details(
        db,
        invoice_id=invoice_id
    )
    
    return match_details


@router.post("/batch-approval")
async def batch_approve_invoices(
    batch_request: BatchInvoiceApproval,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Approve or reject multiple invoices.
    """
    results = []
    success_count = 0
    error_count = 0
    
    for invoice_id in batch_request.invoice_ids:
        try:
            invoice = invoice_service.get_purchase_invoice(db, invoice_id=invoice_id)
            if not invoice:
                results.append({
                    "invoice_id": invoice_id,
                    "success": False,
                    "error": "Invoice not found"
                })
                error_count += 1
                continue
            
            if invoice.approval_status != ApprovalStatus.PENDING:
                results.append({
                    "invoice_id": invoice_id,
                    "success": False,
                    "error": f"Approval status is {invoice.approval_status}"
                })
                error_count += 1
                continue
            
            if batch_request.action == "approve":
                # Check approval authority
                if not invoice_service.has_approval_authority(
                    db,
                    user_id=current_user.id,
                    amount=invoice.total_amount
                ):
                    results.append({
                        "invoice_id": invoice_id,
                        "success": False,
                        "error": "Insufficient approval authority"
                    })
                    error_count += 1
                    continue
                
                invoice_service.approve_invoice(
                    db,
                    invoice_id=invoice_id,
                    approved_by=current_user.id,
                    comments=batch_request.comments
                )
            else:
                invoice_service.reject_invoice(
                    db,
                    invoice_id=invoice_id,
                    rejected_by=current_user.id,
                    reason=batch_request.comments or "Batch rejection"
                )
            
            success_count += 1
            results.append({
                "invoice_id": invoice_id,
                "success": True,
                "action": batch_request.action
            })
            
        except Exception as e:
            error_count += 1
            results.append({
                "invoice_id": invoice_id,
                "success": False,
                "error": str(e)
            })
    
    return {
        "action": batch_request.action,
        "total_processed": len(batch_request.invoice_ids),
        "success_count": success_count,
        "error_count": error_count,
        "results": results
    }


@router.get("/outstanding")
async def get_outstanding_invoices(
    supplier_code: Optional[str] = None,
    as_of_date: Optional[date] = None,
    include_partial: bool = Query(True),
    min_amount: Decimal = Query(0),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Get outstanding (unpaid) invoices.
    """
    outstanding = invoice_service.get_outstanding_invoices(
        db,
        supplier_code=supplier_code,
        as_of_date=as_of_date or date.today(),
        include_partial=include_partial,
        min_amount=min_amount
    )
    
    summary = {
        "total_outstanding": sum(inv.balance_due for inv in outstanding),
        "invoice_count": len(outstanding),
        "by_supplier": {}
    }
    
    # Group by supplier
    for inv in outstanding:
        if inv.supplier_code not in summary["by_supplier"]:
            summary["by_supplier"][inv.supplier_code] = {
                "supplier_name": inv.supplier_name,
                "count": 0,
                "amount": Decimal(0)
            }
        summary["by_supplier"][inv.supplier_code]["count"] += 1
        summary["by_supplier"][inv.supplier_code]["amount"] += inv.balance_due
    
    return {
        "summary": summary,
        "invoices": outstanding
    }


@router.get("/{invoice_id}/print")
async def print_invoice(
    invoice_id: int,
    format: str = Query("PDF", regex="^(PDF|HTML|CSV)$"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Generate printable invoice copy.
    """
    invoice = invoice_service.get_purchase_invoice(db, invoice_id=invoice_id)
    if not invoice:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Purchase invoice {invoice_id} not found"
        )
    
    document = invoice_service.generate_invoice_document(
        db,
        invoice_id=invoice_id,
        format=format
    )
    
    if format == "PDF":
        import base64
        return {
            "document_type": "PDF",
            "document_data": base64.b64encode(document).decode(),
            "filename": f"invoice_{invoice.internal_invoice_number}.pdf"
        }
    else:
        return {
            "document_type": format,
            "document_data": document,
            "filename": f"invoice_{invoice.internal_invoice_number}.{format.lower()}"
        }