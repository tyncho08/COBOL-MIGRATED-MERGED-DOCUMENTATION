"""Purchase Ledger Purchase Order Management API endpoints"""

from fastapi import APIRouter, Depends, HTTPException, status, Query, Body
from sqlalchemy.orm import Session
from typing import List, Optional, Dict, Any
from datetime import date, datetime
from decimal import Decimal

from app.api import deps
from app.services.pl import supplier_master as po_service
from app.schemas.pl import (
    PurchaseOrder, PurchaseOrderCreate, PurchaseOrderUpdate,
    PurchaseOrderListResponse, PurchaseOrderStatus, ApprovalStatus,
    GoodsReceipt, GoodsReceiptCreate, PaymentTerms
)

router = APIRouter()


@router.get("", response_model=PurchaseOrderListResponse)
async def list_purchase_orders(
    supplier_code: Optional[str] = Query(None, description="Filter by supplier"),
    status: Optional[PurchaseOrderStatus] = Query(None, description="Filter by status"),
    start_date: Optional[date] = Query(None, description="Order date from"),
    end_date: Optional[date] = Query(None, description="Order date to"),
    buyer_code: Optional[str] = Query(None, description="Filter by buyer"),
    min_amount: Decimal = Query(0, description="Minimum order amount"),
    skip: int = 0,
    limit: int = 100,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    List purchase orders with optional filters.
    
    Returns PO headers with summary information.
    """
    orders = po_service.get_purchase_orders(
        db,
        supplier_code=supplier_code,
        status=status,
        start_date=start_date,
        end_date=end_date,
        buyer_code=buyer_code,
        min_amount=min_amount,
        skip=skip,
        limit=limit
    )
    
    total = po_service.count_purchase_orders(
        db,
        supplier_code=supplier_code,
        status=status,
        start_date=start_date,
        end_date=end_date,
        buyer_code=buyer_code,
        min_amount=min_amount
    )
    
    return PurchaseOrderListResponse(
        orders=orders,
        total=total,
        skip=skip,
        limit=limit
    )


@router.get("/{po_id}", response_model=PurchaseOrder)
async def get_purchase_order(
    po_id: int,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Get specific purchase order by ID."""
    order = po_service.get_purchase_order(db, po_id=po_id)
    if not order:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Purchase order {po_id} not found"
        )
    return order


@router.get("/po-number/{po_number}", response_model=PurchaseOrder)
async def get_purchase_order_by_number(
    po_number: str,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Get purchase order by PO number."""
    order = po_service.get_purchase_order_by_number(db, po_number=po_number)
    if not order:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Purchase order {po_number} not found"
        )
    return order


@router.post("", response_model=PurchaseOrder, status_code=status.HTTP_201_CREATED)
async def create_purchase_order(
    po_in: PurchaseOrderCreate,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Create new purchase order.
    
    Validates supplier, items, and creates approval workflow.
    """
    # Validate supplier
    supplier = po_service.validate_supplier(db, supplier_code=po_in.supplier_code)
    if not supplier:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Supplier {po_in.supplier_code} not found"
        )
    
    if supplier.status != "ACTIVE":
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Supplier {po_in.supplier_code} is not active"
        )
    
    # Check credit limit
    if supplier.credit_limit > 0:
        total_amount = sum(line.quantity * line.unit_price for line in po_in.lines)
        if supplier.current_balance + total_amount > supplier.credit_limit:
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail=f"Order exceeds supplier credit limit"
            )
    
    # Create PO
    order = po_service.create_purchase_order(
        db,
        po_in=po_in,
        created_by=current_user.id
    )
    
    # Create approval request if required
    if po_in.requires_approval:
        po_service.create_approval_request(
            db,
            po_id=order.id,
            requested_by=current_user.id
        )
    
    return order


@router.put("/{po_id}", response_model=PurchaseOrder)
async def update_purchase_order(
    po_id: int,
    po_update: PurchaseOrderUpdate,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Update purchase order.
    
    Only draft and approved POs can be updated.
    """
    order = po_service.get_purchase_order(db, po_id=po_id)
    if not order:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Purchase order {po_id} not found"
        )
    
    if order.status not in [PurchaseOrderStatus.DRAFT, PurchaseOrderStatus.APPROVED]:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Cannot update PO with status {order.status}"
        )
    
    updated = po_service.update_purchase_order(
        db,
        po_id=po_id,
        po_update=po_update,
        updated_by=current_user.id
    )
    
    return updated


@router.delete("/{po_id}")
async def delete_purchase_order(
    po_id: int,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Delete draft purchase order.
    
    Only draft POs can be deleted.
    """
    order = po_service.get_purchase_order(db, po_id=po_id)
    if not order:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Purchase order {po_id} not found"
        )
    
    if order.status != PurchaseOrderStatus.DRAFT:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Can only delete draft purchase orders"
        )
    
    po_service.delete_purchase_order(db, po_id=po_id)
    
    return {"message": f"Purchase order {po_id} deleted successfully"}


@router.post("/{po_id}/approve")
async def approve_purchase_order(
    po_id: int,
    comments: Optional[str] = Body(None),
    approved_amount: Optional[Decimal] = Body(None),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Approve purchase order.
    
    Must have approval authority for the amount.
    """
    order = po_service.get_purchase_order(db, po_id=po_id)
    if not order:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Purchase order {po_id} not found"
        )
    
    if order.approval_status != ApprovalStatus.PENDING:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"PO approval status is {order.approval_status}"
        )
    
    # Check approval authority
    if not po_service.has_approval_authority(
        db,
        user_id=current_user.id,
        amount=approved_amount or order.total_amount
    ):
        raise HTTPException(
            status_code=status.HTTP_403_FORBIDDEN,
            detail="Insufficient approval authority for this amount"
        )
    
    result = po_service.approve_purchase_order(
        db,
        po_id=po_id,
        approved_by=current_user.id,
        comments=comments,
        approved_amount=approved_amount
    )
    
    return {
        "po_id": po_id,
        "po_number": order.po_number,
        "status": result.status,
        "approval_status": result.approval_status,
        "approved_at": result.approved_at
    }


@router.post("/{po_id}/reject")
async def reject_purchase_order(
    po_id: int,
    reason: str = Body(..., min_length=10),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Reject purchase order.
    """
    order = po_service.get_purchase_order(db, po_id=po_id)
    if not order:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Purchase order {po_id} not found"
        )
    
    if order.approval_status != ApprovalStatus.PENDING:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"PO approval status is {order.approval_status}"
        )
    
    result = po_service.reject_purchase_order(
        db,
        po_id=po_id,
        rejected_by=current_user.id,
        reason=reason
    )
    
    return {
        "po_id": po_id,
        "po_number": order.po_number,
        "approval_status": result.approval_status,
        "rejected_at": datetime.now()
    }


@router.post("/{po_id}/send")
async def send_purchase_order(
    po_id: int,
    email_to: Optional[str] = Body(None),
    include_attachments: bool = Body(True),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Send PO to supplier.
    
    Email or print for mailing.
    """
    order = po_service.get_purchase_order(db, po_id=po_id)
    if not order:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Purchase order {po_id} not found"
        )
    
    if order.status == PurchaseOrderStatus.DRAFT:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Cannot send draft purchase order"
        )
    
    result = po_service.send_purchase_order(
        db,
        po_id=po_id,
        email_to=email_to,
        include_attachments=include_attachments,
        sent_by=current_user.id
    )
    
    return {
        "po_id": po_id,
        "po_number": order.po_number,
        "sent_to": result["sent_to"],
        "sent_at": result["sent_at"],
        "method": result["method"]
    }


@router.post("/{po_id}/receive", response_model=GoodsReceipt)
async def receive_goods(
    po_id: int,
    receipt_in: GoodsReceiptCreate,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Receive goods against purchase order.
    
    Creates GRN and updates inventory.
    """
    order = po_service.get_purchase_order(db, po_id=po_id)
    if not order:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Purchase order {po_id} not found"
        )
    
    if order.status not in [PurchaseOrderStatus.APPROVED, PurchaseOrderStatus.SENT, PurchaseOrderStatus.PARTIAL]:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Cannot receive goods for PO with status {order.status}"
        )
    
    # Validate receipt lines against PO
    for line in receipt_in.lines:
        po_line = next((l for l in order.lines if l.id == line.po_line_id), None)
        if not po_line:
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail=f"PO line {line.po_line_id} not found"
            )
        
        if line.quantity_received > po_line.quantity_outstanding:
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail=f"Over-receipt on line {po_line.line_number}: outstanding {po_line.quantity_outstanding}"
            )
    
    receipt = po_service.create_goods_receipt(
        db,
        po_id=po_id,
        receipt_in=receipt_in,
        received_by=current_user.id
    )
    
    return receipt


@router.post("/{po_id}/cancel")
async def cancel_purchase_order(
    po_id: int,
    reason: str = Body(..., min_length=10),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Cancel purchase order.
    
    Cannot cancel if goods received or invoiced.
    """
    order = po_service.get_purchase_order(db, po_id=po_id)
    if not order:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Purchase order {po_id} not found"
        )
    
    if order.status in [PurchaseOrderStatus.RECEIVED, PurchaseOrderStatus.CLOSED]:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Cannot cancel PO with status {order.status}"
        )
    
    if order.received_amount > 0:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Cannot cancel PO with received goods"
        )
    
    result = po_service.cancel_purchase_order(
        db,
        po_id=po_id,
        reason=reason,
        cancelled_by=current_user.id
    )
    
    return {
        "po_id": po_id,
        "po_number": order.po_number,
        "status": result.status,
        "cancelled_at": datetime.now()
    }


@router.post("/{po_id}/close")
async def close_purchase_order(
    po_id: int,
    force_close: bool = Body(False, description="Force close with outstanding items"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Close purchase order.
    
    Normally auto-closed when fully received/invoiced.
    """
    order = po_service.get_purchase_order(db, po_id=po_id)
    if not order:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Purchase order {po_id} not found"
        )
    
    if order.status == PurchaseOrderStatus.CLOSED:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Purchase order is already closed"
        )
    
    # Check for outstanding items
    has_outstanding = any(line.quantity_outstanding > 0 for line in order.lines)
    if has_outstanding and not force_close:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Purchase order has outstanding items. Use force_close=true to override."
        )
    
    result = po_service.close_purchase_order(
        db,
        po_id=po_id,
        force=force_close,
        closed_by=current_user.id
    )
    
    return {
        "po_id": po_id,
        "po_number": order.po_number,
        "status": result.status,
        "closed_at": datetime.now()
    }


@router.get("/{po_id}/print")
async def print_purchase_order(
    po_id: int,
    format: str = Query("PDF", regex="^(PDF|HTML|CSV)$"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Generate printable purchase order.
    """
    order = po_service.get_purchase_order(db, po_id=po_id)
    if not order:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Purchase order {po_id} not found"
        )
    
    document = po_service.generate_po_document(
        db,
        po_id=po_id,
        format=format
    )
    
    if format == "PDF":
        import base64
        return {
            "document_type": "PDF",
            "document_data": base64.b64encode(document).decode(),
            "filename": f"PO_{order.po_number}.pdf"
        }
    else:
        return {
            "document_type": format,
            "document_data": document,
            "filename": f"PO_{order.po_number}.{format.lower()}"
        }


@router.get("/pending-receipts")
async def get_pending_receipts(
    supplier_code: Optional[str] = None,
    buyer_code: Optional[str] = None,
    overdue_only: bool = Query(False),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Get purchase orders pending receipt.
    """
    orders = po_service.get_pending_receipts(
        db,
        supplier_code=supplier_code,
        buyer_code=buyer_code,
        overdue_only=overdue_only
    )
    
    return {
        "total_orders": len(orders),
        "total_value": sum(o.outstanding_value for o in orders),
        "orders": orders
    }


@router.post("/batch-approve")
async def batch_approve_orders(
    po_ids: List[int] = Body(...),
    comments: Optional[str] = Body(None),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Approve multiple purchase orders.
    """
    results = []
    success_count = 0
    error_count = 0
    
    for po_id in po_ids:
        try:
            order = po_service.get_purchase_order(db, po_id=po_id)
            if not order:
                results.append({
                    "po_id": po_id,
                    "success": False,
                    "error": "Purchase order not found"
                })
                error_count += 1
                continue
            
            if order.approval_status != ApprovalStatus.PENDING:
                results.append({
                    "po_id": po_id,
                    "success": False,
                    "error": f"Approval status is {order.approval_status}"
                })
                error_count += 1
                continue
            
            # Check approval authority
            if not po_service.has_approval_authority(
                db,
                user_id=current_user.id,
                amount=order.total_amount
            ):
                results.append({
                    "po_id": po_id,
                    "success": False,
                    "error": "Insufficient approval authority"
                })
                error_count += 1
                continue
            
            po_service.approve_purchase_order(
                db,
                po_id=po_id,
                approved_by=current_user.id,
                comments=comments
            )
            
            success_count += 1
            results.append({
                "po_id": po_id,
                "po_number": order.po_number,
                "success": True
            })
            
        except Exception as e:
            error_count += 1
            results.append({
                "po_id": po_id,
                "success": False,
                "error": str(e)
            })
    
    return {
        "total_processed": len(po_ids),
        "success_count": success_count,
        "error_count": error_count,
        "results": results
    }