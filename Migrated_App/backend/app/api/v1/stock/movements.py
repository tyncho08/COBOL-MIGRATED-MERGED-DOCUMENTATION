"""Stock Movement Management API endpoints"""

from fastapi import APIRouter, Depends, HTTPException, status, Query, Body
from sqlalchemy.orm import Session
from typing import List, Optional, Dict, Any
from datetime import date, datetime
from decimal import Decimal

from app.api import deps
from app.services.stock import movement_service
from app.schemas.stock import (
    StockMovement, StockMovementCreate, StockMovementListResponse,
    MovementType, BatchMovement, StockTransferCreate, StockTransfer
)

router = APIRouter()


@router.get("", response_model=StockMovementListResponse)
async def list_movements(
    item_code: Optional[str] = Query(None, description="Filter by item"),
    location_code: Optional[str] = Query(None, description="Filter by location"),
    movement_type: Optional[MovementType] = Query(None, description="Filter by type"),
    start_date: Optional[date] = Query(None, description="Start date"),
    end_date: Optional[date] = Query(None, description="End date"),
    reference_number: Optional[str] = Query(None, description="Reference number search"),
    skip: int = 0,
    limit: int = 100,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    List stock movements with optional filters.
    
    Shows all inventory transactions.
    """
    movements = movement_service.get_movements(
        db,
        item_code=item_code,
        location_code=location_code,
        movement_type=movement_type,
        start_date=start_date,
        end_date=end_date,
        reference_number=reference_number,
        skip=skip,
        limit=limit
    )
    
    total = movement_service.count_movements(
        db,
        item_code=item_code,
        location_code=location_code,
        movement_type=movement_type,
        start_date=start_date,
        end_date=end_date,
        reference_number=reference_number
    )
    
    return StockMovementListResponse(
        movements=movements,
        total=total,
        skip=skip,
        limit=limit
    )


@router.get("/{movement_id}", response_model=StockMovement)
async def get_movement(
    movement_id: int,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Get specific movement by ID."""
    movement = movement_service.get_movement(db, movement_id=movement_id)
    if not movement:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Movement {movement_id} not found"
        )
    return movement


@router.post("/receipt", response_model=StockMovement, status_code=status.HTTP_201_CREATED)
async def create_receipt(
    movement_in: StockMovementCreate,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Create stock receipt.
    
    Records goods received into inventory.
    """
    if movement_in.movement_type != MovementType.RECEIPT:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Movement type must be RECEIPT"
        )
    
    # Validate item
    item = movement_service.validate_item(db, item_code=movement_in.item_code)
    if not item:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Item {movement_in.item_code} not found"
        )
    
    # Validate location
    location = movement_service.validate_location(db, location_code=movement_in.to_location)
    if not location:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Location {movement_in.to_location} not found"
        )
    
    movement = movement_service.create_receipt(
        db,
        movement_in=movement_in,
        created_by=current_user.id
    )
    
    return movement


@router.post("/issue", response_model=StockMovement, status_code=status.HTTP_201_CREATED)
async def create_issue(
    movement_in: StockMovementCreate,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Create stock issue.
    
    Records goods issued from inventory.
    """
    if movement_in.movement_type != MovementType.ISSUE:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Movement type must be ISSUE"
        )
    
    # Check stock availability
    available = movement_service.check_stock_availability(
        db,
        item_code=movement_in.item_code,
        location_code=movement_in.from_location,
        quantity=movement_in.quantity
    )
    
    if not available["available"]:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Insufficient stock. Available: {available['qty_available']}, Requested: {movement_in.quantity}"
        )
    
    movement = movement_service.create_issue(
        db,
        movement_in=movement_in,
        created_by=current_user.id
    )
    
    return movement


@router.post("/adjustment", response_model=StockMovement, status_code=status.HTTP_201_CREATED)
async def create_adjustment(
    movement_in: StockMovementCreate,
    reason_code: str = Body(..., description="Adjustment reason code"),
    approval_ref: Optional[str] = Body(None, description="Approval reference"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Create stock adjustment.
    
    Adjusts inventory quantities for corrections.
    """
    if movement_in.movement_type != MovementType.ADJUSTMENT:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Movement type must be ADJUSTMENT"
        )
    
    # Validate adjustment reason
    valid_reasons = movement_service.get_adjustment_reasons(db)
    if reason_code not in valid_reasons:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Invalid reason code. Valid codes: {', '.join(valid_reasons)}"
        )
    
    # Check if adjustment requires approval
    if movement_service.requires_approval(
        db,
        adjustment_value=abs(movement_in.quantity * movement_in.unit_cost),
        user_id=current_user.id
    ):
        if not approval_ref:
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail="This adjustment requires approval reference"
            )
    
    movement = movement_service.create_adjustment(
        db,
        movement_in=movement_in,
        reason_code=reason_code,
        approval_ref=approval_ref,
        created_by=current_user.id
    )
    
    return movement


@router.post("/transfer", response_model=StockTransfer, status_code=status.HTTP_201_CREATED)
async def create_transfer(
    transfer_in: StockTransferCreate,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Create stock transfer between locations.
    
    Moves inventory between warehouses/locations.
    """
    # Validate from location
    from_location = movement_service.validate_location(db, location_code=transfer_in.from_location)
    if not from_location:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"From location {transfer_in.from_location} not found"
        )
    
    # Validate to location
    to_location = movement_service.validate_location(db, location_code=transfer_in.to_location)
    if not to_location:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"To location {transfer_in.to_location} not found"
        )
    
    if transfer_in.from_location == transfer_in.to_location:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Cannot transfer to same location"
        )
    
    # Check availability for all items
    for line in transfer_in.lines:
        available = movement_service.check_stock_availability(
            db,
            item_code=line.item_code,
            location_code=transfer_in.from_location,
            quantity=line.quantity
        )
        
        if not available["available"]:
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail=f"Insufficient stock for {line.item_code}. Available: {available['qty_available']}"
            )
    
    transfer = movement_service.create_transfer(
        db,
        transfer_in=transfer_in,
        created_by=current_user.id
    )
    
    return transfer


@router.post("/batch", status_code=status.HTTP_201_CREATED)
async def create_batch_movements(
    batch_in: BatchMovement,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Create multiple movements in batch.
    
    Processes multiple inventory transactions together.
    """
    if not batch_in.movements:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="No movements provided"
        )
    
    # Validate all movements first
    if batch_in.validate_only:
        validation_results = []
        for idx, movement in enumerate(batch_in.movements):
            try:
                movement_service.validate_movement(db, movement=movement)
                validation_results.append({
                    "index": idx,
                    "item_code": movement.item_code,
                    "valid": True
                })
            except ValueError as e:
                validation_results.append({
                    "index": idx,
                    "item_code": movement.item_code,
                    "valid": False,
                    "error": str(e)
                })
        
        return {
            "validation_only": True,
            "results": validation_results
        }
    
    # Process movements
    results = movement_service.create_batch_movements(
        db,
        batch_in=batch_in,
        created_by=current_user.id
    )
    
    return {
        "batch_number": results["batch_number"],
        "total_movements": len(batch_in.movements),
        "successful": results["successful"],
        "failed": results["failed"],
        "gl_posted": batch_in.post_to_gl,
        "results": results["details"]
    }


@router.get("/history/{item_code}")
async def get_item_movement_history(
    item_code: str,
    location_code: Optional[str] = None,
    days: int = Query(30, ge=1, le=365),
    include_costs: bool = Query(False),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Get movement history for an item.
    
    Shows transaction history with running balances.
    """
    item = movement_service.validate_item(db, item_code=item_code)
    if not item:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Item {item_code} not found"
        )
    
    history = movement_service.get_item_history(
        db,
        item_code=item_code,
        location_code=location_code,
        days=days,
        include_costs=include_costs and current_user.has_permission("view_costs")
    )
    
    return {
        "item_code": item_code,
        "item_description": item.description,
        "period_days": days,
        "movement_count": len(history["movements"]),
        "opening_balance": history["opening_balance"],
        "closing_balance": history["closing_balance"],
        "total_receipts": history["total_receipts"],
        "total_issues": history["total_issues"],
        "movements": history["movements"]
    }


@router.post("/return")
async def create_return(
    original_reference: str = Body(..., description="Original issue reference"),
    return_items: List[Dict[str, Any]] = Body(..., description="Items to return"),
    reason: str = Body(..., description="Return reason"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Process stock return.
    
    Returns previously issued items back to inventory.
    """
    # Find original movement
    original = movement_service.get_movement_by_reference(
        db,
        reference=original_reference
    )
    
    if not original:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Original movement {original_reference} not found"
        )
    
    if original.movement_type != MovementType.ISSUE:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Can only return against issue movements"
        )
    
    # Process return
    return_result = movement_service.create_return(
        db,
        original_movement_id=original.id,
        return_items=return_items,
        reason=reason,
        created_by=current_user.id
    )
    
    return {
        "return_number": return_result["return_number"],
        "movements_created": len(return_result["movements"]),
        "total_value": return_result["total_value"],
        "original_reference": original_reference
    }


@router.post("/{movement_id}/reverse")
async def reverse_movement(
    movement_id: int,
    reason: str = Body(..., min_length=10),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Reverse a stock movement.
    
    Creates opposite movement to cancel effect.
    """
    movement = movement_service.get_movement(db, movement_id=movement_id)
    if not movement:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Movement {movement_id} not found"
        )
    
    # Check if already reversed
    if movement_service.is_reversed(db, movement_id=movement_id):
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Movement has already been reversed"
        )
    
    reversal = movement_service.reverse_movement(
        db,
        movement_id=movement_id,
        reason=reason,
        reversed_by=current_user.id
    )
    
    return {
        "original_movement_id": movement_id,
        "reversal_movement_id": reversal.id,
        "reversal_number": reversal.movement_number,
        "reversed_at": datetime.now()
    }


@router.get("/pending-transfers")
async def get_pending_transfers(
    location_code: Optional[str] = None,
    include_in_transit: bool = Query(True),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Get pending transfer movements.
    
    Shows transfers awaiting receipt confirmation.
    """
    transfers = movement_service.get_pending_transfers(
        db,
        location_code=location_code,
        include_in_transit=include_in_transit
    )
    
    return {
        "total_pending": len(transfers),
        "by_status": {
            "awaiting_shipment": sum(1 for t in transfers if t["status"] == "pending"),
            "in_transit": sum(1 for t in transfers if t["status"] == "in_transit"),
            "awaiting_receipt": sum(1 for t in transfers if t["status"] == "awaiting_receipt")
        },
        "transfers": transfers
    }


@router.post("/transfer/{transfer_id}/receive")
async def receive_transfer(
    transfer_id: int,
    received_items: List[Dict[str, Any]] = Body(...),
    discrepancy_notes: Optional[str] = Body(None),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Receive transfer at destination.
    
    Confirms receipt and handles discrepancies.
    """
    transfer = movement_service.get_transfer(db, transfer_id=transfer_id)
    if not transfer:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Transfer {transfer_id} not found"
        )
    
    if transfer.status != "in_transit":
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Transfer status is {transfer.status}, expected in_transit"
        )
    
    result = movement_service.receive_transfer(
        db,
        transfer_id=transfer_id,
        received_items=received_items,
        discrepancy_notes=discrepancy_notes,
        received_by=current_user.id
    )
    
    return {
        "transfer_id": transfer_id,
        "status": "completed",
        "received_at": result["received_at"],
        "discrepancies": result["discrepancies"]
    }


@router.get("/cost-analysis")
async def get_movement_cost_analysis(
    start_date: date = Query(...),
    end_date: date = Query(...),
    item_code: Optional[str] = None,
    location_code: Optional[str] = None,
    movement_type: Optional[MovementType] = None,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Analyze movement costs.
    
    Provides cost analysis of inventory movements.
    """
    analysis = movement_service.analyze_movement_costs(
        db,
        start_date=start_date,
        end_date=end_date,
        item_code=item_code,
        location_code=location_code,
        movement_type=movement_type
    )
    
    return {
        "period": {
            "start": start_date,
            "end": end_date
        },
        "total_movements": analysis["total_movements"],
        "total_value": analysis["total_value"],
        "by_type": analysis["by_type"],
        "by_location": analysis["by_location"],
        "top_items_by_value": analysis["top_items"],
        "cost_variances": analysis["variances"]
    }