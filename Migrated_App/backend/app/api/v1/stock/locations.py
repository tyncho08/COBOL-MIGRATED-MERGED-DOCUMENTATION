"""Stock Location Management API endpoints"""

from fastapi import APIRouter, Depends, HTTPException, status, Query, Body
from sqlalchemy.orm import Session
from typing import List, Optional, Dict, Any
from datetime import date, datetime
from decimal import Decimal

from app.api import deps
from app.services.stock import location_service
from app.schemas.stock import (
    Location, LocationCreate, LocationUpdate, LocationListResponse,
    LocationType, StockTake, StockTakeCreate, StockTakeLine
)

router = APIRouter()


@router.get("", response_model=LocationListResponse)
async def list_locations(
    location_type: Optional[LocationType] = Query(None, description="Filter by type"),
    warehouse_code: Optional[str] = Query(None, description="Filter by warehouse"),
    is_active: bool = Query(True, description="Active locations only"),
    search: Optional[str] = Query(None, description="Search in code/name"),
    skip: int = 0,
    limit: int = 100,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    List stock locations with optional filters.
    
    Returns warehouses, shops, production areas, etc.
    """
    locations = location_service.get_locations(
        db,
        location_type=location_type,
        warehouse_code=warehouse_code,
        is_active=is_active,
        search=search,
        skip=skip,
        limit=limit
    )
    
    total = location_service.count_locations(
        db,
        location_type=location_type,
        warehouse_code=warehouse_code,
        is_active=is_active,
        search=search
    )
    
    return LocationListResponse(
        locations=locations,
        total=total,
        skip=skip,
        limit=limit
    )


@router.get("/{location_code}", response_model=Location)
async def get_location(
    location_code: str,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Get specific location by code."""
    location = location_service.get_location(db, location_code=location_code)
    if not location:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Location {location_code} not found"
        )
    return location


@router.post("", response_model=Location, status_code=status.HTTP_201_CREATED)
async def create_location(
    location_in: LocationCreate,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Create new stock location.
    
    Sets up warehouse, shop floor, or other storage area.
    """
    # Check if location code already exists
    existing = location_service.get_location(db, location_code=location_in.location_code)
    if existing:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Location code {location_in.location_code} already exists"
        )
    
    # Validate warehouse code if provided
    if location_in.warehouse_code:
        warehouse = location_service.validate_warehouse(db, warehouse_code=location_in.warehouse_code)
        if not warehouse:
            raise HTTPException(
                status_code=status.HTTP_404_NOT_FOUND,
                detail=f"Warehouse {location_in.warehouse_code} not found"
            )
    
    location = location_service.create_location(
        db,
        location_in=location_in,
        created_by=current_user.id
    )
    
    return location


@router.put("/{location_code}", response_model=Location)
async def update_location(
    location_code: str,
    location_update: LocationUpdate,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Update location information.
    
    Cannot change location code.
    """
    location = location_service.get_location(db, location_code=location_code)
    if not location:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Location {location_code} not found"
        )
    
    updated = location_service.update_location(
        db,
        location_code=location_code,
        location_update=location_update,
        updated_by=current_user.id
    )
    
    return updated


@router.post("/{location_code}/deactivate")
async def deactivate_location(
    location_code: str,
    transfer_to: Optional[str] = Body(None, description="Transfer stock to location"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Deactivate location.
    
    Optionally transfer remaining stock.
    """
    location = location_service.get_location(db, location_code=location_code)
    if not location:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Location {location_code} not found"
        )
    
    if not location.is_active:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Location is already inactive"
        )
    
    # Check for stock
    stock_count = location_service.get_location_stock_count(db, location_code=location_code)
    if stock_count > 0:
        if not transfer_to:
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail=f"Location has {stock_count} items with stock. Specify transfer_to location."
            )
        
        # Validate transfer location
        transfer_location = location_service.get_location(db, location_code=transfer_to)
        if not transfer_location or not transfer_location.is_active:
            raise HTTPException(
                status_code=status.HTTP_404_NOT_FOUND,
                detail=f"Transfer location {transfer_to} not found or inactive"
            )
    
    result = location_service.deactivate_location(
        db,
        location_code=location_code,
        transfer_to=transfer_to,
        updated_by=current_user.id
    )
    
    return {
        "location_code": location_code,
        "deactivated": True,
        "stock_transferred": result.get("stock_transferred", 0),
        "transfer_movements": result.get("transfer_movements", 0)
    }


@router.get("/{location_code}/stock")
async def get_location_stock(
    location_code: str,
    category_code: Optional[str] = None,
    include_zero: bool = Query(False, description="Include zero stock items"),
    include_allocated: bool = Query(True, description="Show allocated quantities"),
    skip: int = 0,
    limit: int = 100,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Get stock levels at location.
    
    Shows all items stored at this location.
    """
    location = location_service.get_location(db, location_code=location_code)
    if not location:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Location {location_code} not found"
        )
    
    stock_items = location_service.get_location_stock(
        db,
        location_code=location_code,
        category_code=category_code,
        include_zero=include_zero,
        include_allocated=include_allocated,
        skip=skip,
        limit=limit
    )
    
    summary = location_service.get_location_stock_summary(
        db,
        location_code=location_code
    )
    
    return {
        "location_code": location_code,
        "location_name": location.location_name,
        "summary": summary,
        "items": stock_items,
        "item_count": len(stock_items),
        "skip": skip,
        "limit": limit
    }


@router.post("/{location_code}/stocktake", response_model=StockTake, status_code=status.HTTP_201_CREATED)
async def create_stocktake(
    location_code: str,
    stocktake_in: StockTakeCreate,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Create stocktake for location.
    
    Initiates physical inventory count process.
    """
    location = location_service.get_location(db, location_code=location_code)
    if not location:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Location {location_code} not found"
        )
    
    # Check for pending stocktakes
    pending = location_service.get_pending_stocktakes(db, location_code=location_code)
    if pending:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Location has {len(pending)} pending stocktakes. Complete or cancel them first."
        )
    
    # Freeze stock if requested
    if stocktake_in.freeze_stock:
        frozen = location_service.freeze_location_stock(
            db,
            location_code=location_code,
            reason=f"Stocktake {stocktake_in.description}"
        )
        if not frozen:
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail="Failed to freeze stock movements"
            )
    
    stocktake = location_service.create_stocktake(
        db,
        location_code=location_code,
        stocktake_in=stocktake_in,
        created_by=current_user.id
    )
    
    return stocktake


@router.put("/stocktake/{stocktake_id}/count")
async def update_stocktake_count(
    stocktake_id: int,
    count_lines: List[StockTakeLine] = Body(...),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Update stocktake counts.
    
    Records physical count quantities.
    """
    stocktake = location_service.get_stocktake(db, stocktake_id=stocktake_id)
    if not stocktake:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Stocktake {stocktake_id} not found"
        )
    
    if stocktake.status not in ["draft", "in_progress"]:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Cannot update counts for stocktake with status {stocktake.status}"
        )
    
    # Update counts
    result = location_service.update_stocktake_counts(
        db,
        stocktake_id=stocktake_id,
        count_lines=count_lines,
        counted_by=current_user.id
    )
    
    return {
        "stocktake_id": stocktake_id,
        "lines_updated": result["lines_updated"],
        "total_variance_value": result["total_variance_value"],
        "completion_percentage": result["completion_percentage"]
    }


@router.post("/stocktake/{stocktake_id}/complete")
async def complete_stocktake(
    stocktake_id: int,
    force_complete: bool = Body(False, description="Complete with uncounted items"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Complete stocktake.
    
    Validates counts and prepares adjustments.
    """
    stocktake = location_service.get_stocktake(db, stocktake_id=stocktake_id)
    if not stocktake:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Stocktake {stocktake_id} not found"
        )
    
    if stocktake.status != "in_progress":
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Stocktake status is {stocktake.status}"
        )
    
    # Check for uncounted items
    uncounted = location_service.get_uncounted_items(db, stocktake_id=stocktake_id)
    if uncounted and not force_complete:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Stocktake has {len(uncounted)} uncounted items. Use force_complete=true to proceed."
        )
    
    result = location_service.complete_stocktake(
        db,
        stocktake_id=stocktake_id,
        completed_by=current_user.id
    )
    
    return {
        "stocktake_id": stocktake_id,
        "status": "completed",
        "variance_items": result["variance_items"],
        "total_variance_value": result["total_variance_value"],
        "adjustment_preview": result["adjustments"]
    }


@router.post("/stocktake/{stocktake_id}/post")
async def post_stocktake_adjustments(
    stocktake_id: int,
    approval_reference: str = Body(..., description="Adjustment approval reference"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Post stocktake adjustments.
    
    Creates inventory adjustments from count variances.
    """
    stocktake = location_service.get_stocktake(db, stocktake_id=stocktake_id)
    if not stocktake:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Stocktake {stocktake_id} not found"
        )
    
    if stocktake.status != "completed":
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Stocktake must be completed before posting. Current status: {stocktake.status}"
        )
    
    # Check if already posted
    if stocktake.posted_at:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Stocktake adjustments have already been posted"
        )
    
    result = location_service.post_stocktake_adjustments(
        db,
        stocktake_id=stocktake_id,
        approval_reference=approval_reference,
        posted_by=current_user.id
    )
    
    # Unfreeze location if it was frozen
    if stocktake.freeze_stock:
        location_service.unfreeze_location_stock(
            db,
            location_code=stocktake.location_code
        )
    
    return {
        "stocktake_id": stocktake_id,
        "status": "posted",
        "adjustments_created": result["adjustments_created"],
        "total_adjustment_value": result["total_value"],
        "gl_entries": result["gl_entries"]
    }


@router.get("/{location_code}/capacity")
async def get_location_capacity(
    location_code: str,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Get location capacity utilization.
    
    Shows space usage and availability.
    """
    location = location_service.get_location(db, location_code=location_code)
    if not location:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Location {location_code} not found"
        )
    
    capacity = location_service.calculate_location_capacity(
        db,
        location_code=location_code
    )
    
    return {
        "location_code": location_code,
        "location_name": location.location_name,
        "capacity_cubic_meters": location.capacity_cubic_meters,
        "used_cubic_meters": capacity["used_space"],
        "available_cubic_meters": capacity["available_space"],
        "utilization_percent": capacity["utilization_percent"],
        "by_category": capacity["by_category"]
    }


@router.get("/{location_code}/movements")
async def get_location_movements(
    location_code: str,
    start_date: Optional[date] = None,
    end_date: Optional[date] = None,
    movement_type: Optional[str] = None,
    skip: int = 0,
    limit: int = 100,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Get recent movements for location.
    
    Shows inbound and outbound activity.
    """
    location = location_service.get_location(db, location_code=location_code)
    if not location:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Location {location_code} not found"
        )
    
    movements = location_service.get_location_movements(
        db,
        location_code=location_code,
        start_date=start_date,
        end_date=end_date,
        movement_type=movement_type,
        skip=skip,
        limit=limit
    )
    
    summary = location_service.get_location_movement_summary(
        db,
        location_code=location_code,
        start_date=start_date,
        end_date=end_date
    )
    
    return {
        "location_code": location_code,
        "period": {
            "start": start_date,
            "end": end_date
        },
        "summary": summary,
        "movements": movements,
        "movement_count": len(movements),
        "skip": skip,
        "limit": limit
    }


@router.post("/{location_code}/bin-mapping")
async def update_bin_mapping(
    location_code: str,
    bin_mappings: List[Dict[str, str]] = Body(..., description="Item to bin mappings"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Update bin location mappings.
    
    Assigns items to specific bins within location.
    """
    location = location_service.get_location(db, location_code=location_code)
    if not location:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Location {location_code} not found"
        )
    
    result = location_service.update_bin_mappings(
        db,
        location_code=location_code,
        bin_mappings=bin_mappings,
        updated_by=current_user.id
    )
    
    return {
        "location_code": location_code,
        "mappings_updated": result["updated"],
        "mappings_created": result["created"],
        "errors": result["errors"]
    }