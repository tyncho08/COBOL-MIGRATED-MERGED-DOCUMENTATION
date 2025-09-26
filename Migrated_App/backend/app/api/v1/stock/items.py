"""
Stock Items API endpoints
"""

from typing import List, Optional
from fastapi import APIRouter, Depends, HTTPException, Query
from sqlalchemy.orm import Session

from app.api import deps
from app.services.stock.stock_master import StockMasterService
from app.core.security import get_current_active_user
from app.models.auth import User

router = APIRouter()


@router.get("/")
def list_stock_items(
    db: Session = Depends(deps.get_db),
    skip: int = Query(0, ge=0),
    limit: int = Query(100, ge=1, le=1000),
    search: Optional[str] = None,
    category: Optional[str] = None,
    location: Optional[str] = None,
    active_only: bool = True,
    low_stock_only: bool = False,
    current_user: User = Depends(get_current_active_user)
):
    """
    Retrieve list of stock items with optional filtering.
    """
    service = StockMasterService(db)
    
    # Build filters
    filters = {}
    if category:
        filters['category'] = category
    if location:
        filters['location'] = location
    if active_only:
        filters['is_active'] = True
    if low_stock_only:
        filters['low_stock'] = True
    
    try:
        items = service.search_items(
            search_term=search,
            filters=filters,
            skip=skip,
            limit=limit
        )
        
        return {
            "items": items,
            "total": len(items),
            "skip": skip,
            "limit": limit
        }
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/")
def create_stock_item(
    item_data: dict,
    db: Session = Depends(deps.get_db),
    current_user: User = Depends(get_current_active_user)
):
    """
    Create a new stock item.
    """
    service = StockMasterService(db)
    
    # Validate required fields
    required_fields = ['item_code', 'description', 'unit_of_measure']
    missing_fields = [f for f in required_fields if f not in item_data]
    
    if missing_fields:
        raise HTTPException(
            status_code=400,
            detail=f"Missing required fields: {', '.join(missing_fields)}"
        )
    
    # Check if item code already exists
    existing = service.get_item_by_code(item_data['item_code'])
    if existing:
        raise HTTPException(
            status_code=400,
            detail=f"Item code {item_data['item_code']} already exists"
        )
    
    try:
        new_item = service.create_item(
            item_data=item_data,
            user_id=current_user.id
        )
        
        return {
            "success": True,
            "item_id": new_item.id,
            "item_code": new_item.item_code,
            "message": "Stock item created successfully"
        }
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/{item_id}")
def get_stock_item(
    item_id: int,
    db: Session = Depends(deps.get_db),
    current_user: User = Depends(get_current_active_user)
):
    """
    Get a specific stock item by ID.
    """
    service = StockMasterService(db)
    
    item = service.get_item(item_id)
    if not item:
        raise HTTPException(status_code=404, detail="Stock item not found")
    
    return item


@router.get("/code/{item_code}")
def get_stock_item_by_code(
    item_code: str,
    db: Session = Depends(deps.get_db),
    current_user: User = Depends(get_current_active_user)
):
    """
    Get a specific stock item by code.
    """
    service = StockMasterService(db)
    
    item = service.get_item_by_code(item_code)
    if not item:
        raise HTTPException(status_code=404, detail="Stock item not found")
    
    return item


@router.put("/{item_id}")
def update_stock_item(
    item_id: int,
    update_data: dict,
    db: Session = Depends(deps.get_db),
    current_user: User = Depends(get_current_active_user)
):
    """
    Update a stock item.
    """
    service = StockMasterService(db)
    
    try:
        updated_item = service.update_item(
            item_id=item_id,
            update_data=update_data,
            user_id=current_user.id
        )
        
        if not updated_item:
            raise HTTPException(status_code=404, detail="Stock item not found")
        
        return {
            "success": True,
            "message": "Stock item updated successfully"
        }
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.delete("/{item_id}")
def delete_stock_item(
    item_id: int,
    db: Session = Depends(deps.get_db),
    current_user: User = Depends(get_current_active_user)
):
    """
    Delete (deactivate) a stock item.
    """
    service = StockMasterService(db)
    
    # Check if item has stock
    current_stock = service.get_item_stock_levels(item_id)
    if current_stock and current_stock.get('total_quantity', 0) > 0:
        raise HTTPException(
            status_code=400,
            detail="Cannot delete item with existing stock"
        )
    
    success = service.deactivate_item(item_id, current_user.id)
    
    if not success:
        raise HTTPException(status_code=404, detail="Stock item not found")
    
    return {"message": "Stock item deactivated successfully"}


@router.get("/{item_id}/availability")
def check_item_availability(
    item_id: int,
    quantity: float = Query(..., gt=0),
    location_id: Optional[int] = None,
    db: Session = Depends(deps.get_db),
    current_user: User = Depends(get_current_active_user)
):
    """
    Check if a specific quantity of an item is available.
    """
    service = StockMasterService(db)
    
    try:
        availability = service.check_availability(
            item_id=item_id,
            quantity=quantity,
            location_id=location_id
        )
        
        return availability
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/{item_id}/stock-levels")
def get_item_stock_levels(
    item_id: int,
    location_id: Optional[int] = None,
    db: Session = Depends(deps.get_db),
    current_user: User = Depends(get_current_active_user)
):
    """
    Get current stock levels for an item.
    """
    service = StockMasterService(db)
    
    try:
        stock_levels = service.get_item_stock_levels(
            item_id=item_id,
            location_id=location_id
        )
        
        if not stock_levels:
            raise HTTPException(status_code=404, detail="Stock item not found")
        
        return stock_levels
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/{item_id}/history")
def get_item_movement_history(
    item_id: int,
    start_date: Optional[str] = None,
    end_date: Optional[str] = None,
    movement_type: Optional[str] = None,
    skip: int = Query(0, ge=0),
    limit: int = Query(100, ge=1, le=1000),
    db: Session = Depends(deps.get_db),
    current_user: User = Depends(get_current_active_user)
):
    """
    Get movement history for a stock item.
    """
    service = StockMasterService(db)
    
    try:
        history = service.get_item_history(
            item_id=item_id,
            start_date=start_date,
            end_date=end_date,
            movement_type=movement_type,
            skip=skip,
            limit=limit
        )
        
        return history
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/{item_id}/reorder")
def set_reorder_parameters(
    item_id: int,
    reorder_level: float = Query(..., ge=0),
    reorder_quantity: float = Query(..., gt=0),
    maximum_level: Optional[float] = None,
    db: Session = Depends(deps.get_db),
    current_user: User = Depends(get_current_active_user)
):
    """
    Set reorder parameters for a stock item.
    """
    service = StockMasterService(db)
    
    try:
        success = service.set_reorder_parameters(
            item_id=item_id,
            reorder_level=reorder_level,
            reorder_quantity=reorder_quantity,
            maximum_level=maximum_level,
            user_id=current_user.id
        )
        
        if not success:
            raise HTTPException(status_code=404, detail="Stock item not found")
        
        return {
            "success": True,
            "message": "Reorder parameters updated successfully"
        }
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))