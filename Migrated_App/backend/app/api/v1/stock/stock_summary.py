"""Stock Control Summary API endpoints - Simplified without auth"""

from fastapi import APIRouter, Depends
from sqlalchemy import func
from sqlalchemy.orm import Session
from typing import Dict, Any, List
from datetime import datetime, timedelta

from app.core.database import get_db
from app.models.stock import StockRec

router = APIRouter()


@router.get("/summary")
async def get_stock_summary(
    db: Session = Depends(get_db)
) -> Dict[str, Any]:
    """Get stock control summary statistics"""
    
    # Get stock statistics
    total_items = db.query(StockRec).count()
    
    # Calculate total stock value
    stock_value_result = db.query(
        func.sum(StockRec.stock_qty_on_hand * StockRec.stock_avg_cost)
    ).scalar()
    total_value = float(stock_value_result) if stock_value_result else 0.0
    
    # Calculate total quantity on hand
    total_quantity_result = db.query(
        func.sum(StockRec.stock_qty_on_hand)
    ).scalar()
    total_quantity = float(total_quantity_result) if total_quantity_result else 0.0
    
    # Count low stock items
    low_stock_items = db.query(StockRec).filter(
        StockRec.stock_qty_on_hand <= StockRec.stock_reorder_point
    ).count()
    
    # Count negative stock items
    negative_stock_items = db.query(StockRec).filter(
        StockRec.stock_qty_on_hand < 0
    ).count()
    
    # Simulate slow moving items (in real app, would analyze movement history)
    slow_moving_items = int(total_items * 0.1)  # Estimate 10% slow moving
    
    # Simulate counts (in real app, would query actual tables)
    categories_count = 12
    locations_count = 8
    
    return {
        "total_items": total_items,
        "total_value": total_value,
        "total_quantity": total_quantity,
        "low_stock_items": low_stock_items,
        "negative_stock_items": negative_stock_items,
        "slow_moving_items": slow_moving_items,
        "categories_count": categories_count,
        "locations_count": locations_count
    }


@router.get("/recent-movements")
async def get_recent_movements(
    db: Session = Depends(get_db)
) -> List[Dict[str, Any]]:
    """Get recent stock movements"""
    
    # For now, return mock data
    # In production, this would query actual stock_movements table
    return [
        {
            "id": 1,
            "item_code": "ITM001",
            "description": "Widget Assembly A1",
            "movement_type": "RECEIPT",
            "quantity": 100,
            "location": "WH-01",
            "date": datetime.utcnow().isoformat() + "Z",
            "reference": "PO-2024-0089"
        },
        {
            "id": 2,
            "item_code": "ITM002",
            "description": "Component B2",
            "movement_type": "ISSUE",
            "quantity": -50,
            "location": "WH-01",
            "date": (datetime.utcnow() - timedelta(hours=2)).isoformat() + "Z",
            "reference": "SO-2024-0156"
        },
        {
            "id": 3,
            "item_code": "ITM003",
            "description": "Raw Material C3",
            "movement_type": "TRANSFER",
            "quantity": 25,
            "location": "WH-02",
            "date": (datetime.utcnow() - timedelta(hours=5)).isoformat() + "Z",
            "reference": "TRF-2024-0034"
        }
    ]


@router.get("/items-list")
async def get_stock_items(
    skip: int = 0,
    limit: int = 100,
    db: Session = Depends(get_db)
) -> List[Dict[str, Any]]:
    """Get simplified stock items list without authentication"""
    
    items = db.query(StockRec).offset(skip).limit(limit).all()
    
    return [
        {
            "stock_key": item.stock_key,
            "stock_desc": item.stock_desc,
            "stock_qty_on_hand": float(item.stock_qty_on_hand),
            "stock_avg_cost": float(item.stock_avg_cost),
            "stock_reorder_point": float(item.stock_reorder_point),
            "stock_location": item.stock_location,
            "stock_unit_of_measure": item.stock_unit_of_measure,
            "is_low_stock": item.stock_qty_on_hand <= item.stock_reorder_point,
            "value": float(item.stock_qty_on_hand * item.stock_avg_cost)
        }
        for item in items
    ]


@router.get("/low-stock-items")
async def get_low_stock_items(
    db: Session = Depends(get_db)
) -> List[Dict[str, Any]]:
    """Get low stock items that need reordering"""
    
    low_stock_items = db.query(StockRec).filter(
        StockRec.stock_qty_on_hand <= StockRec.stock_reorder_point
    ).limit(10).all()
    
    return [
        {
            "item_code": item.stock_key,
            "description": item.stock_desc,
            "on_hand": float(item.stock_qty_on_hand),
            "reorder_point": float(item.stock_reorder_point),
            "reorder_quantity": float(item.stock_reorder_qty),
            "severity": "critical" if item.stock_qty_on_hand < 0 else "warning"
        }
        for item in low_stock_items
    ]