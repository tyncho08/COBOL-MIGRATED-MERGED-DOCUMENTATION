"""
COBOL-Compatible Stock Summary API
Designed to work with the actual migrated COBOL PostgreSQL database structure
"""
from fastapi import APIRouter, Depends, HTTPException
from sqlalchemy.orm import Session
from sqlalchemy import func, and_
from decimal import Decimal
from datetime import datetime, timedelta
from typing import Dict, List, Any

from app.core.database import get_db
from app.models.stock import StockRec

router = APIRouter()

@router.get("/summary")
async def get_cobol_stock_summary(db: Session = Depends(get_db)) -> Dict[str, Any]:
    """
    Get stock summary using only REAL COBOL database structure
    - Uses stock_rec (item master) for stock data
    - Compatible with frontend expectations
    """
    try:
        # === BASIC STOCK STATISTICS (using real COBOL data) ===
        
        # Total stock items
        total_items = db.query(func.count(StockRec.stock_key)).scalar() or 0
        
        # Total stock value (quantity on hand * average cost)
        total_value_query = db.query(
            func.sum(StockRec.stock_qty_on_hand * StockRec.stock_avg_cost)
        ).scalar() or Decimal('0.00')
        
        # Total quantity on hand
        total_quantity = db.query(
            func.sum(StockRec.stock_qty_on_hand)
        ).scalar() or Decimal('0.00')
        
        # Low stock items (where qty <= reorder point)
        low_stock_items = db.query(
            func.count(StockRec.stock_key)
        ).filter(
            and_(
                StockRec.stock_qty_on_hand <= StockRec.stock_reorder_point,
                StockRec.stock_reorder_point > 0
            )
        ).scalar() or 0
        
        # Negative stock items
        negative_stock_items = db.query(
            func.count(StockRec.stock_key)
        ).filter(
            StockRec.stock_qty_on_hand < 0
        ).scalar() or 0
        
        # Items with no movement (simplified - items with 0 qty)
        slow_moving_items = db.query(
            func.count(StockRec.stock_key)
        ).filter(
            StockRec.stock_qty_on_hand == 0
        ).scalar() or 0
        
        # Count distinct product groups (categories)
        categories_count = db.query(
            func.count(func.distinct(StockRec.stock_product_group))
        ).filter(
            StockRec.stock_product_group != ''
        ).scalar() or 0
        
        # Count distinct locations
        locations_count = db.query(
            func.count(func.distinct(StockRec.stock_location))
        ).filter(
            StockRec.stock_location != ''
        ).scalar() or 0
        
        # === TOP STOCK ITEMS BY VALUE ===
        
        top_items_query = db.query(
            StockRec.stock_key,
            StockRec.stock_desc,
            StockRec.stock_qty_on_hand,
            StockRec.stock_avg_cost,
            (StockRec.stock_qty_on_hand * StockRec.stock_avg_cost).label('total_value')
        ).filter(
            StockRec.stock_qty_on_hand > 0
        ).order_by(
            (StockRec.stock_qty_on_hand * StockRec.stock_avg_cost).desc()
        ).limit(10).all()
        
        top_items = []
        for item in top_items_query:
            top_items.append({
                "item_code": item.stock_key,
                "description": item.stock_desc,
                "quantity": float(item.stock_qty_on_hand or 0),
                "unit_cost": float(item.stock_avg_cost or 0),
                "total_value": float(item.total_value or 0)
            })
        
        # === LOW STOCK ALERTS ===
        
        low_stock_query = db.query(
            StockRec.stock_key,
            StockRec.stock_desc,
            StockRec.stock_qty_on_hand,
            StockRec.stock_reorder_point,
            StockRec.stock_reorder_qty
        ).filter(
            and_(
                StockRec.stock_qty_on_hand <= StockRec.stock_reorder_point,
                StockRec.stock_reorder_point > 0
            )
        ).order_by(
            StockRec.stock_qty_on_hand.asc()
        ).limit(10).all()
        
        low_stock_alerts = []
        for item in low_stock_query:
            low_stock_alerts.append({
                "item_code": item.stock_key,
                "description": item.stock_desc,
                "current_qty": float(item.stock_qty_on_hand or 0),
                "reorder_point": float(item.stock_reorder_point or 0),
                "reorder_qty": float(item.stock_reorder_qty or 0)
            })
        
        # === SUMMARY STRUCTURE COMPATIBLE WITH FRONTEND ===
        
        stock_summary = {
            # Original COBOL field names
            "total_items": total_items,
            "total_value": float(total_value_query),
            "total_quantity": float(total_quantity),
            "low_stock_items": low_stock_items,
            "negative_stock_items": negative_stock_items,
            "slow_moving_items": slow_moving_items,
            "categories_count": categories_count,
            "locations_count": locations_count,
            
            # Frontend compatibility fields
            "totalItems": total_items,
            "totalValue": float(total_value_query),
            "totalQuantity": float(total_quantity),
            "lowStockItems": low_stock_items,
            "negativeStockItems": negative_stock_items,
            "slowMovingItems": slow_moving_items,
            "categoriesCount": categories_count,
            "locationsCount": locations_count,
            "averageValue": float(total_value_query / total_items) if total_items > 0 else 0
        }
        
        return {
            "summary": stock_summary,
            "recentMovements": [],  # Empty for COBOL system - no movement history table
            "topItems": top_items,
            "lowStockAlerts": low_stock_alerts,
            "data_source": "COBOL migrated database (stock master records)",
            "timestamp": datetime.now().isoformat()
        }
        
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Database error: {str(e)}")

@router.get("/items")
async def get_stock_items(db: Session = Depends(get_db)) -> Dict[str, Any]:
    """
    Get all stock items using COBOL structure
    """
    try:
        items = db.query(
            StockRec.stock_key,
            StockRec.stock_desc,
            StockRec.stock_qty_on_hand,
            StockRec.stock_avg_cost,
            StockRec.stock_location,
            StockRec.stock_product_group,
            StockRec.stock_reorder_point,
            StockRec.stock_reorder_qty
        ).order_by(
            StockRec.stock_desc
        ).all()
        
        item_list = []
        total_value = Decimal('0.00')
        
        for item in items:
            item_value = (item.stock_qty_on_hand or 0) * (item.stock_avg_cost or 0)
            total_value += item_value
            
            item_list.append({
                "item_code": item.stock_key,
                "description": item.stock_desc,
                "quantity": float(item.stock_qty_on_hand or 0),
                "unit_cost": float(item.stock_avg_cost or 0),
                "total_value": float(item_value),
                "location": item.stock_location or '',
                "product_group": item.stock_product_group or '',
                "reorder_point": float(item.stock_reorder_point or 0),
                "reorder_qty": float(item.stock_reorder_qty or 0)
            })
        
        return {
            "items": item_list,
            "summary": {
                "total_items": len(item_list),
                "total_value": float(total_value)
            }
        }
        
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Database error: {str(e)}")