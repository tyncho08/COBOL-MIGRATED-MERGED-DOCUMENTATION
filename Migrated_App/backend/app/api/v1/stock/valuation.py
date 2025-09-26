"""Stock Valuation Management API endpoints - Real Implementation"""

from fastapi import APIRouter, Depends, HTTPException, status, Query, Body
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func, select, case
from typing import List, Optional, Dict, Any
from datetime import date, datetime, timedelta
from decimal import Decimal
import logging

from app.api import deps
from app.models import StockRec, StockMovementRec, StockLocationRec, StockAdjustmentRec, StockValuationRec
from app.schemas.stock import (
    StockValuation, ValuationSummary, CostingMethod,
    BatchAdjustment, SlowMovingStock, StockAnalysis,
    StockValuationBase
)
from app.core.gl_integration import GLIntegration
from app.core.exceptions import InsufficientPermissionsError

router = APIRouter()
logger = logging.getLogger(__name__)


@router.get("/current", response_model=StockValuation)
async def get_current_valuation(
    category_code: Optional[str] = Query(None, description="Filter by category"),
    location_code: Optional[str] = Query(None, description="Filter by location"),
    costing_method: Optional[CostingMethod] = Query(None, description="Filter by method"),
    min_value: Decimal = Query(0, description="Minimum value threshold"),
    include_zero_stock: bool = Query(False, description="Include zero stock items"),
    skip: int = 0,
    limit: int = 100,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Get current stock valuation with comprehensive calculations.
    
    Shows inventory value by item using configured costing method.
    Supports FIFO, LIFO, Average Cost, and Standard Cost methods.
    """
    try:
        # Build query for valuations
        query = db.query(
            Stock.item_code,
            StockItem.description,
            StockItem.category_code,
            StockItem.unit_of_measure,
            func.sum(Stock.quantity_on_hand).label("total_quantity"),
            func.sum(Stock.allocated_quantity).label("allocated_quantity"),
            case(
                (StockItem.costing_method == "FIFO", Stock.fifo_cost),
                (StockItem.costing_method == "LIFO", Stock.lifo_cost),
                (StockItem.costing_method == "AVERAGE", Stock.average_cost),
                else_=Stock.standard_cost
            ).label("unit_cost"),
            StockItem.costing_method
        ).join(
            StockItem, Stock.item_code == StockItem.item_code
        ).filter(
            Stock.is_active == True
        )
        
        # Apply filters
        if category_code:
            query = query.filter(StockItem.category_code == category_code)
        if location_code:
            query = query.filter(Stock.location_code == location_code)
        if costing_method:
            query = query.filter(StockItem.costing_method == costing_method)
        if not include_zero_stock:
            query = query.having(func.sum(Stock.quantity_on_hand) > 0)
            
        # Group by item
        query = query.group_by(
            Stock.item_code,
            StockItem.description,
            StockItem.category_code,
            StockItem.unit_of_measure,
            Stock.fifo_cost,
            Stock.lifo_cost,
            Stock.average_cost,
            Stock.standard_cost,
            StockItem.costing_method
        )
        
        # Calculate values and filter by min_value
        valuations = []
        total_value = Decimal(0)
        total_quantity = Decimal(0)
        
        for row in query.all():
            value = row.total_quantity * row.unit_cost
            if value >= min_value:
                valuations.append({
                    "item_code": row.item_code,
                    "description": row.description,
                    "category_code": row.category_code,
                    "unit_of_measure": row.unit_of_measure,
                    "quantity_on_hand": float(row.total_quantity),
                    "allocated_quantity": float(row.allocated_quantity or 0),
                    "available_quantity": float(row.total_quantity - (row.allocated_quantity or 0)),
                    "unit_cost": float(row.unit_cost),
                    "total_value": float(value),
                    "costing_method": row.costing_method,
                    "last_updated": datetime.now()
                })
                total_value += value
                total_quantity += row.total_quantity
        
        # Apply pagination
        paginated_valuations = valuations[skip:skip + limit]
        
        # Calculate summary statistics
        summary = {
            "total_items": len(valuations),
            "total_quantity": float(total_quantity),
            "total_value": float(total_value),
            "average_value_per_item": float(total_value / len(valuations)) if valuations else 0,
            "items_with_value": len([v for v in valuations if v["total_value"] > 0]),
            "items_below_min_stock": 0,  # TODO: Implement min stock level check
            "value_by_category": {}
        }
        
        # Calculate value by category
        category_values = {}
        for val in valuations:
            cat = val["category_code"] or "UNCATEGORIZED"
            if cat not in category_values:
                category_values[cat] = Decimal(0)
            category_values[cat] += Decimal(str(val["total_value"]))
        summary["value_by_category"] = {k: float(v) for k, v in category_values.items()}
        
        logger.info(f"Stock valuation retrieved by {current_user.email}: {len(valuations)} items")
        
        return StockValuationResponse(
            valuation_date=date.today(),
            summary=summary,
            items=paginated_valuations,
            item_count=len(paginated_valuations),
            total_count=len(valuations),
            skip=skip,
            limit=limit
        )
        
    except Exception as e:
        logger.error(f"Error retrieving stock valuation: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to retrieve stock valuation"
        )


@router.get("/summary", response_model=ValuationSummary)
async def get_valuation_summary(
    as_at_date: Optional[date] = Query(None, description="Valuation date"),
    group_by: str = Query("category", regex="^(category|location|costing_method)$"),
    include_analysis: bool = Query(True, description="Include analysis metrics"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Get comprehensive stock valuation summary with analytics.
    
    Provides high-level inventory valuation metrics grouped by category,
    location, or costing method with optional analysis.
    """
    try:
        valuation_date = as_at_date or date.today()
        
        # Base query for stock values
        base_query = db.query(
            func.count(distinct(Stock.item_code)).label("item_count"),
            func.sum(Stock.quantity_on_hand).label("total_quantity"),
            func.sum(
                Stock.quantity_on_hand * case(
                    (StockItem.costing_method == "FIFO", Stock.fifo_cost),
                    (StockItem.costing_method == "LIFO", Stock.lifo_cost),
                    (StockItem.costing_method == "AVERAGE", Stock.average_cost),
                    else_=Stock.standard_cost
                )
            ).label("total_value")
        ).join(
            StockItem, Stock.item_code == StockItem.item_code
        ).filter(
            Stock.is_active == True,
            Stock.quantity_on_hand > 0
        )
        
        # Group by requested field
        if group_by == "category":
            query = base_query.add_columns(StockItem.category_code.label("group_key"))
            query = query.group_by(StockItem.category_code)
        elif group_by == "location":
            query = base_query.add_columns(Stock.location_code.label("group_key"))
            query = query.group_by(Stock.location_code)
        elif group_by == "costing_method":
            query = base_query.add_columns(StockItem.costing_method.label("group_key"))
            query = query.group_by(StockItem.costing_method)
        
        # Execute query and build results
        grouped_data = []
        total_value = Decimal(0)
        total_quantity = Decimal(0)
        total_items = 0
        
        for row in query.all():
            grouped_data.append({
                "group_key": row.group_key or "UNSPECIFIED",
                "item_count": row.item_count,
                "total_quantity": float(row.total_quantity or 0),
                "total_value": float(row.total_value or 0),
                "percentage_of_value": 0  # Will calculate after
            })
            total_value += row.total_value or 0
            total_quantity += row.total_quantity or 0
            total_items += row.item_count
        
        # Calculate percentages
        for group in grouped_data:
            group["percentage_of_value"] = float(
                (Decimal(str(group["total_value"])) / total_value * 100)
            ) if total_value > 0 else 0
        
        # Build summary
        summary = ValuationSummary(
            valuation_date=valuation_date,
            total_items=total_items,
            total_quantity=float(total_quantity),
            total_value=float(total_value),
            average_value_per_item=float(total_value / total_items) if total_items > 0 else 0,
            group_by=group_by,
            grouped_data=grouped_data,
            currency="USD"  # TODO: Get from system settings
        )
        
        # Include analysis if requested
        if include_analysis:
            # Identify slow-moving stock (no movement in 90 days)
            slow_moving_query = db.query(
                Stock.item_code,
                func.sum(Stock.quantity_on_hand).label("quantity"),
                func.max(StockMovement.movement_date).label("last_movement")
            ).outerjoin(
                StockMovement,
                and_(
                    Stock.item_code == StockMovement.item_code,
                    Stock.location_code == StockMovement.location_code
                )
            ).filter(
                Stock.quantity_on_hand > 0
            ).group_by(
                Stock.item_code
            ).having(
                or_(
                    func.max(StockMovement.movement_date) < valuation_date - timedelta(days=90),
                    func.max(StockMovement.movement_date).is_(None)
                )
            )
            
            slow_moving_value = Decimal(0)
            slow_moving_items = 0
            
            for item in slow_moving_query.all():
                stock_value = db.query(
                    func.sum(
                        Stock.quantity_on_hand * case(
                            (StockItem.costing_method == "FIFO", Stock.fifo_cost),
                            (StockItem.costing_method == "LIFO", Stock.lifo_cost),
                            (StockItem.costing_method == "AVERAGE", Stock.average_cost),
                            else_=Stock.standard_cost
                        )
                    )
                ).join(
                    StockItem, Stock.item_code == StockItem.item_code
                ).filter(
                    Stock.item_code == item.item_code
                ).scalar() or 0
                
                slow_moving_value += stock_value
                slow_moving_items += 1
            
            # Calculate obsolete stock (no movement in 365 days)
            obsolete_query = slow_moving_query.having(
                or_(
                    func.max(StockMovement.movement_date) < valuation_date - timedelta(days=365),
                    func.max(StockMovement.movement_date).is_(None)
                )
            )
            
            obsolete_value = Decimal(0)
            obsolete_items = 0
            
            for item in obsolete_query.all():
                stock_value = db.query(
                    func.sum(
                        Stock.quantity_on_hand * case(
                            (StockItem.costing_method == "FIFO", Stock.fifo_cost),
                            (StockItem.costing_method == "LIFO", Stock.lifo_cost),
                            (StockItem.costing_method == "AVERAGE", Stock.average_cost),
                            else_=Stock.standard_cost
                        )
                    )
                ).join(
                    StockItem, Stock.item_code == StockItem.item_code
                ).filter(
                    Stock.item_code == item.item_code
                ).scalar() or 0
                
                obsolete_value += stock_value
                obsolete_items += 1
            
            summary.analysis = {
                "slow_moving_items": slow_moving_items,
                "slow_moving_value": float(slow_moving_value),
                "slow_moving_percentage": float(slow_moving_value / total_value * 100) if total_value > 0 else 0,
                "obsolete_items": obsolete_items,
                "obsolete_value": float(obsolete_value),
                "obsolete_percentage": float(obsolete_value / total_value * 100) if total_value > 0 else 0,
                "turnover_ratio": 0,  # TODO: Calculate based on COGS
                "days_inventory_outstanding": 0  # TODO: Calculate based on usage
            }
        
        logger.info(f"Valuation summary generated by {current_user.email} grouped by {group_by}")
        
        return summary
        
    except Exception as e:
        logger.error(f"Error generating valuation summary: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to generate valuation summary"
        )


@router.get("/history/{item_code}")
async def get_valuation_history(
    item_code: str,
    start_date: Optional[date] = Query(None, description="Start date"),
    end_date: Optional[date] = Query(None, description="End date"),
    include_movements: bool = Query(False, description="Include movement details"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Get comprehensive valuation history for an item.
    
    Shows cost changes, value fluctuations, and movement impacts over time.
    Includes opening/closing balances and percentage changes.
    """
    try:
        # Validate item exists
        item = db.query(StockItem).filter(
            StockItem.item_code == item_code,
            StockItem.is_active == True
        ).first()
        
        if not item:
            raise HTTPException(
                status_code=status.HTTP_404_NOT_FOUND,
                detail=f"Item {item_code} not found"
            )
        
        # Set date range
        if not end_date:
            end_date = date.today()
        if not start_date:
            start_date = end_date - timedelta(days=365)  # Default to 1 year history
        
        # Get cost adjustments history
        adjustments_query = db.query(
            StockAdjustment.adjustment_date,
            StockAdjustment.old_cost,
            StockAdjustment.new_cost,
            StockAdjustment.quantity_affected,
            StockAdjustment.adjustment_type,
            StockAdjustment.reason
        ).filter(
            StockAdjustment.item_code == item_code,
            StockAdjustment.adjustment_date.between(start_date, end_date)
        ).order_by(
            StockAdjustment.adjustment_date
        )
        
        # Get stock levels at start and end
        opening_stock = db.query(
            func.sum(Stock.quantity_on_hand).label("quantity"),
            func.avg(
                case(
                    (item.costing_method == "FIFO", Stock.fifo_cost),
                    (item.costing_method == "LIFO", Stock.lifo_cost),
                    (item.costing_method == "AVERAGE", Stock.average_cost),
                    else_=Stock.standard_cost
                )
            ).label("unit_cost")
        ).filter(
            Stock.item_code == item_code,
            Stock.last_movement_date < start_date
        ).first()
        
        closing_stock = db.query(
            func.sum(Stock.quantity_on_hand).label("quantity"),
            func.avg(
                case(
                    (item.costing_method == "FIFO", Stock.fifo_cost),
                    (item.costing_method == "LIFO", Stock.lifo_cost),
                    (item.costing_method == "AVERAGE", Stock.average_cost),
                    else_=Stock.standard_cost
                )
            ).label("unit_cost")
        ).filter(
            Stock.item_code == item_code,
            Stock.is_active == True
        ).first()
        
        # Calculate opening and closing values
        opening_qty = float(opening_stock.quantity or 0) if opening_stock else 0
        opening_cost = float(opening_stock.unit_cost or 0) if opening_stock else 0
        opening_value = opening_qty * opening_cost
        
        closing_qty = float(closing_stock.quantity or 0) if closing_stock else 0
        closing_cost = float(closing_stock.unit_cost or 0) if closing_stock else 0
        closing_value = closing_qty * closing_cost
        
        # Build valuation history points
        valuations = []
        current_cost = opening_cost
        
        # Add opening balance
        valuations.append({
            "date": start_date,
            "event_type": "opening_balance",
            "quantity": opening_qty,
            "unit_cost": opening_cost,
            "total_value": opening_value,
            "cost_change": 0,
            "value_change": 0
        })
        
        # Add adjustments
        for adj in adjustments_query.all():
            value_before = adj.quantity_affected * float(adj.old_cost)
            value_after = adj.quantity_affected * float(adj.new_cost)
            
            valuations.append({
                "date": adj.adjustment_date,
                "event_type": "cost_adjustment",
                "adjustment_type": adj.adjustment_type,
                "quantity": float(adj.quantity_affected),
                "unit_cost": float(adj.new_cost),
                "old_cost": float(adj.old_cost),
                "total_value": value_after,
                "cost_change": float(adj.new_cost - adj.old_cost),
                "value_change": value_after - value_before,
                "reason": adj.reason
            })
            current_cost = float(adj.new_cost)
        
        # Include movements if requested
        movements = []
        if include_movements:
            movements_query = db.query(
                StockMovement.movement_date,
                StockMovement.movement_type,
                StockMovement.quantity,
                StockMovement.unit_cost,
                StockMovement.reference_number,
                StockMovement.from_location,
                StockMovement.to_location
            ).filter(
                StockMovement.item_code == item_code,
                StockMovement.movement_date.between(start_date, end_date),
                StockMovement.status == "completed"
            ).order_by(
                StockMovement.movement_date
            )
            
            for mov in movements_query.all():
                movements.append({
                    "date": mov.movement_date,
                    "type": mov.movement_type,
                    "quantity": float(mov.quantity),
                    "unit_cost": float(mov.unit_cost),
                    "value": float(mov.quantity * mov.unit_cost),
                    "reference": mov.reference_number,
                    "from_location": mov.from_location,
                    "to_location": mov.to_location
                })
                
                # Add significant cost changes to valuations
                if abs(float(mov.unit_cost) - current_cost) > 0.01:
                    valuations.append({
                        "date": mov.movement_date,
                        "event_type": "cost_change_from_movement",
                        "movement_type": mov.movement_type,
                        "quantity": float(mov.quantity),
                        "unit_cost": float(mov.unit_cost),
                        "total_value": float(mov.quantity * mov.unit_cost),
                        "cost_change": float(mov.unit_cost - current_cost),
                        "reference": mov.reference_number
                    })
                    current_cost = float(mov.unit_cost)
        
        # Add closing balance
        valuations.append({
            "date": end_date,
            "event_type": "closing_balance",
            "quantity": closing_qty,
            "unit_cost": closing_cost,
            "total_value": closing_value,
            "cost_change": closing_cost - current_cost,
            "value_change": closing_value - opening_value
        })
        
        # Sort valuations by date
        valuations.sort(key=lambda x: x["date"])
        
        # Calculate summary metrics
        value_change = closing_value - opening_value
        value_change_percent = (value_change / opening_value * 100) if opening_value > 0 else 0
        
        logger.info(f"Valuation history retrieved for {item_code} by {current_user.email}")
        
        return {
            "item_code": item_code,
            "item_description": item.description,
            "costing_method": item.costing_method,
            "period": {
                "start": start_date,
                "end": end_date
            },
            "history_points": len(valuations),
            "opening_balance": {
                "quantity": opening_qty,
                "unit_cost": opening_cost,
                "value": opening_value
            },
            "closing_balance": {
                "quantity": closing_qty,
                "unit_cost": closing_cost,
                "value": closing_value
            },
            "value_change": value_change,
            "value_change_percent": value_change_percent,
            "average_cost_in_period": sum(v["unit_cost"] for v in valuations) / len(valuations) if valuations else 0,
            "valuations": valuations,
            "movements": movements
        }
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error retrieving valuation history for {item_code}: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to retrieve valuation history"
        )


@router.post("/adjust-cost")
async def adjust_item_cost(
    item_code: str = Body(...),
    new_cost: Decimal = Body(..., ge=0),
    adjustment_type: str = Body(..., regex="^(revaluation|correction|write_down|write_up)$"),
    reason: str = Body(..., min_length=10),
    effective_date: Optional[date] = Body(None),
    apply_to_location: Optional[str] = Body(None),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Adjust item cost with full GL integration and audit trail.
    
    Creates cost adjustment, updates inventory values, and posts to GL.
    Requires admin privileges and may need additional approval for large adjustments.
    """
    try:
        # Validate item exists and is active
        item = db.query(StockItem).filter(
            StockItem.item_code == item_code,
            StockItem.is_active == True
        ).first()
        
        if not item:
            raise HTTPException(
                status_code=status.HTTP_404_NOT_FOUND,
                detail=f"Item {item_code} not found or inactive"
            )
        
        # Build query for current stock
        stock_query = db.query(
            func.sum(Stock.quantity_on_hand).label("total_quantity"),
            func.avg(
                case(
                    (item.costing_method == "FIFO", Stock.fifo_cost),
                    (item.costing_method == "LIFO", Stock.lifo_cost),
                    (item.costing_method == "AVERAGE", Stock.average_cost),
                    else_=Stock.standard_cost
                )
            ).label("current_cost")
        ).filter(
            Stock.item_code == item_code,
            Stock.quantity_on_hand > 0
        )
        
        # Apply location filter if specified
        if apply_to_location:
            location = db.query(Location).filter(
                Location.location_code == apply_to_location,
                Location.is_active == True
            ).first()
            
            if not location:
                raise HTTPException(
                    status_code=status.HTTP_404_NOT_FOUND,
                    detail=f"Location {apply_to_location} not found or inactive"
                )
            
            stock_query = stock_query.filter(Stock.location_code == apply_to_location)
        
        current_stock = stock_query.first()
        
        if not current_stock or not current_stock.total_quantity:
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail=f"No stock available to revalue" + 
                       (f" at location {apply_to_location}" if apply_to_location else "")
            )
        
        quantity_affected = float(current_stock.total_quantity)
        old_cost = float(current_stock.current_cost)
        
        # Calculate adjustment value
        old_value = Decimal(str(quantity_affected * old_cost))
        new_value = Decimal(str(quantity_affected * float(new_cost)))
        adjustment_value = new_value - old_value
        
        # Check approval limits
        approval_threshold = Decimal("10000")  # TODO: Get from system settings
        if abs(adjustment_value) > approval_threshold:
            # Check if user has override permission
            if not current_user.has_permission("override_approval_limits"):
                raise HTTPException(
                    status_code=status.HTTP_403_FORBIDDEN,
                    detail=f"Cost adjustment of ${abs(adjustment_value):,.2f} exceeds approval limit of ${approval_threshold:,.2f}"
                )
        
        # Begin transaction
        try:
            # Create adjustment record
            adjustment = StockAdjustment(
                adjustment_type="cost_" + adjustment_type,
                item_code=item_code,
                location_code=apply_to_location,
                adjustment_date=effective_date or date.today(),
                quantity_affected=Decimal(str(quantity_affected)),
                old_cost=Decimal(str(old_cost)),
                new_cost=new_cost,
                value_adjustment=adjustment_value,
                reason=reason,
                reference_number=f"COST-{datetime.now().strftime('%Y%m%d%H%M%S')}",
                created_by=current_user.id,
                approved_by=current_user.id if abs(adjustment_value) <= approval_threshold else None,
                status="posted"
            )
            db.add(adjustment)
            
            # Update stock costs
            stocks_to_update = db.query(Stock).filter(
                Stock.item_code == item_code,
                Stock.quantity_on_hand > 0
            )
            
            if apply_to_location:
                stocks_to_update = stocks_to_update.filter(
                    Stock.location_code == apply_to_location
                )
            
            for stock in stocks_to_update.all():
                # Update cost based on costing method
                if item.costing_method == "FIFO":
                    stock.fifo_cost = new_cost
                elif item.costing_method == "LIFO":
                    stock.lifo_cost = new_cost
                elif item.costing_method == "AVERAGE":
                    stock.average_cost = new_cost
                else:  # Standard cost
                    stock.standard_cost = new_cost
                
                stock.last_cost_update = datetime.now()
                stock.updated_by = current_user.id
            
            # Create GL entries
            gl_entries = []
            
            # Determine accounts based on adjustment type
            if adjustment_type == "revaluation":
                if adjustment_value > 0:
                    debit_account = "1310"  # Inventory
                    credit_account = "5920"  # Inventory Revaluation Gain
                else:
                    debit_account = "5930"  # Inventory Revaluation Loss
                    credit_account = "1310"  # Inventory
            elif adjustment_type == "write_down":
                debit_account = "5910"  # Inventory Write-Down
                credit_account = "1310"  # Inventory
            elif adjustment_type == "write_up":
                debit_account = "1310"  # Inventory
                credit_account = "5920"  # Inventory Write-Up Gain
            else:  # correction
                if adjustment_value > 0:
                    debit_account = "1310"  # Inventory
                    credit_account = "5940"  # Inventory Adjustment
                else:
                    debit_account = "5940"  # Inventory Adjustment
                    credit_account = "1310"  # Inventory
            
            # Post GL entries
            gl_result = gl_integration.post_inventory_adjustment(
                db,
                adjustment_id=adjustment.id,
                debit_account=debit_account,
                credit_account=credit_account,
                amount=abs(adjustment_value),
                description=f"Cost adjustment: {item_code} - {adjustment_type}",
                reference=adjustment.reference_number,
                posted_by=current_user.id
            )
            
            gl_entries.extend(gl_result["entries"])
            
            db.commit()
            
            logger.info(
                f"Cost adjustment completed: {item_code} from ${old_cost:.4f} to ${float(new_cost):.4f} "
                f"affecting {quantity_affected} units by {current_user.email}"
            )
            
            return {
                "adjustment_id": adjustment.id,
                "reference_number": adjustment.reference_number,
                "item_code": item_code,
                "old_cost": old_cost,
                "new_cost": float(new_cost),
                "quantity_affected": quantity_affected,
                "value_adjustment": float(adjustment_value),
                "gl_entries": gl_entries,
                "effective_date": adjustment.adjustment_date,
                "status": "posted",
                "location_affected": apply_to_location or "ALL"
            }
            
        except Exception as e:
            db.rollback()
            raise e
            
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error adjusting cost for {item_code}: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to process cost adjustment"
        )


@router.get("/variance-report")
async def get_cost_variance_report(
    variance_threshold: Decimal = Query(10.0, description="Minimum variance % to report"),
    period_days: int = Query(30, ge=1, le=365, description="Analysis period in days"),
    category_code: Optional[str] = Query(None),
    location_code: Optional[str] = Query(None),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Get comprehensive cost variance analysis report.
    
    Identifies items with significant cost changes, analyzes trends,
    and provides actionable insights for inventory management.
    """
    try:
        end_date = date.today()
        start_date = end_date - timedelta(days=period_days)
        
        # Query for cost changes in the period
        variance_query = db.query(
            StockAdjustment.item_code,
            StockItem.description,
            StockItem.category_code,
            func.min(StockAdjustment.old_cost).label("min_cost"),
            func.max(StockAdjustment.new_cost).label("max_cost"),
            func.avg(StockAdjustment.old_cost).label("avg_old_cost"),
            func.avg(StockAdjustment.new_cost).label("avg_new_cost"),
            func.count(StockAdjustment.id).label("adjustment_count"),
            func.sum(StockAdjustment.value_adjustment).label("total_adjustment")
        ).join(
            StockItem, StockAdjustment.item_code == StockItem.item_code
        ).filter(
            StockAdjustment.adjustment_date.between(start_date, end_date),
            StockAdjustment.adjustment_type.like("cost_%")
        )
        
        # Apply filters
        if category_code:
            variance_query = variance_query.filter(StockItem.category_code == category_code)
        if location_code:
            variance_query = variance_query.filter(StockAdjustment.location_code == location_code)
        
        variance_query = variance_query.group_by(
            StockAdjustment.item_code,
            StockItem.description,
            StockItem.category_code
        )
        
        # Process variances
        variances = []
        price_increases = []
        price_decreases = []
        high_volatility = []
        
        for row in variance_query.all():
            # Calculate variance percentage
            if row.avg_old_cost > 0:
                variance_pct = ((row.avg_new_cost - row.avg_old_cost) / row.avg_old_cost * 100)
            else:
                variance_pct = 100 if row.avg_new_cost > 0 else 0
            
            # Only include if exceeds threshold
            if abs(variance_pct) >= float(variance_threshold):
                # Get current stock value for impact calculation
                current_stock = db.query(
                    func.sum(Stock.quantity_on_hand).label("quantity")
                ).filter(
                    Stock.item_code == row.item_code
                ).scalar() or 0
                
                value_impact = current_stock * (row.avg_new_cost - row.avg_old_cost)
                
                variance_data = {
                    "item_code": row.item_code,
                    "description": row.description,
                    "category_code": row.category_code,
                    "variance_type": "price_change",
                    "old_cost": float(row.avg_old_cost),
                    "new_cost": float(row.avg_new_cost),
                    "variance_amount": float(row.avg_new_cost - row.avg_old_cost),
                    "variance_percent": float(variance_pct),
                    "min_cost_in_period": float(row.min_cost),
                    "max_cost_in_period": float(row.max_cost),
                    "adjustment_count": row.adjustment_count,
                    "current_stock_quantity": float(current_stock),
                    "value_impact": float(value_impact),
                    "total_adjustments": float(row.total_adjustment or 0)
                }
                
                variances.append(variance_data)
                
                # Categorize
                if variance_pct > 0:
                    price_increases.append(variance_data)
                else:
                    price_decreases.append(variance_data)
                
                # Check for high volatility (multiple adjustments)
                if row.adjustment_count >= 3:
                    volatility = (row.max_cost - row.min_cost) / row.avg_old_cost * 100 if row.avg_old_cost > 0 else 0
                    if volatility > float(variance_threshold) * 2:
                        variance_data["volatility_score"] = float(volatility)
                        high_volatility.append(variance_data)
        
        # Check standard vs actual costs for standard cost items
        standard_variances = []
        std_items = db.query(
            Stock.item_code,
            StockItem.description,
            StockItem.standard_cost,
            func.avg(Stock.average_cost).label("actual_avg_cost"),
            func.sum(Stock.quantity_on_hand).label("quantity")
        ).join(
            StockItem, Stock.item_code == StockItem.item_code
        ).filter(
            StockItem.costing_method == "STANDARD",
            StockItem.standard_cost > 0,
            Stock.quantity_on_hand > 0
        ).group_by(
            Stock.item_code,
            StockItem.description,
            StockItem.standard_cost
        )
        
        for item in std_items.all():
            if item.actual_avg_cost and item.standard_cost:
                std_variance_pct = ((item.actual_avg_cost - item.standard_cost) / item.standard_cost * 100)
                if abs(std_variance_pct) >= float(variance_threshold):
                    std_variance = {
                        "item_code": item.item_code,
                        "description": item.description,
                        "variance_type": "standard_vs_actual",
                        "standard_cost": float(item.standard_cost),
                        "actual_cost": float(item.actual_avg_cost),
                        "variance_amount": float(item.actual_avg_cost - item.standard_cost),
                        "variance_percent": float(std_variance_pct),
                        "current_stock_quantity": float(item.quantity),
                        "value_impact": float(item.quantity * (item.actual_avg_cost - item.standard_cost))
                    }
                    standard_variances.append(std_variance)
                    variances.append(std_variance)
        
        # Calculate summary statistics
        total_impact = sum(v["value_impact"] for v in variances)
        
        # Sort and get top variances
        top_by_impact = sorted(variances, key=lambda x: abs(x["value_impact"]), reverse=True)[:10]
        top_by_percent = sorted(variances, key=lambda x: abs(x["variance_percent"]), reverse=True)[:10]
        
        # Generate insights
        insights = []
        if len(price_increases) > len(price_decreases) * 2:
            insights.append({
                "type": "trend",
                "severity": "high",
                "message": f"Significant inflationary trend: {len(price_increases)} items with cost increases vs {len(price_decreases)} with decreases",
                "action": "Review pricing strategy and consider forward buying for high-impact items"
            })
        
        if high_volatility:
            insights.append({
                "type": "volatility",
                "severity": "medium",
                "message": f"{len(high_volatility)} items show high cost volatility with multiple adjustments",
                "action": "Investigate supply chain stability and consider alternative suppliers"
            })
        
        if standard_variances:
            avg_std_variance = sum(abs(v["variance_percent"]) for v in standard_variances) / len(standard_variances)
            if avg_std_variance > 15:
                insights.append({
                    "type": "standard_cost",
                    "severity": "high",
                    "message": f"Standard costs significantly out of date: average variance {avg_std_variance:.1f}%",
                    "action": "Update standard costs to reflect current market conditions"
                })
        
        logger.info(f"Variance report generated by {current_user.email}: {len(variances)} variances found")
        
        return {
            "report_date": date.today(),
            "period": {
                "start": start_date,
                "end": end_date,
                "days": period_days
            },
            "variance_threshold": float(variance_threshold),
            "total_variances": len(variances),
            "total_impact": float(total_impact),
            "summary": {
                "price_increases": len(price_increases),
                "price_decreases": len(price_decreases),
                "high_volatility_items": len(high_volatility),
                "standard_variances": len(standard_variances),
                "average_variance_percent": float(sum(abs(v["variance_percent"]) for v in variances) / len(variances)) if variances else 0
            },
            "variances_by_type": {
                "price_increases": sorted(price_increases, key=lambda x: x["variance_percent"], reverse=True),
                "price_decreases": sorted(price_decreases, key=lambda x: abs(x["variance_percent"]), reverse=True),
                "high_volatility": sorted(high_volatility, key=lambda x: x.get("volatility_score", 0), reverse=True),
                "standard_vs_actual": sorted(standard_variances, key=lambda x: abs(x["variance_percent"]), reverse=True)
            },
            "top_variances_by_impact": top_by_impact,
            "top_variances_by_percent": top_by_percent,
            "insights": insights
        }
        
    except Exception as e:
        logger.error(f"Error generating variance report: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to generate variance report"
        )


@router.post("/recalculate")
async def recalculate_costs(
    item_codes: Optional[List[str]] = Body(None, description="Specific items to recalculate"),
    category_code: Optional[str] = Body(None, description="Recalculate category"),
    from_date: Optional[date] = Body(None, description="Recalculate from date"),
    force_recalc: bool = Body(False, description="Force recalculation"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Recalculate item costs based on costing method and movement history.
    
    Reprocesses FIFO, LIFO, or Average costs from historical movements.
    Critical for correcting cost discrepancies and period-end processing.
    """
    try:
        # Validate input
        if not item_codes and not category_code:
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail="Specify either item_codes or category_code"
            )
        
        # Get items to recalculate
        if item_codes:
            # Validate all items exist
            items = db.query(StockItem).filter(
                StockItem.item_code.in_(item_codes),
                StockItem.is_active == True
            ).all()
            
            if len(items) != len(item_codes):
                found_codes = [item.item_code for item in items]
                missing = set(item_codes) - set(found_codes)
                raise HTTPException(
                    status_code=status.HTTP_404_NOT_FOUND,
                    detail=f"Items not found: {', '.join(missing)}"
                )
            
            items_to_process = item_codes
        else:
            # Get all items in category
            items = db.query(StockItem.item_code).filter(
                StockItem.category_code == category_code,
                StockItem.is_active == True
            ).all()
            
            if not items:
                raise HTTPException(
                    status_code=status.HTTP_404_NOT_FOUND,
                    detail=f"No active items found in category {category_code}"
                )
            
            items_to_process = [item.item_code for item in items]
        
        # Set calculation date range
        if not from_date:
            from_date = date.today().replace(day=1)  # Default to start of current month
        
        # Check if recalculation is needed (unless forced)
        if not force_recalc:
            # Check for movements since last calculation
            recent_movements = db.query(
                func.count(StockMovement.id)
            ).filter(
                StockMovement.item_code.in_(items_to_process),
                StockMovement.movement_date >= from_date,
                StockMovement.status == "completed"
            ).scalar() or 0
            
            if recent_movements == 0:
                return {
                    "message": "No movements found requiring recalculation",
                    "items_checked": len(items_to_process),
                    "from_date": from_date,
                    "skipped": True
                }
        
        # Start recalculation process
        start_time = datetime.now()
        batch_id = f"RECALC-{start_time.strftime('%Y%m%d%H%M%S')}"
        
        items_processed = 0
        items_updated = 0
        total_adjustments = 0
        total_value_impact = Decimal(0)
        errors = []
        
        logger.info(f"Starting cost recalculation batch {batch_id} for {len(items_to_process)} items")
        
        for item_code in items_to_process:
            try:
                # Get item details
                item = db.query(StockItem).filter(
                    StockItem.item_code == item_code
                ).first()
                
                if item.costing_method == "STANDARD":
                    # Skip standard cost items
                    continue
                
                items_processed += 1
                
                # Get all movements from the start date
                movements = db.query(StockMovement).filter(
                    StockMovement.item_code == item_code,
                    StockMovement.movement_date >= from_date,
                    StockMovement.status == "completed"
                ).order_by(
                    StockMovement.movement_date,
                    StockMovement.id
                ).all()
                
                if not movements:
                    continue
                
                # Get opening balance
                opening_balance = db.query(
                    func.sum(Stock.quantity_on_hand).label("quantity"),
                    func.avg(
                        case(
                            (item.costing_method == "FIFO", Stock.fifo_cost),
                            (item.costing_method == "LIFO", Stock.lifo_cost),
                            else_=Stock.average_cost
                        )
                    ).label("cost")
                ).filter(
                    Stock.item_code == item_code
                ).first()
                
                # Recalculate based on method
                if item.costing_method == "AVERAGE":
                    # Average cost calculation
                    total_qty = Decimal(str(opening_balance.quantity or 0))
                    total_value = total_qty * Decimal(str(opening_balance.cost or 0))
                    
                    for movement in movements:
                        if movement.movement_type in ["RECEIPT", "RETURN"]:
                            # Add to inventory
                            total_qty += movement.quantity
                            total_value += movement.quantity * movement.unit_cost
                        elif movement.movement_type in ["ISSUE", "TRANSFER_OUT"]:
                            # Remove from inventory at average cost
                            if total_qty > 0:
                                avg_cost = total_value / total_qty
                                total_qty -= movement.quantity
                                total_value -= movement.quantity * avg_cost
                    
                    # Update average cost
                    new_avg_cost = total_value / total_qty if total_qty > 0 else Decimal(0)
                    
                    # Update all stock records
                    db.query(Stock).filter(
                        Stock.item_code == item_code
                    ).update({
                        "average_cost": new_avg_cost,
                        "last_cost_update": datetime.now()
                    })
                    
                    items_updated += 1
                    
                elif item.costing_method == "FIFO":
                    # FIFO calculation - maintain cost layers
                    cost_layers = []  # [(quantity, unit_cost), ...]
                    
                    # TODO: Implement full FIFO layer tracking
                    # This is a simplified version
                    
                    # Get most recent receipt cost
                    latest_receipt = db.query(StockMovement).filter(
                        StockMovement.item_code == item_code,
                        StockMovement.movement_type.in_(["RECEIPT", "RETURN"]),
                        StockMovement.status == "completed"
                    ).order_by(
                        StockMovement.movement_date.desc()
                    ).first()
                    
                    if latest_receipt:
                        db.query(Stock).filter(
                            Stock.item_code == item_code
                        ).update({
                            "fifo_cost": latest_receipt.unit_cost,
                            "last_cost_update": datetime.now()
                        })
                        items_updated += 1
                    
                elif item.costing_method == "LIFO":
                    # LIFO calculation - use most recent costs first
                    # Get most recent receipt cost
                    latest_receipt = db.query(StockMovement).filter(
                        StockMovement.item_code == item_code,
                        StockMovement.movement_type.in_(["RECEIPT", "RETURN"]),
                        StockMovement.status == "completed"
                    ).order_by(
                        StockMovement.movement_date.desc()
                    ).first()
                    
                    if latest_receipt:
                        db.query(Stock).filter(
                            Stock.item_code == item_code
                        ).update({
                            "lifo_cost": latest_receipt.unit_cost,
                            "last_cost_update": datetime.now()
                        })
                        items_updated += 1
                
                # Calculate value impact
                # TODO: Track actual value changes
                total_adjustments += 1
                
            except Exception as e:
                errors.append({
                    "item_code": item_code,
                    "error": str(e)
                })
                logger.error(f"Error recalculating {item_code}: {str(e)}")
                continue
        
        # Commit all changes
        db.commit()
        
        # Calculate processing time
        processing_time = (datetime.now() - start_time).total_seconds()
        
        logger.info(
            f"Cost recalculation {batch_id} completed: "
            f"{items_processed} processed, {items_updated} updated in {processing_time:.2f} seconds"
        )
        
        return {
            "recalculation_id": batch_id,
            "status": "completed",
            "items_processed": items_processed,
            "items_updated": items_updated,
            "items_skipped": items_processed - items_updated,
            "total_adjustments": total_adjustments,
            "value_impact": float(total_value_impact),
            "processing_time": processing_time,
            "from_date": from_date,
            "errors": errors,
            "forced": force_recalc
        }
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error in cost recalculation: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to process cost recalculation"
        )


@router.get("/slow-moving")
async def get_slow_moving_analysis(
    days_threshold: int = Query(90, ge=30, le=365, description="Days since last movement"),
    min_value: Decimal = Query(100, ge=0, description="Minimum value to report"),
    category_code: Optional[str] = None,
    location_code: Optional[str] = None,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Comprehensive slow-moving and obsolete stock analysis.
    
    Identifies low-turnover items, calculates carrying costs,
    and provides disposal recommendations.
    """
    try:
        analysis_date = date.today()
        cutoff_date = analysis_date - timedelta(days=days_threshold)
        
        # Query for items with no recent movements
        slow_query = db.query(
            Stock.item_code,
            StockItem.description,
            StockItem.category_code,
            Stock.location_code,
            func.sum(Stock.quantity_on_hand).label("quantity"),
            func.avg(
                case(
                    (StockItem.costing_method == "FIFO", Stock.fifo_cost),
                    (StockItem.costing_method == "LIFO", Stock.lifo_cost),
                    (StockItem.costing_method == "AVERAGE", Stock.average_cost),
                    else_=Stock.standard_cost
                )
            ).label("unit_cost"),
            func.max(StockMovement.movement_date).label("last_movement_date"),
            StockItem.obsolete_flag,
            StockItem.disposal_code
        ).join(
            StockItem, Stock.item_code == StockItem.item_code
        ).outerjoin(
            StockMovement,
            and_(
                Stock.item_code == StockMovement.item_code,
                Stock.location_code == StockMovement.location_code,
                StockMovement.movement_type.in_(["ISSUE", "TRANSFER_OUT", "SALE"])
            )
        ).filter(
            Stock.quantity_on_hand > 0,
            Stock.is_active == True
        )
        
        # Apply filters
        if category_code:
            slow_query = slow_query.filter(StockItem.category_code == category_code)
        if location_code:
            slow_query = slow_query.filter(Stock.location_code == location_code)
        
        # Group and filter by last movement
        slow_query = slow_query.group_by(
            Stock.item_code,
            StockItem.description,
            StockItem.category_code,
            Stock.location_code,
            StockItem.obsolete_flag,
            StockItem.disposal_code
        ).having(
            or_(
                func.max(StockMovement.movement_date) < cutoff_date,
                func.max(StockMovement.movement_date).is_(None)
            )
        )
        
        # Process results
        slow_items = []
        total_value = Decimal(0)
        by_age = {
            "90_180_days": [],
            "180_365_days": [],
            "over_365_days": [],
            "never_moved": []
        }
        
        for item in slow_query.all():
            item_value = Decimal(str(item.quantity * (item.unit_cost or 0)))
            
            if item_value < min_value:
                continue
            
            # Calculate days since movement
            if item.last_movement_date:
                days_since = (analysis_date - item.last_movement_date).days
            else:
                days_since = 9999  # Never moved
            
            # Calculate carrying cost (simplified)
            annual_carrying_rate = Decimal("0.25")  # 25% per year (storage, insurance, etc.)
            days_held = min(days_since, 365)
            carrying_cost = item_value * annual_carrying_rate * (Decimal(days_held) / 365)
            
            slow_item = {
                "item_code": item.item_code,
                "description": item.description,
                "category_code": item.category_code,
                "location_code": item.location_code,
                "quantity": float(item.quantity),
                "unit_cost": float(item.unit_cost or 0),
                "total_value": float(item_value),
                "last_movement_date": item.last_movement_date,
                "days_since_movement": days_since,
                "carrying_cost": float(carrying_cost),
                "obsolete_flag": item.obsolete_flag,
                "disposal_code": item.disposal_code
            }
            
            slow_items.append(slow_item)
            total_value += item_value
            
            # Categorize by age
            if days_since == 9999:
                by_age["never_moved"].append(slow_item)
            elif days_since >= 365:
                by_age["over_365_days"].append(slow_item)
            elif days_since >= 180:
                by_age["180_365_days"].append(slow_item)
            else:
                by_age["90_180_days"].append(slow_item)
        
        # Calculate disposal recommendations
        disposal_options = {
            "liquidation": [],
            "donation": [],
            "scrap": [],
            "markdown": [],
            "return_to_vendor": []
        }
        
        total_recovery = Decimal(0)
        
        for item in slow_items:
            # Determine best disposal option
            if item["days_since_movement"] > 365:
                if item["total_value"] < 500:
                    # Small value - consider donation or scrap
                    recovery_rate = Decimal("0.1") if item["obsolete_flag"] else Decimal("0.2")
                    disposal_options["donation" if recovery_rate < Decimal("0.15") else "scrap"].append({
                        "item_code": item["item_code"],
                        "quantity": item["quantity"],
                        "book_value": item["total_value"],
                        "recovery_value": float(Decimal(str(item["total_value"])) * recovery_rate),
                        "recovery_rate": float(recovery_rate * 100)
                    })
                else:
                    # Higher value - try liquidation
                    recovery_rate = Decimal("0.3") if item["obsolete_flag"] else Decimal("0.5")
                    disposal_options["liquidation"].append({
                        "item_code": item["item_code"],
                        "quantity": item["quantity"],
                        "book_value": item["total_value"],
                        "recovery_value": float(Decimal(str(item["total_value"])) * recovery_rate),
                        "recovery_rate": float(recovery_rate * 100)
                    })
                
                total_recovery += Decimal(str(item["total_value"])) * recovery_rate
                
            elif item["days_since_movement"] > 180:
                # Consider markdown for faster movement
                markdown_pct = Decimal("0.25")
                recovery_value = Decimal(str(item["total_value"])) * (1 - markdown_pct)
                disposal_options["markdown"].append({
                    "item_code": item["item_code"],
                    "quantity": item["quantity"],
                    "book_value": item["total_value"],
                    "markdown_percent": float(markdown_pct * 100),
                    "recovery_value": float(recovery_value)
                })
                total_recovery += recovery_value
        
        # Calculate summary metrics
        total_carrying_cost = sum(item["carrying_cost"] for item in slow_items)
        
        # Generate insights
        insights = []
        if len(by_age["never_moved"]) > 0:
            insights.append({
                "type": "critical",
                "message": f"{len(by_age['never_moved'])} items have never moved since receipt",
                "action": "Review purchasing decisions and consider immediate disposal"
            })
        
        if total_carrying_cost > total_value * Decimal("0.1"):
            insights.append({
                "type": "warning",
                "message": f"Carrying costs ({float(total_carrying_cost):,.2f}) exceed 10% of inventory value",
                "action": "Accelerate disposal to reduce holding costs"
            })
        
        logger.info(f"Slow-moving analysis completed by {current_user.email}: {len(slow_items)} items identified")
        
        return {
            "analysis_date": analysis_date,
            "days_threshold": days_threshold,
            "min_value_threshold": float(min_value),
            "total_items": len(slow_items),
            "total_value": float(total_value),
            "total_carrying_cost": float(total_carrying_cost),
            "by_age_bracket": {
                "90_180_days": {
                    "items": by_age["90_180_days"],
                    "count": len(by_age["90_180_days"]),
                    "value": sum(i["total_value"] for i in by_age["90_180_days"])
                },
                "180_365_days": {
                    "items": by_age["180_365_days"],
                    "count": len(by_age["180_365_days"]),
                    "value": sum(i["total_value"] for i in by_age["180_365_days"])
                },
                "over_365_days": {
                    "items": by_age["over_365_days"],
                    "count": len(by_age["over_365_days"]),
                    "value": sum(i["total_value"] for i in by_age["over_365_days"])
                },
                "never_moved": {
                    "items": by_age["never_moved"],
                    "count": len(by_age["never_moved"]),
                    "value": sum(i["total_value"] for i in by_age["never_moved"])
                }
            },
            "disposal_recommendations": disposal_options,
            "total_potential_recovery": float(total_recovery),
            "recovery_percentage": float(total_recovery / total_value * 100) if total_value > 0 else 0,
            "insights": insights,
            "top_value_items": sorted(slow_items, key=lambda x: x["total_value"], reverse=True)[:20]
        }
        
    except Exception as e:
        logger.error(f"Error in slow-moving analysis: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to perform slow-moving analysis"
        )


@router.get("/abc-analysis")
async def get_abc_analysis(
    analysis_basis: str = Query("value", regex="^(value|quantity|transactions)$"),
    a_threshold: Decimal = Query(80, description="A items threshold %"),
    b_threshold: Decimal = Query(15, description="B items threshold %"),
    period_months: int = Query(12, ge=1, le=24),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Comprehensive ABC analysis for inventory optimization.
    
    Categorizes items by importance using value, quantity, or transaction frequency.
    Provides management recommendations for each category.
    """
    try:
        analysis_date = date.today()
        start_date = analysis_date - timedelta(days=period_months * 30)
        
        # Base query for all active items with stock
        base_query = db.query(
            Stock.item_code,
            StockItem.description,
            StockItem.category_code,
            func.sum(Stock.quantity_on_hand).label("current_quantity"),
            func.sum(
                Stock.quantity_on_hand * case(
                    (StockItem.costing_method == "FIFO", Stock.fifo_cost),
                    (StockItem.costing_method == "LIFO", Stock.lifo_cost),
                    (StockItem.costing_method == "AVERAGE", Stock.average_cost),
                    else_=Stock.standard_cost
                )
            ).label("current_value")
        ).join(
            StockItem, Stock.item_code == StockItem.item_code
        ).filter(
            Stock.quantity_on_hand > 0,
            Stock.is_active == True
        ).group_by(
            Stock.item_code,
            StockItem.description,
            StockItem.category_code
        )
        
        # Add analysis basis specific data
        if analysis_basis == "value":
            # Use current inventory value
            analysis_query = base_query
            sort_column = "current_value"
        elif analysis_basis == "quantity":
            # Use current quantity on hand
            analysis_query = base_query
            sort_column = "current_quantity"
        else:  # transactions
            # Count transactions in period
            analysis_query = base_query.add_columns(
                func.count(StockMovement.id).label("transaction_count")
            ).outerjoin(
                StockMovement,
                and_(
                    Stock.item_code == StockMovement.item_code,
                    StockMovement.movement_date >= start_date,
                    StockMovement.status == "completed"
                )
            ).group_by(
                Stock.item_code,
                StockItem.description,
                StockItem.category_code
            )
            sort_column = "transaction_count"
        
        # Execute query and sort by analysis basis
        items = []
        for row in analysis_query.all():
            item_data = {
                "item_code": row.item_code,
                "description": row.description,
                "category_code": row.category_code,
                "current_quantity": float(row.current_quantity or 0),
                "current_value": float(row.current_value or 0)
            }
            
            if analysis_basis == "value":
                item_data["analysis_value"] = float(row.current_value or 0)
            elif analysis_basis == "quantity":
                item_data["analysis_value"] = float(row.current_quantity or 0)
            else:  # transactions
                item_data["transaction_count"] = getattr(row, "transaction_count", 0) or 0
                item_data["analysis_value"] = item_data["transaction_count"]
            
            items.append(item_data)
        
        # Sort items by analysis value (descending)
        items.sort(key=lambda x: x["analysis_value"], reverse=True)
        
        if not items:
            return {
                "analysis_date": analysis_date,
                "message": "No items found for ABC analysis",
                "items_analyzed": 0
            }
        
        # Calculate cumulative percentages
        total_value = sum(item["analysis_value"] for item in items)
        cumulative_value = 0
        cumulative_items = 0
        
        for item in items:
            cumulative_value += item["analysis_value"]
            cumulative_items += 1
            item["cumulative_value_percent"] = (cumulative_value / total_value * 100) if total_value > 0 else 0
            item["cumulative_items_percent"] = (cumulative_items / len(items) * 100)
        
        # Classify items into A, B, C categories
        a_items = []
        b_items = []
        c_items = []
        
        for item in items:
            if item["cumulative_value_percent"] <= float(a_threshold):
                item["abc_category"] = "A"
                a_items.append(item)
            elif item["cumulative_value_percent"] <= float(a_threshold + b_threshold):
                item["abc_category"] = "B"
                b_items.append(item)
            else:
                item["abc_category"] = "C"
                c_items.append(item)
        
        # Calculate summary statistics
        total_items = len(items)
        a_value = sum(item["analysis_value"] for item in a_items)
        b_value = sum(item["analysis_value"] for item in b_items)
        c_value = sum(item["analysis_value"] for item in c_items)
        
        summary = {
            "A_items": {
                "count": len(a_items),
                "percentage_of_items": (len(a_items) / total_items * 100) if total_items > 0 else 0,
                "total_value": float(a_value),
                "percentage_of_value": (a_value / total_value * 100) if total_value > 0 else 0,
                "avg_value_per_item": float(a_value / len(a_items)) if a_items else 0
            },
            "B_items": {
                "count": len(b_items),
                "percentage_of_items": (len(b_items) / total_items * 100) if total_items > 0 else 0,
                "total_value": float(b_value),
                "percentage_of_value": (b_value / total_value * 100) if total_value > 0 else 0,
                "avg_value_per_item": float(b_value / len(b_items)) if b_items else 0
            },
            "C_items": {
                "count": len(c_items),
                "percentage_of_items": (len(c_items) / total_items * 100) if total_items > 0 else 0,
                "total_value": float(c_value),
                "percentage_of_value": (c_value / total_value * 100) if total_value > 0 else 0,
                "avg_value_per_item": float(c_value / len(c_items)) if c_items else 0
            }
        }
        
        # Generate management recommendations
        recommendations = {
            "A_items": [
                "Implement tight inventory control with frequent cycle counts",
                "Consider just-in-time or vendor-managed inventory",
                "Negotiate volume discounts and favorable payment terms",
                "Monitor stock levels daily and maintain safety stock",
                "Consider alternative suppliers to reduce risk"
            ],
            "B_items": [
                "Implement periodic review with moderate safety stock",
                "Use economic order quantity (EOQ) calculations",
                "Monitor monthly and adjust reorder points seasonally",
                "Negotiate standard payment terms",
                "Consider consignment arrangements for high-value B items"
            ],
            "C_items": [
                "Use simple two-bin system or periodic ordering",
                "Order larger quantities less frequently to reduce ordering costs",
                "Accept higher inventory levels to avoid stockouts",
                "Consider annual purchasing agreements",
                "Review annually for potential elimination"
            ]
        }
        
        # Category-specific insights
        insights = []
        
        if summary["A_items"]["percentage_of_items"] > 30:
            insights.append({
                "type": "warning",
                "message": f"A-items represent {summary['A_items']['percentage_of_items']:.1f}% of items (typically 20%)",
                "action": "Review threshold settings or consider more selective purchasing"
            })
        
        if summary["C_items"]["percentage_of_value"] > 20:
            insights.append({
                "type": "opportunity",
                "message": f"C-items represent {summary['C_items']['percentage_of_value']:.1f}% of value (typically <20%)",
                "action": "Consider reducing C-item variety to improve efficiency"
            })
        
        # Identify outliers
        outliers = {
            "high_value_c_items": [],
            "low_value_a_items": []
        }
        
        if c_items:
            c_avg = c_value / len(c_items)
            outliers["high_value_c_items"] = [
                item for item in c_items if item["analysis_value"] > c_avg * 3
            ][:10]
        
        if a_items:
            a_avg = a_value / len(a_items)
            outliers["low_value_a_items"] = [
                item for item in a_items if item["analysis_value"] < a_avg * 0.5
            ][:10]
        
        logger.info(f"ABC analysis completed by {current_user.email}: {total_items} items analyzed")
        
        return {
            "analysis_date": analysis_date,
            "analysis_basis": analysis_basis,
            "period_months": period_months,
            "total_items": total_items,
            "total_value": float(total_value),
            "thresholds": {
                "A": f"Top {float(a_threshold)}% of value",
                "B": f"Next {float(b_threshold)}% of value",
                "C": f"Remaining {float(100-a_threshold-b_threshold)}% of value"
            },
            "summary": summary,
            "items_by_category": {
                "A_items": a_items[:50],  # Limit for response size
                "B_items": b_items[:50],
                "C_items": c_items[:50]
            },
            "recommendations": recommendations,
            "insights": insights,
            "outliers": outliers,
            "pareto_principle_validation": {
                "passes_80_20_rule": summary["A_items"]["percentage_of_value"] >= 70,
                "a_items_value_percent": summary["A_items"]["percentage_of_value"],
                "a_items_count_percent": summary["A_items"]["percentage_of_items"]
            }
        }
        
    except Exception as e:
        logger.error(f"Error in ABC analysis: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to perform ABC analysis"
        )


@router.post("/batch-adjustment")
async def create_batch_adjustment(
    adjustment: BatchAdjustment,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Process batch cost adjustments.
    
    Adjusts multiple items in single transaction.
    """
    if not adjustment.adjustments:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="No adjustments provided"
        )
    
    # Validate all items first
    total_impact = Decimal(0)
    for adj in adjustment.adjustments:
        item = valuation_service.validate_item(db, item_code=adj["item_code"])
        if not item:
            raise HTTPException(
                status_code=status.HTTP_404_NOT_FOUND,
                detail=f"Item {adj['item_code']} not found"
            )
        
        current = valuation_service.get_item_current_valuation(
            db,
            item_code=adj["item_code"]
        )
        
        impact = abs((adj["new_cost"] - current["unit_cost"]) * current["quantity"])
        total_impact += impact
    
    # Check approval requirement for total
    if valuation_service.requires_approval(db, adjustment_value=total_impact):
        raise HTTPException(
            status_code=status.HTTP_403_FORBIDDEN,
            detail=f"Batch adjustment of ${total_impact:,.2f} requires additional approval"
        )
    
    # Process batch
    result = valuation_service.process_batch_adjustment(
        db,
        adjustment=adjustment,
        processed_by=current_user.id
    )
    
    return {
        "batch_id": result["batch_id"],
        "adjustments_processed": result["processed"],
        "adjustments_failed": result["failed"],
        "total_value_impact": result["total_impact"],
        "gl_batch_posted": result["gl_posted"],
        "results": result["details"]
    }


@router.get("/landed-cost")
async def calculate_landed_cost(
    po_number: Optional[str] = Query(None, description="Purchase order number"),
    receipt_number: Optional[str] = Query(None, description="Goods receipt number"),
    include_projections: bool = Query(True),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Calculate landed cost for receipts.
    
    Includes freight, duty, and other charges.
    """
    if not po_number and not receipt_number:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Specify either po_number or receipt_number"
        )
    
    landed_costs = valuation_service.calculate_landed_costs(
        db,
        po_number=po_number,
        receipt_number=receipt_number,
        include_projections=include_projections
    )
    
    return {
        "reference": po_number or receipt_number,
        "items": landed_costs["items"],
        "cost_breakdown": landed_costs["breakdown"],
        "total_product_cost": landed_costs["total_product"],
        "total_landed_cost": landed_costs["total_landed"],
        "cost_increase_percent": landed_costs["increase_percent"],
        "projections": landed_costs.get("projections", {}) if include_projections else {}
    }