"""Stock Order Management API endpoints"""

from fastapi import APIRouter, Depends, HTTPException, status, Query, Body
from sqlalchemy.orm import Session
from typing import List, Optional, Dict, Any
from datetime import date, datetime
from decimal import Decimal

from app.api import deps
from app.services.stock import order_service
from app.schemas.stock import (
    StockOrder, StockOrderCreate, StockOrderUpdate, StockOrderListResponse,
    OrderStatus, StockOrderLine, ReorderSuggestion, BillOfMaterials,
    BillOfMaterialsCreate
)

router = APIRouter()


@router.get("", response_model=StockOrderListResponse)
async def list_orders(
    order_type: str = Query(..., regex="^(purchase|sales|transfer|production)$"),
    status: Optional[OrderStatus] = Query(None, description="Filter by status"),
    supplier_customer_code: Optional[str] = Query(None),
    start_date: Optional[date] = Query(None, description="Order date from"),
    end_date: Optional[date] = Query(None, description="Order date to"),
    priority: Optional[str] = Query(None, regex="^(low|normal|high|urgent)$"),
    skip: int = 0,
    limit: int = 100,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    List stock orders with optional filters.
    
    Handles purchase, sales, transfer, and production orders.
    """
    orders = order_service.get_stock_orders(
        db,
        order_type=order_type,
        status=status,
        supplier_customer_code=supplier_customer_code,
        start_date=start_date,
        end_date=end_date,
        priority=priority,
        skip=skip,
        limit=limit
    )
    
    total = order_service.count_stock_orders(
        db,
        order_type=order_type,
        status=status,
        supplier_customer_code=supplier_customer_code,
        start_date=start_date,
        end_date=end_date,
        priority=priority
    )
    
    return StockOrderListResponse(
        orders=orders,
        total=total,
        skip=skip,
        limit=limit
    )


@router.get("/{order_id}", response_model=StockOrder)
async def get_order(
    order_id: int,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Get specific order by ID."""
    order = order_service.get_stock_order(db, order_id=order_id)
    if not order:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Order {order_id} not found"
        )
    return order


@router.get("/number/{order_number}", response_model=StockOrder)
async def get_order_by_number(
    order_number: str,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Get order by order number."""
    order = order_service.get_order_by_number(db, order_number=order_number)
    if not order:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Order {order_number} not found"
        )
    return order


@router.post("", response_model=StockOrder, status_code=status.HTTP_201_CREATED)
async def create_order(
    order_in: StockOrderCreate,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Create new stock order.
    
    Auto-allocates inventory for sales/transfer orders.
    """
    # Validate supplier/customer
    if order_in.order_type in ["purchase", "sales"]:
        entity = order_service.validate_entity(
            db,
            entity_code=order_in.supplier_customer_code,
            entity_type=order_in.order_type
        )
        if not entity:
            entity_name = "Supplier" if order_in.order_type == "purchase" else "Customer"
            raise HTTPException(
                status_code=status.HTTP_404_NOT_FOUND,
                detail=f"{entity_name} {order_in.supplier_customer_code} not found"
            )
    
    # Validate delivery location
    location = order_service.validate_location(db, location_code=order_in.delivery_location)
    if not location:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Delivery location {order_in.delivery_location} not found"
        )
    
    # For sales/transfer orders, check availability
    if order_in.order_type in ["sales", "transfer"] and order_in.auto_allocate:
        for line in order_in.lines:
            available = order_service.check_item_availability(
                db,
                item_code=line.item_code,
                quantity=line.quantity_ordered,
                location_code=order_in.delivery_location if order_in.order_type == "transfer" else None
            )
            
            if not available["available"]:
                raise HTTPException(
                    status_code=status.HTTP_400_BAD_REQUEST,
                    detail=f"Insufficient stock for {line.item_code}. Available: {available['qty_available']}"
                )
    
    # Create order
    order = order_service.create_stock_order(
        db,
        order_in=order_in,
        created_by=current_user.id
    )
    
    # Auto-allocate if requested
    if order_in.auto_allocate and order_in.order_type in ["sales", "transfer"]:
        allocation_result = order_service.allocate_order_stock(
            db,
            order_id=order.id,
            allocated_by=current_user.id
        )
        
        if not allocation_result["fully_allocated"]:
            # Return warning about partial allocation
            order.allocation_status = "partial"
    
    return order


@router.put("/{order_id}", response_model=StockOrder)
async def update_order(
    order_id: int,
    order_update: StockOrderUpdate,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Update stock order.
    
    Only draft and confirmed orders can be updated.
    """
    order = order_service.get_stock_order(db, order_id=order_id)
    if not order:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Order {order_id} not found"
        )
    
    if order.status not in [OrderStatus.DRAFT, OrderStatus.CONFIRMED]:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Cannot update order with status {order.status}"
        )
    
    updated = order_service.update_stock_order(
        db,
        order_id=order_id,
        order_update=order_update,
        updated_by=current_user.id
    )
    
    return updated


@router.post("/{order_id}/confirm")
async def confirm_order(
    order_id: int,
    send_notification: bool = Body(True),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Confirm stock order.
    
    Finalizes order and triggers fulfillment process.
    """
    order = order_service.get_stock_order(db, order_id=order_id)
    if not order:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Order {order_id} not found"
        )
    
    if order.status != OrderStatus.DRAFT:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Order status is {order.status}, expected DRAFT"
        )
    
    # Check stock allocation for sales/transfer
    if order.order_type in ["sales", "transfer"]:
        allocation_check = order_service.check_order_allocation(db, order_id=order_id)
        if not allocation_check["allocated"]:
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail="Order must be allocated before confirmation"
            )
    
    result = order_service.confirm_order(
        db,
        order_id=order_id,
        confirmed_by=current_user.id,
        send_notification=send_notification
    )
    
    return {
        "order_id": order_id,
        "order_number": order.order_number,
        "status": result["status"],
        "confirmed_at": result["confirmed_at"],
        "notifications_sent": result.get("notifications_sent", [])
    }


@router.post("/{order_id}/allocate")
async def allocate_order_stock(
    order_id: int,
    allocation_strategy: str = Body("fifo", regex="^(fifo|lifo|specific|nearest)$"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Allocate stock to order.
    
    Reserves inventory for sales/transfer orders.
    """
    order = order_service.get_stock_order(db, order_id=order_id)
    if not order:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Order {order_id} not found"
        )
    
    if order.order_type not in ["sales", "transfer"]:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Only sales and transfer orders can be allocated"
        )
    
    if order.status not in [OrderStatus.DRAFT, OrderStatus.CONFIRMED]:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Cannot allocate order with status {order.status}"
        )
    
    result = order_service.allocate_order_stock(
        db,
        order_id=order_id,
        strategy=allocation_strategy,
        allocated_by=current_user.id
    )
    
    return {
        "order_id": order_id,
        "fully_allocated": result["fully_allocated"],
        "lines_allocated": result["lines_allocated"],
        "lines_partial": result["lines_partial"],
        "lines_failed": result["lines_failed"],
        "allocation_details": result["details"]
    }


@router.post("/{order_id}/pick")
async def create_pick_list(
    order_id: int,
    consolidate_locations: bool = Body(True),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Generate pick list for order.
    
    Creates optimized picking instructions.
    """
    order = order_service.get_stock_order(db, order_id=order_id)
    if not order:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Order {order_id} not found"
        )
    
    if order.order_type not in ["sales", "transfer"]:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Pick lists only for sales and transfer orders"
        )
    
    if order.status != OrderStatus.CONFIRMED:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Order must be confirmed before picking"
        )
    
    pick_list = order_service.generate_pick_list(
        db,
        order_id=order_id,
        consolidate_locations=consolidate_locations,
        generated_by=current_user.id
    )
    
    return {
        "pick_list_number": pick_list["pick_list_number"],
        "order_id": order_id,
        "total_items": pick_list["total_items"],
        "total_locations": pick_list["total_locations"],
        "estimated_pick_time": pick_list["estimated_minutes"],
        "pick_sequence": pick_list["sequence"]
    }


@router.post("/{order_id}/ship")
async def ship_order(
    order_id: int,
    carrier: str = Body(...),
    tracking_number: Optional[str] = Body(None),
    ship_date: Optional[date] = Body(None),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Mark order as shipped.
    
    Updates inventory and creates shipment record.
    """
    order = order_service.get_stock_order(db, order_id=order_id)
    if not order:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Order {order_id} not found"
        )
    
    if order.order_type not in ["sales", "transfer"]:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Only sales and transfer orders can be shipped"
        )
    
    if order.status not in [OrderStatus.CONFIRMED, OrderStatus.PROCESSING]:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Cannot ship order with status {order.status}"
        )
    
    result = order_service.ship_order(
        db,
        order_id=order_id,
        carrier=carrier,
        tracking_number=tracking_number,
        ship_date=ship_date or date.today(),
        shipped_by=current_user.id
    )
    
    return {
        "order_id": order_id,
        "shipment_number": result["shipment_number"],
        "status": "shipped",
        "tracking_number": tracking_number,
        "inventory_movements": result["movements_created"]
    }


@router.post("/{order_id}/receive")
async def receive_order_goods(
    order_id: int,
    receipt_lines: List[Dict[str, Any]] = Body(...),
    receipt_date: Optional[date] = Body(None),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Receive goods against purchase order.
    
    Updates inventory and matches to order.
    """
    order = order_service.get_stock_order(db, order_id=order_id)
    if not order:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Order {order_id} not found"
        )
    
    if order.order_type != "purchase":
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Only purchase orders can receive goods"
        )
    
    if order.status not in [OrderStatus.CONFIRMED, OrderStatus.PARTIAL]:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Cannot receive goods for order with status {order.status}"
        )
    
    # Validate receipt quantities
    for receipt in receipt_lines:
        order_line = next((l for l in order.lines if l.id == receipt["line_id"]), None)
        if not order_line:
            raise HTTPException(
                status_code=status.HTTP_404_NOT_FOUND,
                detail=f"Order line {receipt['line_id']} not found"
            )
        
        if receipt["quantity"] > order_line.quantity_outstanding:
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail=f"Over-receipt on line {order_line.line_number}"
            )
    
    result = order_service.receive_order_goods(
        db,
        order_id=order_id,
        receipt_lines=receipt_lines,
        receipt_date=receipt_date or date.today(),
        received_by=current_user.id
    )
    
    return {
        "order_id": order_id,
        "receipt_number": result["receipt_number"],
        "lines_received": result["lines_received"],
        "order_complete": result["order_complete"],
        "inventory_movements": result["movements_created"]
    }


@router.post("/{order_id}/cancel")
async def cancel_order(
    order_id: int,
    reason: str = Body(..., min_length=10),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Cancel stock order.
    
    Releases allocations and updates status.
    """
    order = order_service.get_stock_order(db, order_id=order_id)
    if not order:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Order {order_id} not found"
        )
    
    if order.status in [OrderStatus.COMPLETE, OrderStatus.CANCELLED]:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Cannot cancel order with status {order.status}"
        )
    
    # Check for received goods
    if order.order_type == "purchase" and order.total_received > 0:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Cannot cancel order with received goods"
        )
    
    result = order_service.cancel_order(
        db,
        order_id=order_id,
        reason=reason,
        cancelled_by=current_user.id
    )
    
    return {
        "order_id": order_id,
        "order_number": order.order_number,
        "status": "cancelled",
        "allocations_released": result.get("allocations_released", 0),
        "cancelled_at": datetime.now()
    }


@router.get("/backorders")
async def get_backorders(
    item_code: Optional[str] = Query(None),
    customer_code: Optional[str] = Query(None),
    urgent_only: bool = Query(False),
    days_overdue: Optional[int] = Query(None, ge=1),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Get backorder report.
    
    Shows orders waiting for stock.
    """
    backorders = order_service.get_backorders(
        db,
        item_code=item_code,
        customer_code=customer_code,
        urgent_only=urgent_only,
        days_overdue=days_overdue
    )
    
    summary = order_service.calculate_backorder_summary(db, backorders=backorders)
    
    return {
        "report_date": date.today(),
        "total_backorders": len(backorders),
        "total_value": summary["total_value"],
        "customers_affected": summary["customers_affected"],
        "items_backordered": summary["items_backordered"],
        "by_priority": {
            "urgent": [b for b in backorders if b["priority"] == "urgent"],
            "high": [b for b in backorders if b["priority"] == "high"],
            "normal": [b for b in backorders if b["priority"] == "normal"]
        },
        "backorders": backorders
    }


@router.get("/reorder-suggestions")
async def get_reorder_suggestions(
    category_code: Optional[str] = None,
    location_code: Optional[str] = None,
    include_forecasted: bool = Query(True),
    lead_time_buffer_days: int = Query(7),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Get reorder suggestions.
    
    Analyzes stock levels and suggests purchase orders.
    """
    suggestions = order_service.calculate_reorder_suggestions(
        db,
        category_code=category_code,
        location_code=location_code,
        include_forecasted=include_forecasted,
        lead_time_buffer_days=lead_time_buffer_days
    )
    
    # Group by supplier
    by_supplier = {}
    for suggestion in suggestions:
        supplier = suggestion.preferred_supplier
        if supplier not in by_supplier:
            by_supplier[supplier] = {
                "supplier_code": supplier,
                "items": [],
                "total_value": Decimal(0)
            }
        by_supplier[supplier]["items"].append(suggestion)
        by_supplier[supplier]["total_value"] += suggestion.estimated_cost
    
    return {
        "analysis_date": date.today(),
        "total_items": len(suggestions),
        "total_estimated_cost": sum(s.estimated_cost for s in suggestions),
        "urgent_items": sum(1 for s in suggestions if s.urgency == "critical"),
        "by_supplier": by_supplier,
        "suggestions": suggestions
    }


@router.post("/auto-create-purchase-orders")
async def auto_create_purchase_orders(
    suggestions: List[int] = Body(..., description="Suggestion IDs to convert"),
    combine_by_supplier: bool = Body(True),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Auto-create purchase orders from suggestions.
    
    Converts reorder suggestions into draft POs.
    """
    if not suggestions:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="No suggestions provided"
        )
    
    result = order_service.create_purchase_orders_from_suggestions(
        db,
        suggestion_ids=suggestions,
        combine_by_supplier=combine_by_supplier,
        created_by=current_user.id
    )
    
    return {
        "orders_created": len(result["orders"]),
        "total_value": result["total_value"],
        "items_ordered": result["items_ordered"],
        "orders": [
            {
                "order_id": o["id"],
                "order_number": o["number"],
                "supplier": o["supplier"],
                "items": o["item_count"],
                "value": o["value"]
            }
            for o in result["orders"]
        ]
    }


@router.get("/production/bom/{item_code}")
async def get_bill_of_materials(
    item_code: str,
    version: Optional[str] = Query(None),
    include_costs: bool = Query(True),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Get bill of materials for item.
    
    Shows component requirements for production.
    """
    # Validate item
    item = order_service.validate_item(db, item_code=item_code)
    if not item:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Item {item_code} not found"
        )
    
    if not item.is_manufactured:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Item {item_code} is not a manufactured item"
        )
    
    bom = order_service.get_bill_of_materials(
        db,
        item_code=item_code,
        version=version,
        include_costs=include_costs
    )
    
    if not bom:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"No BOM found for {item_code}"
        )
    
    return bom


@router.post("/production/check-availability")
async def check_production_availability(
    item_code: str = Body(...),
    quantity_required: Decimal = Body(..., gt=0),
    production_date: Optional[date] = Body(None),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Check component availability for production.
    
    Verifies if production order can be fulfilled.
    """
    availability = order_service.check_production_availability(
        db,
        item_code=item_code,
        quantity_required=quantity_required,
        production_date=production_date or date.today()
    )
    
    return {
        "item_code": item_code,
        "quantity_required": quantity_required,
        "can_produce": availability["can_produce"],
        "max_quantity_possible": availability["max_quantity"],
        "component_availability": availability["components"],
        "shortage_items": availability["shortages"],
        "suggested_orders": availability.get("suggested_orders", [])
    }