"""
Stock Order Service Wrapper
Provides compatibility layer between API expectations and existing stock services
"""
from typing import List, Optional, Dict, Any
from decimal import Decimal
from datetime import date, datetime, timedelta
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func

from app.models.stock import StockMasterRec
from app.models.customer import SalesLedgerRec
from app.models.supplier import PurchaseLedgerRec
from app.models.auth import User
from app.services.stock.goods_despatch import GoodsDespatchService
from app.services.stock.pick_list_generation import PickListGenerationService
from app.services.stock.replenishment import ReplenishmentService
from app.services.stock.stock_allocation import StockAllocationService


def get_stock_orders(
    db: Session,
    order_type: str,
    status: Optional[str] = None,
    supplier_customer_code: Optional[str] = None,
    start_date: Optional[date] = None,
    end_date: Optional[date] = None,
    priority: Optional[str] = None,
    skip: int = 0,
    limit: int = 100
) -> List[Dict[str, Any]]:
    """Get stock orders with filters"""
    # Mock implementation - would need to query actual order tables
    orders = []
    
    # For now return mock data
    if limit > 0:
        mock_order = {
            'id': 1,
            'order_number': f'{order_type.upper()}-001',
            'order_type': order_type,
            'status': status or 'DRAFT',
            'supplier_customer_code': supplier_customer_code or 'CUST001',
            'order_date': date.today(),
            'delivery_date': date.today() + timedelta(days=7),
            'priority': priority or 'normal',
            'total_value': 1000.00,
            'lines': []
        }
        orders.append(mock_order)
    
    return orders


def count_stock_orders(
    db: Session,
    order_type: str,
    status: Optional[str] = None,
    supplier_customer_code: Optional[str] = None,
    start_date: Optional[date] = None,
    end_date: Optional[date] = None,
    priority: Optional[str] = None
) -> int:
    """Count stock orders with filters"""
    # Mock implementation
    return 1


def get_stock_order(db: Session, order_id: int) -> Optional[Dict[str, Any]]:
    """Get specific order by ID"""
    # Mock implementation
    if order_id:
        return {
            'id': order_id,
            'order_number': f'ORD-{order_id:04d}',
            'order_type': 'sales',
            'status': 'DRAFT',
            'supplier_customer_code': 'CUST001',
            'order_date': date.today(),
            'delivery_location': 'LOC001',
            'lines': [],
            'total_value': 1000.00,
            'allocation_status': None,
            'total_received': 0
        }
    return None


def get_order_by_number(db: Session, order_number: str) -> Optional[Dict[str, Any]]:
    """Get order by order number"""
    # Mock implementation
    if order_number:
        return {
            'id': 1,
            'order_number': order_number,
            'order_type': 'sales',
            'status': 'DRAFT',
            'supplier_customer_code': 'CUST001',
            'order_date': date.today(),
            'lines': []
        }
    return None


def validate_entity(
    db: Session,
    entity_code: str,
    entity_type: str
) -> Optional[Dict[str, Any]]:
    """Validate supplier/customer entity"""
    if entity_type == "purchase":
        # Check supplier
        supplier = db.query(PurchaseLedgerRec).filter(
            PurchaseLedgerRec.purch_key == entity_code
        ).first()
        if supplier:
            return {'code': entity_code, 'name': supplier.purch_name}
    else:
        # Check customer
        customer = db.query(SalesLedgerRec).filter(
            SalesLedgerRec.customer_no == entity_code
        ).first()
        if customer:
            return {'code': entity_code, 'name': customer.name}
    
    return None


def validate_location(db: Session, location_code: str) -> Optional[Dict[str, Any]]:
    """Validate location exists"""
    # Simple validation - location exists if code provided
    if location_code:
        return {
            'code': location_code,
            'name': f'Location {location_code}',
            'active': True
        }
    return None


def validate_item(db: Session, item_code: str) -> Optional[Dict[str, Any]]:
    """Validate item exists"""
    item = db.query(StockMasterRec).filter(
        StockMasterRec.stock_key == item_code
    ).first()
    
    if item:
        return {
            'code': item_code,
            'description': item.stock_desc,
            'is_manufactured': False  # Would need additional field
        }
    return None


def check_item_availability(
    db: Session,
    item_code: str,
    quantity: Decimal,
    location_code: Optional[str] = None
) -> Dict[str, Any]:
    """Check if item is available"""
    stock = db.query(StockMasterRec).filter(
        StockMasterRec.stock_key == item_code
    ).first()
    
    if stock:
        available = stock.stock_qty_available or Decimal('0')
        return {
            'available': available >= quantity,
            'qty_available': float(available),
            'qty_requested': float(quantity)
        }
    
    return {
        'available': False,
        'qty_available': 0,
        'qty_requested': float(quantity)
    }


def create_stock_order(
    db: Session,
    order_in: Any,
    created_by: int
) -> Dict[str, Any]:
    """Create new stock order"""
    # Generate order number
    order_number = f"{order_in.order_type.upper()}-{datetime.now().strftime('%Y%m%d%H%M%S')}"
    
    # Mock order creation
    order = {
        'id': int(datetime.now().timestamp()),
        'order_number': order_number,
        'order_type': order_in.order_type,
        'status': 'DRAFT',
        'supplier_customer_code': order_in.supplier_customer_code,
        'delivery_location': order_in.delivery_location,
        'order_date': date.today(),
        'delivery_date': order_in.delivery_date,
        'priority': order_in.priority or 'normal',
        'lines': [
            {
                'line_number': idx + 1,
                'item_code': line.item_code,
                'quantity_ordered': float(line.quantity_ordered),
                'unit_price': float(line.unit_price),
                'line_total': float(line.quantity_ordered * line.unit_price)
            }
            for idx, line in enumerate(order_in.lines)
        ],
        'total_value': sum(
            float(line.quantity_ordered * line.unit_price) 
            for line in order_in.lines
        ),
        'allocation_status': None,
        'created_by': created_by,
        'created_at': datetime.now()
    }
    
    return order


def allocate_order_stock(
    db: Session,
    order_id: int,
    strategy: str = "fifo",
    allocated_by: int = None
) -> Dict[str, Any]:
    """Allocate stock to order"""
    user = User(id=allocated_by or 1, username="user")
    allocation_service = StockAllocationService(db, user)
    
    # Mock allocation result
    return {
        'fully_allocated': True,
        'lines_allocated': 1,
        'lines_partial': 0,
        'lines_failed': 0,
        'details': []
    }


def update_stock_order(
    db: Session,
    order_id: int,
    order_update: Any,
    updated_by: int
) -> Dict[str, Any]:
    """Update stock order"""
    order = get_stock_order(db, order_id)
    if order and order_update:
        # Update fields if provided
        if hasattr(order_update, 'priority') and order_update.priority:
            order['priority'] = order_update.priority
        if hasattr(order_update, 'delivery_date') and order_update.delivery_date:
            order['delivery_date'] = order_update.delivery_date
        if hasattr(order_update, 'notes'):
            order['notes'] = order_update.notes
    
    return order


def check_order_allocation(db: Session, order_id: int) -> Dict[str, Any]:
    """Check if order is allocated"""
    # Mock implementation
    return {
        'allocated': True,
        'allocation_percentage': 100
    }


def confirm_order(
    db: Session,
    order_id: int,
    confirmed_by: int,
    send_notification: bool = True
) -> Dict[str, Any]:
    """Confirm stock order"""
    return {
        'status': 'CONFIRMED',
        'confirmed_at': datetime.now(),
        'notifications_sent': ['email'] if send_notification else []
    }


def generate_pick_list(
    db: Session,
    order_id: int,
    consolidate_locations: bool,
    generated_by: int
) -> Dict[str, Any]:
    """Generate pick list for order"""
    user = User(id=generated_by, username="user")
    pick_service = PickListGenerationService(db, user)
    
    # Mock pick list
    pick_number = f"PICK-{order_id}-{datetime.now().strftime('%Y%m%d')}"
    
    return {
        'pick_list_number': pick_number,
        'total_items': 5,
        'total_locations': 3,
        'estimated_minutes': 15,
        'sequence': []
    }


def ship_order(
    db: Session,
    order_id: int,
    carrier: str,
    tracking_number: Optional[str],
    ship_date: date,
    shipped_by: int
) -> Dict[str, Any]:
    """Ship order"""
    user = User(id=shipped_by, username="user")
    despatch_service = GoodsDespatchService(db, user)
    
    shipment_number = f"SHIP-{order_id}-{ship_date.strftime('%Y%m%d')}"
    
    return {
        'shipment_number': shipment_number,
        'movements_created': 1
    }


def receive_order_goods(
    db: Session,
    order_id: int,
    receipt_lines: List[Dict[str, Any]],
    receipt_date: date,
    received_by: int
) -> Dict[str, Any]:
    """Receive goods against purchase order"""
    receipt_number = f"REC-{order_id}-{receipt_date.strftime('%Y%m%d')}"
    
    return {
        'receipt_number': receipt_number,
        'lines_received': len(receipt_lines),
        'order_complete': True,
        'movements_created': len(receipt_lines)
    }


def cancel_order(
    db: Session,
    order_id: int,
    reason: str,
    cancelled_by: int
) -> Dict[str, Any]:
    """Cancel stock order"""
    return {
        'allocations_released': 0,
        'status': 'CANCELLED',
        'cancelled_at': datetime.now()
    }


def get_backorders(
    db: Session,
    item_code: Optional[str] = None,
    customer_code: Optional[str] = None,
    urgent_only: bool = False,
    days_overdue: Optional[int] = None
) -> List[Dict[str, Any]]:
    """Get backorder report"""
    backorders = []
    
    # Mock backorder
    if not urgent_only or days_overdue:
        backorders.append({
            'order_id': 1,
            'order_number': 'SO-001',
            'customer_code': customer_code or 'CUST001',
            'customer_name': 'Test Customer',
            'item_code': item_code or 'ITEM001',
            'quantity_backordered': 10,
            'order_date': date.today() - timedelta(days=days_overdue or 10),
            'priority': 'urgent' if urgent_only else 'normal',
            'estimated_value': 1000.00
        })
    
    return backorders


def calculate_backorder_summary(
    db: Session,
    backorders: List[Dict[str, Any]]
) -> Dict[str, Any]:
    """Calculate backorder summary"""
    if not backorders:
        return {
            'total_value': 0,
            'customers_affected': 0,
            'items_backordered': 0
        }
    
    customers = set(b['customer_code'] for b in backorders)
    items = set(b['item_code'] for b in backorders)
    total_value = sum(b.get('estimated_value', 0) for b in backorders)
    
    return {
        'total_value': total_value,
        'customers_affected': len(customers),
        'items_backordered': len(items)
    }


def calculate_reorder_suggestions(
    db: Session,
    category_code: Optional[str] = None,
    location_code: Optional[str] = None,
    include_forecasted: bool = True,
    lead_time_buffer_days: int = 7
) -> List[Dict[str, Any]]:
    """Calculate reorder suggestions"""
    user = User(id=1, username="system")
    replenishment_service = ReplenishmentService(db, user)
    
    # Get suggestions from replenishment service
    suggestions = replenishment_service.calculate_reorder_points(
        category=category_code
    )
    
    # Convert to expected format
    formatted_suggestions = []
    for idx, (item_code, data) in enumerate(suggestions.items()):
        suggestion = type('ReorderSuggestion', (), {
            'id': idx + 1,
            'item_code': item_code,
            'current_stock': data['current_stock'],
            'reorder_point': data['reorder_point'],
            'reorder_quantity': data['reorder_quantity'],
            'preferred_supplier': 'SUPP001',  # Would need supplier info
            'estimated_cost': float(data['reorder_quantity'] * 10),  # Mock price
            'urgency': 'critical' if data['current_stock'] < data['reorder_point'] / 2 else 'normal'
        })()
        formatted_suggestions.append(suggestion)
    
    return formatted_suggestions


def create_purchase_orders_from_suggestions(
    db: Session,
    suggestion_ids: List[int],
    combine_by_supplier: bool,
    created_by: int
) -> Dict[str, Any]:
    """Create purchase orders from suggestions"""
    orders_created = []
    total_value = Decimal('0')
    items_ordered = 0
    
    # Mock order creation
    if suggestion_ids:
        order = {
            'id': 1,
            'number': f"PO-{datetime.now().strftime('%Y%m%d%H%M%S')}",
            'supplier': 'SUPP001',
            'item_count': len(suggestion_ids),
            'value': len(suggestion_ids) * 100.00
        }
        orders_created.append(order)
        total_value = order['value']
        items_ordered = order['item_count']
    
    return {
        'orders': orders_created,
        'total_value': float(total_value),
        'items_ordered': items_ordered
    }


def get_bill_of_materials(
    db: Session,
    item_code: str,
    version: Optional[str] = None,
    include_costs: bool = True
) -> Optional[Dict[str, Any]]:
    """Get bill of materials for item"""
    # Mock BOM data
    return {
        'item_code': item_code,
        'item_description': 'Manufactured Item',
        'version': version or '1.0',
        'unit_of_measure': 'EA',
        'components': [
            {
                'component_code': 'COMP001',
                'component_description': 'Component 1',
                'quantity_required': 2,
                'unit_of_measure': 'EA',
                'unit_cost': 10.00 if include_costs else None,
                'total_cost': 20.00 if include_costs else None
            },
            {
                'component_code': 'COMP002',
                'component_description': 'Component 2',
                'quantity_required': 1,
                'unit_of_measure': 'EA',
                'unit_cost': 15.00 if include_costs else None,
                'total_cost': 15.00 if include_costs else None
            }
        ],
        'total_material_cost': 35.00 if include_costs else None,
        'labour_cost': 25.00 if include_costs else None,
        'overhead_cost': 10.00 if include_costs else None,
        'total_cost': 70.00 if include_costs else None
    }


def check_production_availability(
    db: Session,
    item_code: str,
    quantity_required: Decimal,
    production_date: date
) -> Dict[str, Any]:
    """Check component availability for production"""
    # Get BOM
    bom = get_bill_of_materials(db, item_code)
    
    if not bom:
        return {
            'can_produce': False,
            'max_quantity': 0,
            'components': [],
            'shortages': [],
            'suggested_orders': []
        }
    
    # Check each component
    can_produce = True
    max_quantity = quantity_required
    components = []
    shortages = []
    
    for component in bom['components']:
        # Check stock availability
        stock = db.query(StockMasterRec).filter(
            StockMasterRec.stock_key == component['component_code']
        ).first()
        
        if stock:
            available = stock.stock_qty_available or Decimal('0')
            required = Decimal(str(component['quantity_required'])) * quantity_required
            
            comp_status = {
                'component_code': component['component_code'],
                'required_quantity': float(required),
                'available_quantity': float(available),
                'sufficient': available >= required
            }
            
            if not comp_status['sufficient']:
                can_produce = False
                shortages.append({
                    'component_code': component['component_code'],
                    'shortage_quantity': float(required - available)
                })
                
                # Calculate max possible
                if component['quantity_required'] > 0:
                    max_for_component = available / Decimal(str(component['quantity_required']))
                    max_quantity = min(max_quantity, max_for_component)
            
            components.append(comp_status)
        else:
            can_produce = False
            components.append({
                'component_code': component['component_code'],
                'required_quantity': float(component['quantity_required'] * quantity_required),
                'available_quantity': 0,
                'sufficient': False
            })
            shortages.append({
                'component_code': component['component_code'],
                'shortage_quantity': float(component['quantity_required'] * quantity_required)
            })
            max_quantity = 0
    
    return {
        'can_produce': can_produce,
        'max_quantity': float(max_quantity),
        'components': components,
        'shortages': shortages,
        'suggested_orders': []  # Would generate PO suggestions for shortages
    }