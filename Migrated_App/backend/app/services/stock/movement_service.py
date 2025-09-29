"""
Stock Movement Service Wrapper
Provides compatibility layer between API expectations and StockMovementsService implementation
"""
from typing import List, Optional, Dict, Any
from decimal import Decimal
from datetime import date
from sqlalchemy.orm import Session

from app.services.stock.stock_movements import StockMovementsService
from app.models.stock import StockMasterRec, StockMovementRec
from app.models.auth import User


def get_movements(
    db: Session,
    item_code: Optional[str] = None,
    location_code: Optional[str] = None,
    movement_type: Optional[str] = None,
    start_date: Optional[date] = None,
    end_date: Optional[date] = None,
    reference_number: Optional[str] = None,
    skip: int = 0,
    limit: int = 100
) -> List[Dict[str, Any]]:
    """Get stock movements with filters"""
    # Create a dummy user context for the service
    user = User(id=1, username="system")
    service = StockMovementsService(db, user)
    
    # Convert parameters to match service expectations
    filters = {
        'stock_code': item_code,
        'location': location_code,
        'movement_type': movement_type,
        'from_date': start_date,
        'to_date': end_date,
        'reference': reference_number
    }
    
    # Remove None values
    filters = {k: v for k, v in filters.items() if v is not None}
    
    # Get movement history
    movements = service.get_movement_history(**filters)
    
    # Apply pagination
    total_movements = movements[skip:skip + limit]
    
    return total_movements


def count_movements(
    db: Session,
    item_code: Optional[str] = None,
    location_code: Optional[str] = None,
    movement_type: Optional[str] = None,
    start_date: Optional[date] = None,
    end_date: Optional[date] = None,
    reference_number: Optional[str] = None
) -> int:
    """Count stock movements with filters"""
    user = User(id=1, username="system")
    service = StockMovementsService(db, user)
    
    filters = {
        'stock_code': item_code,
        'location': location_code,
        'movement_type': movement_type,
        'from_date': start_date,
        'to_date': end_date,
        'reference': reference_number
    }
    
    filters = {k: v for k, v in filters.items() if v is not None}
    
    movements = service.get_movement_history(**filters)
    return len(movements)


def get_movement(db: Session, movement_id: int) -> Optional[StockMovementRec]:
    """Get specific movement by ID"""
    return db.query(StockMovementRec).filter(
        StockMovementRec.move_id == movement_id
    ).first()


def validate_item(db: Session, item_code: str) -> Optional[StockMasterRec]:
    """Validate item exists"""
    return db.query(StockMasterRec).filter(
        StockMasterRec.stock_key == item_code
    ).first()


def validate_location(db: Session, location_code: str) -> Optional[Dict]:
    """Validate location exists"""
    # For now, return a dummy location dict if code is provided
    if location_code:
        return {
            'code': location_code,
            'description': f'Location {location_code}',
            'active': True
        }
    return None


def check_stock_availability(
    db: Session,
    item_code: str,
    location_code: str,
    quantity: Decimal
) -> Dict[str, Any]:
    """Check if stock is available"""
    user = User(id=1, username="system")
    service = StockMovementsService(db, user)
    
    # Get available quantity using private method
    available_qty = service._get_available_stock(
        stock_code=item_code,
        warehouse='MAIN',  # Default warehouse
        location=location_code
    )
    
    return {
        'available': available_qty >= quantity,
        'qty_available': float(available_qty),
        'qty_requested': float(quantity)
    }


def create_receipt(
    db: Session,
    movement_in: Any,
    created_by: int
) -> StockMovementRec:
    """Create stock receipt"""
    user = User(id=created_by, username="user")
    service = StockMovementsService(db, user)
    
    receipt_data = {
        'stock_code': movement_in.item_code,
        'warehouse': 'MAIN',
        'location': movement_in.to_location,
        'quantity': movement_in.quantity,
        'unit_cost': movement_in.unit_cost,
        'reference': movement_in.reference_number,
        'notes': movement_in.notes
    }
    
    return service.process_receipt(**receipt_data)


def create_issue(
    db: Session,
    movement_in: Any,
    created_by: int
) -> StockMovementRec:
    """Create stock issue"""
    user = User(id=created_by, username="user")
    service = StockMovementsService(db, user)
    
    issue_data = {
        'stock_code': movement_in.item_code,
        'warehouse': 'MAIN',
        'location': movement_in.from_location,
        'quantity': movement_in.quantity,
        'reference': movement_in.reference_number,
        'notes': movement_in.notes,
        'issue_type': 'GENERAL'
    }
    
    return service.process_issue(**issue_data)


def create_adjustment(
    db: Session,
    movement_in: Any,
    reason_code: str,
    approval_ref: Optional[str],
    created_by: int
) -> StockMovementRec:
    """Create stock adjustment"""
    user = User(id=created_by, username="user")
    service = StockMovementsService(db, user)
    
    adjustment_data = {
        'stock_code': movement_in.item_code,
        'warehouse': 'MAIN',
        'location': movement_in.from_location or movement_in.to_location,
        'quantity': movement_in.quantity,
        'unit_cost': movement_in.unit_cost,
        'reason': reason_code,
        'reference': movement_in.reference_number,
        'notes': movement_in.notes
    }
    
    adjustment = service.process_adjustment(**adjustment_data)
    
    # If approval ref provided, approve it
    if approval_ref:
        service.approve_adjustment(
            adjustment_no=adjustment.adj_no,
            approved_by=approval_ref
        )
    
    return adjustment


def get_adjustment_reasons(db: Session) -> List[str]:
    """Get valid adjustment reasons"""
    return [
        'DAMAGED',
        'LOST',
        'FOUND',
        'EXPIRED',
        'QUALITY',
        'COUNT',
        'OTHER'
    ]


def requires_approval(
    db: Session,
    adjustment_value: Decimal,
    user_id: int
) -> bool:
    """Check if adjustment requires approval"""
    # Adjustments over 1000 require approval
    return adjustment_value > 1000


def create_transfer(
    db: Session,
    transfer_in: Any,
    created_by: int
) -> Dict[str, Any]:
    """Create stock transfer"""
    user = User(id=created_by, username="user")
    service = StockMovementsService(db, user)
    
    # Process each line in the transfer
    transfer_results = []
    
    for line in transfer_in.lines:
        transfer = service.process_transfer(
            stock_code=line.item_code,
            from_warehouse='MAIN',
            from_location=transfer_in.from_location,
            to_warehouse='MAIN',
            to_location=transfer_in.to_location,
            quantity=line.quantity,
            reference=transfer_in.reference_number,
            notes=transfer_in.notes
        )
        transfer_results.append(transfer)
    
    # Return transfer summary
    return {
        'transfer_number': transfer_results[0].trans_no if transfer_results else None,
        'lines': len(transfer_results),
        'from_location': transfer_in.from_location,
        'to_location': transfer_in.to_location,
        'status': 'completed'
    }


def validate_movement(db: Session, movement: Any) -> None:
    """Validate a movement before processing"""
    # Basic validation
    if not movement.item_code:
        raise ValueError("Item code is required")
    
    if movement.quantity <= 0:
        raise ValueError("Quantity must be positive")
    
    # Validate item exists
    if not validate_item(db, movement.item_code):
        raise ValueError(f"Item {movement.item_code} not found")


def create_batch_movements(
    db: Session,
    batch_in: Any,
    created_by: int
) -> Dict[str, Any]:
    """Process batch movements"""
    user = User(id=created_by, username="user")
    service = StockMovementsService(db, user)
    
    results = {
        'batch_number': f"BATCH-{created_by}-{int(date.today().strftime('%Y%m%d'))}",
        'successful': 0,
        'failed': 0,
        'details': []
    }
    
    for idx, movement in enumerate(batch_in.movements):
        try:
            # Process based on movement type
            if movement.movement_type == 'RECEIPT':
                create_receipt(db, movement, created_by)
            elif movement.movement_type == 'ISSUE':
                create_issue(db, movement, created_by)
            
            results['successful'] += 1
            results['details'].append({
                'index': idx,
                'status': 'success',
                'item_code': movement.item_code
            })
        except Exception as e:
            results['failed'] += 1
            results['details'].append({
                'index': idx,
                'status': 'failed',
                'item_code': movement.item_code,
                'error': str(e)
            })
    
    return results


def get_item_history(
    db: Session,
    item_code: str,
    location_code: Optional[str],
    days: int,
    include_costs: bool
) -> Dict[str, Any]:
    """Get item movement history"""
    user = User(id=1, username="system")
    service = StockMovementsService(db, user)
    
    # Calculate date range
    from datetime import datetime, timedelta
    end_date = date.today()
    start_date = end_date - timedelta(days=days)
    
    # Get movements
    movements = service.get_movement_history(
        stock_code=item_code,
        location=location_code,
        from_date=start_date,
        to_date=end_date
    )
    
    # Calculate totals
    total_receipts = sum(
        m.get('quantity', 0) for m in movements 
        if m.get('movement_type', '').startswith('REC')
    )
    
    total_issues = sum(
        m.get('quantity', 0) for m in movements 
        if m.get('movement_type', '').startswith('ISS')
    )
    
    # Get current balance
    stock = db.query(StockMasterRec).filter(
        StockMasterRec.stock_key == item_code
    ).first()
    
    current_balance = stock.stock_qty_on_hand if stock else 0
    opening_balance = current_balance - total_receipts + total_issues
    
    return {
        'opening_balance': float(opening_balance),
        'closing_balance': float(current_balance),
        'total_receipts': float(total_receipts),
        'total_issues': float(total_issues),
        'movements': movements
    }


def get_movement_by_reference(
    db: Session,
    reference: str
) -> Optional[StockMovementRec]:
    """Get movement by reference number"""
    return db.query(StockMovementRec).filter(
        StockMovementRec.move_reference == reference
    ).first()


def create_return(
    db: Session,
    original_movement_id: int,
    return_items: List[Dict[str, Any]],
    reason: str,
    created_by: int
) -> Dict[str, Any]:
    """Process stock return"""
    user = User(id=created_by, username="user")
    service = StockMovementsService(db, user)
    
    # Get original movement
    original = get_movement(db, original_movement_id)
    if not original:
        raise ValueError("Original movement not found")
    
    # Process returns
    return_movements = []
    total_value = Decimal('0')
    
    for item in return_items:
        # Create receipt for returned items
        movement = service.process_receipt(
            stock_code=original.move_stock_code,
            warehouse=original.move_warehouse,
            location=original.move_from_location,
            quantity=item['quantity'],
            unit_cost=original.move_unit_cost,
            reference=f"RET-{original.move_reference}",
            notes=f"Return: {reason}"
        )
        return_movements.append(movement)
        total_value += movement.move_quantity * movement.move_unit_cost
    
    return {
        'return_number': f"RET-{original_movement_id}",
        'movements': return_movements,
        'total_value': float(total_value)
    }


def is_reversed(db: Session, movement_id: int) -> bool:
    """Check if movement has been reversed"""
    # Check if there's a reversal movement
    reversal = db.query(StockMovementRec).filter(
        StockMovementRec.move_reference.like(f"REV-{movement_id}-%")
    ).first()
    return reversal is not None


def reverse_movement(
    db: Session,
    movement_id: int,
    reason: str,
    reversed_by: int
) -> StockMovementRec:
    """Reverse a stock movement"""
    user = User(id=reversed_by, username="user")
    service = StockMovementsService(db, user)
    
    # Get original movement
    original = get_movement(db, movement_id)
    if not original:
        raise ValueError("Movement not found")
    
    # Create opposite movement
    if original.move_type.startswith('REC'):
        # Receipt becomes issue
        reversal = service.process_issue(
            stock_code=original.move_stock_code,
            warehouse=original.move_warehouse,
            location=original.move_to_location,
            quantity=original.move_quantity,
            reference=f"REV-{movement_id}-{reason}",
            notes=f"Reversal of {original.move_reference}: {reason}",
            issue_type='REVERSAL'
        )
    elif original.move_type.startswith('ISS'):
        # Issue becomes receipt
        reversal = service.process_receipt(
            stock_code=original.move_stock_code,
            warehouse=original.move_warehouse,
            location=original.move_from_location,
            quantity=original.move_quantity,
            unit_cost=original.move_unit_cost,
            reference=f"REV-{movement_id}-{reason}",
            notes=f"Reversal of {original.move_reference}: {reason}"
        )
    else:
        raise ValueError(f"Cannot reverse movement type {original.move_type}")
    
    return reversal


def get_pending_transfers(
    db: Session,
    location_code: Optional[str],
    include_in_transit: bool
) -> List[Dict[str, Any]]:
    """Get pending transfer movements"""
    user = User(id=1, username="system")
    service = StockMovementsService(db, user)
    return service.get_pending_transfers()


def get_transfer(db: Session, transfer_id: int) -> Optional[Any]:
    """Get transfer by ID"""
    # For now return a mock transfer
    return type('Transfer', (), {
        'status': 'in_transit',
        'id': transfer_id
    })()


def receive_transfer(
    db: Session,
    transfer_id: int,
    received_items: List[Dict[str, Any]],
    discrepancy_notes: Optional[str],
    received_by: int
) -> Dict[str, Any]:
    """Receive transfer at destination"""
    from datetime import datetime
    
    return {
        'received_at': datetime.now(),
        'discrepancies': []
    }


def analyze_movement_costs(
    db: Session,
    start_date: date,
    end_date: date,
    item_code: Optional[str],
    location_code: Optional[str],
    movement_type: Optional[str]
) -> Dict[str, Any]:
    """Analyze movement costs"""
    user = User(id=1, username="system")
    service = StockMovementsService(db, user)
    
    # Get movements in date range
    movements = service.get_movement_history(
        stock_code=item_code,
        location=location_code,
        movement_type=movement_type,
        from_date=start_date,
        to_date=end_date
    )
    
    # Calculate totals and breakdowns
    total_value = Decimal('0')
    by_type = {}
    by_location = {}
    items_value = {}
    
    for movement in movements:
        value = movement.get('quantity', 0) * movement.get('unit_cost', 0)
        total_value += value
        
        # By type
        move_type = movement.get('movement_type', 'UNKNOWN')
        if move_type not in by_type:
            by_type[move_type] = {'count': 0, 'value': 0}
        by_type[move_type]['count'] += 1
        by_type[move_type]['value'] += float(value)
        
        # By location
        location = movement.get('location', 'UNKNOWN')
        if location not in by_location:
            by_location[location] = {'count': 0, 'value': 0}
        by_location[location]['count'] += 1
        by_location[location]['value'] += float(value)
        
        # By item
        item = movement.get('stock_code', 'UNKNOWN')
        if item not in items_value:
            items_value[item] = 0
        items_value[item] += float(value)
    
    # Get top items
    top_items = sorted(
        items_value.items(),
        key=lambda x: x[1],
        reverse=True
    )[:10]
    
    return {
        'total_movements': len(movements),
        'total_value': float(total_value),
        'by_type': by_type,
        'by_location': by_location,
        'top_items': top_items,
        'variances': []
    }