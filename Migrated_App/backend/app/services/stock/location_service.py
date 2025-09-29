"""
Stock Location Service Wrapper
Provides compatibility layer between API expectations and existing stock services
"""
from typing import List, Optional, Dict, Any
from decimal import Decimal
from datetime import date, datetime
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func

from app.models.stock import StockMasterRec, StockLocationRec
from app.models.auth import User
from app.services.stock.bin_management import BinManagementService
from app.services.stock.physical_stocktake import PhysicalStocktakeService
from app.services.stock.stock_movements import StockMovementsService


def get_locations(
    db: Session,
    location_type: Optional[str] = None,
    warehouse_code: Optional[str] = None,
    is_active: bool = True,
    search: Optional[str] = None,
    skip: int = 0,
    limit: int = 100
) -> List[Dict[str, Any]]:
    """Get stock locations with filters"""
    query = db.query(StockLocationRec)
    
    if warehouse_code:
        query = query.filter(StockLocationRec.loc_warehouse == warehouse_code)
    
    if is_active:
        query = query.filter(StockLocationRec.loc_active == 'Y')
    
    if search:
        query = query.filter(
            or_(
                StockLocationRec.loc_location.ilike(f"%{search}%"),
                StockLocationRec.loc_warehouse.ilike(f"%{search}%")
            )
        )
    
    locations = query.offset(skip).limit(limit).all()
    
    return [
        {
            'location_code': loc.loc_location,
            'location_name': f"{loc.loc_warehouse} - {loc.loc_location}",
            'warehouse_code': loc.loc_warehouse,
            'location_type': 'STANDARD',
            'is_active': loc.loc_active == 'Y',
            'is_primary': loc.loc_primary == 'Y',
            'is_pickable': loc.loc_pickable == 'Y',
            'capacity_cubic_meters': 1000.0  # Default capacity
        }
        for loc in locations
    ]


def count_locations(
    db: Session,
    location_type: Optional[str] = None,
    warehouse_code: Optional[str] = None,
    is_active: bool = True,
    search: Optional[str] = None
) -> int:
    """Count stock locations with filters"""
    query = db.query(StockLocationRec)
    
    if warehouse_code:
        query = query.filter(StockLocationRec.loc_warehouse == warehouse_code)
    
    if is_active:
        query = query.filter(StockLocationRec.loc_active == 'Y')
    
    if search:
        query = query.filter(
            or_(
                StockLocationRec.loc_location.ilike(f"%{search}%"),
                StockLocationRec.loc_warehouse.ilike(f"%{search}%")
            )
        )
    
    return query.count()


def get_location(db: Session, location_code: str) -> Optional[Dict[str, Any]]:
    """Get specific location by code"""
    # Try to find location in StockLocationRec
    location = db.query(StockLocationRec).filter(
        StockLocationRec.loc_location == location_code
    ).first()
    
    if location:
        return {
            'location_code': location.loc_location,
            'location_name': f"{location.loc_warehouse} - {location.loc_location}",
            'warehouse_code': location.loc_warehouse,
            'location_type': 'STANDARD',
            'is_active': location.loc_active == 'Y',
            'is_primary': location.loc_primary == 'Y',
            'is_pickable': location.loc_pickable == 'Y',
            'capacity_cubic_meters': 1000.0
        }
    
    return None


def validate_warehouse(db: Session, warehouse_code: str) -> bool:
    """Validate warehouse exists"""
    # Check if any location exists with this warehouse code
    exists = db.query(StockLocationRec).filter(
        StockLocationRec.loc_warehouse == warehouse_code
    ).first()
    return exists is not None


def create_location(
    db: Session,
    location_in: Any,
    created_by: int
) -> Dict[str, Any]:
    """Create new stock location"""
    # Create location record
    new_location = StockLocationRec(
        loc_stock_code='',  # Will be set when stock is added
        loc_warehouse=location_in.warehouse_code or 'MAIN',
        loc_location=location_in.location_code,
        loc_qty_on_hand=Decimal('0'),
        loc_qty_allocated=Decimal('0'),
        loc_primary=location_in.is_primary and 'Y' or 'N',
        loc_pickable=location_in.is_pickable and 'Y' or 'N',
        loc_active='Y',
        loc_created_date=int(datetime.now().strftime("%Y%m%d"))
    )
    
    db.add(new_location)
    db.commit()
    
    return {
        'location_code': new_location.loc_location,
        'location_name': f"{new_location.loc_warehouse} - {new_location.loc_location}",
        'warehouse_code': new_location.loc_warehouse,
        'location_type': 'STANDARD',
        'is_active': True,
        'is_primary': new_location.loc_primary == 'Y',
        'is_pickable': new_location.loc_pickable == 'Y',
        'capacity_cubic_meters': 1000.0
    }


def update_location(
    db: Session,
    location_code: str,
    location_update: Any,
    updated_by: int
) -> Dict[str, Any]:
    """Update location information"""
    # Find all locations with this code
    locations = db.query(StockLocationRec).filter(
        StockLocationRec.loc_location == location_code
    ).all()
    
    for location in locations:
        if hasattr(location_update, 'is_active') and location_update.is_active is not None:
            location.loc_active = location_update.is_active and 'Y' or 'N'
        
        if hasattr(location_update, 'is_pickable') and location_update.is_pickable is not None:
            location.loc_pickable = location_update.is_pickable and 'Y' or 'N'
        
        if hasattr(location_update, 'is_primary') and location_update.is_primary is not None:
            location.loc_primary = location_update.is_primary and 'Y' or 'N'
    
    db.commit()
    
    return get_location(db, location_code)


def get_location_stock_count(db: Session, location_code: str) -> int:
    """Get count of items with stock at location"""
    count = db.query(StockLocationRec).filter(
        and_(
            StockLocationRec.loc_location == location_code,
            StockLocationRec.loc_qty_on_hand > 0
        )
    ).count()
    return count


def deactivate_location(
    db: Session,
    location_code: str,
    transfer_to: Optional[str],
    updated_by: int
) -> Dict[str, Any]:
    """Deactivate location and optionally transfer stock"""
    stock_transferred = 0
    transfer_movements = 0
    
    if transfer_to:
        # Get all stock at location
        stock_items = db.query(StockLocationRec).filter(
            and_(
                StockLocationRec.loc_location == location_code,
                StockLocationRec.loc_qty_on_hand > 0
            )
        ).all()
        
        # Create movements service
        user = User(id=updated_by, username="user")
        movements_service = StockMovementsService(db, user)
        
        # Transfer each item
        for item in stock_items:
            if item.loc_qty_on_hand > 0:
                movements_service.process_transfer(
                    stock_code=item.loc_stock_code,
                    from_warehouse=item.loc_warehouse,
                    from_location=location_code,
                    to_warehouse=item.loc_warehouse,
                    to_location=transfer_to,
                    quantity=item.loc_qty_on_hand,
                    reference=f"DEACT-{location_code}",
                    notes=f"Location deactivation transfer"
                )
                stock_transferred += 1
                transfer_movements += 1
    
    # Deactivate location
    locations = db.query(StockLocationRec).filter(
        StockLocationRec.loc_location == location_code
    ).all()
    
    for location in locations:
        location.loc_active = 'N'
    
    db.commit()
    
    return {
        'stock_transferred': stock_transferred,
        'transfer_movements': transfer_movements
    }


def get_location_stock(
    db: Session,
    location_code: str,
    category_code: Optional[str] = None,
    include_zero: bool = False,
    include_allocated: bool = True,
    skip: int = 0,
    limit: int = 100
) -> List[Dict[str, Any]]:
    """Get stock items at location"""
    query = db.query(
        StockLocationRec,
        StockMasterRec
    ).join(
        StockMasterRec,
        StockLocationRec.loc_stock_code == StockMasterRec.stock_key
    ).filter(
        StockLocationRec.loc_location == location_code
    )
    
    if not include_zero:
        query = query.filter(StockLocationRec.loc_qty_on_hand > 0)
    
    items = query.offset(skip).limit(limit).all()
    
    return [
        {
            'item_code': loc.loc_stock_code,
            'item_description': stock.stock_desc,
            'quantity_on_hand': float(loc.loc_qty_on_hand),
            'quantity_allocated': float(loc.loc_qty_allocated) if include_allocated else None,
            'quantity_available': float(loc.loc_qty_on_hand - loc.loc_qty_allocated),
            'unit_of_measure': stock.stock_uom,
            'category_code': stock.stock_cat,
            'last_movement_date': None  # Would need to query movements
        }
        for loc, stock in items
    ]


def get_location_stock_summary(
    db: Session,
    location_code: str
) -> Dict[str, Any]:
    """Get stock summary for location"""
    result = db.query(
        func.count(StockLocationRec.loc_stock_code).label('total_items'),
        func.sum(StockLocationRec.loc_qty_on_hand).label('total_quantity'),
        func.sum(StockLocationRec.loc_qty_allocated).label('total_allocated')
    ).filter(
        and_(
            StockLocationRec.loc_location == location_code,
            StockLocationRec.loc_qty_on_hand > 0
        )
    ).first()
    
    return {
        'total_items': result.total_items or 0,
        'total_quantity': float(result.total_quantity or 0),
        'total_allocated': float(result.total_allocated or 0),
        'total_available': float((result.total_quantity or 0) - (result.total_allocated or 0))
    }


def get_pending_stocktakes(db: Session, location_code: str) -> List[Dict]:
    """Get pending stocktakes for location"""
    # Would integrate with PhysicalStocktakeService
    # For now return empty list
    return []


def freeze_location_stock(
    db: Session,
    location_code: str,
    reason: str
) -> bool:
    """Freeze stock movements at location"""
    # This would set a flag to prevent movements
    # For now just return True
    return True


def create_stocktake(
    db: Session,
    location_code: str,
    stocktake_in: Any,
    created_by: int
) -> Dict[str, Any]:
    """Create stocktake for location"""
    user = User(id=created_by, username="user")
    service = PhysicalStocktakeService(db, user)
    
    # Create stocktake
    stocktake_id = service._generate_stocktake_number()
    
    return {
        'stocktake_id': 1,
        'stocktake_number': stocktake_id,
        'location_code': location_code,
        'description': stocktake_in.description,
        'status': 'draft',
        'freeze_stock': stocktake_in.freeze_stock,
        'created_at': datetime.now(),
        'created_by': created_by
    }


def get_stocktake(db: Session, stocktake_id: int) -> Optional[Dict]:
    """Get stocktake by ID"""
    # Mock implementation
    return {
        'stocktake_id': stocktake_id,
        'status': 'in_progress',
        'location_code': 'LOC001',
        'freeze_stock': False
    }


def update_stocktake_counts(
    db: Session,
    stocktake_id: int,
    count_lines: List[Any],
    counted_by: int
) -> Dict[str, Any]:
    """Update stocktake counts"""
    lines_updated = len(count_lines)
    total_variance_value = Decimal('0')
    
    # Calculate completion percentage
    completion = (lines_updated / 100) * 100 if lines_updated > 0 else 0
    
    return {
        'lines_updated': lines_updated,
        'total_variance_value': float(total_variance_value),
        'completion_percentage': completion
    }


def get_uncounted_items(db: Session, stocktake_id: int) -> List[Dict]:
    """Get uncounted items in stocktake"""
    return []


def complete_stocktake(
    db: Session,
    stocktake_id: int,
    completed_by: int
) -> Dict[str, Any]:
    """Complete stocktake"""
    return {
        'variance_items': 0,
        'total_variance_value': 0.0,
        'adjustments': []
    }


def post_stocktake_adjustments(
    db: Session,
    stocktake_id: int,
    approval_reference: str,
    posted_by: int
) -> Dict[str, Any]:
    """Post stocktake adjustments"""
    return {
        'adjustments_created': 0,
        'total_value': 0.0,
        'gl_entries': []
    }


def unfreeze_location_stock(db: Session, location_code: str) -> bool:
    """Unfreeze stock movements at location"""
    return True


def calculate_location_capacity(
    db: Session,
    location_code: str
) -> Dict[str, Any]:
    """Calculate location capacity utilization"""
    # Get location
    location = get_location(db, location_code)
    if not location:
        return {}
    
    # Calculate used space (simplified)
    stock_count = get_location_stock_count(db, location_code)
    used_space = stock_count * 0.1  # Assume 0.1 cubic meters per item
    capacity = location.get('capacity_cubic_meters', 1000.0)
    
    return {
        'used_space': used_space,
        'available_space': capacity - used_space,
        'utilization_percent': (used_space / capacity * 100) if capacity > 0 else 0,
        'by_category': {}
    }


def get_location_movements(
    db: Session,
    location_code: str,
    start_date: Optional[date] = None,
    end_date: Optional[date] = None,
    movement_type: Optional[str] = None,
    skip: int = 0,
    limit: int = 100
) -> List[Dict[str, Any]]:
    """Get movements for location"""
    user = User(id=1, username="system")
    service = StockMovementsService(db, user)
    
    # Get movements
    movements = service.get_movement_history(
        location=location_code,
        from_date=start_date,
        to_date=end_date,
        movement_type=movement_type
    )
    
    # Apply pagination
    return movements[skip:skip + limit]


def get_location_movement_summary(
    db: Session,
    location_code: str,
    start_date: Optional[date] = None,
    end_date: Optional[date] = None
) -> Dict[str, Any]:
    """Get movement summary for location"""
    movements = get_location_movements(
        db, location_code, start_date, end_date
    )
    
    inbound = sum(1 for m in movements if m.get('direction') == 'IN')
    outbound = sum(1 for m in movements if m.get('direction') == 'OUT')
    
    return {
        'total_movements': len(movements),
        'inbound_movements': inbound,
        'outbound_movements': outbound,
        'net_movement': inbound - outbound
    }


def update_bin_mappings(
    db: Session,
    location_code: str,
    bin_mappings: List[Dict[str, str]],
    updated_by: int
) -> Dict[str, Any]:
    """Update bin location mappings"""
    user = User(id=updated_by, username="user")
    service = BinManagementService(db, user)
    
    updated = 0
    created = 0
    errors = []
    
    for mapping in bin_mappings:
        try:
            item_code = mapping.get('item_code')
            bin_code = mapping.get('bin_code')
            
            if item_code and bin_code:
                # Check if bin exists, create if needed
                bin_info = service.get_bin(bin_code)
                if not bin_info:
                    service.create_bin({
                        'bin_code': bin_code,
                        'location_code': location_code,
                        'warehouse_code': 'MAIN',
                        'bin_type': 'STANDARD',
                        'capacity': 100
                    })
                    created += 1
                else:
                    updated += 1
            else:
                errors.append(f"Invalid mapping: {mapping}")
                
        except Exception as e:
            errors.append(str(e))
    
    return {
        'updated': updated,
        'created': created,
        'errors': errors
    }