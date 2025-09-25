"""
Stock Master Service - ST010 migration
Handles stock item creation, maintenance, and configuration
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from datetime import datetime
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func

from app.services.file_handlers.stock_handler import StockFileHandler
from app.services.file_handlers.system_handler import SystemFileHandler
from app.models.stock import (
    StockMasterRec, StockLocationRec, StockBinRec,
    StockSupplierRec, StockPriceRec, StockBarcodeRec
)
from app.core.security import log_user_action
from app.models.auth import User


class StockMasterService:
    """
    Stock Master maintenance
    Implements ST010 functionality with WMS features
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.stock_handler = StockFileHandler(db)
        self.system_handler = SystemFileHandler(db)
        
    def create_stock_item(self, stock_data: Dict) -> Tuple[StockMasterRec, Optional[str]]:
        """
        Create new stock item
        Returns (stock_record, error_message)
        """
        # Validate stock code
        stock_code = stock_data.get('stock_code', '')
        if not self._validate_stock_code(stock_code):
            return None, "Invalid stock code format"
            
        # Check if already exists
        existing, status = self.stock_handler.process(4, key_value=stock_code)
        if status.fs_reply == "00":
            return None, "Stock item already exists"
            
        # Get system defaults
        system_rec, _ = self.system_handler.read_system_params()
        
        # Create stock record
        stock = StockMasterRec(
            stock_code=stock_code,
            stock_desc=stock_data.get('stock_desc', ''),
            stock_desc2=stock_data.get('stock_desc2', ''),
            stock_search_key=stock_data.get('stock_search_key', stock_code.upper()),
            
            # Classification
            stock_type=stock_data.get('stock_type', 'FG'),  # FG=Finished Goods
            stock_group=stock_data.get('stock_group', ''),
            stock_category=stock_data.get('stock_category', ''),
            stock_abc_class=stock_data.get('stock_abc_class', 'C'),
            
            # Units and packaging
            stock_unit=stock_data.get('stock_unit', 'EA'),
            stock_pack_qty=Decimal(str(stock_data.get('stock_pack_qty', 1))),
            stock_weight=Decimal(str(stock_data.get('stock_weight', 0))),
            stock_volume=Decimal(str(stock_data.get('stock_volume', 0))),
            
            # Quantities
            stock_on_hand=Decimal('0'),
            stock_allocated=Decimal('0'),
            stock_on_order=Decimal('0'),
            stock_in_transit=Decimal('0'),
            stock_quarantine=Decimal('0'),
            
            # Valuation
            stock_average_cost=Decimal(str(stock_data.get('stock_average_cost', 0))),
            stock_standard_cost=Decimal(str(stock_data.get('stock_standard_cost', 0))),
            stock_last_cost=Decimal(str(stock_data.get('stock_last_cost', 0))),
            stock_sell_price1=Decimal(str(stock_data.get('stock_sell_price1', 0))),
            stock_sell_price2=Decimal(str(stock_data.get('stock_sell_price2', 0))),
            stock_sell_price3=Decimal(str(stock_data.get('stock_sell_price3', 0))),
            
            # Control parameters
            stock_min_qty=Decimal(str(stock_data.get('stock_min_qty', 0))),
            stock_max_qty=Decimal(str(stock_data.get('stock_max_qty', 0))),
            stock_reorder_qty=Decimal(str(stock_data.get('stock_reorder_qty', 0))),
            stock_reorder_level=Decimal(str(stock_data.get('stock_reorder_level', 0))),
            stock_lead_time=stock_data.get('stock_lead_time', 0),
            
            # Supplier info
            stock_pref_supp=stock_data.get('stock_pref_supp', ''),
            stock_supp_code=stock_data.get('stock_supp_code', ''),
            
            # Status flags
            stock_active='Y',
            stock_sellable=stock_data.get('stock_sellable', 'Y'),
            stock_purchasable=stock_data.get('stock_purchasable', 'Y'),
            stock_manufactured=stock_data.get('stock_manufactured', 'N'),
            stock_lot_control=stock_data.get('stock_lot_control', 'N'),
            stock_serial_control=stock_data.get('stock_serial_control', 'N'),
            stock_expiry_control=stock_data.get('stock_expiry_control', 'N'),
            
            # WMS settings
            stock_pick_sequence=stock_data.get('stock_pick_sequence', 0),
            stock_putaway_group=stock_data.get('stock_putaway_group', ''),
            stock_cycle_count='A' if stock_data.get('stock_abc_class') == 'A' else 'C',
            
            # Dates
            stock_created_date=int(datetime.now().strftime("%Y%m%d")),
            stock_last_movement=0,
            stock_last_counted=0,
            stock_last_sold=0,
            stock_last_purchased=0,
            
            # Statistics
            stock_ytd_sales_qty=Decimal('0'),
            stock_ytd_sales_val=Decimal('0'),
            stock_ytd_cost_val=Decimal('0'),
            stock_movements_mtd=0
        )
        
        # Write stock record
        _, status = self.stock_handler.process(5, record=stock)
        if status.fs_reply != "00":
            return None, f"Failed to create stock item: {status.fs_reply}"
            
        # Create default location if warehouse enabled
        if system_rec and system_rec.stock_locations == 'Y':
            self._create_default_location(stock)
            
        # Create barcode records if provided
        barcodes = stock_data.get('barcodes', [])
        for barcode in barcodes:
            self._add_barcode(stock.stock_code, barcode)
            
        # Log creation
        if self.current_user:
            log_user_action(
                db=self.db,
                user=self.current_user,
                action="CREATE_STOCK_ITEM",
                table="stockmaster_rec",
                key=stock_code,
                new_values=stock_data,
                module="STOCK"
            )
            
        return stock, None
        
    def update_stock_item(self, stock_code: str, updates: Dict) -> Tuple[StockMasterRec, Optional[str]]:
        """
        Update existing stock item
        Returns (stock_record, error_message)
        """
        # Read existing stock
        stock, status = self.stock_handler.process(4, key_value=stock_code)
        if status.fs_reply != "00":
            return None, "Stock item not found"
            
        # Store old values for audit
        old_values = {
            'stock_desc': stock.stock_desc,
            'stock_active': stock.stock_active,
            'stock_min_qty': float(stock.stock_min_qty),
            'stock_reorder_qty': float(stock.stock_reorder_qty)
        }
        
        # Validate updates
        if 'stock_code' in updates and updates['stock_code'] != stock_code:
            return None, "Cannot change stock code"
            
        # Apply updates
        for field, value in updates.items():
            if hasattr(stock, field):
                if field in ['stock_on_hand', 'stock_allocated', 'stock_min_qty', 
                           'stock_max_qty', 'stock_reorder_qty', 'stock_average_cost',
                           'stock_sell_price1', 'stock_sell_price2', 'stock_sell_price3']:
                    value = Decimal(str(value))
                setattr(stock, field, value)
                
        # Update timestamps
        stock.stock_last_updated = int(datetime.now().strftime("%Y%m%d"))
        stock.stock_updated_by = self.current_user.username if self.current_user else 'SYSTEM'
        
        # Rewrite stock
        _, status = self.stock_handler.process(7, record=stock)
        if status.fs_reply != "00":
            return None, f"Failed to update stock item: {status.fs_reply}"
            
        # Log update
        if self.current_user:
            log_user_action(
                db=self.db,
                user=self.current_user,
                action="UPDATE_STOCK_ITEM",
                table="stockmaster_rec",
                key=stock_code,
                old_values=old_values,
                new_values=updates,
                module="STOCK"
            )
            
        return stock, None
        
    def delete_stock_item(self, stock_code: str) -> Optional[str]:
        """
        Delete stock item (mark as inactive)
        Returns error message or None on success
        """
        # Check if stock exists
        stock, status = self.stock_handler.process(4, key_value=stock_code)
        if status.fs_reply != "00":
            return "Stock item not found"
            
        # Check quantities
        if stock.stock_on_hand != 0:
            return "Cannot delete item with stock on hand"
            
        if stock.stock_allocated != 0:
            return "Cannot delete item with allocated stock"
            
        if stock.stock_on_order != 0:
            return "Cannot delete item with stock on order"
            
        # Check for recent transactions
        if self._has_recent_transactions(stock_code):
            return "Cannot delete item with recent transactions"
            
        # Mark as inactive instead of deleting
        stock.stock_active = 'N'
        stock.stock_inactive_date = int(datetime.now().strftime("%Y%m%d"))
        _, status = self.stock_handler.process(7, record=stock)
        
        if status.fs_reply != "00":
            return f"Failed to delete stock item: {status.fs_reply}"
            
        # Log deletion
        if self.current_user:
            log_user_action(
                db=self.db,
                user=self.current_user,
                action="DELETE_STOCK_ITEM",
                table="stockmaster_rec",
                key=stock_code,
                module="STOCK"
            )
            
        return None
        
    def get_stock_list(self, filters: Optional[Dict] = None) -> List[Dict]:
        """Get list of stock items with optional filters"""
        query = self.db.query(StockMasterRec)
        
        if filters:
            if filters.get('active_only'):
                query = query.filter(StockMasterRec.stock_active == 'Y')
                
            if filters.get('search'):
                search_term = f"%{filters['search']}%"
                query = query.filter(
                    or_(
                        StockMasterRec.stock_code.ilike(search_term),
                        StockMasterRec.stock_desc.ilike(search_term),
                        StockMasterRec.stock_search_key.ilike(search_term)
                    )
                )
                
            if filters.get('type'):
                query = query.filter(StockMasterRec.stock_type == filters['type'])
                
            if filters.get('group'):
                query = query.filter(StockMasterRec.stock_group == filters['group'])
                
            if filters.get('category'):
                query = query.filter(StockMasterRec.stock_category == filters['category'])
                
            if filters.get('low_stock'):
                query = query.filter(
                    StockMasterRec.stock_on_hand <= StockMasterRec.stock_reorder_level
                )
                
            if filters.get('supplier'):
                query = query.filter(StockMasterRec.stock_pref_supp == filters['supplier'])
                
        stocks = query.order_by(StockMasterRec.stock_code).all()
        
        return [
            {
                'stock_code': s.stock_code,
                'description': s.stock_desc,
                'type': s.stock_type,
                'group': s.stock_group,
                'unit': s.stock_unit,
                'on_hand': float(s.stock_on_hand),
                'allocated': float(s.stock_allocated),
                'available': float(s.stock_on_hand - s.stock_allocated),
                'on_order': float(s.stock_on_order),
                'reorder_level': float(s.stock_reorder_level),
                'cost': float(s.stock_average_cost),
                'sell_price': float(s.stock_sell_price1),
                'active': s.stock_active == 'Y'
            }
            for s in stocks
        ]
        
    def add_stock_location(self, stock_code: str, location_data: Dict) -> Tuple[StockLocationRec, Optional[str]]:
        """Add warehouse location for stock item"""
        # Verify stock exists
        stock, status = self.stock_handler.process(4, key_value=stock_code)
        if status.fs_reply != "00":
            return None, "Stock item not found"
            
        # Check location doesn't already exist
        existing = self.db.query(StockLocationRec).filter(
            and_(
                StockLocationRec.loc_stock_code == stock_code,
                StockLocationRec.loc_warehouse == location_data.get('warehouse', 'MAIN'),
                StockLocationRec.loc_location == location_data.get('location')
            )
        ).first()
        
        if existing:
            return None, "Location already exists for this item"
            
        # Create location record
        location = StockLocationRec(
            loc_stock_code=stock_code,
            loc_warehouse=location_data.get('warehouse', 'MAIN'),
            loc_location=location_data.get('location'),
            loc_bin=location_data.get('bin', ''),
            loc_qty_on_hand=Decimal(str(location_data.get('qty_on_hand', 0))),
            loc_qty_allocated=Decimal(str(location_data.get('qty_allocated', 0))),
            loc_qty_in_transit=Decimal('0'),
            loc_primary=location_data.get('primary', 'N'),
            loc_pickable=location_data.get('pickable', 'Y'),
            loc_replenish=location_data.get('replenish', 'Y'),
            loc_min_qty=Decimal(str(location_data.get('min_qty', 0))),
            loc_max_qty=Decimal(str(location_data.get('max_qty', 0))),
            loc_active='Y',
            loc_created_date=int(datetime.now().strftime("%Y%m%d"))
        )
        
        # If this is primary, unset other primary locations
        if location.loc_primary == 'Y':
            self.db.query(StockLocationRec).filter(
                and_(
                    StockLocationRec.loc_stock_code == stock_code,
                    StockLocationRec.loc_warehouse == location.loc_warehouse
                )
            ).update({'loc_primary': 'N'})
            
        self.db.add(location)
        self.db.flush()
        
        return location, None
        
    def add_stock_supplier(self, stock_code: str, supplier_data: Dict) -> Tuple[StockSupplierRec, Optional[str]]:
        """Add supplier for stock item"""
        # Verify stock exists
        stock, status = self.stock_handler.process(4, key_value=stock_code)
        if status.fs_reply != "00":
            return None, "Stock item not found"
            
        # Create supplier link
        stock_supplier = StockSupplierRec(
            ss_stock_code=stock_code,
            ss_supplier=supplier_data.get('supplier_code'),
            ss_supplier_code=supplier_data.get('supplier_item_code', ''),
            ss_supplier_desc=supplier_data.get('supplier_desc', ''),
            ss_pack_qty=Decimal(str(supplier_data.get('pack_qty', 1))),
            ss_lead_time=supplier_data.get('lead_time', 0),
            ss_min_order_qty=Decimal(str(supplier_data.get('min_order_qty', 0))),
            ss_last_price=Decimal(str(supplier_data.get('last_price', 0))),
            ss_currency=supplier_data.get('currency', 'GBP'),
            ss_primary=supplier_data.get('primary', 'N'),
            ss_active='Y',
            ss_created_date=int(datetime.now().strftime("%Y%m%d"))
        )
        
        # If this is primary, update stock master
        if stock_supplier.ss_primary == 'Y':
            stock.stock_pref_supp = stock_supplier.ss_supplier
            stock.stock_supp_code = stock_supplier.ss_supplier_code
            stock.stock_lead_time = stock_supplier.ss_lead_time
            self.stock_handler.process(7, record=stock)
            
            # Unset other primary suppliers
            self.db.query(StockSupplierRec).filter(
                StockSupplierRec.ss_stock_code == stock_code
            ).update({'ss_primary': 'N'})
            
        self.db.add(stock_supplier)
        self.db.flush()
        
        return stock_supplier, None
        
    def update_stock_prices(self, stock_code: str, price_data: Dict) -> Tuple[bool, Optional[str]]:
        """Update stock pricing"""
        # Get stock item
        stock, status = self.stock_handler.process(4, key_value=stock_code)
        if status.fs_reply != "00":
            return False, "Stock item not found"
            
        # Store old prices for history
        old_prices = {
            'sell_price1': float(stock.stock_sell_price1),
            'sell_price2': float(stock.stock_sell_price2),
            'sell_price3': float(stock.stock_sell_price3)
        }
        
        # Update prices
        if 'sell_price1' in price_data:
            stock.stock_sell_price1 = Decimal(str(price_data['sell_price1']))
        if 'sell_price2' in price_data:
            stock.stock_sell_price2 = Decimal(str(price_data['sell_price2']))
        if 'sell_price3' in price_data:
            stock.stock_sell_price3 = Decimal(str(price_data['sell_price3']))
            
        stock.stock_price_date = int(datetime.now().strftime("%Y%m%d"))
        
        # Save price history
        for i, (old_key, old_val) in enumerate(old_prices.items(), 1):
            new_val = float(getattr(stock, f"stock_{old_key}"))
            if old_val != new_val:
                price_hist = StockPriceRec(
                    price_stock_code=stock_code,
                    price_list=str(i),
                    price_date=stock.stock_price_date,
                    price_old=Decimal(str(old_val)),
                    price_new=Decimal(str(new_val)),
                    price_updated_by=self.current_user.username if self.current_user else 'SYSTEM'
                )
                self.db.add(price_hist)
                
        # Update stock
        self.stock_handler.process(7, record=stock)
        self.db.flush()
        
        # Log price change
        if self.current_user:
            log_user_action(
                db=self.db,
                user=self.current_user,
                action="UPDATE_STOCK_PRICES",
                table="stockmaster_rec",
                key=stock_code,
                old_values=old_prices,
                new_values=price_data,
                module="STOCK"
            )
            
        return True, None
        
    def add_barcode(self, stock_code: str, barcode: str, barcode_type: str = 'EAN13') -> Tuple[bool, Optional[str]]:
        """Add barcode to stock item"""
        # Verify stock exists
        stock, status = self.stock_handler.process(4, key_value=stock_code)
        if status.fs_reply != "00":
            return False, "Stock item not found"
            
        # Check barcode doesn't already exist
        existing = self.db.query(StockBarcodeRec).filter(
            StockBarcodeRec.barcode_code == barcode
        ).first()
        
        if existing:
            return False, f"Barcode already assigned to {existing.barcode_stock_code}"
            
        # Add barcode
        self._add_barcode(stock_code, {'barcode': barcode, 'type': barcode_type})
        return True, None
        
    def get_stock_availability(self, stock_code: str) -> Dict:
        """Get stock availability across all locations"""
        stock, status = self.stock_handler.process(4, key_value=stock_code)
        if status.fs_reply != "00":
            return {"error": "Stock item not found"}
            
        # Get location details
        locations = self.db.query(StockLocationRec).filter(
            and_(
                StockLocationRec.loc_stock_code == stock_code,
                StockLocationRec.loc_active == 'Y'
            )
        ).all()
        
        # Calculate totals
        total_on_hand = sum(loc.loc_qty_on_hand for loc in locations)
        total_allocated = sum(loc.loc_qty_allocated for loc in locations)
        
        return {
            'stock_code': stock.stock_code,
            'description': stock.stock_desc,
            'summary': {
                'on_hand': float(stock.stock_on_hand),
                'allocated': float(stock.stock_allocated),
                'available': float(stock.stock_on_hand - stock.stock_allocated),
                'on_order': float(stock.stock_on_order),
                'in_transit': float(stock.stock_in_transit),
                'quarantine': float(stock.stock_quarantine)
            },
            'locations': [
                {
                    'warehouse': loc.loc_warehouse,
                    'location': loc.loc_location,
                    'bin': loc.loc_bin,
                    'on_hand': float(loc.loc_qty_on_hand),
                    'allocated': float(loc.loc_qty_allocated),
                    'available': float(loc.loc_qty_on_hand - loc.loc_qty_allocated),
                    'primary': loc.loc_primary == 'Y',
                    'pickable': loc.loc_pickable == 'Y'
                }
                for loc in locations
            ],
            'reorder': {
                'level': float(stock.stock_reorder_level),
                'quantity': float(stock.stock_reorder_qty),
                'min': float(stock.stock_min_qty),
                'max': float(stock.stock_max_qty),
                'lead_time': stock.stock_lead_time
            }
        }
        
    def check_stock_allocation(self, stock_code: str, quantity: Decimal, 
                             warehouse: str = 'MAIN') -> Dict:
        """
        Check if stock can be allocated
        Returns allocation details
        """
        stock, status = self.stock_handler.process(4, key_value=stock_code)
        if status.fs_reply != "00":
            return {"can_allocate": False, "reason": "Stock item not found"}
            
        # Get available stock in warehouse
        available = self._get_available_stock(stock_code, warehouse)
        
        if quantity > available:
            return {
                "can_allocate": False,
                "reason": "Insufficient stock",
                "requested": float(quantity),
                "available": float(available),
                "shortage": float(quantity - available)
            }
            
        # Find best locations for picking
        pick_locations = self._get_pick_locations(stock_code, quantity, warehouse)
        
        return {
            "can_allocate": True,
            "requested": float(quantity),
            "available": float(available),
            "pick_locations": pick_locations
        }
        
    def calculate_stock_value(self, stock_code: Optional[str] = None, 
                            valuation_method: str = 'AVERAGE') -> Dict:
        """Calculate stock valuation"""
        if stock_code:
            # Single item valuation
            stock, status = self.stock_handler.process(4, key_value=stock_code)
            if status.fs_reply != "00":
                return {"error": "Stock item not found"}
                
            if valuation_method == 'AVERAGE':
                unit_cost = stock.stock_average_cost
            elif valuation_method == 'STANDARD':
                unit_cost = stock.stock_standard_cost
            elif valuation_method == 'LAST':
                unit_cost = stock.stock_last_cost
            else:
                unit_cost = stock.stock_average_cost
                
            value = stock.stock_on_hand * unit_cost
            
            return {
                'stock_code': stock.stock_code,
                'description': stock.stock_desc,
                'quantity': float(stock.stock_on_hand),
                'unit_cost': float(unit_cost),
                'total_value': float(value),
                'valuation_method': valuation_method
            }
        else:
            # Total stock valuation
            stocks = self.db.query(StockMasterRec).filter(
                and_(
                    StockMasterRec.stock_active == 'Y',
                    StockMasterRec.stock_on_hand > 0
                )
            ).all()
            
            total_value = Decimal('0')
            item_count = 0
            
            for stock in stocks:
                if valuation_method == 'AVERAGE':
                    unit_cost = stock.stock_average_cost
                elif valuation_method == 'STANDARD':
                    unit_cost = stock.stock_standard_cost
                else:
                    unit_cost = stock.stock_average_cost
                    
                total_value += stock.stock_on_hand * unit_cost
                item_count += 1
                
            return {
                'total_value': float(total_value),
                'item_count': item_count,
                'valuation_method': valuation_method,
                'valuation_date': datetime.now().strftime("%Y-%m-%d")
            }
            
    def _validate_stock_code(self, stock_code: str) -> bool:
        """Validate stock code format"""
        if not stock_code:
            return False
            
        # Check length and format based on system settings
        # Standard format is alphanumeric up to 16 characters
        if len(stock_code) > 16:
            return False
            
        # Must start with letter or number
        if not stock_code[0].isalnum():
            return False
            
        return True
        
    def _create_default_location(self, stock: StockMasterRec):
        """Create default warehouse location for new stock item"""
        location = StockLocationRec(
            loc_stock_code=stock.stock_code,
            loc_warehouse='MAIN',
            loc_location='UNASSIGNED',
            loc_bin='',
            loc_qty_on_hand=Decimal('0'),
            loc_qty_allocated=Decimal('0'),
            loc_primary='Y',
            loc_pickable='Y',
            loc_replenish='Y',
            loc_active='Y',
            loc_created_date=int(datetime.now().strftime("%Y%m%d"))
        )
        
        self.db.add(location)
        self.db.flush()
        
    def _add_barcode(self, stock_code: str, barcode_data: Dict):
        """Add barcode record"""
        barcode = StockBarcodeRec(
            barcode_code=barcode_data.get('barcode'),
            barcode_stock_code=stock_code,
            barcode_type=barcode_data.get('type', 'EAN13'),
            barcode_primary=barcode_data.get('primary', 'N'),
            barcode_active='Y',
            barcode_created_date=int(datetime.now().strftime("%Y%m%d"))
        )
        
        self.db.add(barcode)
        self.db.flush()
        
    def _has_recent_transactions(self, stock_code: str) -> bool:
        """Check if stock has recent transactions"""
        # Check last 90 days
        cutoff_date = int((datetime.now() - timedelta(days=90)).strftime("%Y%m%d"))
        
        # Would check stock movement history
        # For now, check last movement date
        stock, _ = self.stock_handler.process(4, key_value=stock_code)
        if stock and stock.stock_last_movement > cutoff_date:
            return True
            
        return False
        
    def _get_available_stock(self, stock_code: str, warehouse: str) -> Decimal:
        """Get available stock in warehouse"""
        result = self.db.query(
            func.sum(StockLocationRec.loc_qty_on_hand - StockLocationRec.loc_qty_allocated)
        ).filter(
            and_(
                StockLocationRec.loc_stock_code == stock_code,
                StockLocationRec.loc_warehouse == warehouse,
                StockLocationRec.loc_active == 'Y',
                StockLocationRec.loc_pickable == 'Y'
            )
        ).scalar()
        
        return result or Decimal('0')
        
    def _get_pick_locations(self, stock_code: str, quantity: Decimal, 
                          warehouse: str) -> List[Dict]:
        """Get optimal pick locations for quantity"""
        # Get pickable locations ordered by pick sequence
        locations = self.db.query(StockLocationRec).filter(
            and_(
                StockLocationRec.loc_stock_code == stock_code,
                StockLocationRec.loc_warehouse == warehouse,
                StockLocationRec.loc_active == 'Y',
                StockLocationRec.loc_pickable == 'Y',
                StockLocationRec.loc_qty_on_hand > StockLocationRec.loc_qty_allocated
            )
        ).order_by(
            StockLocationRec.loc_primary.desc(),
            StockLocationRec.loc_location
        ).all()
        
        pick_list = []
        remaining = quantity
        
        for loc in locations:
            available = loc.loc_qty_on_hand - loc.loc_qty_allocated
            
            if remaining <= 0:
                break
                
            pick_qty = min(remaining, available)
            
            pick_list.append({
                'location': loc.loc_location,
                'bin': loc.loc_bin,
                'quantity': float(pick_qty),
                'available': float(available)
            })
            
            remaining -= pick_qty
            
        return pick_list