"""
Stock File Handler - COBOL acas011 migration
Handles stock_rec (Inventory/Stock Control)
"""
from typing import Any, Tuple, Optional, List
from decimal import Decimal
from sqlalchemy.orm import Session
from sqlalchemy import or_, func, and_

from app.services.file_handlers.base_handler import BaseFileHandler, FileAccess, COBOLError
from app.models.stock import StockRec, StockAuditRec
from datetime import datetime


class StockFileHandler(BaseFileHandler):
    """
    Stock File Handler (acas011)
    Handles stock/inventory master records
    
    Key types:
    1 = Stock code (primary)
    2 = Abbreviated key
    3 = Description
    """
    
    def __init__(self, db: Session):
        super().__init__(db, file_number=11, table_name="stock_rec")
        
    def get_model_class(self):
        return StockRec
        
    def get_key_field(self, key_type: int = 1):
        """
        Stock file keys:
        1 = Stock code (primary)
        2 = Stock abbreviated key
        3 = Stock description
        """
        key_map = {
            1: "stock_key",
            2: "stock_abrev_key", 
            3: "stock_desc"
        }
        return key_map.get(key_type)
        
    def check_available_quantity(self, stock_key: str, required_qty: float) -> Tuple[bool, float]:
        """
        Check if sufficient quantity is available
        Returns (is_available, available_quantity)
        """
        stock, status = self._read_indexed(stock_key, 1)
        
        if status.fs_reply != "00" or not stock:
            return False, 0
            
        available = float(stock.stock_qty_available)
        is_available = available >= required_qty
        
        return is_available, available
        
    def allocate_stock(self, stock_key: str, quantity: float) -> FileAccess:
        """Allocate stock for an order"""
        stock, status = self._read_indexed(stock_key, 1)
        
        if status.fs_reply != "00" or not stock:
            return status
            
        # Check availability
        available = float(stock.stock_qty_available)
        if available < quantity:
            status.we_error = COBOLError.GENERAL_ERROR
            status.fs_reply = "99"
            return status
            
        # Update quantities
        stock.stock_qty_allocated = Decimal(str(float(stock.stock_qty_allocated) + quantity))
        stock.stock_qty_available = Decimal(str(float(stock.stock_qty_available) - quantity))
        
        _, status = self._rewrite_record(stock, stock_key, 1)
        
        # Create audit record
        if status.fs_reply == "00":
            self._create_audit_record(stock_key, "ALLOCATE", quantity, stock)
            
        return status
        
    def release_allocation(self, stock_key: str, quantity: float) -> FileAccess:
        """Release previously allocated stock"""
        stock, status = self._read_indexed(stock_key, 1)
        
        if status.fs_reply != "00" or not stock:
            return status
            
        # Update quantities
        stock.stock_qty_allocated = Decimal(str(max(0, float(stock.stock_qty_allocated) - quantity)))
        stock.stock_qty_available = Decimal(str(float(stock.stock_qty_available) + quantity))
        
        _, status = self._rewrite_record(stock, stock_key, 1)
        
        # Create audit record
        if status.fs_reply == "00":
            self._create_audit_record(stock_key, "RELEASE", quantity, stock)
            
        return status
        
    def process_sale(self, stock_key: str, quantity: float, cost: Optional[float] = None) -> FileAccess:
        """Process stock sale - reduce on-hand and allocated"""
        stock, status = self._read_indexed(stock_key, 1)
        
        if status.fs_reply != "00" or not stock:
            return status
            
        # Update quantities
        old_on_hand = float(stock.stock_qty_on_hand)
        stock.stock_qty_on_hand = Decimal(str(old_on_hand - quantity))
        stock.stock_qty_allocated = Decimal(str(max(0, float(stock.stock_qty_allocated) - quantity)))
        
        # Update MTD/YTD usage
        stock.stock_mtd_usage = Decimal(str(float(stock.stock_mtd_usage) + quantity))
        stock.stock_ytd_usage = Decimal(str(float(stock.stock_ytd_usage) + quantity))
        stock.stock_date_last_sale = int(datetime.now().strftime("%Y%m%d"))
        
        # Calculate cost if not provided
        if cost is None:
            cost = self._calculate_cost(stock, quantity)
            
        _, status = self._rewrite_record(stock, stock_key, 1)
        
        # Create audit record
        if status.fs_reply == "00":
            self._create_audit_record(stock_key, "ISSUE", quantity, stock, cost)
            
        return status
        
    def process_receipt(self, stock_key: str, quantity: float, cost: float) -> FileAccess:
        """Process stock receipt - increase on-hand and available"""
        stock, status = self._read_indexed(stock_key, 1)
        
        if status.fs_reply != "00" or not stock:
            return status
            
        # Store old values for average cost calculation
        old_on_hand = float(stock.stock_qty_on_hand)
        old_avg_cost = float(stock.stock_avg_cost)
        
        # Update quantities
        stock.stock_qty_on_hand = Decimal(str(old_on_hand + quantity))
        stock.stock_qty_available = Decimal(str(float(stock.stock_qty_available) + quantity))
        stock.stock_qty_on_order = Decimal(str(max(0, float(stock.stock_qty_on_order) - quantity)))
        
        # Update costs
        stock.stock_last_cost = Decimal(str(cost))
        
        # Calculate new average cost
        if stock.stock_costing_method == 'A':  # Average costing
            total_value = (old_on_hand * old_avg_cost) + (quantity * cost)
            new_on_hand = old_on_hand + quantity
            if new_on_hand > 0:
                stock.stock_avg_cost = Decimal(str(total_value / new_on_hand))
                
        stock.stock_date_last_receipt = int(datetime.now().strftime("%Y%m%d"))
        
        _, status = self._rewrite_record(stock, stock_key, 1)
        
        # Create audit record
        if status.fs_reply == "00":
            self._create_audit_record(stock_key, "RECEIPT", quantity, stock, cost)
            
        return status
        
    def adjust_stock(self, stock_key: str, new_quantity: float, reason: str) -> FileAccess:
        """Adjust stock quantity to specific value"""
        stock, status = self._read_indexed(stock_key, 1)
        
        if status.fs_reply != "00" or not stock:
            return status
            
        old_quantity = float(stock.stock_qty_on_hand)
        adjustment = new_quantity - old_quantity
        
        # Update quantities
        stock.stock_qty_on_hand = Decimal(str(new_quantity))
        stock.stock_qty_available = Decimal(str(
            float(stock.stock_qty_available) + adjustment
        ))
        
        _, status = self._rewrite_record(stock, stock_key, 1)
        
        # Create audit record
        if status.fs_reply == "00":
            self._create_audit_record(stock_key, "ADJUST", adjustment, stock, reason=reason)
            
        return status
        
    def transfer_stock(self, stock_key: str, from_location: str, to_location: str, 
                      quantity: float) -> FileAccess:
        """Transfer stock between locations"""
        # For simple implementation, just update location and create audit
        stock, status = self._read_indexed(stock_key, 1)
        
        if status.fs_reply != "00" or not stock:
            return status
            
        old_location = stock.stock_location
        stock.stock_location = to_location
        
        _, status = self._rewrite_record(stock, stock_key, 1)
        
        # Create audit record
        if status.fs_reply == "00":
            audit = StockAuditRec(
                audit_date=int(datetime.now().strftime("%Y%m%d")),
                audit_time=int(datetime.now().strftime("%H%M%S")),
                audit_stock_code=stock_key,
                audit_type="TRANSFER",
                audit_qty=Decimal(str(quantity)),
                audit_location_from=from_location,
                audit_location_to=to_location,
                audit_user="SYSTEM"
            )
            self.db.add(audit)
            self.db.flush()
            
        return status
        
    def get_reorder_items(self) -> List[StockRec]:
        """Get items that need reordering"""
        return self.db.query(StockRec).filter(
            and_(
                StockRec.stock_qty_available <= StockRec.stock_reorder_point,
                StockRec.stock_reorder_point > 0,
                StockRec.stock_discontinued == 'N'
            )
        ).order_by(StockRec.stock_key).all()
        
    def get_low_stock_items(self) -> List[StockRec]:
        """Get items with low stock"""
        return self.db.query(StockRec).filter(
            and_(
                StockRec.stock_qty_available <= StockRec.stock_min_qty,
                StockRec.stock_min_qty > 0,
                StockRec.stock_discontinued == 'N'
            )
        ).order_by(StockRec.stock_key).all()
        
    def get_overstock_items(self) -> List[StockRec]:
        """Get items with excess stock"""
        return self.db.query(StockRec).filter(
            and_(
                StockRec.stock_qty_on_hand > StockRec.stock_max_qty,
                StockRec.stock_max_qty > 0
            )
        ).order_by(StockRec.stock_key).all()
        
    def search_by_description(self, search_term: str) -> List[StockRec]:
        """Search stock items by description"""
        return self.db.query(StockRec).filter(
            func.upper(StockRec.stock_desc).contains(search_term.upper())
        ).order_by(StockRec.stock_desc).limit(50).all()
        
    def get_by_product_group(self, group_code: str) -> List[StockRec]:
        """Get all items in a product group"""
        return self.db.query(StockRec).filter(
            StockRec.stock_product_group == group_code
        ).order_by(StockRec.stock_key).all()
        
    def _calculate_cost(self, stock: StockRec, quantity: float) -> float:
        """Calculate cost based on costing method"""
        if stock.stock_costing_method == 'A':  # Average
            return float(stock.stock_avg_cost)
        elif stock.stock_costing_method == 'S':  # Standard
            return float(stock.stock_std_cost)
        elif stock.stock_costing_method == 'F':  # FIFO
            return float(stock.stock_fifo_cost)
        elif stock.stock_costing_method == 'L':  # LIFO
            return float(stock.stock_lifo_cost)
        else:
            return float(stock.stock_avg_cost)
            
    def _create_audit_record(self, stock_key: str, audit_type: str, quantity: float, 
                           stock: StockRec, cost: Optional[float] = None, reason: str = ""):
        """Create stock audit record"""
        audit = StockAuditRec(
            audit_date=int(datetime.now().strftime("%Y%m%d")),
            audit_time=int(datetime.now().strftime("%H%M%S")),
            audit_stock_code=stock_key,
            audit_type=audit_type,
            audit_qty=Decimal(str(quantity)),
            audit_qty_before=stock.stock_qty_on_hand - Decimal(str(quantity)),
            audit_qty_after=stock.stock_qty_on_hand,
            audit_cost=Decimal(str(cost)) if cost else stock.stock_avg_cost,
            audit_value=Decimal(str(quantity * (cost if cost else float(stock.stock_avg_cost)))),
            audit_location_from=stock.stock_location,
            audit_location_to=stock.stock_location,
            audit_user="SYSTEM",
            audit_reason=reason
        )
        self.db.add(audit)
        self.db.flush()
        
    def calculate_stock_value(self, method: str = "AVG") -> float:
        """Calculate total stock value using specified method"""
        stocks = self.db.query(StockRec).filter(
            StockRec.stock_qty_on_hand > 0
        ).all()
        
        total_value = 0.0
        for stock in stocks:
            qty = float(stock.stock_qty_on_hand)
            if method == "AVG":
                cost = float(stock.stock_avg_cost)
            elif method == "STD":
                cost = float(stock.stock_std_cost)
            elif method == "LAST":
                cost = float(stock.stock_last_cost)
            else:
                cost = float(stock.stock_avg_cost)
                
            total_value += qty * cost
            
        return total_value