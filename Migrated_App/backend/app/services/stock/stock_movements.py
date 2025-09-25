"""
Stock Movements Service - ST030 migration
Handles all stock movement transactions and processing
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from datetime import datetime
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func, desc

from app.services.file_handlers.stock_handler import StockFileHandler
from app.services.file_handlers.system_handler import SystemFileHandler
from app.models.stock import (
    StockMasterRec, StockLocationRec, StockMovementRec,
    StockTransferRec, StockAdjustmentRec, StockCountRec
)
from app.services.gl.journal_entry import JournalEntryService
from app.core.security import log_user_action
from app.models.auth import User


class StockMovementsService:
    """
    Stock Movements functionality
    Implements ST030 - all stock movement processing
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.stock_handler = StockFileHandler(db)
        self.system_handler = SystemFileHandler(db)
        
    def process_receipt(self, receipt_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Process stock receipt (goods in)
        Returns (success, error_message)
        """
        stock_code = receipt_data.get('stock_code')
        warehouse = receipt_data.get('warehouse', 'MAIN')
        location = receipt_data.get('location', 'UNASSIGNED')
        quantity = Decimal(str(receipt_data.get('quantity', 0)))
        unit_cost = Decimal(str(receipt_data.get('unit_cost', 0)))
        reference = receipt_data.get('reference', '')
        
        if quantity <= 0:
            return False, "Quantity must be greater than zero"
            
        # Get stock item
        stock, status = self.stock_handler.process(4, key_value=stock_code)
        if status.fs_reply != "00":
            return False, "Stock item not found"
            
        try:
            # 1. Update stock master quantities
            old_balance = stock.stock_on_hand
            stock.stock_on_hand += quantity
            
            # Update average cost if unit cost provided
            if unit_cost > 0:
                total_value = (old_balance * stock.stock_average_cost) + (quantity * unit_cost)
                new_quantity = old_balance + quantity
                if new_quantity > 0:
                    stock.stock_average_cost = total_value / new_quantity
                stock.stock_last_cost = unit_cost
                
            # Update statistics
            stock.stock_last_movement = int(datetime.now().strftime("%Y%m%d"))
            stock.stock_last_purchased = int(datetime.now().strftime("%Y%m%d"))
            stock.stock_movements_mtd += 1
            
            # Save stock master
            self.stock_handler.process(7, record=stock)
            
            # 2. Update location quantities
            self._update_location_quantity(stock_code, warehouse, location, quantity, 'IN')
            
            # 3. Create movement record
            movement = self._create_movement_record(
                stock_code=stock_code,
                move_type='REC',
                warehouse=warehouse,
                location=location,
                qty_in=quantity,
                qty_out=Decimal('0'),
                unit_cost=unit_cost,
                reference=reference,
                balance=stock.stock_on_hand
            )
            
            # 4. Post to GL if configured
            self._post_movement_to_gl(movement, stock)
            
            self.db.commit()
            
            # Log movement
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="STOCK_RECEIPT",
                    table="stockmaster_rec",
                    key=stock_code,
                    new_values={
                        'quantity': float(quantity),
                        'cost': float(unit_cost),
                        'reference': reference
                    },
                    module="STOCK"
                )
                
            return True, None
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def process_issue(self, issue_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Process stock issue (goods out)
        Returns (success, error_message)
        """
        stock_code = issue_data.get('stock_code')
        warehouse = issue_data.get('warehouse', 'MAIN')
        location = issue_data.get('location')
        quantity = Decimal(str(issue_data.get('quantity', 0)))
        reference = issue_data.get('reference', '')
        cost_method = issue_data.get('cost_method', 'AVERAGE')  # AVERAGE, FIFO, LIFO, STANDARD
        
        if quantity <= 0:
            return False, "Quantity must be greater than zero"
            
        # Get stock item
        stock, status = self.stock_handler.process(4, key_value=stock_code)
        if status.fs_reply != "00":
            return False, "Stock item not found"
            
        # Check availability
        available = self._get_available_stock(stock_code, warehouse, location)
        if quantity > available:
            return False, f"Insufficient stock. Available: {available}, Requested: {quantity}"
            
        try:
            # Determine cost
            unit_cost = self._get_issue_cost(stock, cost_method)
            
            # 1. Update stock master quantities
            old_balance = stock.stock_on_hand
            stock.stock_on_hand -= quantity
            
            # Update statistics
            stock.stock_last_movement = int(datetime.now().strftime("%Y%m%d"))
            stock.stock_movements_mtd += 1
            
            # Save stock master
            self.stock_handler.process(7, record=stock)
            
            # 2. Update location quantities
            if location:
                self._update_location_quantity(stock_code, warehouse, location, quantity, 'OUT')
            else:
                # Auto-select best locations
                self._auto_allocate_issue(stock_code, warehouse, quantity)
                
            # 3. Create movement record
            movement = self._create_movement_record(
                stock_code=stock_code,
                move_type='ISS',
                warehouse=warehouse,
                location=location or 'AUTO',
                qty_in=Decimal('0'),
                qty_out=quantity,
                unit_cost=unit_cost,
                reference=reference,
                balance=stock.stock_on_hand
            )
            
            # 4. Post to GL if configured
            self._post_movement_to_gl(movement, stock)
            
            self.db.commit()
            
            # Log movement
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="STOCK_ISSUE",
                    table="stockmaster_rec",
                    key=stock_code,
                    new_values={
                        'quantity': float(quantity),
                        'cost': float(unit_cost),
                        'reference': reference
                    },
                    module="STOCK"
                )
                
            return True, None
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def process_transfer(self, transfer_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Process stock transfer between locations
        Returns (success, error_message)
        """
        stock_code = transfer_data.get('stock_code')
        from_warehouse = transfer_data.get('from_warehouse')
        from_location = transfer_data.get('from_location')
        to_warehouse = transfer_data.get('to_warehouse')
        to_location = transfer_data.get('to_location')
        quantity = Decimal(str(transfer_data.get('quantity', 0)))
        reference = transfer_data.get('reference', '')
        immediate = transfer_data.get('immediate', True)
        
        if quantity <= 0:
            return False, "Quantity must be greater than zero"
            
        # Validate locations are different
        if (from_warehouse == to_warehouse and from_location == to_location):
            return False, "Source and destination locations cannot be the same"
            
        # Get stock item
        stock, status = self.stock_handler.process(4, key_value=stock_code)
        if status.fs_reply != "00":
            return False, "Stock item not found"
            
        # Check availability
        available = self._get_available_stock(stock_code, from_warehouse, from_location)
        if quantity > available:
            return False, f"Insufficient stock at source location. Available: {available}"
            
        try:
            # Get next transfer number
            transfer_no = self._get_next_transfer_number()
            
            if immediate:
                # Process immediate transfer
                return self._process_immediate_transfer(
                    stock_code, from_warehouse, from_location,
                    to_warehouse, to_location, quantity, reference, transfer_no
                )
            else:
                # Create transfer request
                return self._create_transfer_request(
                    stock_code, from_warehouse, from_location,
                    to_warehouse, to_location, quantity, reference, transfer_no
                )
                
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def process_adjustment(self, adjustment_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Process stock adjustment
        Returns (success, error_message)
        """
        stock_code = adjustment_data.get('stock_code')
        warehouse = adjustment_data.get('warehouse', 'MAIN')
        location = adjustment_data.get('location', 'UNASSIGNED')
        adjustment_qty = Decimal(str(adjustment_data.get('adjustment_qty', 0)))
        reason = adjustment_data.get('reason', '')
        reference = adjustment_data.get('reference', '')
        new_cost = adjustment_data.get('new_cost')  # Optional cost adjustment
        
        if adjustment_qty == 0:
            return False, "Adjustment quantity cannot be zero"
            
        # Get stock item
        stock, status = self.stock_handler.process(4, key_value=stock_code)
        if status.fs_reply != "00":
            return False, "Stock item not found"
            
        # Check if negative adjustment would create negative stock
        if adjustment_qty < 0:
            available = self._get_available_stock(stock_code, warehouse, location)
            if abs(adjustment_qty) > available:
                return False, f"Adjustment would create negative stock. Available: {available}"
                
        try:
            # Get adjustment number
            adj_no = self._get_next_adjustment_number()
            
            # Calculate costs
            if adjustment_qty > 0:
                # Positive adjustment - use current average cost or provided cost
                unit_cost = Decimal(str(new_cost)) if new_cost else stock.stock_average_cost
            else:
                # Negative adjustment - use current average cost
                unit_cost = stock.stock_average_cost
                
            # 1. Create adjustment record
            adjustment = StockAdjustmentRec(
                adj_no=adj_no,
                adj_stock_code=stock_code,
                adj_warehouse=warehouse,
                adj_location=location,
                adj_date=int(datetime.now().strftime("%Y%m%d")),
                adj_time=int(datetime.now().strftime("%H%M%S")),
                adj_quantity=adjustment_qty,
                adj_unit_cost=unit_cost,
                adj_total_cost=adjustment_qty * unit_cost,
                adj_reason=reason,
                adj_reference=reference,
                adj_user=self.current_user.username if self.current_user else 'SYSTEM',
                adj_approved='N',
                adj_posted='N'
            )
            
            self.db.add(adjustment)
            self.db.flush()
            
            # 2. If auto-approved, post immediately
            system_rec, _ = self.system_handler.read_system_params()
            auto_approve = system_rec and system_rec.stock_adj_auto_approve == 'Y'
            
            if auto_approve:
                return self.approve_adjustment(adj_no)
            else:
                self.db.commit()
                return True, f"Adjustment {adj_no} created - pending approval"
                
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def approve_adjustment(self, adj_no: str) -> Tuple[bool, Optional[str]]:
        """
        Approve and post stock adjustment
        Returns (success, error_message)
        """
        # Get adjustment record
        adjustment = self.db.query(StockAdjustmentRec).filter(
            StockAdjustmentRec.adj_no == adj_no
        ).first()
        
        if not adjustment:
            return False, "Adjustment not found"
            
        if adjustment.adj_posted == 'Y':
            return False, "Adjustment already posted"
            
        try:
            stock_code = adjustment.adj_stock_code
            
            # Get stock item
            stock, status = self.stock_handler.process(4, key_value=stock_code)
            if status.fs_reply != "00":
                return False, "Stock item not found"
                
            # 1. Update stock master
            old_balance = stock.stock_on_hand
            stock.stock_on_hand += adjustment.adj_quantity
            
            # Update average cost if positive adjustment
            if adjustment.adj_quantity > 0 and adjustment.adj_unit_cost > 0:
                total_value = (old_balance * stock.stock_average_cost) + (adjustment.adj_quantity * adjustment.adj_unit_cost)
                new_quantity = old_balance + adjustment.adj_quantity
                if new_quantity > 0:
                    stock.stock_average_cost = total_value / new_quantity
                    
            # Update statistics
            stock.stock_last_movement = int(datetime.now().strftime("%Y%m%d"))
            stock.stock_movements_mtd += 1
            
            self.stock_handler.process(7, record=stock)
            
            # 2. Update location quantities
            self._update_location_quantity(
                stock_code, adjustment.adj_warehouse, adjustment.adj_location,
                abs(adjustment.adj_quantity), 'IN' if adjustment.adj_quantity > 0 else 'OUT'
            )
            
            # 3. Create movement record
            movement = self._create_movement_record(
                stock_code=stock_code,
                move_type='ADJ',
                warehouse=adjustment.adj_warehouse,
                location=adjustment.adj_location,
                qty_in=adjustment.adj_quantity if adjustment.adj_quantity > 0 else Decimal('0'),
                qty_out=abs(adjustment.adj_quantity) if adjustment.adj_quantity < 0 else Decimal('0'),
                unit_cost=adjustment.adj_unit_cost,
                reference=f"ADJ-{adj_no}",
                balance=stock.stock_on_hand
            )
            
            # 4. Update adjustment record
            adjustment.adj_approved = 'Y'
            adjustment.adj_posted = 'Y'
            adjustment.adj_approved_by = self.current_user.username if self.current_user else 'SYSTEM'
            adjustment.adj_approved_date = int(datetime.now().strftime("%Y%m%d"))
            adjustment.adj_movement_ref = movement.move_id
            
            # 5. Post to GL
            self._post_adjustment_to_gl(adjustment, stock)
            
            self.db.commit()
            
            # Log approval
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="APPROVE_STOCK_ADJUSTMENT",
                    table="stock_adjustment_rec",
                    key=adj_no,
                    new_values={
                        'quantity': float(adjustment.adj_quantity),
                        'value': float(adjustment.adj_total_cost)
                    },
                    module="STOCK"
                )
                
            return True, None
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def process_count(self, count_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Process stock count and create adjustments
        Returns (success, error_message)
        """
        stock_code = count_data.get('stock_code')
        warehouse = count_data.get('warehouse', 'MAIN')
        location = count_data.get('location', 'UNASSIGNED')
        counted_qty = Decimal(str(count_data.get('counted_qty', 0)))
        count_date = count_data.get('count_date', int(datetime.now().strftime("%Y%m%d")))
        counter = count_data.get('counter', self.current_user.username if self.current_user else 'SYSTEM')
        
        # Get stock item
        stock, status = self.stock_handler.process(4, key_value=stock_code)
        if status.fs_reply != "00":
            return False, "Stock item not found"
            
        # Get current system quantity at location
        system_qty = self._get_location_quantity(stock_code, warehouse, location)
        
        # Calculate variance
        variance = counted_qty - system_qty
        
        try:
            # Get count number
            count_no = self._get_next_count_number()
            
            # Create count record
            count = StockCountRec(
                count_no=count_no,
                count_stock_code=stock_code,
                count_warehouse=warehouse,
                count_location=location,
                count_date=count_date,
                count_time=int(datetime.now().strftime("%H%M%S")),
                count_system_qty=system_qty,
                count_counted_qty=counted_qty,
                count_variance=variance,
                count_counter=counter,
                count_approved='N',
                count_posted='N'
            )
            
            self.db.add(count)
            self.db.flush()
            
            # If variance exists and within tolerance, create adjustment
            system_rec, _ = self.system_handler.read_system_params()
            tolerance = system_rec.stock_count_tolerance if system_rec else Decimal('0')
            
            if variance != 0:
                if tolerance == 0 or abs(variance) <= tolerance:
                    # Auto-create adjustment
                    adjustment_data = {
                        'stock_code': stock_code,
                        'warehouse': warehouse,
                        'location': location,
                        'adjustment_qty': variance,
                        'reason': 'COUNT_VARIANCE',
                        'reference': f"COUNT-{count_no}"
                    }
                    
                    success, error = self.process_adjustment(adjustment_data)
                    if not success:
                        return False, f"Failed to create adjustment: {error}"
                        
                    count.count_adjustment_ref = count_no
                    
            # Update stock count date
            stock.stock_last_counted = count_date
            self.stock_handler.process(7, record=stock)
            
            self.db.commit()
            
            # Log count
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="STOCK_COUNT",
                    table="stock_count_rec",
                    key=count_no,
                    new_values={
                        'system_qty': float(system_qty),
                        'counted_qty': float(counted_qty),
                        'variance': float(variance)
                    },
                    module="STOCK"
                )
                
            return True, f"Count {count_no} processed. Variance: {variance}"
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def get_movement_history(self, filters: Dict) -> List[Dict]:
        """Get movement history with filters"""
        query = self.db.query(StockMovementRec)
        
        # Apply filters
        if filters.get('stock_code'):
            query = query.filter(StockMovementRec.move_stock_code == filters['stock_code'])
        if filters.get('warehouse'):
            query = query.filter(StockMovementRec.move_warehouse == filters['warehouse'])
        if filters.get('location'):
            query = query.filter(StockMovementRec.move_location == filters['location'])
        if filters.get('type'):
            query = query.filter(StockMovementRec.move_type == filters['type'])
        if filters.get('date_from'):
            query = query.filter(StockMovementRec.move_date >= filters['date_from'])
        if filters.get('date_to'):
            query = query.filter(StockMovementRec.move_date <= filters['date_to'])
        if filters.get('reference'):
            query = query.filter(StockMovementRec.move_reference.ilike(f"%{filters['reference']}%"))
            
        # Order and limit
        limit = filters.get('limit', 1000)
        movements = query.order_by(desc(StockMovementRec.move_date), desc(StockMovementRec.move_time)).limit(limit).all()
        
        return [
            {
                'movement_id': mov.move_id,
                'date': mov.move_date,
                'time': mov.move_time,
                'stock_code': mov.move_stock_code,
                'type': mov.move_type,
                'type_desc': self._get_movement_type_desc(mov.move_type),
                'warehouse': mov.move_warehouse,
                'location': mov.move_location,
                'quantity_in': float(mov.move_qty_in) if mov.move_qty_in else 0,
                'quantity_out': float(mov.move_qty_out) if mov.move_qty_out else 0,
                'net_quantity': float((mov.move_qty_in or 0) - (mov.move_qty_out or 0)),
                'unit_cost': float(mov.move_unit_cost) if mov.move_unit_cost else 0,
                'total_cost': float(mov.move_total_cost) if mov.move_total_cost else 0,
                'balance': float(mov.move_balance) if mov.move_balance else 0,
                'reference': mov.move_reference,
                'user': mov.move_user
            }
            for mov in movements
        ]
        
    def get_pending_transfers(self, warehouse: Optional[str] = None) -> List[Dict]:
        """Get pending transfer requests"""
        query = self.db.query(StockTransferRec).filter(
            StockTransferRec.trans_status == 'PENDING'
        )
        
        if warehouse:
            query = query.filter(
                or_(
                    StockTransferRec.trans_from_warehouse == warehouse,
                    StockTransferRec.trans_to_warehouse == warehouse
                )
            )
            
        transfers = query.order_by(StockTransferRec.trans_date_created).all()
        
        return [
            {
                'transfer_no': trans.trans_no,
                'stock_code': trans.trans_stock_code,
                'description': trans.trans_description,
                'from_warehouse': trans.trans_from_warehouse,
                'from_location': trans.trans_from_location,
                'to_warehouse': trans.trans_to_warehouse,
                'to_location': trans.trans_to_location,
                'quantity': float(trans.trans_quantity),
                'date_created': trans.trans_date_created,
                'date_required': trans.trans_date_required,
                'created_by': trans.trans_created_by,
                'priority': trans.trans_priority
            }
            for trans in transfers
        ]
        
    def get_pending_adjustments(self) -> List[Dict]:
        """Get pending adjustments requiring approval"""
        adjustments = self.db.query(StockAdjustmentRec).filter(
            and_(
                StockAdjustmentRec.adj_approved == 'N',
                StockAdjustmentRec.adj_posted == 'N'
            )
        ).order_by(StockAdjustmentRec.adj_date).all()
        
        return [
            {
                'adjustment_no': adj.adj_no,
                'stock_code': adj.adj_stock_code,
                'warehouse': adj.adj_warehouse,
                'location': adj.adj_location,
                'date': adj.adj_date,
                'quantity': float(adj.adj_quantity),
                'unit_cost': float(adj.adj_unit_cost),
                'total_cost': float(adj.adj_total_cost),
                'reason': adj.adj_reason,
                'reference': adj.adj_reference,
                'user': adj.adj_user
            }
            for adj in adjustments
        ]
        
    def _update_location_quantity(self, stock_code: str, warehouse: str, location: str, 
                                quantity: Decimal, direction: str):
        """Update location quantity"""
        # Find or create location record
        loc_rec = self.db.query(StockLocationRec).filter(
            and_(
                StockLocationRec.loc_stock_code == stock_code,
                StockLocationRec.loc_warehouse == warehouse,
                StockLocationRec.loc_location == location
            )
        ).first()
        
        if not loc_rec:
            # Create new location
            loc_rec = StockLocationRec(
                loc_stock_code=stock_code,
                loc_warehouse=warehouse,
                loc_location=location,
                loc_qty_on_hand=Decimal('0'),
                loc_qty_allocated=Decimal('0'),
                loc_primary='N',
                loc_pickable='Y',
                loc_active='Y',
                loc_created_date=int(datetime.now().strftime("%Y%m%d"))
            )
            self.db.add(loc_rec)
            self.db.flush()
            
        # Update quantity
        if direction == 'IN':
            loc_rec.loc_qty_on_hand += quantity
        else:
            loc_rec.loc_qty_on_hand -= quantity
            
    def _get_available_stock(self, stock_code: str, warehouse: str, location: Optional[str] = None) -> Decimal:
        """Get available stock quantity"""
        query = self.db.query(
            func.sum(StockLocationRec.loc_qty_on_hand - StockLocationRec.loc_qty_allocated)
        ).filter(
            and_(
                StockLocationRec.loc_stock_code == stock_code,
                StockLocationRec.loc_warehouse == warehouse,
                StockLocationRec.loc_active == 'Y'
            )
        )
        
        if location:
            query = query.filter(StockLocationRec.loc_location == location)
            
        result = query.scalar()
        return result or Decimal('0')
        
    def _get_location_quantity(self, stock_code: str, warehouse: str, location: str) -> Decimal:
        """Get quantity at specific location"""
        result = self.db.query(StockLocationRec.loc_qty_on_hand).filter(
            and_(
                StockLocationRec.loc_stock_code == stock_code,
                StockLocationRec.loc_warehouse == warehouse,
                StockLocationRec.loc_location == location
            )
        ).scalar()
        
        return result or Decimal('0')
        
    def _get_issue_cost(self, stock: StockMasterRec, method: str) -> Decimal:
        """Get cost for stock issue based on method"""
        if method == 'AVERAGE':
            return stock.stock_average_cost
        elif method == 'STANDARD':
            return stock.stock_standard_cost
        elif method == 'LAST':
            return stock.stock_last_cost
        elif method == 'FIFO':
            # Would implement FIFO costing
            return stock.stock_average_cost
        elif method == 'LIFO':
            # Would implement LIFO costing  
            return stock.stock_average_cost
        else:
            return stock.stock_average_cost
            
    def _auto_allocate_issue(self, stock_code: str, warehouse: str, quantity: Decimal):
        """Auto-allocate issue across best locations"""
        # Get pickable locations with stock
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
        
        remaining = quantity
        for loc in locations:
            if remaining <= 0:
                break
                
            available = loc.loc_qty_on_hand - loc.loc_qty_allocated
            take_qty = min(remaining, available)
            
            loc.loc_qty_on_hand -= take_qty
            remaining -= take_qty
            
    def _create_movement_record(self, **kwargs) -> StockMovementRec:
        """Create movement record"""
        movement = StockMovementRec(
            move_stock_code=kwargs['stock_code'],
            move_type=kwargs['move_type'],
            move_date=int(datetime.now().strftime("%Y%m%d")),
            move_time=int(datetime.now().strftime("%H%M%S")),
            move_warehouse=kwargs['warehouse'],
            move_location=kwargs['location'],
            move_qty_in=kwargs['qty_in'],
            move_qty_out=kwargs['qty_out'],
            move_unit_cost=kwargs['unit_cost'],
            move_total_cost=kwargs['unit_cost'] * (kwargs['qty_in'] or kwargs['qty_out']),
            move_balance=kwargs['balance'],
            move_reference=kwargs['reference'],
            move_user=self.current_user.username if self.current_user else 'SYSTEM'
        )
        
        self.db.add(movement)
        self.db.flush()
        return movement
        
    def _process_immediate_transfer(self, stock_code: str, from_warehouse: str, from_location: str,
                                  to_warehouse: str, to_location: str, quantity: Decimal, 
                                  reference: str, transfer_no: str) -> Tuple[bool, Optional[str]]:
        """Process immediate transfer"""
        # Get stock item for costing
        stock, _ = self.stock_handler.process(4, key_value=stock_code)
        unit_cost = stock.stock_average_cost if stock else Decimal('0')
        
        # 1. Issue from source location
        self._update_location_quantity(stock_code, from_warehouse, from_location, quantity, 'OUT')
        
        # 2. Receipt to destination location  
        self._update_location_quantity(stock_code, to_warehouse, to_location, quantity, 'IN')
        
        # 3. Create movement records
        # Transfer out
        self._create_movement_record(
            stock_code=stock_code,
            move_type='TRO',
            warehouse=from_warehouse,
            location=from_location,
            qty_in=Decimal('0'),
            qty_out=quantity,
            unit_cost=unit_cost,
            reference=f"TRANSFER-{transfer_no}",
            balance=stock.stock_on_hand if stock else Decimal('0')
        )
        
        # Transfer in
        self._create_movement_record(
            stock_code=stock_code,
            move_type='TRI',
            warehouse=to_warehouse,
            location=to_location,
            qty_in=quantity,
            qty_out=Decimal('0'),
            unit_cost=unit_cost,
            reference=f"TRANSFER-{transfer_no}",
            balance=stock.stock_on_hand if stock else Decimal('0')
        )
        
        self.db.commit()
        return True, f"Transfer {transfer_no} completed"
        
    def _create_transfer_request(self, stock_code: str, from_warehouse: str, from_location: str,
                               to_warehouse: str, to_location: str, quantity: Decimal,
                               reference: str, transfer_no: str) -> Tuple[bool, Optional[str]]:
        """Create transfer request"""
        # Get stock description
        stock, _ = self.stock_handler.process(4, key_value=stock_code)
        description = stock.stock_desc if stock else ''
        
        transfer = StockTransferRec(
            trans_no=transfer_no,
            trans_stock_code=stock_code,
            trans_description=description,
            trans_from_warehouse=from_warehouse,
            trans_from_location=from_location,
            trans_to_warehouse=to_warehouse,
            trans_to_location=to_location,
            trans_quantity=quantity,
            trans_date_created=int(datetime.now().strftime("%Y%m%d")),
            trans_date_required=int(datetime.now().strftime("%Y%m%d")),
            trans_reference=reference,
            trans_status='PENDING',
            trans_created_by=self.current_user.username if self.current_user else 'SYSTEM',
            trans_priority='NORMAL'
        )
        
        self.db.add(transfer)
        self.db.commit()
        
        return True, f"Transfer request {transfer_no} created"
        
    def _post_movement_to_gl(self, movement: StockMovementRec, stock: StockMasterRec):
        """Post movement to General Ledger"""
        system_rec, _ = self.system_handler.read_system_params()
        if not system_rec or system_rec.gl_interface != 'Y':
            return
            
        # Only post certain movement types
        if movement.move_type not in ['REC', 'ISS', 'ADJ']:
            return
            
        je_service = JournalEntryService(self.db, self.current_user)
        
        # Create batch
        batch = je_service.create_journal_batch(
            description=f"Stock Movement {movement.move_type}",
            source="STOCK"
        )
        
        # Determine accounts based on movement type
        if movement.move_type == 'REC':
            # Receipt: Dr Stock, Cr GRN/Purchases
            je_service.add_journal_line(batch.batch_no, {
                'account': system_rec.bl_stock_ac,
                'debit': float(movement.move_total_cost),
                'credit': 0,
                'reference': movement.move_reference,
                'description': f"Stock Receipt {stock.stock_code}"
            })
            
        elif movement.move_type == 'ISS':
            # Issue: Dr Cost of Sales, Cr Stock
            je_service.add_journal_line(batch.batch_no, {
                'account': system_rec.bl_cos_ac,
                'debit': float(movement.move_total_cost),
                'credit': 0,
                'reference': movement.move_reference,
                'description': f"Stock Issue {stock.stock_code}"
            })
            
            je_service.add_journal_line(batch.batch_no, {
                'account': system_rec.bl_stock_ac,
                'debit': 0,
                'credit': float(movement.move_total_cost),
                'reference': movement.move_reference,
                'description': f"Stock Issue {stock.stock_code}"
            })
            
        # Post the batch
        je_service.post_batch(batch.batch_no)
        
    def _post_adjustment_to_gl(self, adjustment: StockAdjustmentRec, stock: StockMasterRec):
        """Post adjustment to General Ledger"""
        system_rec, _ = self.system_handler.read_system_params()
        if not system_rec or system_rec.gl_interface != 'Y':
            return
            
        je_service = JournalEntryService(self.db, self.current_user)
        
        # Create batch
        batch = je_service.create_journal_batch(
            description=f"Stock Adjustment {adjustment.adj_no}",
            source="STOCK"
        )
        
        if adjustment.adj_quantity > 0:
            # Positive adjustment: Dr Stock, Cr Stock Variance
            je_service.add_journal_line(batch.batch_no, {
                'account': system_rec.bl_stock_ac,
                'debit': float(abs(adjustment.adj_total_cost)),
                'credit': 0,
                'reference': adjustment.adj_reference,
                'description': f"Stock Adj+ {stock.stock_code}"
            })
            
            je_service.add_journal_line(batch.batch_no, {
                'account': system_rec.bl_stock_var_ac,
                'debit': 0,
                'credit': float(abs(adjustment.adj_total_cost)),
                'reference': adjustment.adj_reference,
                'description': f"Stock Adj+ {stock.stock_code}"
            })
        else:
            # Negative adjustment: Dr Stock Variance, Cr Stock
            je_service.add_journal_line(batch.batch_no, {
                'account': system_rec.bl_stock_var_ac,
                'debit': float(abs(adjustment.adj_total_cost)),
                'credit': 0,
                'reference': adjustment.adj_reference,
                'description': f"Stock Adj- {stock.stock_code}"
            })
            
            je_service.add_journal_line(batch.batch_no, {
                'account': system_rec.bl_stock_ac,
                'debit': 0,
                'credit': float(abs(adjustment.adj_total_cost)),
                'reference': adjustment.adj_reference,
                'description': f"Stock Adj- {stock.stock_code}"
            })
            
        # Post the batch
        je_service.post_batch(batch.batch_no)
        
    def _get_next_transfer_number(self) -> str:
        """Generate next transfer number"""
        return f"TR{datetime.now().strftime('%Y%m%d%H%M%S')}"
        
    def _get_next_adjustment_number(self) -> str:
        """Generate next adjustment number"""
        return f"ADJ{datetime.now().strftime('%Y%m%d%H%M%S')}"
        
    def _get_next_count_number(self) -> str:
        """Generate next count number"""
        return f"CNT{datetime.now().strftime('%Y%m%d%H%M%S')}"
        
    def _get_movement_type_desc(self, move_type: str) -> str:
        """Get description for movement type"""
        types = {
            'REC': 'Receipt',
            'ISS': 'Issue',
            'ADJ': 'Adjustment', 
            'TRI': 'Transfer In',
            'TRO': 'Transfer Out',
            'SAL': 'Sales',
            'PUR': 'Purchase',
            'RET': 'Return',
            'CNT': 'Count',
            'WAS': 'Wastage'
        }
        return types.get(move_type, move_type)