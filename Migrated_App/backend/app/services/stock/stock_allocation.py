"""
Stock Allocation Service - ST070 migration
Handles stock allocation and reservation management
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from datetime import datetime, timedelta
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func, desc, asc

from app.services.file_handlers.stock_handler import StockFileHandler
from app.services.file_handlers.system_handler import SystemFileHandler
from app.models.stock import (
    StockMasterRec, StockLocationRec, StockAllocationRec,
    StockReservationRec, StockBackorderRec
)
from app.models.sales import SalesOrderRec, SalesOrderLineRec
from app.models.production import WorkOrderRec, WorkOrderLineRec
from app.core.security import log_user_action
from app.models.auth import User


class StockAllocationService:
    """
    Stock Allocation functionality
    Implements ST070 - stock allocation and reservation management
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.stock_handler = StockFileHandler(db)
        self.system_handler = SystemFileHandler(db)
        
    def allocate_sales_order(self, so_allocation_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Allocate stock for sales order
        Returns (success, error_message)
        """
        so_no = so_allocation_data.get('so_no')
        warehouse = so_allocation_data.get('warehouse', 'MAIN')
        allocation_method = so_allocation_data.get('method', 'FIFO')  # FIFO, LIFO, FEFO, OPTIMAL
        partial_allocation = so_allocation_data.get('partial_allocation', True)
        priority = so_allocation_data.get('priority', 'NORMAL')
        
        # Get sales order
        so = self.db.query(SalesOrderRec).filter(
            SalesOrderRec.so_no == so_no
        ).first()
        
        if not so:
            return False, "Sales order not found"
            
        if so.so_status not in ['OPEN', 'PARTIAL']:
            return False, f"Cannot allocate SO with status: {so.so_status}"
            
        try:
            # Get unallocated SO lines
            so_lines = self.db.query(SalesOrderLineRec).filter(
                and_(
                    SalesOrderLineRec.sol_so_no == so_no,
                    SalesOrderLineRec.sol_outstanding > 0
                )
            ).order_by(SalesOrderLineRec.sol_line_no).all()
            
            allocation_results = []
            fully_allocated = True
            
            # Process each line
            for so_line in so_lines:
                line_result = self._allocate_so_line(
                    so_line, warehouse, allocation_method, partial_allocation
                )
                
                allocation_results.append({
                    'line_no': so_line.sol_line_no,
                    'stock_code': so_line.sol_stock_code,
                    'required': float(so_line.sol_outstanding),
                    'allocated': line_result['allocated'],
                    'shortfall': line_result['shortfall'],
                    'allocations': line_result['allocations']
                })
                
                if line_result['shortfall'] > 0:
                    fully_allocated = False
                    
                    # Create backorder if required
                    if not partial_allocation:
                        self._create_backorder(so_line, line_result['shortfall'])
                        
            # Update SO status
            if fully_allocated:
                so.so_status = 'ALLOCATED'
                so.so_allocated_date = int(datetime.now().strftime("%Y%m%d"))
            elif any(r['allocated'] > 0 for r in allocation_results):
                so.so_status = 'PARTIAL'
                
            self.db.commit()
            
            # Log allocation
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="ALLOCATE_SALES_ORDER",
                    table="sales_order_rec",
                    key=so_no,
                    new_values={
                        'warehouse': warehouse,
                        'method': allocation_method,
                        'fully_allocated': fully_allocated
                    },
                    module="STOCK"
                )
                
            return True, {
                'so_no': so_no,
                'fully_allocated': fully_allocated,
                'results': allocation_results
            }
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def _allocate_so_line(self, so_line: SalesOrderLineRec, warehouse: str, 
                         method: str, partial: bool) -> Dict:
        """Allocate stock for individual SO line"""
        stock_code = so_line.sol_stock_code
        qty_required = so_line.sol_outstanding
        
        # Get available locations
        locations = self._get_available_locations(stock_code, warehouse, method)
        
        allocations = []
        total_allocated = Decimal('0')
        
        for location in locations:
            if total_allocated >= qty_required:
                break
                
            available = location['available']
            if available <= 0:
                continue
                
            # Calculate allocation quantity
            remaining = qty_required - total_allocated
            alloc_qty = min(remaining, available)
            
            # Create allocation record
            allocation = self._create_allocation(
                stock_code=stock_code,
                warehouse=warehouse,
                location=location['location'],
                bin_location=location['bin'],
                quantity=alloc_qty,
                reference_type='SO',
                reference_no=so_line.sol_so_no,
                reference_line=so_line.sol_line_no,
                priority=so_line.sol_priority if hasattr(so_line, 'sol_priority') else 'NORMAL'
            )
            
            allocations.append({
                'location': location['location'],
                'bin': location['bin'],
                'quantity': float(alloc_qty),
                'allocation_id': allocation.alloc_id
            })
            
            # Update location allocated quantity
            self._update_location_allocation(stock_code, warehouse, location['location'], alloc_qty, 'ADD')
            
            total_allocated += alloc_qty
            
        # Update SO line
        so_line.sol_allocated = float(total_allocated)
        shortfall = float(qty_required - total_allocated)
        
        return {
            'allocated': float(total_allocated),
            'shortfall': shortfall,
            'allocations': allocations
        }
        
    def allocate_work_order(self, wo_allocation_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Allocate materials for work order
        Returns (success, error_message)
        """
        wo_no = wo_allocation_data.get('wo_no')
        warehouse = wo_allocation_data.get('warehouse', 'MAIN')
        allocation_method = wo_allocation_data.get('method', 'FIFO')
        
        # Get work order
        wo = self.db.query(WorkOrderRec).filter(
            WorkOrderRec.wo_no == wo_no
        ).first()
        
        if not wo:
            return False, "Work order not found"
            
        try:
            # Get WO material requirements
            wo_lines = self.db.query(WorkOrderLineRec).filter(
                and_(
                    WorkOrderLineRec.wol_wo_no == wo_no,
                    WorkOrderLineRec.wol_outstanding > 0
                )
            ).all()
            
            allocation_results = []
            fully_allocated = True
            
            # Process each material
            for wo_line in wo_lines:
                line_result = self._allocate_wo_line(
                    wo_line, warehouse, allocation_method
                )
                
                allocation_results.append({
                    'line_no': wo_line.wol_line_no,
                    'stock_code': wo_line.wol_stock_code,
                    'required': float(wo_line.wol_outstanding),
                    'allocated': line_result['allocated'],
                    'shortfall': line_result['shortfall'],
                    'allocations': line_result['allocations']
                })
                
                if line_result['shortfall'] > 0:
                    fully_allocated = False
                    
            # Update WO status
            if fully_allocated:
                wo.wo_status = 'ALLOCATED'
                wo.wo_allocated_date = int(datetime.now().strftime("%Y%m%d"))
            elif any(r['allocated'] > 0 for r in allocation_results):
                wo.wo_status = 'PARTIAL'
                
            self.db.commit()
            
            return True, {
                'wo_no': wo_no,
                'fully_allocated': fully_allocated,
                'results': allocation_results
            }
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def _allocate_wo_line(self, wo_line: WorkOrderLineRec, warehouse: str, method: str) -> Dict:
        """Allocate stock for work order line"""
        stock_code = wo_line.wol_stock_code
        qty_required = wo_line.wol_outstanding
        
        # Get available locations
        locations = self._get_available_locations(stock_code, warehouse, method)
        
        allocations = []
        total_allocated = Decimal('0')
        
        for location in locations:
            if total_allocated >= qty_required:
                break
                
            available = location['available']
            if available <= 0:
                continue
                
            remaining = qty_required - total_allocated
            alloc_qty = min(remaining, available)
            
            # Create allocation record
            allocation = self._create_allocation(
                stock_code=stock_code,
                warehouse=warehouse,
                location=location['location'],
                bin_location=location['bin'],
                quantity=alloc_qty,
                reference_type='WO',
                reference_no=wo_line.wol_wo_no,
                reference_line=wo_line.wol_line_no,
                priority='HIGH'  # Production usually high priority
            )
            
            allocations.append({
                'location': location['location'],
                'bin': location['bin'],
                'quantity': float(alloc_qty),
                'allocation_id': allocation.alloc_id
            })
            
            # Update location allocated quantity
            self._update_location_allocation(stock_code, warehouse, location['location'], alloc_qty, 'ADD')
            
            total_allocated += alloc_qty
            
        # Update WO line
        wo_line.wol_allocated = float(total_allocated)
        
        return {
            'allocated': float(total_allocated),
            'shortfall': float(qty_required - total_allocated),
            'allocations': allocations
        }
        
    def deallocate_order(self, deallocation_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Deallocate stock from order
        Returns (success, error_message)
        """
        reference_type = deallocation_data.get('reference_type')  # SO, WO, etc.
        reference_no = deallocation_data.get('reference_no')
        reference_line = deallocation_data.get('reference_line')  # Optional - specific line
        
        try:
            # Build query for allocations
            query = self.db.query(StockAllocationRec).filter(
                and_(
                    StockAllocationRec.alloc_reference_type == reference_type,
                    StockAllocationRec.alloc_reference_no == reference_no,
                    StockAllocationRec.alloc_status == 'ALLOCATED'
                )
            )
            
            if reference_line:
                query = query.filter(StockAllocationRec.alloc_reference_line == reference_line)
                
            allocations = query.all()
            
            if not allocations:
                return False, "No allocations found to deallocate"
                
            # Process each allocation
            for allocation in allocations:
                # Update location allocated quantity
                self._update_location_allocation(
                    allocation.alloc_stock_code,
                    allocation.alloc_warehouse, 
                    allocation.alloc_location,
                    allocation.alloc_quantity,
                    'SUBTRACT'
                )
                
                # Mark allocation as deallocated
                allocation.alloc_status = 'DEALLOCATED'
                allocation.alloc_deallocated_date = int(datetime.now().strftime("%Y%m%d"))
                allocation.alloc_deallocated_by = self.current_user.username if self.current_user else 'SYSTEM'
                
            # Update order status
            if reference_type == 'SO':
                so = self.db.query(SalesOrderRec).filter(SalesOrderRec.so_no == reference_no).first()
                if so:
                    so.so_status = 'OPEN'  # Reset to open
                    
                    # Update line allocations
                    if reference_line:
                        so_line = self.db.query(SalesOrderLineRec).filter(
                            and_(
                                SalesOrderLineRec.sol_so_no == reference_no,
                                SalesOrderLineRec.sol_line_no == reference_line
                            )
                        ).first()
                        if so_line:
                            so_line.sol_allocated = 0
                    else:
                        # Deallocate all lines
                        so_lines = self.db.query(SalesOrderLineRec).filter(
                            SalesOrderLineRec.sol_so_no == reference_no
                        ).all()
                        for so_line in so_lines:
                            so_line.sol_allocated = 0
                            
            self.db.commit()
            
            # Log deallocation
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="DEALLOCATE_ORDER",
                    table="stock_allocation_rec",
                    key=f"{reference_type}-{reference_no}",
                    new_values={
                        'allocations_removed': len(allocations),
                        'reference_line': reference_line
                    },
                    module="STOCK"
                )
                
            return True, f"Deallocated {len(allocations)} allocations"
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def create_reservation(self, reservation_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Create stock reservation
        Returns (success, error_message)
        """
        stock_code = reservation_data.get('stock_code')
        warehouse = reservation_data.get('warehouse', 'MAIN')
        quantity = Decimal(str(reservation_data.get('quantity', 0)))
        reserved_for = reservation_data.get('reserved_for', '')
        reference = reservation_data.get('reference', '')
        expiry_date = reservation_data.get('expiry_date', 0)
        priority = reservation_data.get('priority', 'NORMAL')
        
        if quantity <= 0:
            return False, "Reservation quantity must be greater than zero"
            
        # Check availability
        available = self._get_total_available_stock(stock_code, warehouse)
        if quantity > available:
            return False, f"Insufficient stock for reservation. Available: {available}"
            
        try:
            # Get next reservation number
            reservation_no = self._get_next_reservation_number()
            
            # Create reservation record
            reservation = StockReservationRec(
                res_no=reservation_no,
                res_stock_code=stock_code,
                res_warehouse=warehouse,
                res_quantity=quantity,
                res_reserved_for=reserved_for,
                res_reference=reference,
                res_date=int(datetime.now().strftime("%Y%m%d")),
                res_expiry_date=expiry_date,
                res_priority=priority,
                res_status='ACTIVE',
                res_created_by=self.current_user.username if self.current_user else 'SYSTEM'
            )
            
            self.db.add(reservation)
            self.db.flush()
            
            # Update stock master reserved quantity
            stock, _ = self.stock_handler.process(4, key_value=stock_code)
            if stock:
                stock.stock_reserved += quantity
                self.stock_handler.process(7, record=stock)
                
            self.db.commit()
            
            # Log reservation
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="CREATE_RESERVATION",
                    table="stock_reservation_rec",
                    key=reservation_no,
                    new_values={
                        'stock_code': stock_code,
                        'quantity': float(quantity),
                        'reserved_for': reserved_for
                    },
                    module="STOCK"
                )
                
            return True, f"Reservation {reservation_no} created successfully"
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def release_reservation(self, reservation_no: str) -> Tuple[bool, Optional[str]]:
        """
        Release stock reservation
        Returns (success, error_message)
        """
        reservation = self.db.query(StockReservationRec).filter(
            StockReservationRec.res_no == reservation_no
        ).first()
        
        if not reservation:
            return False, "Reservation not found"
            
        if reservation.res_status != 'ACTIVE':
            return False, f"Cannot release reservation with status: {reservation.res_status}"
            
        try:
            # Update reservation status
            reservation.res_status = 'RELEASED'
            reservation.res_released_date = int(datetime.now().strftime("%Y%m%d"))
            reservation.res_released_by = self.current_user.username if self.current_user else 'SYSTEM'
            
            # Update stock master reserved quantity
            stock, _ = self.stock_handler.process(4, key_value=reservation.res_stock_code)
            if stock:
                stock.stock_reserved -= reservation.res_quantity
                self.stock_handler.process(7, record=stock)
                
            self.db.commit()
            
            # Log release
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="RELEASE_RESERVATION",
                    table="stock_reservation_rec",
                    key=reservation_no,
                    new_values={'quantity': float(reservation.res_quantity)},
                    module="STOCK"
                )
                
            return True, f"Reservation {reservation_no} released successfully"
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def process_backorders(self, backorder_options: Optional[Dict] = None) -> Dict:
        """
        Process backorders and attempt allocation
        Returns processing results
        """
        options = backorder_options or {}
        warehouse = options.get('warehouse')
        priority = options.get('priority')
        customer = options.get('customer')
        
        # Build query for active backorders
        query = self.db.query(StockBackorderRec).filter(
            StockBackorderRec.bo_status == 'ACTIVE'
        )
        
        if warehouse:
            query = query.filter(StockBackorderRec.bo_warehouse == warehouse)
        if priority:
            query = query.filter(StockBackorderRec.bo_priority == priority)
        if customer:
            query = query.filter(StockBackorderRec.bo_customer == customer)
            
        # Order by priority and date
        backorders = query.order_by(
            StockBackorderRec.bo_priority.desc(),
            StockBackorderRec.bo_date
        ).all()
        
        processed = []
        fulfilled = 0
        
        for backorder in backorders:
            # Check if stock now available
            available = self._get_total_available_stock(
                backorder.bo_stock_code, 
                backorder.bo_warehouse
            )
            
            if available >= backorder.bo_quantity:
                # Try to allocate
                if backorder.bo_reference_type == 'SO':
                    # Reallocate to sales order
                    so_line = self.db.query(SalesOrderLineRec).filter(
                        and_(
                            SalesOrderLineRec.sol_so_no == backorder.bo_reference_no,
                            SalesOrderLineRec.sol_line_no == backorder.bo_reference_line
                        )
                    ).first()
                    
                    if so_line:
                        result = self._allocate_so_line(
                            so_line, backorder.bo_warehouse, 'FIFO', True
                        )
                        
                        if result['allocated'] >= float(backorder.bo_quantity):
                            # Mark backorder as fulfilled
                            backorder.bo_status = 'FULFILLED'
                            backorder.bo_fulfilled_date = int(datetime.now().strftime("%Y%m%d"))
                            fulfilled += 1
                            
            processed.append({
                'backorder_no': backorder.bo_no,
                'stock_code': backorder.bo_stock_code,
                'quantity': float(backorder.bo_quantity),
                'available': float(available),
                'fulfilled': backorder.bo_status == 'FULFILLED'
            })
            
        self.db.commit()
        
        return {
            'total_processed': len(processed),
            'fulfilled': fulfilled,
            'backorders': processed
        }
        
    def get_allocation_summary(self, filters: Optional[Dict] = None) -> Dict:
        """Get allocation summary with filters"""
        filters = filters or {}
        
        # Build base query
        query = self.db.query(StockAllocationRec).filter(
            StockAllocationRec.alloc_status == 'ALLOCATED'
        )
        
        # Apply filters
        if filters.get('warehouse'):
            query = query.filter(StockAllocationRec.alloc_warehouse == filters['warehouse'])
        if filters.get('stock_code'):
            query = query.filter(StockAllocationRec.alloc_stock_code == filters['stock_code'])
        if filters.get('reference_type'):
            query = query.filter(StockAllocationRec.alloc_reference_type == filters['reference_type'])
        if filters.get('priority'):
            query = query.filter(StockAllocationRec.alloc_priority == filters['priority'])
            
        allocations = query.all()
        
        # Group by various dimensions
        by_warehouse = {}
        by_stock_code = {}
        by_reference_type = {}
        by_priority = {}
        
        total_allocations = 0
        total_quantity = Decimal('0')
        
        for alloc in allocations:
            total_allocations += 1
            total_quantity += alloc.alloc_quantity
            
            # Group by warehouse
            wh = alloc.alloc_warehouse
            if wh not in by_warehouse:
                by_warehouse[wh] = {'count': 0, 'quantity': Decimal('0')}
            by_warehouse[wh]['count'] += 1
            by_warehouse[wh]['quantity'] += alloc.alloc_quantity
            
            # Group by stock code
            sc = alloc.alloc_stock_code
            if sc not in by_stock_code:
                by_stock_code[sc] = {'count': 0, 'quantity': Decimal('0')}
            by_stock_code[sc]['count'] += 1
            by_stock_code[sc]['quantity'] += alloc.alloc_quantity
            
            # Group by reference type
            rt = alloc.alloc_reference_type
            if rt not in by_reference_type:
                by_reference_type[rt] = {'count': 0, 'quantity': Decimal('0')}
            by_reference_type[rt]['count'] += 1
            by_reference_type[rt]['quantity'] += alloc.alloc_quantity
            
            # Group by priority
            pri = alloc.alloc_priority
            if pri not in by_priority:
                by_priority[pri] = {'count': 0, 'quantity': Decimal('0')}
            by_priority[pri]['count'] += 1
            by_priority[pri]['quantity'] += alloc.alloc_quantity
            
        # Convert to float for JSON serialization
        for group in [by_warehouse, by_stock_code, by_reference_type, by_priority]:
            for key, value in group.items():
                value['quantity'] = float(value['quantity'])
                
        return {
            'summary': {
                'total_allocations': total_allocations,
                'total_quantity': float(total_quantity)
            },
            'by_warehouse': by_warehouse,
            'by_stock_code': dict(list(by_stock_code.items())[:20]),  # Top 20
            'by_reference_type': by_reference_type,
            'by_priority': by_priority
        }
        
    def get_availability_analysis(self, stock_codes: List[str], warehouse: str = 'MAIN') -> List[Dict]:
        """Get detailed availability analysis for stock codes"""
        results = []
        
        for stock_code in stock_codes:
            # Get stock master
            stock, _ = self.stock_handler.process(4, key_value=stock_code)
            if not stock:
                results.append({
                    'stock_code': stock_code,
                    'error': 'Stock item not found'
                })
                continue
                
            # Get location details
            locations = self.db.query(StockLocationRec).filter(
                and_(
                    StockLocationRec.loc_stock_code == stock_code,
                    StockLocationRec.loc_warehouse == warehouse,
                    StockLocationRec.loc_active == 'Y'
                )
            ).all()
            
            # Calculate totals
            total_on_hand = sum(loc.loc_qty_on_hand for loc in locations)
            total_allocated = sum(loc.loc_qty_allocated for loc in locations)
            total_available = total_on_hand - total_allocated
            
            # Get pending orders
            pending_sales = self._get_pending_sales_demand(stock_code)
            pending_production = self._get_pending_production_demand(stock_code)
            
            results.append({
                'stock_code': stock_code,
                'description': stock.stock_desc,
                'warehouse': warehouse,
                'quantities': {
                    'on_hand': float(total_on_hand),
                    'allocated': float(total_allocated),
                    'available': float(total_available),
                    'reserved': float(stock.stock_reserved if hasattr(stock, 'stock_reserved') else 0),
                    'on_order': float(stock.stock_on_order)
                },
                'demand': {
                    'sales_orders': pending_sales,
                    'production_orders': pending_production,
                    'total_demand': pending_sales + pending_production
                },
                'locations': [
                    {
                        'location': loc.loc_location,
                        'bin': loc.loc_bin,
                        'on_hand': float(loc.loc_qty_on_hand),
                        'allocated': float(loc.loc_qty_allocated),
                        'available': float(loc.loc_qty_on_hand - loc.loc_qty_allocated),
                        'pickable': loc.loc_pickable == 'Y'
                    }
                    for loc in locations
                ],
                'reorder_info': {
                    'reorder_level': float(stock.stock_reorder_level),
                    'reorder_qty': float(stock.stock_reorder_qty),
                    'min_qty': float(stock.stock_min_qty),
                    'max_qty': float(stock.stock_max_qty),
                    'needs_reorder': total_available <= float(stock.stock_reorder_level)
                }
            })
            
        return results
        
    def _get_available_locations(self, stock_code: str, warehouse: str, method: str) -> List[Dict]:
        """Get available locations sorted by allocation method"""
        locations = self.db.query(StockLocationRec).filter(
            and_(
                StockLocationRec.loc_stock_code == stock_code,
                StockLocationRec.loc_warehouse == warehouse,
                StockLocationRec.loc_active == 'Y',
                StockLocationRec.loc_pickable == 'Y',
                StockLocationRec.loc_qty_on_hand > StockLocationRec.loc_qty_allocated
            )
        )
        
        # Apply sorting based on method
        if method == 'FIFO':
            # First locations received from (would need receipt date tracking)
            locations = locations.order_by(StockLocationRec.loc_created_date)
        elif method == 'LIFO':
            # Last locations received to
            locations = locations.order_by(desc(StockLocationRec.loc_created_date))
        elif method == 'FEFO':
            # First expiry first (would need expiry date tracking)
            locations = locations.order_by(StockLocationRec.loc_location)
        else:  # OPTIMAL
            # Primary locations first, then by pick sequence
            locations = locations.order_by(
                desc(StockLocationRec.loc_primary),
                StockLocationRec.loc_location
            )
            
        return [
            {
                'location': loc.loc_location,
                'bin': loc.loc_bin,
                'available': loc.loc_qty_on_hand - loc.loc_qty_allocated,
                'primary': loc.loc_primary == 'Y'
            }
            for loc in locations.all()
            if (loc.loc_qty_on_hand - loc.loc_qty_allocated) > 0
        ]
        
    def _create_allocation(self, **kwargs) -> StockAllocationRec:
        """Create allocation record"""
        allocation = StockAllocationRec(
            alloc_stock_code=kwargs['stock_code'],
            alloc_warehouse=kwargs['warehouse'],
            alloc_location=kwargs['location'],
            alloc_bin_location=kwargs['bin_location'],
            alloc_quantity=kwargs['quantity'],
            alloc_reference_type=kwargs['reference_type'],
            alloc_reference_no=kwargs['reference_no'],
            alloc_reference_line=kwargs['reference_line'],
            alloc_date=int(datetime.now().strftime("%Y%m%d")),
            alloc_time=int(datetime.now().strftime("%H%M%S")),
            alloc_priority=kwargs['priority'],
            alloc_status='ALLOCATED',
            alloc_allocated_by=self.current_user.username if self.current_user else 'SYSTEM'
        )
        
        self.db.add(allocation)
        self.db.flush()
        return allocation
        
    def _update_location_allocation(self, stock_code: str, warehouse: str, location: str, 
                                  quantity: Decimal, operation: str):
        """Update location allocated quantity"""
        loc_rec = self.db.query(StockLocationRec).filter(
            and_(
                StockLocationRec.loc_stock_code == stock_code,
                StockLocationRec.loc_warehouse == warehouse,
                StockLocationRec.loc_location == location
            )
        ).first()
        
        if loc_rec:
            if operation == 'ADD':
                loc_rec.loc_qty_allocated += quantity
            else:  # SUBTRACT
                loc_rec.loc_qty_allocated -= quantity
                
            # Ensure allocated doesn't go negative
            if loc_rec.loc_qty_allocated < 0:
                loc_rec.loc_qty_allocated = Decimal('0')
                
    def _create_backorder(self, so_line: SalesOrderLineRec, shortfall: float):
        """Create backorder record"""
        backorder_no = self._get_next_backorder_number()
        
        backorder = StockBackorderRec(
            bo_no=backorder_no,
            bo_stock_code=so_line.sol_stock_code,
            bo_warehouse='MAIN',  # Default warehouse
            bo_quantity=Decimal(str(shortfall)),
            bo_reference_type='SO',
            bo_reference_no=so_line.sol_so_no,
            bo_reference_line=so_line.sol_line_no,
            bo_customer=so_line.sol_customer if hasattr(so_line, 'sol_customer') else '',
            bo_date=int(datetime.now().strftime("%Y%m%d")),
            bo_priority=so_line.sol_priority if hasattr(so_line, 'sol_priority') else 'NORMAL',
            bo_status='ACTIVE',
            bo_created_by=self.current_user.username if self.current_user else 'SYSTEM'
        )
        
        self.db.add(backorder)
        
    def _get_total_available_stock(self, stock_code: str, warehouse: str) -> Decimal:
        """Get total available stock across all locations"""
        result = self.db.query(
            func.sum(StockLocationRec.loc_qty_on_hand - StockLocationRec.loc_qty_allocated)
        ).filter(
            and_(
                StockLocationRec.loc_stock_code == stock_code,
                StockLocationRec.loc_warehouse == warehouse,
                StockLocationRec.loc_active == 'Y'
            )
        ).scalar()
        
        return result or Decimal('0')
        
    def _get_pending_sales_demand(self, stock_code: str) -> float:
        """Get pending sales order demand"""
        result = self.db.query(
            func.sum(SalesOrderLineRec.sol_outstanding)
        ).filter(
            and_(
                SalesOrderLineRec.sol_stock_code == stock_code,
                SalesOrderLineRec.sol_outstanding > 0
            )
        ).scalar()
        
        return float(result or 0)
        
    def _get_pending_production_demand(self, stock_code: str) -> float:
        """Get pending production order demand"""
        # This would query work order requirements
        return 0.0  # Placeholder
        
    def _get_next_reservation_number(self) -> str:
        """Generate next reservation number"""
        return f"RES{datetime.now().strftime('%Y%m%d%H%M%S')}"
        
    def _get_next_backorder_number(self) -> str:
        """Generate next backorder number"""
        return f"BO{datetime.now().strftime('%Y%m%d%H%M%S')}"