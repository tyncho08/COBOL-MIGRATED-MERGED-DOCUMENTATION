"""
Pick List Generation Service - ST080 migration
Handles optimized pick list generation for warehouse operations
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from datetime import datetime
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func, desc, asc

from app.services.file_handlers.stock_handler import StockFileHandler
from app.services.file_handlers.system_handler import SystemFileHandler
from app.models.stock import (
    StockMasterRec, StockLocationRec, StockAllocationRec,
    PickListRec, PickListLineRec, PickWaveRec
)
from app.models.sales import SalesOrderRec, SalesOrderLineRec
from app.models.warehouse import WarehouseZoneRec, PickRouteRec
from app.core.security import log_user_action
from app.models.auth import User


class PickListGenerationService:
    """
    Pick List Generation functionality
    Implements ST080 - optimized pick list generation
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.stock_handler = StockFileHandler(db)
        self.system_handler = SystemFileHandler(db)
        
    def generate_pick_list(self, pick_request: Dict) -> Tuple[bool, Optional[str]]:
        """
        Generate optimized pick list
        Returns (success, error_message or pick_list_data)
        """
        warehouse = pick_request.get('warehouse', 'MAIN')
        pick_method = pick_request.get('method', 'ORDER_BY_ORDER')  # ORDER_BY_ORDER, BATCH, WAVE, ZONE
        optimization = pick_request.get('optimization', 'DISTANCE')  # DISTANCE, TIME, SEQUENCE
        max_lines = pick_request.get('max_lines', 50)
        max_orders = pick_request.get('max_orders', 10)
        picker = pick_request.get('picker', '')
        priority = pick_request.get('priority', 'NORMAL')
        
        # Get source data based on method
        if pick_method == 'ORDER_BY_ORDER':
            return self._generate_single_order_picks(warehouse, pick_request)
        elif pick_method == 'BATCH':
            return self._generate_batch_picks(warehouse, max_lines, max_orders, optimization)
        elif pick_method == 'WAVE':
            return self._generate_wave_picks(warehouse, pick_request)
        elif pick_method == 'ZONE':
            return self._generate_zone_picks(warehouse, pick_request)
        else:
            return False, f"Unknown pick method: {pick_method}"
            
    def _generate_single_order_picks(self, warehouse: str, pick_request: Dict) -> Tuple[bool, Optional[str]]:
        """Generate pick list for single order"""
        so_no = pick_request.get('so_no')
        if not so_no:
            return False, "Sales order number required for single order picking"
            
        # Get sales order
        so = self.db.query(SalesOrderRec).filter(SalesOrderRec.so_no == so_no).first()
        if not so:
            return False, "Sales order not found"
            
        if so.so_status != 'ALLOCATED':
            return False, f"Sales order status must be ALLOCATED, currently: {so.so_status}"
            
        try:
            # Get allocated lines
            allocations = self.db.query(StockAllocationRec).filter(
                and_(
                    StockAllocationRec.alloc_reference_type == 'SO',
                    StockAllocationRec.alloc_reference_no == so_no,
                    StockAllocationRec.alloc_warehouse == warehouse,
                    StockAllocationRec.alloc_status == 'ALLOCATED'
                )
            ).all()
            
            if not allocations:
                return False, "No allocations found for this sales order"
                
            # Create pick list header
            pick_list_no = self._get_next_pick_list_number()
            
            pick_list = PickListRec(
                pick_no=pick_list_no,
                pick_warehouse=warehouse,
                pick_type='SINGLE_ORDER',
                pick_method='ORDER_BY_ORDER',
                pick_status='GENERATED',
                pick_priority='NORMAL',
                pick_created_date=int(datetime.now().strftime("%Y%m%d")),
                pick_created_time=int(datetime.now().strftime("%H%M%S")),
                pick_created_by=self.current_user.username if self.current_user else 'SYSTEM',
                pick_picker=pick_request.get('picker', ''),
                pick_total_lines=len(allocations),
                pick_total_qty=sum(alloc.alloc_quantity for alloc in allocations),
                pick_estimated_time=self._estimate_pick_time(allocations)
            )
            
            self.db.add(pick_list)
            self.db.flush()
            
            # Create optimized pick lines
            optimized_allocations = self._optimize_pick_sequence(allocations, 'DISTANCE')
            
            for seq, allocation in enumerate(optimized_allocations, 1):
                # Get stock details
                stock, _ = self.stock_handler.process(4, key_value=allocation.alloc_stock_code)
                
                pick_line = PickListLineRec(
                    pick_line_pick_no=pick_list_no,
                    pick_line_seq=seq,
                    pick_line_stock_code=allocation.alloc_stock_code,
                    pick_line_description=stock.stock_desc if stock else '',
                    pick_line_location=allocation.alloc_location,
                    pick_line_bin=allocation.alloc_bin_location,
                    pick_line_qty_required=allocation.alloc_quantity,
                    pick_line_qty_picked=Decimal('0'),
                    pick_line_unit=stock.stock_unit if stock else 'EA',
                    pick_line_reference_type=allocation.alloc_reference_type,
                    pick_line_reference_no=allocation.alloc_reference_no,
                    pick_line_reference_line=allocation.alloc_reference_line,
                    pick_line_allocation_id=allocation.alloc_id,
                    pick_line_status='PENDING',
                    pick_line_zone=self._get_location_zone(allocation.alloc_location),
                    pick_line_weight=stock.stock_weight * allocation.alloc_quantity if stock else Decimal('0')
                )
                
                self.db.add(pick_line)
                
            self.db.commit()
            
            # Log generation
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="GENERATE_PICK_LIST",
                    table="pick_list_rec",
                    key=pick_list_no,
                    new_values={
                        'so_no': so_no,
                        'warehouse': warehouse,
                        'total_lines': len(allocations)
                    },
                    module="STOCK"
                )
                
            return True, {
                'pick_list_no': pick_list_no,
                'total_lines': len(allocations),
                'estimated_time': pick_list.pick_estimated_time,
                'pick_sequence': [
                    {
                        'seq': i+1,
                        'stock_code': alloc.alloc_stock_code,
                        'location': alloc.alloc_location,
                        'quantity': float(alloc.alloc_quantity)
                    }
                    for i, alloc in enumerate(optimized_allocations)
                ]
            }
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def _generate_batch_picks(self, warehouse: str, max_lines: int, max_orders: int, optimization: str) -> Tuple[bool, Optional[str]]:
        """Generate batch pick list combining multiple orders"""
        try:
            # Get allocated orders ready for picking
            allocated_orders = self.db.query(SalesOrderRec).filter(
                and_(
                    SalesOrderRec.so_status == 'ALLOCATED',
                    SalesOrderRec.so_warehouse == warehouse
                )
            ).order_by(SalesOrderRec.so_priority.desc(), SalesOrderRec.so_due_date).limit(max_orders).all()
            
            if not allocated_orders:
                return False, "No allocated orders available for batch picking"
                
            # Get all allocations for these orders
            so_numbers = [so.so_no for so in allocated_orders]
            allocations = self.db.query(StockAllocationRec).filter(
                and_(
                    StockAllocationRec.alloc_reference_type == 'SO',
                    StockAllocationRec.alloc_reference_no.in_(so_numbers),
                    StockAllocationRec.alloc_warehouse == warehouse,
                    StockAllocationRec.alloc_status == 'ALLOCATED'
                )
            ).all()
            
            # Group by location and consolidate
            consolidated_picks = self._consolidate_batch_picks(allocations)
            
            # Apply line limit
            if len(consolidated_picks) > max_lines:
                consolidated_picks = consolidated_picks[:max_lines]
                
            # Create batch pick list
            pick_list_no = self._get_next_pick_list_number()
            
            pick_list = PickListRec(
                pick_no=pick_list_no,
                pick_warehouse=warehouse,
                pick_type='BATCH',
                pick_method='BATCH',
                pick_status='GENERATED',
                pick_priority='NORMAL',
                pick_created_date=int(datetime.now().strftime("%Y%m%d")),
                pick_created_time=int(datetime.now().strftime("%H%M%S")),
                pick_created_by=self.current_user.username if self.current_user else 'SYSTEM',
                pick_total_lines=len(consolidated_picks),
                pick_total_qty=sum(item['quantity'] for item in consolidated_picks),
                pick_estimated_time=self._estimate_batch_pick_time(consolidated_picks)
            )
            
            self.db.add(pick_list)
            self.db.flush()
            
            # Create optimized pick lines
            optimized_picks = self._optimize_batch_sequence(consolidated_picks, optimization)
            
            for seq, pick_item in enumerate(optimized_picks, 1):
                stock, _ = self.stock_handler.process(4, key_value=pick_item['stock_code'])
                
                pick_line = PickListLineRec(
                    pick_line_pick_no=pick_list_no,
                    pick_line_seq=seq,
                    pick_line_stock_code=pick_item['stock_code'],
                    pick_line_description=stock.stock_desc if stock else '',
                    pick_line_location=pick_item['location'],
                    pick_line_bin=pick_item['bin'],
                    pick_line_qty_required=pick_item['quantity'],
                    pick_line_qty_picked=Decimal('0'),
                    pick_line_unit=stock.stock_unit if stock else 'EA',
                    pick_line_reference_type='BATCH',
                    pick_line_reference_no=','.join(pick_item['orders']),
                    pick_line_status='PENDING',
                    pick_line_zone=self._get_location_zone(pick_item['location']),
                    pick_line_orders=pick_item['order_details']
                )
                
                self.db.add(pick_line)
                
            self.db.commit()
            
            return True, {
                'pick_list_no': pick_list_no,
                'orders_included': len(allocated_orders),
                'total_lines': len(consolidated_picks),
                'estimated_time': pick_list.pick_estimated_time
            }
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def _generate_wave_picks(self, warehouse: str, pick_request: Dict) -> Tuple[bool, Optional[str]]:
        """Generate wave-based pick lists"""
        wave_criteria = pick_request.get('wave_criteria', {})
        
        try:
            # Create wave
            wave_no = self._get_next_wave_number()
            
            wave = PickWaveRec(
                wave_no=wave_no,
                wave_warehouse=warehouse,
                wave_status='PLANNING',
                wave_created_date=int(datetime.now().strftime("%Y%m%d")),
                wave_created_time=int(datetime.now().strftime("%H%M%S")),
                wave_created_by=self.current_user.username if self.current_user else 'SYSTEM',
                wave_criteria=str(wave_criteria)
            )
            
            self.db.add(wave)
            self.db.flush()
            
            # Select orders for wave based on criteria
            orders = self._select_orders_for_wave(warehouse, wave_criteria)
            
            # Generate pick lists for wave
            pick_lists = []
            for order_group in self._group_orders_for_wave(orders):
                pick_list_result = self._generate_wave_pick_list(wave_no, order_group)
                if pick_list_result[0]:
                    pick_lists.append(pick_list_result[1])
                    
            # Update wave status
            wave.wave_status = 'RELEASED'
            wave.wave_pick_lists = len(pick_lists)
            wave.wave_total_orders = sum(len(group) for group in self._group_orders_for_wave(orders))
            
            self.db.commit()
            
            return True, {
                'wave_no': wave_no,
                'pick_lists': pick_lists,
                'total_orders': wave.wave_total_orders
            }
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def _generate_zone_picks(self, warehouse: str, pick_request: Dict) -> Tuple[bool, Optional[str]]:
        """Generate zone-based pick lists"""
        zone = pick_request.get('zone')
        if not zone:
            return False, "Zone required for zone picking"
            
        try:
            # Get allocations in specified zone
            allocations = self.db.query(StockAllocationRec).join(
                StockLocationRec,
                and_(
                    StockLocationRec.loc_stock_code == StockAllocationRec.alloc_stock_code,
                    StockLocationRec.loc_warehouse == StockAllocationRec.alloc_warehouse,
                    StockLocationRec.loc_location == StockAllocationRec.alloc_location
                )
            ).filter(
                and_(
                    StockAllocationRec.alloc_warehouse == warehouse,
                    StockAllocationRec.alloc_status == 'ALLOCATED',
                    StockLocationRec.loc_zone == zone
                )
            ).all()
            
            if not allocations:
                return False, f"No allocations found in zone {zone}"
                
            # Create zone pick list
            pick_list_no = self._get_next_pick_list_number()
            
            pick_list = PickListRec(
                pick_no=pick_list_no,
                pick_warehouse=warehouse,
                pick_type='ZONE',
                pick_method='ZONE',
                pick_zone=zone,
                pick_status='GENERATED',
                pick_created_date=int(datetime.now().strftime("%Y%m%d")),
                pick_created_time=int(datetime.now().strftime("%H%M%S")),
                pick_created_by=self.current_user.username if self.current_user else 'SYSTEM',
                pick_total_lines=len(allocations),
                pick_total_qty=sum(alloc.alloc_quantity for alloc in allocations)
            )
            
            self.db.add(pick_list)
            self.db.flush()
            
            # Create pick lines optimized for zone
            zone_optimized = self._optimize_zone_sequence(allocations, zone)
            
            for seq, allocation in enumerate(zone_optimized, 1):
                stock, _ = self.stock_handler.process(4, key_value=allocation.alloc_stock_code)
                
                pick_line = PickListLineRec(
                    pick_line_pick_no=pick_list_no,
                    pick_line_seq=seq,
                    pick_line_stock_code=allocation.alloc_stock_code,
                    pick_line_description=stock.stock_desc if stock else '',
                    pick_line_location=allocation.alloc_location,
                    pick_line_bin=allocation.alloc_bin_location,
                    pick_line_qty_required=allocation.alloc_quantity,
                    pick_line_qty_picked=Decimal('0'),
                    pick_line_reference_type=allocation.alloc_reference_type,
                    pick_line_reference_no=allocation.alloc_reference_no,
                    pick_line_reference_line=allocation.alloc_reference_line,
                    pick_line_allocation_id=allocation.alloc_id,
                    pick_line_status='PENDING',
                    pick_line_zone=zone
                )
                
                self.db.add(pick_line)
                
            self.db.commit()
            
            return True, {
                'pick_list_no': pick_list_no,
                'zone': zone,
                'total_lines': len(allocations)
            }
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def confirm_pick(self, pick_confirmation: Dict) -> Tuple[bool, Optional[str]]:
        """
        Confirm picked quantities
        Returns (success, error_message)
        """
        pick_list_no = pick_confirmation.get('pick_list_no')
        line_confirmations = pick_confirmation.get('lines', [])
        picker = pick_confirmation.get('picker', '')
        
        pick_list = self.db.query(PickListRec).filter(
            PickListRec.pick_no == pick_list_no
        ).first()
        
        if not pick_list:
            return False, "Pick list not found"
            
        if pick_list.pick_status not in ['GENERATED', 'IN_PROGRESS']:
            return False, f"Cannot confirm pick list with status: {pick_list.pick_status}"
            
        try:
            total_picked = Decimal('0')
            lines_completed = 0
            
            for confirmation in line_confirmations:
                line_seq = confirmation.get('line_seq')
                qty_picked = Decimal(str(confirmation.get('qty_picked', 0)))
                variance_reason = confirmation.get('variance_reason', '')
                
                # Get pick line
                pick_line = self.db.query(PickListLineRec).filter(
                    and_(
                        PickListLineRec.pick_line_pick_no == pick_list_no,
                        PickListLineRec.pick_line_seq == line_seq
                    )
                ).first()
                
                if not pick_line:
                    continue
                    
                # Update pick line
                pick_line.pick_line_qty_picked = qty_picked
                pick_line.pick_line_picked_date = int(datetime.now().strftime("%Y%m%d"))
                pick_line.pick_line_picked_time = int(datetime.now().strftime("%H%M%S"))
                pick_line.pick_line_picker = picker
                
                if qty_picked >= pick_line.pick_line_qty_required:
                    pick_line.pick_line_status = 'COMPLETE'
                    lines_completed += 1
                elif qty_picked > 0:
                    pick_line.pick_line_status = 'PARTIAL'
                    pick_line.pick_line_variance_reason = variance_reason
                else:
                    pick_line.pick_line_status = 'SHORT'
                    pick_line.pick_line_variance_reason = variance_reason
                    
                total_picked += qty_picked
                
                # Update allocation status
                if pick_line.pick_line_allocation_id:
                    allocation = self.db.query(StockAllocationRec).filter(
                        StockAllocationRec.alloc_id == pick_line.pick_line_allocation_id
                    ).first()
                    
                    if allocation and qty_picked >= pick_line.pick_line_qty_required:
                        allocation.alloc_status = 'PICKED'
                        allocation.alloc_picked_date = int(datetime.now().strftime("%Y%m%d"))
                        
            # Update pick list status
            pick_list.pick_total_picked = total_picked
            pick_list.pick_completed_lines = lines_completed
            pick_list.pick_picker = picker
            pick_list.pick_completed_date = int(datetime.now().strftime("%Y%m%d"))
            pick_list.pick_completed_time = int(datetime.now().strftime("%H%M%S"))
            
            if lines_completed == pick_list.pick_total_lines:
                pick_list.pick_status = 'COMPLETE'
            elif lines_completed > 0:
                pick_list.pick_status = 'PARTIAL'
            else:
                pick_list.pick_status = 'SHORT'
                
            self.db.commit()
            
            # Log confirmation
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="CONFIRM_PICK_LIST",
                    table="pick_list_rec",
                    key=pick_list_no,
                    new_values={
                        'picker': picker,
                        'total_picked': float(total_picked),
                        'lines_completed': lines_completed
                    },
                    module="STOCK"
                )
                
            return True, f"Pick list {pick_list_no} confirmed with {lines_completed} complete lines"
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def get_pick_list_details(self, pick_list_no: str) -> Dict:
        """Get detailed pick list information"""
        pick_list = self.db.query(PickListRec).filter(
            PickListRec.pick_no == pick_list_no
        ).first()
        
        if not pick_list:
            return {"error": "Pick list not found"}
            
        # Get pick lines
        pick_lines = self.db.query(PickListLineRec).filter(
            PickListLineRec.pick_line_pick_no == pick_list_no
        ).order_by(PickListLineRec.pick_line_seq).all()
        
        return {
            'header': {
                'pick_no': pick_list.pick_no,
                'warehouse': pick_list.pick_warehouse,
                'type': pick_list.pick_type,
                'method': pick_list.pick_method,
                'status': pick_list.pick_status,
                'priority': pick_list.pick_priority,
                'zone': pick_list.pick_zone,
                'picker': pick_list.pick_picker,
                'created_date': pick_list.pick_created_date,
                'created_time': pick_list.pick_created_time,
                'estimated_time': pick_list.pick_estimated_time,
                'totals': {
                    'lines': pick_list.pick_total_lines,
                    'quantity': float(pick_list.pick_total_qty),
                    'picked': float(pick_list.pick_total_picked or 0),
                    'completed_lines': pick_list.pick_completed_lines or 0
                }
            },
            'lines': [
                {
                    'seq': line.pick_line_seq,
                    'stock_code': line.pick_line_stock_code,
                    'description': line.pick_line_description,
                    'location': line.pick_line_location,
                    'bin': line.pick_line_bin,
                    'zone': line.pick_line_zone,
                    'qty_required': float(line.pick_line_qty_required),
                    'qty_picked': float(line.pick_line_qty_picked or 0),
                    'unit': line.pick_line_unit,
                    'status': line.pick_line_status,
                    'reference': {
                        'type': line.pick_line_reference_type,
                        'no': line.pick_line_reference_no,
                        'line': line.pick_line_reference_line
                    },
                    'variance_reason': line.pick_line_variance_reason,
                    'weight': float(line.pick_line_weight or 0)
                }
                for line in pick_lines
            ]
        }
        
    def get_pick_performance(self, performance_filters: Dict) -> Dict:
        """Get pick performance analysis"""
        warehouse = performance_filters.get('warehouse')
        date_from = performance_filters.get('date_from')
        date_to = performance_filters.get('date_to')
        picker = performance_filters.get('picker')
        
        # Build query
        query = self.db.query(PickListRec)
        
        if warehouse:
            query = query.filter(PickListRec.pick_warehouse == warehouse)
        if date_from:
            query = query.filter(PickListRec.pick_created_date >= date_from)
        if date_to:
            query = query.filter(PickListRec.pick_created_date <= date_to)
        if picker:
            query = query.filter(PickListRec.pick_picker == picker)
            
        pick_lists = query.all()
        
        # Calculate performance metrics
        total_picks = len(pick_lists)
        completed_picks = len([p for p in pick_lists if p.pick_status == 'COMPLETE'])
        total_lines = sum(p.pick_total_lines for p in pick_lists)
        total_qty = sum(float(p.pick_total_qty) for p in pick_lists)
        
        # Picker performance
        picker_stats = {}
        for pick_list in pick_lists:
            if not pick_list.pick_picker:
                continue
                
            picker_name = pick_list.pick_picker
            if picker_name not in picker_stats:
                picker_stats[picker_name] = {
                    'picks': 0,
                    'lines': 0,
                    'quantity': 0,
                    'completed': 0
                }
                
            picker_stats[picker_name]['picks'] += 1
            picker_stats[picker_name]['lines'] += pick_list.pick_total_lines
            picker_stats[picker_name]['quantity'] += float(pick_list.pick_total_qty)
            if pick_list.pick_status == 'COMPLETE':
                picker_stats[picker_name]['completed'] += 1
                
        return {
            'summary': {
                'total_pick_lists': total_picks,
                'completed_pick_lists': completed_picks,
                'completion_rate': (completed_picks / total_picks * 100) if total_picks > 0 else 0,
                'total_lines': total_lines,
                'total_quantity': total_qty,
                'avg_lines_per_pick': total_lines / total_picks if total_picks > 0 else 0
            },
            'picker_performance': picker_stats,
            'efficiency_metrics': self._calculate_pick_efficiency(pick_lists)
        }
        
    def _optimize_pick_sequence(self, allocations: List[StockAllocationRec], method: str) -> List[StockAllocationRec]:
        """Optimize pick sequence based on method"""
        if method == 'DISTANCE':
            # Sort by location to minimize travel distance
            return sorted(allocations, key=lambda a: (a.alloc_location, a.alloc_bin_location))
        elif method == 'TIME':
            # Sort by estimated pick time
            return sorted(allocations, key=lambda a: self._estimate_item_pick_time(a))
        elif method == 'SEQUENCE':
            # Sort by predefined location sequence
            return sorted(allocations, key=lambda a: self._get_location_sequence(a.alloc_location))
        else:
            return allocations
            
    def _consolidate_batch_picks(self, allocations: List[StockAllocationRec]) -> List[Dict]:
        """Consolidate allocations for batch picking"""
        consolidated = {}
        
        for allocation in allocations:
            key = f"{allocation.alloc_stock_code}|{allocation.alloc_location}|{allocation.alloc_bin_location}"
            
            if key not in consolidated:
                consolidated[key] = {
                    'stock_code': allocation.alloc_stock_code,
                    'location': allocation.alloc_location,
                    'bin': allocation.alloc_bin_location,
                    'quantity': Decimal('0'),
                    'orders': set(),
                    'order_details': []
                }
                
            consolidated[key]['quantity'] += allocation.alloc_quantity
            consolidated[key]['orders'].add(allocation.alloc_reference_no)
            consolidated[key]['order_details'].append({
                'order_no': allocation.alloc_reference_no,
                'line_no': allocation.alloc_reference_line,
                'quantity': float(allocation.alloc_quantity)
            })
            
        # Convert to list and clean up
        result = []
        for item in consolidated.values():
            item['orders'] = list(item['orders'])
            result.append(item)
            
        return result
        
    def _optimize_batch_sequence(self, consolidated_picks: List[Dict], method: str) -> List[Dict]:
        """Optimize batch pick sequence"""
        if method == 'DISTANCE':
            return sorted(consolidated_picks, key=lambda p: (p['location'], p['bin']))
        elif method == 'VOLUME':
            return sorted(consolidated_picks, key=lambda p: p['quantity'], reverse=True)
        else:
            return consolidated_picks
            
    def _optimize_zone_sequence(self, allocations: List[StockAllocationRec], zone: str) -> List[StockAllocationRec]:
        """Optimize sequence within zone"""
        # Get zone-specific routing
        route = self._get_zone_route(zone)
        if route:
            return sorted(allocations, key=lambda a: route.get(a.alloc_location, 999))
        else:
            return sorted(allocations, key=lambda a: a.alloc_location)
            
    def _estimate_pick_time(self, allocations: List[StockAllocationRec]) -> int:
        """Estimate total pick time in seconds"""
        base_time = 30  # seconds per location
        item_time = 15  # seconds per item
        
        locations = set(alloc.alloc_location for alloc in allocations)
        total_items = len(allocations)
        
        return len(locations) * base_time + total_items * item_time
        
    def _estimate_batch_pick_time(self, consolidated_picks: List[Dict]) -> int:
        """Estimate batch pick time"""
        base_time = 45  # seconds per location (batch picking is slightly slower)
        item_time = 12  # seconds per consolidated item (faster due to batching)
        
        return len(consolidated_picks) * (base_time + item_time)
        
    def _estimate_item_pick_time(self, allocation: StockAllocationRec) -> int:
        """Estimate time to pick individual item"""
        base_time = 15
        qty_factor = int(float(allocation.alloc_quantity) / 10)  # Extra time for large quantities
        return base_time + qty_factor
        
    def _get_location_zone(self, location: str) -> str:
        """Get zone for location"""
        # This would lookup zone mapping
        if location.startswith('A'):
            return 'ZONE_A'
        elif location.startswith('B'):
            return 'ZONE_B'
        else:
            return 'ZONE_GENERAL'
            
    def _get_location_sequence(self, location: str) -> int:
        """Get predefined sequence number for location"""
        # This would lookup routing sequence
        sequences = {
            'A-01': 1, 'A-02': 2, 'A-03': 3,
            'B-01': 10, 'B-02': 11, 'B-03': 12
        }
        return sequences.get(location, 999)
        
    def _get_zone_route(self, zone: str) -> Dict:
        """Get optimal route within zone"""
        # This would return zone-specific routing
        return {}
        
    def _select_orders_for_wave(self, warehouse: str, criteria: Dict) -> List[SalesOrderRec]:
        """Select orders for wave based on criteria"""
        query = self.db.query(SalesOrderRec).filter(
            and_(
                SalesOrderRec.so_status == 'ALLOCATED',
                SalesOrderRec.so_warehouse == warehouse
            )
        )
        
        # Apply wave criteria
        if criteria.get('priority'):
            query = query.filter(SalesOrderRec.so_priority == criteria['priority'])
        if criteria.get('customer_type'):
            query = query.filter(SalesOrderRec.so_customer_type == criteria['customer_type'])
        if criteria.get('due_date'):
            query = query.filter(SalesOrderRec.so_due_date <= criteria['due_date'])
            
        return query.order_by(SalesOrderRec.so_priority.desc()).limit(50).all()
        
    def _group_orders_for_wave(self, orders: List[SalesOrderRec]) -> List[List[SalesOrderRec]]:
        """Group orders into optimal pick lists"""
        # Simple grouping by priority for now
        groups = []
        current_group = []
        
        for order in orders:
            current_group.append(order)
            if len(current_group) >= 5:  # Max 5 orders per group
                groups.append(current_group)
                current_group = []
                
        if current_group:
            groups.append(current_group)
            
        return groups
        
    def _generate_wave_pick_list(self, wave_no: str, order_group: List[SalesOrderRec]) -> Tuple[bool, str]:
        """Generate pick list for wave order group"""
        # Similar to batch picking but with wave reference
        pick_list_no = self._get_next_pick_list_number()
        
        # Implementation similar to batch picking
        return True, pick_list_no
        
    def _calculate_pick_efficiency(self, pick_lists: List[PickListRec]) -> Dict:
        """Calculate pick efficiency metrics"""
        if not pick_lists:
            return {}
            
        total_estimated_time = sum(p.pick_estimated_time or 0 for p in pick_lists)
        # Would calculate actual time from timestamps
        total_actual_time = total_estimated_time * 1.2  # Placeholder
        
        return {
            'estimated_time': total_estimated_time,
            'actual_time': total_actual_time,
            'efficiency_pct': (total_estimated_time / total_actual_time * 100) if total_actual_time > 0 else 100,
            'lines_per_hour': (sum(p.pick_total_lines for p in pick_lists) / (total_actual_time / 3600)) if total_actual_time > 0 else 0
        }
        
    def _get_next_pick_list_number(self) -> str:
        """Generate next pick list number"""
        system_rec, _ = self.system_handler.read_system_params()
        if system_rec:
            next_no = system_rec.pick_next_number + 1
            system_rec.pick_next_number = next_no
            return f"PL{next_no:06d}"
        return f"PL{datetime.now().strftime('%Y%m%d%H%M%S')}"
        
    def _get_next_wave_number(self) -> str:
        """Generate next wave number"""
        return f"WAVE{datetime.now().strftime('%Y%m%d%H%M%S')}"