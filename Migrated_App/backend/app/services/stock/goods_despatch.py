"""
Goods Despatch Service - ST090 migration
Handles goods despatch processing and documentation
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from datetime import datetime
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func, desc

from app.services.file_handlers.stock_handler import StockFileHandler
from app.services.file_handlers.system_handler import SystemFileHandler
from app.models.stock import StockMasterRec, PickListRec, PickListLineRec
from app.models.sales import SalesOrderRec, DespatchRec, DespatchLineRec, LoadingAdviceRec
from app.models.transport import CarrierRec, VehicleRec, RouteRec
from app.core.security import log_user_action
from app.models.auth import User


class GoodsDespatchService:
    """
    Goods Despatch functionality
    Implements ST090 - despatch processing and documentation
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.stock_handler = StockFileHandler(db)
        self.system_handler = SystemFileHandler(db)
        
    def create_despatch(self, despatch_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Create despatch from pick lists
        Returns (success, error_message or despatch_data)
        """
        pick_list_nos = despatch_data.get('pick_lists', [])
        carrier_code = despatch_data.get('carrier_code', '')
        vehicle_id = despatch_data.get('vehicle_id', '')
        route_code = despatch_data.get('route_code', '')
        despatch_date = despatch_data.get('despatch_date', int(datetime.now().strftime("%Y%m%d")))
        
        if not pick_list_nos:
            return False, "Pick lists required for despatch"
            
        try:
            # Validate pick lists
            pick_lists = []
            for pl_no in pick_list_nos:
                pl = self.db.query(PickListRec).filter(PickListRec.pick_no == pl_no).first()
                if not pl:
                    return False, f"Pick list {pl_no} not found"
                if pl.pick_status != 'COMPLETE':
                    return False, f"Pick list {pl_no} not complete"
                pick_lists.append(pl)
                
            # Create despatch header
            despatch_no = self._get_next_despatch_number()
            
            despatch = DespatchRec(
                desp_no=despatch_no,
                desp_date=despatch_date,
                desp_warehouse=pick_lists[0].pick_warehouse,
                desp_carrier_code=carrier_code,
                desp_vehicle_id=vehicle_id,
                desp_route_code=route_code,
                desp_status='CREATED',
                desp_created_date=int(datetime.now().strftime("%Y%m%d")),
                desp_created_time=int(datetime.now().strftime("%H%M%S")),
                desp_created_by=self.current_user.username if self.current_user else 'SYSTEM',
                desp_total_orders=0,
                desp_total_items=0,
                desp_total_weight=Decimal('0'),
                desp_total_volume=Decimal('0')
            )
            
            self.db.add(despatch)
            self.db.flush()
            
            # Process pick lists
            total_orders = set()
            total_items = 0
            total_weight = Decimal('0')
            total_volume = Decimal('0')
            
            for pick_list in pick_lists:
                # Get pick lines
                pick_lines = self.db.query(PickListLineRec).filter(
                    PickListLineRec.pick_line_pick_no == pick_list.pick_no
                ).all()
                
                for pick_line in pick_lines:
                    # Create despatch line
                    desp_line = DespatchLineRec(
                        desp_line_despatch_no=despatch_no,
                        desp_line_pick_list=pick_list.pick_no,
                        desp_line_pick_seq=pick_line.pick_line_seq,
                        desp_line_stock_code=pick_line.pick_line_stock_code,
                        desp_line_description=pick_line.pick_line_description,
                        desp_line_quantity=pick_line.pick_line_qty_picked,
                        desp_line_unit=pick_line.pick_line_unit,
                        desp_line_weight=pick_line.pick_line_weight or Decimal('0'),
                        desp_line_order_no=pick_line.pick_line_reference_no,
                        desp_line_order_line=pick_line.pick_line_reference_line,
                        desp_line_status='DESPATCHED'
                    )
                    
                    self.db.add(desp_line)
                    
                    # Accumulate totals
                    if pick_line.pick_line_reference_no:
                        total_orders.add(pick_line.pick_line_reference_no)
                    total_items += 1
                    total_weight += pick_line.pick_line_weight or Decimal('0')
                    
                    # Calculate volume from stock master
                    stock, _ = self.stock_handler.process(4, key_value=pick_line.pick_line_stock_code)
                    if stock:
                        total_volume += stock.stock_volume * pick_line.pick_line_qty_picked
                        
                # Update pick list status
                pick_list.pick_status = 'DESPATCHED'
                pick_list.pick_despatch_no = despatch_no
                
            # Update despatch totals
            despatch.desp_total_orders = len(total_orders)
            despatch.desp_total_items = total_items
            despatch.desp_total_weight = total_weight
            despatch.desp_total_volume = total_volume
            
            self.db.commit()
            
            # Log creation
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="CREATE_DESPATCH",
                    table="despatch_rec",
                    key=despatch_no,
                    new_values={
                        'pick_lists': len(pick_lists),
                        'orders': len(total_orders),
                        'items': total_items
                    },
                    module="STOCK"
                )
                
            return True, {
                'despatch_no': despatch_no,
                'total_orders': len(total_orders),
                'total_items': total_items,
                'total_weight': float(total_weight),
                'pick_lists': pick_list_nos
            }
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def generate_loading_advice(self, loading_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Generate loading advice for despatch
        Returns (success, error_message or loading_advice_data)
        """
        despatch_no = loading_data.get('despatch_no')
        loading_bay = loading_data.get('loading_bay', '')
        scheduled_time = loading_data.get('scheduled_time', '')
        
        despatch = self.db.query(DespatchRec).filter(
            DespatchRec.desp_no == despatch_no
        ).first()
        
        if not despatch:
            return False, "Despatch not found"
            
        if despatch.desp_status not in ['CREATED', 'READY']:
            return False, f"Cannot generate loading advice for despatch with status: {despatch.desp_status}"
            
        try:
            # Get despatch lines grouped by order
            despatch_lines = self.db.query(DespatchLineRec).filter(
                DespatchLineRec.desp_line_despatch_no == despatch_no
            ).order_by(DespatchLineRec.desp_line_order_no).all()
            
            # Group by order
            orders = {}
            for line in despatch_lines:
                order_no = line.desp_line_order_no
                if order_no not in orders:
                    orders[order_no] = {
                        'lines': [],
                        'total_weight': Decimal('0'),
                        'total_items': 0
                    }
                    
                orders[order_no]['lines'].append(line)
                orders[order_no]['total_weight'] += line.desp_line_weight
                orders[order_no]['total_items'] += 1
                
            # Get next loading advice number
            loading_no = self._get_next_loading_number()
            
            # Create loading advice
            loading_advice = LoadingAdviceRec(
                loading_no=loading_no,
                loading_despatch_no=despatch_no,
                loading_date=int(datetime.now().strftime("%Y%m%d")),
                loading_bay=loading_bay,
                loading_scheduled_time=scheduled_time,
                loading_carrier=despatch.desp_carrier_code,
                loading_vehicle=despatch.desp_vehicle_id,
                loading_status='SCHEDULED',
                loading_total_orders=len(orders),
                loading_total_weight=despatch.desp_total_weight,
                loading_created_by=self.current_user.username if self.current_user else 'SYSTEM'
            )
            
            self.db.add(loading_advice)
            self.db.flush()
            
            # Update despatch status
            despatch.desp_status = 'LOADING_SCHEDULED'
            despatch.desp_loading_no = loading_no
            
            self.db.commit()
            
            return True, {
                'loading_no': loading_no,
                'despatch_no': despatch_no,
                'loading_bay': loading_bay,
                'scheduled_time': scheduled_time,
                'orders': list(orders.keys()),
                'total_weight': float(despatch.desp_total_weight)
            }
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def confirm_loading(self, loading_confirmation: Dict) -> Tuple[bool, Optional[str]]:
        """
        Confirm goods loaded onto vehicle
        Returns (success, error_message)
        """
        loading_no = loading_confirmation.get('loading_no')
        actual_start_time = loading_confirmation.get('actual_start_time', '')
        actual_end_time = loading_confirmation.get('actual_end_time', '')
        loader = loading_confirmation.get('loader', '')
        notes = loading_confirmation.get('notes', '')
        
        loading_advice = self.db.query(LoadingAdviceRec).filter(
            LoadingAdviceRec.loading_no == loading_no
        ).first()
        
        if not loading_advice:
            return False, "Loading advice not found"
            
        if loading_advice.loading_status != 'SCHEDULED':
            return False, f"Cannot confirm loading with status: {loading_advice.loading_status}"
            
        try:
            # Update loading advice
            loading_advice.loading_status = 'LOADED'
            loading_advice.loading_actual_start = actual_start_time
            loading_advice.loading_actual_end = actual_end_time
            loading_advice.loading_loader = loader
            loading_advice.loading_notes = notes
            loading_advice.loading_completed_date = int(datetime.now().strftime("%Y%m%d"))
            
            # Update despatch status
            despatch = self.db.query(DespatchRec).filter(
                DespatchRec.desp_no == loading_advice.loading_despatch_no
            ).first()
            
            if despatch:
                despatch.desp_status = 'LOADED'
                despatch.desp_loaded_date = int(datetime.now().strftime("%Y%m%d"))
                despatch.desp_loaded_by = loader
                
            self.db.commit()
            
            # Log confirmation
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="CONFIRM_LOADING",
                    table="loading_advice_rec",
                    key=loading_no,
                    new_values={
                        'loader': loader,
                        'start_time': actual_start_time,
                        'end_time': actual_end_time
                    },
                    module="STOCK"
                )
                
            return True, f"Loading {loading_no} confirmed as complete"
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def despatch_vehicle(self, vehicle_despatch: Dict) -> Tuple[bool, Optional[str]]:
        """
        Despatch vehicle with goods
        Returns (success, error_message)
        """
        despatch_no = vehicle_despatch.get('despatch_no')
        driver_name = vehicle_despatch.get('driver_name', '')
        departure_time = vehicle_despatch.get('departure_time', '')
        estimated_arrival = vehicle_despatch.get('estimated_arrival', '')
        
        despatch = self.db.query(DespatchRec).filter(
            DespatchRec.desp_no == despatch_no
        ).first()
        
        if not despatch:
            return False, "Despatch not found"
            
        if despatch.desp_status != 'LOADED':
            return False, f"Cannot despatch vehicle with status: {despatch.desp_status}"
            
        try:
            # Update despatch
            despatch.desp_status = 'DESPATCHED'
            despatch.desp_driver_name = driver_name
            despatch.desp_departure_time = departure_time
            despatch.desp_estimated_arrival = estimated_arrival
            despatch.desp_despatched_date = int(datetime.now().strftime("%Y%m%d"))
            despatch.desp_despatched_by = self.current_user.username if self.current_user else 'SYSTEM'
            
            # Update sales orders status
            despatch_lines = self.db.query(DespatchLineRec).filter(
                DespatchLineRec.desp_line_despatch_no == despatch_no
            ).all()
            
            order_nos = set(line.desp_line_order_no for line in despatch_lines if line.desp_line_order_no)
            
            for order_no in order_nos:
                so = self.db.query(SalesOrderRec).filter(SalesOrderRec.so_no == order_no).first()
                if so:
                    so.so_status = 'DESPATCHED'
                    so.so_despatch_date = int(datetime.now().strftime("%Y%m%d"))
                    so.so_despatch_no = despatch_no
                    
            self.db.commit()
            
            # Log despatch
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="DESPATCH_VEHICLE",
                    table="despatch_rec",
                    key=despatch_no,
                    new_values={
                        'driver': driver_name,
                        'departure_time': departure_time,
                        'orders': len(order_nos)
                    },
                    module="STOCK"
                )
                
            return True, f"Vehicle despatched with {len(order_nos)} orders"
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def get_despatch_summary(self, filters: Optional[Dict] = None) -> List[Dict]:
        """Get despatch summary with filters"""
        filters = filters or {}
        
        query = self.db.query(DespatchRec)
        
        # Apply filters
        if filters.get('warehouse'):
            query = query.filter(DespatchRec.desp_warehouse == filters['warehouse'])
        if filters.get('carrier'):
            query = query.filter(DespatchRec.desp_carrier_code == filters['carrier'])
        if filters.get('date_from'):
            query = query.filter(DespatchRec.desp_date >= filters['date_from'])
        if filters.get('date_to'):
            query = query.filter(DespatchRec.desp_date <= filters['date_to'])
        if filters.get('status'):
            query = query.filter(DespatchRec.desp_status == filters['status'])
            
        # Order and limit
        limit = filters.get('limit', 100)
        despatches = query.order_by(desc(DespatchRec.desp_date)).limit(limit).all()
        
        return [
            {
                'despatch_no': desp.desp_no,
                'date': desp.desp_date,
                'warehouse': desp.desp_warehouse,
                'carrier': desp.desp_carrier_code,
                'vehicle': desp.desp_vehicle_id,
                'route': desp.desp_route_code,
                'status': desp.desp_status,
                'totals': {
                    'orders': desp.desp_total_orders,
                    'items': desp.desp_total_items,
                    'weight': float(desp.desp_total_weight),
                    'volume': float(desp.desp_total_volume)
                },
                'driver': desp.desp_driver_name,
                'departure_time': desp.desp_departure_time,
                'created_by': desp.desp_created_by
            }
            for desp in despatches
        ]
        
    def get_loading_schedule(self, schedule_date: int, warehouse: str = None) -> List[Dict]:
        """Get loading schedule for date"""
        query = self.db.query(LoadingAdviceRec).filter(
            LoadingAdviceRec.loading_date == schedule_date
        )
        
        if warehouse:
            query = query.join(DespatchRec).filter(
                DespatchRec.desp_warehouse == warehouse
            )
            
        loading_schedule = query.order_by(LoadingAdviceRec.loading_scheduled_time).all()
        
        return [
            {
                'loading_no': loading.loading_no,
                'despatch_no': loading.loading_despatch_no,
                'loading_bay': loading.loading_bay,
                'scheduled_time': loading.loading_scheduled_time,
                'carrier': loading.loading_carrier,
                'vehicle': loading.loading_vehicle,
                'status': loading.loading_status,
                'total_orders': loading.loading_total_orders,
                'total_weight': float(loading.loading_total_weight),
                'actual_start': loading.loading_actual_start,
                'actual_end': loading.loading_actual_end,
                'loader': loading.loading_loader
            }
            for loading in loading_schedule
        ]
        
    def get_despatch_performance(self, performance_filters: Dict) -> Dict:
        """Get despatch performance metrics"""
        warehouse = performance_filters.get('warehouse')
        date_from = performance_filters.get('date_from')
        date_to = performance_filters.get('date_to')
        
        # Build query
        query = self.db.query(DespatchRec)
        
        if warehouse:
            query = query.filter(DespatchRec.desp_warehouse == warehouse)
        if date_from:
            query = query.filter(DespatchRec.desp_date >= date_from)
        if date_to:
            query = query.filter(DespatchRec.desp_date <= date_to)
            
        despatches = query.all()
        
        # Calculate metrics
        total_despatches = len(despatches)
        total_orders = sum(d.desp_total_orders for d in despatches)
        total_items = sum(d.desp_total_items for d in despatches)
        total_weight = sum(float(d.desp_total_weight) for d in despatches)
        
        # Status breakdown
        status_counts = {}
        for despatch in despatches:
            status = despatch.desp_status
            status_counts[status] = status_counts.get(status, 0) + 1
            
        # Carrier performance
        carrier_stats = {}
        for despatch in despatches:
            carrier = despatch.desp_carrier_code
            if carrier not in carrier_stats:
                carrier_stats[carrier] = {
                    'despatches': 0,
                    'orders': 0,
                    'weight': 0
                }
            carrier_stats[carrier]['despatches'] += 1
            carrier_stats[carrier]['orders'] += despatch.desp_total_orders
            carrier_stats[carrier]['weight'] += float(despatch.desp_total_weight)
            
        return {
            'summary': {
                'total_despatches': total_despatches,
                'total_orders': total_orders,
                'total_items': total_items,
                'total_weight': total_weight,
                'avg_orders_per_despatch': total_orders / total_despatches if total_despatches > 0 else 0,
                'avg_weight_per_despatch': total_weight / total_despatches if total_despatches > 0 else 0
            },
            'status_breakdown': status_counts,
            'carrier_performance': carrier_stats,
            'efficiency_metrics': self._calculate_despatch_efficiency(despatches)
        }
        
    def _calculate_despatch_efficiency(self, despatches: List[DespatchRec]) -> Dict:
        """Calculate despatch efficiency metrics"""
        if not despatches:
            return {}
            
        # On-time despatch rate (assuming scheduled vs actual)
        on_time = 0
        late = 0
        
        for despatch in despatches:
            if despatch.desp_status == 'DESPATCHED':
                # Would compare scheduled vs actual times
                on_time += 1  # Simplified
            else:
                late += 1
                
        total = len(despatches)
        on_time_rate = (on_time / total * 100) if total > 0 else 0
        
        # Vehicle utilization
        total_capacity = sum(100 for d in despatches)  # Assume 100% capacity each
        total_utilization = sum(min(float(d.desp_total_weight) / 1000, 100) for d in despatches)  # Simplified
        utilization_rate = (total_utilization / total_capacity * 100) if total_capacity > 0 else 0
        
        return {
            'on_time_despatch_rate': on_time_rate,
            'vehicle_utilization_rate': utilization_rate,
            'average_loading_time': 45,  # minutes - would calculate from actual data
            'despatch_frequency': len(despatches) / 30  # per day over 30 day period
        }
        
    def _get_next_despatch_number(self) -> str:
        """Generate next despatch number"""
        system_rec, _ = self.system_handler.read_system_params()
        if system_rec:
            next_no = system_rec.despatch_next_number + 1
            system_rec.despatch_next_number = next_no
            return f"DESP{next_no:06d}"
        return f"DESP{datetime.now().strftime('%Y%m%d%H%M%S')}"
        
    def _get_next_loading_number(self) -> str:
        """Generate next loading advice number"""
        return f"LOAD{datetime.now().strftime('%Y%m%d%H%M%S')}"