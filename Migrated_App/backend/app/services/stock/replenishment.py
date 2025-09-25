"""
Replenishment Service - ST110 migration
Handles automatic replenishment suggestions and processing
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from datetime import datetime, timedelta
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func, desc

from app.services.file_handlers.stock_handler import StockFileHandler
from app.services.file_handlers.system_handler import SystemFileHandler
from app.models.stock import (
    StockMasterRec, StockLocationRec, StockMovementRec, 
    ReplenishmentRec, ReplenishmentTaskRec
)
from app.models.warehouse import BinLocationRec
from app.core.security import log_user_action
from app.models.auth import User


class ReplenishmentService:
    """
    Replenishment functionality
    Implements ST110 - automatic replenishment processing
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.stock_handler = StockFileHandler(db)
        self.system_handler = SystemFileHandler(db)
        
    def analyze_replenishment_requirements(self, analysis_options: Optional[Dict] = None) -> Dict:
        """
        Analyze replenishment requirements across warehouse
        Returns analysis results and recommendations
        """
        options = analysis_options or {}
        warehouse = options.get('warehouse', 'MAIN')
        zone = options.get('zone')
        min_priority = options.get('min_priority', 1)
        
        try:
            # Get all pick locations that may need replenishment
            query = self.db.query(StockLocationRec).filter(
                and_(
                    StockLocationRec.loc_warehouse == warehouse,
                    StockLocationRec.loc_pickable == 'Y',
                    StockLocationRec.loc_active == 'Y'
                )
            )
            
            if zone:
                query = query.filter(StockLocationRec.loc_zone == zone)
                
            pick_locations = query.all()
            
            replenishment_needs = []
            
            for location in pick_locations:
                # Check if location needs replenishment
                need = self._assess_location_replenishment_need(location)
                
                if need and need['priority'] >= min_priority:
                    # Find source locations
                    sources = self._find_replenishment_sources(location, need)
                    
                    if sources:
                        replenishment_needs.append({
                            'location': location.loc_location,
                            'stock_code': location.loc_stock_code,
                            'need': need,
                            'sources': sources,
                            'recommended_qty': need['recommended_qty'],
                            'priority': need['priority']
                        })
                        
            # Sort by priority
            replenishment_needs.sort(key=lambda x: x['priority'], reverse=True)
            
            # Group by stock code for efficiency
            by_stock_code = {}
            for need in replenishment_needs:
                stock_code = need['stock_code']
                if stock_code not in by_stock_code:
                    by_stock_code[stock_code] = []
                by_stock_code[stock_code].append(need)
                
            return {
                'warehouse': warehouse,
                'zone': zone,
                'analysis_date': datetime.now().strftime("%Y-%m-%d"),
                'summary': {
                    'locations_analyzed': len(pick_locations),
                    'replenishment_required': len(replenishment_needs),
                    'high_priority': len([n for n in replenishment_needs if n['priority'] >= 8]),
                    'medium_priority': len([n for n in replenishment_needs if 5 <= n['priority'] < 8]),
                    'low_priority': len([n for n in replenishment_needs if n['priority'] < 5])
                },
                'replenishment_needs': replenishment_needs,
                'by_stock_code': by_stock_code,
                'recommendations': self._generate_replenishment_recommendations(replenishment_needs)
            }
            
        except Exception as e:
            return {"error": str(e)}
            
    def create_replenishment_tasks(self, task_request: Dict) -> Tuple[bool, Optional[str]]:
        """
        Create replenishment tasks from analysis
        Returns (success, error_message or task_data)
        """
        warehouse = task_request.get('warehouse', 'MAIN')
        replenishment_needs = task_request.get('needs', [])
        task_type = task_request.get('task_type', 'STANDARD')  # STANDARD, EMERGENCY, BULK
        assigned_to = task_request.get('assigned_to', '')
        
        if not replenishment_needs:
            return False, "No replenishment needs specified"
            
        try:
            # Get next replenishment batch number
            batch_no = self._get_next_replenishment_batch()
            
            # Create batch header
            batch = ReplenishmentRec(
                repl_batch_no=batch_no,
                repl_warehouse=warehouse,
                repl_type=task_type,
                repl_status='CREATED',
                repl_created_date=int(datetime.now().strftime("%Y%m%d")),
                repl_created_time=int(datetime.now().strftime("%H%M%S")),
                repl_created_by=self.current_user.username if self.current_user else 'SYSTEM',
                repl_assigned_to=assigned_to,
                repl_total_tasks=len(replenishment_needs),
                repl_completed_tasks=0,
                repl_priority='HIGH' if task_type == 'EMERGENCY' else 'NORMAL'
            )
            
            self.db.add(batch)
            self.db.flush()
            
            # Create individual tasks
            tasks_created = 0
            
            for need in replenishment_needs:
                task = ReplenishmentTaskRec(
                    task_batch_no=batch_no,
                    task_seq=tasks_created + 1,
                    task_stock_code=need.get('stock_code'),
                    task_from_location=need.get('source_location'),
                    task_to_location=need.get('destination_location'),
                    task_quantity=Decimal(str(need.get('quantity', 0))),
                    task_priority=need.get('priority', 5),
                    task_type=task_type,
                    task_status='PENDING',
                    task_assigned_to=assigned_to,
                    task_created_date=int(datetime.now().strftime("%Y%m%d")),
                    task_estimated_time=self._estimate_replenishment_time(need)
                )
                
                self.db.add(task)
                tasks_created += 1
                
            batch.repl_total_tasks = tasks_created
            
            self.db.commit()
            
            # Log creation
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="CREATE_REPLENISHMENT_TASKS",
                    table="replenishment_rec",
                    key=batch_no,
                    new_values={
                        'tasks': tasks_created,
                        'type': task_type,
                        'assigned_to': assigned_to
                    },
                    module="STOCK"
                )
                
            return True, {
                'batch_no': batch_no,
                'tasks_created': tasks_created,
                'type': task_type,
                'assigned_to': assigned_to
            }
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def complete_replenishment_task(self, task_completion: Dict) -> Tuple[bool, Optional[str]]:
        """
        Complete replenishment task and move stock
        Returns (success, error_message)
        """
        batch_no = task_completion.get('batch_no')
        task_seq = task_completion.get('task_seq')
        actual_qty = Decimal(str(task_completion.get('actual_qty', 0)))
        completion_notes = task_completion.get('notes', '')
        
        # Get task
        task = self.db.query(ReplenishmentTaskRec).filter(
            and_(
                ReplenishmentTaskRec.task_batch_no == batch_no,
                ReplenishmentTaskRec.task_seq == task_seq
            )
        ).first()
        
        if not task:
            return False, "Replenishment task not found"
            
        if task.task_status != 'PENDING':
            return False, f"Cannot complete task with status: {task.task_status}"
            
        try:
            # Validate stock availability at source
            from_location = self.db.query(StockLocationRec).filter(
                and_(
                    StockLocationRec.loc_stock_code == task.task_stock_code,
                    StockLocationRec.loc_location == task.task_from_location
                )
            ).first()
            
            if not from_location:
                return False, "Source location not found"
                
            available = from_location.loc_qty_on_hand - from_location.loc_qty_allocated
            if actual_qty > available:
                return False, f"Insufficient stock at source. Available: {available}"
                
            # Move stock from source to destination
            success = self._move_stock_for_replenishment(
                task.task_stock_code,
                task.task_from_location,
                task.task_to_location,
                actual_qty,
                batch_no
            )
            
            if not success:
                return False, "Failed to move stock"
                
            # Update task
            task.task_status = 'COMPLETED'
            task.task_actual_qty = actual_qty
            task.task_completed_date = int(datetime.now().strftime("%Y%m%d"))
            task.task_completed_time = int(datetime.now().strftime("%H%M%S"))
            task.task_completed_by = self.current_user.username if self.current_user else 'SYSTEM'
            task.task_notes = completion_notes
            
            # Update batch progress
            batch = self.db.query(ReplenishmentRec).filter(
                ReplenishmentRec.repl_batch_no == batch_no
            ).first()
            
            if batch:
                batch.repl_completed_tasks += 1
                
                # Check if batch is complete
                if batch.repl_completed_tasks >= batch.repl_total_tasks:
                    batch.repl_status = 'COMPLETED'
                    batch.repl_completed_date = int(datetime.now().strftime("%Y%m%d"))
                elif batch.repl_status == 'CREATED':
                    batch.repl_status = 'IN_PROGRESS'
                    
            self.db.commit()
            
            # Log completion
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="COMPLETE_REPLENISHMENT_TASK",
                    table="replenishment_task_rec",
                    key=f"{batch_no}-{task_seq}",
                    new_values={
                        'actual_qty': float(actual_qty),
                        'notes': completion_notes
                    },
                    module="STOCK"
                )
                
            return True, f"Replenishment task {batch_no}-{task_seq} completed"
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def get_replenishment_workload(self, workload_filters: Optional[Dict] = None) -> List[Dict]:
        """Get replenishment workload for users"""
        filters = workload_filters or {}
        
        query = self.db.query(ReplenishmentTaskRec).filter(
            ReplenishmentTaskRec.task_status == 'PENDING'
        )
        
        # Apply filters
        if filters.get('assigned_to'):
            query = query.filter(ReplenishmentTaskRec.task_assigned_to == filters['assigned_to'])
        if filters.get('warehouse'):
            query = query.join(ReplenishmentRec).filter(
                ReplenishmentRec.repl_warehouse == filters['warehouse']
            )
        if filters.get('priority'):
            query = query.filter(ReplenishmentTaskRec.task_priority >= filters['priority'])
            
        # Order by priority and creation date
        tasks = query.order_by(
            desc(ReplenishmentTaskRec.task_priority),
            ReplenishmentTaskRec.task_created_date
        ).all()
        
        workload = []
        for task in tasks:
            # Get stock details
            stock, _ = self.stock_handler.process(4, key_value=task.task_stock_code)
            
            workload.append({
                'batch_no': task.task_batch_no,
                'task_seq': task.task_seq,
                'stock_code': task.task_stock_code,
                'description': stock.stock_desc if stock else '',
                'from_location': task.task_from_location,
                'to_location': task.task_to_location,
                'quantity': float(task.task_quantity),
                'unit': stock.stock_unit if stock else 'EA',
                'priority': task.task_priority,
                'type': task.task_type,
                'assigned_to': task.task_assigned_to,
                'estimated_time': task.task_estimated_time,
                'created_date': task.task_created_date,
                'age_hours': self._calculate_task_age_hours(task.task_created_date),
                'distance': self._calculate_travel_distance(task.task_from_location, task.task_to_location)
            })
            
        return workload
        
    def optimize_replenishment_routes(self, route_request: Dict) -> Dict:
        """Optimize replenishment routes for efficiency"""
        warehouse = route_request.get('warehouse', 'MAIN')
        assigned_to = route_request.get('assigned_to')
        max_tasks = route_request.get('max_tasks', 20)
        
        try:
            # Get pending tasks
            query = self.db.query(ReplenishmentTaskRec).filter(
                ReplenishmentTaskRec.task_status == 'PENDING'
            )
            
            if assigned_to:
                query = query.filter(ReplenishmentTaskRec.task_assigned_to == assigned_to)
                
            tasks = query.order_by(desc(ReplenishmentTaskRec.task_priority)).limit(max_tasks).all()
            
            if not tasks:
                return {"message": "No pending replenishment tasks found"}
                
            # Group tasks by proximity and optimize routes
            optimized_routes = self._optimize_task_routes(tasks)
            
            return {
                'warehouse': warehouse,
                'assigned_to': assigned_to,
                'total_tasks': len(tasks),
                'optimized_routes': optimized_routes,
                'estimated_time_savings': self._calculate_route_time_savings(tasks, optimized_routes),
                'total_distance': sum(route['total_distance'] for route in optimized_routes)
            }
            
        except Exception as e:
            return {"error": str(e)}
            
    def get_replenishment_performance(self, performance_period: int = 30) -> Dict:
        """Get replenishment performance metrics"""
        try:
            # Date range
            end_date = int(datetime.now().strftime("%Y%m%d"))
            start_date = int((datetime.now() - timedelta(days=performance_period)).strftime("%Y%m%d"))
            
            # Get completed tasks
            completed_tasks = self.db.query(ReplenishmentTaskRec).filter(
                and_(
                    ReplenishmentTaskRec.task_status == 'COMPLETED',
                    ReplenishmentTaskRec.task_completed_date >= start_date,
                    ReplenishmentTaskRec.task_completed_date <= end_date
                )
            ).all()
            
            # Get performance metrics
            total_tasks = len(completed_tasks)
            total_time_estimated = sum(task.task_estimated_time for task in completed_tasks)
            
            # Calculate actual time (simplified - would use real timestamps)
            total_time_actual = sum(task.task_estimated_time * 1.1 for task in completed_tasks)  # Assume 10% over
            
            # Task completion by user
            user_performance = {}
            for task in completed_tasks:
                user = task.task_completed_by
                if user not in user_performance:
                    user_performance[user] = {
                        'tasks_completed': 0,
                        'total_quantity': Decimal('0'),
                        'avg_time_per_task': 0
                    }
                    
                user_performance[user]['tasks_completed'] += 1
                user_performance[user]['total_quantity'] += task.task_actual_qty or task.task_quantity
                
            # Calculate averages
            for user_data in user_performance.values():
                if user_data['tasks_completed'] > 0:
                    user_data['avg_time_per_task'] = total_time_actual / total_tasks  # Simplified
                    
            return {
                'period_days': performance_period,
                'summary': {
                    'total_tasks_completed': total_tasks,
                    'estimated_time_minutes': total_time_estimated,
                    'actual_time_minutes': total_time_actual,
                    'efficiency_pct': (total_time_estimated / total_time_actual * 100) if total_time_actual > 0 else 100,
                    'avg_tasks_per_day': total_tasks / performance_period if performance_period > 0 else 0
                },
                'user_performance': user_performance,
                'trends': self._calculate_replenishment_trends(completed_tasks)
            }
            
        except Exception as e:
            return {"error": str(e)}
            
    def _assess_location_replenishment_need(self, location: StockLocationRec) -> Optional[Dict]:
        """Assess if location needs replenishment"""
        current_qty = location.loc_qty_on_hand
        min_qty = location.loc_min_qty
        max_qty = location.loc_max_qty
        
        if current_qty <= min_qty:
            # Calculate priority based on how far below minimum
            shortage_pct = (min_qty - current_qty) / min_qty * 100 if min_qty > 0 else 100
            
            if shortage_pct >= 75:
                priority = 10  # Critical
            elif shortage_pct >= 50:
                priority = 8   # High
            elif shortage_pct >= 25:
                priority = 6   # Medium
            else:
                priority = 4   # Low
                
            # Calculate recommended replenishment quantity
            recommended_qty = max_qty - current_qty if max_qty > current_qty else min_qty
            
            return {
                'current_qty': float(current_qty),
                'min_qty': float(min_qty),
                'max_qty': float(max_qty),
                'shortage_pct': shortage_pct,
                'priority': priority,
                'recommended_qty': float(recommended_qty),
                'urgency': 'CRITICAL' if priority >= 9 else 'HIGH' if priority >= 7 else 'NORMAL'
            }
            
        return None
        
    def _find_replenishment_sources(self, destination: StockLocationRec, need: Dict) -> List[Dict]:
        """Find potential source locations for replenishment"""
        stock_code = destination.loc_stock_code
        warehouse = destination.loc_warehouse
        required_qty = Decimal(str(need['recommended_qty']))
        
        # Find bulk/reserve locations with available stock
        sources = self.db.query(StockLocationRec).filter(
            and_(
                StockLocationRec.loc_stock_code == stock_code,
                StockLocationRec.loc_warehouse == warehouse,
                StockLocationRec.loc_location != destination.loc_location,
                StockLocationRec.loc_replenish == 'Y',
                StockLocationRec.loc_qty_on_hand > StockLocationRec.loc_qty_allocated,
                StockLocationRec.loc_active == 'Y'
            )
        ).order_by(desc(StockLocationRec.loc_qty_on_hand)).all()
        
        source_options = []
        for source in sources:
            available = source.loc_qty_on_hand - source.loc_qty_allocated
            
            if available > 0:
                source_options.append({
                    'location': source.loc_location,
                    'available_qty': float(available),
                    'can_supply_full': available >= required_qty,
                    'distance': self._calculate_travel_distance(source.loc_location, destination.loc_location),
                    'zone': getattr(source, 'loc_zone', 'UNKNOWN')
                })
                
        # Sort by ability to supply full quantity, then by distance
        source_options.sort(key=lambda x: (-x['can_supply_full'], x['distance']))
        
        return source_options[:3]  # Return top 3 options
        
    def _move_stock_for_replenishment(self, stock_code: str, from_location: str, 
                                    to_location: str, quantity: Decimal, batch_no: str) -> bool:
        """Move stock from source to destination for replenishment"""
        try:
            # Update source location
            source_loc = self.db.query(StockLocationRec).filter(
                and_(
                    StockLocationRec.loc_stock_code == stock_code,
                    StockLocationRec.loc_location == from_location
                )
            ).first()
            
            if source_loc:
                source_loc.loc_qty_on_hand -= quantity
                
            # Update destination location
            dest_loc = self.db.query(StockLocationRec).filter(
                and_(
                    StockLocationRec.loc_stock_code == stock_code,
                    StockLocationRec.loc_location == to_location
                )
            ).first()
            
            if dest_loc:
                dest_loc.loc_qty_on_hand += quantity
            else:
                # Create destination location if it doesn't exist
                stock, _ = self.stock_handler.process(4, key_value=stock_code)
                if stock:
                    new_loc = StockLocationRec(
                        loc_stock_code=stock_code,
                        loc_warehouse=source_loc.loc_warehouse if source_loc else 'MAIN',
                        loc_location=to_location,
                        loc_qty_on_hand=quantity,
                        loc_qty_allocated=Decimal('0'),
                        loc_primary='N',
                        loc_pickable='Y',
                        loc_active='Y',
                        loc_created_date=int(datetime.now().strftime("%Y%m%d"))
                    )
                    self.db.add(new_loc)
                    
            # Create movement records
            from app.services.stock.stock_movements import StockMovementsService
            movements_service = StockMovementsService(self.db, self.current_user)
            
            # Transfer out
            movements_service._create_movement_record(
                stock_code=stock_code,
                move_type='TRO',
                warehouse=source_loc.loc_warehouse if source_loc else 'MAIN',
                location=from_location,
                qty_in=Decimal('0'),
                qty_out=quantity,
                unit_cost=Decimal('0'),  # No cost change for internal transfers
                reference=f"REPL-{batch_no}",
                balance=Decimal('0')  # Would calculate actual balance
            )
            
            # Transfer in
            movements_service._create_movement_record(
                stock_code=stock_code,
                move_type='TRI',
                warehouse=source_loc.loc_warehouse if source_loc else 'MAIN',
                location=to_location,
                qty_in=quantity,
                qty_out=Decimal('0'),
                unit_cost=Decimal('0'),
                reference=f"REPL-{batch_no}",
                balance=Decimal('0')
            )
            
            return True
            
        except Exception:
            return False
            
    def _estimate_replenishment_time(self, need: Dict) -> int:
        """Estimate time required for replenishment task (minutes)"""
        base_time = 5  # Base time per task
        quantity_factor = int(float(need.get('quantity', 0)) / 10)  # Extra time for large quantities
        distance_factor = int(need.get('distance', 0) / 10)  # Extra time for distance
        
        return base_time + quantity_factor + distance_factor
        
    def _calculate_travel_distance(self, from_location: str, to_location: str) -> float:
        """Calculate travel distance between locations (simplified)"""
        # This would use actual warehouse coordinates
        # For now, use location code similarity as approximation
        if from_location[:1] == to_location[:1]:  # Same zone
            return 10.0
        elif from_location[:2] == to_location[:2]:  # Same aisle
            return 20.0
        else:
            return 50.0
            
    def _calculate_task_age_hours(self, created_date: int) -> float:
        """Calculate task age in hours"""
        created_dt = datetime.strptime(str(created_date), "%Y%m%d")
        age_delta = datetime.now() - created_dt
        return age_delta.total_seconds() / 3600
        
    def _optimize_task_routes(self, tasks: List[ReplenishmentTaskRec]) -> List[Dict]:
        """Optimize task routes for minimum travel"""
        # Simple optimization - group by zones and sort by proximity
        zone_groups = {}
        
        for task in tasks:
            from_zone = task.task_from_location[:1]  # First character as zone
            if from_zone not in zone_groups:
                zone_groups[from_zone] = []
            zone_groups[from_zone].append(task)
            
        optimized_routes = []
        for zone, zone_tasks in zone_groups.items():
            # Sort tasks within zone by location proximity
            zone_tasks.sort(key=lambda t: t.task_from_location)
            
            route_distance = sum(
                self._calculate_travel_distance(
                    zone_tasks[i].task_from_location,
                    zone_tasks[i+1].task_from_location if i+1 < len(zone_tasks) else zone_tasks[i].task_to_location
                )
                for i in range(len(zone_tasks))
            )
            
            optimized_routes.append({
                'zone': zone,
                'tasks': [
                    {
                        'batch_no': t.task_batch_no,
                        'seq': t.task_seq,
                        'stock_code': t.task_stock_code,
                        'from_location': t.task_from_location,
                        'to_location': t.task_to_location,
                        'quantity': float(t.task_quantity)
                    }
                    for t in zone_tasks
                ],
                'total_distance': route_distance,
                'estimated_time': sum(task.task_estimated_time for task in zone_tasks)
            })
            
        return optimized_routes
        
    def _calculate_route_time_savings(self, original_tasks: List[ReplenishmentTaskRec], 
                                    optimized_routes: List[Dict]) -> Dict:
        """Calculate time savings from route optimization"""
        original_time = sum(task.task_estimated_time for task in original_tasks)
        optimized_time = sum(route['estimated_time'] for route in optimized_routes)
        
        # Add travel time reduction benefit (simplified)
        travel_time_saved = len(original_tasks) * 2  # 2 minutes saved per task on average
        
        return {
            'original_time_minutes': original_time,
            'optimized_time_minutes': optimized_time,
            'travel_time_saved_minutes': travel_time_saved,
            'total_time_saved_minutes': travel_time_saved,
            'efficiency_improvement_pct': (travel_time_saved / original_time * 100) if original_time > 0 else 0
        }
        
    def _calculate_replenishment_trends(self, completed_tasks: List[ReplenishmentTaskRec]) -> Dict:
        """Calculate replenishment trends"""
        if not completed_tasks:
            return {}
            
        # Group by week
        weekly_counts = {}
        for task in completed_tasks:
            task_date = datetime.strptime(str(task.task_completed_date), "%Y%m%d")
            week_start = task_date - timedelta(days=task_date.weekday())
            week_key = week_start.strftime("%Y-%W")
            
            weekly_counts[week_key] = weekly_counts.get(week_key, 0) + 1
            
        # Simple trend calculation
        weeks = sorted(weekly_counts.keys())
        if len(weeks) >= 2:
            recent_avg = sum(weekly_counts[w] for w in weeks[-2:]) / 2
            older_avg = sum(weekly_counts[w] for w in weeks[:-2]) / max(1, len(weeks) - 2)
            trend_pct = (recent_avg - older_avg) / older_avg * 100 if older_avg > 0 else 0
        else:
            trend_pct = 0
            
        return {
            'weekly_task_counts': weekly_counts,
            'trend_direction': 'INCREASING' if trend_pct > 5 else 'DECREASING' if trend_pct < -5 else 'STABLE',
            'trend_pct': trend_pct
        }
        
    def _generate_replenishment_recommendations(self, needs: List[Dict]) -> List[Dict]:
        """Generate recommendations for replenishment optimization"""
        recommendations = []
        
        # High priority locations
        high_priority = [n for n in needs if n['priority'] >= 8]
        if len(high_priority) > 5:
            recommendations.append({
                'type': 'URGENT_ATTENTION',
                'priority': 'HIGH',
                'description': f"{len(high_priority)} locations require urgent replenishment",
                'action': 'Create emergency replenishment tasks'
            })
            
        # Frequent replenishment locations
        frequent_locations = {}
        for need in needs:
            loc = need['location']
            frequent_locations[loc] = frequent_locations.get(loc, 0) + 1
            
        frequent = [loc for loc, count in frequent_locations.items() if count > 3]
        if frequent:
            recommendations.append({
                'type': 'INVENTORY_OPTIMIZATION',
                'priority': 'MEDIUM',
                'description': f"{len(frequent)} locations frequently need replenishment",
                'action': 'Consider increasing minimum stock levels or max quantities'
            })
            
        return recommendations
        
    def _get_next_replenishment_batch(self) -> str:
        """Generate next replenishment batch number"""
        return f"REPL{datetime.now().strftime('%Y%m%d%H%M%S')}"