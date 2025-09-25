"""
Cycle Counting Service - ST120 migration
Handles cycle count scheduling and processing
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from datetime import datetime, timedelta
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func, desc, asc

from app.services.file_handlers.stock_handler import StockFileHandler
from app.services.file_handlers.system_handler import SystemFileHandler
from app.models.stock import (
    StockMasterRec, StockLocationRec, StockCountRec,
    CycleCountScheduleRec, CycleCountTaskRec, CountVarianceRec
)
from app.services.stock.stock_movements import StockMovementsService
from app.core.security import log_user_action
from app.models.auth import User


class CycleCountingService:
    """
    Cycle Counting functionality
    Implements ST120 - cycle count scheduling and processing
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.stock_handler = StockFileHandler(db)
        self.system_handler = SystemFileHandler(db)
        self.movements_service = StockMovementsService(db, current_user)
        
    def generate_cycle_count_schedule(self, schedule_options: Dict) -> Tuple[bool, Optional[str]]:
        """
        Generate cycle count schedule based on ABC classification and rules
        Returns (success, error_message or schedule_data)
        """
        warehouse = schedule_options.get('warehouse', 'MAIN')
        count_period_days = schedule_options.get('count_period_days', 30)
        abc_frequencies = schedule_options.get('abc_frequencies', {
            'A': 7,   # A-class items every 7 days
            'B': 30,  # B-class items every 30 days
            'C': 90   # C-class items every 90 days
        })
        start_date = schedule_options.get('start_date', int(datetime.now().strftime("%Y%m%d")))
        
        try:
            # Get stock items that need scheduling
            stock_items = self.db.query(StockMasterRec).filter(
                and_(
                    StockMasterRec.stock_active == 'Y',
                    StockMasterRec.stock_on_hand > 0
                )
            ).all()
            
            schedule_records = []
            current_date = datetime.strptime(str(start_date), "%Y%m%d")
            
            for stock in stock_items:
                abc_class = stock.stock_abc_class or 'C'
                frequency_days = abc_frequencies.get(abc_class, 90)
                
                # Calculate next count date based on last counted
                if stock.stock_last_counted and stock.stock_last_counted > 0:
                    last_count_date = datetime.strptime(str(stock.stock_last_counted), "%Y%m%d")
                    next_count_date = last_count_date + timedelta(days=frequency_days)
                else:
                    # Never counted - schedule based on ABC priority
                    priority_offset = {'A': 0, 'B': 7, 'C': 14}.get(abc_class, 21)
                    next_count_date = current_date + timedelta(days=priority_offset)
                    
                # Only schedule if within the planning period
                if next_count_date <= current_date + timedelta(days=count_period_days):
                    schedule_record = CycleCountScheduleRec(
                        sched_warehouse=warehouse,
                        sched_stock_code=stock.stock_code,
                        sched_location='',  # Will be assigned when creating tasks
                        sched_abc_class=abc_class,
                        sched_frequency_days=frequency_days,
                        sched_next_count_date=int(next_count_date.strftime("%Y%m%d")),
                        sched_priority=self._get_count_priority(stock, abc_class),
                        sched_status='SCHEDULED',
                        sched_created_date=int(datetime.now().strftime("%Y%m%d")),
                        sched_created_by=self.current_user.username if self.current_user else 'SYSTEM'
                    )
                    
                    schedule_records.append(schedule_record)
                    self.db.add(schedule_record)
                    
            self.db.commit()
            
            # Group schedule by date for summary
            date_summary = {}
            for record in schedule_records:
                date_key = record.sched_next_count_date
                if date_key not in date_summary:
                    date_summary[date_key] = {'A': 0, 'B': 0, 'C': 0}
                date_summary[date_key][record.sched_abc_class] += 1
                
            # Log schedule generation
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="GENERATE_CYCLE_COUNT_SCHEDULE",
                    table="cycle_count_schedule_rec",
                    key=warehouse,
                    new_values={
                        'items_scheduled': len(schedule_records),
                        'period_days': count_period_days
                    },
                    module="STOCK"
                )
                
            return True, {
                'warehouse': warehouse,
                'items_scheduled': len(schedule_records),
                'period_days': count_period_days,
                'abc_frequencies': abc_frequencies,
                'date_summary': date_summary
            }
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def create_daily_count_tasks(self, task_date: int, warehouse: str = 'MAIN') -> Tuple[bool, Optional[str]]:
        """
        Create count tasks for specific date
        Returns (success, error_message or task_data)
        """
        try:
            # Get scheduled items for the date
            scheduled_items = self.db.query(CycleCountScheduleRec).filter(
                and_(
                    CycleCountScheduleRec.sched_warehouse == warehouse,
                    CycleCountScheduleRec.sched_next_count_date == task_date,
                    CycleCountScheduleRec.sched_status == 'SCHEDULED'
                )
            ).order_by(desc(CycleCountScheduleRec.sched_priority)).all()
            
            if not scheduled_items:
                return False, f"No scheduled counts found for date {task_date}"
                
            # Get count batch number
            batch_no = self._get_next_count_batch()
            
            tasks_created = 0
            
            for schedule in scheduled_items:
                # Get stock locations for the item
                locations = self.db.query(StockLocationRec).filter(
                    and_(
                        StockLocationRec.loc_stock_code == schedule.sched_stock_code,
                        StockLocationRec.loc_warehouse == warehouse,
                        StockLocationRec.loc_active == 'Y',
                        StockLocationRec.loc_qty_on_hand > 0
                    )
                ).all()
                
                # Create task for each location
                for location in locations:
                    task = CycleCountTaskRec(
                        task_batch_no=batch_no,
                        task_seq=tasks_created + 1,
                        task_warehouse=warehouse,
                        task_stock_code=schedule.sched_stock_code,
                        task_location=location.loc_location,
                        task_bin=location.loc_bin,
                        task_abc_class=schedule.sched_abc_class,
                        task_system_qty=location.loc_qty_on_hand,
                        task_counted_qty=Decimal('0'),
                        task_status='PENDING',
                        task_priority=schedule.sched_priority,
                        task_count_date=task_date,
                        task_created_date=int(datetime.now().strftime("%Y%m%d")),
                        task_assigned_to='',
                        task_count_method='PHYSICAL'  # Could be RFID, BARCODE, etc.
                    )
                    
                    self.db.add(task)
                    tasks_created += 1
                    
                # Update schedule status
                schedule.sched_status = 'TASKS_CREATED'
                schedule.sched_task_batch = batch_no
                
            self.db.commit()
            
            # Log task creation
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="CREATE_CYCLE_COUNT_TASKS",
                    table="cycle_count_task_rec",
                    key=batch_no,
                    new_values={
                        'count_date': task_date,
                        'tasks_created': tasks_created,
                        'warehouse': warehouse
                    },
                    module="STOCK"
                )
                
            return True, {
                'batch_no': batch_no,
                'count_date': task_date,
                'tasks_created': tasks_created,
                'warehouse': warehouse
            }
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def assign_count_tasks(self, assignment_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Assign count tasks to users
        Returns (success, error_message)
        """
        batch_no = assignment_data.get('batch_no')
        assignments = assignment_data.get('assignments', [])  # [{'user': 'USER1', 'task_seqs': [1,2,3]}]
        
        try:
            tasks_assigned = 0
            
            for assignment in assignments:
                user = assignment.get('user')
                task_seqs = assignment.get('task_seqs', [])
                
                # Update tasks
                tasks = self.db.query(CycleCountTaskRec).filter(
                    and_(
                        CycleCountTaskRec.task_batch_no == batch_no,
                        CycleCountTaskRec.task_seq.in_(task_seqs),
                        CycleCountTaskRec.task_status == 'PENDING'
                    )
                ).all()
                
                for task in tasks:
                    task.task_assigned_to = user
                    task.task_assigned_date = int(datetime.now().strftime("%Y%m%d"))
                    task.task_status = 'ASSIGNED'
                    tasks_assigned += 1
                    
            self.db.commit()
            
            # Log assignments
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="ASSIGN_CYCLE_COUNT_TASKS",
                    table="cycle_count_task_rec",
                    key=batch_no,
                    new_values={
                        'tasks_assigned': tasks_assigned,
                        'assignments': len(assignments)
                    },
                    module="STOCK"
                )
                
            return True, f"{tasks_assigned} tasks assigned to {len(assignments)} users"
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def record_count_result(self, count_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Record count result and process variance
        Returns (success, error_message)
        """
        batch_no = count_data.get('batch_no')
        task_seq = count_data.get('task_seq')
        counted_qty = Decimal(str(count_data.get('counted_qty', 0)))
        counter = count_data.get('counter', '')
        count_method = count_data.get('count_method', 'PHYSICAL')
        notes = count_data.get('notes', '')
        
        # Get count task
        task = self.db.query(CycleCountTaskRec).filter(
            and_(
                CycleCountTaskRec.task_batch_no == batch_no,
                CycleCountTaskRec.task_seq == task_seq
            )
        ).first()
        
        if not task:
            return False, "Count task not found"
            
        if task.task_status not in ['ASSIGNED', 'PENDING']:
            return False, f"Cannot record count for task with status: {task.task_status}"
            
        try:
            # Calculate variance
            system_qty = task.task_system_qty
            variance_qty = counted_qty - system_qty
            variance_pct = (float(variance_qty) / float(system_qty) * 100) if system_qty != 0 else 100
            
            # Update task
            task.task_counted_qty = counted_qty
            task.task_variance_qty = variance_qty
            task.task_variance_pct = Decimal(str(variance_pct))
            task.task_counter = counter
            task.task_count_method = count_method
            task.task_notes = notes
            task.task_completed_date = int(datetime.now().strftime("%Y%m%d"))
            task.task_completed_time = int(datetime.now().strftime("%H%M%S"))
            task.task_status = 'COUNTED'
            
            # Determine if variance is significant
            system_rec, _ = self.system_handler.read_system_params()
            variance_tolerance = float(system_rec.cycle_count_tolerance) if system_rec else 2.0
            
            significant_variance = abs(variance_pct) > variance_tolerance
            
            if significant_variance:
                # Create variance record
                variance_record = CountVarianceRec(
                    var_batch_no=batch_no,
                    var_task_seq=task_seq,
                    var_stock_code=task.task_stock_code,
                    var_location=task.task_location,
                    var_system_qty=system_qty,
                    var_counted_qty=counted_qty,
                    var_variance_qty=variance_qty,
                    var_variance_pct=Decimal(str(variance_pct)),
                    var_status='PENDING',
                    var_reason='',
                    var_created_date=int(datetime.now().strftime("%Y%m%d")),
                    var_counter=counter
                )
                
                self.db.add(variance_record)
                task.task_status = 'VARIANCE_PENDING'
                
                self.db.commit()
                
                return True, {
                    'variance_detected': True,
                    'variance_qty': float(variance_qty),
                    'variance_pct': variance_pct,
                    'requires_approval': True,
                    'tolerance': variance_tolerance
                }
            else:
                # Auto-approve small variances
                success, error = self._auto_process_count_variance(task, variance_qty)
                
                if success:
                    task.task_status = 'COMPLETED'
                    self.db.commit()
                    
                    return True, {
                        'variance_detected': variance_qty != 0,
                        'variance_qty': float(variance_qty),
                        'variance_pct': variance_pct,
                        'auto_processed': True
                    }
                else:
                    return False, f"Failed to process variance: {error}"
                    
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def approve_count_variance(self, variance_approval: Dict) -> Tuple[bool, Optional[str]]:
        """
        Approve count variance and create adjustment
        Returns (success, error_message)
        """
        batch_no = variance_approval.get('batch_no')
        task_seq = variance_approval.get('task_seq')
        approved = variance_approval.get('approved', True)
        approval_reason = variance_approval.get('reason', '')
        
        # Get variance record
        variance = self.db.query(CountVarianceRec).filter(
            and_(
                CountVarianceRec.var_batch_no == batch_no,
                CountVarianceRec.var_task_seq == task_seq,
                CountVarianceRec.var_status == 'PENDING'
            )
        ).first()
        
        if not variance:
            return False, "Variance record not found"
            
        try:
            if approved:
                # Create stock adjustment
                adjustment_data = {
                    'stock_code': variance.var_stock_code,
                    'warehouse': 'MAIN',  # Would get from task
                    'location': variance.var_location,
                    'adjustment_qty': float(variance.var_variance_qty),
                    'reason': 'CYCLE_COUNT_VARIANCE',
                    'reference': f"CC-{batch_no}-{task_seq}"
                }
                
                success, error = self.movements_service.process_adjustment(adjustment_data)
                
                if success:
                    variance.var_status = 'APPROVED'
                    variance.var_approved_by = self.current_user.username if self.current_user else 'SYSTEM'
                    variance.var_approved_date = int(datetime.now().strftime("%Y%m%d"))
                    variance.var_reason = approval_reason
                    
                    # Update task
                    task = self.db.query(CycleCountTaskRec).filter(
                        and_(
                            CycleCountTaskRec.task_batch_no == batch_no,
                            CycleCountTaskRec.task_seq == task_seq
                        )
                    ).first()
                    
                    if task:
                        task.task_status = 'COMPLETED'
                        
                    self.db.commit()
                    
                    # Log approval
                    if self.current_user:
                        log_user_action(
                            db=self.db,
                            user=self.current_user,
                            action="APPROVE_COUNT_VARIANCE",
                            table="count_variance_rec",
                            key=f"{batch_no}-{task_seq}",
                            new_values={
                                'approved': approved,
                                'variance_qty': float(variance.var_variance_qty),
                                'reason': approval_reason
                            },
                            module="STOCK"
                        )
                        
                    return True, f"Variance approved and adjustment created"
                else:
                    return False, f"Failed to create adjustment: {error}"
            else:
                # Reject variance - may require recount
                variance.var_status = 'REJECTED'
                variance.var_rejected_by = self.current_user.username if self.current_user else 'SYSTEM'
                variance.var_rejected_date = int(datetime.now().strftime("%Y%m%d"))
                variance.var_reason = approval_reason
                
                # Update task for recount
                task = self.db.query(CycleCountTaskRec).filter(
                    and_(
                        CycleCountTaskRec.task_batch_no == batch_no,
                        CycleCountTaskRec.task_seq == task_seq
                    )
                ).first()
                
                if task:
                    task.task_status = 'RECOUNT_REQUIRED'
                    
                self.db.commit()
                
                return True, f"Variance rejected - recount required"
                
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def get_count_workload(self, workload_filters: Optional[Dict] = None) -> List[Dict]:
        """Get cycle count workload for users"""
        filters = workload_filters or {}
        
        query = self.db.query(CycleCountTaskRec).filter(
            CycleCountTaskRec.task_status.in_(['PENDING', 'ASSIGNED'])
        )
        
        # Apply filters
        if filters.get('assigned_to'):
            query = query.filter(CycleCountTaskRec.task_assigned_to == filters['assigned_to'])
        if filters.get('warehouse'):
            query = query.filter(CycleCountTaskRec.task_warehouse == filters['warehouse'])
        if filters.get('count_date'):
            query = query.filter(CycleCountTaskRec.task_count_date == filters['count_date'])
        if filters.get('abc_class'):
            query = query.filter(CycleCountTaskRec.task_abc_class == filters['abc_class'])
            
        # Order by priority and location
        tasks = query.order_by(
            desc(CycleCountTaskRec.task_priority),
            CycleCountTaskRec.task_location
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
                'location': task.task_location,
                'bin': task.task_bin,
                'abc_class': task.task_abc_class,
                'system_qty': float(task.task_system_qty),
                'unit': stock.stock_unit if stock else 'EA',
                'status': task.task_status,
                'priority': task.task_priority,
                'assigned_to': task.task_assigned_to,
                'count_date': task.task_count_date,
                'days_overdue': self._calculate_count_overdue_days(task.task_count_date)
            })
            
        return workload
        
    def get_count_performance(self, performance_period: int = 30) -> Dict:
        """Get cycle count performance metrics"""
        try:
            # Date range
            end_date = int(datetime.now().strftime("%Y%m%d"))
            start_date = int((datetime.now() - timedelta(days=performance_period)).strftime("%Y%m%d"))
            
            # Get completed tasks
            completed_tasks = self.db.query(CycleCountTaskRec).filter(
                and_(
                    CycleCountTaskRec.task_status.in_(['COMPLETED', 'VARIANCE_PENDING']),
                    CycleCountTaskRec.task_completed_date >= start_date,
                    CycleCountTaskRec.task_completed_date <= end_date
                )
            ).all()
            
            # Get variances
            variances = self.db.query(CountVarianceRec).filter(
                and_(
                    CountVarianceRec.var_created_date >= start_date,
                    CountVarianceRec.var_created_date <= end_date
                )
            ).all()
            
            # Calculate metrics
            total_counts = len(completed_tasks)
            total_variances = len(variances)
            variance_rate = (total_variances / total_counts * 100) if total_counts > 0 else 0
            
            # Accuracy by ABC class
            abc_accuracy = {}
            for abc_class in ['A', 'B', 'C']:
                class_tasks = [t for t in completed_tasks if t.task_abc_class == abc_class]
                class_variances = [v for v in variances if v.var_stock_code in [t.task_stock_code for t in class_tasks]]
                
                class_accuracy = ((len(class_tasks) - len(class_variances)) / len(class_tasks) * 100) if class_tasks else 100
                abc_accuracy[f'class_{abc_class}'] = {
                    'counts': len(class_tasks),
                    'variances': len(class_variances),
                    'accuracy_pct': class_accuracy
                }
                
            # Counter performance
            counter_performance = {}
            for task in completed_tasks:
                counter = task.task_counter or 'UNASSIGNED'
                if counter not in counter_performance:
                    counter_performance[counter] = {
                        'counts': 0,
                        'variances': 0,
                        'accuracy_pct': 100
                    }
                    
                counter_performance[counter]['counts'] += 1
                
                # Check if this task had a variance
                if any(v.var_batch_no == task.task_batch_no and v.var_task_seq == task.task_seq for v in variances):
                    counter_performance[counter]['variances'] += 1
                    
            # Calculate accuracy percentages
            for counter_data in counter_performance.values():
                if counter_data['counts'] > 0:
                    counter_data['accuracy_pct'] = (
                        (counter_data['counts'] - counter_data['variances']) / 
                        counter_data['counts'] * 100
                    )
                    
            return {
                'period_days': performance_period,
                'summary': {
                    'total_counts': total_counts,
                    'total_variances': total_variances,
                    'variance_rate_pct': variance_rate,
                    'accuracy_rate_pct': 100 - variance_rate,
                    'avg_counts_per_day': total_counts / performance_period if performance_period > 0 else 0
                },
                'abc_performance': abc_accuracy,
                'counter_performance': counter_performance,
                'trends': self._calculate_count_trends(completed_tasks, variances)
            }
            
        except Exception as e:
            return {"error": str(e)}
            
    def _get_count_priority(self, stock: StockMasterRec, abc_class: str) -> int:
        """Calculate count priority for stock item"""
        priority = {'A': 9, 'B': 6, 'C': 3}.get(abc_class, 3)
        
        # Increase priority for high-value items
        if stock.stock_on_hand * stock.stock_average_cost > 10000:
            priority += 2
            
        # Increase priority for items not counted recently
        if stock.stock_last_counted == 0:
            priority += 1
        elif stock.stock_last_counted > 0:
            days_since_count = (datetime.now() - datetime.strptime(str(stock.stock_last_counted), "%Y%m%d")).days
            if days_since_count > 180:
                priority += 2
            elif days_since_count > 90:
                priority += 1
                
        return min(priority, 10)  # Cap at 10
        
    def _auto_process_count_variance(self, task: CycleCountTaskRec, variance_qty: Decimal) -> Tuple[bool, Optional[str]]:
        """Auto-process small count variances"""
        if variance_qty == 0:
            return True, None
            
        # Create automatic adjustment for small variances
        adjustment_data = {
            'stock_code': task.task_stock_code,
            'warehouse': task.task_warehouse,
            'location': task.task_location,
            'adjustment_qty': float(variance_qty),
            'reason': 'CYCLE_COUNT_AUTO_ADJUSTMENT',
            'reference': f"CC-{task.task_batch_no}-{task.task_seq}"
        }
        
        return self.movements_service.process_adjustment(adjustment_data)
        
    def _calculate_count_overdue_days(self, count_date: int) -> int:
        """Calculate days overdue for count"""
        if count_date == 0:
            return 0
            
        count_dt = datetime.strptime(str(count_date), "%Y%m%d").date()
        today = datetime.now().date()
        
        if count_dt < today:
            return (today - count_dt).days
        return 0
        
    def _calculate_count_trends(self, completed_tasks: List[CycleCountTaskRec], 
                              variances: List[CountVarianceRec]) -> Dict:
        """Calculate count performance trends"""
        if not completed_tasks:
            return {}
            
        # Group by week
        weekly_counts = {}
        weekly_variances = {}
        
        for task in completed_tasks:
            if task.task_completed_date:
                task_date = datetime.strptime(str(task.task_completed_date), "%Y%m%d")
                week_start = task_date - timedelta(days=task_date.weekday())
                week_key = week_start.strftime("%Y-%W")
                
                weekly_counts[week_key] = weekly_counts.get(week_key, 0) + 1
                
        for variance in variances:
            variance_date = datetime.strptime(str(variance.var_created_date), "%Y%m%d")
            week_start = variance_date - timedelta(days=variance_date.weekday())
            week_key = week_start.strftime("%Y-%W")
            
            weekly_variances[week_key] = weekly_variances.get(week_key, 0) + 1
            
        # Calculate weekly accuracy
        weekly_accuracy = {}
        for week in weekly_counts:
            counts = weekly_counts[week]
            vars_count = weekly_variances.get(week, 0)
            weekly_accuracy[week] = ((counts - vars_count) / counts * 100) if counts > 0 else 100
            
        # Simple trend calculation
        weeks = sorted(weekly_accuracy.keys())
        if len(weeks) >= 2:
            recent_accuracy = weekly_accuracy[weeks[-1]]
            previous_accuracy = weekly_accuracy[weeks[-2]] if len(weeks) > 1 else recent_accuracy
            trend_direction = 'IMPROVING' if recent_accuracy > previous_accuracy else 'DECLINING' if recent_accuracy < previous_accuracy else 'STABLE'
        else:
            trend_direction = 'STABLE'
            
        return {
            'weekly_counts': weekly_counts,
            'weekly_variances': weekly_variances,
            'weekly_accuracy': weekly_accuracy,
            'trend_direction': trend_direction
        }
        
    def _get_next_count_batch(self) -> str:
        """Generate next count batch number"""
        return f"CC{datetime.now().strftime('%Y%m%d%H%M%S')}"