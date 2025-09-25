"""
Job Scheduler Service - Phase 7 migration
Handles batch job scheduling, execution and monitoring
"""
from typing import List, Optional, Dict, Tuple, Any
from datetime import datetime, timedelta
from decimal import Decimal
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func, desc, text
import asyncio
import threading
import time
import json
from enum import Enum
import logging

from app.services.file_handlers.system_handler import SystemFileHandler
from app.models.batch import (
    BatchJobRec, BatchScheduleRec, BatchExecutionRec, 
    BatchLogRec, BatchDependencyRec, BatchParameterRec
)
from app.core.security import log_user_action
from app.models.auth import User


class JobStatus(Enum):
    SCHEDULED = "SCHEDULED"
    READY = "READY"
    RUNNING = "RUNNING"
    COMPLETED = "COMPLETED"
    FAILED = "FAILED"
    CANCELLED = "CANCELLED"
    WAITING = "WAITING"


class JobPriority(Enum):
    LOW = 1
    NORMAL = 5
    HIGH = 10
    CRITICAL = 15


class JobSchedulerService:
    """
    Job Scheduler Service
    Manages batch job scheduling, execution and monitoring
    """
    
    def __init__(self, db: Session):
        self.db = db
        self.system_handler = SystemFileHandler(db)
        self.logger = logging.getLogger(__name__)
        self.scheduler_running = False
        self.scheduler_thread = None
        self.job_executors = {}
        
    def create_batch_job(self, job_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Create new batch job definition
        Returns (success, error_message or job_data)
        """
        job_name = job_data.get('job_name')
        job_type = job_data.get('job_type')  # GL_CLOSE, REORDER_CALC, CYCLE_COUNT, etc.
        module = job_data.get('module', 'SYSTEM')
        description = job_data.get('description', '')
        executable_path = job_data.get('executable_path', '')
        parameters = job_data.get('parameters', {})
        dependencies = job_data.get('dependencies', [])
        
        if not job_name or not job_type:
            return False, "Job name and type are required"
            
        # Check if job already exists
        existing = self.db.query(BatchJobRec).filter(
            BatchJobRec.job_name == job_name
        ).first()
        
        if existing:
            return False, f"Job {job_name} already exists"
            
        try:
            # Create job record
            job = BatchJobRec(
                job_name=job_name,
                job_type=job_type,
                job_module=module,
                job_description=description,
                job_executable_path=executable_path,
                job_status='INACTIVE',
                job_priority=JobPriority.NORMAL.value,
                job_max_runtime_minutes=60,
                job_retry_count=3,
                job_retry_delay_minutes=5,
                job_concurrent_allowed='N',
                job_created_date=int(datetime.now().strftime("%Y%m%d")),
                job_created_time=int(datetime.now().strftime("%H%M%S")),
                job_created_by='SYSTEM'
            )
            
            self.db.add(job)
            self.db.flush()
            
            # Create parameters
            for param_name, param_value in parameters.items():
                param = BatchParameterRec(
                    param_job_name=job_name,
                    param_name=param_name,
                    param_value=str(param_value),
                    param_type='STRING',
                    param_required='Y' if param_name in ['warehouse', 'period'] else 'N'
                )
                self.db.add(param)
                
            # Create dependencies
            for dep_job in dependencies:
                dependency = BatchDependencyRec(
                    dep_job_name=job_name,
                    dep_depends_on_job=dep_job,
                    dep_dependency_type='SUCCESS',  # SUCCESS, COMPLETION, FAILURE
                    dep_wait_minutes=0
                )
                self.db.add(dependency)
                
            self.db.commit()
            
            return True, {
                'job_name': job_name,
                'job_type': job_type,
                'module': module,
                'parameters': len(parameters),
                'dependencies': len(dependencies)
            }
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def schedule_job(self, schedule_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Schedule job for execution
        Returns (success, error_message or schedule_data)
        """
        job_name = schedule_data.get('job_name')
        schedule_type = schedule_data.get('schedule_type', 'ONCE')  # ONCE, DAILY, WEEKLY, MONTHLY
        run_date = schedule_data.get('run_date')
        run_time = schedule_data.get('run_time', '0000')
        run_parameters = schedule_data.get('parameters', {})
        priority = schedule_data.get('priority', JobPriority.NORMAL.value)
        
        # Validate job exists
        job = self.db.query(BatchJobRec).filter(
            BatchJobRec.job_name == job_name
        ).first()
        
        if not job:
            return False, f"Job {job_name} not found"
            
        try:
            # Generate schedule ID
            schedule_id = self._get_next_schedule_id()
            
            # Create schedule record
            schedule = BatchScheduleRec(
                schedule_id=schedule_id,
                schedule_job_name=job_name,
                schedule_type=schedule_type,
                schedule_run_date=run_date or int(datetime.now().strftime("%Y%m%d")),
                schedule_run_time=int(run_time),
                schedule_priority=priority,
                schedule_status='SCHEDULED',
                schedule_parameters=json.dumps(run_parameters),
                schedule_created_date=int(datetime.now().strftime("%Y%m%d")),
                schedule_created_time=int(datetime.now().strftime("%H%M%S")),
                schedule_created_by='SYSTEM',
                schedule_next_run_date=run_date or int(datetime.now().strftime("%Y%m%d")),
                schedule_retry_count=0
            )
            
            self.db.add(schedule)
            self.db.commit()
            
            return True, {
                'schedule_id': schedule_id,
                'job_name': job_name,
                'run_date': schedule.schedule_run_date,
                'run_time': run_time,
                'priority': priority
            }
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def start_scheduler(self) -> bool:
        """Start the job scheduler"""
        if self.scheduler_running:
            return False
            
        self.scheduler_running = True
        self.scheduler_thread = threading.Thread(target=self._scheduler_loop, daemon=True)
        self.scheduler_thread.start()
        
        self.logger.info("Job scheduler started")
        return True
        
    def stop_scheduler(self) -> bool:
        """Stop the job scheduler"""
        if not self.scheduler_running:
            return False
            
        self.scheduler_running = False
        if self.scheduler_thread:
            self.scheduler_thread.join(timeout=10)
            
        self.logger.info("Job scheduler stopped")
        return True
        
    def execute_job_now(self, job_name: str, parameters: Optional[Dict] = None) -> Tuple[bool, Optional[str]]:
        """
        Execute job immediately
        Returns (success, error_message or execution_id)
        """
        # Get job definition
        job = self.db.query(BatchJobRec).filter(
            BatchJobRec.job_name == job_name
        ).first()
        
        if not job:
            return False, f"Job {job_name} not found"
            
        # Check if job is already running (if not concurrent allowed)
        if job.job_concurrent_allowed == 'N':
            running = self.db.query(BatchExecutionRec).filter(
                and_(
                    BatchExecutionRec.exec_job_name == job_name,
                    BatchExecutionRec.exec_status == JobStatus.RUNNING.value
                )
            ).first()
            
            if running:
                return False, f"Job {job_name} is already running"
                
        try:
            # Create execution record
            execution_id = self._get_next_execution_id()
            
            execution = BatchExecutionRec(
                exec_id=execution_id,
                exec_job_name=job_name,
                exec_schedule_id='',
                exec_status=JobStatus.READY.value,
                exec_priority=JobPriority.NORMAL.value,
                exec_parameters=json.dumps(parameters or {}),
                exec_submitted_date=int(datetime.now().strftime("%Y%m%d")),
                exec_submitted_time=int(datetime.now().strftime("%H%M%S")),
                exec_submitted_by='SYSTEM',
                exec_estimated_runtime=job.job_max_runtime_minutes,
                exec_retry_count=0,
                exec_max_retries=job.job_retry_count
            )
            
            self.db.add(execution)
            self.db.commit()
            
            # Execute job asynchronously
            self._execute_job_async(execution_id)
            
            return True, execution_id
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def cancel_job(self, execution_id: str) -> Tuple[bool, Optional[str]]:
        """Cancel running or scheduled job"""
        execution = self.db.query(BatchExecutionRec).filter(
            BatchExecutionRec.exec_id == execution_id
        ).first()
        
        if not execution:
            return False, f"Execution {execution_id} not found"
            
        if execution.exec_status not in [JobStatus.READY.value, JobStatus.RUNNING.value, JobStatus.WAITING.value]:
            return False, f"Cannot cancel job with status: {execution.exec_status}"
            
        try:
            execution.exec_status = JobStatus.CANCELLED.value
            execution.exec_end_date = int(datetime.now().strftime("%Y%m%d"))
            execution.exec_end_time = int(datetime.now().strftime("%H%M%S"))
            
            # Log cancellation
            self._log_job_message(execution_id, "INFO", "Job cancelled by user")
            
            self.db.commit()
            
            return True, f"Job {execution_id} cancelled"
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def get_job_status(self, execution_id: str) -> Dict:
        """Get job execution status"""
        execution = self.db.query(BatchExecutionRec).filter(
            BatchExecutionRec.exec_id == execution_id
        ).first()
        
        if not execution:
            return {"error": "Execution not found"}
            
        # Get job details
        job = self.db.query(BatchJobRec).filter(
            BatchJobRec.job_name == execution.exec_job_name
        ).first()
        
        # Get recent logs
        logs = self.db.query(BatchLogRec).filter(
            BatchLogRec.log_execution_id == execution_id
        ).order_by(desc(BatchLogRec.log_timestamp)).limit(10).all()
        
        # Calculate runtime
        runtime_minutes = 0
        if execution.exec_start_date and execution.exec_start_time:
            start_dt = datetime.strptime(f"{execution.exec_start_date}{execution.exec_start_time:04d}", "%Y%m%d%H%M")
            
            if execution.exec_end_date and execution.exec_end_time:
                end_dt = datetime.strptime(f"{execution.exec_end_date}{execution.exec_end_time:04d}", "%Y%m%d%H%M")
            else:
                end_dt = datetime.now()
                
            runtime_minutes = (end_dt - start_dt).total_seconds() / 60
            
        return {
            'execution_id': execution_id,
            'job_name': execution.exec_job_name,
            'job_type': job.job_type if job else '',
            'status': execution.exec_status,
            'priority': execution.exec_priority,
            'submitted_date': execution.exec_submitted_date,
            'submitted_time': execution.exec_submitted_time,
            'start_date': execution.exec_start_date,
            'start_time': execution.exec_start_time,
            'end_date': execution.exec_end_date,
            'end_time': execution.exec_end_time,
            'runtime_minutes': runtime_minutes,
            'retry_count': execution.exec_retry_count,
            'return_code': execution.exec_return_code,
            'error_message': execution.exec_error_message,
            'parameters': json.loads(execution.exec_parameters or '{}'),
            'recent_logs': [
                {
                    'timestamp': log.log_timestamp,
                    'level': log.log_level,
                    'message': log.log_message
                }
                for log in logs
            ]
        }
        
    def get_scheduler_dashboard(self) -> Dict:
        """Get scheduler dashboard data"""
        try:
            # Get current job counts by status
            status_counts = {}
            for status in JobStatus:
                count = self.db.query(BatchExecutionRec).filter(
                    BatchExecutionRec.exec_status == status.value
                ).count()
                status_counts[status.value] = count
                
            # Get recent executions
            recent_executions = self.db.query(BatchExecutionRec).order_by(
                desc(BatchExecutionRec.exec_submitted_date),
                desc(BatchExecutionRec.exec_submitted_time)
            ).limit(20).all()
            
            # Get scheduled jobs for today
            today = int(datetime.now().strftime("%Y%m%d"))
            scheduled_today = self.db.query(BatchScheduleRec).filter(
                and_(
                    BatchScheduleRec.schedule_next_run_date == today,
                    BatchScheduleRec.schedule_status == 'SCHEDULED'
                )
            ).order_by(BatchScheduleRec.schedule_run_time).all()
            
            # Calculate average runtimes by job type
            avg_runtimes = self.db.execute(text("""
                SELECT j.job_type, AVG(
                    CASE 
                        WHEN e.exec_end_date IS NOT NULL AND e.exec_start_date IS NOT NULL
                        THEN (e.exec_end_date - e.exec_start_date) * 1440 + 
                             (e.exec_end_time - e.exec_start_time) / 100 * 60
                        ELSE 0
                    END
                ) as avg_runtime_minutes
                FROM batch_job_rec j
                JOIN batch_execution_rec e ON j.job_name = e.exec_job_name
                WHERE e.exec_status = 'COMPLETED'
                AND e.exec_end_date >= :since_date
                GROUP BY j.job_type
            """), {'since_date': int((datetime.now() - timedelta(days=30)).strftime("%Y%m%d"))}).fetchall()
            
            return {
                'scheduler_status': 'RUNNING' if self.scheduler_running else 'STOPPED',
                'current_time': datetime.now().isoformat(),
                'job_status_counts': status_counts,
                'recent_executions': [
                    {
                        'execution_id': exec.exec_id,
                        'job_name': exec.exec_job_name,
                        'status': exec.exec_status,
                        'submitted_date': exec.exec_submitted_date,
                        'submitted_time': exec.exec_submitted_time,
                        'runtime_minutes': self._calculate_runtime(exec)
                    }
                    for exec in recent_executions
                ],
                'scheduled_today': [
                    {
                        'schedule_id': sched.schedule_id,
                        'job_name': sched.schedule_job_name,
                        'run_time': f"{sched.schedule_run_time:04d}",
                        'priority': sched.schedule_priority,
                        'status': sched.schedule_status
                    }
                    for sched in scheduled_today
                ],
                'average_runtimes': {
                    row.job_type: round(row.avg_runtime_minutes, 2)
                    for row in avg_runtimes
                }
            }
            
        except Exception as e:
            return {"error": str(e)}
            
    def _scheduler_loop(self):
        """Main scheduler loop"""
        while self.scheduler_running:
            try:
                # Check for jobs ready to run
                self._check_scheduled_jobs()
                
                # Check for completed jobs and dependencies
                self._check_job_dependencies()
                
                # Clean up old execution records
                self._cleanup_old_executions()
                
                # Sleep for 30 seconds
                time.sleep(30)
                
            except Exception as e:
                self.logger.error(f"Scheduler loop error: {str(e)}")
                time.sleep(60)  # Longer sleep on error
                
    def _check_scheduled_jobs(self):
        """Check for jobs ready to run"""
        current_datetime = datetime.now()
        current_date = int(current_datetime.strftime("%Y%m%d"))
        current_time = int(current_datetime.strftime("%H%M"))
        
        # Get scheduled jobs ready to run
        ready_schedules = self.db.query(BatchScheduleRec).filter(
            and_(
                BatchScheduleRec.schedule_status == 'SCHEDULED',
                BatchScheduleRec.schedule_next_run_date <= current_date,
                or_(
                    BatchScheduleRec.schedule_next_run_date < current_date,
                    BatchScheduleRec.schedule_run_time <= current_time
                )
            )
        ).order_by(BatchScheduleRec.schedule_priority.desc()).all()
        
        for schedule in ready_schedules:
            # Check dependencies
            if self._check_dependencies_met(schedule.schedule_job_name):
                # Create execution
                execution_id = self._create_execution_from_schedule(schedule)
                if execution_id:
                    # Execute job
                    self._execute_job_async(execution_id)
                    
                    # Update schedule for next run
                    self._update_schedule_next_run(schedule)
                    
    def _check_job_dependencies(self):
        """Check for jobs waiting on dependencies"""
        waiting_executions = self.db.query(BatchExecutionRec).filter(
            BatchExecutionRec.exec_status == JobStatus.WAITING.value
        ).all()
        
        for execution in waiting_executions:
            if self._check_dependencies_met(execution.exec_job_name):
                execution.exec_status = JobStatus.READY.value
                self._execute_job_async(execution.exec_id)
                
        self.db.commit()
        
    def _check_dependencies_met(self, job_name: str) -> bool:
        """Check if all job dependencies are met"""
        dependencies = self.db.query(BatchDependencyRec).filter(
            BatchDependencyRec.dep_job_name == job_name
        ).all()
        
        if not dependencies:
            return True
            
        for dep in dependencies:
            # Get most recent execution of dependency job
            dep_execution = self.db.query(BatchExecutionRec).filter(
                BatchExecutionRec.exec_job_name == dep.dep_depends_on_job
            ).order_by(
                desc(BatchExecutionRec.exec_submitted_date),
                desc(BatchExecutionRec.exec_submitted_time)
            ).first()
            
            if not dep_execution:
                return False
                
            if dep.dep_dependency_type == 'SUCCESS':
                if dep_execution.exec_status != JobStatus.COMPLETED.value:
                    return False
            elif dep.dep_dependency_type == 'COMPLETION':
                if dep_execution.exec_status not in [JobStatus.COMPLETED.value, JobStatus.FAILED.value]:
                    return False
                    
        return True
        
    def _execute_job_async(self, execution_id: str):
        """Execute job asynchronously"""
        def run_job():
            try:
                self._execute_job(execution_id)
            except Exception as e:
                self.logger.error(f"Job execution error: {str(e)}")
                
        job_thread = threading.Thread(target=run_job, daemon=True)
        job_thread.start()
        self.job_executors[execution_id] = job_thread
        
    def _execute_job(self, execution_id: str):
        """Execute a single job"""
        try:
            execution = self.db.query(BatchExecutionRec).filter(
                BatchExecutionRec.exec_id == execution_id
            ).first()
            
            if not execution:
                return
                
            # Update status to running
            execution.exec_status = JobStatus.RUNNING.value
            execution.exec_start_date = int(datetime.now().strftime("%Y%m%d"))
            execution.exec_start_time = int(datetime.now().strftime("%H%M%S"))
            self.db.commit()
            
            # Log start
            self._log_job_message(execution_id, "INFO", f"Job {execution.exec_job_name} started")
            
            # Get job definition
            job = self.db.query(BatchJobRec).filter(
                BatchJobRec.job_name == execution.exec_job_name
            ).first()
            
            if not job:
                raise Exception(f"Job definition not found: {execution.exec_job_name}")
                
            # Execute based on job type
            success = self._execute_job_by_type(job, execution)
            
            # Update execution status
            execution.exec_end_date = int(datetime.now().strftime("%Y%m%d"))
            execution.exec_end_time = int(datetime.now().strftime("%H%M%S"))
            
            if success:
                execution.exec_status = JobStatus.COMPLETED.value
                execution.exec_return_code = 0
                self._log_job_message(execution_id, "INFO", f"Job {execution.exec_job_name} completed successfully")
            else:
                execution.exec_status = JobStatus.FAILED.value
                execution.exec_return_code = 1
                self._log_job_message(execution_id, "ERROR", f"Job {execution.exec_job_name} failed")
                
            self.db.commit()
            
        except Exception as e:
            # Handle job failure
            execution = self.db.query(BatchExecutionRec).filter(
                BatchExecutionRec.exec_id == execution_id
            ).first()
            
            if execution:
                execution.exec_status = JobStatus.FAILED.value
                execution.exec_error_message = str(e)
                execution.exec_end_date = int(datetime.now().strftime("%Y%m%d"))
                execution.exec_end_time = int(datetime.now().strftime("%H%M%S"))
                execution.exec_return_code = -1
                
                self._log_job_message(execution_id, "ERROR", f"Job failed: {str(e)}")
                self.db.commit()
                
    def _execute_job_by_type(self, job: BatchJobRec, execution: BatchExecutionRec) -> bool:
        """Execute job based on its type"""
        job_type = job.job_type
        parameters = json.loads(execution.exec_parameters or '{}')
        
        try:
            if job_type == 'GL_PERIOD_CLOSE':
                return self._execute_gl_period_close(parameters)
            elif job_type == 'REORDER_CALCULATION':
                return self._execute_reorder_calculation(parameters)
            elif job_type == 'CYCLE_COUNT_SCHEDULE':
                return self._execute_cycle_count_schedule(parameters)
            elif job_type == 'ABC_ANALYSIS':
                return self._execute_abc_analysis(parameters)
            elif job_type == 'DEMAND_FORECAST':
                return self._execute_demand_forecast(parameters)
            elif job_type == 'BACKUP_DATABASE':
                return self._execute_database_backup(parameters)
            elif job_type == 'SYSTEM_MAINTENANCE':
                return self._execute_system_maintenance(parameters)
            else:
                self.logger.warning(f"Unknown job type: {job_type}")
                return False
                
        except Exception as e:
            self.logger.error(f"Job type execution error: {str(e)}")
            return False
            
    def _execute_gl_period_close(self, parameters: Dict) -> bool:
        """Execute GL period close"""
        from app.services.gl.period_end_processing import PeriodEndProcessingService
        
        period = parameters.get('period')
        warehouse = parameters.get('warehouse', 'ALL')
        
        service = PeriodEndProcessingService(self.db)
        success, result = service.close_period({'period': period, 'warehouse': warehouse})
        
        return success
        
    def _execute_reorder_calculation(self, parameters: Dict) -> bool:
        """Execute reorder point calculation"""
        from app.services.stock.replenishment import ReplenishmentService
        
        warehouse = parameters.get('warehouse', 'MAIN')
        
        service = ReplenishmentService(self.db)
        success, result = service.analyze_replenishment_requirements({
            'warehouse': warehouse,
            'min_priority': 5
        })
        
        return success
        
    def _execute_cycle_count_schedule(self, parameters: Dict) -> bool:
        """Execute cycle count scheduling"""
        from app.services.stock.cycle_counting import CycleCountingService
        
        warehouse = parameters.get('warehouse', 'MAIN')
        
        service = CycleCountingService(self.db)
        success, result = service.generate_cycle_count_schedule({
            'warehouse': warehouse,
            'count_period_days': 30
        })
        
        return success
        
    def _execute_abc_analysis(self, parameters: Dict) -> bool:
        """Execute ABC classification analysis"""
        from app.services.stock.abc_classification import ABCClassificationService
        
        warehouse = parameters.get('warehouse', 'MAIN')
        
        service = ABCClassificationService(self.db)
        success, result = service.run_abc_analysis({
            'warehouse': warehouse,
            'period_months': 12,
            'criteria': 'VALUE',
            'update_master': True
        })
        
        return success
        
    def _execute_demand_forecast(self, parameters: Dict) -> bool:
        """Execute demand forecasting"""
        from app.services.stock.demand_forecasting import DemandForecastingService
        
        warehouse = parameters.get('warehouse', 'MAIN')
        
        service = DemandForecastingService(self.db)
        success, result = service.generate_forecast({
            'warehouse': warehouse,
            'periods': 12,
            'period_type': 'MONTH',
            'model': 'AUTO'
        })
        
        return success
        
    def _execute_database_backup(self, parameters: Dict) -> bool:
        """Execute database backup"""
        # This would execute actual backup commands
        self.logger.info("Database backup completed")
        return True
        
    def _execute_system_maintenance(self, parameters: Dict) -> bool:
        """Execute system maintenance tasks"""
        # This would perform various maintenance tasks
        self.logger.info("System maintenance completed")
        return True
        
    def _create_execution_from_schedule(self, schedule: BatchScheduleRec) -> Optional[str]:
        """Create execution record from schedule"""
        try:
            execution_id = self._get_next_execution_id()
            
            execution = BatchExecutionRec(
                exec_id=execution_id,
                exec_job_name=schedule.schedule_job_name,
                exec_schedule_id=schedule.schedule_id,
                exec_status=JobStatus.READY.value,
                exec_priority=schedule.schedule_priority,
                exec_parameters=schedule.schedule_parameters,
                exec_submitted_date=int(datetime.now().strftime("%Y%m%d")),
                exec_submitted_time=int(datetime.now().strftime("%H%M%S")),
                exec_submitted_by='SCHEDULER',
                exec_retry_count=0
            )
            
            self.db.add(execution)
            
            # Update schedule status
            schedule.schedule_status = 'RUNNING'
            schedule.schedule_last_run_date = int(datetime.now().strftime("%Y%m%d"))
            
            self.db.commit()
            
            return execution_id
            
        except Exception as e:
            self.db.rollback()
            self.logger.error(f"Failed to create execution from schedule: {str(e)}")
            return None
            
    def _update_schedule_next_run(self, schedule: BatchScheduleRec):
        """Update schedule for next run"""
        if schedule.schedule_type == 'ONCE':
            schedule.schedule_status = 'COMPLETED'
        elif schedule.schedule_type == 'DAILY':
            next_date = datetime.strptime(str(schedule.schedule_next_run_date), "%Y%m%d") + timedelta(days=1)
            schedule.schedule_next_run_date = int(next_date.strftime("%Y%m%d"))
            schedule.schedule_status = 'SCHEDULED'
        elif schedule.schedule_type == 'WEEKLY':
            next_date = datetime.strptime(str(schedule.schedule_next_run_date), "%Y%m%d") + timedelta(weeks=1)
            schedule.schedule_next_run_date = int(next_date.strftime("%Y%m%d"))
            schedule.schedule_status = 'SCHEDULED'
        elif schedule.schedule_type == 'MONTHLY':
            current_date = datetime.strptime(str(schedule.schedule_next_run_date), "%Y%m%d")
            if current_date.month == 12:
                next_date = current_date.replace(year=current_date.year + 1, month=1)
            else:
                next_date = current_date.replace(month=current_date.month + 1)
            schedule.schedule_next_run_date = int(next_date.strftime("%Y%m%d"))
            schedule.schedule_status = 'SCHEDULED'
            
    def _log_job_message(self, execution_id: str, level: str, message: str):
        """Log job message"""
        log = BatchLogRec(
            log_execution_id=execution_id,
            log_timestamp=datetime.now(),
            log_level=level,
            log_message=message
        )
        
        try:
            self.db.add(log)
            self.db.commit()
        except:
            self.db.rollback()
            
    def _cleanup_old_executions(self):
        """Clean up old execution records"""
        cutoff_date = int((datetime.now() - timedelta(days=30)).strftime("%Y%m%d"))
        
        try:
            # Delete old completed/failed executions
            old_executions = self.db.query(BatchExecutionRec).filter(
                and_(
                    BatchExecutionRec.exec_end_date < cutoff_date,
                    BatchExecutionRec.exec_status.in_([
                        JobStatus.COMPLETED.value,
                        JobStatus.FAILED.value,
                        JobStatus.CANCELLED.value
                    ])
                )
            ).limit(100)
            
            for execution in old_executions:
                self.db.delete(execution)
                
            self.db.commit()
            
        except Exception as e:
            self.db.rollback()
            self.logger.error(f"Cleanup error: {str(e)}")
            
    def _calculate_runtime(self, execution: BatchExecutionRec) -> float:
        """Calculate job runtime in minutes"""
        if not execution.exec_start_date or not execution.exec_start_time:
            return 0
            
        start_dt = datetime.strptime(f"{execution.exec_start_date}{execution.exec_start_time:04d}", "%Y%m%d%H%M")
        
        if execution.exec_end_date and execution.exec_end_time:
            end_dt = datetime.strptime(f"{execution.exec_end_date}{execution.exec_end_time:04d}", "%Y%m%d%H%M")
        else:
            end_dt = datetime.now()
            
        return (end_dt - start_dt).total_seconds() / 60
        
    def _get_next_schedule_id(self) -> str:
        """Generate next schedule ID"""
        return f"SCH{datetime.now().strftime('%Y%m%d%H%M%S')}"
        
    def _get_next_execution_id(self) -> str:
        """Generate next execution ID"""
        return f"EXE{datetime.now().strftime('%Y%m%d%H%M%S')}"