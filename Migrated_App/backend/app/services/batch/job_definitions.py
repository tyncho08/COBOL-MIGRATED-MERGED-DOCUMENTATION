"""
Batch Job Definitions Service - Phase 7 migration
Pre-defined batch jobs for all modules with parameters and dependencies
"""
from typing import List, Optional, Dict, Tuple
from datetime import datetime, timedelta
from sqlalchemy.orm import Session

from app.services.batch.job_scheduler import JobSchedulerService
from app.models.auth import User


class BatchJobDefinitionsService:
    """
    Batch Job Definitions Service
    Creates and manages pre-defined batch jobs for all modules
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.scheduler_service = JobSchedulerService(db)
        
    def create_standard_batch_jobs(self) -> Tuple[bool, Optional[str]]:
        """
        Create all standard batch jobs for ACAS system
        Returns (success, error_message or summary)
        """
        try:
            jobs_created = 0
            
            # General Ledger batch jobs
            gl_jobs = self._create_gl_batch_jobs()
            jobs_created += len(gl_jobs)
            
            # Sales Ledger batch jobs
            sl_jobs = self._create_sl_batch_jobs()
            jobs_created += len(sl_jobs)
            
            # Purchase Ledger batch jobs
            pl_jobs = self._create_pl_batch_jobs()
            jobs_created += len(pl_jobs)
            
            # Stock Control batch jobs
            st_jobs = self._create_stock_batch_jobs()
            jobs_created += len(st_jobs)
            
            # System maintenance jobs
            sys_jobs = self._create_system_batch_jobs()
            jobs_created += len(sys_jobs)
            
            return True, {
                'jobs_created': jobs_created,
                'gl_jobs': len(gl_jobs),
                'sl_jobs': len(sl_jobs),
                'pl_jobs': len(pl_jobs),
                'stock_jobs': len(st_jobs),
                'system_jobs': len(sys_jobs)
            }
            
        except Exception as e:
            return False, str(e)
            
    def _create_gl_batch_jobs(self) -> List[str]:
        """Create General Ledger batch jobs"""
        gl_jobs = [
            {
                'job_name': 'GL_DAILY_CLOSE',
                'job_type': 'GL_PERIOD_CLOSE',
                'module': 'GL',
                'description': 'Daily General Ledger closing process',
                'parameters': {
                    'period': 'CURRENT',
                    'warehouse': 'ALL',
                    'run_reports': 'Y',
                    'validate_balances': 'Y'
                }
            },
            {
                'job_name': 'GL_PERIOD_CLOSE',
                'job_type': 'GL_PERIOD_CLOSE',
                'module': 'GL',
                'description': 'Monthly General Ledger period close',
                'parameters': {
                    'period': 'CURRENT',
                    'warehouse': 'ALL',
                    'create_accruals': 'Y',
                    'run_reports': 'Y',
                    'archive_data': 'Y'
                },
                'dependencies': ['GL_DAILY_CLOSE']
            },
            {
                'job_name': 'GL_YEAR_END_CLOSE',
                'job_type': 'GL_YEAR_END',
                'module': 'GL',
                'description': 'Annual General Ledger year-end close',
                'parameters': {
                    'year': 'CURRENT',
                    'create_closing_entries': 'Y',
                    'rollforward_balances': 'Y',
                    'archive_transactions': 'Y'
                },
                'dependencies': ['GL_PERIOD_CLOSE']
            },
            {
                'job_name': 'GL_TRIAL_BALANCE',
                'job_type': 'GL_REPORT',
                'module': 'GL',
                'description': 'Generate trial balance report',
                'parameters': {
                    'period': 'CURRENT',
                    'include_budget': 'Y',
                    'format': 'PDF',
                    'email_report': 'Y'
                }
            },
            {
                'job_name': 'GL_BUDGET_VARIANCE',
                'job_type': 'GL_BUDGET_ANALYSIS',
                'module': 'GL',
                'description': 'Budget vs Actual variance analysis',
                'parameters': {
                    'period': 'CURRENT',
                    'variance_threshold': '10',
                    'alert_managers': 'Y'
                }
            },
            {
                'job_name': 'GL_ACCOUNT_RECONCILIATION',
                'job_type': 'GL_RECONCILIATION',
                'module': 'GL',
                'description': 'Bank and control account reconciliation',
                'parameters': {
                    'reconcile_bank': 'Y',
                    'reconcile_controls': 'Y',
                    'auto_match': 'Y'
                }
            }
        ]
        
        created_jobs = []
        for job_data in gl_jobs:
            success, result = self.scheduler_service.create_batch_job(job_data)
            if success:
                created_jobs.append(job_data['job_name'])
                
        return created_jobs
        
    def _create_sl_batch_jobs(self) -> List[str]:
        """Create Sales Ledger batch jobs"""
        sl_jobs = [
            {
                'job_name': 'SL_AGED_DEBTORS',
                'job_type': 'SL_AGING',
                'module': 'SL',
                'description': 'Calculate aged debtors analysis',
                'parameters': {
                    'aging_buckets': '30,60,90,120',
                    'include_credit_notes': 'Y',
                    'currency': 'ALL'
                }
            },
            {
                'job_name': 'SL_STATEMENT_GENERATION',
                'job_type': 'SL_STATEMENTS',
                'module': 'SL',
                'description': 'Generate customer statements',
                'parameters': {
                    'statement_type': 'MONTHLY',
                    'email_statements': 'Y',
                    'print_statements': 'Y',
                    'customer_filter': 'ACTIVE'
                }
            },
            {
                'job_name': 'SL_CREDIT_LIMIT_CHECK',
                'job_type': 'SL_CREDIT_CONTROL',
                'module': 'SL',
                'description': 'Check credit limits and create alerts',
                'parameters': {
                    'check_orders': 'Y',
                    'check_invoices': 'Y',
                    'alert_threshold': '90',
                    'auto_hold_orders': 'Y'
                }
            },
            {
                'job_name': 'SL_OVERDUE_REMINDERS',
                'job_type': 'SL_COLLECTIONS',
                'module': 'SL',
                'description': 'Generate overdue payment reminders',
                'parameters': {
                    'days_overdue': '30',
                    'reminder_type': 'EMAIL',
                    'escalation_levels': '3',
                    'exclude_disputes': 'Y'
                }
            },
            {
                'job_name': 'SL_COMMISSION_CALCULATION',
                'job_type': 'SL_COMMISSION',
                'module': 'SL',
                'description': 'Calculate sales commissions',
                'parameters': {
                    'period': 'CURRENT',
                    'commission_basis': 'INVOICES',
                    'include_returns': 'N'
                }
            },
            {
                'job_name': 'SL_VAT_RETURN',
                'job_type': 'SL_VAT',
                'module': 'SL',
                'description': 'Prepare VAT return data',
                'parameters': {
                    'period': 'QUARTER',
                    'vat_type': 'OUTPUT',
                    'include_eu_sales': 'Y'
                }
            }
        ]
        
        created_jobs = []
        for job_data in sl_jobs:
            success, result = self.scheduler_service.create_batch_job(job_data)
            if success:
                created_jobs.append(job_data['job_name'])
                
        return created_jobs
        
    def _create_pl_batch_jobs(self) -> List[str]:
        """Create Purchase Ledger batch jobs"""
        pl_jobs = [
            {
                'job_name': 'PL_AGED_CREDITORS',
                'job_type': 'PL_AGING',
                'module': 'PL',
                'description': 'Calculate aged creditors analysis',
                'parameters': {
                    'aging_buckets': '30,60,90,120',
                    'include_credit_notes': 'Y',
                    'currency': 'ALL'
                }
            },
            {
                'job_name': 'PL_PAYMENT_RUN',
                'job_type': 'PL_PAYMENTS',
                'module': 'PL',
                'description': 'Process supplier payments',
                'parameters': {
                    'payment_method': 'BACS',
                    'payment_date': 'TODAY+2',
                    'min_payment': '10.00',
                    'settlement_discount': 'Y'
                }
            },
            {
                'job_name': 'PL_REMITTANCE_ADVICE',
                'job_type': 'PL_REMITTANCE',
                'module': 'PL',
                'description': 'Generate remittance advices',
                'parameters': {
                    'payment_run': 'LATEST',
                    'email_remittances': 'Y',
                    'print_remittances': 'Y'
                },
                'dependencies': ['PL_PAYMENT_RUN']
            },
            {
                'job_name': 'PL_THREE_WAY_MATCHING',
                'job_type': 'PL_MATCHING',
                'module': 'PL',
                'description': 'Three-way matching (PO/GRN/Invoice)',
                'parameters': {
                    'tolerance_percentage': '5',
                    'tolerance_amount': '10.00',
                    'auto_match': 'Y',
                    'create_exceptions': 'Y'
                }
            },
            {
                'job_name': 'PL_ACCRUALS_CALCULATION',
                'job_type': 'PL_ACCRUALS',
                'module': 'PL',
                'description': 'Calculate purchase accruals',
                'parameters': {
                    'period': 'CURRENT',
                    'include_grn': 'Y',
                    'exclude_services': 'N'
                }
            },
            {
                'job_name': 'PL_VAT_RETURN',
                'job_type': 'PL_VAT',
                'module': 'PL',
                'description': 'Prepare input VAT return data',
                'parameters': {
                    'period': 'QUARTER',
                    'vat_type': 'INPUT',
                    'include_eu_purchases': 'Y'
                }
            }
        ]
        
        created_jobs = []
        for job_data in pl_jobs:
            success, result = self.scheduler_service.create_batch_job(job_data)
            if success:
                created_jobs.append(job_data['job_name'])
                
        return created_jobs
        
    def _create_stock_batch_jobs(self) -> List[str]:
        """Create Stock Control batch jobs"""
        stock_jobs = [
            {
                'job_name': 'ST_REORDER_CALCULATION',
                'job_type': 'REORDER_CALCULATION',
                'module': 'STOCK',
                'description': 'Calculate reorder points and quantities',
                'parameters': {
                    'warehouse': 'ALL',
                    'lead_time_days': '7',
                    'service_level': '95',
                    'create_orders': 'N'
                }
            },
            {
                'job_name': 'ST_ABC_ANALYSIS',
                'job_type': 'ABC_ANALYSIS',
                'module': 'STOCK',
                'description': 'ABC classification analysis',
                'parameters': {
                    'warehouse': 'ALL',
                    'period_months': '12',
                    'criteria': 'VALUE',
                    'update_master': 'Y'
                }
            },
            {
                'job_name': 'ST_CYCLE_COUNT_SCHEDULE',
                'job_type': 'CYCLE_COUNT_SCHEDULE',
                'module': 'STOCK',
                'description': 'Generate cycle count schedule',
                'parameters': {
                    'warehouse': 'ALL',
                    'count_period_days': '30',
                    'abc_weighted': 'Y',
                    'exclude_new_stock': 'Y'
                }
            },
            {
                'job_name': 'ST_DEAD_STOCK_ANALYSIS',
                'job_type': 'DEAD_STOCK_ANALYSIS',
                'module': 'STOCK',
                'description': 'Identify dead and slow-moving stock',
                'parameters': {
                    'no_movement_days': '180',
                    'slow_movement_days': '90',
                    'exclude_new_stock': 'Y',
                    'create_disposition': 'Y'
                }
            },
            {
                'job_name': 'ST_STOCK_VALUATION',
                'job_type': 'STOCK_VALUATION',
                'module': 'STOCK',
                'description': 'Stock valuation and costing update',
                'parameters': {
                    'valuation_method': 'AVERAGE',
                    'update_costs': 'Y',
                    'create_variances': 'Y'
                }
            },
            {
                'job_name': 'ST_DEMAND_FORECAST',
                'job_type': 'DEMAND_FORECAST',
                'module': 'STOCK',
                'description': 'Generate demand forecasts',
                'parameters': {
                    'warehouse': 'ALL',
                    'periods': '12',
                    'period_type': 'MONTH',
                    'model': 'AUTO'
                }
            },
            {
                'job_name': 'ST_LOCATION_REPLENISHMENT',
                'job_type': 'REPLENISHMENT',
                'module': 'STOCK',
                'description': 'Generate replenishment tasks',
                'parameters': {
                    'warehouse': 'ALL',
                    'min_max_method': 'Y',
                    'create_tasks': 'Y',
                    'optimize_routes': 'Y'
                }
            },
            {
                'job_name': 'ST_EXPIRY_MONITORING',
                'job_type': 'EXPIRY_CHECK',
                'module': 'STOCK',
                'description': 'Monitor stock expiry dates',
                'parameters': {
                    'warning_days': '30',
                    'alert_days': '7',
                    'create_disposal': 'Y'
                }
            }
        ]
        
        created_jobs = []
        for job_data in stock_jobs:
            success, result = self.scheduler_service.create_batch_job(job_data)
            if success:
                created_jobs.append(job_data['job_name'])
                
        return created_jobs
        
    def _create_system_batch_jobs(self) -> List[str]:
        """Create system maintenance batch jobs"""
        system_jobs = [
            {
                'job_name': 'SYS_DATABASE_BACKUP',
                'job_type': 'BACKUP_DATABASE',
                'module': 'SYSTEM',
                'description': 'Database backup and maintenance',
                'parameters': {
                    'backup_type': 'FULL',
                    'compress': 'Y',
                    'verify_backup': 'Y',
                    'cleanup_old': 'Y'
                }
            },
            {
                'job_name': 'SYS_LOG_CLEANUP',
                'job_type': 'LOG_CLEANUP',
                'module': 'SYSTEM',
                'description': 'Clean up old log files and audit trails',
                'parameters': {
                    'retain_days': '90',
                    'archive_logs': 'Y',
                    'compress_archives': 'Y'
                }
            },
            {
                'job_name': 'SYS_ARCHIVE_DATA',
                'job_type': 'DATA_ARCHIVAL',
                'module': 'SYSTEM',
                'description': 'Archive old transactional data',
                'parameters': {
                    'archive_months': '24',
                    'archive_transactions': 'Y',
                    'archive_movements': 'Y',
                    'create_summary': 'Y'
                }
            },
            {
                'job_name': 'SYS_PERFORMANCE_STATS',
                'job_type': 'SYSTEM_STATS',
                'module': 'SYSTEM',
                'description': 'Collect system performance statistics',
                'parameters': {
                    'collect_db_stats': 'Y',
                    'collect_user_stats': 'Y',
                    'update_indexes': 'Y'
                }
            },
            {
                'job_name': 'SYS_HEALTH_CHECK',
                'job_type': 'SYSTEM_HEALTH',
                'module': 'SYSTEM',
                'description': 'System health check and monitoring',
                'parameters': {
                    'check_disk_space': 'Y',
                    'check_connections': 'Y',
                    'check_locks': 'Y',
                    'alert_threshold': '80'
                }
            },
            {
                'job_name': 'SYS_USER_SESSION_CLEANUP',
                'job_type': 'SESSION_CLEANUP',
                'module': 'SYSTEM',
                'description': 'Clean up expired user sessions',
                'parameters': {
                    'session_timeout': '24',
                    'cleanup_tokens': 'Y',
                    'cleanup_temp_data': 'Y'
                }
            }
        ]
        
        created_jobs = []
        for job_data in system_jobs:
            success, result = self.scheduler_service.create_batch_job(job_data)
            if success:
                created_jobs.append(job_data['job_name'])
                
        return created_jobs
        
    def create_default_schedules(self) -> Tuple[bool, Optional[str]]:
        """
        Create default schedules for standard batch jobs
        Returns (success, error_message or summary)
        """
        try:
            schedules_created = 0
            
            # Daily schedules
            daily_jobs = [
                ('GL_DAILY_CLOSE', '2300'),
                ('SL_CREDIT_LIMIT_CHECK', '0800'),
                ('PL_THREE_WAY_MATCHING', '0900'),
                ('ST_REORDER_CALCULATION', '0600'),
                ('ST_EXPIRY_MONITORING', '0700'),
                ('SYS_HEALTH_CHECK', '0500'),
                ('SYS_USER_SESSION_CLEANUP', '0200')
            ]
            
            for job_name, run_time in daily_jobs:
                schedule_data = {
                    'job_name': job_name,
                    'schedule_type': 'DAILY',
                    'run_time': run_time,
                    'priority': 10
                }
                
                success, result = self.scheduler_service.schedule_job(schedule_data)
                if success:
                    schedules_created += 1
                    
            # Weekly schedules
            weekly_jobs = [
                ('SL_AGED_DEBTORS', '0800'),
                ('PL_AGED_CREDITORS', '0900'),
                ('ST_ABC_ANALYSIS', '0600'),
                ('ST_CYCLE_COUNT_SCHEDULE', '0700'),
                ('ST_DEAD_STOCK_ANALYSIS', '1000'),
                ('SYS_DATABASE_BACKUP', '0100'),
                ('SYS_PERFORMANCE_STATS', '0500')
            ]
            
            # Schedule weekly jobs for Sundays
            sunday_date = self._get_next_sunday()
            
            for job_name, run_time in weekly_jobs:
                schedule_data = {
                    'job_name': job_name,
                    'schedule_type': 'WEEKLY',
                    'run_date': sunday_date,
                    'run_time': run_time,
                    'priority': 8
                }
                
                success, result = self.scheduler_service.schedule_job(schedule_data)
                if success:
                    schedules_created += 1
                    
            # Monthly schedules
            monthly_jobs = [
                ('GL_PERIOD_CLOSE', '2200'),
                ('SL_STATEMENT_GENERATION', '0800'),
                ('SL_COMMISSION_CALCULATION', '1000'),
                ('PL_PAYMENT_RUN', '1400'),
                ('PL_ACCRUALS_CALCULATION', '1600'),
                ('ST_STOCK_VALUATION', '0600'),
                ('ST_DEMAND_FORECAST', '0700'),
                ('SYS_ARCHIVE_DATA', '0200'),
                ('SYS_LOG_CLEANUP', '0300')
            ]
            
            # Schedule monthly jobs for last day of month
            month_end_date = self._get_month_end_date()
            
            for job_name, run_time in monthly_jobs:
                schedule_data = {
                    'job_name': job_name,
                    'schedule_type': 'MONTHLY',
                    'run_date': month_end_date,
                    'run_time': run_time,
                    'priority': 15
                }
                
                success, result = self.scheduler_service.schedule_job(schedule_data)
                if success:
                    schedules_created += 1
                    
            # Quarterly schedules
            quarterly_jobs = [
                ('SL_VAT_RETURN', '1000'),
                ('PL_VAT_RETURN', '1100')
            ]
            
            # Schedule quarterly jobs for quarter end
            quarter_end_date = self._get_quarter_end_date()
            
            for job_name, run_time in quarterly_jobs:
                schedule_data = {
                    'job_name': job_name,
                    'schedule_type': 'MONTHLY',  # Will be scheduled monthly but run quarterly
                    'run_date': quarter_end_date,
                    'run_time': run_time,
                    'priority': 20
                }
                
                success, result = self.scheduler_service.schedule_job(schedule_data)
                if success:
                    schedules_created += 1
                    
            return True, {
                'schedules_created': schedules_created,
                'daily_schedules': len(daily_jobs),
                'weekly_schedules': len(weekly_jobs),
                'monthly_schedules': len(monthly_jobs),
                'quarterly_schedules': len(quarterly_jobs)
            }
            
        except Exception as e:
            return False, str(e)
            
    def _get_next_sunday(self) -> int:
        """Get date of next Sunday"""
        today = datetime.now()
        days_ahead = 6 - today.weekday()  # Sunday is 6
        if days_ahead <= 0:
            days_ahead += 7
        sunday = today + timedelta(days=days_ahead)
        return int(sunday.strftime("%Y%m%d"))
        
    def _get_month_end_date(self) -> int:
        """Get last day of current month"""
        today = datetime.now()
        if today.month == 12:
            next_month = today.replace(year=today.year + 1, month=1, day=1)
        else:
            next_month = today.replace(month=today.month + 1, day=1)
        last_day = next_month - timedelta(days=1)
        return int(last_day.strftime("%Y%m%d"))
        
    def _get_quarter_end_date(self) -> int:
        """Get last day of current quarter"""
        today = datetime.now()
        quarter = (today.month - 1) // 3 + 1
        
        if quarter == 1:  # Q1 ends March 31
            quarter_end = today.replace(month=3, day=31)
        elif quarter == 2:  # Q2 ends June 30
            quarter_end = today.replace(month=6, day=30)
        elif quarter == 3:  # Q3 ends September 30
            quarter_end = today.replace(month=9, day=30)
        else:  # Q4 ends December 31
            quarter_end = today.replace(month=12, day=31)
            
        return int(quarter_end.strftime("%Y%m%d"))