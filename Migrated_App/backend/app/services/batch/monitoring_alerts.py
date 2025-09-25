"""
Batch Monitoring and Alerting Service - Phase 7 migration
Monitors batch job performance and sends alerts for failures
"""
from typing import List, Optional, Dict, Tuple, Any
from datetime import datetime, timedelta
from decimal import Decimal
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func, desc, text
import smtplib
from email.mime.text import MIMEText
from email.mime.multipart import MIMEMultipart
import json
import logging

from app.services.file_handlers.system_handler import SystemFileHandler
from app.models.batch import (
    BatchJobRec, BatchExecutionRec, BatchLogRec, 
    BatchAlertRec, BatchMetricsRec, BatchThresholdRec
)
from app.core.security import log_user_action
from app.models.auth import User


class BatchMonitoringService:
    """
    Batch Monitoring and Alerting Service
    Monitors job performance, creates alerts, and sends notifications
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.system_handler = SystemFileHandler(db)
        self.logger = logging.getLogger(__name__)
        
    def monitor_batch_jobs(self, monitoring_options: Optional[Dict] = None) -> Dict:
        """
        Monitor all batch jobs and create alerts
        Returns monitoring summary
        """
        options = monitoring_options or {}
        check_failures = options.get('check_failures', True)
        check_performance = options.get('check_performance', True)
        check_schedules = options.get('check_schedules', True)
        send_alerts = options.get('send_alerts', True)
        
        try:
            monitoring_summary = {
                'check_time': datetime.now().isoformat(),
                'alerts_created': 0,
                'notifications_sent': 0,
                'jobs_monitored': 0,
                'issues_found': []
            }
            
            # Check for job failures
            if check_failures:
                failure_alerts = self._check_job_failures()
                monitoring_summary['alerts_created'] += len(failure_alerts)
                monitoring_summary['issues_found'].extend(failure_alerts)
                
            # Check performance issues
            if check_performance:
                performance_alerts = self._check_performance_issues()
                monitoring_summary['alerts_created'] += len(performance_alerts)
                monitoring_summary['issues_found'].extend(performance_alerts)
                
            # Check schedule adherence
            if check_schedules:
                schedule_alerts = self._check_schedule_adherence()
                monitoring_summary['alerts_created'] += len(schedule_alerts)
                monitoring_summary['issues_found'].extend(schedule_alerts)
                
            # Check disk space and system resources
            resource_alerts = self._check_system_resources()
            monitoring_summary['alerts_created'] += len(resource_alerts)
            monitoring_summary['issues_found'].extend(resource_alerts)
            
            # Send notifications if requested
            if send_alerts and monitoring_summary['alerts_created'] > 0:
                notifications_sent = self._send_alert_notifications(monitoring_summary['issues_found'])
                monitoring_summary['notifications_sent'] = notifications_sent
                
            # Update monitoring metrics
            self._update_monitoring_metrics(monitoring_summary)
            
            return monitoring_summary
            
        except Exception as e:
            self.logger.error(f"Monitoring error: {str(e)}")
            return {"error": str(e)}
            
    def _check_job_failures(self) -> List[Dict]:
        """Check for job failures in last 24 hours"""
        cutoff_time = datetime.now() - timedelta(hours=24)
        cutoff_date = int(cutoff_time.strftime("%Y%m%d"))
        cutoff_time_int = int(cutoff_time.strftime("%H%M%S"))
        
        failed_jobs = self.db.query(BatchExecutionRec).filter(
            and_(
                BatchExecutionRec.exec_status == 'FAILED',
                or_(
                    BatchExecutionRec.exec_end_date > cutoff_date,
                    and_(
                        BatchExecutionRec.exec_end_date == cutoff_date,
                        BatchExecutionRec.exec_end_time >= cutoff_time_int
                    )
                )
            )
        ).all()
        
        alerts = []
        for job in failed_jobs:
            # Check if we already alerted for this failure
            existing_alert = self.db.query(BatchAlertRec).filter(
                and_(
                    BatchAlertRec.alert_execution_id == job.exec_id,
                    BatchAlertRec.alert_type == 'FAILURE'
                )
            ).first()
            
            if not existing_alert:
                alert = self._create_failure_alert(job)
                alerts.append(alert)
                
        return alerts
        
    def _check_performance_issues(self) -> List[Dict]:
        """Check for performance degradation"""
        alerts = []
        
        # Get average runtimes for jobs over last 30 days
        thirty_days_ago = int((datetime.now() - timedelta(days=30)).strftime("%Y%m%d"))
        
        job_performance = self.db.execute(text("""
            SELECT 
                e.exec_job_name,
                AVG(
                    CASE 
                        WHEN e.exec_end_date IS NOT NULL AND e.exec_start_date IS NOT NULL
                        THEN (e.exec_end_date - e.exec_start_date) * 1440 + 
                             (e.exec_end_time - e.exec_start_time) / 100 * 60
                        ELSE 0
                    END
                ) as avg_runtime_minutes,
                COUNT(*) as execution_count,
                MAX(
                    CASE 
                        WHEN e.exec_end_date IS NOT NULL AND e.exec_start_date IS NOT NULL
                        THEN (e.exec_end_date - e.exec_start_date) * 1440 + 
                             (e.exec_end_time - e.exec_start_time) / 100 * 60
                        ELSE 0
                    END
                ) as max_runtime_minutes
            FROM batch_execution_rec e
            WHERE e.exec_status = 'COMPLETED'
            AND e.exec_end_date >= :since_date
            GROUP BY e.exec_job_name
            HAVING COUNT(*) >= 3
        """), {'since_date': thirty_days_ago}).fetchall()
        
        for job_perf in job_performance:
            job_name = job_perf.exec_job_name
            avg_runtime = job_perf.avg_runtime_minutes
            max_runtime = job_perf.max_runtime_minutes
            
            # Get threshold for this job
            threshold = self._get_performance_threshold(job_name)
            
            # Check if recent executions exceed threshold
            if max_runtime > threshold * 1.5:  # 50% over threshold
                alert = self._create_performance_alert(job_name, max_runtime, threshold)
                alerts.append(alert)
                
        return alerts
        
    def _check_schedule_adherence(self) -> List[Dict]:
        """Check for jobs that missed their scheduled time"""
        current_time = datetime.now()
        current_date = int(current_time.strftime("%Y%m%d"))
        current_time_int = int(current_time.strftime("%H%M"))
        
        # Find schedules that should have run but didn't
        missed_schedules = self.db.query(BatchScheduleRec).filter(
            and_(
                BatchScheduleRec.schedule_status == 'SCHEDULED',
                BatchScheduleRec.schedule_next_run_date <= current_date,
                or_(
                    BatchScheduleRec.schedule_next_run_date < current_date,
                    BatchScheduleRec.schedule_run_time < current_time_int - 60  # 1 hour grace period
                )
            )
        ).all()
        
        alerts = []
        for schedule in missed_schedules:
            # Check if execution exists
            execution = self.db.query(BatchExecutionRec).filter(
                and_(
                    BatchExecutionRec.exec_schedule_id == schedule.schedule_id,
                    BatchExecutionRec.exec_submitted_date == current_date
                )
            ).first()
            
            if not execution:
                alert = self._create_schedule_alert(schedule)
                alerts.append(alert)
                
        return alerts
        
    def _check_system_resources(self) -> List[Dict]:
        """Check system resources (disk space, connections, etc.)"""
        alerts = []
        
        # Check database connections
        connection_count = self.db.execute(text("""
            SELECT COUNT(*) as active_connections 
            FROM pg_stat_activity 
            WHERE state = 'active'
        """)).scalar()
        
        if connection_count > 80:  # Threshold
            alert = {
                'type': 'RESOURCE',
                'severity': 'WARNING',
                'message': f'High database connection count: {connection_count}',
                'details': {'connection_count': connection_count}
            }
            self._create_resource_alert('DB_CONNECTIONS', alert)
            alerts.append(alert)
            
        # Check long-running queries
        long_queries = self.db.execute(text("""
            SELECT query, state, now() - query_start as duration
            FROM pg_stat_activity
            WHERE state = 'active'
            AND now() - query_start > interval '30 minutes'
            AND query NOT LIKE '%pg_stat_activity%'
        """)).fetchall()
        
        if long_queries:
            alert = {
                'type': 'RESOURCE',
                'severity': 'WARNING',
                'message': f'{len(long_queries)} long-running queries detected',
                'details': {'query_count': len(long_queries)}
            }
            self._create_resource_alert('LONG_QUERIES', alert)
            alerts.append(alert)
            
        # Check for deadlocks
        deadlock_count = self.db.execute(text("""
            SELECT COUNT(*) as deadlocks
            FROM pg_stat_database
            WHERE datname = current_database()
            AND deadlocks > 0
        """)).scalar()
        
        if deadlock_count and deadlock_count > 0:
            alert = {
                'type': 'RESOURCE',
                'severity': 'ERROR',
                'message': f'Database deadlocks detected: {deadlock_count}',
                'details': {'deadlock_count': deadlock_count}
            }
            self._create_resource_alert('DEADLOCKS', alert)
            alerts.append(alert)
            
        return alerts
        
    def _create_failure_alert(self, execution: BatchExecutionRec) -> Dict:
        """Create failure alert"""
        alert_data = {
            'type': 'FAILURE',
            'severity': 'ERROR',
            'job_name': execution.exec_job_name,
            'execution_id': execution.exec_id,
            'message': f'Job {execution.exec_job_name} failed',
            'details': {
                'error_message': execution.exec_error_message,
                'return_code': execution.exec_return_code,
                'end_time': f"{execution.exec_end_date}{execution.exec_end_time:06d}" if execution.exec_end_time else None
            }
        }
        
        # Create alert record
        alert = BatchAlertRec(
            alert_type='FAILURE',
            alert_severity='ERROR',
            alert_job_name=execution.exec_job_name,
            alert_execution_id=execution.exec_id,
            alert_message=alert_data['message'],
            alert_details=json.dumps(alert_data['details']),
            alert_created_date=int(datetime.now().strftime("%Y%m%d")),
            alert_created_time=int(datetime.now().strftime("%H%M%S")),
            alert_status='ACTIVE',
            alert_notified='N'
        )
        
        self.db.add(alert)
        self.db.commit()
        
        return alert_data
        
    def _create_performance_alert(self, job_name: str, runtime: float, threshold: float) -> Dict:
        """Create performance alert"""
        alert_data = {
            'type': 'PERFORMANCE',
            'severity': 'WARNING',
            'job_name': job_name,
            'message': f'Job {job_name} exceeded performance threshold',
            'details': {
                'runtime_minutes': runtime,
                'threshold_minutes': threshold,
                'variance_percent': ((runtime - threshold) / threshold * 100)
            }
        }
        
        alert = BatchAlertRec(
            alert_type='PERFORMANCE',
            alert_severity='WARNING',
            alert_job_name=job_name,
            alert_message=alert_data['message'],
            alert_details=json.dumps(alert_data['details']),
            alert_created_date=int(datetime.now().strftime("%Y%m%d")),
            alert_created_time=int(datetime.now().strftime("%H%M%S")),
            alert_status='ACTIVE',
            alert_notified='N'
        )
        
        self.db.add(alert)
        self.db.commit()
        
        return alert_data
        
    def _create_schedule_alert(self, schedule: BatchScheduleRec) -> Dict:
        """Create schedule adherence alert"""
        alert_data = {
            'type': 'SCHEDULE',
            'severity': 'WARNING',
            'job_name': schedule.schedule_job_name,
            'schedule_id': schedule.schedule_id,
            'message': f'Job {schedule.schedule_job_name} missed scheduled time',
            'details': {
                'scheduled_date': schedule.schedule_next_run_date,
                'scheduled_time': f"{schedule.schedule_run_time:04d}",
                'delay_minutes': self._calculate_schedule_delay(schedule)
            }
        }
        
        alert = BatchAlertRec(
            alert_type='SCHEDULE',
            alert_severity='WARNING',
            alert_job_name=schedule.schedule_job_name,
            alert_schedule_id=schedule.schedule_id,
            alert_message=alert_data['message'],
            alert_details=json.dumps(alert_data['details']),
            alert_created_date=int(datetime.now().strftime("%Y%m%d")),
            alert_created_time=int(datetime.now().strftime("%H%M%S")),
            alert_status='ACTIVE',
            alert_notified='N'
        )
        
        self.db.add(alert)
        self.db.commit()
        
        return alert_data
        
    def _create_resource_alert(self, resource_type: str, alert_data: Dict):
        """Create system resource alert"""
        alert = BatchAlertRec(
            alert_type='RESOURCE',
            alert_severity=alert_data['severity'],
            alert_resource_type=resource_type,
            alert_message=alert_data['message'],
            alert_details=json.dumps(alert_data['details']),
            alert_created_date=int(datetime.now().strftime("%Y%m%d")),
            alert_created_time=int(datetime.now().strftime("%H%M%S")),
            alert_status='ACTIVE',
            alert_notified='N'
        )
        
        self.db.add(alert)
        self.db.commit()
        
    def _send_alert_notifications(self, alerts: List[Dict]) -> int:
        """Send email notifications for alerts"""
        try:
            # Get notification recipients
            recipients = self._get_alert_recipients()
            if not recipients:
                return 0
                
            notifications_sent = 0
            
            # Group alerts by severity
            error_alerts = [a for a in alerts if a.get('severity') == 'ERROR']
            warning_alerts = [a for a in alerts if a.get('severity') == 'WARNING']
            
            # Send error alerts immediately
            if error_alerts:
                subject = f"ACAS Batch System - {len(error_alerts)} Critical Alert(s)"
                body = self._format_alert_email(error_alerts, 'ERROR')
                
                if self._send_email(recipients, subject, body):
                    notifications_sent += len(error_alerts)
                    self._mark_alerts_notified(error_alerts)
                    
            # Send warning alerts (may be batched)
            if warning_alerts:
                subject = f"ACAS Batch System - {len(warning_alerts)} Warning Alert(s)"
                body = self._format_alert_email(warning_alerts, 'WARNING')
                
                if self._send_email(recipients, subject, body):
                    notifications_sent += len(warning_alerts)
                    self._mark_alerts_notified(warning_alerts)
                    
            return notifications_sent
            
        except Exception as e:
            self.logger.error(f"Failed to send notifications: {str(e)}")
            return 0
            
    def _get_alert_recipients(self) -> List[str]:
        """Get list of email addresses for alert notifications"""
        # This would typically come from system configuration
        # For now, return default recipients
        return [
            'batch.admin@company.com',
            'it.support@company.com'
        ]
        
    def _format_alert_email(self, alerts: List[Dict], severity: str) -> str:
        """Format alert email body"""
        email_body = f"""
ACAS Batch System Alert Report
Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}
Severity: {severity}

{"="*50}

"""
        
        for alert in alerts:
            email_body += f"""
Alert Type: {alert.get('type', 'UNKNOWN')}
Job Name: {alert.get('job_name', 'N/A')}
Message: {alert.get('message', 'No message')}
"""
            
            if alert.get('details'):
                email_body += f"Details: {json.dumps(alert['details'], indent=2)}\n"
                
            email_body += "-" * 30 + "\n"
            
        email_body += f"""

{"="*50}

This is an automated message from the ACAS Batch Monitoring System.
Please investigate these alerts and take appropriate action.

For support, contact IT Help Desk.
"""
        
        return email_body
        
    def _send_email(self, recipients: List[str], subject: str, body: str) -> bool:
        """Send email notification"""
        try:
            # This would use actual SMTP configuration
            # For now, just log the email that would be sent
            self.logger.info(f"Email notification: {subject}")
            self.logger.info(f"Recipients: {', '.join(recipients)}")
            self.logger.info(f"Body: {body[:200]}...")  # Log first 200 chars
            
            # In production, implement actual email sending:
            # smtp_server = smtplib.SMTP('smtp.company.com', 587)
            # smtp_server.starttls()
            # smtp_server.login('batch.system@company.com', 'password')
            # 
            # msg = MIMEMultipart()
            # msg['From'] = 'batch.system@company.com'
            # msg['To'] = ', '.join(recipients)
            # msg['Subject'] = subject
            # msg.attach(MIMEText(body, 'plain'))
            # 
            # smtp_server.send_message(msg)
            # smtp_server.quit()
            
            return True
            
        except Exception as e:
            self.logger.error(f"Email send error: {str(e)}")
            return False
            
    def _mark_alerts_notified(self, alerts: List[Dict]):
        """Mark alerts as notified"""
        try:
            for alert_data in alerts:
                if alert_data.get('job_name'):
                    # Find and update the alert record
                    alert = self.db.query(BatchAlertRec).filter(
                        and_(
                            BatchAlertRec.alert_job_name == alert_data['job_name'],
                            BatchAlertRec.alert_type == alert_data['type'],
                            BatchAlertRec.alert_notified == 'N'
                        )
                    ).first()
                    
                    if alert:
                        alert.alert_notified = 'Y'
                        alert.alert_notified_date = int(datetime.now().strftime("%Y%m%d"))
                        alert.alert_notified_time = int(datetime.now().strftime("%H%M%S"))
                        
            self.db.commit()
            
        except Exception as e:
            self.logger.error(f"Failed to mark alerts as notified: {str(e)}")
            self.db.rollback()
            
    def _get_performance_threshold(self, job_name: str) -> float:
        """Get performance threshold for job"""
        threshold_rec = self.db.query(BatchThresholdRec).filter(
            BatchThresholdRec.threshold_job_name == job_name
        ).first()
        
        if threshold_rec:
            return float(threshold_rec.threshold_max_runtime_minutes)
        else:
            # Return default thresholds based on job type
            default_thresholds = {
                'GL_PERIOD_CLOSE': 120,
                'SL_AGED_DEBTORS': 60,
                'PL_PAYMENT_RUN': 90,
                'ST_REORDER_CALCULATION': 45,
                'SYS_DATABASE_BACKUP': 180
            }
            
            for pattern, threshold in default_thresholds.items():
                if pattern in job_name:
                    return threshold
                    
            return 60  # Default 1 hour
            
    def _calculate_schedule_delay(self, schedule: BatchScheduleRec) -> int:
        """Calculate how many minutes late a scheduled job is"""
        current_time = datetime.now()
        scheduled_datetime = datetime.strptime(
            f"{schedule.schedule_next_run_date}{schedule.schedule_run_time:04d}",
            "%Y%m%d%H%M"
        )
        
        delay = current_time - scheduled_datetime
        return max(0, int(delay.total_seconds() / 60))
        
    def _update_monitoring_metrics(self, summary: Dict):
        """Update monitoring metrics"""
        try:
            metrics = BatchMetricsRec(
                metrics_date=int(datetime.now().strftime("%Y%m%d")),
                metrics_time=int(datetime.now().strftime("%H%M%S")),
                metrics_alerts_created=summary['alerts_created'],
                metrics_notifications_sent=summary['notifications_sent'],
                metrics_jobs_monitored=summary['jobs_monitored'],
                metrics_issues_found=len(summary['issues_found'])
            )
            
            self.db.add(metrics)
            self.db.commit()
            
        except Exception as e:
            self.logger.error(f"Failed to update metrics: {str(e)}")
            self.db.rollback()
            
    def get_alert_dashboard(self, dashboard_filters: Optional[Dict] = None) -> Dict:
        """Get alert dashboard data"""
        try:
            filters = dashboard_filters or {}
            date_from = filters.get('date_from', int((datetime.now() - timedelta(days=7)).strftime("%Y%m%d")))
            date_to = filters.get('date_to', int(datetime.now().strftime("%Y%m%d")))
            severity = filters.get('severity')
            
            # Build query
            alert_query = self.db.query(BatchAlertRec).filter(
                and_(
                    BatchAlertRec.alert_created_date >= date_from,
                    BatchAlertRec.alert_created_date <= date_to
                )
            )
            
            if severity:
                alert_query = alert_query.filter(BatchAlertRec.alert_severity == severity)
                
            alerts = alert_query.order_by(desc(BatchAlertRec.alert_created_date),
                                        desc(BatchAlertRec.alert_created_time)).all()
            
            # Calculate summary statistics
            total_alerts = len(alerts)
            active_alerts = len([a for a in alerts if a.alert_status == 'ACTIVE'])
            error_alerts = len([a for a in alerts if a.alert_severity == 'ERROR'])
            warning_alerts = len([a for a in alerts if a.alert_severity == 'WARNING'])
            
            # Alert trends
            alert_trends = {}
            for alert in alerts:
                alert_date = str(alert.alert_created_date)
                alert_trends[alert_date] = alert_trends.get(alert_date, 0) + 1
                
            # Top failing jobs
            job_failures = {}
            for alert in alerts:
                if alert.alert_job_name and alert.alert_type == 'FAILURE':
                    job_failures[alert.alert_job_name] = job_failures.get(alert.alert_job_name, 0) + 1
                    
            top_failing_jobs = sorted(job_failures.items(), key=lambda x: x[1], reverse=True)[:5]
            
            return {
                'period': {
                    'date_from': date_from,
                    'date_to': date_to,
                    'severity_filter': severity
                },
                'summary': {
                    'total_alerts': total_alerts,
                    'active_alerts': active_alerts,
                    'error_alerts': error_alerts,
                    'warning_alerts': warning_alerts,
                    'resolution_rate': ((total_alerts - active_alerts) / total_alerts * 100) if total_alerts > 0 else 0
                },
                'recent_alerts': [
                    {
                        'id': alert.alert_id,
                        'type': alert.alert_type,
                        'severity': alert.alert_severity,
                        'job_name': alert.alert_job_name,
                        'message': alert.alert_message,
                        'created_date': alert.alert_created_date,
                        'created_time': f"{alert.alert_created_time:06d}",
                        'status': alert.alert_status,
                        'notified': alert.alert_notified == 'Y'
                    }
                    for alert in alerts[:20]  # Latest 20 alerts
                ],
                'alert_trends': alert_trends,
                'top_failing_jobs': [
                    {'job_name': job, 'failure_count': count}
                    for job, count in top_failing_jobs
                ]
            }
            
        except Exception as e:
            return {"error": str(e)}
            
    def acknowledge_alert(self, alert_id: int, acknowledged_by: str, notes: str = '') -> Tuple[bool, Optional[str]]:
        """Acknowledge an alert"""
        try:
            alert = self.db.query(BatchAlertRec).filter(
                BatchAlertRec.alert_id == alert_id
            ).first()
            
            if not alert:
                return False, "Alert not found"
                
            if alert.alert_status != 'ACTIVE':
                return False, f"Cannot acknowledge alert with status: {alert.alert_status}"
                
            alert.alert_status = 'ACKNOWLEDGED'
            alert.alert_acknowledged_by = acknowledged_by
            alert.alert_acknowledged_date = int(datetime.now().strftime("%Y%m%d"))
            alert.alert_acknowledged_time = int(datetime.now().strftime("%H%M%S"))
            alert.alert_notes = notes
            
            self.db.commit()
            
            return True, "Alert acknowledged successfully"
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)