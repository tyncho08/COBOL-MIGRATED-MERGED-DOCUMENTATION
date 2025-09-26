"""
Report Scheduler Service

Handles scheduling and automated generation of reports.
Manages recurring reports, email distribution, and report queues.
"""

import asyncio
from typing import Dict, List, Any, Optional, Callable
from datetime import datetime, timedelta
from dataclasses import dataclass, field
from enum import Enum
import logging
import json
from croniter import croniter

from app.schemas.reports import ReportRequest, ScheduledReport, ScheduleFrequency

logger = logging.getLogger(__name__)


class ScheduleStatus(Enum):
    ACTIVE = "active"
    PAUSED = "paused"
    COMPLETED = "completed"
    FAILED = "failed"


@dataclass
class ScheduledReportJob:
    """Represents a scheduled report job"""
    job_id: str
    report_request: ReportRequest
    cron_expression: str
    next_run: datetime
    last_run: Optional[datetime] = None
    status: ScheduleStatus = ScheduleStatus.ACTIVE
    recipients: List[str] = field(default_factory=list)
    created_at: datetime = field(default_factory=datetime.now)
    created_by: str = "system"
    retry_count: int = 0
    max_retries: int = 3


class ReportScheduler:
    """Service for scheduling and managing automated reports"""
    
    def __init__(self):
        self.scheduled_jobs: Dict[str, ScheduledReportJob] = {}
        self.running = False
        self.scheduler_task = None
        self.report_generator = None  # Will be injected
    
    def set_report_generator(self, generator: Callable):
        """Set the report generator function"""
        self.report_generator = generator
    
    async def schedule_report(self, 
                            report_request: ReportRequest,
                            frequency: ScheduleFrequency,
                            recipients: List[str] = None,
                            start_date: Optional[datetime] = None,
                            end_date: Optional[datetime] = None) -> str:
        """
        Schedule a report for automated generation
        
        Args:
            report_request: Report configuration
            frequency: How often to run the report
            recipients: Email recipients for the report
            start_date: When to start the schedule
            end_date: When to end the schedule (optional)
            
        Returns:
            Job ID for the scheduled report
        """
        try:
            # Generate job ID
            job_id = f"sched_{datetime.now().strftime('%Y%m%d_%H%M%S')}_{report_request.report_type}"
            
            # Convert frequency to cron expression
            cron_expression = self._frequency_to_cron(frequency)
            
            # Calculate next run time
            start_time = start_date or datetime.now()
            cron = croniter(cron_expression, start_time)
            next_run = cron.get_next(datetime)
            
            # Create scheduled job
            job = ScheduledReportJob(
                job_id=job_id,
                report_request=report_request,
                cron_expression=cron_expression,
                next_run=next_run,
                recipients=recipients or []
            )
            
            # Store job
            self.scheduled_jobs[job_id] = job
            
            logger.info(f"Report scheduled: {job_id}, next run: {next_run}")
            
            return job_id
            
        except Exception as e:
            logger.error(f"Error scheduling report: {str(e)}")
            raise
    
    def _frequency_to_cron(self, frequency: ScheduleFrequency) -> str:
        """Convert frequency enum to cron expression"""
        cron_expressions = {
            ScheduleFrequency.DAILY: "0 9 * * *",      # 9 AM daily
            ScheduleFrequency.WEEKLY: "0 9 * * 1",     # 9 AM Monday
            ScheduleFrequency.MONTHLY: "0 9 1 * *",    # 9 AM 1st of month
            ScheduleFrequency.QUARTERLY: "0 9 1 1,4,7,10 *",  # 9 AM 1st of quarter
            ScheduleFrequency.YEARLY: "0 9 1 1 *",     # 9 AM Jan 1st
            ScheduleFrequency.HOURLY: "0 * * * *",     # Every hour
        }
        return cron_expressions.get(frequency, "0 9 * * *")
    
    async def unschedule_report(self, job_id: str) -> bool:
        """
        Remove a scheduled report
        
        Args:
            job_id: ID of the scheduled job
            
        Returns:
            True if job was removed, False if not found
        """
        try:
            if job_id in self.scheduled_jobs:
                del self.scheduled_jobs[job_id]
                logger.info(f"Report unscheduled: {job_id}")
                return True
            return False
            
        except Exception as e:
            logger.error(f"Error unscheduling report {job_id}: {str(e)}")
            raise
    
    async def pause_report(self, job_id: str) -> bool:
        """
        Pause a scheduled report
        
        Args:
            job_id: ID of the scheduled job
            
        Returns:
            True if job was paused, False if not found
        """
        try:
            if job_id in self.scheduled_jobs:
                self.scheduled_jobs[job_id].status = ScheduleStatus.PAUSED
                logger.info(f"Report paused: {job_id}")
                return True
            return False
            
        except Exception as e:
            logger.error(f"Error pausing report {job_id}: {str(e)}")
            raise
    
    async def resume_report(self, job_id: str) -> bool:
        """
        Resume a paused scheduled report
        
        Args:
            job_id: ID of the scheduled job
            
        Returns:
            True if job was resumed, False if not found
        """
        try:
            if job_id in self.scheduled_jobs:
                job = self.scheduled_jobs[job_id]
                if job.status == ScheduleStatus.PAUSED:
                    job.status = ScheduleStatus.ACTIVE
                    # Recalculate next run
                    cron = croniter(job.cron_expression, datetime.now())
                    job.next_run = cron.get_next(datetime)
                    logger.info(f"Report resumed: {job_id}, next run: {job.next_run}")
                return True
            return False
            
        except Exception as e:
            logger.error(f"Error resuming report {job_id}: {str(e)}")
            raise
    
    async def get_scheduled_reports(self) -> List[Dict[str, Any]]:
        """
        Get all scheduled reports
        
        Returns:
            List of scheduled report information
        """
        try:
            reports = []
            
            for job_id, job in self.scheduled_jobs.items():
                reports.append({
                    'job_id': job_id,
                    'report_type': job.report_request.report_type,
                    'title': job.report_request.title or job.report_request.report_type.title(),
                    'cron_expression': job.cron_expression,
                    'next_run': job.next_run.isoformat() if job.next_run else None,
                    'last_run': job.last_run.isoformat() if job.last_run else None,
                    'status': job.status.value,
                    'recipients': job.recipients,
                    'created_at': job.created_at.isoformat(),
                    'created_by': job.created_by,
                    'retry_count': job.retry_count
                })
            
            return sorted(reports, key=lambda x: x['next_run'] or '9999-12-31')
            
        except Exception as e:
            logger.error(f"Error getting scheduled reports: {str(e)}")
            raise
    
    async def start_scheduler(self):
        """Start the report scheduler"""
        try:
            if self.running:
                logger.warning("Scheduler is already running")
                return
            
            self.running = True
            self.scheduler_task = asyncio.create_task(self._scheduler_loop())
            logger.info("Report scheduler started")
            
        except Exception as e:
            logger.error(f"Error starting scheduler: {str(e)}")
            raise
    
    async def stop_scheduler(self):
        """Stop the report scheduler"""
        try:
            self.running = False
            
            if self.scheduler_task:
                self.scheduler_task.cancel()
                try:
                    await self.scheduler_task
                except asyncio.CancelledError:
                    pass
                self.scheduler_task = None
            
            logger.info("Report scheduler stopped")
            
        except Exception as e:
            logger.error(f"Error stopping scheduler: {str(e)}")
            raise
    
    async def _scheduler_loop(self):
        """Main scheduler loop"""
        logger.info("Scheduler loop started")
        
        while self.running:
            try:
                await self._process_scheduled_jobs()
                await asyncio.sleep(60)  # Check every minute
                
            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.error(f"Error in scheduler loop: {str(e)}")
                await asyncio.sleep(60)  # Continue after error
        
        logger.info("Scheduler loop ended")
    
    async def _process_scheduled_jobs(self):
        """Process all scheduled jobs and run those that are due"""
        current_time = datetime.now()
        
        for job_id, job in list(self.scheduled_jobs.items()):
            try:
                # Skip if paused or completed
                if job.status in [ScheduleStatus.PAUSED, ScheduleStatus.COMPLETED]:
                    continue
                
                # Check if job is due
                if current_time >= job.next_run:
                    logger.info(f"Processing scheduled job: {job_id}")
                    await self._execute_scheduled_job(job)
                    
            except Exception as e:
                logger.error(f"Error processing job {job_id}: {str(e)}")
                await self._handle_job_failure(job)
    
    async def _execute_scheduled_job(self, job: ScheduledReportJob):
        """Execute a scheduled job"""
        try:
            if not self.report_generator:
                logger.error(f"No report generator configured for job {job.job_id}")
                return
            
            # Generate report
            logger.info(f"Generating report for job: {job.job_id}")
            report_response = await self.report_generator(job.report_request)
            
            # Update job status
            job.last_run = datetime.now()
            job.retry_count = 0
            
            # Calculate next run
            cron = croniter(job.cron_expression, job.last_run)
            job.next_run = cron.get_next(datetime)
            
            # Send report if recipients specified
            if job.recipients:
                await self._send_report(job, report_response)
            
            logger.info(f"Job {job.job_id} completed successfully. Next run: {job.next_run}")
            
        except Exception as e:
            logger.error(f"Error executing job {job.job_id}: {str(e)}")
            await self._handle_job_failure(job)
            raise
    
    async def _handle_job_failure(self, job: ScheduledReportJob):
        """Handle job execution failure"""
        try:
            job.retry_count += 1
            
            if job.retry_count >= job.max_retries:
                job.status = ScheduleStatus.FAILED
                logger.error(f"Job {job.job_id} failed after {job.max_retries} retries")
            else:
                # Schedule retry in 5 minutes
                job.next_run = datetime.now() + timedelta(minutes=5)
                logger.info(f"Job {job.job_id} retry {job.retry_count} scheduled for {job.next_run}")
                
        except Exception as e:
            logger.error(f"Error handling job failure for {job.job_id}: {str(e)}")
    
    async def _send_report(self, job: ScheduledReportJob, report_response: Any):
        """Send generated report to recipients"""
        try:
            # This would integrate with an email service
            # For now, we'll just log the action
            logger.info(f"Report {report_response.report_id} would be sent to: {job.recipients}")
            
            # In a real implementation, this would:
            # 1. Export the report to PDF/Excel
            # 2. Send email with attachment
            # 3. Log delivery status
            
        except Exception as e:
            logger.error(f"Error sending report for job {job.job_id}: {str(e)}")
    
    async def run_job_now(self, job_id: str) -> bool:
        """
        Execute a scheduled job immediately
        
        Args:
            job_id: ID of the scheduled job
            
        Returns:
            True if job was executed, False if not found
        """
        try:
            if job_id in self.scheduled_jobs:
                job = self.scheduled_jobs[job_id]
                await self._execute_scheduled_job(job)
                return True
            return False
            
        except Exception as e:
            logger.error(f"Error running job {job_id} now: {str(e)}")
            raise
    
    async def get_job_history(self, job_id: str, limit: int = 50) -> List[Dict[str, Any]]:
        """
        Get execution history for a scheduled job
        
        Args:
            job_id: ID of the scheduled job
            limit: Maximum number of history entries to return
            
        Returns:
            List of job execution history
        """
        try:
            # In a real implementation, this would query a job history table
            # For now, return mock data
            if job_id in self.scheduled_jobs:
                job = self.scheduled_jobs[job_id]
                history = []
                
                if job.last_run:
                    history.append({
                        'execution_id': f"exec_{job.last_run.strftime('%Y%m%d_%H%M%S')}",
                        'job_id': job_id,
                        'executed_at': job.last_run.isoformat(),
                        'status': 'completed' if job.retry_count == 0 else 'failed',
                        'duration_seconds': 15,  # Mock duration
                        'report_id': f"report_{job.last_run.strftime('%Y%m%d_%H%M%S')}",
                        'error_message': None
                    })
                
                return history
            
            return []
            
        except Exception as e:
            logger.error(f"Error getting job history for {job_id}: {str(e)}")
            raise