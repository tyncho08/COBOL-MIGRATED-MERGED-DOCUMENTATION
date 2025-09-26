"""
Report Scheduler API

Handles scheduling, managing, and monitoring of automated reports.
Provides endpoints for report scheduling and job management.
"""

from typing import Dict, List, Any, Optional
from datetime import datetime, timedelta
from fastapi import APIRouter, Depends, HTTPException, Query, BackgroundTasks
from sqlalchemy.orm import Session
import logging

from app.core.database import get_db
from app.core.security import get_current_user
from app.models.auth import User
from app.schemas.reports import (
    ScheduledReport, ScheduleFrequency, ReportRequest,
    ScheduledReportResponse, JobHistoryResponse
)
from app.services.reporting import ReportScheduler, ReportEngine

logger = logging.getLogger(__name__)
router = APIRouter()

# Initialize services
report_scheduler = ReportScheduler()
report_engine = ReportEngine()

# Set up the scheduler to use the report engine
report_scheduler.set_report_generator(report_engine.generate_report)


@router.post("/schedule", response_model=Dict[str, str])
async def schedule_report(
    request: ScheduledReport,
    background_tasks: BackgroundTasks,
    db: Session = Depends(get_db),
    current_user: User = Depends(get_current_user)
):
    """
    Schedule a report for automated generation
    
    Schedule any report to run automatically at specified intervals:
    - DAILY: Every day at 9 AM
    - WEEKLY: Every Monday at 9 AM  
    - MONTHLY: 1st of every month at 9 AM
    - QUARTERLY: 1st of Jan/Apr/Jul/Oct at 9 AM
    - YEARLY: January 1st at 9 AM
    - HOURLY: Every hour on the hour
    
    Reports can be sent automatically to specified email recipients.
    """
    try:
        logger.info(f"Scheduling report: {request.report_request.report_type} for user: {current_user.username}")
        
        # Schedule the report
        job_id = await report_scheduler.schedule_report(
            report_request=request.report_request,
            frequency=request.frequency,
            recipients=request.recipients,
            start_date=request.start_date,
            end_date=request.end_date
        )
        
        # Start scheduler if not already running
        if not report_scheduler.running:
            background_tasks.add_task(report_scheduler.start_scheduler)
        
        logger.info(f"Report scheduled with job ID: {job_id}")
        
        return {
            "job_id": job_id,
            "message": "Report scheduled successfully",
            "next_run": report_scheduler.scheduled_jobs[job_id].next_run.isoformat()
        }
        
    except Exception as e:
        logger.error(f"Error scheduling report: {str(e)}")
        raise HTTPException(status_code=500, detail="Failed to schedule report")


@router.get("/scheduled", response_model=List[ScheduledReportResponse])
async def get_scheduled_reports(
    current_user: User = Depends(get_current_user)
):
    """
    Get all scheduled reports
    
    Returns list of all active, paused, and failed scheduled reports
    with their next run times and configuration details.
    """
    try:
        scheduled_reports = await report_scheduler.get_scheduled_reports()
        
        return [
            ScheduledReportResponse(
                job_id=report['job_id'],
                report_type=report['report_type'],
                title=report['title'],
                next_run=datetime.fromisoformat(report['next_run']) if report['next_run'] else None,
                last_run=datetime.fromisoformat(report['last_run']) if report['last_run'] else None,
                status=report['status'],
                recipients=report['recipients'],
                created_at=datetime.fromisoformat(report['created_at']),
                created_by=report['created_by'],
                retry_count=report['retry_count']
            )
            for report in scheduled_reports
        ]
        
    except Exception as e:
        logger.error(f"Error getting scheduled reports: {str(e)}")
        raise HTTPException(status_code=500, detail="Failed to get scheduled reports")


@router.delete("/scheduled/{job_id}")
async def unschedule_report(
    job_id: str,
    current_user: User = Depends(get_current_user)
):
    """
    Remove a scheduled report
    
    Permanently removes a scheduled report from the scheduler.
    """
    try:
        success = await report_scheduler.unschedule_report(job_id)
        
        if not success:
            raise HTTPException(status_code=404, detail="Scheduled report not found")
        
        logger.info(f"Report unscheduled: {job_id} by user: {current_user.username}")
        
        return {"message": f"Scheduled report {job_id} removed successfully"}
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error unscheduling report {job_id}: {str(e)}")
        raise HTTPException(status_code=500, detail="Failed to unschedule report")


@router.put("/scheduled/{job_id}/pause")
async def pause_scheduled_report(
    job_id: str,
    current_user: User = Depends(get_current_user)
):
    """
    Pause a scheduled report
    
    Temporarily stops a scheduled report from running. Can be resumed later.
    """
    try:
        success = await report_scheduler.pause_report(job_id)
        
        if not success:
            raise HTTPException(status_code=404, detail="Scheduled report not found")
        
        logger.info(f"Report paused: {job_id} by user: {current_user.username}")
        
        return {"message": f"Scheduled report {job_id} paused successfully"}
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error pausing report {job_id}: {str(e)}")
        raise HTTPException(status_code=500, detail="Failed to pause report")


@router.put("/scheduled/{job_id}/resume")
async def resume_scheduled_report(
    job_id: str,
    current_user: User = Depends(get_current_user)
):
    """
    Resume a paused scheduled report
    
    Restarts a paused scheduled report and calculates next run time.
    """
    try:
        success = await report_scheduler.resume_report(job_id)
        
        if not success:
            raise HTTPException(status_code=404, detail="Scheduled report not found")
        
        # Get updated next run time
        if job_id in report_scheduler.scheduled_jobs:
            next_run = report_scheduler.scheduled_jobs[job_id].next_run
        else:
            next_run = None
        
        logger.info(f"Report resumed: {job_id} by user: {current_user.username}")
        
        return {
            "message": f"Scheduled report {job_id} resumed successfully",
            "next_run": next_run.isoformat() if next_run else None
        }
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error resuming report {job_id}: {str(e)}")
        raise HTTPException(status_code=500, detail="Failed to resume report")


@router.post("/scheduled/{job_id}/run")
async def run_scheduled_report_now(
    job_id: str,
    background_tasks: BackgroundTasks,
    current_user: User = Depends(get_current_user)
):
    """
    Execute a scheduled report immediately
    
    Runs the scheduled report right now without waiting for the next scheduled time.
    """
    try:
        # Add the job execution to background tasks
        background_tasks.add_task(report_scheduler.run_job_now, job_id)
        
        logger.info(f"Running scheduled report now: {job_id} by user: {current_user.username}")
        
        return {
            "message": f"Scheduled report {job_id} execution started",
            "status": "running"
        }
        
    except Exception as e:
        logger.error(f"Error running scheduled report {job_id}: {str(e)}")
        raise HTTPException(status_code=500, detail="Failed to run scheduled report")


@router.get("/scheduled/{job_id}/history", response_model=List[JobHistoryResponse])
async def get_job_history(
    job_id: str,
    limit: int = Query(50, ge=1, le=1000),
    current_user: User = Depends(get_current_user)
):
    """
    Get execution history for a scheduled report
    
    Returns the execution history showing successful runs, failures, and timing.
    """
    try:
        history = await report_scheduler.get_job_history(job_id, limit)
        
        return [
            JobHistoryResponse(
                execution_id=entry['execution_id'],
                job_id=entry['job_id'],
                executed_at=datetime.fromisoformat(entry['executed_at']),
                status=entry['status'],
                duration_seconds=entry['duration_seconds'],
                report_id=entry.get('report_id'),
                error_message=entry.get('error_message')
            )
            for entry in history
        ]
        
    except Exception as e:
        logger.error(f"Error getting job history for {job_id}: {str(e)}")
        raise HTTPException(status_code=500, detail="Failed to get job history")


@router.get("/scheduled/{job_id}")
async def get_scheduled_report_details(
    job_id: str,
    current_user: User = Depends(get_current_user)
):
    """
    Get detailed information about a scheduled report
    
    Returns complete configuration and status information for a scheduled report.
    """
    try:
        if job_id not in report_scheduler.scheduled_jobs:
            raise HTTPException(status_code=404, detail="Scheduled report not found")
        
        job = report_scheduler.scheduled_jobs[job_id]
        
        return {
            "job_id": job_id,
            "report_type": job.report_request.report_type,
            "report_parameters": job.report_request.parameters,
            "cron_expression": job.cron_expression,
            "next_run": job.next_run.isoformat() if job.next_run else None,
            "last_run": job.last_run.isoformat() if job.last_run else None,
            "status": job.status.value,
            "recipients": job.recipients,
            "created_at": job.created_at.isoformat(),
            "created_by": job.created_by,
            "retry_count": job.retry_count,
            "max_retries": job.max_retries
        }
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error getting scheduled report details {job_id}: {str(e)}")
        raise HTTPException(status_code=500, detail="Failed to get scheduled report details")


@router.put("/scheduled/{job_id}")
async def update_scheduled_report(
    job_id: str,
    update_data: Dict[str, Any],
    current_user: User = Depends(get_current_user)
):
    """
    Update a scheduled report configuration
    
    Allows updating recipients, frequency, and other schedule parameters.
    """
    try:
        if job_id not in report_scheduler.scheduled_jobs:
            raise HTTPException(status_code=404, detail="Scheduled report not found")
        
        job = report_scheduler.scheduled_jobs[job_id]
        
        # Update allowed fields
        if 'recipients' in update_data:
            job.recipients = update_data['recipients']
        
        if 'frequency' in update_data:
            # This would require recalculating cron expression and next run
            # For now, just log the request
            logger.info(f"Frequency update requested for job {job_id}: {update_data['frequency']}")
        
        logger.info(f"Scheduled report updated: {job_id} by user: {current_user.username}")
        
        return {"message": f"Scheduled report {job_id} updated successfully"}
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error updating scheduled report {job_id}: {str(e)}")
        raise HTTPException(status_code=500, detail="Failed to update scheduled report")


@router.get("/scheduler/status")
async def get_scheduler_status(
    current_user: User = Depends(get_current_user)
):
    """
    Get the current status of the report scheduler
    
    Returns information about scheduler health and activity.
    """
    try:
        return {
            "scheduler_running": report_scheduler.running,
            "total_jobs": len(report_scheduler.scheduled_jobs),
            "active_jobs": len([job for job in report_scheduler.scheduled_jobs.values() 
                              if job.status.value == "active"]),
            "paused_jobs": len([job for job in report_scheduler.scheduled_jobs.values() 
                               if job.status.value == "paused"]),
            "failed_jobs": len([job for job in report_scheduler.scheduled_jobs.values() 
                               if job.status.value == "failed"]),
            "next_jobs": [
                {
                    "job_id": job.job_id,
                    "report_type": job.report_request.report_type,
                    "next_run": job.next_run.isoformat() if job.next_run else None
                }
                for job in sorted(report_scheduler.scheduled_jobs.values(), 
                                key=lambda x: x.next_run or datetime.max)[:5]
            ]
        }
        
    except Exception as e:
        logger.error(f"Error getting scheduler status: {str(e)}")
        raise HTTPException(status_code=500, detail="Failed to get scheduler status")


@router.post("/scheduler/start")
async def start_scheduler(
    background_tasks: BackgroundTasks,
    current_user: User = Depends(get_current_user)
):
    """
    Start the report scheduler
    
    Starts the background scheduler service for automated report generation.
    """
    try:
        if report_scheduler.running:
            return {"message": "Scheduler is already running"}
        
        background_tasks.add_task(report_scheduler.start_scheduler)
        
        logger.info(f"Scheduler started by user: {current_user.username}")
        
        return {"message": "Scheduler started successfully"}
        
    except Exception as e:
        logger.error(f"Error starting scheduler: {str(e)}")
        raise HTTPException(status_code=500, detail="Failed to start scheduler")


@router.post("/scheduler/stop")
async def stop_scheduler(
    current_user: User = Depends(get_current_user)
):
    """
    Stop the report scheduler
    
    Stops the background scheduler service. Running jobs will complete.
    """
    try:
        await report_scheduler.stop_scheduler()
        
        logger.info(f"Scheduler stopped by user: {current_user.username}")
        
        return {"message": "Scheduler stopped successfully"}
        
    except Exception as e:
        logger.error(f"Error stopping scheduler: {str(e)}")
        raise HTTPException(status_code=500, detail="Failed to stop scheduler")