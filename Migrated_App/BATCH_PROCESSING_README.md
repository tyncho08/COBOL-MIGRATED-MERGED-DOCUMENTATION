# ACAS Batch Processing System - Phase 7

## Overview

The ACAS Batch Processing System provides comprehensive job scheduling, execution, monitoring, and management capabilities for all migrated COBOL modules. This system handles automated processing for General Ledger, Sales Ledger, Purchase Ledger, Stock Control, and System Maintenance operations.

## Architecture

### Core Components

1. **Job Scheduler Service** (`job_scheduler.py`)
   - Job creation and definition management
   - Execution scheduling (daily, weekly, monthly, quarterly)
   - Dependency management between jobs
   - Asynchronous job execution with threading
   - Job status monitoring and retry logic
   - Dashboard for monitoring scheduled and running jobs

2. **Job Definitions Service** (`job_definitions.py`)
   - Pre-defined batch jobs for all modules
   - Standard job parameters and dependencies
   - Default scheduling configuration
   - Module-specific job creation (GL, SL, PL, Stock, System)

3. **Monitoring and Alerting Service** (`monitoring_alerts.py`)
   - Real-time job performance monitoring
   - Failure detection and alerting
   - Performance degradation analysis
   - Schedule adherence checking
   - System resource monitoring
   - Email notification system

4. **Log Viewer and Management Service** (`log_viewer.py`)
   - Comprehensive log viewing with filtering
   - Log analysis and pattern detection
   - Export capabilities (CSV, JSON, TXT)
   - Log cleanup and retention management
   - Performance metrics and reliability scoring

## Database Tables

### Core Batch Tables
- `batch_job_rec` - Job definitions and configurations
- `batch_schedule_rec` - Job scheduling information
- `batch_execution_rec` - Job execution records
- `batch_log_rec` - Detailed execution logs
- `batch_dependency_rec` - Job dependencies
- `batch_parameter_rec` - Job parameters

### Monitoring Tables  
- `batch_alert_rec` - System alerts and notifications
- `batch_metrics_rec` - Performance metrics
- `batch_threshold_rec` - Performance thresholds
- `quarantine_rec` - System resource alerts

## Pre-defined Batch Jobs

### General Ledger Jobs
- **GL_DAILY_CLOSE** - Daily GL closing process
- **GL_PERIOD_CLOSE** - Monthly period close
- **GL_YEAR_END_CLOSE** - Annual year-end close
- **GL_TRIAL_BALANCE** - Generate trial balance report
- **GL_BUDGET_VARIANCE** - Budget vs Actual analysis
- **GL_ACCOUNT_RECONCILIATION** - Bank and control account reconciliation

### Sales Ledger Jobs
- **SL_AGED_DEBTORS** - Calculate aged debtors analysis
- **SL_STATEMENT_GENERATION** - Generate customer statements
- **SL_CREDIT_LIMIT_CHECK** - Check credit limits and alerts
- **SL_OVERDUE_REMINDERS** - Generate overdue payment reminders
- **SL_COMMISSION_CALCULATION** - Calculate sales commissions
- **SL_VAT_RETURN** - Prepare VAT return data

### Purchase Ledger Jobs
- **PL_AGED_CREDITORS** - Calculate aged creditors analysis
- **PL_PAYMENT_RUN** - Process supplier payments
- **PL_REMITTANCE_ADVICE** - Generate remittance advices
- **PL_THREE_WAY_MATCHING** - PO/GRN/Invoice matching
- **PL_ACCRUALS_CALCULATION** - Calculate purchase accruals
- **PL_VAT_RETURN** - Prepare input VAT return data

### Stock Control Jobs
- **ST_REORDER_CALCULATION** - Calculate reorder points
- **ST_ABC_ANALYSIS** - ABC classification analysis
- **ST_CYCLE_COUNT_SCHEDULE** - Generate cycle count schedule
- **ST_DEAD_STOCK_ANALYSIS** - Identify dead and slow-moving stock
- **ST_STOCK_VALUATION** - Stock valuation and costing
- **ST_DEMAND_FORECAST** - Generate demand forecasts
- **ST_LOCATION_REPLENISHMENT** - Generate replenishment tasks
- **ST_EXPIRY_MONITORING** - Monitor stock expiry dates

### System Maintenance Jobs
- **SYS_DATABASE_BACKUP** - Database backup and maintenance
- **SYS_LOG_CLEANUP** - Clean up old log files
- **SYS_ARCHIVE_DATA** - Archive old transactional data
- **SYS_PERFORMANCE_STATS** - Collect performance statistics
- **SYS_HEALTH_CHECK** - System health monitoring
- **SYS_USER_SESSION_CLEANUP** - Clean up expired sessions

## Default Schedules

### Daily (runs every day)
- GL_DAILY_CLOSE (23:00)
- SL_CREDIT_LIMIT_CHECK (08:00)
- PL_THREE_WAY_MATCHING (09:00)
- ST_REORDER_CALCULATION (06:00)
- ST_EXPIRY_MONITORING (07:00)
- SYS_HEALTH_CHECK (05:00)
- SYS_USER_SESSION_CLEANUP (02:00)

### Weekly (runs on Sunday)
- SL_AGED_DEBTORS (08:00)
- PL_AGED_CREDITORS (09:00)
- ST_ABC_ANALYSIS (06:00)
- ST_CYCLE_COUNT_SCHEDULE (07:00)
- ST_DEAD_STOCK_ANALYSIS (10:00)
- SYS_DATABASE_BACKUP (01:00)
- SYS_PERFORMANCE_STATS (05:00)

### Monthly (runs on last day of month)
- GL_PERIOD_CLOSE (22:00)
- SL_STATEMENT_GENERATION (08:00)
- SL_COMMISSION_CALCULATION (10:00)
- PL_PAYMENT_RUN (14:00)
- PL_ACCRUALS_CALCULATION (16:00)
- ST_STOCK_VALUATION (06:00)
- ST_DEMAND_FORECAST (07:00)
- SYS_ARCHIVE_DATA (02:00)
- SYS_LOG_CLEANUP (03:00)

### Quarterly (runs on quarter end)
- SL_VAT_RETURN (10:00)
- PL_VAT_RETURN (11:00)

## Key Features

### Job Management
- Create, schedule, and execute batch jobs
- Job dependency management
- Parameter passing and validation
- Retry logic with configurable attempts
- Concurrent execution control
- Priority-based scheduling

### Monitoring and Alerting
- Real-time job status monitoring
- Performance threshold monitoring
- Failure detection and notification
- Schedule adherence checking
- System resource monitoring
- Email alert notifications

### Logging and Analysis
- Comprehensive execution logging
- Log filtering and search capabilities
- Performance analysis and trends
- Error pattern detection
- Log export capabilities
- Automatic log cleanup

### Dashboard and Reporting
- Scheduler status dashboard
- Job execution summary
- Performance metrics
- Alert management
- System health indicators
- Historical trend analysis

## Usage Examples

### Initialize Services
```python
from app.services.batch import (
    JobSchedulerService, 
    BatchJobDefinitionsService,
    BatchMonitoringService,
    BatchLogViewerService
)

# Initialize services
scheduler = JobSchedulerService(db)
definitions = BatchJobDefinitionsService(db, current_user)
monitoring = BatchMonitoringService(db, current_user)
log_viewer = BatchLogViewerService(db, current_user)
```

### Create Standard Jobs
```python
# Create all standard batch jobs
success, result = definitions.create_standard_batch_jobs()
print(f"Created {result['jobs_created']} batch jobs")

# Create default schedules
success, result = definitions.create_default_schedules()
print(f"Created {result['schedules_created']} schedules")
```

### Start/Stop Scheduler
```python
# Start the scheduler
scheduler.start_scheduler()

# Execute a job immediately
success, execution_id = scheduler.execute_job_now('GL_DAILY_CLOSE', {
    'period': 'CURRENT',
    'warehouse': 'ALL'
})
```

### Monitor Jobs
```python
# Get scheduler dashboard
dashboard = scheduler.get_scheduler_dashboard()

# Monitor jobs and send alerts
monitoring_summary = monitoring.monitor_batch_jobs({
    'check_failures': True,
    'check_performance': True,
    'send_alerts': True
})
```

### View Logs
```python
# Get execution logs
log_data = log_viewer.get_execution_logs({
    'job_name': 'GL_DAILY_CLOSE',
    'date_from': 20241201,
    'log_level': 'ERROR'
})

# Export logs
success, file_path = log_viewer.export_logs({
    'job_name': 'GL_DAILY_CLOSE',
    'format': 'CSV',
    'date_from': 20241201
})
```

## Configuration

### Job Parameters
Each job supports configurable parameters:
- Warehouse filters
- Date ranges
- Processing options
- Output formats
- Notification settings

### Performance Thresholds
Set performance monitoring thresholds:
- Maximum runtime limits
- Resource usage limits
- Error rate thresholds
- Schedule adherence tolerances

### Alert Configuration
Configure alert notifications:
- Email recipients
- Severity levels
- Notification frequency
- Escalation rules

## Error Handling

### Job Failures
- Automatic retry with exponential backoff
- Error logging and categorization
- Alert generation for critical failures
- Dependency impact analysis

### System Failures
- Database connection handling
- Resource monitoring and alerts
- Graceful shutdown procedures
- Recovery mechanisms

## Performance Considerations

### Scalability
- Threaded job execution
- Database connection pooling
- Batch processing for large datasets
- Resource usage monitoring

### Optimization
- Job scheduling optimization
- Log retention policies
- Database query optimization
- Memory usage management

## Security

### Access Control
- User-based job execution permissions
- Audit logging for all operations
- Secure parameter handling
- Role-based access to monitoring

### Data Protection
- Sensitive data masking in logs
- Secure parameter storage
- Encrypted alert notifications
- Audit trail maintenance

## Integration

### Module Integration
The batch system integrates with all ACAS modules:
- General Ledger services
- Sales Ledger services  
- Purchase Ledger services
- Stock Control services
- System services

### External Integration
- Email system integration
- Database backup systems
- Monitoring tools integration
- Reporting system integration

## Maintenance

### Regular Tasks
- Log cleanup and archiving
- Performance threshold review
- Job schedule optimization
- Alert configuration updates

### Troubleshooting
- Job failure analysis
- Performance bottleneck identification  
- Schedule conflict resolution
- Resource usage optimization

---

This batch processing system provides a complete solution for automating all ACAS operations while maintaining the reliability and control required for business-critical processes.