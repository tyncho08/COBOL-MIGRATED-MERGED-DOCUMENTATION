"""
Batch Processing Models
Handles batch jobs, scheduling, and monitoring
"""
from sqlalchemy import Column, String, Integer, DateTime, Text, Boolean, ForeignKey, Numeric
from sqlalchemy.orm import relationship
from sqlalchemy.sql import func
from app.core.database import Base


class BatchJobRec(Base):
    """Batch job definition record"""
    __tablename__ = "batch_jobs"
    
    job_id = Column(Integer, primary_key=True, autoincrement=True)
    job_code = Column(String(20), unique=True, nullable=False, doc="Job code")
    job_name = Column(String(100), nullable=False, doc="Job name")
    job_description = Column(Text, doc="Job description")
    
    # Job type and category
    job_type = Column(String(20), nullable=False, doc="Type: REPORT, PROCESS, INTERFACE, MAINTENANCE")
    job_category = Column(String(20), doc="Category: GL, AR, AP, STOCK, etc")
    
    # Execution details
    program_name = Column(String(100), doc="Program/script to execute")
    command_line = Column(Text, doc="Command line arguments")
    working_directory = Column(String(255), doc="Working directory")
    
    # Configuration
    enabled = Column(Boolean, default=True, doc="Job enabled flag")
    priority = Column(Integer, default=50, doc="Priority (1-100, lower is higher)")
    max_runtime = Column(Integer, default=3600, doc="Maximum runtime in seconds")
    retry_count = Column(Integer, default=0, doc="Number of retries on failure")
    retry_interval = Column(Integer, default=300, doc="Retry interval in seconds")
    
    # Dependencies
    depends_on_success = Column(Text, doc="Comma-separated list of job codes that must succeed")
    depends_on_complete = Column(Text, doc="Comma-separated list of job codes that must complete")
    
    # Notification
    notify_on_start = Column(String(255), doc="Email addresses for start notification")
    notify_on_success = Column(String(255), doc="Email addresses for success notification")
    notify_on_failure = Column(String(255), doc="Email addresses for failure notification")
    
    # Audit
    created_by = Column(String(30), nullable=False)
    created_date = Column(DateTime(timezone=True), server_default=func.current_timestamp())
    modified_by = Column(String(30))
    modified_date = Column(DateTime(timezone=True), onupdate=func.current_timestamp())
    
    # Relationships
    schedules = relationship("BatchScheduleRec", back_populates="job")
    executions = relationship("BatchExecutionRec", back_populates="job")
    parameters = relationship("BatchParameterRec", back_populates="job")


class BatchScheduleRec(Base):
    """Batch job schedule record"""
    __tablename__ = "batch_schedules"
    
    schedule_id = Column(Integer, primary_key=True, autoincrement=True)
    job_id = Column(Integer, ForeignKey("batch_jobs.job_id"), nullable=False)
    schedule_name = Column(String(100), nullable=False)
    
    # Schedule type
    schedule_type = Column(String(20), nullable=False, doc="Type: ONCE, DAILY, WEEKLY, MONTHLY, CRON")
    
    # Schedule details
    start_date = Column(Integer, nullable=False, doc="Start date YYYYMMDD")
    end_date = Column(Integer, doc="End date YYYYMMDD (optional)")
    start_time = Column(String(5), nullable=False, doc="Start time HH:MM")
    
    # Frequency details
    frequency_interval = Column(Integer, doc="Interval for recurring schedules")
    frequency_unit = Column(String(10), doc="Unit: MINUTE, HOUR, DAY, WEEK, MONTH")
    
    # Weekly schedule (bit mask: 1=Sun, 2=Mon, 4=Tue, etc.)
    weekdays = Column(Integer, doc="Weekday mask for weekly schedules")
    
    # Monthly schedule
    day_of_month = Column(Integer, doc="Day of month (1-31) for monthly schedules")
    week_of_month = Column(Integer, doc="Week of month (1-5) for monthly schedules")
    
    # CRON expression
    cron_expression = Column(String(100), doc="CRON expression for complex schedules")
    
    # Control
    enabled = Column(Boolean, default=True)
    last_run_date = Column(DateTime(timezone=True))
    next_run_date = Column(DateTime(timezone=True))
    
    # Relationships
    job = relationship("BatchJobRec", back_populates="schedules")


class BatchExecutionRec(Base):
    """Batch job execution history"""
    __tablename__ = "batch_executions"
    
    execution_id = Column(Integer, primary_key=True, autoincrement=True)
    job_id = Column(Integer, ForeignKey("batch_jobs.job_id"), nullable=False)
    schedule_id = Column(Integer, ForeignKey("batch_schedules.schedule_id"))
    
    # Execution details
    start_time = Column(DateTime(timezone=True), nullable=False)
    end_time = Column(DateTime(timezone=True))
    duration_seconds = Column(Integer)
    
    # Status
    status = Column(String(20), nullable=False, doc="Status: RUNNING, SUCCESS, FAILED, CANCELLED, TIMEOUT")
    exit_code = Column(Integer)
    error_message = Column(Text)
    
    # Resource usage
    cpu_seconds = Column(Numeric(10, 2))
    memory_mb = Column(Integer)
    disk_io_mb = Column(Integer)
    
    # Output
    output_file = Column(String(255), doc="Path to output file")
    error_file = Column(String(255), doc="Path to error file")
    records_processed = Column(Integer)
    
    # Triggered by
    triggered_by = Column(String(30), doc="User or SCHEDULER")
    
    # Relationships
    job = relationship("BatchJobRec", back_populates="executions")
    logs = relationship("BatchLogRec", back_populates="execution")


class BatchLogRec(Base):
    """Batch job execution logs"""
    __tablename__ = "batch_logs"
    
    log_id = Column(Integer, primary_key=True, autoincrement=True)
    execution_id = Column(Integer, ForeignKey("batch_executions.execution_id"), nullable=False)
    
    # Log details
    log_time = Column(DateTime(timezone=True), nullable=False)
    log_level = Column(String(10), nullable=False, doc="Level: DEBUG, INFO, WARN, ERROR, FATAL")
    log_source = Column(String(100), doc="Source module/function")
    log_message = Column(Text, nullable=False)
    
    # Additional context
    record_number = Column(Integer, doc="Record being processed")
    error_code = Column(String(20), doc="Application error code")
    
    # Relationships
    execution = relationship("BatchExecutionRec", back_populates="logs")


class BatchParameterRec(Base):
    """Batch job parameters"""
    __tablename__ = "batch_parameters"
    
    parameter_id = Column(Integer, primary_key=True, autoincrement=True)
    job_id = Column(Integer, ForeignKey("batch_jobs.job_id"), nullable=False)
    
    # Parameter details
    parameter_name = Column(String(50), nullable=False)
    parameter_type = Column(String(20), nullable=False, doc="Type: STRING, INTEGER, DECIMAL, DATE, BOOLEAN")
    parameter_value = Column(String(255))
    
    # Validation
    required = Column(Boolean, default=False)
    default_value = Column(String(255))
    min_value = Column(String(255))
    max_value = Column(String(255))
    valid_values = Column(Text, doc="Comma-separated list of valid values")
    
    # Description
    description = Column(Text)
    
    # Relationships
    job = relationship("BatchJobRec", back_populates="parameters")


class BatchDependencyRec(Base):
    """Batch job dependencies"""
    __tablename__ = "batch_dependencies"
    
    dependency_id = Column(Integer, primary_key=True, autoincrement=True)
    job_id = Column(Integer, ForeignKey("batch_jobs.job_id"), nullable=False)
    depends_on_job_id = Column(Integer, ForeignKey("batch_jobs.job_id"), nullable=False)
    
    # Dependency type
    dependency_type = Column(String(20), nullable=False, doc="Type: SUCCESS, COMPLETE, FAILURE")
    
    # Timing
    wait_minutes = Column(Integer, default=0, doc="Minutes to wait after dependency completes")
    timeout_minutes = Column(Integer, doc="Maximum minutes to wait for dependency")
    
    # Control
    enabled = Column(Boolean, default=True)
    
    # Relationships
    job = relationship("BatchJobRec", foreign_keys=[job_id])
    depends_on = relationship("BatchJobRec", foreign_keys=[depends_on_job_id])


class BatchAlertRec(Base):
    """Batch job alerts and notifications"""
    __tablename__ = "batch_alerts"
    
    alert_id = Column(Integer, primary_key=True, autoincrement=True)
    job_id = Column(Integer, ForeignKey("batch_jobs.job_id"))
    execution_id = Column(Integer, ForeignKey("batch_executions.execution_id"))
    
    # Alert details
    alert_time = Column(DateTime(timezone=True), nullable=False)
    alert_type = Column(String(20), nullable=False, doc="Type: START, SUCCESS, FAILURE, WARNING, TIMEOUT")
    alert_level = Column(String(10), nullable=False, doc="Level: INFO, WARN, ERROR, CRITICAL")
    
    # Message
    alert_message = Column(Text, nullable=False)
    alert_details = Column(Text)
    
    # Notification
    notification_sent = Column(Boolean, default=False)
    notification_method = Column(String(20), doc="Method: EMAIL, SMS, WEBHOOK")
    notification_address = Column(String(255))
    notification_time = Column(DateTime(timezone=True))
    notification_status = Column(String(20))
    
    # Acknowledgment
    acknowledged = Column(Boolean, default=False)
    acknowledged_by = Column(String(30))
    acknowledged_time = Column(DateTime(timezone=True))
    acknowledgment_notes = Column(Text)


class BatchQueueRec(Base):
    """Batch job queue for scheduled and ad-hoc executions"""
    __tablename__ = "batch_queue"
    
    queue_id = Column(Integer, primary_key=True, autoincrement=True)
    job_id = Column(Integer, ForeignKey("batch_jobs.job_id"), nullable=False)
    schedule_id = Column(Integer, ForeignKey("batch_schedules.schedule_id"))
    
    # Queue details
    queue_time = Column(DateTime(timezone=True), nullable=False)
    scheduled_time = Column(DateTime(timezone=True), nullable=False)
    priority = Column(Integer, default=50)
    
    # Status
    status = Column(String(20), nullable=False, doc="Status: QUEUED, RUNNING, COMPLETE, FAILED, CANCELLED")
    
    # Parameters override
    parameters = Column(Text, doc="JSON parameters override")
    
    # Control
    hold = Column(Boolean, default=False, doc="Hold job from executing")
    hold_reason = Column(String(255))
    
    # Requested by
    requested_by = Column(String(30), nullable=False)
    request_time = Column(DateTime(timezone=True), nullable=False)


class BatchResourceRec(Base):
    """Batch job resource constraints and usage"""
    __tablename__ = "batch_resources"
    
    resource_id = Column(Integer, primary_key=True, autoincrement=True)
    job_id = Column(Integer, ForeignKey("batch_jobs.job_id"), nullable=False)
    
    # Resource constraints
    max_cpu_percent = Column(Integer, doc="Maximum CPU percentage")
    max_memory_mb = Column(Integer, doc="Maximum memory in MB")
    max_disk_io_mbps = Column(Integer, doc="Maximum disk I/O in MB/s")
    
    # Concurrency
    max_concurrent = Column(Integer, default=1, doc="Maximum concurrent executions")
    exclusive_execution = Column(Boolean, default=False, doc="Exclusive execution flag")
    
    # Time windows
    allowed_hours = Column(String(100), doc="Allowed execution hours (e.g., '08:00-18:00')")
    blackout_dates = Column(Text, doc="Comma-separated blackout dates")
    
    # Resource pools
    resource_pool = Column(String(50), doc="Resource pool name")
    pool_priority = Column(Integer, default=50)


class BatchMetricsRec(Base):
    """Batch job execution metrics"""
    __tablename__ = "batch_metrics"
    
    metric_id = Column(Integer, primary_key=True, autoincrement=True)
    execution_id = Column(Integer, ForeignKey("batch_executions.execution_id"), nullable=False)
    
    # Performance metrics
    cpu_usage_avg = Column(Numeric(5, 2), doc="Average CPU usage percentage")
    cpu_usage_max = Column(Numeric(5, 2), doc="Maximum CPU usage percentage")
    memory_usage_avg = Column(Integer, doc="Average memory usage in MB")
    memory_usage_max = Column(Integer, doc="Maximum memory usage in MB")
    disk_io_read_mb = Column(Integer, doc="Total disk read in MB")
    disk_io_write_mb = Column(Integer, doc="Total disk write in MB")
    
    # Database metrics
    db_queries_count = Column(Integer, doc="Number of database queries")
    db_rows_read = Column(Integer, doc="Number of rows read")
    db_rows_written = Column(Integer, doc="Number of rows written")
    db_time_ms = Column(Integer, doc="Total database time in milliseconds")
    
    # Processing metrics
    records_processed = Column(Integer, doc="Total records processed")
    records_success = Column(Integer, doc="Successfully processed records")
    records_failed = Column(Integer, doc="Failed records")
    records_skipped = Column(Integer, doc="Skipped records")
    
    # Network metrics
    network_calls = Column(Integer, doc="Number of network calls")
    network_bytes_sent = Column(Integer, doc="Bytes sent over network")
    network_bytes_received = Column(Integer, doc="Bytes received over network")
    
    # Custom metrics (JSON)
    custom_metrics = Column(Text, doc="JSON custom metrics")
    
    # Timestamps
    collected_at = Column(DateTime, server_default=func.now())


class BatchThresholdRec(Base):
    """Batch job performance thresholds for alerting"""
    __tablename__ = "batch_thresholds"
    
    threshold_id = Column(Integer, primary_key=True, autoincrement=True)
    job_id = Column(Integer, ForeignKey("batch_jobs.job_id"), nullable=False)
    
    # Duration thresholds
    warning_duration_minutes = Column(Integer, doc="Warning if execution exceeds this duration")
    critical_duration_minutes = Column(Integer, doc="Critical alert if execution exceeds this duration")
    
    # Record thresholds
    min_records_expected = Column(Integer, doc="Minimum records expected to be processed")
    max_records_expected = Column(Integer, doc="Maximum records expected to be processed")
    
    # Success rate thresholds
    warning_success_rate = Column(Numeric(5, 2), default=95.0, doc="Warning if success rate below this")
    critical_success_rate = Column(Numeric(5, 2), default=90.0, doc="Critical if success rate below this")
    
    # Resource thresholds
    max_cpu_threshold = Column(Numeric(5, 2), default=80.0, doc="Alert if CPU exceeds this percentage")
    max_memory_threshold = Column(Integer, doc="Alert if memory exceeds this MB")
    
    # Active flag
    active = Column(Boolean, default=True)
    
    # Audit
    created_date = Column(DateTime, server_default=func.now())
    created_by = Column(String(50))
    updated_date = Column(DateTime, onupdate=func.now())
    updated_by = Column(String(50))
