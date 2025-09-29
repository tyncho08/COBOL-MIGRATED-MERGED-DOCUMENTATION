"""
GL Reports Models
Handles General Ledger report definitions and parameters
"""
from sqlalchemy import Column, String, Integer, DateTime, Text, Boolean, ForeignKey, Numeric, JSON
from sqlalchemy.orm import relationship
from sqlalchemy.sql import func
from app.core.database import Base


class GLReportDefinitionRec(Base):
    """GL report definition record"""
    __tablename__ = "gl_report_definitions"
    
    report_id = Column(Integer, primary_key=True, autoincrement=True)
    report_code = Column(String(20), unique=True, nullable=False, doc="Report code")
    report_name = Column(String(100), nullable=False, doc="Report name")
    report_description = Column(Text, doc="Report description")
    
    # Report type and category
    report_type = Column(String(20), nullable=False, doc="Type: STANDARD, CUSTOM, ANALYTICAL")
    report_category = Column(String(20), doc="Category: BALANCE_SHEET, P&L, TRIAL_BALANCE, etc")
    
    # Report format
    output_format = Column(String(10), default='PDF', doc="Format: PDF, EXCEL, CSV, HTML")
    template_name = Column(String(100), doc="Report template name")
    
    # Layout
    orientation = Column(String(10), default='PORTRAIT', doc="PORTRAIT or LANDSCAPE")
    page_size = Column(String(10), default='A4', doc="A4, LETTER, LEGAL, etc")
    margins = Column(JSON, doc="Page margins configuration")
    
    # Content configuration
    include_summary = Column(Boolean, default=True)
    include_details = Column(Boolean, default=True)
    include_comparatives = Column(Boolean, default=False)
    include_charts = Column(Boolean, default=False)
    
    # GL specific
    account_range_from = Column(String(10), doc="From GL account")
    account_range_to = Column(String(10), doc="To GL account")
    account_types = Column(Text, doc="Comma-separated account types")
    cost_centers = Column(Text, doc="Comma-separated cost centers")
    
    # Grouping and sorting
    group_by = Column(String(50), doc="Grouping: ACCOUNT_TYPE, COST_CENTER, etc")
    sort_by = Column(String(50), doc="Sorting: ACCOUNT_CODE, DESCRIPTION, BALANCE")
    subtotals = Column(Boolean, default=True)
    page_break_on_group = Column(Boolean, default=False)
    
    # Filters
    exclude_zero_balances = Column(Boolean, default=True)
    exclude_inactive_accounts = Column(Boolean, default=True)
    movement_only = Column(Boolean, default=False)
    
    # Security
    restricted = Column(Boolean, default=False)
    allowed_roles = Column(Text, doc="Comma-separated allowed roles")
    
    # Audit
    created_by = Column(String(30), nullable=False)
    created_date = Column(DateTime(timezone=True), server_default=func.current_timestamp())
    modified_by = Column(String(30))
    modified_date = Column(DateTime(timezone=True), onupdate=func.current_timestamp())
    
    # Status
    active = Column(Boolean, default=True)
    
    # Relationships
    parameters = relationship("GLReportParameterRec", back_populates="report_definition")
    columns = relationship("GLReportColumnRec", back_populates="report_definition")
    schedules = relationship("GLReportScheduleRec", back_populates="report_definition")


class GLReportParameterRec(Base):
    """GL report parameter definition"""
    __tablename__ = "gl_report_parameters"
    
    parameter_id = Column(Integer, primary_key=True, autoincrement=True)
    report_id = Column(Integer, ForeignKey("gl_report_definitions.report_id"), nullable=False)
    
    # Parameter details
    parameter_name = Column(String(50), nullable=False)
    parameter_label = Column(String(100), nullable=False)
    parameter_type = Column(String(20), nullable=False, doc="Type: DATE, PERIOD, ACCOUNT, TEXT, NUMBER, LIST")
    
    # Configuration
    required = Column(Boolean, default=True)
    default_value = Column(String(255))
    sequence = Column(Integer, default=0)
    
    # Validation
    min_value = Column(String(255))
    max_value = Column(String(255))
    valid_values = Column(Text, doc="Comma-separated valid values or SQL query")
    validation_rule = Column(Text, doc="Custom validation rule")
    
    # UI hints
    display_width = Column(Integer)
    help_text = Column(Text)
    placeholder = Column(String(100))
    
    # Dependencies
    depends_on = Column(String(50), doc="Parameter it depends on")
    
    # Relationships
    report_definition = relationship("GLReportDefinitionRec", back_populates="parameters")


class GLReportColumnRec(Base):
    """GL report column definition"""
    __tablename__ = "gl_report_columns"
    
    column_id = Column(Integer, primary_key=True, autoincrement=True)
    report_id = Column(Integer, ForeignKey("gl_report_definitions.report_id"), nullable=False)
    
    # Column details
    column_name = Column(String(50), nullable=False)
    column_heading = Column(String(100), nullable=False)
    column_type = Column(String(20), nullable=False, doc="Type: TEXT, NUMBER, CURRENCY, PERCENTAGE, DATE")
    
    # Position
    sequence = Column(Integer, nullable=False)
    width = Column(Integer, doc="Column width in characters or pixels")
    
    # Data source
    data_source = Column(String(20), doc="Source: FIELD, CALCULATION, CONSTANT")
    field_name = Column(String(100), doc="Field name or expression")
    calculation_formula = Column(Text, doc="Calculation formula")
    
    # Formatting
    format_string = Column(String(50), doc="Format string (e.g., '#,##0.00')")
    alignment = Column(String(10), default='LEFT', doc="LEFT, CENTER, RIGHT")
    
    # Aggregation
    total_function = Column(String(20), doc="Function: SUM, AVG, COUNT, MIN, MAX")
    show_total = Column(Boolean, default=False)
    
    # Conditional formatting
    conditional_format = Column(JSON, doc="Conditional formatting rules")
    
    # Display
    visible = Column(Boolean, default=True)
    print_on_new_page = Column(Boolean, default=False)
    
    # Relationships
    report_definition = relationship("GLReportDefinitionRec", back_populates="columns")


class GLReportScheduleRec(Base):
    """GL report schedule"""
    __tablename__ = "gl_report_schedules"
    
    schedule_id = Column(Integer, primary_key=True, autoincrement=True)
    report_id = Column(Integer, ForeignKey("gl_report_definitions.report_id"), nullable=False)
    
    # Schedule details
    schedule_name = Column(String(100), nullable=False)
    schedule_type = Column(String(20), nullable=False, doc="Type: DAILY, WEEKLY, MONTHLY, PERIOD_END")
    
    # Timing
    run_time = Column(String(5), doc="Run time HH:MM")
    run_day = Column(Integer, doc="Day of week (1-7) or month (1-31)")
    
    # Period-based scheduling
    run_after_period_close = Column(Boolean, default=False)
    days_after_period_close = Column(Integer, default=0)
    
    # Parameters
    parameter_values = Column(JSON, doc="Report parameter values")
    
    # Distribution
    email_to = Column(Text, doc="Email recipients (comma-separated)")
    email_cc = Column(Text)
    email_subject = Column(String(255))
    email_body = Column(Text)
    
    # File output
    output_path = Column(String(255), doc="Output file path")
    output_filename_pattern = Column(String(100), doc="Filename pattern with placeholders")
    
    # Control
    active = Column(Boolean, default=True)
    last_run_date = Column(DateTime(timezone=True))
    next_run_date = Column(DateTime(timezone=True))
    
    # Relationships
    report_definition = relationship("GLReportDefinitionRec", back_populates="schedules")


class GLReportHistoryRec(Base):
    """GL report execution history"""
    __tablename__ = "gl_report_history"
    
    history_id = Column(Integer, primary_key=True, autoincrement=True)
    report_id = Column(Integer, ForeignKey("gl_report_definitions.report_id"), nullable=False)
    schedule_id = Column(Integer, ForeignKey("gl_report_schedules.schedule_id"))
    
    # Execution details
    run_date = Column(DateTime(timezone=True), nullable=False)
    run_by = Column(String(30), nullable=False)
    run_type = Column(String(20), doc="Type: SCHEDULED, MANUAL, API")
    
    # Parameters used
    parameter_values = Column(JSON)
    
    # Period information
    period_year = Column(Integer)
    period_month = Column(Integer)
    as_of_date = Column(Integer, doc="As of date YYYYMMDD")
    
    # Output
    output_format = Column(String(10))
    output_file = Column(String(255))
    file_size = Column(Integer)
    page_count = Column(Integer)
    
    # Performance
    execution_time = Column(Numeric(10, 2), doc="Execution time in seconds")
    records_processed = Column(Integer)
    
    # Status
    status = Column(String(20), nullable=False, doc="Status: SUCCESS, FAILED, CANCELLED")
    error_message = Column(Text)
    
    # Distribution
    distributed = Column(Boolean, default=False)
    distribution_time = Column(DateTime(timezone=True))
    distribution_status = Column(String(20))


class GLReportTemplateRec(Base):
    """GL report templates"""
    __tablename__ = "gl_report_templates"
    
    template_id = Column(Integer, primary_key=True, autoincrement=True)
    template_code = Column(String(20), unique=True, nullable=False)
    template_name = Column(String(100), nullable=False)
    template_type = Column(String(20), doc="Type: EXCEL, WORD, HTML, CUSTOM")
    
    # Template content
    template_file = Column(String(255), doc="Template file path")
    template_content = Column(Text, doc="Template content for database storage")
    
    # Variables
    template_variables = Column(JSON, doc="Available template variables")
    
    # Version
    version = Column(String(10), default='1.0')
    
    # Audit
    created_by = Column(String(30), nullable=False)
    created_date = Column(DateTime(timezone=True), server_default=func.current_timestamp())
    modified_by = Column(String(30))
    modified_date = Column(DateTime(timezone=True), onupdate=func.current_timestamp())


class GLReportBurstingRec(Base):
    """GL report bursting configuration"""
    __tablename__ = "gl_report_bursting"
    
    bursting_id = Column(Integer, primary_key=True, autoincrement=True)
    report_id = Column(Integer, ForeignKey("gl_report_definitions.report_id"), nullable=False)
    
    # Bursting configuration
    burst_by = Column(String(50), nullable=False, doc="Field to burst by: COST_CENTER, DEPARTMENT, etc")
    
    # Distribution rules
    distribution_method = Column(String(20), doc="Method: EMAIL, FILE, PRINT")
    
    # Email mapping
    email_lookup_table = Column(String(50), doc="Table for email lookup")
    email_lookup_field = Column(String(50), doc="Field for email lookup")
    
    # File output
    file_path_pattern = Column(String(255), doc="Path pattern with placeholders")
    file_name_pattern = Column(String(100), doc="Filename pattern with placeholders")
    
    # Control
    active = Column(Boolean, default=True)
    
    # Security
    encrypt_output = Column(Boolean, default=False)
    password_protect = Column(Boolean, default=False)
    password_pattern = Column(String(100), doc="Password pattern or lookup")


class GLReportSecurityRec(Base):
    """GL report security settings"""
    __tablename__ = "gl_report_security"
    
    security_id = Column(Integer, primary_key=True, autoincrement=True)
    report_id = Column(Integer, ForeignKey("gl_report_definitions.report_id"), nullable=False)
    
    # Access control
    role_name = Column(String(50), nullable=False)
    
    # Permissions
    can_view = Column(Boolean, default=True)
    can_run = Column(Boolean, default=True)
    can_schedule = Column(Boolean, default=False)
    can_modify = Column(Boolean, default=False)
    can_delete = Column(Boolean, default=False)
    
    # Data restrictions
    account_restriction = Column(Text, doc="Restricted account ranges")
    cost_center_restriction = Column(Text, doc="Restricted cost centers")
    period_restriction = Column(String(50), doc="Period restriction rule")
    
    # Audit
    granted_by = Column(String(30), nullable=False)
    granted_date = Column(DateTime(timezone=True), nullable=False)


class ReportCategoryRec(Base):
    """Report categories for organization and navigation"""
    __tablename__ = "report_categories"
    
    category_id = Column(Integer, primary_key=True, autoincrement=True)
    category_name = Column(String(50), nullable=False, unique=True)
    category_description = Column(Text)
    
    # Hierarchy
    parent_category_id = Column(Integer, ForeignKey("report_categories.category_id"))
    
    # Display
    display_order = Column(Integer, default=0)
    icon_name = Column(String(50))  # Icon identifier
    color_class = Column(String(20))  # CSS color class
    
    # Status
    is_active = Column(Boolean, default=True)
    
    # Audit
    created_by = Column(String(30), nullable=False)
    created_date = Column(DateTime(timezone=True), server_default=func.current_timestamp())
    modified_by = Column(String(30))
    modified_date = Column(DateTime(timezone=True), onupdate=func.current_timestamp())
    
    # Relationships
    parent_category = relationship("ReportCategoryRec", remote_side=[category_id])
    child_categories = relationship("ReportCategoryRec", back_populates="parent_category")


class ReportUsageStatsRec(Base):
    """Report usage statistics for popularity tracking"""
    __tablename__ = "report_usage_stats"
    
    stat_id = Column(Integer, primary_key=True, autoincrement=True)
    report_id = Column(Integer, ForeignKey("gl_report_definitions.report_id"), nullable=False)
    
    # User and execution info
    user_id = Column(String(30), nullable=False)
    execution_date = Column(DateTime(timezone=True), nullable=False)
    
    # Performance metrics
    execution_time = Column(Numeric(10, 2))  # Seconds
    records_returned = Column(Integer)
    file_size = Column(Integer)  # Bytes
    
    # Usage context
    execution_type = Column(String(20))  # MANUAL, SCHEDULED, API
    parameters_used = Column(JSON)  # Parameters passed to report
    output_format = Column(String(10))  # PDF, EXCEL, CSV
    
    # Session info
    session_id = Column(String(50))
    ip_address = Column(String(45))  # IPv6 compatible
    user_agent = Column(Text)
    
    # Status
    status = Column(String(20), nullable=False)  # SUCCESS, FAILED, CANCELLED
    error_message = Column(Text)
    
    # Relationships
    report_definition = relationship("GLReportDefinitionRec")