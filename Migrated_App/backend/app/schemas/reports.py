"""
Report Schemas

Pydantic schemas for report generation, scheduling, and management.
Handles validation and serialization for all report-related data.
"""

from typing import Dict, List, Any, Optional, Union
from datetime import datetime, date
from enum import Enum
from pydantic import BaseModel, Field, field_validator
from decimal import Decimal


class ExportFormat(str, Enum):
    """Export format options"""
    JSON = "json"
    PDF = "pdf" 
    EXCEL = "excel"
    CSV = "csv"


class ScheduleFrequency(str, Enum):
    """Schedule frequency options"""
    HOURLY = "hourly"
    DAILY = "daily"
    WEEKLY = "weekly"
    MONTHLY = "monthly"
    QUARTERLY = "quarterly"
    YEARLY = "yearly"


class ReportParameter(BaseModel):
    """Individual report parameter"""
    name: str
    value: Any
    data_type: str = "string"  # string, integer, decimal, date, boolean


class ReportRequest(BaseModel):
    """Request to generate a report"""
    report_type: str = Field(..., description="Type of report to generate")
    title: Optional[str] = Field(None, description="Custom title for the report")
    parameters: Dict[str, Any] = Field(default_factory=dict, description="Report parameters")
    format: ExportFormat = Field(ExportFormat.JSON, description="Output format")
    
    @field_validator('report_type')
    @classmethod
    def validate_report_type(cls, v):
        allowed_types = [
            'trial_balance', 'profit_loss', 'balance_sheet',
            'customer_aging', 'supplier_aging', 'stock_valuation',
            'sales_analysis', 'purchase_analysis'
        ]
        if v not in allowed_types:
            raise ValueError(f"Invalid report type. Must be one of: {', '.join(allowed_types)}")
        return v


class ReportResponse(BaseModel):
    """Response from report generation"""
    report_id: str
    report_type: str
    title: str
    generated_at: datetime
    parameters: Dict[str, Any]
    data: Union[List[Dict[str, Any]], Dict[str, Any]]
    total_records: int
    format: ExportFormat
    
    class Config:
        json_encoders = {
            datetime: lambda v: v.isoformat(),
            Decimal: lambda v: float(v)
        }


class ReportListResponse(BaseModel):
    """Response for report listing"""
    reports: List[Dict[str, Any]]
    total_count: int
    limit: int
    offset: int


# Trial Balance Schemas
class TrialBalanceEntry(BaseModel):
    """Single entry in trial balance"""
    account_code: str
    account_name: str
    account_type: str
    account_category: str
    debit_amount: Decimal = Field(default=Decimal('0.00'))
    credit_amount: Decimal = Field(default=Decimal('0.00'))
    balance: Decimal = Field(default=Decimal('0.00'))


class TrialBalanceReport(BaseModel):
    """Trial balance report data"""
    as_at_date: date
    entries: List[TrialBalanceEntry]
    total_debits: Decimal
    total_credits: Decimal
    
    class Config:
        json_encoders = {
            Decimal: lambda v: float(v),
            date: lambda v: v.isoformat()
        }


# Profit & Loss Schemas
class ProfitLossEntry(BaseModel):
    """Single entry in P&L"""
    account_code: str
    account_name: str
    amount: Decimal
    
    class Config:
        json_encoders = {Decimal: lambda v: float(v)}


class ProfitLossReport(BaseModel):
    """Profit & Loss report data"""
    period_from: date
    period_to: date
    income: List[ProfitLossEntry]
    income_total: Decimal
    expenses: List[ProfitLossEntry]
    expense_total: Decimal
    net_profit: Decimal
    
    class Config:
        json_encoders = {
            Decimal: lambda v: float(v),
            date: lambda v: v.isoformat()
        }


# Balance Sheet Schemas
class BalanceSheetEntry(BaseModel):
    """Single entry in balance sheet"""
    account_code: str
    account_name: str
    account_category: str
    amount: Decimal
    
    class Config:
        json_encoders = {Decimal: lambda v: float(v)}


class BalanceSheetReport(BaseModel):
    """Balance sheet report data"""
    as_at_date: date
    assets: List[BalanceSheetEntry]
    total_assets: Decimal
    liabilities: List[BalanceSheetEntry]
    total_liabilities: Decimal
    equity: List[BalanceSheetEntry]
    total_equity: Decimal
    
    class Config:
        json_encoders = {
            Decimal: lambda v: float(v),
            date: lambda v: v.isoformat()
        }


# Aging Report Schemas
class CustomerAgingEntry(BaseModel):
    """Single entry in customer aging"""
    customer_code: str
    customer_name: str
    credit_limit: Decimal = Field(default=Decimal('0.00'))
    current: Decimal = Field(default=Decimal('0.00'))
    days_31_60: Decimal = Field(default=Decimal('0.00'))
    days_61_90: Decimal = Field(default=Decimal('0.00'))
    days_over_90: Decimal = Field(default=Decimal('0.00'))
    total_outstanding: Decimal = Field(default=Decimal('0.00'))
    
    class Config:
        json_encoders = {Decimal: lambda v: float(v)}


class SupplierAgingEntry(BaseModel):
    """Single entry in supplier aging"""
    supplier_code: str
    supplier_name: str
    current: Decimal = Field(default=Decimal('0.00'))
    days_31_60: Decimal = Field(default=Decimal('0.00'))
    days_61_90: Decimal = Field(default=Decimal('0.00'))
    days_over_90: Decimal = Field(default=Decimal('0.00'))
    total_outstanding: Decimal = Field(default=Decimal('0.00'))
    
    class Config:
        json_encoders = {Decimal: lambda v: float(v)}


# Stock Valuation Schemas
class StockValuationEntry(BaseModel):
    """Single entry in stock valuation"""
    item_code: str
    item_name: str
    item_category: str
    costing_method: str
    quantity_on_hand: int
    unit_cost: Decimal
    total_cost: Decimal
    standard_cost: Decimal = Field(default=Decimal('0.00'))
    
    class Config:
        json_encoders = {Decimal: lambda v: float(v)}


# Sales Analysis Schemas
class SalesAnalysisEntry(BaseModel):
    """Single entry in sales analysis"""
    customer_code: str
    customer_name: str
    invoice_count: int
    sales_total: Decimal
    average_sale: Decimal
    
    class Config:
        json_encoders = {Decimal: lambda v: float(v)}


# Purchase Analysis Schemas
class PurchaseAnalysisEntry(BaseModel):
    """Single entry in purchase analysis"""
    supplier_code: str
    supplier_name: str
    invoice_count: int
    purchase_total: Decimal
    average_purchase: Decimal
    
    class Config:
        json_encoders = {Decimal: lambda v: float(v)}


# Custom Report Builder Schemas
class ReportField(BaseModel):
    """Field definition for custom reports"""
    field_key: str = Field(..., description="Unique field identifier")
    label: str = Field(..., description="Display label for the field")
    aggregation: Optional[str] = Field(None, description="Aggregation function (sum, count, avg, max, min)")
    
    @field_validator('aggregation')
    @classmethod
    def validate_aggregation(cls, v):
        if v and v not in ['sum', 'count', 'avg', 'max', 'min']:
            raise ValueError("Invalid aggregation function")
        return v


class ReportFilter(BaseModel):
    """Filter definition for custom reports"""
    field_key: str = Field(..., description="Field to filter on")
    operator: str = Field(..., description="Filter operator")
    value: Any = Field(..., description="Filter value")
    
    @field_validator('operator')
    @classmethod
    def validate_operator(cls, v):
        allowed_operators = [
            'equals', 'not_equals', 'greater_than', 'less_than',
            'greater_equal', 'less_equal', 'contains', 'starts_with',
            'ends_with', 'in', 'between', 'is_null', 'is_not_null'
        ]
        if v not in allowed_operators:
            raise ValueError(f"Invalid operator. Must be one of: {', '.join(allowed_operators)}")
        return v


class CustomReportRequest(BaseModel):
    """Request to build a custom report"""
    report_name: Optional[str] = Field(None, description="Name for the custom report")
    fields: List[ReportField] = Field(..., description="Fields to include in the report")
    filters: Optional[List[ReportFilter]] = Field(None, description="Filters to apply")
    group_by: Optional[List[str]] = Field(None, description="Fields to group by")
    sort_by: Optional[List[Dict[str, str]]] = Field(None, description="Sorting specification")
    limit: Optional[int] = Field(None, ge=1, le=10000, description="Maximum number of records")


# Scheduling Schemas
class ScheduledReport(BaseModel):
    """Scheduled report configuration"""
    report_request: ReportRequest
    frequency: ScheduleFrequency
    recipients: Optional[List[str]] = Field(None, description="Email recipients")
    start_date: Optional[datetime] = Field(None, description="When to start the schedule")
    end_date: Optional[datetime] = Field(None, description="When to end the schedule")
    
    @field_validator('recipients')
    @classmethod
    def validate_recipients(cls, v):
        if v:
            # Basic email validation
            import re
            email_pattern = re.compile(r'^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$')
            for email in v:
                if not email_pattern.match(email):
                    raise ValueError(f"Invalid email address: {email}")
        return v


class ScheduledReportResponse(BaseModel):
    """Response for scheduled report information"""
    job_id: str
    report_type: str
    title: str
    next_run: Optional[datetime]
    last_run: Optional[datetime]
    status: str
    recipients: List[str]
    created_at: datetime
    created_by: str
    retry_count: int
    
    class Config:
        json_encoders = {datetime: lambda v: v.isoformat()}


class JobHistoryResponse(BaseModel):
    """Response for job execution history"""
    execution_id: str
    job_id: str
    executed_at: datetime
    status: str
    duration_seconds: int
    report_id: Optional[str]
    error_message: Optional[str]
    
    class Config:
        json_encoders = {datetime: lambda v: v.isoformat()}


# Report Template Schemas
class ReportTemplate(BaseModel):
    """Report template definition"""
    template_id: str
    name: str
    description: str
    category: str
    report_type: str
    parameters: List[str]
    is_custom: bool = False
    created_by: Optional[str] = None
    created_at: Optional[datetime] = None
    
    class Config:
        json_encoders = {datetime: lambda v: v.isoformat()}


# Export Schemas
class ExportRequest(BaseModel):
    """Request to export a report"""
    report_id: str
    format: ExportFormat
    template: Optional[str] = Field(None, description="Template to use for formatting")


class ExportResponse(BaseModel):
    """Response from export operation"""
    file_id: str
    download_url: str
    file_size: int
    expires_at: datetime
    
    class Config:
        json_encoders = {datetime: lambda v: v.isoformat()}


# Validation Schemas
class ValidationError(BaseModel):
    """Validation error details"""
    field: str
    message: str
    code: str


class ValidationResponse(BaseModel):
    """Response from parameter validation"""
    valid: bool
    errors: List[ValidationError]
    warnings: List[ValidationError]


# Dashboard/Summary Schemas
class ReportSummary(BaseModel):
    """Summary statistics for reports"""
    total_reports_generated: int
    reports_generated_today: int
    scheduled_reports: int
    failed_reports: int
    most_popular_report: Optional[str]
    last_generated: Optional[datetime]
    
    class Config:
        json_encoders = {datetime: lambda v: v.isoformat()}


# File Management Schemas
class ReportFile(BaseModel):
    """Report file information"""
    file_id: str
    report_id: str
    filename: str
    file_size: int
    format: ExportFormat
    created_at: datetime
    expires_at: Optional[datetime]
    download_count: int = 0
    
    class Config:
        json_encoders = {datetime: lambda v: v.isoformat()}