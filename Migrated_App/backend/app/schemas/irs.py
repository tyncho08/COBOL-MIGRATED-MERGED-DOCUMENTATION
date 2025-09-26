"""IRS Module Pydantic Schemas"""

from pydantic import BaseModel, Field, field_validator, ConfigDict
from typing import Optional, List, Dict, Any, Union
from datetime import datetime, date
from decimal import Decimal
from enum import Enum


# Base schemas
class IRSBase(BaseModel):
    """Base schema for IRS module"""
    model_config = ConfigDict(
        from_attributes=True,
        json_encoders={
            datetime: lambda v: v.isoformat(),
            date: lambda v: v.isoformat(),
            Decimal: lambda v: float(v)
        }
    )


# Company Configuration Schemas
class CompanyConfigBase(BaseModel):
    company_name: str
    employer_id_number: str = Field(..., pattern="^[0-9]{2}-[0-9]{7}$")
    business_type: str
    fiscal_year_end: str = Field(..., pattern="^(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])$")
    tax_filing_status: str
    state: str = Field(..., min_length=2, max_length=2)
    is_active: bool = True


class CompanyConfigCreate(CompanyConfigBase):
    pass


class CompanyConfigUpdate(BaseModel):
    company_name: Optional[str] = None
    business_type: Optional[str] = None
    fiscal_year_end: Optional[str] = None
    tax_filing_status: Optional[str] = None
    is_active: Optional[bool] = None


class CompanyConfig(CompanyConfigBase, IRSBase):
    id: int
    created_at: datetime
    updated_at: Optional[datetime] = None


# Transaction Schemas
class TransactionType(str, Enum):
    INCOME = "INCOME"
    EXPENSE = "EXPENSE"
    ASSET = "ASSET"
    LIABILITY = "LIABILITY"
    EQUITY = "EQUITY"


class IRSTransactionBase(BaseModel):
    company_id: int
    transaction_date: date
    transaction_type: TransactionType
    account_code: str
    description: str
    amount: Decimal = Field(...)
    reference_number: Optional[str] = None
    posted: bool = False


class IRSTransactionCreate(IRSTransactionBase):
    pass


class IRSTransactionUpdate(BaseModel):
    transaction_date: Optional[date] = None
    description: Optional[str] = None
    amount: Optional[Decimal] = None
    reference_number: Optional[str] = None


class IRSTransaction(IRSTransactionBase, IRSBase):
    id: int
    posted_at: Optional[datetime] = None
    posted_by: Optional[int] = None
    voided: bool = False
    voided_at: Optional[datetime] = None


class BatchTransaction(BaseModel):
    transactions: List[IRSTransactionCreate]
    batch_description: str


class TransactionValidation(BaseModel):
    is_valid: bool
    errors: List[str] = []
    warnings: List[str] = []


class TransactionSummary(BaseModel):
    company_id: int
    start_date: date
    end_date: date
    total_income: Decimal
    total_expenses: Decimal
    net_income: Decimal
    transaction_count: int
    by_type: Dict[str, Dict[str, Union[Decimal, int]]]


# Bank Reconciliation Schemas
class BankStatement(BaseModel):
    id: int
    company_id: int
    bank_account_id: int
    statement_date: date
    beginning_balance: Decimal
    ending_balance: Decimal
    total_deposits: Decimal
    total_withdrawals: Decimal
    uploaded_at: datetime
    uploaded_by: int


class BankTransaction(BaseModel):
    id: int
    statement_id: int
    transaction_date: date
    description: str
    amount: Decimal
    transaction_type: str  # DEPOSIT or WITHDRAWAL
    reference_number: Optional[str] = None
    matched: bool = False
    matched_transaction_id: Optional[int] = None


class BankReconciliationBase(BaseModel):
    statement_id: int
    reconciliation_date: date
    book_balance: Decimal
    statement_balance: Decimal
    status: str = "draft"  # draft, completed, approved


class BankReconciliationCreate(BankReconciliationBase):
    pass


class BankReconciliationUpdate(BaseModel):
    status: Optional[str] = None
    notes: Optional[str] = None


class BankReconciliation(BankReconciliationBase, IRSBase):
    id: int
    created_by: int
    completed_at: Optional[datetime] = None
    approved_at: Optional[datetime] = None
    approved_by: Optional[int] = None


class ReconciliationItem(BaseModel):
    id: int
    reconciliation_id: int
    item_type: str  # outstanding_check, deposit_in_transit, bank_charge, interest
    description: str
    amount: Decimal
    cleared: bool = False


class ReconciliationSummary(BaseModel):
    reconciliation_id: int
    book_balance: Decimal
    add_deposits_in_transit: Decimal
    less_outstanding_checks: Decimal
    adjusted_book_balance: Decimal
    statement_balance: Decimal
    difference: Decimal
    is_balanced: bool


# Tax Calculation Schemas
class TaxCalculationRequest(BaseModel):
    company_id: int
    tax_year: int
    income_data: Dict[str, Decimal]
    deductions: List[Dict[str, Any]]
    credits: List[Dict[str, Any]]
    filing_status: str
    entity_type: str = "individual"


class TaxCalculationResponse(BaseModel):
    gross_income: Decimal
    adjusted_gross_income: Decimal
    taxable_income: Decimal
    tax_before_credits: Decimal
    total_credits: Decimal
    total_tax: Decimal
    effective_rate: Decimal
    marginal_rate: Decimal
    calculation_details: Dict[str, Any]


class TaxBracket(BaseModel):
    min_income: Decimal
    max_income: Optional[Decimal] = None
    tax_rate: Decimal
    base_tax: Decimal


class TaxDeduction(BaseModel):
    deduction_type: str
    description: str
    amount: Decimal
    category: str
    is_itemized: bool = False


class TaxCredit(BaseModel):
    credit_type: str
    description: str
    amount: Decimal
    is_refundable: bool = False
    income_limit: Optional[Decimal] = None


class EstimatedTax(BaseModel):
    company_id: int
    tax_year: int
    total_estimated_tax: Decimal
    quarterly_payment: Decimal
    safe_harbor_amount: Optional[Decimal] = None
    payment_schedule: List[Dict[str, Any]]


class QuarterlyPayment(BaseModel):
    company_id: int
    tax_year: int
    quarter: int  # 1-4
    due_date: date
    amount_due: Decimal
    amount_paid: Optional[Decimal] = None
    payment_date: Optional[date] = None
    confirmation_number: Optional[str] = None


class TaxLiability(BaseModel):
    company_id: int
    tax_year: int
    total_tax: Decimal
    payments_made: Decimal
    estimated_payments: Decimal
    withholdings: Decimal
    credits: Decimal
    balance_due: Decimal
    overpayment: Decimal


# Tax Return Schemas
class TaxReturnBase(BaseModel):
    company_id: int
    tax_year: int
    filing_status: str
    form_type: str  # 1040, 1065, 1120, etc.


class TaxReturnCreate(TaxReturnBase):
    pass


class TaxReturnUpdate(BaseModel):
    filing_status: Optional[str] = None
    amended: bool = False


class TaxReturn(TaxReturnBase, IRSBase):
    id: int
    status: str  # draft, under_review, approved, filed, accepted, rejected
    created_at: datetime
    filed_at: Optional[datetime] = None
    accepted_at: Optional[datetime] = None


class TaxReturnForm(BaseModel):
    id: int
    return_id: int
    form_type: str
    form_data: Dict[str, Any]
    created_at: datetime


class TaxReturnSchedule(BaseModel):
    id: int
    return_id: int
    schedule_type: str  # A, B, C, D, E, etc.
    schedule_data: Dict[str, Any]


class TaxReturnStatus(BaseModel):
    return_id: int
    status: str
    last_updated: datetime
    irs_status: Optional[str] = None
    confirmation_number: Optional[str] = None


class TaxReturnValidation(BaseModel):
    is_valid: bool
    errors: List[Dict[str, str]] = []
    warnings: List[Dict[str, str]] = []
    missing_forms: List[str] = []
    validation_timestamp: datetime


class FilingConfirmation(BaseModel):
    return_id: int
    submission_id: str
    filing_method: str
    filed_at: datetime
    confirmation_number: str
    status: str
    estimated_processing_days: int


# Electronic Filing Schemas
class EfilingConfiguration(BaseModel):
    company_id: int
    efin: str  # Electronic Filing Identification Number
    username: str
    password: str  # Should be encrypted
    is_active: bool = True
    test_mode: bool = True


class EfilingSubmission(BaseModel):
    id: str
    return_id: int
    submission_date: datetime
    status: str  # pending, transmitted, accepted, rejected
    test_mode: bool
    irs_submission_id: Optional[str] = None


class EfilingStatus(BaseModel):
    submission_id: str
    status: str
    irs_status_code: Optional[str] = None
    last_updated: datetime
    message: Optional[str] = None


class EfilingValidation(BaseModel):
    is_valid: bool
    errors: List[Dict[str, str]] = []
    warnings: List[Dict[str, str]] = []
    schema_version: str
    timestamp: datetime


class TransmissionReceipt(BaseModel):
    submission_id: str
    irs_submission_id: str
    received_timestamp: datetime
    acknowledgment_timestamp: Optional[datetime] = None
    status_code: str
    status_message: str


class EfilingError(BaseModel):
    error_code: str
    error_message: str
    field_xpath: Optional[str] = None
    severity: str  # error, warning


class BatchSubmission(BaseModel):
    batch_id: str
    return_count: int
    submissions: List[EfilingSubmission]
    created_at: datetime


class EfilingHistory(BaseModel):
    submission_id: str
    return_id: int
    company_id: int
    tax_year: int
    form_type: str
    submission_date: datetime
    status: str
    test_mode: bool


# Audit Trail Schemas
class AuditEntry(BaseModel):
    id: int
    entity_type: str
    entity_id: int
    event_type: str
    description: str
    user_id: int
    username: str
    timestamp: datetime
    ip_address: Optional[str] = None
    changes: Dict[str, Any] = {}
    metadata: Dict[str, Any] = {}


class AuditFilter(BaseModel):
    company_id: Optional[int] = None
    entity_type: Optional[str] = None
    entity_id: Optional[int] = None
    user_id: Optional[int] = None
    event_type: Optional[str] = None
    start_date: Optional[datetime] = None
    end_date: Optional[datetime] = None


class AuditSummary(BaseModel):
    total_events: int
    events_by_type: Dict[str, int]
    events_by_user: Dict[str, int]
    events_by_entity: Dict[str, int]
    peak_activity_hour: int
    average_events_per_day: float


class AuditReport(BaseModel):
    report_id: str
    generated_at: datetime
    period_start: date
    period_end: date
    total_entries: int
    summary: AuditSummary
    entries: List[AuditEntry]


class ComplianceReport(BaseModel):
    report_date: date
    compliance_status: str  # compliant, non_compliant, needs_review
    total_transactions: int
    flagged_transactions: int
    missing_documentation: List[Dict[str, Any]]
    recommendations: List[str]


