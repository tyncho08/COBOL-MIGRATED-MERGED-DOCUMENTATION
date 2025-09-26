"""Sales Ledger Comprehensive Schemas"""

from pydantic import BaseModel, Field, field_validator, ConfigDict
from typing import Optional, List, Dict, Any
from datetime import datetime, date
from decimal import Decimal
from enum import Enum


# Enums
class InvoiceStatus(str, Enum):
    DRAFT = "draft"
    POSTED = "posted"
    PAID = "paid"
    PARTIAL = "partial"
    OVERDUE = "overdue"
    VOID = "void"


class PaymentMethod(str, Enum):
    CASH = "cash"
    CHECK = "check"
    TRANSFER = "transfer"
    CREDIT_CARD = "credit_card"
    DEBIT_CARD = "debit_card"
    ACH = "ach"
    WIRE = "wire"
    OTHER = "other"


class CustomerStatus(str, Enum):
    ACTIVE = "active"
    INACTIVE = "inactive"
    SUSPENDED = "suspended"
    BLACKLISTED = "blacklisted"


class CreditStatus(str, Enum):
    GOOD = "good"
    WARNING = "warning"
    HOLD = "hold"
    BLOCKED = "blocked"


class StatementFrequency(str, Enum):
    WEEKLY = "weekly"
    MONTHLY = "monthly"
    QUARTERLY = "quarterly"
    ON_DEMAND = "on_demand"


# Customer Schemas
class CustomerBase(BaseModel):
    customer_code: str = Field(..., min_length=3, max_length=20)
    customer_name: str = Field(..., min_length=3, max_length=100)
    contact_person: Optional[str] = None
    email: Optional[str] = None
    phone: Optional[str] = None
    fax: Optional[str] = None
    billing_address1: str
    billing_address2: Optional[str] = None
    billing_city: str
    billing_state: str
    billing_zip: str
    billing_country: str = "USA"
    credit_limit: Decimal = Field(default=0, ge=0)
    payment_terms: str = Field(default="NET30")
    discount_percent: Decimal = Field(default=0, ge=0, le=100)
    tax_exempt: bool = False
    tax_id: Optional[str] = None
    currency_code: str = Field(default="USD", max_length=3)
    price_list_code: Optional[str] = None
    salesperson_code: Optional[str] = None
    territory_code: Optional[str] = None


class CustomerCreate(CustomerBase):
    shipping_same_as_billing: bool = True
    shipping_address1: Optional[str] = None
    shipping_address2: Optional[str] = None
    shipping_city: Optional[str] = None
    shipping_state: Optional[str] = None
    shipping_zip: Optional[str] = None
    shipping_country: Optional[str] = None


class CustomerUpdate(BaseModel):
    customer_name: Optional[str] = None
    contact_person: Optional[str] = None
    email: Optional[str] = None
    phone: Optional[str] = None
    credit_limit: Optional[Decimal] = None
    payment_terms: Optional[str] = None
    status: Optional[CustomerStatus] = None
    credit_status: Optional[CreditStatus] = None


class Customer(CustomerBase):
    id: int
    status: CustomerStatus
    credit_status: CreditStatus
    current_balance: Decimal
    credit_available: Decimal
    ytd_sales: Decimal
    last_payment_date: Optional[date] = None
    last_payment_amount: Optional[Decimal] = None
    days_since_last_payment: Optional[int] = None
    average_days_to_pay: Optional[float] = None
    created_at: datetime
    updated_at: Optional[datetime] = None
    
    model_config = ConfigDict(from_attributes=True)


class CustomerSummary(BaseModel):
    customer: Customer
    open_invoices: int
    overdue_invoices: int
    total_outstanding: Decimal
    current_amount: Decimal
    overdue_30_days: Decimal
    overdue_60_days: Decimal
    overdue_90_days: Decimal
    overdue_over_90: Decimal
    credit_analysis: Dict[str, Any]
    recent_transactions: List[Dict[str, Any]]


# Invoice Schemas
class InvoiceLineBase(BaseModel):
    line_number: int = Field(..., ge=1)
    item_code: str
    description: str
    quantity: Decimal = Field(..., gt=0)
    unit_price: Decimal = Field(..., ge=0)
    discount_percent: Decimal = Field(default=0, ge=0, le=100)
    tax_code: Optional[str] = None
    tax_rate: Decimal = Field(default=0)
    gl_account: Optional[str] = None
    department: Optional[str] = None
    project: Optional[str] = None


class InvoiceLine(InvoiceLineBase):
    id: int
    line_amount: Decimal
    discount_amount: Decimal
    tax_amount: Decimal
    net_amount: Decimal
    
    model_config = ConfigDict(from_attributes=True)


class InvoiceBase(BaseModel):
    customer_code: str
    invoice_date: date
    due_date: Optional[date] = None
    po_number: Optional[str] = None
    reference: Optional[str] = None
    terms: Optional[str] = None
    ship_date: Optional[date] = None
    ship_via: Optional[str] = None
    fob: Optional[str] = None
    lines: List[InvoiceLineBase]


class InvoiceCreate(InvoiceBase):
    manual_number: Optional[str] = None
    apply_customer_discount: bool = True
    notes: Optional[str] = None


class InvoiceUpdate(BaseModel):
    due_date: Optional[date] = None
    po_number: Optional[str] = None
    reference: Optional[str] = None
    notes: Optional[str] = None
    lines: Optional[List[InvoiceLineBase]] = None


class Invoice(InvoiceBase):
    id: int
    invoice_number: str
    status: InvoiceStatus
    lines: List[InvoiceLine]
    subtotal: Decimal
    discount_amount: Decimal
    tax_amount: Decimal
    freight_amount: Decimal
    total_amount: Decimal
    paid_amount: Decimal
    balance_due: Decimal
    days_overdue: Optional[int] = None
    created_by: int
    created_at: datetime
    posted_at: Optional[datetime] = None
    last_payment_date: Optional[date] = None
    
    model_config = ConfigDict(from_attributes=True)


class InvoicePost(BaseModel):
    posting_date: Optional[date] = None
    create_gl_entries: bool = True


# Payment Schemas
class PaymentAllocationBase(BaseModel):
    invoice_number: str
    amount_applied: Decimal = Field(..., gt=0)
    discount_taken: Decimal = Field(default=0, ge=0)


class PaymentAllocation(PaymentAllocationBase):
    id: int
    invoice_id: int
    remaining_balance: Decimal
    
    model_config = ConfigDict(from_attributes=True)


class PaymentBase(BaseModel):
    customer_code: str
    payment_date: date
    payment_method: PaymentMethod
    reference_number: Optional[str] = None
    amount: Decimal = Field(..., gt=0)
    bank_account: Optional[str] = None
    check_number: Optional[str] = None
    allocations: List[PaymentAllocationBase]


class PaymentCreate(PaymentBase):
    auto_allocate: bool = False
    notes: Optional[str] = None


class Payment(PaymentBase):
    id: int
    receipt_number: str
    allocations: List[PaymentAllocation]
    allocated_amount: Decimal
    unallocated_amount: Decimal
    gl_posted: bool
    created_by: int
    created_at: datetime
    posted_at: Optional[datetime] = None
    
    model_config = ConfigDict(from_attributes=True)


# Credit Note Schemas
class CreditNoteLineBase(BaseModel):
    line_number: int
    invoice_line_id: Optional[int] = None
    item_code: str
    description: str
    quantity: Decimal
    unit_price: Decimal
    tax_code: Optional[str] = None
    tax_rate: Decimal = Field(default=0)
    gl_account: Optional[str] = None


class CreditNoteLine(CreditNoteLineBase):
    id: int
    line_amount: Decimal
    tax_amount: Decimal
    total_amount: Decimal
    
    model_config = ConfigDict(from_attributes=True)


class CreditNoteBase(BaseModel):
    customer_code: str
    credit_date: date
    original_invoice_number: Optional[str] = None
    reason: str
    lines: List[CreditNoteLineBase]


class CreditNoteCreate(CreditNoteBase):
    auto_allocate: bool = True


class CreditNote(CreditNoteBase):
    id: int
    credit_note_number: str
    status: str
    lines: List[CreditNoteLine]
    subtotal: Decimal
    tax_amount: Decimal
    total_amount: Decimal
    allocated_amount: Decimal
    unallocated_amount: Decimal
    created_by: int
    created_at: datetime
    posted_at: Optional[datetime] = None
    
    model_config = ConfigDict(from_attributes=True)


# Statement Schemas
class StatementLine(BaseModel):
    transaction_date: date
    transaction_type: str
    reference_number: str
    description: str
    debit_amount: Decimal
    credit_amount: Decimal
    balance: Decimal


class CustomerStatement(BaseModel):
    customer_code: str
    customer_name: str
    statement_date: date
    from_date: date
    to_date: date
    opening_balance: Decimal
    closing_balance: Decimal
    total_charges: Decimal
    total_credits: Decimal
    lines: List[StatementLine]
    aging_summary: Dict[str, Decimal]
    overdue_amount: Decimal


# Aged Analysis Schemas
class AgedBalanceDetail(BaseModel):
    invoice_number: str
    invoice_date: date
    due_date: date
    original_amount: Decimal
    paid_amount: Decimal
    balance: Decimal
    days_overdue: int
    aging_bucket: str


class CustomerAgedBalance(BaseModel):
    customer_code: str
    customer_name: str
    credit_limit: Decimal
    current_balance: Decimal
    current: Decimal
    overdue_30: Decimal
    overdue_60: Decimal
    overdue_90: Decimal
    overdue_120_plus: Decimal
    details: List[AgedBalanceDetail]


class AgedAnalysisSummary(BaseModel):
    as_of_date: date
    total_outstanding: Decimal
    current: Decimal
    overdue_30: Decimal
    overdue_60: Decimal
    overdue_90: Decimal
    overdue_120_plus: Decimal
    customer_count: int
    overdue_customer_count: int
    customers: List[CustomerAgedBalance]


# Credit Control Schemas
class CreditHoldRequest(BaseModel):
    customer_code: str
    reason: str
    hold_orders: bool = True
    hold_shipments: bool = True
    notes: Optional[str] = None


class CreditReleaseRequest(BaseModel):
    customer_code: str
    reason: str
    release_orders: bool = True
    release_shipments: bool = True
    notes: Optional[str] = None


class CollectionNote(BaseModel):
    customer_code: str
    contact_date: datetime
    contact_method: str
    spoke_with: Optional[str] = None
    promise_date: Optional[date] = None
    promise_amount: Optional[Decimal] = None
    notes: str
    follow_up_date: Optional[date] = None


class DunningLetter(BaseModel):
    customer_code: str
    letter_level: int = Field(..., ge=1, le=5)
    letter_date: date
    due_amount: Decimal
    days_overdue: int
    letter_template: str


# Response Models
class InvoiceListResponse(BaseModel):
    invoices: List[Invoice]
    total: int
    skip: int
    limit: int


class PaymentListResponse(BaseModel):
    payments: List[Payment]
    total: int
    skip: int
    limit: int


class CustomerListResponse(BaseModel):
    customers: List[Customer]
    total: int
    skip: int
    limit: int


# Batch Processing Schemas
class BatchInvoicePost(BaseModel):
    invoice_ids: List[int]
    posting_date: Optional[date] = None


class BatchPaymentAllocation(BaseModel):
    payment_ids: List[int]
    allocation_method: str = Field(default="FIFO", pattern="^(FIFO|LIFO|MANUAL)$")


class BatchStatementGeneration(BaseModel):
    customer_codes: Optional[List[str]] = None
    statement_date: date
    include_zero_balance: bool = False
    email_statements: bool = False


# Sales Analysis Schemas
class SalesAnalysisPeriod(BaseModel):
    period_name: str
    start_date: date
    end_date: date
    invoice_count: int
    total_sales: Decimal
    total_cost: Decimal
    gross_profit: Decimal
    gross_margin_percent: float


class CustomerSalesAnalysis(BaseModel):
    customer_code: str
    customer_name: str
    periods: List[SalesAnalysisPeriod]
    ytd_sales: Decimal
    ytd_cost: Decimal
    ytd_profit: Decimal
    ytd_margin_percent: float
    sales_trend: str
    top_products: List[Dict[str, Any]]