"""Purchase Ledger Comprehensive Schemas"""

from pydantic import BaseModel, Field, field_validator, ConfigDict
from typing import Optional, List, Dict, Any
from datetime import datetime, date
from decimal import Decimal
from enum import Enum


# Enums
class PurchaseOrderStatus(str, Enum):
    DRAFT = "draft"
    APPROVED = "approved"
    SENT = "sent"
    PARTIAL = "partial"
    RECEIVED = "received"
    CANCELLED = "cancelled"
    CLOSED = "closed"


class PurchaseInvoiceStatus(str, Enum):
    DRAFT = "draft"
    PENDING_APPROVAL = "pending_approval"
    APPROVED = "approved"
    POSTED = "posted"
    PAID = "paid"
    PARTIAL = "partial"
    DISPUTED = "disputed"
    VOID = "void"


class SupplierStatus(str, Enum):
    ACTIVE = "active"
    INACTIVE = "inactive"
    SUSPENDED = "suspended"
    BLACKLISTED = "blacklisted"


class PaymentTerms(str, Enum):
    IMMEDIATE = "immediate"
    NET10 = "net10"
    NET30 = "net30"
    NET45 = "net45"
    NET60 = "net60"
    NET90 = "net90"
    EOM = "eom"  # End of month
    CUSTOM = "custom"


class ApprovalStatus(str, Enum):
    PENDING = "pending"
    APPROVED = "approved"
    REJECTED = "rejected"
    ESCALATED = "escalated"


# Supplier Schemas
class SupplierBase(BaseModel):
    supplier_code: str = Field(..., min_length=3, max_length=20)
    supplier_name: str = Field(..., min_length=3, max_length=100)
    contact_person: Optional[str] = None
    email: Optional[str] = None
    phone: Optional[str] = None
    fax: Optional[str] = None
    address1: str
    address2: Optional[str] = None
    city: str
    state: str
    zip_code: str
    country: str = "USA"
    tax_id: Optional[str] = None
    payment_terms: PaymentTerms = PaymentTerms.NET30
    payment_method: str = Field(default="CHECK")
    bank_account: Optional[str] = None
    currency_code: str = Field(default="USD", max_length=3)
    is_1099_vendor: bool = False
    w9_on_file: bool = False


class SupplierCreate(SupplierBase):
    credit_limit: Decimal = Field(default=0, ge=0)
    discount_percent: Decimal = Field(default=0, ge=0, le=100)
    early_payment_discount: Decimal = Field(default=0, ge=0, le=100)
    early_payment_days: int = Field(default=0, ge=0)
    default_gl_account: Optional[str] = None
    default_department: Optional[str] = None


class SupplierUpdate(BaseModel):
    supplier_name: Optional[str] = None
    contact_person: Optional[str] = None
    email: Optional[str] = None
    phone: Optional[str] = None
    payment_terms: Optional[PaymentTerms] = None
    credit_limit: Optional[Decimal] = None
    status: Optional[SupplierStatus] = None


class Supplier(SupplierBase):
    id: int
    status: SupplierStatus
    credit_limit: Decimal
    current_balance: Decimal
    credit_available: Decimal
    ytd_purchases: Decimal
    ytd_payments: Decimal
    ytd_1099_amount: Optional[Decimal] = None
    last_purchase_date: Optional[date] = None
    last_payment_date: Optional[date] = None
    average_days_to_pay: Optional[float] = None
    created_at: datetime
    updated_at: Optional[datetime] = None
    
    model_config = ConfigDict(from_attributes=True)


class SupplierSummary(BaseModel):
    supplier: Supplier
    open_pos: int
    open_invoices: int
    total_outstanding: Decimal
    current_amount: Decimal
    overdue_30_days: Decimal
    overdue_60_days: Decimal
    overdue_90_days: Decimal
    pending_deliveries: int
    recent_transactions: List[Dict[str, Any]]


# Purchase Order Schemas
class PurchaseOrderLineBase(BaseModel):
    line_number: int = Field(..., ge=1)
    item_code: str
    description: str
    quantity: Decimal = Field(..., gt=0)
    unit_of_measure: str
    unit_price: Decimal = Field(..., ge=0)
    discount_percent: Decimal = Field(default=0, ge=0, le=100)
    tax_code: Optional[str] = None
    tax_rate: Decimal = Field(default=0)
    gl_account: str
    department: Optional[str] = None
    project: Optional[str] = None
    requested_date: Optional[date] = None


class PurchaseOrderLine(PurchaseOrderLineBase):
    id: int
    line_amount: Decimal
    discount_amount: Decimal
    tax_amount: Decimal
    net_amount: Decimal
    quantity_received: Decimal
    quantity_invoiced: Decimal
    quantity_outstanding: Decimal
    
    model_config = ConfigDict(from_attributes=True)


class PurchaseOrderBase(BaseModel):
    supplier_code: str
    order_date: date
    required_date: Optional[date] = None
    ship_to_address1: Optional[str] = None
    ship_to_address2: Optional[str] = None
    ship_to_city: Optional[str] = None
    ship_to_state: Optional[str] = None
    ship_to_zip: Optional[str] = None
    buyer_code: Optional[str] = None
    terms_override: Optional[PaymentTerms] = None
    notes: Optional[str] = None
    lines: List[PurchaseOrderLineBase]


class PurchaseOrderCreate(PurchaseOrderBase):
    manual_po_number: Optional[str] = None
    requires_approval: bool = True


class PurchaseOrderUpdate(BaseModel):
    required_date: Optional[date] = None
    notes: Optional[str] = None
    lines: Optional[List[PurchaseOrderLineBase]] = None


class PurchaseOrder(PurchaseOrderBase):
    id: int
    po_number: str
    status: PurchaseOrderStatus
    lines: List[PurchaseOrderLine]
    subtotal: Decimal
    discount_amount: Decimal
    tax_amount: Decimal
    freight_amount: Decimal
    total_amount: Decimal
    received_amount: Decimal
    invoiced_amount: Decimal
    approval_status: Optional[ApprovalStatus] = None
    approved_by: Optional[int] = None
    approved_at: Optional[datetime] = None
    created_by: int
    created_at: datetime
    sent_at: Optional[datetime] = None
    
    model_config = ConfigDict(from_attributes=True)


# Purchase Invoice Schemas
class PurchaseInvoiceLineBase(BaseModel):
    line_number: int = Field(..., ge=1)
    po_line_id: Optional[int] = None
    item_code: str
    description: str
    quantity: Decimal = Field(..., gt=0)
    unit_price: Decimal = Field(..., ge=0)
    discount_percent: Decimal = Field(default=0, ge=0, le=100)
    tax_code: Optional[str] = None
    tax_rate: Decimal = Field(default=0)
    gl_account: str
    department: Optional[str] = None
    project: Optional[str] = None


class PurchaseInvoiceLine(PurchaseInvoiceLineBase):
    id: int
    line_amount: Decimal
    discount_amount: Decimal
    tax_amount: Decimal
    net_amount: Decimal
    
    model_config = ConfigDict(from_attributes=True)


class PurchaseInvoiceBase(BaseModel):
    supplier_code: str
    invoice_number: str
    invoice_date: date
    due_date: Optional[date] = None
    po_number: Optional[str] = None
    reference: Optional[str] = None
    terms_override: Optional[PaymentTerms] = None
    lines: List[PurchaseInvoiceLineBase]


class PurchaseInvoiceCreate(PurchaseInvoiceBase):
    match_to_po: bool = True
    auto_calculate_tax: bool = True
    notes: Optional[str] = None


class PurchaseInvoiceUpdate(BaseModel):
    due_date: Optional[date] = None
    reference: Optional[str] = None
    notes: Optional[str] = None
    lines: Optional[List[PurchaseInvoiceLineBase]] = None


class PurchaseInvoice(PurchaseInvoiceBase):
    id: int
    internal_invoice_number: str
    status: PurchaseInvoiceStatus
    lines: List[PurchaseInvoiceLine]
    subtotal: Decimal
    discount_amount: Decimal
    tax_amount: Decimal
    freight_amount: Decimal
    total_amount: Decimal
    paid_amount: Decimal
    balance_due: Decimal
    approval_status: Optional[ApprovalStatus] = None
    approved_by: Optional[int] = None
    approved_at: Optional[datetime] = None
    matched_to_po: bool
    three_way_match_status: Optional[str] = None
    created_by: int
    created_at: datetime
    posted_at: Optional[datetime] = None
    
    model_config = ConfigDict(from_attributes=True)


# Payment Schemas
class PaymentAllocationBase(BaseModel):
    invoice_id: int
    invoice_number: str
    amount_applied: Decimal = Field(..., gt=0)
    discount_taken: Decimal = Field(default=0, ge=0)


class PaymentAllocation(PaymentAllocationBase):
    id: int
    remaining_balance: Decimal
    
    model_config = ConfigDict(from_attributes=True)


class PurchasePaymentBase(BaseModel):
    supplier_code: str
    payment_date: date
    payment_method: str
    check_number: Optional[str] = None
    bank_account: str
    amount: Decimal = Field(..., gt=0)
    allocations: List[PaymentAllocationBase]


class PurchasePaymentCreate(PurchasePaymentBase):
    reference: Optional[str] = None
    notes: Optional[str] = None
    print_check: bool = False


class PurchasePayment(PurchasePaymentBase):
    id: int
    payment_number: str
    allocations: List[PaymentAllocation]
    allocated_amount: Decimal
    unallocated_amount: Decimal
    check_printed: bool
    check_printed_at: Optional[datetime] = None
    gl_posted: bool
    created_by: int
    created_at: datetime
    posted_at: Optional[datetime] = None
    void_status: bool = False
    void_date: Optional[date] = None
    void_reason: Optional[str] = None
    
    model_config = ConfigDict(from_attributes=True)


# Receipt/GRN Schemas
class GoodsReceiptLineBase(BaseModel):
    po_line_id: int
    quantity_received: Decimal = Field(..., gt=0)
    quantity_rejected: Decimal = Field(default=0, ge=0)
    bin_location: Optional[str] = None
    lot_number: Optional[str] = None
    expiry_date: Optional[date] = None
    notes: Optional[str] = None


class GoodsReceiptLine(GoodsReceiptLineBase):
    id: int
    item_code: str
    description: str
    unit_price: Decimal
    total_value: Decimal
    
    model_config = ConfigDict(from_attributes=True)


class GoodsReceiptBase(BaseModel):
    po_number: str
    receipt_date: date
    supplier_delivery_note: Optional[str] = None
    lines: List[GoodsReceiptLineBase]


class GoodsReceiptCreate(GoodsReceiptBase):
    update_inventory: bool = True
    send_notification: bool = True


class GoodsReceipt(GoodsReceiptBase):
    id: int
    grn_number: str
    supplier_code: str
    supplier_name: str
    lines: List[GoodsReceiptLine]
    total_quantity: Decimal
    total_value: Decimal
    matched_to_invoice: bool
    received_by: int
    created_at: datetime
    
    model_config = ConfigDict(from_attributes=True)


# Debit Note Schemas
class DebitNoteLineBase(BaseModel):
    line_number: int
    invoice_line_id: Optional[int] = None
    item_code: str
    description: str
    quantity: Decimal
    unit_price: Decimal
    tax_code: Optional[str] = None
    tax_rate: Decimal = Field(default=0)
    gl_account: str


class DebitNoteLine(DebitNoteLineBase):
    id: int
    line_amount: Decimal
    tax_amount: Decimal
    total_amount: Decimal
    
    model_config = ConfigDict(from_attributes=True)


class DebitNoteBase(BaseModel):
    supplier_code: str
    debit_date: date
    original_invoice_number: Optional[str] = None
    reason: str
    lines: List[DebitNoteLineBase]


class DebitNoteCreate(DebitNoteBase):
    auto_allocate: bool = True


class DebitNote(DebitNoteBase):
    id: int
    debit_note_number: str
    status: str
    lines: List[DebitNoteLine]
    subtotal: Decimal
    tax_amount: Decimal
    total_amount: Decimal
    allocated_amount: Decimal
    unallocated_amount: Decimal
    created_by: int
    created_at: datetime
    posted_at: Optional[datetime] = None
    
    model_config = ConfigDict(from_attributes=True)


# Aged Payables Schemas
class AgedPayableDetail(BaseModel):
    invoice_number: str
    invoice_date: date
    due_date: date
    original_amount: Decimal
    paid_amount: Decimal
    balance: Decimal
    days_overdue: int
    aging_bucket: str


class SupplierAgedBalance(BaseModel):
    supplier_code: str
    supplier_name: str
    current_balance: Decimal
    current: Decimal
    overdue_30: Decimal
    overdue_60: Decimal
    overdue_90: Decimal
    overdue_120_plus: Decimal
    details: List[AgedPayableDetail]


class AgedPayablesSummary(BaseModel):
    as_of_date: date
    total_outstanding: Decimal
    current: Decimal
    overdue_30: Decimal
    overdue_60: Decimal
    overdue_90: Decimal
    overdue_120_plus: Decimal
    supplier_count: int
    suppliers: List[SupplierAgedBalance]


# Approval Schemas
class ApprovalRequest(BaseModel):
    document_type: str  # PO, Invoice
    document_id: int
    amount: Decimal
    supplier_code: str
    supplier_name: str
    description: str
    requested_by: int
    requested_at: datetime
    urgency: str = "normal"  # low, normal, high, urgent


class ApprovalResponse(BaseModel):
    request_id: int
    action: str  # approve, reject, escalate
    comments: Optional[str] = None
    approved_amount: Optional[Decimal] = None


# Response Models
class SupplierListResponse(BaseModel):
    suppliers: List[Supplier]
    total: int
    skip: int
    limit: int


class PurchaseOrderListResponse(BaseModel):
    orders: List[PurchaseOrder]
    total: int
    skip: int
    limit: int


class PurchaseInvoiceListResponse(BaseModel):
    invoices: List[PurchaseInvoice]
    total: int
    skip: int
    limit: int


class PaymentListResponse(BaseModel):
    payments: List[PurchasePayment]
    total: int
    skip: int
    limit: int


# Batch Processing Schemas
class BatchInvoiceApproval(BaseModel):
    invoice_ids: List[int]
    action: str = Field(..., pattern="^(approve|reject)$")
    comments: Optional[str] = None


class BatchPaymentRun(BaseModel):
    supplier_codes: Optional[List[str]] = None
    payment_date: date
    payment_method: str
    bank_account: str
    include_overdue_only: bool = False
    min_payment_amount: Decimal = Field(default=0, ge=0)


class RemittanceAdvice(BaseModel):
    supplier_code: str
    payment_number: str
    payment_date: date
    payment_amount: Decimal
    invoices_paid: List[Dict[str, Any]]
    send_email: bool = True
    email_override: Optional[str] = None