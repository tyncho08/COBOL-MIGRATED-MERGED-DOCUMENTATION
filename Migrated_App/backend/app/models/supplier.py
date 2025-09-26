"""
ACAS Supplier (Purchase Ledger) Models
SQLAlchemy models for supplier management and purchase processing
"""
from sqlalchemy import (
    Column, String, Integer, Numeric, DateTime, Text,
    ForeignKey, CheckConstraint, Index
)
from sqlalchemy.orm import relationship
from sqlalchemy.sql import func
from app.core.database import Base

class PurchaseLedgerRec(Base):
    """
    Purchase Ledger Record - Supplier Master
    
    Represents supplier master data with complete financial history.
    Matches the actual PostgreSQL schema structure.
    """
    __tablename__ = "puledger_rec"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key - Supplier Code
    purch_key = Column(String(10), primary_key=True, doc="Supplier code")
    
    # Supplier Identity Information
    purch_name = Column(String(40), nullable=False, default='', doc="Supplier name")
    purch_address_1 = Column(String(30), default='', doc="Address line 1")
    purch_address_2 = Column(String(30), default='', doc="Address line 2")
    purch_address_3 = Column(String(30), default='', doc="Address line 3")
    purch_address_4 = Column(String(30), default='', doc="Address line 4")
    purch_address_5 = Column(String(12), default='', doc="Address line 5 (postcode)")
    purch_country = Column(String(24), default='', doc="Country")
    
    # Contact Information
    purch_contact = Column(String(30), default='', doc="Primary contact person")
    purch_phone = Column(String(20), default='', doc="Phone number")
    purch_fax = Column(String(20), default='', doc="Fax number")
    purch_email = Column(String(50), default='', doc="Email address")
    purch_mobile = Column(String(20), default='', doc="Mobile number")
    
    # Financial Information
    purch_balance = Column(Numeric(12, 2), default=0.00, doc="Current account balance")
    purch_ytd_turnover = Column(Numeric(12, 2), default=0.00, doc="Year-to-date turnover")
    purch_last_year_turnover = Column(Numeric(12, 2), default=0.00, doc="Last year turnover")
    
    # Terms and Configuration
    purch_payment_terms = Column(String(4), default='30', doc="Payment terms")
    purch_settlement_disc = Column(Numeric(4, 2), default=0.00, doc="Settlement discount")
    purch_tax_code = Column(String(4), default='VSTD', doc="Tax/VAT code")
    purch_currency = Column(String(3), default='GBP', doc="Currency code")
    
    # Banking Information
    purch_our_account_no = Column(String(20), default='', doc="Our account number with supplier")
    purch_bank_name = Column(String(30), default='', doc="Supplier bank name")
    purch_bank_address = Column(String(60), default='', doc="Supplier bank address")
    purch_bank_account = Column(String(20), default='', doc="Supplier bank account")
    purch_bank_sort_code = Column(String(10), default='', doc="Supplier bank sort code")
    purch_bank_swift = Column(String(15), default='', doc="Supplier bank SWIFT code")
    
    # Status and Control Flags
    purch_account_status = Column(String(1), default='A', doc="Account status: A=Active, H=Hold, C=Closed")
    purch_approval_required = Column(String(1), default='N', doc="Approval required flag")
    purch_remittance_flag = Column(String(1), default='Y', doc="Send remittance flag")
    purch_1099_required = Column(String(1), default='N', doc="1099 required flag")
    
    # Analysis Fields
    purch_analysis_1 = Column(String(10), default='', doc="Analysis field 1")
    purch_analysis_2 = Column(String(10), default='', doc="Analysis field 2")
    purch_analysis_3 = Column(String(10), default='', doc="Analysis field 3")
    purch_default_nominal = Column(Integer, default=0, doc="Default nominal account")
    
    # Date Fields (stored as integers in YYYYMMDD format)
    purch_date_opened = Column(Integer, default=0, doc="Date opened (YYYYMMDD)")
    purch_date_last_purchase = Column(Integer, default=0, doc="Last purchase date (YYYYMMDD)")
    purch_date_last_payment = Column(Integer, default=0, doc="Last payment date (YYYYMMDD)")
    
    # Statistics
    purch_transactions_mtd = Column(Integer, default=0, doc="Transactions month-to-date")
    purch_transactions_ytd = Column(Integer, default=0, doc="Transactions year-to-date")
    purch_invoices_mtd = Column(Integer, default=0, doc="Invoices month-to-date")
    purch_invoices_ytd = Column(Integer, default=0, doc="Invoices year-to-date")
    purch_average_days = Column(Integer, default=0, doc="Average payment days")
    purch_oldest_item = Column(Integer, default=0, doc="Oldest item date (YYYYMMDD)")
    
    # Notes
    purch_notes = Column(Text, doc="Supplier notes")
    
    # Audit Trail
    created_at = Column(DateTime(timezone=True), server_default=func.current_timestamp(), doc="Record creation timestamp")
    updated_at = Column(DateTime(timezone=True), server_default=func.current_timestamp(), onupdate=func.current_timestamp(), doc="Last update timestamp")
    updated_by = Column(String(30), server_default=func.current_user(), doc="Updated by user")
    
    # Relationships
    invoices = relationship("PurchaseInvoiceRec", back_populates="supplier")
    items = relationship("PurchaseItemRec", back_populates="supplier")
    
    # Table constraints
    __table_args__ = (
        CheckConstraint("purch_account_status IN ('A', 'H', 'C')", name='ck_puledger_valid_status'),
        CheckConstraint("purch_approval_required IN ('Y', 'N')", name='ck_puledger_valid_approval'),
        CheckConstraint("purch_remittance_flag IN ('Y', 'N')", name='ck_puledger_valid_remittance'),
        CheckConstraint("purch_1099_required IN ('Y', 'N')", name='ck_puledger_valid_1099'),
        Index('idx_puledger_name', 'purch_name'),
        Index('idx_puledger_status', 'purch_account_status'),
        Index('idx_puledger_balance', 'purch_balance'),
        {'schema': 'acas'}
    )

class PurchaseInvoiceRec(Base):
    """Purchase Invoice Record"""
    __tablename__ = "puinvoice_rec"
    __table_args__ = {'schema': 'acas'}
    
    invoice_key = Column(String(20), primary_key=True, doc="Invoice number")
    purch_key = Column(String(10), ForeignKey("acas.puledger_rec.purch_key", ondelete="RESTRICT"), nullable=False, doc="Supplier code")
    invoice_date = Column(Integer, nullable=False, doc="Invoice date (YYYYMMDD)")
    invoice_amount = Column(Numeric(12, 2), default=0.00, doc="Invoice amount")
    invoice_status = Column(String(1), default='O', doc="Invoice status")
    
    # Audit Trail
    created_at = Column(DateTime(timezone=True), server_default=func.current_timestamp())
    updated_at = Column(DateTime(timezone=True), server_default=func.current_timestamp(), onupdate=func.current_timestamp())
    
    # Relationships
    supplier = relationship("PurchaseLedgerRec", back_populates="invoices")
    lines = relationship("PurchaseInvoiceLineRec", back_populates="invoice")

class PurchaseInvoiceLineRec(Base):
    """Purchase Invoice Line Record"""
    __tablename__ = "puinvoice_lines_rec"
    __table_args__ = {'schema': 'acas'}
    
    line_id = Column(Integer, primary_key=True, autoincrement=True, doc="Line ID")
    invoice_key = Column(String(20), ForeignKey("acas.puinvoice_rec.invoice_key", ondelete="CASCADE"), nullable=False, doc="Invoice number")
    line_number = Column(Integer, nullable=False, doc="Line number")
    stock_key = Column(String(30), doc="Stock item code")
    description = Column(String(40), doc="Line description")
    quantity = Column(Numeric(15, 3), default=0.000, doc="Quantity")
    unit_price = Column(Numeric(15, 4), default=0.0000, doc="Unit price")
    line_total = Column(Numeric(12, 2), default=0.00, doc="Line total")
    
    # Relationships
    invoice = relationship("PurchaseInvoiceRec", back_populates="lines")

class PurchaseItemRec(Base):
    """Purchase Item Record - Historical purchase data"""
    __tablename__ = "puitm5_rec"
    __table_args__ = {'schema': 'acas'}
    
    item_id = Column(Integer, primary_key=True, autoincrement=True, doc="Item ID")
    purch_key = Column(String(10), ForeignKey("acas.puledger_rec.purch_key", ondelete="RESTRICT"), nullable=False, doc="Supplier code")
    stock_key = Column(String(30), doc="Stock item code")
    transaction_date = Column(Integer, doc="Transaction date (YYYYMMDD)")
    quantity = Column(Numeric(15, 3), default=0.000, doc="Quantity")
    value = Column(Numeric(12, 2), default=0.00, doc="Transaction value")
    
    # Relationships
    supplier = relationship("PurchaseLedgerRec", back_populates="items")


class SupplierContactRec(Base):
    """Supplier Contact Record - Multiple contacts per supplier"""
    __tablename__ = "supplier_contacts"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    contact_id = Column(Integer, primary_key=True, autoincrement=True, doc="Contact ID")
    
    # Supplier Reference
    purch_key = Column(String(10), ForeignKey("acas.puledger_rec.purch_key", ondelete="CASCADE"), nullable=False, doc="Supplier code")
    
    # Contact Information
    contact_type = Column(String(20), nullable=False, doc="Contact type (Primary, Accounts, Ordering, etc.)")
    contact_name = Column(String(50), nullable=False, doc="Contact person name")
    title = Column(String(20), doc="Contact title/position")
    department = Column(String(30), doc="Department")
    
    # Communication Details
    phone = Column(String(20), doc="Phone number")
    mobile = Column(String(20), doc="Mobile number")
    fax = Column(String(20), doc="Fax number")
    email = Column(String(100), doc="Email address")
    
    # Address (if different from main supplier address)
    address_1 = Column(String(30), doc="Address line 1")
    address_2 = Column(String(30), doc="Address line 2")
    address_3 = Column(String(30), doc="Address line 3")
    city = Column(String(30), doc="City")
    postcode = Column(String(12), doc="Postcode")
    country = Column(String(24), doc="Country")
    
    # Status and Preferences
    is_active = Column(String(1), default='Y', doc="Active contact flag")
    is_primary = Column(String(1), default='N', doc="Primary contact flag")
    preferred_contact_method = Column(String(10), default='EMAIL', doc="Preferred contact method")
    
    # Communication Preferences
    send_orders = Column(String(1), default='Y', doc="Send purchase orders flag")
    send_remittances = Column(String(1), default='Y', doc="Send remittance advices flag")
    send_statements = Column(String(1), default='N', doc="Send statements flag")
    
    # Notes
    notes = Column(Text, doc="Contact notes")
    
    # Audit Trail
    created_at = Column(DateTime(timezone=True), server_default=func.current_timestamp())
    updated_at = Column(DateTime(timezone=True), server_default=func.current_timestamp(), onupdate=func.current_timestamp())
    
    # Relationships
    supplier = relationship("PurchaseLedgerRec", foreign_keys=[purch_key])


class SupplierBankRec(Base):
    """Supplier Bank Record - Banking details for payments"""
    __tablename__ = "supplier_banks"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    bank_id = Column(Integer, primary_key=True, autoincrement=True, doc="Bank record ID")
    
    # Supplier Reference
    purch_key = Column(String(10), ForeignKey("acas.puledger_rec.purch_key", ondelete="CASCADE"), nullable=False, doc="Supplier code")
    
    # Bank Information
    bank_name = Column(String(50), nullable=False, doc="Bank name")
    bank_branch = Column(String(50), doc="Branch name")
    
    # Bank Address
    bank_address_1 = Column(String(30), doc="Bank address line 1")
    bank_address_2 = Column(String(30), doc="Bank address line 2")
    bank_address_3 = Column(String(30), doc="Bank address line 3")
    bank_city = Column(String(30), doc="Bank city")
    bank_postcode = Column(String(12), doc="Bank postcode")
    bank_country = Column(String(24), doc="Bank country")
    
    # Account Details
    account_number = Column(String(30), nullable=False, doc="Bank account number")
    account_name = Column(String(50), doc="Account holder name")
    sort_code = Column(String(10), doc="Sort code")
    iban = Column(String(34), doc="IBAN")
    swift_code = Column(String(15), doc="SWIFT/BIC code")
    
    # Currency and Type
    currency = Column(String(3), default='USD', doc="Account currency")
    account_type = Column(String(20), default='CHECKING', doc="Account type")
    
    # Payment Information
    is_default = Column(String(1), default='N', doc="Default payment account flag")
    payment_method = Column(String(20), default='BANK_TRANSFER', doc="Preferred payment method")
    payment_terms = Column(String(20), doc="Special payment terms")
    minimum_payment = Column(Numeric(12, 2), default=0.00, doc="Minimum payment amount")
    
    # Status and Control
    is_active = Column(String(1), default='Y', doc="Active account flag")
    requires_authorization = Column(String(1), default='N', doc="Requires authorization flag")
    
    # Verification
    bank_verified = Column(String(1), default='N', doc="Bank details verified flag")
    verified_date = Column(Integer, doc="Verification date (YYYYMMDD)")
    verified_by = Column(String(30), doc="Verified by user")
    
    # Additional Information
    reference_code = Column(String(20), doc="Our reference for payments")
    special_instructions = Column(Text, doc="Special payment instructions")
    
    # Audit Trail
    created_by = Column(String(30), nullable=False, doc="Created by user")
    created_at = Column(DateTime(timezone=True), server_default=func.current_timestamp())
    updated_by = Column(String(30), doc="Last updated by user")
    updated_at = Column(DateTime(timezone=True), server_default=func.current_timestamp(), onupdate=func.current_timestamp())
    
    # Relationships
    supplier = relationship("PurchaseLedgerRec", foreign_keys=[purch_key])


class PurchaseOpenItemRec(Base):
    """Purchase Open Item Record - Outstanding purchase transactions"""
    __tablename__ = "puoi_rec"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    oi_id = Column(Integer, primary_key=True, autoincrement=True, doc="Open item ID")
    
    # References
    purch_oi_key = Column(String(20), nullable=False, doc="Open item key")
    purch_oi_supplier = Column(String(10), ForeignKey("acas.puledger_rec.purch_key", ondelete="RESTRICT"), nullable=False, doc="Supplier code")
    
    # Transaction Details
    purch_oi_type = Column(String(4), nullable=False, doc="Transaction type (INV, CRN, PAY)")
    purch_oi_our_ref = Column(String(20), nullable=False, doc="Our reference")
    purch_oi_their_ref = Column(String(20), default='', doc="Their reference")
    purch_oi_date = Column(Integer, nullable=False, doc="Transaction date (YYYYMMDD)")
    purch_oi_due_date = Column(Integer, nullable=False, doc="Due date (YYYYMMDD)")
    
    # Financial Values
    purch_oi_goods = Column(Numeric(12, 2), default=0.00, doc="Goods value")
    purch_oi_tax = Column(Numeric(12, 2), default=0.00, doc="Tax value")
    purch_oi_gross = Column(Numeric(12, 2), default=0.00, doc="Gross value")
    purch_oi_amount = Column(Numeric(12, 2), default=0.00, doc="Outstanding amount")
    
    # Currency and Exchange
    purch_oi_currency = Column(String(3), default='USD', doc="Transaction currency")
    purch_oi_exchange_rate = Column(Numeric(10, 6), default=1.000000, doc="Exchange rate")
    
    # Control Information
    purch_oi_period = Column(Integer, nullable=False, doc="Accounting period")
    purch_oi_posted = Column(String(1), default='Y', doc="Posted flag")
    purch_oi_matched = Column(String(1), default='N', doc="Matched flag")
    purch_oi_on_hold = Column(String(1), default='N', doc="On hold flag")
    
    # Payment Information
    purch_oi_paid_date = Column(Integer, default=0, doc="Paid date (YYYYMMDD)")
    purch_oi_discount_taken = Column(Numeric(12, 2), default=0.00, doc="Settlement discount taken")
    purch_oi_payment_method = Column(String(10), default='', doc="Payment method")
    
    # Analysis
    purch_oi_analysis_1 = Column(String(10), default='', doc="Analysis field 1")
    purch_oi_analysis_2 = Column(String(10), default='', doc="Analysis field 2")
    
    # Aging (calculated fields)
    purch_oi_days_outstanding = Column(Integer, default=0, doc="Days outstanding")
    purch_oi_age_band = Column(String(10), default='', doc="Age band")
    
    # Notes
    purch_oi_notes = Column(Text, doc="Open item notes")
    
    # Audit Trail
    created_at = Column(DateTime(timezone=True), server_default=func.current_timestamp())
    updated_at = Column(DateTime(timezone=True), server_default=func.current_timestamp(), onupdate=func.current_timestamp())
    
    # Relationships
    supplier = relationship("PurchaseLedgerRec", foreign_keys=[purch_oi_supplier])


class PurchaseOrderRec(Base):
    """Purchase Order Record - Purchase orders header"""
    __tablename__ = "purchase_orders"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    order_id = Column(Integer, primary_key=True, autoincrement=True, doc="Order ID")
    order_number = Column(String(20), unique=True, nullable=False, doc="Purchase order number")
    
    # Supplier Information
    supplier_code = Column(String(10), ForeignKey("acas.puledger_rec.purch_key", ondelete="RESTRICT"), nullable=False, doc="Supplier code")
    
    # Order Details
    order_date = Column(Integer, nullable=False, doc="Order date (YYYYMMDD)")
    required_date = Column(Integer, doc="Required date (YYYYMMDD)")
    delivery_date = Column(Integer, doc="Delivery date (YYYYMMDD)")
    
    # Financial Information
    currency = Column(String(3), default='USD', doc="Order currency")
    exchange_rate = Column(Numeric(10, 6), default=1.000000, doc="Exchange rate")
    subtotal = Column(Numeric(15, 2), default=0.00, doc="Order subtotal")
    tax_amount = Column(Numeric(15, 2), default=0.00, doc="Tax amount")
    total_amount = Column(Numeric(15, 2), default=0.00, doc="Total order amount")
    
    # Delivery Information
    delivery_address_1 = Column(String(30), doc="Delivery address line 1")
    delivery_address_2 = Column(String(30), doc="Delivery address line 2")
    delivery_address_3 = Column(String(30), doc="Delivery address line 3")
    delivery_city = Column(String(30), doc="Delivery city")
    delivery_postcode = Column(String(12), doc="Delivery postcode")
    delivery_country = Column(String(24), doc="Delivery country")
    
    # Status and Control
    order_status = Column(String(20), default='DRAFT', doc="Order status")
    approved_by = Column(String(30), doc="Approved by user")
    approved_date = Column(Integer, doc="Approval date (YYYYMMDD)")
    
    # Terms and Conditions
    payment_terms = Column(String(20), doc="Payment terms")
    delivery_terms = Column(String(20), doc="Delivery terms")
    freight_terms = Column(String(20), doc="Freight terms")
    
    # References
    requisition_number = Column(String(20), doc="Requisition number")
    buyer_code = Column(String(10), doc="Buyer code")
    department_code = Column(String(10), doc="Department code")
    project_code = Column(String(15), doc="Project code")
    
    # Notes
    notes = Column(Text, doc="Order notes")
    special_instructions = Column(Text, doc="Special instructions")
    
    # Audit Trail
    created_by = Column(String(30), nullable=False, doc="Created by user")
    created_at = Column(DateTime(timezone=True), server_default=func.current_timestamp())
    updated_at = Column(DateTime(timezone=True), server_default=func.current_timestamp(), onupdate=func.current_timestamp())
    
    # Relationships
    supplier = relationship("PurchaseLedgerRec", foreign_keys=[supplier_code])
    lines = relationship("PurchaseOrderLineRec", back_populates="order", cascade="all, delete-orphan")


class PurchaseOrderLineRec(Base):
    """Purchase Order Line Record - Purchase order line items"""
    __tablename__ = "purchase_order_lines"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    line_id = Column(Integer, primary_key=True, autoincrement=True, doc="Line ID")
    
    # Order Reference
    order_id = Column(Integer, ForeignKey("acas.purchase_orders.order_id", ondelete="CASCADE"), nullable=False, doc="Purchase order ID")
    line_number = Column(Integer, nullable=False, doc="Line number")
    
    # Item Information
    stock_code = Column(String(30), doc="Stock item code")
    supplier_part_number = Column(String(30), doc="Supplier part number")
    description = Column(String(100), nullable=False, doc="Item description")
    
    # Quantity Information
    quantity_ordered = Column(Numeric(15, 3), nullable=False, doc="Quantity ordered")
    quantity_received = Column(Numeric(15, 3), default=0.000, doc="Quantity received")
    quantity_invoiced = Column(Numeric(15, 3), default=0.000, doc="Quantity invoiced")
    unit_of_measure = Column(String(6), default='EA', doc="Unit of measure")
    
    # Pricing Information
    unit_price = Column(Numeric(15, 4), nullable=False, doc="Unit price")
    discount_percent = Column(Numeric(5, 2), default=0.00, doc="Discount percentage")
    discount_amount = Column(Numeric(15, 2), default=0.00, doc="Discount amount")
    line_total = Column(Numeric(15, 2), nullable=False, doc="Line total")
    
    # Delivery Information
    required_date = Column(Integer, doc="Required date (YYYYMMDD)")
    promised_date = Column(Integer, doc="Promised date (YYYYMMDD)")
    delivery_location = Column(String(10), doc="Delivery location")
    
    # Status and Control
    line_status = Column(String(20), default='OPEN', doc="Line status")
    closed_date = Column(Integer, doc="Line closed date (YYYYMMDD)")
    
    # GL and Cost Information
    gl_account = Column(String(10), doc="GL account code")
    cost_center = Column(String(10), doc="Cost center")
    project_code = Column(String(15), doc="Project code")
    
    # Receipt Information
    last_receipt_date = Column(Integer, doc="Last receipt date (YYYYMMDD)")
    last_receipt_quantity = Column(Numeric(15, 3), default=0.000, doc="Last receipt quantity")
    
    # Notes
    line_notes = Column(Text, doc="Line notes")
    
    # Relationships
    order = relationship("PurchaseOrderRec", back_populates="lines")