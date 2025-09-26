"""
ACAS Customer (Sales Ledger) Models
SQLAlchemy models for customer management and sales processing
"""
from sqlalchemy import (
    Column, String, Integer, Numeric, DateTime, Text,
    ForeignKey, CheckConstraint, Index
)
from sqlalchemy.orm import relationship
from sqlalchemy.sql import func
from app.core.database import Base

class SalesLedgerRec(Base):
    """
    Sales Ledger Record - Customer Master
    
    Represents customer master data with complete financial history.
    Matches the actual PostgreSQL schema structure.
    """
    __tablename__ = "saledger_rec"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key - Customer Code
    sales_key = Column(String(10), primary_key=True, doc="Customer code")
    
    # Customer Identity Information
    sales_name = Column(String(40), nullable=False, default='', doc="Customer name")
    sales_address_1 = Column(String(30), default='', doc="Address line 1")
    sales_address_2 = Column(String(30), default='', doc="Address line 2")
    sales_address_3 = Column(String(30), default='', doc="Address line 3")
    sales_address_4 = Column(String(30), default='', doc="Address line 4")
    sales_address_5 = Column(String(12), default='', doc="Address line 5 (postcode)")
    sales_country = Column(String(24), default='', doc="Country")
    
    # Contact Information
    sales_contact = Column(String(30), default='', doc="Primary contact person")
    sales_phone = Column(String(20), default='', doc="Phone number")
    sales_fax = Column(String(20), default='', doc="Fax number")
    sales_email = Column(String(50), default='', doc="Email address")
    sales_mobile = Column(String(20), default='', doc="Mobile number")
    
    # Financial Information
    sales_credit_limit = Column(Numeric(12, 2), default=0.00, doc="Customer credit limit")
    sales_balance = Column(Numeric(12, 2), default=0.00, doc="Current account balance")
    sales_ytd_turnover = Column(Numeric(12, 2), default=0.00, doc="Year-to-date turnover")
    sales_last_year_turnover = Column(Numeric(12, 2), default=0.00, doc="Last year turnover")
    
    # Terms and Configuration
    sales_payment_terms = Column(String(4), default='30', doc="Payment terms")
    sales_discount_rate = Column(Numeric(4, 2), default=0.00, doc="Discount rate")
    sales_settlement_disc = Column(Numeric(4, 2), default=0.00, doc="Settlement discount")
    sales_price_level = Column(Integer, default=1, doc="Price level")
    sales_tax_code = Column(String(4), default='VSTD', doc="Tax/VAT code")
    sales_currency = Column(String(3), default='GBP', doc="Currency code")
    
    # Status and Control Flags
    sales_account_status = Column(String(1), default='A', doc="Account status: A=Active, H=Hold, C=Closed")
    sales_credit_status = Column(String(1), default='O', doc="Credit status")
    sales_statement_flag = Column(String(1), default='Y', doc="Send statements flag")
    sales_dunning_flag = Column(String(1), default='Y', doc="Send dunning letters flag")
    sales_charge_interest = Column(String(1), default='N', doc="Charge interest flag")
    
    # Analysis Fields
    sales_analysis_1 = Column(String(10), default='', doc="Analysis field 1")
    sales_analysis_2 = Column(String(10), default='', doc="Analysis field 2")
    sales_analysis_3 = Column(String(10), default='', doc="Analysis field 3")
    sales_territory = Column(String(6), default='', doc="Sales territory")
    sales_rep = Column(String(6), default='', doc="Sales representative")
    
    # Date Fields (stored as integers in YYYYMMDD format)
    sales_date_opened = Column(Integer, default=0, doc="Date opened (YYYYMMDD)")
    sales_date_last_sale = Column(Integer, default=0, doc="Last sale date (YYYYMMDD)")
    sales_date_last_payment = Column(Integer, default=0, doc="Last payment date (YYYYMMDD)")
    
    # Statistics
    sales_transactions_mtd = Column(Integer, default=0, doc="Transactions month-to-date")
    sales_transactions_ytd = Column(Integer, default=0, doc="Transactions year-to-date")
    sales_invoices_mtd = Column(Integer, default=0, doc="Invoices month-to-date")
    sales_invoices_ytd = Column(Integer, default=0, doc="Invoices year-to-date")
    sales_average_days = Column(Integer, default=0, doc="Average payment days")
    sales_oldest_item = Column(Integer, default=0, doc="Oldest item date (YYYYMMDD)")
    
    # Notes
    sales_notes = Column(Text, doc="Customer notes")
    
    # Audit Trail
    created_at = Column(DateTime(timezone=True), server_default=func.current_timestamp(), doc="Record creation timestamp")
    updated_at = Column(DateTime(timezone=True), server_default=func.current_timestamp(), onupdate=func.current_timestamp(), doc="Last update timestamp")
    updated_by = Column(String(30), server_default=func.current_user(), doc="Updated by user")
    
    @property
    def is_active(self) -> bool:
        """Return True if account is active"""
        return self.sales_account_status == 'A'
    
    @property
    def available_credit(self) -> float:
        """Calculate available credit"""
        return float(self.sales_credit_limit - self.sales_balance)
    
    @property
    def is_over_limit(self) -> bool:
        """Check if account is over credit limit"""
        return self.sales_balance > self.sales_credit_limit
    
    # Relationships
    invoices = relationship("SalesInvoiceRec", back_populates="customer")
    items = relationship("SalesItemRec", back_populates="customer")
    # Sales module relationships (imported to avoid circular imports)
    open_items = relationship("SalesOpenItemRec", back_populates="customer", foreign_keys="SalesOpenItemRec.sales_key")
    orders = relationship("SalesOrderRec", back_populates="customer", foreign_keys="SalesOrderRec.sales_key")
    receipts = relationship("SalesReceiptRec", back_populates="customer", foreign_keys="SalesReceiptRec.sales_key")
    history = relationship("SalesHistoryRec", back_populates="customer", foreign_keys="SalesHistoryRec.sales_key")
    
    # Table constraints
    __table_args__ = (
        CheckConstraint("sales_account_status IN ('A', 'H', 'C')", name='ck_saledger_valid_status'),
        CheckConstraint("sales_credit_status IN ('O', 'H', 'S')", name='ck_saledger_valid_credit'),
        CheckConstraint("sales_statement_flag IN ('Y', 'N')", name='ck_saledger_valid_statement'),
        CheckConstraint("sales_dunning_flag IN ('Y', 'N')", name='ck_saledger_valid_dunning'),
        CheckConstraint("sales_charge_interest IN ('Y', 'N')", name='ck_saledger_valid_interest'),
        Index('idx_saledger_name', 'sales_name'),
        Index('idx_saledger_status', 'sales_account_status'),
        Index('idx_saledger_balance', 'sales_balance'),
        {'schema': 'acas'}
    )

class SalesInvoiceRec(Base):
    """Sales Invoice Record"""
    __tablename__ = "sainvoice_rec"
    __table_args__ = {'schema': 'acas'}
    
    invoice_key = Column(String(20), primary_key=True, doc="Invoice number")
    sales_key = Column(String(10), ForeignKey("acas.saledger_rec.sales_key", ondelete="RESTRICT"), nullable=False, doc="Customer code")
    invoice_date = Column(Integer, nullable=False, doc="Invoice date (YYYYMMDD)")
    invoice_amount = Column(Numeric(12, 2), default=0.00, doc="Invoice amount")
    invoice_status = Column(String(1), default='O', doc="Invoice status")
    
    # Audit Trail
    created_at = Column(DateTime(timezone=True), server_default=func.current_timestamp())
    updated_at = Column(DateTime(timezone=True), server_default=func.current_timestamp(), onupdate=func.current_timestamp())
    
    # Relationships
    customer = relationship("SalesLedgerRec", back_populates="invoices")
    lines = relationship("SalesInvoiceLineRec", back_populates="invoice")

class SalesInvoiceLineRec(Base):
    """Sales Invoice Line Record"""
    __tablename__ = "sainvoice_lines_rec"
    __table_args__ = {'schema': 'acas'}
    
    line_id = Column(Integer, primary_key=True, autoincrement=True, doc="Line ID")
    invoice_key = Column(String(20), ForeignKey("acas.sainvoice_rec.invoice_key", ondelete="CASCADE"), nullable=False, doc="Invoice number")
    line_number = Column(Integer, nullable=False, doc="Line number")
    stock_key = Column(String(30), doc="Stock item code")
    description = Column(String(40), doc="Line description")
    quantity = Column(Numeric(15, 3), default=0.000, doc="Quantity")
    unit_price = Column(Numeric(15, 4), default=0.0000, doc="Unit price")
    line_total = Column(Numeric(12, 2), default=0.00, doc="Line total")
    
    # Relationships
    invoice = relationship("SalesInvoiceRec", back_populates="lines")

class SalesItemRec(Base):
    """Sales Item Record - Historical sales data"""
    __tablename__ = "saitm3_rec"
    __table_args__ = {'schema': 'acas'}
    
    item_id = Column(Integer, primary_key=True, autoincrement=True, doc="Item ID")
    sales_key = Column(String(10), ForeignKey("acas.saledger_rec.sales_key", ondelete="RESTRICT"), nullable=False, doc="Customer code")
    stock_key = Column(String(30), doc="Stock item code")
    transaction_date = Column(Integer, doc="Transaction date (YYYYMMDD)")
    quantity = Column(Numeric(15, 3), default=0.000, doc="Quantity")
    value = Column(Numeric(12, 2), default=0.00, doc="Transaction value")
    
    # Relationships
    customer = relationship("SalesLedgerRec", back_populates="items")


class CustomerContactRec(Base):
    """Customer Contact Record - Multiple contacts per customer"""
    __tablename__ = "customer_contacts"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    contact_id = Column(Integer, primary_key=True, autoincrement=True, doc="Contact ID")
    
    # Customer Reference
    sales_key = Column(String(10), ForeignKey("acas.saledger_rec.sales_key", ondelete="CASCADE"), nullable=False, doc="Customer code")
    
    # Contact Information
    contact_type = Column(String(20), nullable=False, doc="Contact type (Primary, Billing, Shipping, etc.)")
    contact_name = Column(String(50), nullable=False, doc="Contact person name")
    title = Column(String(20), doc="Contact title/position")
    department = Column(String(30), doc="Department")
    
    # Communication Details
    phone = Column(String(20), doc="Phone number")
    mobile = Column(String(20), doc="Mobile number")
    fax = Column(String(20), doc="Fax number")
    email = Column(String(100), doc="Email address")
    
    # Address (if different from main customer address)
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
    send_statements = Column(String(1), default='Y', doc="Send statements flag")
    send_invoices = Column(String(1), default='Y', doc="Send invoices flag")
    send_marketing = Column(String(1), default='N', doc="Send marketing material flag")
    
    # Notes
    notes = Column(Text, doc="Contact notes")
    
    # Audit Trail
    created_at = Column(DateTime(timezone=True), server_default=func.current_timestamp())
    updated_at = Column(DateTime(timezone=True), server_default=func.current_timestamp(), onupdate=func.current_timestamp())
    
    # Relationships
    customer = relationship("SalesLedgerRec", foreign_keys=[sales_key])


class CustomerCreditRec(Base):
    """Customer Credit Record - Credit control and risk management"""
    __tablename__ = "customer_credit"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    credit_id = Column(Integer, primary_key=True, autoincrement=True, doc="Credit record ID")
    
    # Customer Reference
    sales_key = Column(String(10), ForeignKey("acas.saledger_rec.sales_key", ondelete="CASCADE"), nullable=False, doc="Customer code")
    
    # Credit Information
    credit_limit = Column(Numeric(12, 2), nullable=False, doc="Current credit limit")
    credit_limit_currency = Column(String(3), default='USD', doc="Credit limit currency")
    temporary_limit = Column(Numeric(12, 2), default=0.00, doc="Temporary credit limit")
    temporary_limit_expiry = Column(Integer, doc="Temporary limit expiry date (YYYYMMDD)")
    
    # Credit Terms
    payment_terms_days = Column(Integer, default=30, doc="Payment terms in days")
    discount_percent = Column(Numeric(5, 2), default=0.00, doc="Early payment discount %")
    discount_days = Column(Integer, default=0, doc="Days for early payment discount")
    
    # Risk Assessment
    credit_rating = Column(String(5), doc="Credit rating (AAA, AA, A, BBB, etc.)")
    risk_category = Column(String(10), default='STANDARD', doc="Risk category")
    credit_status = Column(String(20), default='APPROVED', doc="Credit status")
    
    # Insurance
    credit_insurance = Column(String(1), default='N', doc="Credit insurance flag")
    insurance_limit = Column(Numeric(12, 2), default=0.00, doc="Insurance coverage limit")
    insurance_policy_number = Column(String(30), doc="Insurance policy number")
    
    # Hold/Block Flags
    credit_hold = Column(String(1), default='N', doc="Credit hold flag")
    shipping_hold = Column(String(1), default='N', doc="Shipping hold flag")
    orders_hold = Column(String(1), default='N', doc="Orders hold flag")
    
    # Review Information
    last_review_date = Column(Integer, doc="Last credit review date (YYYYMMDD)")
    next_review_date = Column(Integer, doc="Next credit review date (YYYYMMDD)")
    reviewed_by = Column(String(30), doc="Last reviewed by user")
    
    # Financial Ratios and Metrics
    debt_to_equity_ratio = Column(Numeric(8, 4), doc="Debt to equity ratio")
    current_ratio = Column(Numeric(8, 4), doc="Current ratio")
    quick_ratio = Column(Numeric(8, 4), doc="Quick ratio")
    
    # Payment Behavior
    average_days_to_pay = Column(Numeric(6, 1), doc="Average days to pay")
    payment_trend = Column(String(10), doc="Payment trend (IMPROVING, STABLE, DECLINING)")
    disputed_amount = Column(Numeric(12, 2), default=0.00, doc="Total disputed amount")
    
    # Comments and Notes
    credit_comments = Column(Text, doc="Credit assessment comments")
    internal_notes = Column(Text, doc="Internal credit notes")
    
    # Audit Trail
    created_by = Column(String(30), nullable=False, doc="Created by user")
    created_at = Column(DateTime(timezone=True), server_default=func.current_timestamp())
    updated_by = Column(String(30), doc="Last updated by user")
    updated_at = Column(DateTime(timezone=True), server_default=func.current_timestamp(), onupdate=func.current_timestamp())
    
    # Relationships
    customer = relationship("SalesLedgerRec", foreign_keys=[sales_key])