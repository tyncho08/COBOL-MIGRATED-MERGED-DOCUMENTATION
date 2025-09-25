"""
ACAS Customer (Sales Ledger) Models
SQLAlchemy models for customer management and sales processing
"""
from sqlalchemy import (
    Column, String, Integer, Numeric, Boolean, DateTime, 
    ForeignKey, CheckConstraint, Index
)
from sqlalchemy.orm import relationship
from sqlalchemy.sql import func
from app.core.database import Base

class SalesLedgerRec(Base):
    """
    Sales Ledger Record - Customer Master
    
    Represents customer master data with complete financial history.
    Mirrors COBOL SALEDGER_REC structure with 47 fields.
    """
    __tablename__ = "saledger_rec"
    
    # Primary Key - Customer Code
    sales_key = Column(
        String(7), 
        primary_key=True,
        doc="Customer code (7 characters)"
    )
    
    # Customer Identity Information
    sales_name = Column(
        String(30), 
        nullable=False, 
        default='',
        doc="Customer name"
    )
    sales_address_1 = Column(
        String(30), 
        nullable=False, 
        default='',
        doc="Address line 1"
    )
    sales_address_2 = Column(
        String(30), 
        nullable=False, 
        default='',
        doc="Address line 2"
    )
    sales_address_3 = Column(
        String(30), 
        nullable=False, 
        default='',
        doc="Address line 3"
    )
    sales_address_4 = Column(
        String(30), 
        nullable=False, 
        default='',
        doc="Address line 4"
    )
    sales_address_5 = Column(
        String(30), 
        nullable=False, 
        default='',
        doc="Address line 5 (postcode)"
    )
    
    # Contact Information
    sales_contact = Column(
        String(25), 
        nullable=False, 
        default='',
        doc="Primary contact person"
    )
    sales_phone = Column(
        String(20), 
        nullable=False, 
        default='',
        doc="Phone number"
    )
    sales_email = Column(
        String(40), 
        nullable=False, 
        default='',
        doc="Email address"
    )
    sales_fax = Column(
        String(20), 
        nullable=False, 
        default='',
        doc="Fax number"
    )
    
    # Financial Terms and Configuration
    sales_credit_limit = Column(
        Numeric(10, 2), 
        nullable=False, 
        default=0.00,
        doc="Customer credit limit"
    )
    sales_discount_rate = Column(
        Numeric(5, 2), 
        nullable=False, 
        default=0.00,
        doc="Customer discount rate percentage"
    )
    sales_payment_terms = Column(
        String(10), 
        nullable=False, 
        default='',
        doc="Payment terms code"
    )
    sales_tax_code = Column(
        String(6), 
        nullable=False, 
        default='',
        doc="Tax/VAT code"
    )
    
    # Account Status and Control
    sales_account_status = Column(
        String(1), 
        nullable=False, 
        default='A',
        doc="Account status: A=Active, H=Hold, C=Closed"
    )
    sales_hold_flag = Column(
        Boolean, 
        nullable=False, 
        default=False,
        doc="Account on hold for credit reasons"
    )
    sales_credit_rating = Column(
        String(1), 
        nullable=False, 
        default='B',
        doc="Credit rating: A=Excellent, B=Good, C=Fair, D=Poor"
    )
    
    # Current Financial Position
    sales_balance = Column(
        Numeric(11, 2), 
        nullable=False, 
        default=0.00,
        doc="Current account balance"
    )
    sales_ytd_turnover = Column(
        Numeric(12, 2), 
        nullable=False, 
        default=0.00,
        doc="Year-to-date turnover"
    )
    sales_last_invoice_date = Column(
        Integer,
        nullable=True,
        doc="Last invoice date (YYYYMMDD format)"
    )
    sales_last_payment_date = Column(
        Integer,
        nullable=True,
        doc="Last payment date (YYYYMMDD format)"
    )
    
    # Monthly Turnover History (13 periods for year + adjustment)
    sales_turn_01 = Column(Numeric(10, 2), nullable=False, default=0.00, doc="Period 1 turnover")
    sales_turn_02 = Column(Numeric(10, 2), nullable=False, default=0.00, doc="Period 2 turnover")
    sales_turn_03 = Column(Numeric(10, 2), nullable=False, default=0.00, doc="Period 3 turnover")
    sales_turn_04 = Column(Numeric(10, 2), nullable=False, default=0.00, doc="Period 4 turnover")
    sales_turn_05 = Column(Numeric(10, 2), nullable=False, default=0.00, doc="Period 5 turnover")
    sales_turn_06 = Column(Numeric(10, 2), nullable=False, default=0.00, doc="Period 6 turnover")
    sales_turn_07 = Column(Numeric(10, 2), nullable=False, default=0.00, doc="Period 7 turnover")
    sales_turn_08 = Column(Numeric(10, 2), nullable=False, default=0.00, doc="Period 8 turnover")
    sales_turn_09 = Column(Numeric(10, 2), nullable=False, default=0.00, doc="Period 9 turnover")
    sales_turn_10 = Column(Numeric(10, 2), nullable=False, default=0.00, doc="Period 10 turnover")
    sales_turn_11 = Column(Numeric(10, 2), nullable=False, default=0.00, doc="Period 11 turnover")
    sales_turn_12 = Column(Numeric(10, 2), nullable=False, default=0.00, doc="Period 12 turnover")
    sales_turn_13 = Column(Numeric(10, 2), nullable=False, default=0.00, doc="Period 13 turnover (adjustment)")
    
    # Audit Trail
    created_at = Column(
        DateTime(timezone=True), 
        server_default=func.now(),
        doc="Record creation timestamp"
    )
    updated_at = Column(
        DateTime(timezone=True), 
        server_default=func.now(),
        onupdate=func.now(),
        doc="Last update timestamp"
    )
    
    # Relationships
    invoices = relationship("SalesInvoiceRec", back_populates="customer")
    items = relationship("SalesItemRec", back_populates="customer")
    
    # Table constraints
    __table_args__ = (
        CheckConstraint(
            "sales_account_status IN ('A', 'H', 'C')", 
            name='ck_saledger_valid_status'
        ),
        CheckConstraint(
            "sales_credit_rating IN ('A', 'B', 'C', 'D')", 
            name='ck_saledger_valid_rating'
        ),
        CheckConstraint(
            'sales_credit_limit >= 0', 
            name='ck_saledger_valid_credit_limit'
        ),
        CheckConstraint(
            'sales_discount_rate >= 0 AND sales_discount_rate <= 100', 
            name='ck_saledger_valid_discount_rate'
        ),
        Index('ix_saledger_name', 'sales_name'),
        Index('ix_saledger_status', 'sales_account_status'),
        {
            'comment': 'Customer master records with financial history'
        }
    )
    
    def __repr__(self):
        return f"<SalesLedger(key='{self.sales_key}', name='{self.sales_name}')>"
    
    @property
    def is_active(self) -> bool:
        """Check if customer account is active"""
        return self.sales_account_status == 'A' and not self.sales_hold_flag
    
    @property
    def available_credit(self) -> float:
        """Calculate available credit"""
        return float(self.sales_credit_limit - self.sales_balance)
    
    @property
    def is_over_limit(self) -> bool:
        """Check if customer is over credit limit"""
        return self.sales_balance > self.sales_credit_limit
    
    def get_full_address(self) -> str:
        """Get formatted full address"""
        address_lines = [
            self.sales_address_1, self.sales_address_2, self.sales_address_3,
            self.sales_address_4, self.sales_address_5
        ]
        return '\n'.join(line for line in address_lines if line.strip())
    
    def get_period_turnover(self, period: int) -> float:
        """Get turnover for specific period (1-13)"""
        if period < 1 or period > 13:
            return 0.0
        return float(getattr(self, f'sales_turn_{period:02d}'))

class SalesInvoiceRec(Base):
    """
    Sales Invoice Header Record
    
    Represents sales invoice headers with financial totals.
    """
    __tablename__ = "sainvoice_rec"
    
    # Primary Key
    invoice_key = Column(
        String(15), 
        primary_key=True,
        doc="Invoice number/key"
    )
    
    # Foreign Key to Customer
    sales_key = Column(
        String(7), 
        ForeignKey("saledger_rec.sales_key", ondelete="RESTRICT"),
        nullable=False,
        doc="Customer code"
    )
    
    # Invoice Dates
    invoice_date = Column(
        Integer, 
        nullable=False,
        doc="Invoice date (YYYYMMDD format)"
    )
    due_date = Column(
        Integer, 
        nullable=False,
        doc="Due date (YYYYMMDD format)"
    )
    
    # Invoice Status and References
    invoice_status = Column(
        String(1), 
        nullable=False, 
        default='O',
        doc="Invoice status: O=Open, P=Paid, C=Cancelled"
    )
    order_number = Column(
        String(15), 
        nullable=False, 
        default='',
        doc="Original order number"
    )
    customer_po = Column(
        String(20), 
        nullable=False, 
        default='',
        doc="Customer purchase order number"
    )
    
    # Financial Totals
    net_amount = Column(
        Numeric(12, 2), 
        nullable=False, 
        default=0.00,
        doc="Net amount before tax"
    )
    tax_amount = Column(
        Numeric(12, 2), 
        nullable=False, 
        default=0.00,
        doc="Total tax amount"
    )
    gross_amount = Column(
        Numeric(12, 2), 
        nullable=False, 
        default=0.00,
        doc="Gross amount including tax"
    )
    discount_amount = Column(
        Numeric(12, 2), 
        nullable=False, 
        default=0.00,
        doc="Total discount amount"
    )
    amount_outstanding = Column(
        Numeric(12, 2), 
        nullable=False, 
        default=0.00,
        doc="Amount still outstanding"
    )
    
    # Payment Terms
    payment_terms = Column(
        String(10), 
        nullable=False, 
        default='',
        doc="Payment terms code"
    )
    discount_percent = Column(
        Numeric(5, 2), 
        nullable=False, 
        default=0.00,
        doc="Settlement discount percentage"
    )
    
    # Delivery Information
    delivery_address = Column(
        String(150), 
        nullable=False, 
        default='',
        doc="Delivery address"
    )
    delivery_date = Column(
        Integer,
        nullable=True,
        doc="Delivery date (YYYYMMDD format)"
    )
    
    # General Ledger Integration
    posted_to_gl = Column(
        Boolean, 
        nullable=False, 
        default=False,
        doc="Posted to General Ledger flag"
    )
    gl_batch_key = Column(
        Integer,
        nullable=True,
        doc="GL batch key if posted"
    )
    
    # Audit Trail
    created_by = Column(
        String(10), 
        nullable=False, 
        default='',
        doc="Created by user ID"
    )
    created_at = Column(
        DateTime(timezone=True), 
        server_default=func.now(),
        doc="Record creation timestamp"
    )
    updated_at = Column(
        DateTime(timezone=True), 
        server_default=func.now(),
        onupdate=func.now(),
        doc="Last update timestamp"
    )
    
    # Relationships
    customer = relationship("SalesLedgerRec", back_populates="invoices")
    lines = relationship("SalesInvoiceLineRec", back_populates="invoice", cascade="all, delete-orphan")
    
    # Table constraints
    __table_args__ = (
        CheckConstraint(
            "invoice_status IN ('O', 'P', 'C')", 
            name='ck_sainvoice_valid_status'
        ),
        CheckConstraint(
            'gross_amount = net_amount + tax_amount', 
            name='ck_sainvoice_valid_totals'
        ),
        Index('ix_sainvoice_customer', 'sales_key'),
        Index('ix_sainvoice_date', 'invoice_date'),
        Index('ix_sainvoice_status', 'invoice_status'),
        {
            'comment': 'Sales invoice headers with financial totals'
        }
    )
    
    def __repr__(self):
        return f"<SalesInvoice(key='{self.invoice_key}', customer='{self.sales_key}', amount={self.gross_amount})>"

class SalesInvoiceLineRec(Base):
    """
    Sales Invoice Line Record
    
    Represents individual line items on sales invoices.
    """
    __tablename__ = "sainv_lines_rec"
    
    # Composite Primary Key
    invoice_key = Column(
        String(15), 
        ForeignKey("sainvoice_rec.invoice_key", ondelete="CASCADE"),
        primary_key=True,
        doc="Invoice number"
    )
    line_number = Column(
        Integer, 
        primary_key=True,
        doc="Line number within invoice"
    )
    
    # Stock Item Reference
    stock_key = Column(
        String(13), 
        nullable=False, 
        default='',
        doc="Stock item code"
    )
    item_description = Column(
        String(40), 
        nullable=False, 
        default='',
        doc="Item description"
    )
    
    # Quantity and Pricing
    quantity = Column(
        Numeric(12, 3), 
        nullable=False, 
        default=0.000,
        doc="Quantity"
    )
    unit_price = Column(
        Numeric(10, 4), 
        nullable=False, 
        default=0.0000,
        doc="Unit price"
    )
    line_discount_percent = Column(
        Numeric(5, 2), 
        nullable=False, 
        default=0.00,
        doc="Line discount percentage"
    )
    
    # Line Totals
    line_net_amount = Column(
        Numeric(12, 2), 
        nullable=False, 
        default=0.00,
        doc="Line net amount"
    )
    line_tax_code = Column(
        String(6), 
        nullable=False, 
        default='',
        doc="Line tax code"
    )
    line_tax_amount = Column(
        Numeric(12, 2), 
        nullable=False, 
        default=0.00,
        doc="Line tax amount"
    )
    line_total_amount = Column(
        Numeric(12, 2), 
        nullable=False, 
        default=0.00,
        doc="Line total amount"
    )
    
    # Relationships
    invoice = relationship("SalesInvoiceRec", back_populates="lines")
    
    # Table constraints
    __table_args__ = (
        CheckConstraint(
            'line_number > 0', 
            name='ck_sainv_lines_valid_line_number'
        ),
        CheckConstraint(
            'quantity >= 0', 
            name='ck_sainv_lines_valid_quantity'
        ),
        CheckConstraint(
            'unit_price >= 0', 
            name='ck_sainv_lines_valid_unit_price'
        ),
        CheckConstraint(
            'line_total_amount = line_net_amount + line_tax_amount', 
            name='ck_sainv_lines_valid_totals'
        ),
        {
            'comment': 'Sales invoice line items'
        }
    )
    
    def __repr__(self):
        return f"<SalesInvoiceLine(invoice='{self.invoice_key}', line={self.line_number}, item='{self.stock_key}')>"

class SalesItemRec(Base):
    """
    Sales Item Record (Open Items)
    
    Represents individual sales transactions for aging and allocation.
    Mirrors COBOL SAITM3_REC structure.
    """
    __tablename__ = "saitm3_rec"
    
    # Primary Key
    item_key = Column(
        String(20), 
        primary_key=True,
        doc="Unique item key"
    )
    
    # Customer Reference
    sales_key = Column(
        String(7), 
        ForeignKey("saledger_rec.sales_key", ondelete="RESTRICT"),
        nullable=False,
        doc="Customer code"
    )
    
    # Item Type and Dates
    item_type = Column(
        String(2), 
        nullable=False,
        doc="Item type: IN=Invoice, PY=Payment, CM=Credit Memo, etc."
    )
    item_date = Column(
        Integer, 
        nullable=False,
        doc="Item date (YYYYMMDD format)"
    )
    due_date = Column(
        Integer,
        nullable=True,
        doc="Due date (YYYYMMDD format)"
    )
    
    # Financial Information
    item_reference = Column(
        String(15), 
        nullable=False, 
        default='',
        doc="Reference number (invoice/payment)"
    )
    item_amount = Column(
        Numeric(12, 2), 
        nullable=False, 
        default=0.00,
        doc="Original item amount"
    )
    item_outstanding = Column(
        Numeric(12, 2), 
        nullable=False, 
        default=0.00,
        doc="Outstanding amount"
    )
    
    # Status and Allocation
    item_status = Column(
        String(1), 
        nullable=False, 
        default='O',
        doc="Status: O=Open, C=Closed, P=Partially Paid"
    )
    allocation_key = Column(
        String(20), 
        nullable=False, 
        default='',
        doc="Allocation reference"
    )
    
    # Aging Information
    days_outstanding = Column(
        Integer, 
        nullable=False, 
        default=0,
        doc="Days outstanding"
    )
    aging_bucket = Column(
        String(1), 
        nullable=False, 
        default='1',
        doc="Aging bucket: 1=Current, 2=30 days, 3=60 days, 4=90+ days"
    )
    
    # Relationships
    customer = relationship("SalesLedgerRec", back_populates="items")
    
    # Table constraints
    __table_args__ = (
        CheckConstraint(
            "item_type IN ('IN', 'PY', 'CM', 'DM', 'JN')", 
            name='ck_saitm3_valid_item_type'
        ),
        CheckConstraint(
            "item_status IN ('O', 'C', 'P')", 
            name='ck_saitm3_valid_status'
        ),
        CheckConstraint(
            "aging_bucket IN ('1', '2', '3', '4')", 
            name='ck_saitm3_valid_aging_bucket'
        ),
        Index('ix_saitm3_customer', 'sales_key'),
        Index('ix_saitm3_type', 'item_type'),
        Index('ix_saitm3_date', 'item_date'),
        Index('ix_saitm3_aging', 'aging_bucket'),
        {
            'comment': 'Sales open items for aging and allocation'
        }
    )
    
    def __repr__(self):
        return f"<SalesItem(key='{self.item_key}', customer='{self.sales_key}', type='{self.item_type}')>"