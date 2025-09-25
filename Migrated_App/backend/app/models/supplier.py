"""
ACAS Supplier (Purchase Ledger) Models
SQLAlchemy models for supplier management and purchase processing
"""
from sqlalchemy import (
    Column, String, Integer, Numeric, Boolean, DateTime, 
    ForeignKey, CheckConstraint, Index
)
from sqlalchemy.orm import relationship
from sqlalchemy.sql import func
from app.core.database import Base

class PurchaseLedgerRec(Base):
    """
    Purchase Ledger Record - Supplier Master
    
    Represents supplier master data with complete financial history.
    Mirrors COBOL PULEDGER_REC structure.
    """
    __tablename__ = "puledger_rec"
    
    # Primary Key - Supplier Code
    purch_key = Column(
        String(7), 
        primary_key=True,
        doc="Supplier code (7 characters)"
    )
    
    # Supplier Identity Information
    purch_name = Column(
        String(30), 
        nullable=False, 
        default='',
        doc="Supplier name"
    )
    purch_address_1 = Column(
        String(30), 
        nullable=False, 
        default='',
        doc="Address line 1"
    )
    purch_address_2 = Column(
        String(30), 
        nullable=False, 
        default='',
        doc="Address line 2"
    )
    purch_address_3 = Column(
        String(30), 
        nullable=False, 
        default='',
        doc="Address line 3"
    )
    purch_address_4 = Column(
        String(30), 
        nullable=False, 
        default='',
        doc="Address line 4"
    )
    purch_address_5 = Column(
        String(30), 
        nullable=False, 
        default='',
        doc="Address line 5 (postcode)"
    )
    
    # Contact Information
    purch_contact = Column(
        String(25), 
        nullable=False, 
        default='',
        doc="Primary contact person"
    )
    purch_phone = Column(
        String(20), 
        nullable=False, 
        default='',
        doc="Phone number"
    )
    purch_email = Column(
        String(40), 
        nullable=False, 
        default='',
        doc="Email address"
    )
    purch_fax = Column(
        String(20), 
        nullable=False, 
        default='',
        doc="Fax number"
    )
    
    # Payment Terms and Configuration
    purch_payment_terms = Column(
        String(10), 
        nullable=False, 
        default='',
        doc="Payment terms code"
    )
    purch_discount_rate = Column(
        Numeric(5, 2), 
        nullable=False, 
        default=0.00,
        doc="Supplier discount rate percentage"
    )
    purch_tax_code = Column(
        String(6), 
        nullable=False, 
        default='',
        doc="Tax/VAT code"
    )
    
    # Banking Details for Payments
    purch_bank_name = Column(
        String(30), 
        nullable=False, 
        default='',
        doc="Bank name"
    )
    purch_bank_sort_code = Column(
        String(10), 
        nullable=False, 
        default='',
        doc="Bank sort code"
    )
    purch_bank_account = Column(
        String(15), 
        nullable=False, 
        default='',
        doc="Bank account number"
    )
    
    # Account Status and Control
    purch_account_status = Column(
        String(1), 
        nullable=False, 
        default='A',
        doc="Account status: A=Active, H=Hold, C=Closed"
    )
    purch_hold_flag = Column(
        Boolean, 
        nullable=False, 
        default=False,
        doc="Account on hold for payment reasons"
    )
    
    # Current Financial Position
    purch_balance = Column(
        Numeric(11, 2), 
        nullable=False, 
        default=0.00,
        doc="Current account balance (credit balance)"
    )
    purch_ytd_turnover = Column(
        Numeric(12, 2), 
        nullable=False, 
        default=0.00,
        doc="Year-to-date purchase turnover"
    )
    purch_last_invoice_date = Column(
        Integer,
        nullable=True,
        doc="Last invoice date (YYYYMMDD format)"
    )
    purch_last_payment_date = Column(
        Integer,
        nullable=True,
        doc="Last payment date (YYYYMMDD format)"
    )
    
    # Monthly Purchase History (13 periods for year + adjustment)
    purch_turn_01 = Column(Numeric(10, 2), nullable=False, default=0.00, doc="Period 1 purchases")
    purch_turn_02 = Column(Numeric(10, 2), nullable=False, default=0.00, doc="Period 2 purchases")
    purch_turn_03 = Column(Numeric(10, 2), nullable=False, default=0.00, doc="Period 3 purchases")
    purch_turn_04 = Column(Numeric(10, 2), nullable=False, default=0.00, doc="Period 4 purchases")
    purch_turn_05 = Column(Numeric(10, 2), nullable=False, default=0.00, doc="Period 5 purchases")
    purch_turn_06 = Column(Numeric(10, 2), nullable=False, default=0.00, doc="Period 6 purchases")
    purch_turn_07 = Column(Numeric(10, 2), nullable=False, default=0.00, doc="Period 7 purchases")
    purch_turn_08 = Column(Numeric(10, 2), nullable=False, default=0.00, doc="Period 8 purchases")
    purch_turn_09 = Column(Numeric(10, 2), nullable=False, default=0.00, doc="Period 9 purchases")
    purch_turn_10 = Column(Numeric(10, 2), nullable=False, default=0.00, doc="Period 10 purchases")
    purch_turn_11 = Column(Numeric(10, 2), nullable=False, default=0.00, doc="Period 11 purchases")
    purch_turn_12 = Column(Numeric(10, 2), nullable=False, default=0.00, doc="Period 12 purchases")
    purch_turn_13 = Column(Numeric(10, 2), nullable=False, default=0.00, doc="Period 13 purchases (adjustment)")
    
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
    invoices = relationship("PurchaseInvoiceRec", back_populates="supplier")
    items = relationship("PurchaseItemRec", back_populates="supplier")
    
    # Table constraints
    __table_args__ = (
        CheckConstraint(
            "purch_account_status IN ('A', 'H', 'C')", 
            name='ck_puledger_valid_status'
        ),
        CheckConstraint(
            'purch_discount_rate >= 0 AND purch_discount_rate <= 100', 
            name='ck_puledger_valid_discount_rate'
        ),
        Index('ix_puledger_name', 'purch_name'),
        Index('ix_puledger_status', 'purch_account_status'),
        {
            'comment': 'Supplier master records with financial history'
        }
    )
    
    def __repr__(self):
        return f"<PurchaseLedger(key='{self.purch_key}', name='{self.purch_name}')>"
    
    @property
    def is_active(self) -> bool:
        """Check if supplier account is active"""
        return self.purch_account_status == 'A' and not self.purch_hold_flag
    
    def get_full_address(self) -> str:
        """Get formatted full address"""
        address_lines = [
            self.purch_address_1, self.purch_address_2, self.purch_address_3,
            self.purch_address_4, self.purch_address_5
        ]
        return '\n'.join(line for line in address_lines if line.strip())
    
    def get_period_purchases(self, period: int) -> float:
        """Get purchases for specific period (1-13)"""
        if period < 1 or period > 13:
            return 0.0
        return float(getattr(self, f'purch_turn_{period:02d}'))

class PurchaseInvoiceRec(Base):
    """
    Purchase Invoice Header Record
    
    Represents purchase invoice headers with matching status.
    """
    __tablename__ = "puinvoice_rec"
    
    # Primary Key
    invoice_key = Column(
        String(15), 
        primary_key=True,
        doc="Internal invoice number/key"
    )
    
    # Foreign Key to Supplier
    purch_key = Column(
        String(7), 
        ForeignKey("puledger_rec.purch_key", ondelete="RESTRICT"),
        nullable=False,
        doc="Supplier code"
    )
    
    # Supplier Invoice Information
    supplier_invoice_no = Column(
        String(20), 
        nullable=False, 
        default='',
        doc="Supplier's invoice number"
    )
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
    purchase_order_no = Column(
        String(15), 
        nullable=False, 
        default='',
        doc="Purchase order number"
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
    
    # Three-Way Matching Status
    three_way_matched = Column(
        Boolean, 
        nullable=False, 
        default=False,
        doc="Three-way matching completed"
    )
    goods_received = Column(
        Boolean, 
        nullable=False, 
        default=False,
        doc="Goods receipt confirmed"
    )
    goods_receipt_ref = Column(
        String(15), 
        nullable=False, 
        default='',
        doc="Goods receipt reference"
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
    supplier = relationship("PurchaseLedgerRec", back_populates="invoices")
    lines = relationship("PurchaseInvoiceLineRec", back_populates="invoice", cascade="all, delete-orphan")
    
    # Table constraints
    __table_args__ = (
        CheckConstraint(
            "invoice_status IN ('O', 'P', 'C')", 
            name='ck_puinvoice_valid_status'
        ),
        CheckConstraint(
            'gross_amount = net_amount + tax_amount', 
            name='ck_puinvoice_valid_totals'
        ),
        Index('ix_puinvoice_supplier', 'purch_key'),
        Index('ix_puinvoice_date', 'invoice_date'),
        Index('ix_puinvoice_status', 'invoice_status'),
        Index('ix_puinvoice_supplier_no', 'supplier_invoice_no'),
        {
            'comment': 'Purchase invoice headers with matching status'
        }
    )
    
    def __repr__(self):
        return f"<PurchaseInvoice(key='{self.invoice_key}', supplier='{self.purch_key}', amount={self.gross_amount})>"

class PurchaseInvoiceLineRec(Base):
    """
    Purchase Invoice Line Record
    
    Represents individual line items on purchase invoices with matching info.
    """
    __tablename__ = "puinv_lines_rec"
    
    # Composite Primary Key
    invoice_key = Column(
        String(15), 
        ForeignKey("puinvoice_rec.invoice_key", ondelete="CASCADE"),
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
    unit_cost = Column(
        Numeric(10, 4), 
        nullable=False, 
        default=0.0000,
        doc="Unit cost"
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
    
    # Three-Way Matching Information
    po_line_ref = Column(
        String(20), 
        nullable=False, 
        default='',
        doc="Purchase order line reference"
    )
    gr_line_ref = Column(
        String(20), 
        nullable=False, 
        default='',
        doc="Goods receipt line reference"
    )
    matched = Column(
        Boolean, 
        nullable=False, 
        default=False,
        doc="Line matched in three-way matching"
    )
    
    # Relationships
    invoice = relationship("PurchaseInvoiceRec", back_populates="lines")
    
    # Table constraints
    __table_args__ = (
        CheckConstraint(
            'line_number > 0', 
            name='ck_puinv_lines_valid_line_number'
        ),
        CheckConstraint(
            'quantity >= 0', 
            name='ck_puinv_lines_valid_quantity'
        ),
        CheckConstraint(
            'unit_cost >= 0', 
            name='ck_puinv_lines_valid_unit_cost'
        ),
        CheckConstraint(
            'line_total_amount = line_net_amount + line_tax_amount', 
            name='ck_puinv_lines_valid_totals'
        ),
        {
            'comment': 'Purchase invoice line items with matching information'
        }
    )
    
    def __repr__(self):
        return f"<PurchaseInvoiceLine(invoice='{self.invoice_key}', line={self.line_number}, item='{self.stock_key}')>"

class PurchaseItemRec(Base):
    """
    Purchase Item Record (Open Items)
    
    Represents individual purchase transactions for aging and allocation.
    Mirrors COBOL PUITM5_REC structure.
    """
    __tablename__ = "puitm5_rec"
    
    # Primary Key
    item_key = Column(
        String(20), 
        primary_key=True,
        doc="Unique item key"
    )
    
    # Supplier Reference
    purch_key = Column(
        String(7), 
        ForeignKey("puledger_rec.purch_key", ondelete="RESTRICT"),
        nullable=False,
        doc="Supplier code"
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
    supplier = relationship("PurchaseLedgerRec", back_populates="items")
    
    # Table constraints
    __table_args__ = (
        CheckConstraint(
            "item_type IN ('IN', 'PY', 'CM', 'DM', 'JN')", 
            name='ck_puitm5_valid_item_type'
        ),
        CheckConstraint(
            "item_status IN ('O', 'C', 'P')", 
            name='ck_puitm5_valid_status'
        ),
        CheckConstraint(
            "aging_bucket IN ('1', '2', '3', '4')", 
            name='ck_puitm5_valid_aging_bucket'
        ),
        Index('ix_puitm5_supplier', 'purch_key'),
        Index('ix_puitm5_type', 'item_type'),
        Index('ix_puitm5_date', 'item_date'),
        Index('ix_puitm5_aging', 'aging_bucket'),
        {
            'comment': 'Purchase open items for aging and allocation'
        }
    )
    
    def __repr__(self):
        return f"<PurchaseItem(key='{self.item_key}', supplier='{self.purch_key}', type='{self.item_type}')>"