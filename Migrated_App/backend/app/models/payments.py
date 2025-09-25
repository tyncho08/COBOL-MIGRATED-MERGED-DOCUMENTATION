"""
ACAS Payment Models
SQLAlchemy models for payment processing and allocation
"""
from sqlalchemy import (
    Column, String, Integer, Numeric, Boolean, DateTime, 
    ForeignKey, CheckConstraint, Index
)
from sqlalchemy.orm import relationship
from sqlalchemy.sql import func
from app.core.database import Base
from decimal import Decimal

class PurchasePaymentRec(Base):
    """
    Purchase Payment Header Record
    
    Represents purchase payment runs and check/EFT batches.
    Mirrors COBOL PLPAY_REC structure.
    """
    __tablename__ = "plpay_rec"
    
    # Primary Key - Payment Reference
    pay_key = Column(
        String(9), 
        primary_key=True,
        doc="Payment batch key (9 characters)"
    )
    
    # Payment Control Information
    pay_cont = Column(
        String(1), 
        nullable=False, 
        default='',
        doc="Payment control flag"
    )
    pay_dat = Column(
        Integer, 
        nullable=False,
        doc="Payment date (YYYYMMDD format)"
    )
    
    # Check Information
    pay_cheque = Column(
        Integer, 
        nullable=False,
        doc="Check number (if check payment)"
    )
    
    # Banking Information
    pay_sortcode = Column(
        Integer, 
        nullable=False,
        doc="Bank sort code"
    )
    pay_account = Column(
        Integer, 
        nullable=False,
        doc="Bank account number"
    )
    
    # Payment Total
    pay_gross = Column(
        Numeric(10, 2), 
        nullable=False, 
        default=0.00,
        doc="Gross payment amount"
    )
    
    # Audit Trail
    created_at = Column(
        DateTime(timezone=True), 
        server_default=func.now(),
        doc="Record creation timestamp"
    )
    
    # Relationships
    lines = relationship(
        "PurchasePaymentLineRec", 
        back_populates="payment_header", 
        cascade="all, delete-orphan"
    )
    
    # Table constraints
    __table_args__ = (
        CheckConstraint(
            'pay_dat > 0', 
            name='ck_plpay_valid_payment_date'
        ),
        CheckConstraint(
            'pay_cheque >= 0', 
            name='ck_plpay_valid_cheque_number'
        ),
        CheckConstraint(
            'pay_sortcode >= 0', 
            name='ck_plpay_valid_sortcode'
        ),
        CheckConstraint(
            'pay_account >= 0', 
            name='ck_plpay_valid_account'
        ),
        CheckConstraint(
            'pay_gross >= 0', 
            name='ck_plpay_valid_gross_amount'
        ),
        Index('ix_plpay_date', 'pay_dat'),
        Index('ix_plpay_cheque', 'pay_cheque'),
        Index('ix_plpay_account', 'pay_account'),
        {
            'comment': 'Purchase payment batch headers'
        }
    )
    
    def __repr__(self):
        return f"<PurchasePayment(key='{self.pay_key}', amount={self.pay_gross})>"
    
    @property
    def payment_date_formatted(self) -> str:
        """Get formatted payment date (DD/MM/YYYY)"""
        if self.pay_dat > 0:
            date_str = str(self.pay_dat).zfill(8)
            year = date_str[:4]
            month = date_str[4:6]
            day = date_str[6:8]
            return f"{day}/{month}/{year}"
        return ""
    
    @property
    def is_check_payment(self) -> bool:
        """Check if this is a check payment"""
        return self.pay_cheque > 0
    
    @property
    def bank_account_formatted(self) -> str:
        """Get formatted bank account with sort code"""
        return f"{self.pay_sortcode:06d}-{self.pay_account:08d}"
    
    def calculate_total_lines(self) -> Decimal:
        """Calculate total of all payment lines"""
        total = Decimal('0.00')
        for line in self.lines:
            total += Decimal(str(line.pay_value)) - Decimal(str(line.pay_deduct))
        return total

class PurchasePaymentLineRec(Base):
    """
    Purchase Payment Line Record
    
    Individual payment line items with invoice allocation.
    Mirrors COBOL PLPAY_REC_RG01 structure.
    """
    __tablename__ = "plpay_rec_lines"
    
    # Composite Primary Key
    pay_key = Column(
        String(9), 
        ForeignKey("plpay_rec.pay_key", ondelete="CASCADE"),
        primary_key=True,
        doc="Payment batch key"
    )
    level_j = Column(
        Integer, 
        primary_key=True,
        doc="Line sequence number"
    )
    
    # Payment Line Information
    pay_folio = Column(
        Integer, 
        nullable=False,
        doc="Supplier folio/account number"
    )
    pay_period = Column(
        Integer, 
        nullable=False,
        doc="Payment period"
    )
    
    # Financial Information
    pay_value = Column(
        Numeric(14, 4), 
        nullable=False, 
        default=0.0000,
        doc="Payment value before deductions"
    )
    pay_deduct = Column(
        Numeric(14, 4), 
        nullable=False, 
        default=0.0000,
        doc="Deductions (discounts, etc.)"
    )
    pay_invoice = Column(
        String(10), 
        nullable=False, 
        default='',
        doc="Invoice reference being paid"
    )
    
    # Relationships
    payment_header = relationship("PurchasePaymentRec", back_populates="lines")
    
    # Table constraints
    __table_args__ = (
        CheckConstraint(
            'level_j > 0', 
            name='ck_plpay_lines_valid_level'
        ),
        CheckConstraint(
            'pay_folio >= 0', 
            name='ck_plpay_lines_valid_folio'
        ),
        CheckConstraint(
            'pay_period >= 0', 
            name='ck_plpay_lines_valid_period'
        ),
        CheckConstraint(
            'pay_value >= 0', 
            name='ck_plpay_lines_valid_value'
        ),
        CheckConstraint(
            'pay_deduct >= 0', 
            name='ck_plpay_lines_valid_deduct'
        ),
        Index('ix_plpay_lines_folio', 'pay_folio'),
        Index('ix_plpay_lines_invoice', 'pay_invoice'),
        {
            'comment': 'Purchase payment line items with invoice allocation'
        }
    )
    
    def __repr__(self):
        return f"<PurchasePaymentLine(pay='{self.pay_key}', line={self.level_j}, invoice='{self.pay_invoice}')>"
    
    @property
    def net_payment_amount(self) -> Decimal:
        """Get net payment amount after deductions"""
        return Decimal(str(self.pay_value)) - Decimal(str(self.pay_deduct))
    
    @property
    def discount_percentage(self) -> Decimal:
        """Calculate discount percentage"""
        if self.pay_value == 0:
            return Decimal('0.00')
        return (Decimal(str(self.pay_deduct)) / Decimal(str(self.pay_value))) * Decimal('100')
    
    def validate_amounts(self) -> bool:
        """Validate payment amounts are reasonable"""
        return (self.pay_value >= 0 and 
                self.pay_deduct >= 0 and 
                self.pay_deduct <= self.pay_value)

# Additional payment-related models can be added here for:
# - Sales payments (cash receipts)
# - Payment allocations
# - Bank reconciliation
# - Electronic payments (BACS, CHAPS, etc.)

class SalesPaymentRec(Base):
    """
    Sales Payment Record (Cash Receipts)
    
    Represents customer payments received.
    """
    __tablename__ = "sales_payment_rec"
    
    # Primary Key
    payment_key = Column(
        String(15), 
        primary_key=True,
        doc="Unique payment reference"
    )
    
    # Customer Reference
    sales_key = Column(
        String(7), 
        ForeignKey("saledger_rec.sales_key", ondelete="RESTRICT"),
        nullable=False,
        doc="Customer code"
    )
    
    # Payment Information
    payment_date = Column(
        Integer, 
        nullable=False,
        doc="Payment date (YYYYMMDD)"
    )
    payment_method = Column(
        String(2), 
        nullable=False,
        doc="Payment method: CH=Check, CA=Cash, BT=Bank Transfer, CC=Credit Card"
    )
    payment_reference = Column(
        String(20), 
        nullable=False, 
        default='',
        doc="External reference (check number, transfer ref, etc.)"
    )
    
    # Financial Information
    payment_amount = Column(
        Numeric(12, 2), 
        nullable=False,
        doc="Total payment amount received"
    )
    discount_taken = Column(
        Numeric(12, 2), 
        nullable=False, 
        default=0.00,
        doc="Settlement discount taken by customer"
    )
    bank_charges = Column(
        Numeric(12, 2), 
        nullable=False, 
        default=0.00,
        doc="Bank charges deducted"
    )
    
    # Allocation Status
    allocated_amount = Column(
        Numeric(12, 2), 
        nullable=False, 
        default=0.00,
        doc="Amount allocated to invoices"
    )
    unallocated_amount = Column(
        Numeric(12, 2), 
        nullable=False, 
        default=0.00,
        doc="Unallocated amount (payments on account)"
    )
    
    # Banking Information
    bank_account = Column(
        String(10), 
        nullable=False, 
        default='',
        doc="Bank account code where deposited"
    )
    deposit_date = Column(
        Integer,
        nullable=True,
        doc="Date deposited to bank (YYYYMMDD)"
    )
    
    # Status and Control
    payment_status = Column(
        String(1), 
        nullable=False, 
        default='U',
        doc="Status: U=Unallocated, A=Allocated, R=Reversed"
    )
    
    # General Ledger Integration
    posted_to_gl = Column(
        Boolean, 
        nullable=False, 
        default=False,
        doc="Posted to General Ledger"
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
        doc="User who entered payment"
    )
    created_at = Column(
        DateTime(timezone=True), 
        server_default=func.now(),
        doc="Entry timestamp"
    )
    updated_at = Column(
        DateTime(timezone=True), 
        server_default=func.now(),
        onupdate=func.now(),
        doc="Last update timestamp"
    )
    
    # Table constraints
    __table_args__ = (
        CheckConstraint(
            "payment_method IN ('CH', 'CA', 'BT', 'CC', 'DD', 'SO')", 
            name='ck_sales_payment_valid_method'
        ),
        CheckConstraint(
            "payment_status IN ('U', 'A', 'R')", 
            name='ck_sales_payment_valid_status'
        ),
        CheckConstraint(
            'payment_amount > 0', 
            name='ck_sales_payment_valid_amount'
        ),
        CheckConstraint(
            'allocated_amount + unallocated_amount = payment_amount - discount_taken - bank_charges', 
            name='ck_sales_payment_valid_allocation'
        ),
        Index('ix_sales_payment_customer', 'sales_key'),
        Index('ix_sales_payment_date', 'payment_date'),
        Index('ix_sales_payment_method', 'payment_method'),
        Index('ix_sales_payment_status', 'payment_status'),
        {
            'comment': 'Customer payments received with allocation status'
        }
    )
    
    def __repr__(self):
        return f"<SalesPayment(key='{self.payment_key}', customer='{self.sales_key}', amount={self.payment_amount})>"
    
    @property
    def net_payment_amount(self) -> Decimal:
        """Get net payment amount after deductions"""
        return (Decimal(str(self.payment_amount)) - 
                Decimal(str(self.discount_taken)) - 
                Decimal(str(self.bank_charges)))
    
    @property
    def is_fully_allocated(self) -> bool:
        """Check if payment is fully allocated"""
        return self.payment_status == 'A' and self.unallocated_amount == 0
    
    @property
    def payment_method_description(self) -> str:
        """Get human-readable payment method"""
        methods = {
            'CH': 'Check/Cheque',
            'CA': 'Cash',
            'BT': 'Bank Transfer',
            'CC': 'Credit Card',
            'DD': 'Direct Debit',
            'SO': 'Standing Order'
        }
        return methods.get(self.payment_method, 'Unknown')
    
    @property 
    def payment_date_formatted(self) -> str:
        """Get formatted payment date"""
        if self.payment_date > 0:
            date_str = str(self.payment_date).zfill(8)
            year = date_str[:4]
            month = date_str[4:6]
            day = date_str[6:8]
            return f"{day}/{month}/{year}"
        return ""