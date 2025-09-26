"""
ACAS General Ledger Models
SQLAlchemy models for general ledger accounts and postings
"""
from sqlalchemy import (
    Column, String, Integer, Numeric, Boolean, DateTime, BigInteger,
    ForeignKey, CheckConstraint, Index
)
from sqlalchemy.orm import relationship
from sqlalchemy.sql import func
from app.core.database import Base
from decimal import Decimal

class GLLedgerRec(Base):
    """
    General Ledger Account Record
    
    Represents chart of accounts with current and historical balances.
    Mirrors COBOL GLLEDGER_REC structure.
    """
    __tablename__ = "glledger_rec"
    
    # Primary Key - Account Code
    ledger_key = Column(
        Integer, 
        primary_key=True,
        doc="General Ledger account code (8 digits)"
    )
    
    # Account Classification
    ledger_type = Column(
        Integer, 
        nullable=False,
        doc="Account type: 1=Asset, 2=Liability, 3=Capital, 4=Income, 5=Expense"
    )
    ledger_place = Column(
        String(1), 
        nullable=False,
        doc="Balance sheet/P&L placement: B=Balance Sheet, P=P&L"
    )
    ledger_level = Column(
        Integer, 
        nullable=False,
        doc="Account level in hierarchy (1-9)"
    )
    
    # Account Description
    ledger_name = Column(
        String(32), 
        nullable=False,
        doc="Account name/description"
    )
    
    # Current and Historical Balances
    ledger_balance = Column(
        Numeric(10, 2), 
        nullable=False, 
        default=0.00,
        doc="Current period balance"
    )
    ledger_last = Column(
        Numeric(10, 2), 
        nullable=False, 
        default=0.00,
        doc="Previous period balance"
    )
    
    # Quarterly Balances for Comparison
    ledger_q1 = Column(
        Numeric(10, 2), 
        nullable=False, 
        default=0.00,
        doc="Quarter 1 balance"
    )
    ledger_q2 = Column(
        Numeric(10, 2), 
        nullable=False, 
        default=0.00,
        doc="Quarter 2 balance"
    )
    ledger_q3 = Column(
        Numeric(10, 2), 
        nullable=False, 
        default=0.00,
        doc="Quarter 3 balance"
    )
    ledger_q4 = Column(
        Numeric(10, 2), 
        nullable=False, 
        default=0.00,
        doc="Quarter 4 balance"
    )
    
    # Relationships
    debit_postings = relationship(
        "GLPostingRec", 
        foreign_keys="GLPostingRec.post_dr",
        back_populates="debit_account"
    )
    credit_postings = relationship(
        "GLPostingRec", 
        foreign_keys="GLPostingRec.post_cr",
        back_populates="credit_account"
    )
    budgets = relationship(
        "GLBudgetRec",
        back_populates="account"
    )
    
    # Table constraints
    __table_args__ = (
        CheckConstraint(
            'ledger_type >= 1 AND ledger_type <= 5', 
            name='ck_glledger_valid_type'
        ),
        CheckConstraint(
            "ledger_place IN ('B', 'P')", 
            name='ck_glledger_valid_placement'
        ),
        CheckConstraint(
            'ledger_level >= 1 AND ledger_level <= 9', 
            name='ck_glledger_valid_level'
        ),
        CheckConstraint(
            'ledger_key >= 10000000 AND ledger_key <= 99999999', 
            name='ck_glledger_valid_account_code'
        ),
        Index('ix_glledger_type', 'ledger_type'),
        Index('ix_glledger_name', 'ledger_name'),
        Index('ix_glledger_level', 'ledger_level'),
        {
            'comment': 'Chart of accounts with current and historical balances',
            'schema': 'acas'
        }
    )
    
    def __repr__(self):
        return f"<GLAccount(code={self.ledger_key}, name='{self.ledger_name}')>"
    
    @property
    def account_type_description(self) -> str:
        """Get human-readable account type"""
        types = {
            1: "Asset",
            2: "Liability", 
            3: "Capital/Equity",
            4: "Income/Revenue",
            5: "Expense/Cost"
        }
        return types.get(self.ledger_type, "Unknown")
    
    @property
    def is_balance_sheet_account(self) -> bool:
        """Check if this is a balance sheet account"""
        return self.ledger_place == 'B'
    
    @property
    def is_profit_loss_account(self) -> bool:
        """Check if this is a profit & loss account"""
        return self.ledger_place == 'P'
    
    @property
    def normal_balance_side(self) -> str:
        """Get normal balance side for this account type"""
        # Assets and Expenses have normal debit balance
        # Liabilities, Capital, and Income have normal credit balance
        return "DR" if self.ledger_type in [1, 5] else "CR"
    
    def get_quarterly_balance(self, quarter: int) -> Decimal:
        """Get balance for specific quarter (1-4)"""
        if quarter < 1 or quarter > 4:
            return Decimal('0.00')
        return Decimal(str(getattr(self, f'ledger_q{quarter}')))
    
    def update_balance(self, amount: Decimal, is_debit: bool) -> None:
        """Update account balance with posting"""
        current_balance = Decimal(str(self.ledger_balance))
        
        if is_debit:
            # Debit increases assets and expenses, decreases others
            if self.ledger_type in [1, 5]:  # Asset, Expense
                self.ledger_balance = float(current_balance + amount)
            else:  # Liability, Capital, Income
                self.ledger_balance = float(current_balance - amount)
        else:
            # Credit decreases assets and expenses, increases others
            if self.ledger_type in [1, 5]:  # Asset, Expense
                self.ledger_balance = float(current_balance - amount)
            else:  # Liability, Capital, Income
                self.ledger_balance = float(current_balance + amount)

class GLBatchRec(Base):
    """
    General Ledger Batch Control Record
    
    Represents batch control for GL postings with validation totals.
    Mirrors COBOL GLBATCH_REC structure.
    """
    __tablename__ = "glbatch_rec"
    
    # Primary Key - Batch Number
    batch_key = Column(
        Integer, 
        primary_key=True,
        doc="Batch control number"
    )
    
    # Batch Statistics
    items = Column(
        Integer, 
        nullable=False,
        doc="Number of items in batch"
    )
    batch_status = Column(
        Integer, 
        nullable=False,
        doc="Batch status: 0=Open, 1=Balanced, 2=Posted"
    )
    cleared_status = Column(
        Integer, 
        nullable=False,
        doc="Cleared status for reconciliation"
    )
    bcycle = Column(
        Integer, 
        nullable=False,
        doc="Batch cycle number"
    )
    
    # Date Information (YYYYMMDD format)
    entered = Column(
        Integer, 
        nullable=False,
        doc="Date entered"
    )
    proofed = Column(
        Integer, 
        nullable=False,
        doc="Date proofed/balanced"
    )
    posted = Column(
        Integer, 
        nullable=False,
        doc="Date posted"
    )
    stored = Column(
        Integer, 
        nullable=False,
        doc="Date stored/archived"
    )
    
    # Financial Control Totals
    input_gross = Column(
        Numeric(14, 2), 
        nullable=False,
        doc="Input gross total"
    )
    input_vat = Column(
        Numeric(14, 2), 
        nullable=False,
        doc="Input VAT total"
    )
    actual_gross = Column(
        Numeric(14, 2), 
        nullable=False,
        doc="Actual gross total"
    )
    actual_vat = Column(
        Numeric(14, 2), 
        nullable=False,
        doc="Actual VAT total"
    )
    
    # Batch Description and Configuration
    description = Column(
        String(24), 
        nullable=False,
        doc="Batch description"
    )
    bdefault = Column(
        Integer, 
        nullable=False,
        doc="Batch default settings"
    )
    convention = Column(
        String(2), 
        nullable=False,
        doc="Posting convention"
    )
    
    # Default Posting Information
    batch_def_ac = Column(
        Integer, 
        nullable=False,
        doc="Default account code"
    )
    batch_def_pc = Column(
        Integer, 
        nullable=False,
        doc="Default posting code"
    )
    batch_def_code = Column(
        String(2), 
        nullable=False,
        doc="Default transaction code"
    )
    batch_def_vat = Column(
        String(1), 
        nullable=False,
        doc="Default VAT code"
    )
    batch_start = Column(
        Integer, 
        nullable=False,
        doc="Starting sequence number"
    )
    
    # Relationships
    postings = relationship("GLPostingRec", back_populates="batch")
    
    # Table constraints
    __table_args__ = (
        CheckConstraint(
            'batch_status >= 0 AND batch_status <= 2', 
            name='ck_glbatch_valid_status'
        ),
        CheckConstraint(
            'items >= 0', 
            name='ck_glbatch_valid_item_count'
        ),
        CheckConstraint(
            'input_gross >= 0', 
            name='ck_glbatch_valid_input_gross'
        ),
        CheckConstraint(
            'actual_gross >= 0', 
            name='ck_glbatch_valid_actual_gross'
        ),
        Index('ix_glbatch_status', 'batch_status'),
        Index('ix_glbatch_entered', 'entered'),
        {
            'comment': 'General Ledger batch control with validation totals',
            'schema': 'acas'
        }
    )
    
    def __repr__(self):
        return f"<GLBatch(key={self.batch_key}, status={self.batch_status}, items={self.items})>"
    
    @property
    def is_balanced(self) -> bool:
        """Check if batch is balanced (input = actual)"""
        return (self.input_gross == self.actual_gross and 
                self.input_vat == self.actual_vat)
    
    @property
    def status_description(self) -> str:
        """Get human-readable status"""
        statuses = {
            0: "Open",
            1: "Balanced",
            2: "Posted"
        }
        return statuses.get(self.batch_status, "Unknown")
    
    @property
    def variance_gross(self) -> Decimal:
        """Calculate gross variance (input - actual)"""
        return Decimal(str(self.input_gross)) - Decimal(str(self.actual_gross))
    
    @property
    def variance_vat(self) -> Decimal:
        """Calculate VAT variance (input - actual)"""
        return Decimal(str(self.input_vat)) - Decimal(str(self.actual_vat))

class GLPostingRec(Base):
    """
    General Ledger Posting Record
    
    Individual GL posting transactions with full audit trail.
    Mirrors COBOL GLPOSTING_REC structure.
    """
    __tablename__ = "glposting_rec"
    
    # Primary Key - Auto-increment sequence
    post_rrn = Column(
        Integer, 
        primary_key=True,
        autoincrement=True,
        doc="Relative record number (sequence)"
    )
    
    # Posting Key and Reference
    post_key = Column(
        BigInteger, 
        nullable=False,
        doc="Posting key/reference number"
    )
    post_code = Column(
        String(2), 
        nullable=False,
        doc="Transaction/posting code"
    )
    post_dat = Column(
        String(8), 
        nullable=False,
        doc="Posting date (YYYYMMDD)"
    )
    
    # Debit Side Information
    post_dr = Column(
        Integer, 
        ForeignKey("acas.glledger_rec.ledger_key", ondelete="RESTRICT"),
        nullable=False,
        doc="Debit account code"
    )
    dr_pc = Column(
        Integer, 
        nullable=False,
        doc="Debit posting code"
    )
    
    # Credit Side Information
    post_cr = Column(
        Integer, 
        ForeignKey("acas.glledger_rec.ledger_key", ondelete="RESTRICT"),
        nullable=False,
        doc="Credit account code"
    )
    cr_pc = Column(
        Integer, 
        nullable=False,
        doc="Credit posting code"
    )
    
    # Financial Information
    post_amount = Column(
        Numeric(10, 2), 
        nullable=False,
        doc="Posting amount"
    )
    post_legend = Column(
        String(32), 
        nullable=False,
        doc="Posting description/legend"
    )
    
    # VAT Information
    vat_ac = Column(
        Integer, 
        nullable=False,
        doc="VAT account code"
    )
    vat_pc = Column(
        Integer, 
        nullable=False,
        doc="VAT posting code"
    )
    post_vat_side = Column(
        String(2), 
        nullable=False,
        doc="VAT posting side"
    )
    vat_amount = Column(
        Numeric(10, 2), 
        nullable=False,
        doc="VAT amount"
    )
    
    # Batch Reference
    batch_key = Column(
        Integer,
        ForeignKey("acas.glbatch_rec.batch_key", ondelete="SET NULL"),
        nullable=True,
        doc="Batch control number"
    )
    
    # Relationships
    debit_account = relationship(
        "GLLedgerRec", 
        foreign_keys=[post_dr],
        back_populates="debit_postings"
    )
    credit_account = relationship(
        "GLLedgerRec", 
        foreign_keys=[post_cr],
        back_populates="credit_postings"
    )
    batch = relationship("GLBatchRec", back_populates="postings")
    
    # Table constraints
    __table_args__ = (
        CheckConstraint(
            'post_amount > 0', 
            name='ck_glposting_valid_amount'
        ),
        CheckConstraint(
            'post_dr != post_cr', 
            name='ck_glposting_different_accounts'
        ),
        CheckConstraint(
            'LENGTH(post_dat) = 8', 
            name='ck_glposting_valid_date_format'
        ),
        Index('ix_glposting_dr', 'post_dr'),
        Index('ix_glposting_cr', 'post_cr'),
        Index('ix_glposting_date', 'post_dat'),
        Index('ix_glposting_key', 'post_key'),
        Index('ix_glposting_batch', 'batch_key'),
        {
            'comment': 'Individual GL posting transactions with full audit trail',
            'schema': 'acas'
        }
    )
    
    def __repr__(self):
        return f"<GLPosting(rrn={self.post_rrn}, dr={self.post_dr}, cr={self.post_cr}, amount={self.post_amount})>"
    
    @property
    def total_amount(self) -> Decimal:
        """Get total amount including VAT"""
        return Decimal(str(self.post_amount)) + Decimal(str(self.vat_amount))
    
    @property
    def posting_date_formatted(self) -> str:
        """Get formatted posting date (DD/MM/YYYY)"""
        if len(self.post_dat) == 8:
            year = self.post_dat[:4]
            month = self.post_dat[4:6]
            day = self.post_dat[6:8]
            return f"{day}/{month}/{year}"
        return self.post_dat
    
    def validate_double_entry(self) -> bool:
        """Validate double-entry principle (DR = CR)"""
        return self.post_dr != self.post_cr and self.post_amount > 0


class GLPeriodStatusRec(Base):
    """
    General Ledger Period Status Record
    
    Tracks the status of accounting periods (open, closed, archived).
    Used for period-end closing and financial reporting controls.
    """
    __tablename__ = "glperiod_status_rec"
    
    # Primary Key - Period identifier
    period_key = Column(
        Integer,
        primary_key=True,
        doc="Period identifier (YYYYMM format)"
    )
    
    # Period Information
    fiscal_year = Column(
        Integer,
        nullable=False,
        doc="Fiscal year"
    )
    period_number = Column(
        Integer,
        nullable=False,
        doc="Period number (1-13, 13 for adjustments)"
    )
    period_name = Column(
        String(20),
        nullable=False,
        doc="Period description"
    )
    
    # Date Information (YYYYMMDD format)
    start_date = Column(
        Integer,
        nullable=False,
        doc="Period start date"
    )
    end_date = Column(
        Integer,
        nullable=False,
        doc="Period end date"
    )
    closed_date = Column(
        Integer,
        nullable=False,
        default=0,
        doc="Date period was closed"
    )
    
    # Status Information
    status = Column(
        String(1),
        nullable=False,
        default='O',
        doc="Period status: O=Open, C=Closed, A=Archived"
    )
    is_adjustment_period = Column(
        Boolean,
        nullable=False,
        default=False,
        doc="Is this an adjustment period"
    )
    
    # User Information
    closed_by = Column(
        String(30),
        nullable=True,
        doc="User who closed the period"
    )
    
    # Control Totals
    total_debits = Column(
        Numeric(15, 2),
        nullable=False,
        default=0,
        doc="Total debits for period"
    )
    total_credits = Column(
        Numeric(15, 2),
        nullable=False,
        default=0,
        doc="Total credits for period"
    )
    transaction_count = Column(
        Integer,
        nullable=False,
        default=0,
        doc="Number of transactions in period"
    )
    
    # Table constraints
    __table_args__ = (
        CheckConstraint(
            "status IN ('O', 'C', 'A')",
            name='ck_period_valid_status'
        ),
        CheckConstraint(
            'period_number >= 1 AND period_number <= 13',
            name='ck_period_valid_number'
        ),
        CheckConstraint(
            'total_debits >= 0',
            name='ck_period_valid_debits'
        ),
        CheckConstraint(
            'total_credits >= 0',
            name='ck_period_valid_credits'
        ),
        Index('ix_period_fiscal_year', 'fiscal_year'),
        Index('ix_period_status', 'status'),
        Index('ix_period_dates', 'start_date', 'end_date'),
        {
            'comment': 'GL period status and control information',
            'schema': 'acas'
        }
    )
    
    def __repr__(self):
        return f"<GLPeriodStatus(key={self.period_key}, year={self.fiscal_year}, period={self.period_number}, status={self.status})>"
    
    @property
    def is_open(self) -> bool:
        """Check if period is open for posting"""
        return self.status == 'O'
    
    @property
    def is_closed(self) -> bool:
        """Check if period is closed"""
        return self.status in ['C', 'A']
    
    @property
    def is_balanced(self) -> bool:
        """Check if period debits equal credits"""
        return self.total_debits == self.total_credits
    
    @property
    def variance(self) -> Decimal:
        """Calculate variance between debits and credits"""
        return Decimal(str(self.total_debits)) - Decimal(str(self.total_credits))
    
    @property
    def status_description(self) -> str:
        """Get human-readable status"""
        statuses = {
            'O': 'Open',
            'C': 'Closed',
            'A': 'Archived'
        }
        return statuses.get(self.status, 'Unknown')


class GLBudgetRec(Base):
    """
    General Ledger Budget Record
    
    Stores budget amounts by account and period for variance analysis.
    Used for budget vs actual reporting and financial planning.
    """
    __tablename__ = "glbudget_rec"
    
    # Primary Key - Composite key
    budget_id = Column(
        Integer,
        primary_key=True,
        autoincrement=True,
        doc="Budget record identifier"
    )
    
    # Budget Identification
    budget_year = Column(
        Integer,
        nullable=False,
        doc="Budget fiscal year"
    )
    account_code = Column(
        Integer,
        ForeignKey("acas.glledger_rec.ledger_key", ondelete="CASCADE"),
        nullable=False,
        doc="Account code"
    )
    budget_version = Column(
        String(10),
        nullable=False,
        default='ORIGINAL',
        doc="Budget version (ORIGINAL, REVISED, etc.)"
    )
    
    # Monthly Budget Amounts
    budget_01 = Column(
        Numeric(12, 2),
        nullable=False,
        default=0,
        doc="January budget amount"
    )
    budget_02 = Column(
        Numeric(12, 2),
        nullable=False,
        default=0,
        doc="February budget amount"
    )
    budget_03 = Column(
        Numeric(12, 2),
        nullable=False,
        default=0,
        doc="March budget amount"
    )
    budget_04 = Column(
        Numeric(12, 2),
        nullable=False,
        default=0,
        doc="April budget amount"
    )
    budget_05 = Column(
        Numeric(12, 2),
        nullable=False,
        default=0,
        doc="May budget amount"
    )
    budget_06 = Column(
        Numeric(12, 2),
        nullable=False,
        default=0,
        doc="June budget amount"
    )
    budget_07 = Column(
        Numeric(12, 2),
        nullable=False,
        default=0,
        doc="July budget amount"
    )
    budget_08 = Column(
        Numeric(12, 2),
        nullable=False,
        default=0,
        doc="August budget amount"
    )
    budget_09 = Column(
        Numeric(12, 2),
        nullable=False,
        default=0,
        doc="September budget amount"
    )
    budget_10 = Column(
        Numeric(12, 2),
        nullable=False,
        default=0,
        doc="October budget amount"
    )
    budget_11 = Column(
        Numeric(12, 2),
        nullable=False,
        default=0,
        doc="November budget amount"
    )
    budget_12 = Column(
        Numeric(12, 2),
        nullable=False,
        default=0,
        doc="December budget amount"
    )
    budget_13 = Column(
        Numeric(12, 2),
        nullable=False,
        default=0,
        doc="Adjustment period budget amount"
    )
    
    # Status and Control
    status = Column(
        String(1),
        nullable=False,
        default='A',
        doc="Budget status: A=Active, I=Inactive, L=Locked"
    )
    created_by = Column(
        String(30),
        nullable=False,
        doc="User who created budget"
    )
    created_date = Column(
        Integer,
        nullable=False,
        doc="Date budget was created (YYYYMMDD)"
    )
    approved_by = Column(
        String(30),
        nullable=True,
        doc="User who approved budget"
    )
    approved_date = Column(
        Integer,
        nullable=False,
        default=0,
        doc="Date budget was approved (YYYYMMDD)"
    )
    
    # Comments and Notes
    comments = Column(
        String(100),
        nullable=True,
        doc="Budget comments or notes"
    )
    
    # Relationships
    account = relationship(
        "GLLedgerRec",
        back_populates="budgets"
    )
    
    # Table constraints
    __table_args__ = (
        CheckConstraint(
            "status IN ('A', 'I', 'L')",
            name='ck_budget_valid_status'
        ),
        CheckConstraint(
            'budget_year >= 2000 AND budget_year <= 2099',
            name='ck_budget_valid_year'
        ),
        Index('ix_budget_year_account', 'budget_year', 'account_code'),
        Index('ix_budget_status', 'status'),
        Index('ix_budget_version', 'budget_version'),
        {
            'comment': 'GL budget amounts by account and period',
            'schema': 'acas'
        }
    )
    
    def __repr__(self):
        return f"<GLBudget(id={self.budget_id}, year={self.budget_year}, account={self.account_code}, version={self.budget_version})>"
    
    @property
    def annual_total(self) -> Decimal:
        """Calculate total annual budget amount"""
        total = Decimal('0.00')
        for period in range(1, 14):  # Include adjustment period
            amount = getattr(self, f'budget_{period:02d}', 0)
            total += Decimal(str(amount))
        return total
    
    @property
    def quarterly_totals(self) -> dict:
        """Get quarterly budget totals"""
        return {
            'Q1': self.get_quarterly_total(1),
            'Q2': self.get_quarterly_total(2),
            'Q3': self.get_quarterly_total(3),
            'Q4': self.get_quarterly_total(4)
        }
    
    def get_period_amount(self, period: int) -> Decimal:
        """Get budget amount for specific period (1-13)"""
        if period < 1 or period > 13:
            return Decimal('0.00')
        amount = getattr(self, f'budget_{period:02d}', 0)
        return Decimal(str(amount))
    
    def get_quarterly_total(self, quarter: int) -> Decimal:
        """Get total budget for specific quarter (1-4)"""
        if quarter < 1 or quarter > 4:
            return Decimal('0.00')
        
        start_period = (quarter - 1) * 3 + 1
        end_period = quarter * 3
        
        total = Decimal('0.00')
        for period in range(start_period, end_period + 1):
            total += self.get_period_amount(period)
        
        return total
    
    def set_period_amount(self, period: int, amount: Decimal) -> None:
        """Set budget amount for specific period"""
        if 1 <= period <= 13:
            setattr(self, f'budget_{period:02d}', float(amount))
    
    @property
    def is_active(self) -> bool:
        """Check if budget is active"""
        return self.status == 'A'
    
    @property
    def is_approved(self) -> bool:
        """Check if budget is approved"""
        return self.approved_date > 0