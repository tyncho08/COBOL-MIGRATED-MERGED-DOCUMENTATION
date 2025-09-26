"""
General Ledger Models
Maps to GL accounts, journal entries, postings, and budget tables
"""
from sqlalchemy import Column, String, Integer, Text, DECIMAL, Boolean, DateTime, Date, ForeignKey, TIMESTAMP
from sqlalchemy.orm import relationship
from sqlalchemy.dialects.postgresql import UUID
from app.core.database import Base
from datetime import datetime
import uuid


class GLAccount(Base):
    """Chart of Accounts"""
    __tablename__ = "accounts"
    __table_args__ = {"schema": "acas"}
    
    account_id = Column(Integer, primary_key=True)
    account_number = Column(String(20), unique=True, nullable=False, index=True)
    account_name = Column(String(100), nullable=False)
    account_type = Column(String(20), nullable=False)  # ASSET, LIABILITY, EQUITY, INCOME, EXPENSE
    normal_balance = Column(String(10), nullable=False)  # DEBIT, CREDIT
    parent_account_id = Column(Integer, ForeignKey("acas.accounts.account_id"))
    
    # Balances
    current_balance = Column(DECIMAL(15, 2), default=0)
    ytd_balance = Column(DECIMAL(15, 2), default=0)
    budget_amount = Column(DECIMAL(15, 2), default=0)
    
    # Configuration
    currency_code = Column(String(3), default="USD")
    is_active = Column(Boolean, default=True)
    allow_manual_entry = Column(Boolean, default=True)
    require_department = Column(Boolean, default=False)
    require_project = Column(Boolean, default=False)
    
    # Timestamps
    created_at = Column(TIMESTAMP(timezone=True), default=datetime.utcnow)
    updated_at = Column(TIMESTAMP(timezone=True), default=datetime.utcnow, onupdate=datetime.utcnow)
    
    # Relationships
    parent_account = relationship("GLAccount", remote_side=[account_id])
    child_accounts = relationship("GLAccount", back_populates="parent_account")
    journal_lines = relationship("JournalLine", back_populates="account")
    postings = relationship("GLPosting", back_populates="account")


class JournalEntry(Base):
    """Journal Entries Header"""
    __tablename__ = "journal_entries"
    __table_args__ = {"schema": "acas"}
    
    entry_id = Column(Integer, primary_key=True)
    entry_number = Column(String(20), unique=True, nullable=False)
    entry_date = Column(Date, nullable=False)
    description = Column(String(500), nullable=False)
    reference_number = Column(String(50))
    journal_type = Column(String(10), default="GJ")  # GJ=General, AP=Accounts Payable, etc.
    source_document = Column(String(100))
    
    # Status
    status = Column(String(20), default="DRAFT")  # DRAFT, POSTED, VOID
    total_debits = Column(DECIMAL(15, 2), default=0)
    total_credits = Column(DECIMAL(15, 2), default=0)
    
    # Posting information
    posted_at = Column(TIMESTAMP(timezone=True))
    posted_by = Column(String(30))
    period_id = Column(Integer, ForeignKey("acas.accounting_periods.period_id"))
    
    # Audit
    created_at = Column(TIMESTAMP(timezone=True), default=datetime.utcnow)
    created_by = Column(String(30), nullable=False)
    
    # Relationships
    lines = relationship("JournalLine", back_populates="journal_entry", cascade="all, delete-orphan")
    period = relationship("AccountingPeriod", back_populates="journal_entries")


class JournalLine(Base):
    """Journal Entry Lines"""
    __tablename__ = "journal_lines"
    __table_args__ = {"schema": "acas"}
    
    line_id = Column(Integer, primary_key=True)
    entry_id = Column(Integer, ForeignKey("acas.journal_entries.entry_id"), nullable=False)
    line_number = Column(Integer, nullable=False)
    account_id = Column(Integer, ForeignKey("acas.accounts.account_id"), nullable=False)
    
    description = Column(String(200), nullable=False)
    debit_amount = Column(DECIMAL(15, 2), default=0)
    credit_amount = Column(DECIMAL(15, 2), default=0)
    
    # Additional dimensions
    department_code = Column(String(20))
    project_code = Column(String(20))
    reference = Column(String(50))
    
    # Posting reference
    posting_id = Column(Integer, ForeignKey("acas.gl_postings.posting_id"))
    
    # Relationships
    journal_entry = relationship("JournalEntry", back_populates="lines")
    account = relationship("GLAccount", back_populates="journal_lines")
    posting = relationship("GLPosting", back_populates="journal_line")


class GLPosting(Base):
    """Posted GL Transactions"""
    __tablename__ = "gl_postings"
    __table_args__ = {"schema": "acas"}
    
    posting_id = Column(Integer, primary_key=True)
    posting_number = Column(String(20), unique=True, nullable=False)
    posting_date = Column(Date, nullable=False)
    account_id = Column(Integer, ForeignKey("acas.accounts.account_id"), nullable=False)
    
    debit_amount = Column(DECIMAL(15, 2), default=0)
    credit_amount = Column(DECIMAL(15, 2), default=0)
    description = Column(String(200), nullable=False)
    reference = Column(String(50))
    
    # Source tracking
    source_module = Column(String(10), nullable=False)  # GL, AP, AR, etc.
    source_id = Column(Integer, nullable=False)
    
    # Period and status
    period_id = Column(Integer, ForeignKey("acas.accounting_periods.period_id"))
    status = Column(String(20), default="POSTED")  # POSTED, REVERSED
    
    # Timestamps
    created_at = Column(TIMESTAMP(timezone=True), default=datetime.utcnow)
    
    # Relationships
    account = relationship("GLAccount", back_populates="postings")
    period = relationship("AccountingPeriod", back_populates="postings")
    journal_line = relationship("JournalLine", back_populates="posting")


class AccountingPeriod(Base):
    """Accounting Periods for Financial Reporting"""
    __tablename__ = "accounting_periods"
    __table_args__ = {"schema": "acas"}
    
    period_id = Column(Integer, primary_key=True)
    period_number = Column(Integer, nullable=False)  # 1-13 (13 for adjustments)
    fiscal_year = Column(Integer, nullable=False)
    start_date = Column(Date, nullable=False)
    end_date = Column(Date, nullable=False)
    period_name = Column(String(50), nullable=False)
    
    # Status
    is_open = Column(Boolean, default=True)
    is_adjustment_period = Column(Boolean, default=False)
    closed_date = Column(TIMESTAMP(timezone=True))
    closed_by = Column(String(30))
    
    # Relationships
    journal_entries = relationship("JournalEntry", back_populates="period")
    postings = relationship("GLPosting", back_populates="period")
    budgets = relationship("Budget", back_populates="period")


class Budget(Base):
    """Budget Management"""
    __tablename__ = "budgets"
    __table_args__ = {"schema": "acas"}
    
    budget_id = Column(Integer, primary_key=True)
    budget_name = Column(String(100), nullable=False)
    fiscal_year = Column(Integer, nullable=False)
    budget_type = Column(String(20), nullable=False)  # ANNUAL, QUARTERLY, MONTHLY
    
    # Status
    is_active = Column(Boolean, default=True)
    total_amount = Column(DECIMAL(15, 2), default=0)
    
    # Approval
    approved_at = Column(TIMESTAMP(timezone=True))
    approved_by = Column(String(30))
    
    # Timestamps
    created_at = Column(TIMESTAMP(timezone=True), default=datetime.utcnow)
    
    # Relationships
    period_id = Column(Integer, ForeignKey("acas.accounting_periods.period_id"))
    period = relationship("AccountingPeriod", back_populates="budgets")
    lines = relationship("BudgetLine", back_populates="budget", cascade="all, delete-orphan")


class BudgetLine(Base):
    """Budget Line Items"""
    __tablename__ = "budget_lines"
    __table_args__ = {"schema": "acas"}
    
    line_id = Column(Integer, primary_key=True)
    budget_id = Column(Integer, ForeignKey("acas.budgets.budget_id"), nullable=False)
    account_id = Column(Integer, ForeignKey("acas.accounts.account_id"), nullable=False)
    
    # Monthly amounts (periods 1-13)
    period_1 = Column(DECIMAL(15, 2), default=0)
    period_2 = Column(DECIMAL(15, 2), default=0)
    period_3 = Column(DECIMAL(15, 2), default=0)
    period_4 = Column(DECIMAL(15, 2), default=0)
    period_5 = Column(DECIMAL(15, 2), default=0)
    period_6 = Column(DECIMAL(15, 2), default=0)
    period_7 = Column(DECIMAL(15, 2), default=0)
    period_8 = Column(DECIMAL(15, 2), default=0)
    period_9 = Column(DECIMAL(15, 2), default=0)
    period_10 = Column(DECIMAL(15, 2), default=0)
    period_11 = Column(DECIMAL(15, 2), default=0)
    period_12 = Column(DECIMAL(15, 2), default=0)
    period_13 = Column(DECIMAL(15, 2), default=0)  # Adjustments
    
    # Relationships
    budget = relationship("Budget", back_populates="lines")
    account = relationship("GLAccount")


class RecurringJournal(Base):
    """Recurring Journal Entry Templates"""
    __tablename__ = "recurring_journals"
    __table_args__ = {"schema": "acas"}
    
    template_id = Column(Integer, primary_key=True)
    template_name = Column(String(100), nullable=False)
    frequency = Column(String(20), nullable=False)  # DAILY, WEEKLY, MONTHLY, etc.
    
    # Schedule
    next_date = Column(Date, nullable=False)
    end_date = Column(Date)
    occurrences_remaining = Column(Integer)
    last_generated_date = Column(Date)
    total_generated = Column(Integer, default=0)
    
    # Template data (JSON)
    journal_template = Column(Text, nullable=False)  # JSON string
    
    # Status
    is_active = Column(Boolean, default=True)
    
    # Timestamps
    created_at = Column(TIMESTAMP(timezone=True), default=datetime.utcnow)