"""General Ledger Comprehensive Schemas"""

from pydantic import BaseModel, Field, field_validator, ConfigDict
from typing import Optional, List, Dict, Any
from datetime import datetime, date
from decimal import Decimal
from enum import Enum


# Enums
class AccountType(str, Enum):
    ASSET = "asset"
    LIABILITY = "liability"
    EQUITY = "equity"
    INCOME = "income"
    EXPENSE = "expense"


class JournalStatus(str, Enum):
    DRAFT = "draft"
    POSTED = "posted"
    VOID = "void"


class PostingStatus(str, Enum):
    PENDING = "pending"
    POSTED = "posted"
    REVERSED = "reversed"


# Base GL Account Schemas
class GLAccountBase(BaseModel):
    account_number: str = Field(..., min_length=4, max_length=20)
    account_name: str = Field(..., min_length=3, max_length=100)
    account_type: AccountType
    normal_balance: str = Field(..., pattern="^(debit|credit)$")
    parent_account_number: Optional[str] = None
    currency_code: str = Field(default="USD", max_length=3)
    is_active: bool = True
    allow_manual_entry: bool = True
    require_department: bool = False
    require_project: bool = False


class GLAccountCreate(GLAccountBase):
    opening_balance: Optional[Decimal] = Field(default=0)
    opening_balance_date: Optional[date] = None


class GLAccountUpdate(BaseModel):
    account_name: Optional[str] = None
    parent_account_number: Optional[str] = None
    is_active: Optional[bool] = None
    allow_manual_entry: Optional[bool] = None
    require_department: Optional[bool] = None
    require_project: Optional[bool] = None


class GLAccount(GLAccountBase):
    id: int
    current_balance: Decimal = Field(...)
    ytd_balance: Decimal = Field(...)
    budget_amount: Optional[Decimal] = Field(None)
    created_at: datetime
    updated_at: Optional[datetime] = None
    
    model_config = ConfigDict(from_attributes=True)


class AccountBalance(BaseModel):
    account_number: str
    account_name: str
    debit_balance: Decimal = Field(...)
    credit_balance: Decimal = Field(...)
    net_balance: Decimal = Field(...)
    period_activity: Decimal = Field(...)
    ytd_activity: Decimal = Field(...)
    as_of_date: date
    includes_children: bool = False


class AccountSummary(BaseModel):
    account: GLAccount
    current_period_activity: Dict[str, Decimal]
    ytd_activity: Dict[str, Decimal]
    budget_vs_actual: Optional[Dict[str, Any]] = None
    child_accounts: List[GLAccount] = []
    recent_transactions: List[Dict[str, Any]] = []


class ChartOfAccountsTree(BaseModel):
    accounts: List[Dict[str, Any]]
    total_accounts: int
    asset_accounts: int
    liability_accounts: int
    equity_accounts: int
    income_accounts: int
    expense_accounts: int
    as_of_date: Optional[date] = None


# Journal Entry Schemas
class JournalLineBase(BaseModel):
    account_number: str
    description: str = Field(..., max_length=200)
    debit_amount: Optional[Decimal] = Field(None, ge=0)
    credit_amount: Optional[Decimal] = Field(None, ge=0)
    department_code: Optional[str] = None
    project_code: Optional[str] = None
    reference: Optional[str] = None
    
    @field_validator('credit_amount')
    @classmethod
    def validate_debit_or_credit(cls, v, values):
        if v is None and values.get('debit_amount') is None:
            raise ValueError('Either debit_amount or credit_amount must be provided')
        if v is not None and values.get('debit_amount') is not None:
            if v > 0 and values['debit_amount'] > 0:
                raise ValueError('Cannot have both debit and credit amounts')
        return v


class JournalLine(JournalLineBase):
    id: int
    line_number: int
    posting_id: Optional[int] = None
    
    model_config = ConfigDict(from_attributes=True)


class JournalEntryBase(BaseModel):
    entry_date: date
    description: str = Field(..., max_length=500)
    reference_number: Optional[str] = None
    journal_type: str = Field(default="GJ")
    source_document: Optional[str] = None
    lines: List[JournalLineBase]


class JournalEntryCreate(JournalEntryBase):
    pass


class JournalEntryUpdate(BaseModel):
    entry_date: Optional[date] = None
    description: Optional[str] = None
    reference_number: Optional[str] = None
    lines: Optional[List[JournalLineBase]] = None


class JournalEntry(JournalEntryBase):
    id: int
    entry_number: str
    status: JournalStatus
    lines: List[JournalLine]
    total_debits: Decimal
    total_credits: Decimal
    created_by: int
    created_at: datetime
    posted_by: Optional[int] = None
    posted_at: Optional[datetime] = None
    
    model_config = ConfigDict(from_attributes=True)


class JournalEntryPost(BaseModel):
    posting_date: Optional[date] = None
    post_to_period: Optional[int] = None


class JournalEntryValidation(BaseModel):
    is_valid: bool
    is_balanced: bool
    errors: List[str] = []
    warnings: List[str] = []
    total_debits: Decimal
    total_credits: Decimal


# Recurring Journal Schemas
class RecurringJournalBase(BaseModel):
    template_name: str
    frequency: str = Field(..., pattern="^(daily|weekly|monthly|quarterly|annually)$")
    next_date: date
    end_date: Optional[date] = None
    occurrences_remaining: Optional[int] = None
    journal_template: JournalEntryBase
    is_active: bool = True


class RecurringJournalCreate(RecurringJournalBase):
    pass


class RecurringJournal(RecurringJournalBase):
    id: int
    last_generated_date: Optional[date] = None
    total_generated: int = 0
    created_at: datetime
    
    model_config = ConfigDict(from_attributes=True)


# GL Posting Schemas
class GLPostingBase(BaseModel):
    posting_date: date
    account_number: str
    debit_amount: Optional[Decimal] = Field(None, ge=0)
    credit_amount: Optional[Decimal] = Field(None, ge=0)
    description: str = Field(..., max_length=200)
    reference: Optional[str] = None
    source_module: str = Field(..., max_length=10)
    source_id: int


class GLPostingCreate(GLPostingBase):
    pass


class GLPosting(GLPostingBase):
    id: int
    posting_number: str
    period_id: int
    status: PostingStatus
    journal_entry_id: Optional[int] = None
    created_at: datetime
    
    model_config = ConfigDict(from_attributes=True)


# Period Management Schemas
class AccountingPeriodBase(BaseModel):
    period_number: int = Field(..., ge=1, le=13)
    fiscal_year: int
    start_date: date
    end_date: date
    period_name: str
    is_open: bool = True
    is_adjustment_period: bool = False


class AccountingPeriodCreate(AccountingPeriodBase):
    pass


class AccountingPeriod(AccountingPeriodBase):
    id: int
    closed_date: Optional[datetime] = None
    closed_by: Optional[int] = None
    
    model_config = ConfigDict(from_attributes=True)


# Budget Schemas
class BudgetLineBase(BaseModel):
    account_number: str
    period_1: Decimal = Field(default=0)
    period_2: Decimal = Field(default=0)
    period_3: Decimal = Field(default=0)
    period_4: Decimal = Field(default=0)
    period_5: Decimal = Field(default=0)
    period_6: Decimal = Field(default=0)
    period_7: Decimal = Field(default=0)
    period_8: Decimal = Field(default=0)
    period_9: Decimal = Field(default=0)
    period_10: Decimal = Field(default=0)
    period_11: Decimal = Field(default=0)
    period_12: Decimal = Field(default=0)
    period_13: Optional[Decimal] = Field(None)


class BudgetBase(BaseModel):
    budget_name: str
    fiscal_year: int
    budget_type: str = Field(..., pattern="^(annual|quarterly|monthly)$")
    is_active: bool = True
    lines: List[BudgetLineBase]


class BudgetCreate(BudgetBase):
    pass


class Budget(BudgetBase):
    id: int
    total_amount: Decimal
    created_at: datetime
    approved_at: Optional[datetime] = None
    approved_by: Optional[int] = None
    
    model_config = ConfigDict(from_attributes=True)


# Financial Statement Schemas
class TrialBalanceRow(BaseModel):
    account_number: str
    account_name: str
    account_type: AccountType
    beginning_debit: Decimal = Field(...)
    beginning_credit: Decimal = Field(...)
    period_debit: Decimal = Field(...)
    period_credit: Decimal = Field(...)
    ending_debit: Decimal = Field(...)
    ending_credit: Decimal = Field(...)


class TrialBalance(BaseModel):
    period_id: int
    as_of_date: date
    rows: List[TrialBalanceRow]
    total_beginning_debit: Decimal
    total_beginning_credit: Decimal
    total_period_debit: Decimal
    total_period_credit: Decimal
    total_ending_debit: Decimal
    total_ending_credit: Decimal
    is_balanced: bool


class FinancialStatementLine(BaseModel):
    line_type: str  # header, detail, subtotal, total
    description: str
    amount: Optional[Decimal] = None
    percentage: Optional[float] = None
    indent_level: int = 0
    is_bold: bool = False


class IncomeStatement(BaseModel):
    period_id: int
    period_name: str
    start_date: date
    end_date: date
    lines: List[FinancialStatementLine]
    total_revenue: Decimal
    total_expenses: Decimal
    net_income: Decimal


class BalanceSheet(BaseModel):
    period_id: int
    as_of_date: date
    assets: List[FinancialStatementLine]
    liabilities: List[FinancialStatementLine]
    equity: List[FinancialStatementLine]
    total_assets: Decimal
    total_liabilities: Decimal
    total_equity: Decimal
    is_balanced: bool


# Response Models for Lists
class GLAccountListResponse(BaseModel):
    accounts: List[GLAccount]
    total: int
    skip: int
    limit: int


class JournalEntryListResponse(BaseModel):
    entries: List[JournalEntry]
    total: int
    skip: int
    limit: int


class JournalLineCreate(JournalLineBase):
    """Alias for backward compatibility"""
    pass


class JournalEntryResponse(JournalEntry):
    """Alias for backward compatibility"""
    pass


class GLAccountResponse(GLAccount):
    """Alias for backward compatibility"""
    pass