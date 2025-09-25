"""
ACAS General Ledger Schemas
Simplified schemas for GL accounts and postings
"""
from pydantic import BaseModel, Field, validator
from typing import Optional
from decimal import Decimal
from datetime import datetime

class GLAccountBase(BaseModel):
    """Base GL account schema"""
    ledger_name: str = Field(..., max_length=32, description="Account name")
    ledger_type: int = Field(..., ge=1, le=5, description="1=Asset, 2=Liability, 3=Capital, 4=Income, 5=Expense")
    ledger_place: str = Field(..., max_length=1, description="B=Balance Sheet, P=P&L")
    ledger_level: int = Field(..., ge=1, le=9, description="Account level")

class GLAccountCreate(GLAccountBase):
    """GL account creation schema"""
    ledger_key: int = Field(..., ge=10000000, le=99999999, description="8-digit account code")

class GLAccountUpdate(BaseModel):
    """GL account update schema"""
    ledger_name: Optional[str] = Field(None, max_length=32)
    ledger_level: Optional[int] = Field(None, ge=1, le=9)

class GLAccountResponse(GLAccountBase):
    """GL account response schema"""
    ledger_key: int
    ledger_balance: Decimal = Field(..., decimal_places=2, description="Current balance")
    ledger_last: Decimal = Field(..., decimal_places=2, description="Previous period balance")
    account_type_description: str
    is_balance_sheet_account: bool
    is_profit_loss_account: bool
    normal_balance_side: str = Field(..., description="DR or CR")
    
    class Config:
        orm_mode = True

class GLPostingBase(BaseModel):
    """Base GL posting schema"""
    post_dat: str = Field(..., min_length=8, max_length=8, description="Posting date YYYYMMDD")
    post_amount: Decimal = Field(..., gt=0, decimal_places=2, description="Posting amount")
    post_legend: str = Field(..., max_length=32, description="Posting description")
    post_code: str = Field("JE", max_length=2, description="Posting code")
    vat_amount: Decimal = Field(Decimal('0.00'), ge=0, decimal_places=2, description="VAT amount")

class GLPostingCreate(GLPostingBase):
    """GL posting creation schema"""
    post_dr: int = Field(..., description="Debit account code")
    post_cr: int = Field(..., description="Credit account code")
    
    @validator('post_cr')
    def validate_different_accounts(cls, v, values):
        """Ensure debit and credit accounts are different"""
        if 'post_dr' in values and v == values['post_dr']:
            raise ValueError('Debit and credit accounts must be different')
        return v

class GLPostingResponse(GLPostingBase):
    """GL posting response schema"""
    post_rrn: int
    post_key: int
    post_dr: int
    post_cr: int
    dr_pc: int
    cr_pc: int
    vat_ac: int
    vat_pc: int
    post_vat_side: str
    total_amount: Decimal = Field(..., decimal_places=2, description="Amount including VAT")
    posting_date_formatted: str = Field(..., description="Formatted date DD/MM/YYYY")
    
    class Config:
        orm_mode = True