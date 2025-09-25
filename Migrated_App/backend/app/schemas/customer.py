"""
ACAS Customer (Sales Ledger) Schemas
Pydantic models for customer management and sales processing API
"""
from pydantic import BaseModel, Field, validator, EmailStr
from typing import Optional, List
from decimal import Decimal
from datetime import datetime
from .common import AuditInfo

class CustomerBase(BaseModel):
    """
    Base customer schema with core fields
    """
    # Identity Information
    sales_name: str = Field(..., max_length=30, description="Customer name")
    sales_address_1: str = Field("", max_length=30, description="Address line 1")
    sales_address_2: str = Field("", max_length=30, description="Address line 2")
    sales_address_3: str = Field("", max_length=30, description="Address line 3")
    sales_address_4: str = Field("", max_length=30, description="Address line 4")
    sales_address_5: str = Field("", max_length=30, description="Address line 5 (postcode)")
    
    # Contact Information
    sales_contact: str = Field("", max_length=25, description="Primary contact person")
    sales_phone: str = Field("", max_length=20, description="Phone number")
    sales_email: str = Field("", max_length=40, description="Email address")
    sales_fax: str = Field("", max_length=20, description="Fax number")
    
    # Financial Terms
    sales_credit_limit: Decimal = Field(
        Decimal('0.00'), 
        ge=0, 
        decimal_places=2, 
        description="Credit limit"
    )
    sales_discount_rate: Decimal = Field(
        Decimal('0.00'), 
        ge=0, 
        le=100, 
        decimal_places=2, 
        description="Discount rate percentage"
    )
    sales_payment_terms: str = Field("", max_length=10, description="Payment terms code")
    sales_tax_code: str = Field("", max_length=6, description="Tax/VAT code")
    
    # Account Configuration
    sales_account_status: str = Field(
        "A", 
        max_length=1, 
        description="Account status: A=Active, H=Hold, C=Closed"
    )
    sales_hold_flag: bool = Field(False, description="Account on hold")
    sales_credit_rating: str = Field(
        "B", 
        max_length=1, 
        description="Credit rating: A=Excellent, B=Good, C=Fair, D=Poor"
    )
    
    @validator('sales_account_status')
    def validate_account_status(cls, v):
        """Validate account status"""
        if v not in ['A', 'H', 'C']:
            raise ValueError('Account status must be A, H, or C')
        return v
    
    @validator('sales_credit_rating')
    def validate_credit_rating(cls, v):
        """Validate credit rating"""
        if v not in ['A', 'B', 'C', 'D']:
            raise ValueError('Credit rating must be A, B, C, or D')
        return v
    
    @validator('sales_email')
    def validate_email_format(cls, v):
        """Validate email format if provided"""
        if v and '@' not in v:
            raise ValueError('Invalid email format')
        return v
    
    class Config:
        schema_extra = {
            "example": {
                "sales_name": "ACME Corporation Ltd",
                "sales_address_1": "456 Customer Avenue",
                "sales_address_2": "Commercial District",
                "sales_address_3": "Customer City",
                "sales_address_4": "County",
                "sales_address_5": "CM1 2CD",
                "sales_contact": "John Customer",
                "sales_phone": "01234 567890",
                "sales_email": "accounts@acmecorp.com",
                "sales_fax": "01234 567891",
                "sales_credit_limit": "5000.00",
                "sales_discount_rate": "2.50",
                "sales_payment_terms": "30",
                "sales_tax_code": "VSTD",
                "sales_account_status": "A",
                "sales_hold_flag": False,
                "sales_credit_rating": "B"
            }
        }

class CustomerCreate(CustomerBase):
    """
    Customer creation schema
    """
    sales_key: str = Field(
        ..., 
        min_length=1, 
        max_length=7, 
        description="Customer code (up to 7 characters)"
    )
    
    @validator('sales_key')
    def validate_customer_code(cls, v):
        """Validate customer code format"""
        if not v.replace('-', '').replace('_', '').isalnum():
            raise ValueError('Customer code must contain only alphanumeric characters, hyphens, and underscores')
        return v.upper()

class CustomerUpdate(BaseModel):
    """
    Customer update schema
    
    All fields optional for partial updates
    """
    # Identity Information
    sales_name: Optional[str] = Field(None, max_length=30)
    sales_address_1: Optional[str] = Field(None, max_length=30)
    sales_address_2: Optional[str] = Field(None, max_length=30)
    sales_address_3: Optional[str] = Field(None, max_length=30)
    sales_address_4: Optional[str] = Field(None, max_length=30)
    sales_address_5: Optional[str] = Field(None, max_length=30)
    
    # Contact Information
    sales_contact: Optional[str] = Field(None, max_length=25)
    sales_phone: Optional[str] = Field(None, max_length=20)
    sales_email: Optional[str] = Field(None, max_length=40)
    sales_fax: Optional[str] = Field(None, max_length=20)
    
    # Financial Terms
    sales_credit_limit: Optional[Decimal] = Field(None, ge=0, decimal_places=2)
    sales_discount_rate: Optional[Decimal] = Field(None, ge=0, le=100, decimal_places=2)
    sales_payment_terms: Optional[str] = Field(None, max_length=10)
    sales_tax_code: Optional[str] = Field(None, max_length=6)
    
    # Account Configuration
    sales_account_status: Optional[str] = Field(None, max_length=1)
    sales_hold_flag: Optional[bool] = None
    sales_credit_rating: Optional[str] = Field(None, max_length=1)

class CustomerResponse(CustomerBase):
    """
    Customer response schema
    
    Includes read-only fields and financial summary
    """
    sales_key: str = Field(..., description="Customer code")
    
    # Current Financial Position
    sales_balance: Decimal = Field(..., decimal_places=2, description="Current balance")
    sales_ytd_turnover: Decimal = Field(..., decimal_places=2, description="Year-to-date turnover")
    sales_last_invoice_date: Optional[int] = Field(None, description="Last invoice date (YYYYMMDD)")
    sales_last_payment_date: Optional[int] = Field(None, description="Last payment date (YYYYMMDD)")
    
    # Calculated Fields
    available_credit: Decimal = Field(..., decimal_places=2, description="Available credit")
    is_active: bool = Field(..., description="Account is active")
    is_over_limit: bool = Field(..., description="Account is over credit limit")
    
    # Audit Information
    created_at: datetime = Field(..., description="Creation timestamp")
    updated_at: Optional[datetime] = Field(None, description="Last update timestamp")
    
    class Config:
        orm_mode = True
        schema_extra = {
            "example": {
                "sales_key": "CUST001",
                "sales_name": "ACME Corporation Ltd",
                "sales_address_1": "456 Customer Avenue",
                "sales_address_2": "Commercial District",
                "sales_address_3": "Customer City",
                "sales_address_4": "County",
                "sales_address_5": "CM1 2CD",
                "sales_contact": "John Customer",
                "sales_phone": "01234 567890",
                "sales_email": "accounts@acmecorp.com",
                "sales_credit_limit": "5000.00",
                "sales_discount_rate": "2.50",
                "sales_payment_terms": "30",
                "sales_tax_code": "VSTD",
                "sales_account_status": "A",
                "sales_hold_flag": False,
                "sales_credit_rating": "B",
                "sales_balance": "1250.00",
                "sales_ytd_turnover": "15000.00",
                "sales_last_invoice_date": 20241201,
                "sales_last_payment_date": 20241120,
                "available_credit": "3750.00",
                "is_active": True,
                "is_over_limit": False,
                "created_at": "2024-01-15T09:00:00Z",
                "updated_at": "2024-12-01T14:30:00Z"
            }
        }

class CustomerSummary(BaseModel):
    """
    Customer summary for lists and lookups
    """
    sales_key: str = Field(..., description="Customer code")
    sales_name: str = Field(..., description="Customer name")
    sales_balance: Decimal = Field(..., decimal_places=2, description="Current balance")
    sales_credit_limit: Decimal = Field(..., decimal_places=2, description="Credit limit")
    sales_account_status: str = Field(..., description="Account status")
    is_active: bool = Field(..., description="Account is active")
    
    class Config:
        orm_mode = True

# Sales Invoice Schemas
class SalesInvoiceLineBase(BaseModel):
    """
    Base sales invoice line schema
    """
    stock_key: str = Field("", max_length=13, description="Stock item code")
    item_description: str = Field(..., max_length=40, description="Item description")
    quantity: Decimal = Field(..., ge=0, decimal_places=3, description="Quantity")
    unit_price: Decimal = Field(..., ge=0, decimal_places=4, description="Unit price")
    line_discount_percent: Decimal = Field(
        Decimal('0.00'), 
        ge=0, 
        le=100, 
        decimal_places=2, 
        description="Line discount percentage"
    )
    line_tax_code: str = Field("", max_length=6, description="Tax code")
    
    class Config:
        schema_extra = {
            "example": {
                "stock_key": "ITEM001",
                "item_description": "Sample Product Item",
                "quantity": "10.000",
                "unit_price": "15.5000",
                "line_discount_percent": "0.00",
                "line_tax_code": "VSTD"
            }
        }

class SalesInvoiceLineCreate(SalesInvoiceLineBase):
    """
    Sales invoice line creation schema
    """
    line_number: int = Field(..., ge=1, description="Line number")

class SalesInvoiceLineResponse(SalesInvoiceLineBase):
    """
    Sales invoice line response schema
    """
    invoice_key: str = Field(..., description="Invoice number")
    line_number: int = Field(..., description="Line number")
    line_net_amount: Decimal = Field(..., decimal_places=2, description="Line net amount")
    line_tax_amount: Decimal = Field(..., decimal_places=2, description="Line tax amount")
    line_total_amount: Decimal = Field(..., decimal_places=2, description="Line total amount")
    
    class Config:
        orm_mode = True

class SalesInvoiceBase(BaseModel):
    """
    Base sales invoice schema
    """
    invoice_date: int = Field(..., ge=19000101, le=29991231, description="Invoice date (YYYYMMDD)")
    due_date: int = Field(..., ge=19000101, le=29991231, description="Due date (YYYYMMDD)")
    order_number: str = Field("", max_length=15, description="Order number")
    customer_po: str = Field("", max_length=20, description="Customer PO number")
    payment_terms: str = Field("", max_length=10, description="Payment terms")
    discount_percent: Decimal = Field(
        Decimal('0.00'), 
        ge=0, 
        le=100, 
        decimal_places=2, 
        description="Settlement discount"
    )
    delivery_address: str = Field("", max_length=150, description="Delivery address")
    delivery_date: Optional[int] = Field(None, ge=19000101, le=29991231, description="Delivery date")
    
    @validator('due_date')
    def validate_due_date(cls, v, values):
        """Ensure due date is not before invoice date"""
        if 'invoice_date' in values and v < values['invoice_date']:
            raise ValueError('Due date cannot be before invoice date')
        return v

class SalesInvoiceCreate(SalesInvoiceBase):
    """
    Sales invoice creation schema
    """
    sales_key: str = Field(..., max_length=7, description="Customer code")
    lines: List[SalesInvoiceLineCreate] = Field(..., min_items=1, description="Invoice lines")
    
    @validator('lines')
    def validate_line_numbers(cls, v):
        """Ensure line numbers are unique and sequential"""
        line_numbers = [line.line_number for line in v]
        if len(set(line_numbers)) != len(line_numbers):
            raise ValueError('Line numbers must be unique')
        return v

class SalesInvoiceResponse(SalesInvoiceBase):
    """
    Sales invoice response schema
    """
    invoice_key: str = Field(..., description="Invoice number")
    sales_key: str = Field(..., description="Customer code")
    invoice_status: str = Field(..., description="Invoice status")
    
    # Financial Totals
    net_amount: Decimal = Field(..., decimal_places=2, description="Net amount")
    tax_amount: Decimal = Field(..., decimal_places=2, description="Tax amount")
    gross_amount: Decimal = Field(..., decimal_places=2, description="Gross amount")
    discount_amount: Decimal = Field(..., decimal_places=2, description="Discount amount")
    amount_outstanding: Decimal = Field(..., decimal_places=2, description="Outstanding amount")
    
    # Status Flags
    posted_to_gl: bool = Field(..., description="Posted to General Ledger")
    
    # Invoice Lines
    lines: List[SalesInvoiceLineResponse] = Field(..., description="Invoice lines")
    
    # Audit Information
    created_by: str = Field(..., description="Created by user")
    created_at: datetime = Field(..., description="Creation timestamp")
    updated_at: Optional[datetime] = Field(None, description="Update timestamp")
    
    class Config:
        orm_mode = True
        schema_extra = {
            "example": {
                "invoice_key": "INV-2024-001",
                "sales_key": "CUST001",
                "invoice_date": 20241201,
                "due_date": 20241231,
                "invoice_status": "O",
                "order_number": "ORD-001",
                "customer_po": "PO12345",
                "payment_terms": "30",
                "discount_percent": "2.00",
                "delivery_address": "Customer Site A",
                "net_amount": "1000.00",
                "tax_amount": "200.00",
                "gross_amount": "1200.00",
                "discount_amount": "0.00",
                "amount_outstanding": "1200.00",
                "posted_to_gl": True,
                "lines": [],
                "created_by": "admin",
                "created_at": "2024-12-01T10:00:00Z"
            }
        }