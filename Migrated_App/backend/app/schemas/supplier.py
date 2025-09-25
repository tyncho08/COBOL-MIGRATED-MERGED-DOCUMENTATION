"""
ACAS Supplier (Purchase Ledger) Schemas
Simplified schemas for supplier management
"""
from pydantic import BaseModel, Field, validator
from typing import Optional, List
from decimal import Decimal
from datetime import datetime

class SupplierBase(BaseModel):
    """Base supplier schema"""
    purch_name: str = Field(..., max_length=30, description="Supplier name")
    purch_address_1: str = Field("", max_length=30, description="Address line 1")
    purch_address_2: str = Field("", max_length=30, description="Address line 2") 
    purch_address_3: str = Field("", max_length=30, description="Address line 3")
    purch_address_4: str = Field("", max_length=30, description="Address line 4")
    purch_address_5: str = Field("", max_length=30, description="Address line 5")
    purch_contact: str = Field("", max_length=25, description="Contact person")
    purch_phone: str = Field("", max_length=20, description="Phone")
    purch_email: str = Field("", max_length=40, description="Email")
    purch_payment_terms: str = Field("", max_length=10, description="Payment terms")
    purch_tax_code: str = Field("", max_length=6, description="Tax code")
    purch_account_status: str = Field("A", max_length=1, description="Status: A/H/C")

class SupplierCreate(SupplierBase):
    """Supplier creation schema"""
    purch_key: str = Field(..., min_length=1, max_length=7, description="Supplier code")

class SupplierUpdate(BaseModel):
    """Supplier update schema"""
    purch_name: Optional[str] = Field(None, max_length=30)
    purch_phone: Optional[str] = Field(None, max_length=20)
    purch_email: Optional[str] = Field(None, max_length=40)
    purch_account_status: Optional[str] = Field(None, max_length=1)

class SupplierResponse(SupplierBase):
    """Supplier response schema"""
    purch_key: str = Field(..., description="Supplier code")
    purch_balance: Decimal = Field(..., decimal_places=2, description="Balance")
    purch_ytd_turnover: Decimal = Field(..., decimal_places=2, description="YTD turnover")
    is_active: bool = Field(..., description="Is active")
    created_at: datetime
    updated_at: Optional[datetime] = None
    
    class Config:
        orm_mode = True

# Purchase Invoice schemas (simplified)
class PurchaseInvoiceLineBase(BaseModel):
    """Base purchase invoice line"""
    stock_key: str = Field("", max_length=13)
    item_description: str = Field(..., max_length=40)
    quantity: Decimal = Field(..., ge=0, decimal_places=3)
    unit_cost: Decimal = Field(..., ge=0, decimal_places=4)
    line_tax_code: str = Field("", max_length=6)

class PurchaseInvoiceLineCreate(PurchaseInvoiceLineBase):
    """Purchase invoice line creation"""
    line_number: int = Field(..., ge=1)

class PurchaseInvoiceLineResponse(PurchaseInvoiceLineBase):
    """Purchase invoice line response"""
    invoice_key: str
    line_number: int
    line_net_amount: Decimal = Field(..., decimal_places=2)
    line_tax_amount: Decimal = Field(..., decimal_places=2)
    line_total_amount: Decimal = Field(..., decimal_places=2)
    matched: bool = Field(..., description="Three-way matched")
    
    class Config:
        orm_mode = True

class PurchaseInvoiceBase(BaseModel):
    """Base purchase invoice"""
    supplier_invoice_no: str = Field(..., max_length=20, description="Supplier invoice number")
    invoice_date: int = Field(..., ge=19000101, le=29991231)
    due_date: int = Field(..., ge=19000101, le=29991231)
    purchase_order_no: str = Field("", max_length=15)
    payment_terms: str = Field("", max_length=10)

class PurchaseInvoiceCreate(PurchaseInvoiceBase):
    """Purchase invoice creation"""
    purch_key: str = Field(..., max_length=7, description="Supplier code")
    lines: List[PurchaseInvoiceLineCreate] = Field(..., min_items=1)

class PurchaseInvoiceResponse(PurchaseInvoiceBase):
    """Purchase invoice response"""
    invoice_key: str
    purch_key: str
    invoice_status: str
    net_amount: Decimal = Field(..., decimal_places=2)
    tax_amount: Decimal = Field(..., decimal_places=2)
    gross_amount: Decimal = Field(..., decimal_places=2)
    amount_outstanding: Decimal = Field(..., decimal_places=2)
    three_way_matched: bool
    posted_to_gl: bool
    lines: List[PurchaseInvoiceLineResponse] = []
    created_at: datetime
    
    class Config:
        orm_mode = True