"""
ACAS Stock Control Schemas
Simplified schemas for inventory management
"""
from pydantic import BaseModel, Field, validator
from typing import Optional
from decimal import Decimal
from datetime import datetime

class StockItemBase(BaseModel):
    """Base stock item schema"""
    stock_desc: str = Field(..., max_length=30, description="Item description")
    stock_abrev_key: str = Field("", max_length=10, description="Abbreviated key")
    stock_location: str = Field("", max_length=3, description="Location")
    stock_reorder_point: Decimal = Field(Decimal('0.000'), ge=0, decimal_places=3)
    stock_reorder_qty: Decimal = Field(Decimal('0.000'), ge=0, decimal_places=3)
    stock_std_cost: Decimal = Field(Decimal('0.0000'), ge=0, decimal_places=4)
    stock_list_price: Decimal = Field(Decimal('0.0000'), ge=0, decimal_places=4)
    stock_costing_method: str = Field("A", max_length=1, description="A=Average, F=FIFO, L=LIFO, S=Standard")
    stock_product_group: str = Field("", max_length=3)
    stock_unit_of_measure: str = Field("", max_length=3)
    stock_tax_code: str = Field("", max_length=6)
    stock_active: bool = Field(True)
    stock_sellable: bool = Field(True)
    stock_purchasable: bool = Field(True)

class StockItemCreate(StockItemBase):
    """Stock item creation schema"""
    stock_key: str = Field(..., min_length=1, max_length=13, description="Stock code")

class StockItemUpdate(BaseModel):
    """Stock item update schema"""
    stock_desc: Optional[str] = Field(None, max_length=30)
    stock_reorder_point: Optional[Decimal] = Field(None, ge=0, decimal_places=3)
    stock_reorder_qty: Optional[Decimal] = Field(None, ge=0, decimal_places=3)
    stock_std_cost: Optional[Decimal] = Field(None, ge=0, decimal_places=4)
    stock_list_price: Optional[Decimal] = Field(None, ge=0, decimal_places=4)
    stock_active: Optional[bool] = None

class StockItemResponse(StockItemBase):
    """Stock item response schema"""
    stock_key: str
    stock_qty_on_hand: Decimal = Field(..., decimal_places=3)
    stock_qty_allocated: Decimal = Field(..., decimal_places=3)
    stock_qty_available: Decimal = Field(..., decimal_places=3)
    stock_qty_on_order: Decimal = Field(..., decimal_places=3)
    stock_avg_cost: Decimal = Field(..., decimal_places=4)
    current_value: Decimal = Field(..., decimal_places=2, description="Current inventory value")
    is_below_reorder_point: bool
    created_at: datetime
    updated_at: Optional[datetime] = None
    
    class Config:
        orm_mode = True

class StockMovementBase(BaseModel):
    """Base stock movement schema"""
    movement_date: int = Field(..., ge=19000101, le=29991231, description="Movement date (YYYYMMDD)")
    movement_type: str = Field(..., max_length=2, description="RR=Receipt, IS=Issue, AD=Adjustment")
    document_ref: str = Field("", max_length=15, description="Document reference")
    quantity: Decimal = Field(..., decimal_places=3, description="Quantity (+ for receipts, - for issues)")
    unit_cost: Decimal = Field(..., ge=0, decimal_places=4, description="Unit cost")
    location: str = Field("", max_length=3, description="Location")

class StockMovementCreate(StockMovementBase):
    """Stock movement creation schema"""
    stock_key: str = Field(..., max_length=13, description="Stock item code")

class StockMovementResponse(StockMovementBase):
    """Stock movement response schema"""
    audit_id: int
    stock_key: str
    total_value: Decimal = Field(..., decimal_places=2, description="Total movement value")
    user_id: str = Field(..., description="User who created movement")
    timestamp: datetime
    is_receipt: bool
    is_issue: bool
    movement_description: str
    
    class Config:
        orm_mode = True