"""Stock Control Comprehensive Schemas"""

from pydantic import BaseModel, Field, field_validator, ConfigDict
from typing import Optional, List, Dict, Any
from datetime import datetime, date
from decimal import Decimal
from enum import Enum


# Enums
class MovementType(str, Enum):
    RECEIPT = "receipt"
    ISSUE = "issue"
    ADJUSTMENT = "adjustment"
    TRANSFER = "transfer"
    PRODUCTION = "production"
    RETURN = "return"
    WRITE_OFF = "write_off"


class LocationType(str, Enum):
    WAREHOUSE = "warehouse"
    SHOP = "shop"
    PRODUCTION = "production"
    QUARANTINE = "quarantine"
    TRANSIT = "transit"
    RETURNS = "returns"


class CostingMethod(str, Enum):
    AVERAGE = "average"
    FIFO = "fifo"
    LIFO = "lifo"
    STANDARD = "standard"
    SPECIFIC = "specific"


class StockStatus(str, Enum):
    AVAILABLE = "available"
    ALLOCATED = "allocated"
    RESERVED = "reserved"
    QUARANTINE = "quarantine"
    DAMAGED = "damaged"
    IN_TRANSIT = "in_transit"


class OrderStatus(str, Enum):
    DRAFT = "draft"
    CONFIRMED = "confirmed"
    PROCESSING = "processing"
    PARTIAL = "partial"
    COMPLETE = "complete"
    CANCELLED = "cancelled"
    BACKORDER = "backorder"


# Item Schemas
class StockItemBase(BaseModel):
    item_code: str = Field(..., min_length=1, max_length=20)
    description: str = Field(..., min_length=1, max_length=100)
    short_description: Optional[str] = Field(None, max_length=30)
    category_code: str = Field(..., max_length=10)
    unit_of_measure: str = Field(..., max_length=3)
    alternate_units: Optional[List[str]] = None
    barcode: Optional[str] = None
    manufacturer_code: Optional[str] = None
    manufacturer_part_no: Optional[str] = None
    tax_code: Optional[str] = None
    weight: Decimal = Field(default=0)
    dimensions: Optional[Dict[str, Decimal]] = None
    is_serialized: bool = False
    is_lot_controlled: bool = False
    is_perishable: bool = False
    shelf_life_days: Optional[int] = None
    min_order_qty: Decimal = Field(default=0)
    order_multiple: Decimal = Field(default=1)


class StockItemCreate(StockItemBase):
    costing_method: CostingMethod = CostingMethod.AVERAGE
    standard_cost: Decimal = Field(default=0, ge=0)
    list_price: Decimal = Field(default=0, ge=0)
    reorder_level: Decimal = Field(default=0, ge=0)
    reorder_qty: Decimal = Field(default=0, ge=0)
    max_stock_level: Decimal = Field(default=0, ge=0)
    lead_time_days: int = Field(default=0, ge=0)
    default_location: Optional[str] = None
    is_active: bool = True
    is_sellable: bool = True
    is_purchasable: bool = True
    is_manufactured: bool = False


class StockItemUpdate(BaseModel):
    description: Optional[str] = None
    category_code: Optional[str] = None
    unit_of_measure: Optional[str] = None
    tax_code: Optional[str] = None
    standard_cost: Optional[Decimal] = None
    list_price: Optional[Decimal] = None
    reorder_level: Optional[Decimal] = None
    reorder_qty: Optional[Decimal] = None
    max_stock_level: Optional[Decimal] = None
    lead_time_days: Optional[int] = None
    is_active: Optional[bool] = None


class StockItem(StockItemBase):
    id: int
    costing_method: CostingMethod
    standard_cost: Decimal
    average_cost: Decimal
    last_cost: Decimal
    list_price: Decimal
    qty_on_hand: Decimal
    qty_allocated: Decimal
    qty_available: Decimal
    qty_on_order: Decimal
    qty_in_transit: Decimal
    reorder_level: Decimal
    reorder_qty: Decimal
    max_stock_level: Decimal
    below_reorder: bool
    stock_value: Decimal
    last_receipt_date: Optional[date] = None
    last_issue_date: Optional[date] = None
    last_count_date: Optional[date] = None
    is_active: bool
    created_at: datetime
    updated_at: Optional[datetime] = None
    
    model_config = ConfigDict(from_attributes=True)


# Location Schemas
class LocationBase(BaseModel):
    location_code: str = Field(..., min_length=1, max_length=10)
    location_name: str = Field(..., min_length=1, max_length=50)
    location_type: LocationType
    warehouse_code: Optional[str] = None
    address1: Optional[str] = None
    address2: Optional[str] = None
    city: Optional[str] = None
    state: Optional[str] = None
    zip_code: Optional[str] = None
    country: Optional[str] = None
    contact_person: Optional[str] = None
    phone: Optional[str] = None
    email: Optional[str] = None


class LocationCreate(LocationBase):
    is_active: bool = True
    allow_negative_stock: bool = False
    is_consignment: bool = False
    is_bonded: bool = False
    capacity_cubic_meters: Optional[Decimal] = None
    temperature_controlled: bool = False
    min_temperature: Optional[Decimal] = None
    max_temperature: Optional[Decimal] = None


class LocationUpdate(BaseModel):
    location_name: Optional[str] = None
    location_type: Optional[LocationType] = None
    contact_person: Optional[str] = None
    phone: Optional[str] = None
    email: Optional[str] = None
    is_active: Optional[bool] = None


class Location(LocationBase):
    id: int
    is_active: bool
    allow_negative_stock: bool
    is_consignment: bool
    is_bonded: bool
    item_count: int
    total_value: Decimal
    last_stocktake_date: Optional[date] = None
    created_at: datetime
    updated_at: Optional[datetime] = None
    
    model_config = ConfigDict(from_attributes=True)


# Movement Schemas
class StockMovementBase(BaseModel):
    movement_date: date
    movement_type: MovementType
    item_code: str
    quantity: Decimal = Field(...)
    from_location: Optional[str] = None
    to_location: Optional[str] = None
    unit_cost: Decimal = Field(..., ge=0)
    reference_type: Optional[str] = None
    reference_number: Optional[str] = None
    notes: Optional[str] = None


class StockMovementCreate(StockMovementBase):
    batch_number: Optional[str] = None
    serial_numbers: Optional[List[str]] = None
    expiry_date: Optional[date] = None
    reason_code: Optional[str] = None
    override_cost: bool = False


class StockMovement(StockMovementBase):
    id: int
    movement_number: str
    total_value: Decimal
    new_average_cost: Decimal
    qty_after_movement: Decimal
    batch_number: Optional[str] = None
    gl_posted: bool
    gl_batch_id: Optional[int] = None
    created_by: int
    created_at: datetime
    posted_at: Optional[datetime] = None
    
    model_config = ConfigDict(from_attributes=True)


# Stock Take Schemas
class StockTakeLineBase(BaseModel):
    item_code: str
    location_code: str
    counted_qty: Decimal = Field(...)
    system_qty: Decimal = Field(...)
    variance_qty: Decimal = Field(...)
    batch_number: Optional[str] = None
    notes: Optional[str] = None


class StockTakeLine(StockTakeLineBase):
    id: int
    line_number: int
    item_description: str
    unit_cost: Decimal
    variance_value: Decimal
    count_status: str  # counted, not_counted, recounted
    counted_by: Optional[str] = None
    counted_at: Optional[datetime] = None
    
    model_config = ConfigDict(from_attributes=True)


class StockTakeBase(BaseModel):
    stocktake_date: date
    location_code: Optional[str] = None
    category_code: Optional[str] = None
    description: str
    freeze_stock: bool = True


class StockTakeCreate(StockTakeBase):
    include_zero_stock: bool = False
    cycle_count_only: bool = False


class StockTake(StockTakeBase):
    id: int
    stocktake_number: str
    status: str  # draft, in_progress, completed, posted, cancelled
    lines: List[StockTakeLine]
    total_items: int
    counted_items: int
    total_variance_value: Decimal
    created_by: int
    created_at: datetime
    completed_at: Optional[datetime] = None
    posted_at: Optional[datetime] = None
    
    model_config = ConfigDict(from_attributes=True)


# Valuation Schemas
class StockValuationBase(BaseModel):
    item_code: str
    item_description: str
    category_code: str
    location_code: Optional[str] = None
    quantity_on_hand: Decimal
    unit_cost: Decimal
    total_value: Decimal
    costing_method: CostingMethod


class StockValuation(StockValuationBase):
    average_cost: Decimal
    last_cost: Decimal
    standard_cost: Decimal
    qty_allocated: Decimal
    qty_available: Decimal
    last_movement_date: Optional[date] = None
    age_days: Optional[int] = None
    
    model_config = ConfigDict(from_attributes=True)


class ValuationSummary(BaseModel):
    valuation_date: date
    total_items: int
    total_quantity: Decimal
    total_value: Decimal
    by_category: Dict[str, Dict[str, Any]]
    by_location: Dict[str, Dict[str, Any]]
    slow_moving_value: Decimal
    obsolete_value: Decimal
    
    model_config = ConfigDict(from_attributes=True)


# Order Schemas
class StockOrderLineBase(BaseModel):
    line_number: int
    item_code: str
    description: str
    quantity_ordered: Decimal = Field(..., gt=0)
    unit_price: Decimal = Field(..., ge=0)
    discount_percent: Decimal = Field(default=0, ge=0, le=100)
    tax_code: Optional[str] = None
    required_date: Optional[date] = None
    notes: Optional[str] = None


class StockOrderLine(StockOrderLineBase):
    id: int
    line_amount: Decimal
    tax_amount: Decimal
    total_amount: Decimal
    quantity_received: Decimal
    quantity_outstanding: Decimal
    last_receipt_date: Optional[date] = None
    
    model_config = ConfigDict(from_attributes=True)


class StockOrderBase(BaseModel):
    order_type: str  # purchase, sales, transfer, production
    supplier_customer_code: str
    order_date: date
    required_date: Optional[date] = None
    delivery_location: str
    reference: Optional[str] = None
    notes: Optional[str] = None


class StockOrderCreate(StockOrderBase):
    lines: List[StockOrderLineBase]
    auto_allocate: bool = True
    priority: str = Field(default="normal", pattern="^(low|normal|high|urgent)$")


class StockOrderUpdate(BaseModel):
    required_date: Optional[date] = None
    delivery_location: Optional[str] = None
    notes: Optional[str] = None
    priority: Optional[str] = None


class StockOrder(StockOrderBase):
    id: int
    order_number: str
    status: OrderStatus
    lines: List[StockOrderLine]
    subtotal: Decimal
    tax_amount: Decimal
    total_amount: Decimal
    total_received: Decimal
    total_outstanding: Decimal
    created_by: int
    created_at: datetime
    confirmed_at: Optional[datetime] = None
    completed_at: Optional[datetime] = None
    
    model_config = ConfigDict(from_attributes=True)


# Transfer Schemas
class StockTransferBase(BaseModel):
    transfer_date: date
    from_location: str
    to_location: str
    reference: Optional[str] = None
    notes: Optional[str] = None


class StockTransferLineBase(BaseModel):
    item_code: str
    quantity: Decimal = Field(..., gt=0)
    batch_number: Optional[str] = None
    serial_numbers: Optional[List[str]] = None


class StockTransferCreate(StockTransferBase):
    lines: List[StockTransferLineBase]
    transfer_immediately: bool = True


class StockTransfer(StockTransferBase):
    id: int
    transfer_number: str
    status: str  # draft, in_transit, completed, cancelled
    lines: List[Dict[str, Any]]
    total_items: int
    total_quantity: Decimal
    total_value: Decimal
    shipped_at: Optional[datetime] = None
    received_at: Optional[datetime] = None
    created_by: int
    created_at: datetime
    
    model_config = ConfigDict(from_attributes=True)


# Bill of Materials Schemas
class BOMLineBase(BaseModel):
    component_code: str
    quantity_required: Decimal = Field(..., gt=0)
    unit_of_measure: str
    scrap_percentage: Decimal = Field(default=0, ge=0, le=100)
    notes: Optional[str] = None


class BOMLine(BOMLineBase):
    id: int
    line_number: int
    component_description: str
    component_cost: Decimal
    total_cost: Decimal
    
    model_config = ConfigDict(from_attributes=True)


class BillOfMaterialsBase(BaseModel):
    parent_item_code: str
    version: str = Field(default="1.0", max_length=10)
    description: str
    quantity_produced: Decimal = Field(default=1, gt=0)
    effective_date: date
    expiry_date: Optional[date] = None


class BillOfMaterialsCreate(BillOfMaterialsBase):
    lines: List[BOMLineBase]
    is_active: bool = True


class BillOfMaterials(BillOfMaterialsBase):
    id: int
    lines: List[BOMLine]
    total_material_cost: Decimal
    total_labor_cost: Decimal
    total_overhead_cost: Decimal
    total_cost: Decimal
    is_active: bool
    created_by: int
    created_at: datetime
    approved_by: Optional[int] = None
    approved_at: Optional[datetime] = None
    
    model_config = ConfigDict(from_attributes=True)


# Analysis Schemas
class StockAnalysis(BaseModel):
    item_code: str
    item_description: str
    average_daily_usage: Decimal
    days_on_hand: Decimal
    stockout_risk: str  # low, medium, high, critical
    suggested_order_qty: Decimal
    optimal_order_date: Optional[date] = None
    abc_classification: str  # A, B, C
    xyz_classification: str  # X, Y, Z


class ReorderSuggestion(BaseModel):
    item_code: str
    item_description: str
    current_stock: Decimal
    reorder_level: Decimal
    suggested_qty: Decimal
    preferred_supplier: str
    last_price: Decimal
    estimated_cost: Decimal
    urgency: str  # low, normal, high, critical


class SlowMovingStock(BaseModel):
    item_code: str
    item_description: str
    location_code: str
    quantity: Decimal
    value: Decimal
    last_movement_date: date
    days_since_movement: int
    suggested_action: str  # discount, transfer, write_off


# Response Models
class StockItemListResponse(BaseModel):
    items: List[StockItem]
    total: int
    skip: int
    limit: int


class LocationListResponse(BaseModel):
    locations: List[Location]
    total: int
    skip: int
    limit: int


class StockMovementListResponse(BaseModel):
    movements: List[StockMovement]
    total: int
    skip: int
    limit: int


class StockOrderListResponse(BaseModel):
    orders: List[StockOrder]
    total: int
    skip: int
    limit: int


# Batch Processing Schemas
class BatchMovement(BaseModel):
    movements: List[StockMovementCreate]
    validate_only: bool = False
    post_to_gl: bool = True


class BatchAdjustment(BaseModel):
    adjustment_date: date
    reason_code: str
    adjustments: List[Dict[str, Any]]
    post_immediately: bool = True


class CycleCountSchedule(BaseModel):
    schedule_name: str
    frequency_days: int
    abc_a_items: bool = True
    abc_b_items: bool = True
    abc_c_items: bool = False
    high_value_threshold: Optional[Decimal] = None
    locations: Optional[List[str]] = None