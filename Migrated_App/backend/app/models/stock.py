"""
ACAS Stock Models
SQLAlchemy models for stock/inventory management
"""
from sqlalchemy import (
    Column, String, Integer, Numeric, DateTime, Text,
    ForeignKey, CheckConstraint, Index
)
from sqlalchemy.orm import relationship
from sqlalchemy.sql import func
from app.core.database import Base

class StockRec(Base):
    """
    Stock Record - Item Master
    
    Represents stock/inventory master data with quantities, costs, and control information.
    Matches the actual PostgreSQL schema structure.
    """
    __tablename__ = "stock_rec"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key - Stock Code
    stock_key = Column(String(30), primary_key=True, doc="Stock item code")
    
    # Item Identity Information
    stock_desc = Column(String(40), nullable=False, default='', doc="Stock description")
    stock_abrev_key = Column(String(10), default='', doc="Abbreviated stock code")
    
    # Location and Bin Management
    stock_location = Column(String(6), default='', doc="Default location")
    stock_bin = Column(String(10), default='', doc="Default bin location")
    
    # Quantity Information
    stock_qty_on_hand = Column(Numeric(15, 3), default=0.000, doc="Quantity on hand")
    stock_qty_allocated = Column(Numeric(15, 3), default=0.000, doc="Quantity allocated")
    stock_qty_on_order = Column(Numeric(15, 3), default=0.000, doc="Quantity on order")
    stock_qty_back_order = Column(Numeric(15, 3), default=0.000, doc="Quantity back ordered")
    stock_qty_available = Column(Numeric(15, 3), default=0.000, doc="Quantity available")
    
    # Reorder Information
    stock_reorder_point = Column(Numeric(15, 3), default=0.000, doc="Reorder point")
    stock_reorder_qty = Column(Numeric(15, 3), default=0.000, doc="Reorder quantity")
    stock_min_qty = Column(Numeric(15, 3), default=0.000, doc="Minimum quantity")
    stock_max_qty = Column(Numeric(15, 3), default=0.000, doc="Maximum quantity")
    
    # Cost Information
    stock_std_cost = Column(Numeric(15, 4), default=0.0000, doc="Standard cost")
    stock_avg_cost = Column(Numeric(15, 4), default=0.0000, doc="Average cost")
    stock_last_cost = Column(Numeric(15, 4), default=0.0000, doc="Last cost")
    stock_fifo_cost = Column(Numeric(15, 4), default=0.0000, doc="FIFO cost")
    stock_lifo_cost = Column(Numeric(15, 4), default=0.0000, doc="LIFO cost")
    
    # Pricing Information
    stock_list_price = Column(Numeric(15, 4), default=0.0000, doc="List price")
    stock_price_1 = Column(Numeric(15, 4), default=0.0000, doc="Price level 1")
    stock_price_2 = Column(Numeric(15, 4), default=0.0000, doc="Price level 2")
    stock_price_3 = Column(Numeric(15, 4), default=0.0000, doc="Price level 3")
    stock_price_4 = Column(Numeric(15, 4), default=0.0000, doc="Price level 4")
    stock_price_5 = Column(Numeric(15, 4), default=0.0000, doc="Price level 5")
    
    # Control Information
    stock_costing_method = Column(String(1), default='A', doc="Costing method: A=Average, F=FIFO, L=LIFO, S=Standard")
    stock_product_group = Column(String(6), default='', doc="Product group")
    stock_unit_of_measure = Column(String(6), default='EA', doc="Unit of measure")
    stock_lead_time = Column(Integer, default=0, doc="Lead time in days")
    stock_duty_rate = Column(Numeric(6, 2), default=0.00, doc="Duty rate percentage")
    stock_tax_code = Column(String(4), default='VSTD', doc="Tax/VAT code")
    
    # Status Flags
    stock_discontinued = Column(String(1), default='N', doc="Discontinued flag")
    stock_kit = Column(String(1), default='N', doc="Kit item flag")
    stock_serial_tracked = Column(String(1), default='N', doc="Serial number tracked")
    stock_lot_tracked = Column(String(1), default='N', doc="Lot number tracked")
    stock_expiry_tracked = Column(String(1), default='N', doc="Expiry date tracked")
    stock_consignment = Column(String(1), default='N', doc="Consignment stock flag")
    
    # Analysis Fields
    stock_analysis_1 = Column(String(10), default='', doc="Analysis field 1")
    stock_analysis_2 = Column(String(10), default='', doc="Analysis field 2")
    stock_analysis_3 = Column(String(10), default='', doc="Analysis field 3")
    stock_abc_code = Column(String(1), default='C', doc="ABC classification")
    
    # Supplier Information
    stock_primary_supplier = Column(String(10), default='', doc="Primary supplier code")
    stock_supplier_part_no = Column(String(30), default='', doc="Supplier part number")
    
    # Usage Statistics
    stock_mtd_usage = Column(Numeric(15, 3), default=0.000, doc="Month-to-date usage")
    stock_ytd_usage = Column(Numeric(15, 3), default=0.000, doc="Year-to-date usage")
    stock_last_year_usage = Column(Numeric(15, 3), default=0.000, doc="Last year usage")
    
    # Date Fields (stored as integers in YYYYMMDD format)
    stock_date_last_sale = Column(Integer, default=0, doc="Last sale date (YYYYMMDD)")
    stock_date_last_receipt = Column(Integer, default=0, doc="Last receipt date (YYYYMMDD)")
    stock_date_last_count = Column(Integer, default=0, doc="Last count date (YYYYMMDD)")
    
    # Physical Characteristics
    stock_weight = Column(Numeric(10, 3), default=0.000, doc="Item weight")
    stock_volume = Column(Numeric(10, 3), default=0.000, doc="Item volume")
    stock_barcode = Column(String(30), default='', doc="Barcode")
    
    # Notes
    stock_notes = Column(Text, doc="Stock item notes")
    
    # Audit Trail
    created_at = Column(DateTime(timezone=True), server_default=func.current_timestamp(), doc="Record creation timestamp")
    updated_at = Column(DateTime(timezone=True), server_default=func.current_timestamp(), onupdate=func.current_timestamp(), doc="Last update timestamp")
    updated_by = Column(String(30), server_default=func.current_user(), doc="Updated by user")
    
    # Relationships
    audit_records = relationship("StockAuditRec", back_populates="stock_item")
    movements = relationship("StockMovementRec", back_populates="stock_item")
    bin_records = relationship("StockBinRec", back_populates="stock_item")
    pick_list_lines = relationship("PickListLineRec", back_populates="stock_item")
    
    # Table constraints
    __table_args__ = (
        CheckConstraint("stock_costing_method IN ('A', 'F', 'L', 'S')", name='ck_stock_valid_costing'),
        CheckConstraint("stock_discontinued IN ('Y', 'N')", name='ck_stock_valid_discontinued'),
        CheckConstraint("stock_kit IN ('Y', 'N')", name='ck_stock_valid_kit'),
        CheckConstraint("stock_serial_tracked IN ('Y', 'N')", name='ck_stock_valid_serial'),
        CheckConstraint("stock_lot_tracked IN ('Y', 'N')", name='ck_stock_valid_lot'),
        CheckConstraint("stock_expiry_tracked IN ('Y', 'N')", name='ck_stock_valid_expiry'),
        CheckConstraint("stock_consignment IN ('Y', 'N')", name='ck_stock_valid_consignment'),
        CheckConstraint("stock_abc_code IN ('A', 'B', 'C')", name='ck_stock_valid_abc'),
        Index('idx_stock_desc', 'stock_desc'),
        Index('idx_stock_abrev', 'stock_abrev_key'),
        Index('idx_stock_group', 'stock_product_group'),
        Index('idx_stock_available', 'stock_qty_available'),
        {'schema': 'acas'}
    )

class StockAuditRec(Base):
    """Stock Audit Record - for tracking stock level changes"""
    __tablename__ = "stockaudit_rec"
    __table_args__ = {'schema': 'acas'}
    
    audit_id = Column(Integer, primary_key=True, autoincrement=True, doc="Audit record ID")
    stock_key = Column(String(30), ForeignKey("acas.stock_rec.stock_key", ondelete="RESTRICT"), nullable=False, doc="Stock item code")
    audit_date = Column(Integer, nullable=False, doc="Audit date (YYYYMMDD)")
    audit_type = Column(String(10), nullable=False, doc="Audit type")
    old_quantity = Column(Numeric(15, 3), default=0.000, doc="Previous quantity")
    new_quantity = Column(Numeric(15, 3), default=0.000, doc="New quantity")
    variance = Column(Numeric(15, 3), default=0.000, doc="Quantity variance")
    reason_code = Column(String(10), doc="Reason code")
    reference = Column(String(20), doc="Reference document")
    notes = Column(Text, doc="Audit notes")
    
    # Audit Trail
    created_at = Column(DateTime(timezone=True), server_default=func.current_timestamp())
    created_by = Column(String(30), server_default=func.current_user())
    
    # Relationships
    stock_item = relationship("StockRec", back_populates="audit_records")


# Alias for backward compatibility with services
StockMasterRec = StockRec


class StockLocationRec(Base):
    """
    Stock Location Record - Warehouse/bin locations
    """
    __tablename__ = "stock_locations"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    location_id = Column(Integer, primary_key=True, autoincrement=True, doc="Location ID")
    location_code = Column(String(10), unique=True, nullable=False, doc="Location code")
    
    # Location Information
    location_name = Column(String(50), nullable=False, doc="Location name")
    warehouse_code = Column(String(10), nullable=False, doc="Warehouse code")
    zone_code = Column(String(10), doc="Zone within warehouse")
    
    # Physical Details
    aisle = Column(String(10), doc="Aisle")
    bay = Column(String(10), doc="Bay")
    level = Column(String(10), doc="Level/shelf")
    
    # Capacity Information
    max_capacity = Column(Numeric(15, 3), default=0.000, doc="Maximum capacity")
    current_utilization = Column(Numeric(15, 3), default=0.000, doc="Current utilization")
    
    # Status
    is_active = Column(String(1), default='Y', doc="Active location flag")
    location_type = Column(String(10), default='STORAGE', doc="Location type")
    
    # Control
    allow_mixed_items = Column(String(1), default='Y', doc="Allow multiple SKUs")
    require_lot_control = Column(String(1), default='N', doc="Require lot control")
    
    # Audit Trail
    created_at = Column(DateTime(timezone=True), server_default=func.current_timestamp())
    updated_at = Column(DateTime(timezone=True), server_default=func.current_timestamp(), onupdate=func.current_timestamp())


class StockMovementRec(Base):
    """
    Stock Movement Record - Inventory transactions
    """
    __tablename__ = "stock_movements"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    movement_id = Column(Integer, primary_key=True, autoincrement=True, doc="Movement ID")
    
    # Stock and Location
    stock_key = Column(String(30), ForeignKey("acas.stock_rec.stock_key", ondelete="RESTRICT"), nullable=False, doc="Stock item code")
    location_code = Column(String(10), doc="Location code")
    
    # Movement Details
    movement_date = Column(Integer, nullable=False, doc="Movement date (YYYYMMDD)")
    movement_type = Column(String(10), nullable=False, doc="Movement type")
    quantity = Column(Numeric(15, 3), nullable=False, doc="Movement quantity")
    unit_cost = Column(Numeric(15, 4), default=0.0000, doc="Unit cost")
    
    # Document References
    document_type = Column(String(10), doc="Source document type")
    document_number = Column(String(20), doc="Source document number")
    document_line = Column(Integer, doc="Document line number")
    
    # Additional Information
    reason_code = Column(String(10), doc="Reason/adjustment code")
    reference = Column(String(30), doc="Reference")
    notes = Column(Text, doc="Movement notes")
    
    # Lot/Serial Information
    lot_number = Column(String(30), doc="Lot number")
    serial_number = Column(String(50), doc="Serial number")
    expiry_date = Column(Integer, doc="Expiry date (YYYYMMDD)")
    
    # Audit Trail
    created_by = Column(String(30), nullable=False, doc="Created by user")
    created_at = Column(DateTime(timezone=True), server_default=func.current_timestamp())
    
    # Relationships
    stock_item = relationship("StockRec", foreign_keys=[stock_key])


class StockBinRec(Base):
    """
    Stock Bin Record - Bin-level stock tracking
    """
    __tablename__ = "stock_bins"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    bin_id = Column(Integer, primary_key=True, autoincrement=True, doc="Bin ID")
    
    # Bin Identification
    stock_key = Column(String(30), ForeignKey("acas.stock_rec.stock_key", ondelete="RESTRICT"), nullable=False, doc="Stock item code")
    location_code = Column(String(10), nullable=False, doc="Location code")
    bin_code = Column(String(20), nullable=False, doc="Bin code")
    
    # Quantities
    qty_on_hand = Column(Numeric(15, 3), default=0.000, doc="Quantity on hand")
    qty_allocated = Column(Numeric(15, 3), default=0.000, doc="Quantity allocated")
    qty_available = Column(Numeric(15, 3), default=0.000, doc="Quantity available")
    
    # Lot/Serial Tracking
    lot_number = Column(String(30), doc="Lot number")
    serial_number = Column(String(50), doc="Serial number")
    expiry_date = Column(Integer, doc="Expiry date (YYYYMMDD)")
    
    # Status
    status = Column(String(10), default='ACTIVE', doc="Bin status")
    
    # Audit Trail
    last_movement_date = Column(Integer, doc="Last movement date (YYYYMMDD)")
    last_counted_date = Column(Integer, doc="Last count date (YYYYMMDD)")
    
    # Relationships
    stock_item = relationship("StockRec", foreign_keys=[stock_key])


class PickListRec(Base):
    """
    Pick List Record - Warehouse picking instructions
    """
    __tablename__ = "pick_lists"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    pick_list_id = Column(Integer, primary_key=True, autoincrement=True, doc="Pick list ID")
    pick_list_number = Column(String(20), unique=True, nullable=False, doc="Pick list number")
    
    # References
    warehouse_code = Column(String(10), nullable=False, doc="Warehouse code")
    route_code = Column(String(10), doc="Picking route")
    
    # Dates and Status
    created_date = Column(Integer, nullable=False, doc="Created date (YYYYMMDD)")
    required_date = Column(Integer, doc="Required date (YYYYMMDD)")
    status = Column(String(20), default='OPEN', doc="Pick list status")
    
    # Priority and Control
    priority = Column(String(10), default='NORMAL', doc="Pick priority")
    picker_assigned = Column(String(30), doc="Assigned picker")
    
    # Progress Tracking
    total_lines = Column(Integer, default=0, doc="Total lines")
    lines_picked = Column(Integer, default=0, doc="Lines picked")
    
    # Audit Trail
    created_by = Column(String(30), nullable=False, doc="Created by user")
    created_at = Column(DateTime(timezone=True), server_default=func.current_timestamp())
    started_at = Column(DateTime(timezone=True), doc="Picking started")
    completed_at = Column(DateTime(timezone=True), doc="Picking completed")
    
    # Relationships
    lines = relationship("PickListLineRec", back_populates="pick_list", cascade="all, delete-orphan")


class PickListLineRec(Base):
    """
    Pick List Line Record - Individual pick instructions
    """
    __tablename__ = "pick_list_lines"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    line_id = Column(Integer, primary_key=True, autoincrement=True, doc="Line ID")
    
    # Pick List Reference
    pick_list_id = Column(Integer, ForeignKey("acas.pick_lists.pick_list_id", ondelete="CASCADE"), nullable=False, doc="Pick list ID")
    line_number = Column(Integer, nullable=False, doc="Line number")
    
    # Item Information
    stock_key = Column(String(30), ForeignKey("acas.stock_rec.stock_key", ondelete="RESTRICT"), nullable=False, doc="Stock item code")
    description = Column(String(100), nullable=False, doc="Item description")
    
    # Location and Quantities
    location_code = Column(String(10), doc="Pick location")
    bin_code = Column(String(20), doc="Pick bin")
    quantity_required = Column(Numeric(15, 3), nullable=False, doc="Quantity to pick")
    quantity_picked = Column(Numeric(15, 3), default=0.000, doc="Quantity picked")
    
    # Order Reference
    source_document = Column(String(20), doc="Source order number")
    source_line = Column(Integer, doc="Source line number")
    
    # Status and Control
    status = Column(String(20), default='OPEN', doc="Line status")
    sequence = Column(Integer, doc="Pick sequence")
    
    # Lot/Serial Information
    lot_number = Column(String(30), doc="Lot number")
    serial_number = Column(String(50), doc="Serial number")
    
    # Audit Trail
    picked_by = Column(String(30), doc="Picked by user")
    picked_at = Column(DateTime(timezone=True), doc="Picked timestamp")
    
    # Relationships
    pick_list = relationship("PickListRec", back_populates="lines")
    stock_item = relationship("StockRec", foreign_keys=[stock_key])


class StockSupplierRec(Base):
    """Stock Supplier Record - Supplier-specific stock information"""
    __tablename__ = "stock_suppliers"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    supplier_id = Column(Integer, primary_key=True, autoincrement=True, doc="Supplier record ID")
    
    # References
    stock_key = Column(String(30), ForeignKey("acas.stock_rec.stock_key", ondelete="CASCADE"), nullable=False, doc="Stock item code")
    supplier_code = Column(String(10), nullable=False, doc="Supplier code")
    
    # Supplier Information
    supplier_part_number = Column(String(30), nullable=False, doc="Supplier part number")
    supplier_description = Column(String(100), doc="Supplier description")
    
    # Pricing Information
    cost_price = Column(Numeric(15, 4), default=0.0000, doc="Supplier cost price")
    currency = Column(String(3), default='USD', doc="Cost currency")
    price_break_qty_1 = Column(Numeric(15, 3), default=0.000, doc="Price break quantity 1")
    price_break_cost_1 = Column(Numeric(15, 4), default=0.0000, doc="Price break cost 1")
    price_break_qty_2 = Column(Numeric(15, 3), default=0.000, doc="Price break quantity 2")
    price_break_cost_2 = Column(Numeric(15, 4), default=0.0000, doc="Price break cost 2")
    price_break_qty_3 = Column(Numeric(15, 3), default=0.000, doc="Price break quantity 3")
    price_break_cost_3 = Column(Numeric(15, 4), default=0.0000, doc="Price break cost 3")
    
    # Order Information
    minimum_order_qty = Column(Numeric(15, 3), default=0.000, doc="Minimum order quantity")
    order_multiple = Column(Numeric(15, 3), default=1.000, doc="Order multiple")
    lead_time_days = Column(Integer, default=0, doc="Lead time in days")
    
    # Status and Control
    is_preferred = Column(String(1), default='N', doc="Preferred supplier flag")
    is_active = Column(String(1), default='Y', doc="Active supplier flag")
    last_order_date = Column(Integer, doc="Last order date (YYYYMMDD)")
    last_receipt_date = Column(Integer, doc="Last receipt date (YYYYMMDD)")
    
    # Notes
    notes = Column(Text, doc="Supplier notes")
    
    # Audit Trail
    created_at = Column(DateTime(timezone=True), server_default=func.current_timestamp())
    updated_at = Column(DateTime(timezone=True), server_default=func.current_timestamp(), onupdate=func.current_timestamp())
    
    # Relationships
    stock_item = relationship("StockRec", foreign_keys=[stock_key])


class StockPriceRec(Base):
    """Stock Price Record - Customer price levels"""
    __tablename__ = "stock_prices"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    price_id = Column(Integer, primary_key=True, autoincrement=True, doc="Price record ID")
    
    # References
    stock_key = Column(String(30), ForeignKey("acas.stock_rec.stock_key", ondelete="CASCADE"), nullable=False, doc="Stock item code")
    
    # Price Information
    price_level = Column(Integer, nullable=False, doc="Price level (1-10)")
    price_type = Column(String(10), default='SELLING', doc="Price type")
    price_amount = Column(Numeric(15, 4), nullable=False, doc="Price amount")
    price_currency = Column(String(3), default='USD', doc="Price currency")
    
    # Price Break Information
    break_qty_1 = Column(Numeric(15, 3), default=0.000, doc="Break quantity 1")
    break_price_1 = Column(Numeric(15, 4), default=0.0000, doc="Break price 1")
    break_qty_2 = Column(Numeric(15, 3), default=0.000, doc="Break quantity 2")
    break_price_2 = Column(Numeric(15, 4), default=0.0000, doc="Break price 2")
    break_qty_3 = Column(Numeric(15, 3), default=0.000, doc="Break quantity 3")
    break_price_3 = Column(Numeric(15, 4), default=0.0000, doc="Break price 3")
    
    # Validity Period
    effective_date = Column(Integer, nullable=False, doc="Effective date (YYYYMMDD)")
    expiry_date = Column(Integer, doc="Expiry date (YYYYMMDD)")
    
    # Customer/Group Specific
    customer_code = Column(String(10), doc="Specific customer code")
    customer_group = Column(String(10), doc="Customer group code")
    price_list_code = Column(String(10), doc="Price list code")
    
    # Status
    is_active = Column(String(1), default='Y', doc="Active price flag")
    
    # Audit Trail
    created_by = Column(String(30), nullable=False, doc="Created by user")
    created_at = Column(DateTime(timezone=True), server_default=func.current_timestamp())
    updated_at = Column(DateTime(timezone=True), server_default=func.current_timestamp(), onupdate=func.current_timestamp())
    
    # Relationships
    stock_item = relationship("StockRec", foreign_keys=[stock_key])


class StockBarcodeRec(Base):
    """Stock Barcode Record - Multiple barcodes per item"""
    __tablename__ = "stock_barcodes"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    barcode_id = Column(Integer, primary_key=True, autoincrement=True, doc="Barcode record ID")
    
    # References
    stock_key = Column(String(30), ForeignKey("acas.stock_rec.stock_key", ondelete="CASCADE"), nullable=False, doc="Stock item code")
    
    # Barcode Information
    barcode = Column(String(50), unique=True, nullable=False, doc="Barcode value")
    barcode_type = Column(String(20), default='EAN13', doc="Barcode type")
    
    # Unit Information
    unit_of_measure = Column(String(6), default='EA', doc="Unit of measure for this barcode")
    conversion_factor = Column(Numeric(15, 6), default=1.000000, doc="Conversion to base unit")
    
    # Packaging Information
    pack_size = Column(Numeric(15, 3), default=1.000, doc="Pack size")
    pack_description = Column(String(50), doc="Pack description")
    
    # Status and Control
    is_primary = Column(String(1), default='N', doc="Primary barcode flag")
    is_active = Column(String(1), default='Y', doc="Active barcode flag")
    
    # Supplier Information
    supplier_code = Column(String(10), doc="Supplier code for this barcode")
    supplier_barcode = Column(String(50), doc="Supplier's barcode")
    
    # Notes
    notes = Column(Text, doc="Barcode notes")
    
    # Audit Trail
    created_at = Column(DateTime(timezone=True), server_default=func.current_timestamp())
    updated_at = Column(DateTime(timezone=True), server_default=func.current_timestamp(), onupdate=func.current_timestamp())
    
    # Relationships
    stock_item = relationship("StockRec", foreign_keys=[stock_key])


class StockTransferRec(Base):
    """Stock Transfer Record - Inter-location transfers"""
    __tablename__ = "stock_transfers"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    transfer_id = Column(Integer, primary_key=True, autoincrement=True, doc="Transfer ID")
    transfer_number = Column(String(20), unique=True, nullable=False, doc="Transfer number")
    
    # Transfer Details
    transfer_date = Column(Integer, nullable=False, doc="Transfer date (YYYYMMDD)")
    transfer_type = Column(String(20), default='LOCATION', doc="Transfer type")
    
    # Stock Information
    stock_key = Column(String(30), ForeignKey("acas.stock_rec.stock_key", ondelete="RESTRICT"), nullable=False, doc="Stock item code")
    quantity = Column(Numeric(15, 3), nullable=False, doc="Transfer quantity")
    
    # Location Information
    from_location = Column(String(10), nullable=False, doc="From location")
    from_bin = Column(String(20), doc="From bin")
    to_location = Column(String(10), nullable=False, doc="To location")
    to_bin = Column(String(20), doc="To bin")
    
    # Status and Control
    status = Column(String(20), default='PENDING', doc="Transfer status")
    authorized_by = Column(String(30), doc="Authorized by user")
    authorized_date = Column(Integer, doc="Authorization date (YYYYMMDD)")
    
    # Cost Information
    unit_cost = Column(Numeric(15, 4), default=0.0000, doc="Unit cost")
    total_cost = Column(Numeric(12, 2), default=0.00, doc="Total cost")
    
    # References
    reference = Column(String(30), doc="Transfer reference")
    reason_code = Column(String(10), doc="Reason code")
    notes = Column(Text, doc="Transfer notes")
    
    # Audit Trail
    created_by = Column(String(30), nullable=False, doc="Created by user")
    created_at = Column(DateTime(timezone=True), server_default=func.current_timestamp())
    
    # Relationships
    stock_item = relationship("StockRec", foreign_keys=[stock_key])


class StockAdjustmentRec(Base):
    """Stock Adjustment Record - Quantity adjustments"""
    __tablename__ = "stock_adjustments"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    adjustment_id = Column(Integer, primary_key=True, autoincrement=True, doc="Adjustment ID")
    adjustment_number = Column(String(20), unique=True, nullable=False, doc="Adjustment number")
    
    # Adjustment Details
    adjustment_date = Column(Integer, nullable=False, doc="Adjustment date (YYYYMMDD)")
    adjustment_type = Column(String(20), nullable=False, doc="Adjustment type")
    
    # Stock Information
    stock_key = Column(String(30), ForeignKey("acas.stock_rec.stock_key", ondelete="RESTRICT"), nullable=False, doc="Stock item code")
    location_code = Column(String(10), nullable=False, doc="Location code")
    bin_code = Column(String(20), doc="Bin code")
    
    # Quantity Information
    quantity_before = Column(Numeric(15, 3), nullable=False, doc="Quantity before adjustment")
    quantity_adjusted = Column(Numeric(15, 3), nullable=False, doc="Adjustment quantity (+/-)")
    quantity_after = Column(Numeric(15, 3), nullable=False, doc="Quantity after adjustment")
    
    # Cost Information
    unit_cost = Column(Numeric(15, 4), default=0.0000, doc="Unit cost")
    adjustment_value = Column(Numeric(12, 2), default=0.00, doc="Adjustment value")
    
    # Reason and Authorization
    reason_code = Column(String(10), nullable=False, doc="Reason code")
    reason_description = Column(String(100), doc="Reason description")
    authorized_by = Column(String(30), doc="Authorized by user")
    authorized_date = Column(Integer, doc="Authorization date (YYYYMMDD)")
    
    # References
    reference = Column(String(30), doc="Adjustment reference")
    source_document = Column(String(20), doc="Source document")
    
    # Lot/Serial Information
    lot_number = Column(String(30), doc="Lot number")
    serial_number = Column(String(50), doc="Serial number")
    
    # Notes
    notes = Column(Text, doc="Adjustment notes")
    
    # Audit Trail
    created_by = Column(String(30), nullable=False, doc="Created by user")
    created_at = Column(DateTime(timezone=True), server_default=func.current_timestamp())
    
    # Relationships
    stock_item = relationship("StockRec", foreign_keys=[stock_key])


class StockCountRec(Base):
    """Stock Count Record - Physical counts and cycle counts"""
    __tablename__ = "stock_counts"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    count_id = Column(Integer, primary_key=True, autoincrement=True, doc="Count ID")
    count_number = Column(String(20), unique=True, nullable=False, doc="Count number")
    
    # Count Details
    count_date = Column(Integer, nullable=False, doc="Count date (YYYYMMDD)")
    count_type = Column(String(20), nullable=False, doc="Count type (CYCLE, PHYSICAL, SPOT)")
    count_method = Column(String(20), default='MANUAL', doc="Count method")
    
    # Stock Information
    stock_key = Column(String(30), ForeignKey("acas.stock_rec.stock_key", ondelete="RESTRICT"), nullable=False, doc="Stock item code")
    location_code = Column(String(10), nullable=False, doc="Location code")
    bin_code = Column(String(20), doc="Bin code")
    
    # Quantity Information
    system_quantity = Column(Numeric(15, 3), nullable=False, doc="System quantity")
    counted_quantity = Column(Numeric(15, 3), nullable=False, doc="Counted quantity")
    variance_quantity = Column(Numeric(15, 3), default=0.000, doc="Variance quantity")
    variance_percentage = Column(Numeric(8, 4), default=0.0000, doc="Variance percentage")
    
    # Cost Information
    unit_cost = Column(Numeric(15, 4), default=0.0000, doc="Unit cost")
    variance_value = Column(Numeric(12, 2), default=0.00, doc="Variance value")
    
    # Count Information
    counted_by = Column(String(30), nullable=False, doc="Counted by user")
    count_time = Column(Integer, doc="Count time (HHMMSS)")
    count_sequence = Column(Integer, doc="Count sequence")
    
    # Verification
    verified_by = Column(String(30), doc="Verified by user")
    verified_date = Column(Integer, doc="Verification date (YYYYMMDD)")
    recount_required = Column(String(1), default='N', doc="Recount required flag")
    
    # Status and Control
    count_status = Column(String(20), default='PENDING', doc="Count status")
    adjustment_created = Column(String(1), default='N', doc="Adjustment created flag")
    adjustment_number = Column(String(20), doc="Related adjustment number")
    
    # Lot/Serial Information
    lot_number = Column(String(30), doc="Lot number")
    serial_number = Column(String(50), doc="Serial number")
    expiry_date = Column(Integer, doc="Expiry date (YYYYMMDD)")
    
    # Notes
    notes = Column(Text, doc="Count notes")
    variance_reason = Column(String(100), doc="Variance reason")
    
    # Audit Trail
    created_at = Column(DateTime(timezone=True), server_default=func.current_timestamp())
    updated_at = Column(DateTime(timezone=True), server_default=func.current_timestamp(), onupdate=func.current_timestamp())
    
    # Relationships
    stock_item = relationship("StockRec", foreign_keys=[stock_key])


class StockValuationRec(Base):
    """Stock Valuation Record - Periodic stock valuations"""
    __tablename__ = "stock_valuations"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    valuation_id = Column(Integer, primary_key=True, autoincrement=True, doc="Valuation ID")
    
    # Valuation Details
    valuation_date = Column(Integer, nullable=False, doc="Valuation date (YYYYMMDD)")
    valuation_period = Column(Integer, nullable=False, doc="Valuation period")
    valuation_type = Column(String(20), default='STANDARD', doc="Valuation type")
    
    # Stock Information
    stock_key = Column(String(30), ForeignKey("acas.stock_rec.stock_key", ondelete="RESTRICT"), nullable=False, doc="Stock item code")
    location_code = Column(String(10), nullable=False, doc="Location code")
    
    # Quantity Information
    quantity_on_hand = Column(Numeric(15, 3), nullable=False, doc="Quantity on hand")
    quantity_allocated = Column(Numeric(15, 3), default=0.000, doc="Quantity allocated")
    quantity_available = Column(Numeric(15, 3), default=0.000, doc="Quantity available")
    
    # Cost Information
    unit_cost = Column(Numeric(15, 4), nullable=False, doc="Unit cost")
    standard_cost = Column(Numeric(15, 4), default=0.0000, doc="Standard cost")
    average_cost = Column(Numeric(15, 4), default=0.0000, doc="Average cost")
    fifo_cost = Column(Numeric(15, 4), default=0.0000, doc="FIFO cost")
    lifo_cost = Column(Numeric(15, 4), default=0.0000, doc="LIFO cost")
    
    # Valuation Amounts
    total_value = Column(Numeric(15, 2), nullable=False, doc="Total valuation")
    standard_value = Column(Numeric(15, 2), default=0.00, doc="Standard value")
    variance_value = Column(Numeric(15, 2), default=0.00, doc="Variance value")
    
    # Currency
    currency = Column(String(3), default='USD', doc="Valuation currency")
    exchange_rate = Column(Numeric(10, 6), default=1.000000, doc="Exchange rate")
    
    # Status and Control
    valuation_status = Column(String(20), default='DRAFT', doc="Valuation status")
    posted_to_gl = Column(String(1), default='N', doc="Posted to GL flag")
    posted_date = Column(Integer, doc="GL posting date (YYYYMMDD)")
    
    # Audit Trail
    created_by = Column(String(30), nullable=False, doc="Created by user")
    created_at = Column(DateTime(timezone=True), server_default=func.current_timestamp())
    
    # Relationships
    stock_item = relationship("StockRec", foreign_keys=[stock_key])


class StockRevalRec(Base):
    """Stock Revaluation Record - Revaluation adjustments"""
    __tablename__ = "stock_revaluations"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    reval_id = Column(Integer, primary_key=True, autoincrement=True, doc="Revaluation ID")
    reval_number = Column(String(20), unique=True, nullable=False, doc="Revaluation number")
    
    # Revaluation Details
    reval_date = Column(Integer, nullable=False, doc="Revaluation date (YYYYMMDD)")
    reval_reason = Column(String(100), nullable=False, doc="Revaluation reason")
    reval_type = Column(String(20), default='MARKET', doc="Revaluation type")
    
    # Stock Information
    stock_key = Column(String(30), ForeignKey("acas.stock_rec.stock_key", ondelete="RESTRICT"), nullable=False, doc="Stock item code")
    quantity_affected = Column(Numeric(15, 3), nullable=False, doc="Quantity affected")
    
    # Cost Changes
    old_unit_cost = Column(Numeric(15, 4), nullable=False, doc="Old unit cost")
    new_unit_cost = Column(Numeric(15, 4), nullable=False, doc="New unit cost")
    cost_difference = Column(Numeric(15, 4), nullable=False, doc="Cost difference per unit")
    
    # Value Changes
    old_total_value = Column(Numeric(15, 2), nullable=False, doc="Old total value")
    new_total_value = Column(Numeric(15, 2), nullable=False, doc="New total value")
    revaluation_amount = Column(Numeric(15, 2), nullable=False, doc="Revaluation amount")
    
    # Currency
    currency = Column(String(3), default='USD', doc="Revaluation currency")
    
    # Approval and Control
    approved_by = Column(String(30), doc="Approved by user")
    approved_date = Column(Integer, doc="Approval date (YYYYMMDD)")
    effective_date = Column(Integer, nullable=False, doc="Effective date (YYYYMMDD)")
    
    # GL Integration
    posted_to_gl = Column(String(1), default='N', doc="Posted to GL flag")
    gl_batch_number = Column(String(20), doc="GL batch number")
    gl_posting_date = Column(Integer, doc="GL posting date (YYYYMMDD)")
    
    # Supporting Information
    market_price = Column(Numeric(15, 4), doc="Market price")
    supplier_quote = Column(Numeric(15, 4), doc="Supplier quote")
    appraisal_value = Column(Numeric(15, 4), doc="Appraisal value")
    reference_document = Column(String(50), doc="Reference document")
    
    # Notes
    notes = Column(Text, doc="Revaluation notes")
    
    # Audit Trail
    created_by = Column(String(30), nullable=False, doc="Created by user")
    created_at = Column(DateTime(timezone=True), server_default=func.current_timestamp())
    
    # Relationships
    stock_item = relationship("StockRec", foreign_keys=[stock_key])


# Legacy aliases for backward compatibility with different naming conventions
StockItem = StockRec
StockLocation = StockLocationRec
StockMovement = StockMovementRec