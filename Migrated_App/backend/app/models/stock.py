"""
ACAS Stock Models
SQLAlchemy models for stock/inventory management
"""
from sqlalchemy import (
    Column, String, Integer, Numeric, DateTime, Text,
    ForeignKey, CheckConstraint, Index, UniqueConstraint
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


# Physical Stocktake Models
class PhysicalStocktakeRec(Base):
    """Physical stocktake header record"""
    __tablename__ = "physical_stocktakes"
    
    stocktake_id = Column(Integer, primary_key=True, autoincrement=True)
    stocktake_no = Column(String(20), unique=True, nullable=False, doc="Stocktake number")
    stocktake_date = Column(Integer, nullable=False, doc="Stocktake date YYYYMMDD")
    warehouse_code = Column(String(10), nullable=False, doc="Warehouse code")
    location_code = Column(String(20), doc="Location code (optional)")
    status = Column(String(1), nullable=False, default='O', doc="Status: O=Open, C=Complete, P=Posted, X=Cancelled")
    
    # Control fields
    freeze_date = Column(Integer, doc="Stock freeze date YYYYMMDD")
    count_date = Column(Integer, doc="Count date YYYYMMDD")
    post_date = Column(Integer, doc="Post date YYYYMMDD")
    
    # Totals
    items_counted = Column(Integer, default=0, doc="Number of items counted")
    items_not_counted = Column(Integer, default=0, doc="Number of items not counted")
    variance_count = Column(Integer, default=0, doc="Number of items with variance")
    variance_value = Column(Numeric(15, 2), default=0, doc="Total variance value")
    
    # Audit
    created_by = Column(String(30), nullable=False)
    created_date = Column(Integer, nullable=False)
    posted_by = Column(String(30))
    posted_date = Column(Integer)
    
    # Relationships
    lines = relationship("StocktakeLineRec", back_populates="stocktake")
    tags = relationship("StocktakeTagRec", back_populates="stocktake")


class StocktakeLineRec(Base):
    """Stocktake line item record"""
    __tablename__ = "stocktake_lines"
    
    line_id = Column(Integer, primary_key=True, autoincrement=True)
    stocktake_id = Column(Integer, ForeignKey("physical_stocktakes.stocktake_id"), nullable=False)
    line_no = Column(Integer, nullable=False, doc="Line number")
    
    # Item details
    stock_code = Column(String(20), nullable=False, doc="Stock code")
    location = Column(String(20), nullable=False, doc="Location")
    bin_location = Column(String(20), doc="Bin location")
    
    # Quantities
    system_qty = Column(Numeric(15, 4), nullable=False, doc="System quantity")
    counted_qty = Column(Numeric(15, 4), doc="Counted quantity")
    variance_qty = Column(Numeric(15, 4), doc="Variance quantity")
    
    # Values
    unit_cost = Column(Numeric(15, 4), nullable=False, doc="Unit cost")
    system_value = Column(Numeric(15, 2), nullable=False, doc="System value")
    counted_value = Column(Numeric(15, 2), doc="Counted value")
    variance_value = Column(Numeric(15, 2), doc="Variance value")
    
    # Status
    counted = Column(String(1), default='N', doc="Counted Y/N")
    count_date = Column(Integer, doc="Count date YYYYMMDD")
    counted_by = Column(String(30), doc="Counted by")
    
    # Notes
    notes = Column(Text, doc="Count notes")
    
    # Relationships
    stocktake = relationship("PhysicalStocktakeRec", back_populates="lines")
    variances = relationship("StocktakeVarianceRec", back_populates="line")


class StocktakeVarianceRec(Base):
    """Stocktake variance record"""
    __tablename__ = "stocktake_variances"
    
    variance_id = Column(Integer, primary_key=True, autoincrement=True)
    stocktake_id = Column(Integer, ForeignKey("physical_stocktakes.stocktake_id"), nullable=False)
    line_id = Column(Integer, ForeignKey("stocktake_lines.line_id"), nullable=False)
    
    # Variance details
    stock_code = Column(String(20), nullable=False)
    location = Column(String(20), nullable=False)
    variance_qty = Column(Numeric(15, 4), nullable=False)
    variance_value = Column(Numeric(15, 2), nullable=False)
    variance_pct = Column(Numeric(5, 2), doc="Variance percentage")
    
    # Classification
    variance_type = Column(String(10), doc="Type: OVER, SHORT, DAMAGED, etc")
    reason_code = Column(String(10), doc="Reason code")
    
    # Adjustment
    adjustment_no = Column(String(20), doc="Adjustment reference")
    adjusted = Column(String(1), default='N', doc="Adjusted Y/N")
    adjust_date = Column(Integer, doc="Adjustment date")
    
    # Audit
    notes = Column(Text)
    approved_by = Column(String(30))
    approved_date = Column(Integer)
    
    # Relationships
    line = relationship("StocktakeLineRec", back_populates="variances")


class StocktakeTagRec(Base):
    """Stocktake tag record"""
    __tablename__ = "stocktake_tags"
    
    tag_id = Column(Integer, primary_key=True, autoincrement=True)
    stocktake_id = Column(Integer, ForeignKey("physical_stocktakes.stocktake_id"), nullable=False)
    tag_no = Column(String(20), unique=True, nullable=False, doc="Tag number")
    
    # Tag details
    stock_code = Column(String(20), nullable=False)
    location = Column(String(20), nullable=False)
    bin_location = Column(String(20))
    
    # Count
    quantity = Column(Numeric(15, 4), nullable=False)
    unit_of_measure = Column(String(10))
    
    # Status
    status = Column(String(1), default='O', doc="O=Open, U=Used, V=Void")
    used_date = Column(Integer)
    
    # Audit
    issued_to = Column(String(30))
    issued_date = Column(Integer)
    counted_by = Column(String(30))
    
    # Relationships
    stocktake = relationship("PhysicalStocktakeRec", back_populates="tags")


# Serial Number Tracking Models
class SerialNumberRec(Base):
    """Serial number master record"""
    __tablename__ = "serial_numbers"
    
    serial_id = Column(Integer, primary_key=True, autoincrement=True)
    serial_number = Column(String(50), unique=True, nullable=False, doc="Serial number")
    stock_code = Column(String(20), nullable=False, doc="Stock code")
    
    # Status
    status = Column(String(1), nullable=False, default='A', doc="A=Available, S=Sold, R=Reserved, X=Scrapped")
    location = Column(String(20), doc="Current location")
    warehouse = Column(String(10), doc="Current warehouse")
    
    # Tracking
    receipt_date = Column(Integer, doc="Receipt date YYYYMMDD")
    receipt_ref = Column(String(50), doc="Receipt reference")
    supplier_serial = Column(String(50), doc="Supplier serial number")
    
    # Sales
    sales_date = Column(Integer, doc="Sales date")
    sales_ref = Column(String(50), doc="Sales reference")
    customer_code = Column(String(20), doc="Customer code")
    
    # Warranty
    warranty_start = Column(Integer, doc="Warranty start date")
    warranty_end = Column(Integer, doc="Warranty end date")
    
    # Audit
    created_date = Column(Integer, nullable=False)
    created_by = Column(String(30), nullable=False)
    
    # Relationships
    movements = relationship("SerialMovementRec", back_populates="serial")
    allocations = relationship("SerialAllocationRec", back_populates="serial")


class SerialMovementRec(Base):
    """Serial number movement record"""
    __tablename__ = "serial_movements"
    
    movement_id = Column(Integer, primary_key=True, autoincrement=True)
    serial_id = Column(Integer, ForeignKey("serial_numbers.serial_id"), nullable=False)
    movement_date = Column(Integer, nullable=False, doc="Movement date YYYYMMDD")
    movement_type = Column(String(10), nullable=False, doc="REC, ISS, TRF, ADJ, RET")
    
    # Movement details
    from_location = Column(String(20))
    to_location = Column(String(20))
    reference_no = Column(String(50))
    
    # Transaction
    transaction_type = Column(String(10), doc="SO, PO, TO, etc")
    transaction_ref = Column(String(50))
    
    # Audit
    created_date = Column(Integer, nullable=False)
    created_by = Column(String(30), nullable=False)
    notes = Column(Text)
    
    # Relationships
    serial = relationship("SerialNumberRec", back_populates="movements")


class SerialAllocationRec(Base):
    """Serial number allocation record"""
    __tablename__ = "serial_allocations"
    
    allocation_id = Column(Integer, primary_key=True, autoincrement=True)
    serial_id = Column(Integer, ForeignKey("serial_numbers.serial_id"), nullable=False)
    
    # Allocation details
    allocation_type = Column(String(10), nullable=False, doc="SO, WO, etc")
    allocation_ref = Column(String(50), nullable=False)
    allocation_date = Column(Integer, nullable=False)
    
    # Status
    status = Column(String(1), default='A', doc="A=Active, C=Complete, X=Cancelled")
    
    # Relationships
    serial = relationship("SerialNumberRec", back_populates="allocations")


class SerialHistoryRec(Base):
    """Serial number history record"""
    __tablename__ = "serial_history"
    
    history_id = Column(Integer, primary_key=True, autoincrement=True)
    serial_number = Column(String(50), nullable=False)
    event_date = Column(Integer, nullable=False)
    event_type = Column(String(20), nullable=False)
    event_description = Column(Text)
    
    # References
    reference_type = Column(String(10))
    reference_no = Column(String(50))
    
    # Audit
    created_date = Column(Integer, nullable=False)
    created_by = Column(String(30), nullable=False)


# Lot/Batch Tracking Models
class LotNumberRec(Base):
    """Lot number master record"""
    __tablename__ = "lot_numbers"
    
    lot_id = Column(Integer, primary_key=True, autoincrement=True)
    lot_number = Column(String(50), nullable=False, doc="Lot/batch number")
    stock_code = Column(String(20), nullable=False, doc="Stock code")
    
    # Lot details
    manufacture_date = Column(Integer, doc="Manufacture date YYYYMMDD")
    expiry_date = Column(Integer, doc="Expiry date YYYYMMDD")
    best_before_date = Column(Integer, doc="Best before date YYYYMMDD")
    
    # Quantities
    original_qty = Column(Numeric(15, 4), nullable=False, doc="Original quantity")
    current_qty = Column(Numeric(15, 4), nullable=False, doc="Current quantity")
    allocated_qty = Column(Numeric(15, 4), default=0, doc="Allocated quantity")
    available_qty = Column(Numeric(15, 4), doc="Available quantity")
    
    # Status
    status = Column(String(1), default='A', doc="A=Active, E=Expired, Q=Quarantine, R=Released, X=Destroyed")
    
    # Quality
    quality_status = Column(String(1), default='P', doc="P=Pending, A=Approved, R=Rejected")
    quality_ref = Column(String(50), doc="Quality reference")
    
    # Source
    source_type = Column(String(10), doc="PO, WO, etc")
    source_ref = Column(String(50), doc="Source reference")
    supplier_lot = Column(String(50), doc="Supplier lot number")
    
    # Audit
    created_date = Column(Integer, nullable=False)
    created_by = Column(String(30), nullable=False)
    
    # Unique constraint
    __table_args__ = (
        UniqueConstraint('lot_number', 'stock_code', name='uq_lot_stock'),
    )
    
    # Relationships
    movements = relationship("LotMovementRec", back_populates="lot")
    allocations = relationship("LotAllocationRec", back_populates="lot")


class LotMovementRec(Base):
    """Lot movement record"""
    __tablename__ = "lot_movements"
    
    movement_id = Column(Integer, primary_key=True, autoincrement=True)
    lot_id = Column(Integer, ForeignKey("lot_numbers.lot_id"), nullable=False)
    movement_date = Column(Integer, nullable=False)
    movement_type = Column(String(10), nullable=False)
    
    # Movement details
    quantity = Column(Numeric(15, 4), nullable=False)
    from_location = Column(String(20))
    to_location = Column(String(20))
    reference_no = Column(String(50))
    
    # Audit
    created_date = Column(Integer, nullable=False)
    created_by = Column(String(30), nullable=False)
    
    # Relationships
    lot = relationship("LotNumberRec", back_populates="movements")


class LotAllocationRec(Base):
    """Lot allocation record"""
    __tablename__ = "lot_allocations"
    
    allocation_id = Column(Integer, primary_key=True, autoincrement=True)
    lot_id = Column(Integer, ForeignKey("lot_numbers.lot_id"), nullable=False)
    
    # Allocation details
    allocation_type = Column(String(10), nullable=False)
    allocation_ref = Column(String(50), nullable=False)
    allocation_date = Column(Integer, nullable=False)
    quantity = Column(Numeric(15, 4), nullable=False)
    
    # Status
    status = Column(String(1), default='A', doc="A=Active, C=Complete, X=Cancelled")
    
    # Relationships
    lot = relationship("LotNumberRec", back_populates="allocations")


class ExpiryDateRec(Base):
    """Expiry date tracking record"""
    __tablename__ = "expiry_dates"
    
    expiry_id = Column(Integer, primary_key=True, autoincrement=True)
    stock_code = Column(String(20), nullable=False)
    lot_number = Column(String(50), nullable=False)
    expiry_date = Column(Integer, nullable=False)
    
    # Quantities
    quantity = Column(Numeric(15, 4), nullable=False)
    location = Column(String(20))
    
    # Alerts
    alert_days = Column(Integer, default=30)
    alert_sent = Column(String(1), default='N')
    alert_date = Column(Integer)
    
    # Action
    action_taken = Column(String(20))
    action_date = Column(Integer)
    action_ref = Column(String(50))


class QualityTestRec(Base):
    """Quality test record"""
    __tablename__ = "quality_tests"
    
    test_id = Column(Integer, primary_key=True, autoincrement=True)
    lot_number = Column(String(50), nullable=False)
    stock_code = Column(String(20), nullable=False)
    test_date = Column(Integer, nullable=False)
    
    # Test details
    test_type = Column(String(20), nullable=False)
    test_ref = Column(String(50))
    
    # Results
    test_result = Column(String(1), nullable=False, doc="P=Pass, F=Fail")
    test_value = Column(Numeric(15, 4))
    test_notes = Column(Text)
    
    # Specifications
    spec_min = Column(Numeric(15, 4))
    spec_max = Column(Numeric(15, 4))
    spec_target = Column(Numeric(15, 4))
    
    # Audit
    tested_by = Column(String(30), nullable=False)
    approved_by = Column(String(30))
    approved_date = Column(Integer)


class StockAllocationRec(Base):
    """
    Stock Allocation Record
    
    Tracks stock allocations and reservations for orders, work orders, etc.
    """
    __tablename__ = "stock_allocation_rec"
    
    # Primary key
    alloc_id = Column(Integer, primary_key=True, autoincrement=True)
    
    # Stock reference
    alloc_stock_code = Column(String(20), nullable=False)
    alloc_warehouse = Column(String(10), nullable=False)
    alloc_location = Column(String(10))
    alloc_batch_no = Column(String(20))
    alloc_serial_no = Column(String(20))
    
    # Allocation details
    alloc_quantity = Column(Numeric(15, 3), nullable=False)
    alloc_unit = Column(String(5))
    alloc_date = Column(Integer, nullable=False)
    alloc_time = Column(Integer)
    
    # Reference information
    alloc_reference_type = Column(String(10), nullable=False)  # SO, WO, TO
    alloc_reference_no = Column(String(10), nullable=False)
    alloc_reference_line = Column(Integer)
    
    # Status
    alloc_status = Column(String(10), nullable=False)  # ALLOCATED, RESERVED, PICKED, RELEASED
    alloc_priority = Column(Integer)
    alloc_expiry_date = Column(Integer)
    
    # Audit
    alloc_created_by = Column(String(10))
    alloc_created_date = Column(Integer)
    alloc_released_by = Column(String(10))
    alloc_released_date = Column(Integer)
    
    # Notes
    alloc_notes = Column(Text)


class StockReservationRec(Base):
    """
    Stock Reservation Record
    
    Temporary stock reservations for quotes, planned orders, etc.
    """
    __tablename__ = "stock_reservation_rec"
    
    # Primary key
    reserve_id = Column(Integer, primary_key=True, autoincrement=True)
    
    # Stock reference
    reserve_stock_code = Column(String(20), nullable=False)
    reserve_warehouse = Column(String(10), nullable=False)
    reserve_quantity = Column(Numeric(15, 3), nullable=False)
    reserve_unit = Column(String(5))
    
    # Reservation details
    reserve_date = Column(Integer, nullable=False)
    reserve_expiry_date = Column(Integer, nullable=False)
    reserve_type = Column(String(10))  # QUOTE, PLANNED, MANUAL
    reserve_reference = Column(String(20))
    
    # Status
    reserve_status = Column(String(10))  # ACTIVE, EXPIRED, CONVERTED, CANCELLED
    reserve_converted_to = Column(String(20))  # Allocation reference if converted
    
    # Audit
    reserve_created_by = Column(String(10))
    reserve_notes = Column(Text)


class StockBackorderRec(Base):
    """
    Stock Backorder Record
    
    Tracks items on backorder awaiting stock
    """
    __tablename__ = "stock_backorder_rec"
    
    # Primary key
    backorder_id = Column(Integer, primary_key=True, autoincrement=True)
    
    # Stock reference
    backorder_stock_code = Column(String(20), nullable=False)
    backorder_warehouse = Column(String(10), nullable=False)
    backorder_quantity = Column(Numeric(15, 3), nullable=False)
    backorder_unit = Column(String(5))
    
    # Order reference
    backorder_order_type = Column(String(10))  # SO, WO
    backorder_order_no = Column(String(10), nullable=False)
    backorder_order_line = Column(Integer)
    backorder_customer_code = Column(String(10))
    
    # Dates
    backorder_date = Column(Integer, nullable=False)
    backorder_required_date = Column(Integer)
    backorder_promised_date = Column(Integer)
    
    # Status
    backorder_status = Column(String(10))  # PENDING, PARTIAL, FULFILLED, CANCELLED
    backorder_priority = Column(Integer)
    backorder_qty_fulfilled = Column(Numeric(15, 3))
    
    # Audit
    backorder_created_by = Column(String(10))
    backorder_updated_date = Column(Integer)
    backorder_notes = Column(Text)


class PickWaveRec(Base):
    """
    Pick Wave Record
    
    Tracks wave-based picking operations for efficient warehouse picking
    """
    __tablename__ = "pick_wave_rec"
    __table_args__ = {'schema': 'acas'}
    
    # Primary key
    wave_id = Column(Integer, primary_key=True, autoincrement=True, doc="Wave ID")
    wave_no = Column(String(20), unique=True, nullable=False, doc="Wave number")
    
    # Wave Information
    wave_warehouse = Column(String(10), nullable=False, doc="Warehouse code")
    wave_type = Column(String(20), default='STANDARD', doc="Wave type")
    wave_priority = Column(Integer, default=5, doc="Wave priority (1-10)")
    
    # Status and Dates
    wave_status = Column(String(20), default='PLANNING', doc="Wave status")
    wave_created_date = Column(Integer, nullable=False, doc="Created date (YYYYMMDD)")
    wave_created_time = Column(Integer, nullable=False, doc="Created time (HHMMSS)")
    wave_released_date = Column(Integer, doc="Released date (YYYYMMDD)")
    wave_released_time = Column(Integer, doc="Released time (HHMMSS)")
    wave_completed_date = Column(Integer, doc="Completed date (YYYYMMDD)")
    wave_completed_time = Column(Integer, doc="Completed time (HHMMSS)")
    
    # Planning Criteria
    wave_criteria = Column(Text, doc="Wave selection criteria (JSON)")
    wave_max_orders = Column(Integer, default=50, doc="Maximum orders per wave")
    wave_max_lines = Column(Integer, default=500, doc="Maximum lines per wave")
    wave_max_picks = Column(Integer, default=10, doc="Maximum pick lists per wave")
    
    # Totals and Metrics
    wave_total_orders = Column(Integer, default=0, doc="Total orders in wave")
    wave_total_lines = Column(Integer, default=0, doc="Total lines in wave")
    wave_pick_lists = Column(Integer, default=0, doc="Number of pick lists generated")
    wave_estimated_time = Column(Integer, default=0, doc="Estimated completion time (seconds)")
    wave_actual_time = Column(Integer, doc="Actual completion time (seconds)")
    
    # Performance Metrics
    wave_efficiency_pct = Column(Numeric(5, 2), doc="Wave efficiency percentage")
    wave_accuracy_pct = Column(Numeric(5, 2), doc="Wave picking accuracy percentage")
    wave_completed_picks = Column(Integer, default=0, doc="Completed pick lists")
    
    # Assignment and Control
    wave_supervisor = Column(String(30), doc="Wave supervisor")
    wave_team = Column(String(50), doc="Picking team assigned")
    wave_zone_restriction = Column(String(100), doc="Zone restrictions")
    
    # Notes and Comments
    wave_notes = Column(Text, doc="Wave notes")
    wave_completion_notes = Column(Text, doc="Completion notes")
    
    # Audit Trail
    wave_created_by = Column(String(30), nullable=False, doc="Created by user")
    wave_released_by = Column(String(30), doc="Released by user")
    wave_completed_by = Column(String(30), doc="Completed by user")
    
    # Table constraints
    __table_args__ = (
        CheckConstraint("wave_status IN ('PLANNING', 'RELEASED', 'IN_PROGRESS', 'COMPLETED', 'CANCELLED')", name='ck_wave_status'),
        CheckConstraint("wave_type IN ('STANDARD', 'EXPRESS', 'BULK', 'PRIORITY')", name='ck_wave_type'),
        CheckConstraint("wave_priority BETWEEN 1 AND 10", name='ck_wave_priority'),
        Index('idx_wave_warehouse', 'wave_warehouse'),
        Index('idx_wave_status', 'wave_status'),
        Index('idx_wave_created_date', 'wave_created_date'),
        {'schema': 'acas'}
    )


# Legacy model aliases for compatibility
StockMasterRec = StockRec
StockItem = StockRec
StockLocation = StockLocationRec
StockMovement = StockMovementRec


# Additional models required by various stock services
class StockTransferLineRec(Base):
    """Stock Transfer Line Record - Individual line items in transfers"""
    __tablename__ = "stock_transfer_lines"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    line_id = Column(Integer, primary_key=True, autoincrement=True, doc="Transfer line ID")
    transfer_id = Column(Integer, ForeignKey("acas.stock_transfers.transfer_id", ondelete="CASCADE"), nullable=False, doc="Transfer ID")
    line_number = Column(Integer, nullable=False, doc="Line number")
    
    # Stock Information
    stock_key = Column(String(30), nullable=False, doc="Stock item code")
    description = Column(String(100), doc="Item description")
    quantity = Column(Numeric(15, 3), nullable=False, doc="Transfer quantity")
    unit_of_measure = Column(String(6), default='EA', doc="Unit of measure")
    
    # Location Information
    from_location = Column(String(10), nullable=False, doc="From location")
    from_bin = Column(String(20), doc="From bin")
    to_location = Column(String(10), nullable=False, doc="To location")
    to_bin = Column(String(20), doc="To bin")
    
    # Lot/Serial Information
    lot_number = Column(String(30), doc="Lot number")
    serial_number = Column(String(50), doc="Serial number")
    
    # Cost Information
    unit_cost = Column(Numeric(15, 4), default=0.0000, doc="Unit cost")
    total_cost = Column(Numeric(12, 2), default=0.00, doc="Total cost")
    
    # Status
    line_status = Column(String(20), default='PENDING', doc="Line status")
    
    # Relationships
    transfer = relationship("StockTransferRec", foreign_keys=[transfer_id])


class TransferRequestRec(Base):
    """Transfer Request Record - Transfer requests between locations"""
    __tablename__ = "transfer_requests"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    request_id = Column(Integer, primary_key=True, autoincrement=True, doc="Request ID")
    request_number = Column(String(20), unique=True, nullable=False, doc="Request number")
    
    # Request Information
    request_date = Column(Integer, nullable=False, doc="Request date (YYYYMMDD)")
    requested_by = Column(String(30), nullable=False, doc="Requested by user")
    required_date = Column(Integer, doc="Required date (YYYYMMDD)")
    
    # Locations
    from_warehouse = Column(String(10), nullable=False, doc="From warehouse")
    to_warehouse = Column(String(10), nullable=False, doc="To warehouse")
    
    # Status and Control
    status = Column(String(20), default='PENDING', doc="Request status")
    priority = Column(String(10), default='NORMAL', doc="Request priority")
    
    # Approval
    approved_by = Column(String(30), doc="Approved by user")
    approved_date = Column(Integer, doc="Approval date (YYYYMMDD)")
    
    # Notes
    reason = Column(Text, doc="Transfer reason")
    notes = Column(Text, doc="Request notes")


class AbcClassificationRec(Base):
    """ABC Classification Record - ABC analysis results"""
    __tablename__ = "abc_classifications"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    classification_id = Column(Integer, primary_key=True, autoincrement=True, doc="Classification ID")
    
    # Stock and Analysis Information
    stock_key = Column(String(30), nullable=False, doc="Stock item code")
    analysis_period = Column(Integer, nullable=False, doc="Analysis period (YYYYMM)")
    warehouse = Column(String(10), doc="Warehouse code")
    
    # ABC Classification
    abc_class = Column(String(1), nullable=False, doc="ABC class: A, B, or C")
    classification_method = Column(String(20), default='VALUE', doc="Classification method")
    
    # Analysis Values
    annual_usage_quantity = Column(Numeric(15, 3), default=0.000, doc="Annual usage quantity")
    annual_usage_value = Column(Numeric(15, 2), default=0.00, doc="Annual usage value")
    percentage_of_total_value = Column(Numeric(5, 2), default=0.00, doc="Percentage of total value")
    cumulative_percentage = Column(Numeric(5, 2), default=0.00, doc="Cumulative percentage")
    
    # Ranking
    value_rank = Column(Integer, doc="Rank by value")
    quantity_rank = Column(Integer, doc="Rank by quantity")
    frequency_rank = Column(Integer, doc="Rank by frequency")
    
    # Analysis Date
    analysis_date = Column(Integer, nullable=False, doc="Analysis date (YYYYMMDD)")
    analyzed_by = Column(String(30), doc="Analyzed by user")


class AbcAnalysisRec(Base):
    """ABC Analysis Record - ABC analysis run header"""
    __tablename__ = "abc_analyses"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    analysis_id = Column(Integer, primary_key=True, autoincrement=True, doc="Analysis ID")
    analysis_number = Column(String(20), unique=True, nullable=False, doc="Analysis number")
    
    # Analysis Parameters
    analysis_date = Column(Integer, nullable=False, doc="Analysis date (YYYYMMDD)")
    analysis_period_from = Column(Integer, nullable=False, doc="Period from (YYYYMMDD)")
    analysis_period_to = Column(Integer, nullable=False, doc="Period to (YYYYMMDD)")
    warehouse = Column(String(10), doc="Warehouse code")
    
    # Classification Criteria
    class_a_percentage = Column(Numeric(5, 2), default=80.00, doc="Class A percentage")
    class_b_percentage = Column(Numeric(5, 2), default=15.00, doc="Class B percentage")
    class_c_percentage = Column(Numeric(5, 2), default=5.00, doc="Class C percentage")
    
    # Analysis Method
    classification_method = Column(String(20), default='VALUE', doc="Classification method")
    include_zero_usage = Column(String(1), default='N', doc="Include zero usage items")
    
    # Results Summary
    total_items_analyzed = Column(Integer, default=0, doc="Total items analyzed")
    class_a_items = Column(Integer, default=0, doc="Class A items")
    class_b_items = Column(Integer, default=0, doc="Class B items")
    class_c_items = Column(Integer, default=0, doc="Class C items")
    
    # Status
    status = Column(String(20), default='RUNNING', doc="Analysis status")
    
    # Audit
    created_by = Column(String(30), nullable=False, doc="Created by user")


class StockVelocityRec(Base):
    """Stock Velocity Record - Stock movement velocity analysis"""
    __tablename__ = "stock_velocity"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    velocity_id = Column(Integer, primary_key=True, autoincrement=True, doc="Velocity ID")
    
    # Stock Information
    stock_key = Column(String(30), nullable=False, doc="Stock item code")
    warehouse = Column(String(10), doc="Warehouse code")
    analysis_period = Column(Integer, nullable=False, doc="Analysis period (YYYYMM)")
    
    # Velocity Metrics
    turns_per_year = Column(Numeric(8, 2), default=0.00, doc="Inventory turns per year")
    days_of_supply = Column(Numeric(8, 2), default=0.00, doc="Days of supply")
    velocity_class = Column(String(1), doc="Velocity class: F=Fast, M=Medium, S=Slow")
    
    # Usage Statistics
    average_on_hand = Column(Numeric(15, 3), default=0.000, doc="Average quantity on hand")
    total_usage = Column(Numeric(15, 3), default=0.000, doc="Total usage in period")
    usage_frequency = Column(Integer, default=0, doc="Number of usage transactions")
    
    # Calculation Details
    calculation_date = Column(Integer, nullable=False, doc="Calculation date (YYYYMMDD)")
    calculated_by = Column(String(30), doc="Calculated by user")


class ReplenishmentRec(Base):
    """Replenishment Record - Stock replenishment suggestions"""
    __tablename__ = "replenishments"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    replenishment_id = Column(Integer, primary_key=True, autoincrement=True, doc="Replenishment ID")
    
    # Stock Information
    stock_key = Column(String(30), nullable=False, doc="Stock item code")
    warehouse = Column(String(10), nullable=False, doc="Warehouse code")
    location = Column(String(10), doc="Location code")
    
    # Replenishment Details
    current_quantity = Column(Numeric(15, 3), default=0.000, doc="Current quantity")
    suggested_quantity = Column(Numeric(15, 3), default=0.000, doc="Suggested replenishment quantity")
    reorder_point = Column(Numeric(15, 3), default=0.000, doc="Reorder point")
    max_quantity = Column(Numeric(15, 3), default=0.000, doc="Maximum quantity")
    
    # Replenishment Source
    source_type = Column(String(20), default='PURCHASE', doc="Source type: PURCHASE, TRANSFER, PRODUCTION")
    source_location = Column(String(10), doc="Source location for transfers")
    preferred_supplier = Column(String(10), doc="Preferred supplier")
    
    # Dates and Priority
    suggested_date = Column(Integer, nullable=False, doc="Suggestion date (YYYYMMDD)")
    required_date = Column(Integer, doc="Required date (YYYYMMDD)")
    priority = Column(String(10), default='NORMAL', doc="Replenishment priority")
    
    # Status
    status = Column(String(20), default='SUGGESTED', doc="Replenishment status")
    actioned_by = Column(String(30), doc="Actioned by user")
    actioned_date = Column(Integer, doc="Action date (YYYYMMDD)")
    
    # Action Details
    action_taken = Column(String(50), doc="Action taken")
    purchase_order_no = Column(String(20), doc="Purchase order number")
    transfer_no = Column(String(20), doc="Transfer number")


class ReplenishmentTaskRec(Base):
    """Replenishment Task Record - Individual replenishment tasks"""
    __tablename__ = "replenishment_tasks"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    task_id = Column(Integer, primary_key=True, autoincrement=True, doc="Task ID")
    task_number = Column(String(20), unique=True, nullable=False, doc="Task number")
    
    # Task Information
    stock_key = Column(String(30), nullable=False, doc="Stock item code")
    from_location = Column(String(10), nullable=False, doc="Source location")
    to_location = Column(String(10), nullable=False, doc="Destination location")
    
    # Quantity Information
    quantity_to_move = Column(Numeric(15, 3), nullable=False, doc="Quantity to move")
    quantity_moved = Column(Numeric(15, 3), default=0.000, doc="Quantity actually moved")
    
    # Task Details
    task_type = Column(String(20), default='REPLENISHMENT', doc="Task type")
    priority = Column(String(10), default='NORMAL', doc="Task priority")
    
    # Assignment
    assigned_to = Column(String(30), doc="Assigned to user")
    assigned_date = Column(Integer, doc="Assignment date (YYYYMMDD)")
    
    # Status and Completion
    status = Column(String(20), default='PENDING', doc="Task status")
    completed_by = Column(String(30), doc="Completed by user")
    completed_date = Column(Integer, doc="Completion date (YYYYMMDD)")
    
    # Equipment
    equipment_required = Column(String(50), doc="Required equipment")
    
    # Notes
    instructions = Column(Text, doc="Special instructions")
    completion_notes = Column(Text, doc="Completion notes")


# Quality Control Models
class QualityControlRec(Base):
    """Quality Control Record - Quality control parameters and standards"""
    __tablename__ = "quality_control"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    qc_id = Column(Integer, primary_key=True, autoincrement=True, doc="QC ID")
    
    # Stock Information
    stock_key = Column(String(30), nullable=False, doc="Stock item code")
    warehouse = Column(String(10), doc="Warehouse code")
    
    # Quality Standards
    quality_standard = Column(String(50), doc="Quality standard reference")
    inspection_required = Column(String(1), default='N', doc="Inspection required flag")
    sampling_percentage = Column(Numeric(5, 2), default=100.00, doc="Sampling percentage")
    
    # Test Parameters
    test_frequency = Column(String(20), default='EVERY_RECEIPT', doc="Test frequency")
    hold_period_days = Column(Integer, default=0, doc="Hold period in days")
    
    # Tolerances
    weight_tolerance_pct = Column(Numeric(5, 2), doc="Weight tolerance percentage")
    dimension_tolerance = Column(Numeric(8, 3), doc="Dimension tolerance")
    color_variance_allowed = Column(String(1), default='N', doc="Color variance allowed")
    
    # Status
    is_active = Column(String(1), default='Y', doc="Active QC record")
    effective_date = Column(Integer, doc="Effective date (YYYYMMDD)")
    
    # Audit
    created_by = Column(String(30), nullable=False, doc="Created by user")
    created_date = Column(Integer, nullable=False, doc="Created date (YYYYMMDD)")


class QualityResultRec(Base):
    """Quality Result Record - Quality test results"""
    __tablename__ = "quality_results"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    result_id = Column(Integer, primary_key=True, autoincrement=True, doc="Result ID")
    
    # Test Reference
    inspection_id = Column(Integer, doc="Related inspection ID")
    test_number = Column(String(20), doc="Test number")
    
    # Stock Information
    stock_key = Column(String(30), nullable=False, doc="Stock item code")
    lot_number = Column(String(30), doc="Lot number")
    batch_number = Column(String(30), doc="Batch number")
    
    # Test Details
    test_type = Column(String(50), nullable=False, doc="Test type")
    test_date = Column(Integer, nullable=False, doc="Test date (YYYYMMDD)")
    test_time = Column(Integer, doc="Test time (HHMMSS)")
    
    # Results
    result_status = Column(String(10), nullable=False, doc="Result status: PASS, FAIL, PENDING")
    actual_value = Column(Numeric(15, 4), doc="Actual test value")
    expected_value = Column(Numeric(15, 4), doc="Expected value")
    tolerance_min = Column(Numeric(15, 4), doc="Minimum tolerance")
    tolerance_max = Column(Numeric(15, 4), doc="Maximum tolerance")
    
    # Test Details
    tested_by = Column(String(30), nullable=False, doc="Tested by user")
    equipment_used = Column(String(50), doc="Equipment used")
    test_method = Column(String(50), doc="Test method")
    
    # Results and Actions
    notes = Column(Text, doc="Test notes")
    corrective_action = Column(Text, doc="Corrective action taken")
    
    # Approval
    approved_by = Column(String(30), doc="Approved by user")
    approved_date = Column(Integer, doc="Approval date (YYYYMMDD)")


class QuarantineRec(Base):
    """Quarantine Record - Quarantined stock items"""
    __tablename__ = "quarantine"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    quarantine_id = Column(Integer, primary_key=True, autoincrement=True, doc="Quarantine ID")
    quarantine_number = Column(String(20), unique=True, nullable=False, doc="Quarantine number")
    
    # Stock Information
    stock_key = Column(String(30), nullable=False, doc="Stock item code")
    warehouse = Column(String(10), nullable=False, doc="Warehouse code")
    location = Column(String(10), doc="Quarantine location")
    lot_number = Column(String(30), doc="Lot number")
    
    # Quantity Information
    quarantine_quantity = Column(Numeric(15, 3), nullable=False, doc="Quarantined quantity")
    original_quantity = Column(Numeric(15, 3), doc="Original quantity")
    
    # Quarantine Details
    quarantine_date = Column(Integer, nullable=False, doc="Quarantine date (YYYYMMDD)")
    quarantine_reason = Column(String(100), nullable=False, doc="Quarantine reason")
    quarantine_type = Column(String(20), default='QUALITY', doc="Quarantine type")
    
    # Status and Review
    status = Column(String(20), default='QUARANTINED', doc="Quarantine status")
    review_required_date = Column(Integer, doc="Review required date (YYYYMMDD)")
    reviewed_by = Column(String(30), doc="Reviewed by user")
    reviewed_date = Column(Integer, doc="Review date (YYYYMMDD)")
    
    # Resolution
    disposition = Column(String(50), doc="Final disposition")
    released_quantity = Column(Numeric(15, 3), doc="Released quantity")
    scrapped_quantity = Column(Numeric(15, 3), doc="Scrapped quantity")
    
    # References
    source_document = Column(String(20), doc="Source document")
    inspection_reference = Column(String(20), doc="Inspection reference")
    
    # Notes
    quarantine_notes = Column(Text, doc="Quarantine notes")
    disposition_notes = Column(Text, doc="Disposition notes")
    
    # Audit
    quarantined_by = Column(String(30), nullable=False, doc="Quarantined by user")
    released_by = Column(String(30), doc="Released by user")
    released_date = Column(Integer, doc="Release date (YYYYMMDD)")


class QualityInspectionRec(Base):
    """Quality Inspection Record - Quality inspection header"""
    __tablename__ = "quality_inspections"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    inspection_id = Column(Integer, primary_key=True, autoincrement=True, doc="Inspection ID")
    inspection_number = Column(String(20), unique=True, nullable=False, doc="Inspection number")
    
    # Stock Information
    stock_key = Column(String(30), nullable=False, doc="Stock item code")
    warehouse = Column(String(10), doc="Warehouse code")
    location = Column(String(10), doc="Location code")
    lot_number = Column(String(30), doc="Lot number")
    
    # Inspection Details
    inspection_date = Column(Integer, nullable=False, doc="Inspection date (YYYYMMDD)")
    inspection_type = Column(String(20), default='RECEIVING', doc="Inspection type")
    inspection_level = Column(String(20), default='NORMAL', doc="Inspection level")
    
    # Quantity Information
    quantity_inspected = Column(Numeric(15, 3), nullable=False, doc="Quantity inspected")
    sample_size = Column(Numeric(15, 3), doc="Sample size")
    
    # Results Summary
    overall_result = Column(String(10), default='PENDING', doc="Overall result: PASS, FAIL, PENDING")
    defects_found = Column(Integer, default=0, doc="Number of defects found")
    
    # Inspector Information
    inspector = Column(String(30), nullable=False, doc="Inspector name")
    inspection_standard = Column(String(50), doc="Inspection standard used")
    equipment_used = Column(String(100), doc="Equipment used")
    
    # Status and Approval
    status = Column(String(20), default='IN_PROGRESS', doc="Inspection status")
    approved_by = Column(String(30), doc="Approved by user")
    approved_date = Column(Integer, doc="Approval date (YYYYMMDD)")
    
    # References
    receipt_number = Column(String(20), doc="Receipt number")
    purchase_order = Column(String(20), doc="Purchase order number")
    supplier_code = Column(String(10), doc="Supplier code")
    
    # Notes
    inspection_notes = Column(Text, doc="Inspection notes")
    corrective_action = Column(Text, doc="Corrective action required")


class QualityDefectRec(Base):
    """Quality Defect Record - Individual defects found during inspection"""
    __tablename__ = "quality_defects"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    defect_id = Column(Integer, primary_key=True, autoincrement=True, doc="Defect ID")
    
    # Inspection Reference
    inspection_id = Column(Integer, ForeignKey("acas.quality_inspections.inspection_id", ondelete="CASCADE"), nullable=False, doc="Inspection ID")
    
    # Defect Information
    defect_code = Column(String(20), nullable=False, doc="Defect code")
    defect_description = Column(String(100), nullable=False, doc="Defect description")
    defect_category = Column(String(30), doc="Defect category")
    
    # Severity and Impact
    severity = Column(String(10), default='MINOR', doc="Defect severity: CRITICAL, MAJOR, MINOR")
    quantity_affected = Column(Numeric(15, 3), doc="Quantity affected by defect")
    
    # Location Details
    defect_location = Column(String(100), doc="Physical location of defect")
    component_affected = Column(String(50), doc="Component affected")
    
    # Measurements
    measured_value = Column(Numeric(15, 4), doc="Measured value")
    specification_value = Column(Numeric(15, 4), doc="Specification value")
    tolerance = Column(Numeric(15, 4), doc="Tolerance")
    
    # Detection Details
    detected_by = Column(String(30), doc="Detected by user")
    detection_method = Column(String(50), doc="Detection method")
    
    # Resolution
    corrective_action = Column(Text, doc="Corrective action taken")
    action_taken_by = Column(String(30), doc="Action taken by user")
    action_date = Column(Integer, doc="Action date (YYYYMMDD)")
    
    # Status
    defect_status = Column(String(20), default='OPEN', doc="Defect status")
    
    # Notes
    notes = Column(Text, doc="Additional notes")


# Returns Processing Models
class ReturnRec(Base):
    """Return Record - Customer/supplier returns"""
    __tablename__ = "returns"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    return_id = Column(Integer, primary_key=True, autoincrement=True, doc="Return ID")
    return_number = Column(String(20), unique=True, nullable=False, doc="Return number")
    
    # Return Type and Source
    return_type = Column(String(20), nullable=False, doc="Return type: CUSTOMER, SUPPLIER, INTERNAL")
    source_document = Column(String(20), doc="Source document number")
    original_invoice = Column(String(20), doc="Original invoice number")
    
    # Customer/Supplier Information
    customer_code = Column(String(10), doc="Customer code (for customer returns)")
    supplier_code = Column(String(10), doc="Supplier code (for supplier returns)")
    
    # Return Details
    return_date = Column(Integer, nullable=False, doc="Return date (YYYYMMDD)")
    return_reason = Column(String(100), nullable=False, doc="Return reason")
    warehouse = Column(String(10), nullable=False, doc="Warehouse code")
    
    # Status and Processing
    status = Column(String(20), default='RECEIVED', doc="Return status")
    authorization_required = Column(String(1), default='N', doc="Authorization required")
    authorized_by = Column(String(30), doc="Authorized by user")
    authorized_date = Column(Integer, doc="Authorization date (YYYYMMDD)")
    
    # Financial Information
    total_value = Column(Numeric(15, 2), default=0.00, doc="Total return value")
    credit_issued = Column(Numeric(15, 2), default=0.00, doc="Credit issued")
    restocking_fee = Column(Numeric(15, 2), default=0.00, doc="Restocking fee")
    
    # Processing Details
    received_by = Column(String(30), doc="Received by user")
    processed_by = Column(String(30), doc="Processed by user")
    processed_date = Column(Integer, doc="Processing date (YYYYMMDD)")
    
    # Notes
    return_notes = Column(Text, doc="Return notes")
    customer_complaint = Column(Text, doc="Customer complaint details")


class ReturnLineRec(Base):
    """Return Line Record - Individual items in a return"""
    __tablename__ = "return_lines"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    line_id = Column(Integer, primary_key=True, autoincrement=True, doc="Line ID")
    
    # Return Reference
    return_id = Column(Integer, ForeignKey("acas.returns.return_id", ondelete="CASCADE"), nullable=False, doc="Return ID")
    line_number = Column(Integer, nullable=False, doc="Line number")
    
    # Stock Information
    stock_key = Column(String(30), nullable=False, doc="Stock item code")
    description = Column(String(100), doc="Item description")
    lot_number = Column(String(30), doc="Lot number")
    serial_number = Column(String(50), doc="Serial number")
    
    # Quantity and Condition
    quantity_returned = Column(Numeric(15, 3), nullable=False, doc="Quantity returned")
    condition_code = Column(String(20), doc="Condition code")
    
    # Disposition
    disposition = Column(String(50), doc="Disposition action")
    quantity_restocked = Column(Numeric(15, 3), default=0.000, doc="Quantity restocked")
    quantity_scrapped = Column(Numeric(15, 3), default=0.000, doc="Quantity scrapped")
    quantity_repaired = Column(Numeric(15, 3), default=0.000, doc="Quantity repaired")
    
    # Financial
    unit_value = Column(Numeric(15, 4), default=0.0000, doc="Unit value")
    total_value = Column(Numeric(15, 2), default=0.00, doc="Total line value")
    credit_amount = Column(Numeric(15, 2), default=0.00, doc="Credit amount")
    
    # Status
    line_status = Column(String(20), default='PENDING', doc="Line status")
    
    # Notes
    line_notes = Column(Text, doc="Line notes")
    defect_description = Column(Text, doc="Defect description")
    
    # Relationships
    return_header = relationship("ReturnRec", foreign_keys=[return_id])


class ReturnAuthorizationRec(Base):
    """Return Authorization Record - Return authorizations"""
    __tablename__ = "return_authorizations"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    rma_id = Column(Integer, primary_key=True, autoincrement=True, doc="RMA ID")
    rma_number = Column(String(20), unique=True, nullable=False, doc="RMA number")
    
    # Customer Information
    customer_code = Column(String(10), nullable=False, doc="Customer code")
    contact_name = Column(String(50), doc="Contact name")
    contact_phone = Column(String(20), doc="Contact phone")
    contact_email = Column(String(100), doc="Contact email")
    
    # Authorization Details
    issue_date = Column(Integer, nullable=False, doc="Issue date (YYYYMMDD)")
    expiry_date = Column(Integer, doc="Expiry date (YYYYMMDD)")
    reason_code = Column(String(20), nullable=False, doc="Return reason code")
    reason_description = Column(String(200), doc="Reason description")
    
    # Reference Information
    original_invoice = Column(String(20), doc="Original invoice number")
    original_order = Column(String(20), doc="Original sales order")
    
    # Authorization Limits
    max_return_value = Column(Numeric(15, 2), doc="Maximum return value")
    restocking_fee_pct = Column(Numeric(5, 2), default=0.00, doc="Restocking fee percentage")
    
    # Status and Processing
    status = Column(String(20), default='ISSUED', doc="RMA status")
    used_flag = Column(String(1), default='N', doc="Used flag")
    return_id = Column(Integer, doc="Associated return ID")
    
    # Instructions
    return_instructions = Column(Text, doc="Return instructions")
    special_handling = Column(Text, doc="Special handling instructions")
    
    # Audit
    issued_by = Column(String(30), nullable=False, doc="Issued by user")
    approved_by = Column(String(30), doc="Approved by user")


class ReturnDispositionRec(Base):
    """Return Disposition Record - Disposition actions for returned items"""
    __tablename__ = "return_dispositions"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    disposition_id = Column(Integer, primary_key=True, autoincrement=True, doc="Disposition ID")
    
    # Return Reference
    return_line_id = Column(Integer, ForeignKey("acas.return_lines.line_id", ondelete="CASCADE"), nullable=False, doc="Return line ID")
    
    # Disposition Details
    disposition_code = Column(String(20), nullable=False, doc="Disposition code")
    disposition_description = Column(String(100), doc="Disposition description")
    disposition_date = Column(Integer, nullable=False, doc="Disposition date (YYYYMMDD)")
    
    # Quantity Information
    quantity_processed = Column(Numeric(15, 3), nullable=False, doc="Quantity processed")
    
    # Action Details
    action_taken = Column(String(100), doc="Action taken")
    location_moved_to = Column(String(10), doc="Location moved to")
    cost_center = Column(String(20), doc="Cost center for write-offs")
    
    # Financial Impact
    cost_impact = Column(Numeric(15, 2), default=0.00, doc="Cost impact")
    recovery_value = Column(Numeric(15, 2), default=0.00, doc="Recovery value")
    
    # References
    work_order_no = Column(String(20), doc="Work order number for repairs")
    scrap_certificate = Column(String(50), doc="Scrap certificate number")
    
    # Approval
    approved_by = Column(String(30), doc="Approved by user")
    approved_date = Column(Integer, doc="Approval date (YYYYMMDD)")
    
    # Processing
    processed_by = Column(String(30), nullable=False, doc="Processed by user")
    
    # Notes
    disposition_notes = Column(Text, doc="Disposition notes")
    
    # Relationships
    return_line = relationship("ReturnLineRec", foreign_keys=[return_line_id])


# Kit Management Models
class KitMasterRec(Base):
    """Kit Master Record - Kit/bundle definitions"""
    __tablename__ = "kit_master"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    kit_id = Column(Integer, primary_key=True, autoincrement=True, doc="Kit ID")
    kit_code = Column(String(30), unique=True, nullable=False, doc="Kit code")
    
    # Kit Information
    kit_description = Column(String(100), nullable=False, doc="Kit description")
    kit_type = Column(String(20), default='STANDARD', doc="Kit type")
    
    # Assembly Information
    assembly_time_minutes = Column(Integer, default=0, doc="Assembly time in minutes")
    disassembly_time_minutes = Column(Integer, default=0, doc="Disassembly time in minutes")
    assembly_location = Column(String(10), doc="Default assembly location")
    
    # Cost Information
    kit_cost = Column(Numeric(15, 4), default=0.0000, doc="Total kit cost")
    assembly_cost = Column(Numeric(15, 4), default=0.0000, doc="Assembly cost")
    labour_cost_per_hour = Column(Numeric(15, 4), default=0.0000, doc="Labour cost per hour")
    
    # Status and Control
    is_active = Column(String(1), default='Y', doc="Active kit flag")
    auto_assembly = Column(String(1), default='N', doc="Auto assembly flag")
    allow_partial_assembly = Column(String(1), default='N', doc="Allow partial assembly")
    
    # Inventory Control
    track_components = Column(String(1), default='Y', doc="Track component usage")
    explode_on_sale = Column(String(1), default='N', doc="Explode components on sale")
    
    # Notes
    assembly_instructions = Column(Text, doc="Assembly instructions")
    notes = Column(Text, doc="Kit notes")
    
    # Audit
    created_by = Column(String(30), nullable=False, doc="Created by user")
    created_date = Column(Integer, nullable=False, doc="Created date (YYYYMMDD)")
    
    # Relationships
    components = relationship("KitComponentRec", back_populates="kit", cascade="all, delete-orphan")


class KitComponentRec(Base):
    """Kit Component Record - Components within kits"""
    __tablename__ = "kit_components"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    component_id = Column(Integer, primary_key=True, autoincrement=True, doc="Component ID")
    
    # Kit Reference
    kit_id = Column(Integer, ForeignKey("acas.kit_master.kit_id", ondelete="CASCADE"), nullable=False, doc="Kit ID")
    component_sequence = Column(Integer, nullable=False, doc="Component sequence")
    
    # Component Information
    stock_key = Column(String(30), nullable=False, doc="Component stock code")
    description = Column(String(100), doc="Component description")
    
    # Quantity Information
    required_quantity = Column(Numeric(15, 3), nullable=False, doc="Required quantity")
    unit_of_measure = Column(String(6), default='EA', doc="Unit of measure")
    
    # Substitution
    substitute_allowed = Column(String(1), default='N', doc="Substitute allowed")
    substitute_stock_key = Column(String(30), doc="Substitute stock code")
    substitute_ratio = Column(Numeric(8, 4), default=1.0000, doc="Substitute ratio")
    
    # Cost and Control
    component_cost = Column(Numeric(15, 4), default=0.0000, doc="Component cost")
    is_critical = Column(String(1), default='N', doc="Critical component flag")
    is_phantom = Column(String(1), default='N', doc="Phantom component flag")
    
    # Assembly Information
    assembly_stage = Column(Integer, default=1, doc="Assembly stage")
    assembly_notes = Column(Text, doc="Assembly notes for this component")
    
    # Status
    is_active = Column(String(1), default='Y', doc="Active component flag")
    effective_date = Column(Integer, doc="Effective date (YYYYMMDD)")
    obsolete_date = Column(Integer, doc="Obsolete date (YYYYMMDD)")
    
    # Relationships
    kit = relationship("KitMasterRec", back_populates="components")


class KitAssemblyRec(Base):
    """Kit Assembly Record - Kit assembly transactions"""
    __tablename__ = "kit_assemblies"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    assembly_id = Column(Integer, primary_key=True, autoincrement=True, doc="Assembly ID")
    assembly_number = Column(String(20), unique=True, nullable=False, doc="Assembly number")
    
    # Kit Information
    kit_code = Column(String(30), nullable=False, doc="Kit code")
    quantity_assembled = Column(Numeric(15, 3), nullable=False, doc="Quantity assembled")
    
    # Assembly Details
    assembly_date = Column(Integer, nullable=False, doc="Assembly date (YYYYMMDD)")
    assembly_time = Column(Integer, doc="Assembly time (HHMMSS)")
    warehouse = Column(String(10), nullable=False, doc="Assembly warehouse")
    location = Column(String(10), doc="Assembly location")
    
    # Personnel
    assembled_by = Column(String(30), nullable=False, doc="Assembled by user")
    supervisor = Column(String(30), doc="Supervisor")
    
    # Time Tracking
    start_time = Column(Integer, doc="Start time (HHMMSS)")
    end_time = Column(Integer, doc="End time (HHMMSS)")
    actual_time_minutes = Column(Integer, doc="Actual assembly time in minutes")
    
    # Cost Information
    total_cost = Column(Numeric(15, 2), default=0.00, doc="Total assembly cost")
    labour_cost = Column(Numeric(15, 2), default=0.00, doc="Labour cost")
    material_cost = Column(Numeric(15, 2), default=0.00, doc="Material cost")
    
    # Status and Control
    status = Column(String(20), default='COMPLETED', doc="Assembly status")
    quality_check = Column(String(1), default='N', doc="Quality check performed")
    
    # References
    work_order_no = Column(String(20), doc="Work order number")
    batch_no = Column(String(20), doc="Assembly batch number")
    
    # Notes
    assembly_notes = Column(Text, doc="Assembly notes")
    variance_notes = Column(Text, doc="Variance notes")


class KitDisassemblyRec(Base):
    """Kit Disassembly Record - Kit disassembly transactions"""
    __tablename__ = "kit_disassemblies"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    disassembly_id = Column(Integer, primary_key=True, autoincrement=True, doc="Disassembly ID")
    disassembly_number = Column(String(20), unique=True, nullable=False, doc="Disassembly number")
    
    # Kit Information
    kit_code = Column(String(30), nullable=False, doc="Kit code")
    quantity_disassembled = Column(Numeric(15, 3), nullable=False, doc="Quantity disassembled")
    
    # Disassembly Details
    disassembly_date = Column(Integer, nullable=False, doc="Disassembly date (YYYYMMDD)")
    disassembly_reason = Column(String(100), doc="Disassembly reason")
    warehouse = Column(String(10), nullable=False, doc="Disassembly warehouse")
    location = Column(String(10), doc="Disassembly location")
    
    # Personnel
    disassembled_by = Column(String(30), nullable=False, doc="Disassembled by user")
    authorized_by = Column(String(30), doc="Authorized by user")
    
    # Recovery Information
    components_recovered = Column(Integer, default=0, doc="Number of components recovered")
    recovery_percentage = Column(Numeric(5, 2), doc="Recovery percentage")
    
    # Cost Information
    recovery_value = Column(Numeric(15, 2), default=0.00, doc="Recovery value")
    disassembly_cost = Column(Numeric(15, 2), default=0.00, doc="Disassembly cost")
    
    # Status
    status = Column(String(20), default='COMPLETED', doc="Disassembly status")
    
    # Notes
    disassembly_notes = Column(Text, doc="Disassembly notes")
    
    
# Demand Forecasting Models
class DemandForecastRec(Base):
    """Demand Forecast Record - Demand forecasting data"""
    __tablename__ = "demand_forecasts"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    forecast_id = Column(Integer, primary_key=True, autoincrement=True, doc="Forecast ID")
    
    # Stock and Period Information
    stock_key = Column(String(30), nullable=False, doc="Stock item code")
    warehouse = Column(String(10), doc="Warehouse code")
    forecast_period = Column(Integer, nullable=False, doc="Forecast period (YYYYMM)")
    
    # Forecast Data
    forecast_quantity = Column(Numeric(15, 3), default=0.000, doc="Forecast quantity")
    forecast_method = Column(String(20), doc="Forecast method used")
    confidence_level = Column(Numeric(5, 2), doc="Confidence level percentage")
    
    # Historical Base Data
    historical_average = Column(Numeric(15, 3), doc="Historical average")
    trend_factor = Column(Numeric(8, 4), doc="Trend factor")
    seasonal_factor = Column(Numeric(8, 4), doc="Seasonal factor")
    
    # Accuracy Tracking
    actual_demand = Column(Numeric(15, 3), doc="Actual demand")
    forecast_error = Column(Numeric(15, 3), doc="Forecast error")
    absolute_error = Column(Numeric(15, 3), doc="Absolute error")
    percentage_error = Column(Numeric(8, 2), doc="Percentage error")
    
    # Forecast Parameters
    forecast_horizon = Column(Integer, default=12, doc="Forecast horizon in months")
    alpha = Column(Numeric(4, 3), doc="Smoothing parameter alpha")
    beta = Column(Numeric(4, 3), doc="Trend smoothing parameter beta")
    gamma = Column(Numeric(4, 3), doc="Seasonal smoothing parameter gamma")
    
    # Status and Control
    forecast_status = Column(String(20), default='ACTIVE', doc="Forecast status")
    forecast_date = Column(Integer, nullable=False, doc="Forecast calculation date (YYYYMMDD)")
    calculated_by = Column(String(30), doc="Calculated by user")
    
    # Notes
    notes = Column(Text, doc="Forecast notes")


class ForecastModelRec(Base):
    """Forecast Model Record - Forecasting model definitions"""
    __tablename__ = "forecast_models"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    model_id = Column(Integer, primary_key=True, autoincrement=True, doc="Model ID")
    model_code = Column(String(20), unique=True, nullable=False, doc="Model code")
    
    # Model Information
    model_name = Column(String(50), nullable=False, doc="Model name")
    model_type = Column(String(20), nullable=False, doc="Model type")
    model_description = Column(Text, doc="Model description")
    
    # Model Parameters
    smoothing_alpha = Column(Numeric(4, 3), default=0.300, doc="Alpha smoothing parameter")
    trend_beta = Column(Numeric(4, 3), default=0.100, doc="Beta trend parameter")
    seasonal_gamma = Column(Numeric(4, 3), default=0.100, doc="Gamma seasonal parameter")
    
    # Data Requirements
    minimum_history_periods = Column(Integer, default=12, doc="Minimum history periods required")
    seasonal_periods = Column(Integer, default=12, doc="Number of seasonal periods")
    
    # Performance Metrics
    average_accuracy = Column(Numeric(5, 2), doc="Average accuracy percentage")
    last_calculated_date = Column(Integer, doc="Last calculation date (YYYYMMDD)")
    
    # Status
    is_active = Column(String(1), default='Y', doc="Active model flag")
    is_default = Column(String(1), default='N', doc="Default model flag")
    
    # Audit
    created_by = Column(String(30), nullable=False, doc="Created by user")
    created_date = Column(Integer, nullable=False, doc="Created date (YYYYMMDD)")


class ForecastAccuracyRec(Base):
    """Forecast Accuracy Record - Forecast accuracy tracking"""
    __tablename__ = "forecast_accuracy"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    accuracy_id = Column(Integer, primary_key=True, autoincrement=True, doc="Accuracy ID")
    
    # Reference Information
    stock_key = Column(String(30), nullable=False, doc="Stock item code")
    forecast_period = Column(Integer, nullable=False, doc="Forecast period (YYYYMM)")
    model_code = Column(String(20), doc="Model code used")
    
    # Accuracy Metrics
    mape = Column(Numeric(8, 2), doc="Mean Absolute Percentage Error")
    mad = Column(Numeric(15, 3), doc="Mean Absolute Deviation")
    mse = Column(Numeric(15, 3), doc="Mean Squared Error")
    bias = Column(Numeric(15, 3), doc="Forecast bias")
    
    # Tracking Scores
    tracking_signal = Column(Numeric(8, 2), doc="Tracking signal")
    r_squared = Column(Numeric(6, 4), doc="R-squared value")
    
    # Calculation Details
    calculation_date = Column(Integer, nullable=False, doc="Calculation date (YYYYMMDD)")
    periods_analyzed = Column(Integer, doc="Number of periods analyzed")


class SeasonalPatternRec(Base):
    """Seasonal Pattern Record - Seasonal demand patterns"""
    __tablename__ = "seasonal_patterns"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    pattern_id = Column(Integer, primary_key=True, autoincrement=True, doc="Pattern ID")
    
    # Pattern Information
    stock_key = Column(String(30), nullable=False, doc="Stock item code")
    pattern_year = Column(Integer, nullable=False, doc="Pattern year")
    period_month = Column(Integer, nullable=False, doc="Period month (1-12)")
    
    # Seasonal Factors
    seasonal_index = Column(Numeric(8, 4), default=1.0000, doc="Seasonal index")
    deseasonalized_demand = Column(Numeric(15, 3), doc="Deseasonalized demand")
    
    # Historical Data
    historical_demand = Column(Numeric(15, 3), doc="Historical demand for period")
    pattern_strength = Column(Numeric(5, 2), doc="Pattern strength percentage")
    
    # Calculation Details
    calculation_method = Column(String(20), doc="Calculation method")
    calculated_date = Column(Integer, doc="Calculation date (YYYYMMDD)")


# Cycle Counting Models
class CycleCountScheduleRec(Base):
    """Cycle Count Schedule Record - Cycle counting schedules"""
    __tablename__ = "cycle_count_schedules"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    schedule_id = Column(Integer, primary_key=True, autoincrement=True, doc="Schedule ID")
    schedule_code = Column(String(20), unique=True, nullable=False, doc="Schedule code")
    
    # Schedule Information
    schedule_name = Column(String(50), nullable=False, doc="Schedule name")
    schedule_description = Column(Text, doc="Schedule description")
    
    # Frequency and Timing
    count_frequency = Column(String(20), nullable=False, doc="Count frequency")
    frequency_days = Column(Integer, doc="Frequency in days")
    
    # Selection Criteria
    abc_class_filter = Column(String(10), doc="ABC class filter")
    velocity_class_filter = Column(String(10), doc="Velocity class filter")
    warehouse_filter = Column(String(10), doc="Warehouse filter")
    location_filter = Column(String(50), doc="Location filter")
    
    # Count Parameters
    count_tolerance_pct = Column(Numeric(5, 2), default=5.00, doc="Count tolerance percentage")
    recount_threshold_pct = Column(Numeric(5, 2), default=10.00, doc="Recount threshold percentage")
    
    # Status and Control
    is_active = Column(String(1), default='Y', doc="Active schedule flag")
    next_count_date = Column(Integer, doc="Next count date (YYYYMMDD)")
    last_count_date = Column(Integer, doc="Last count date (YYYYMMDD)")
    
    # Audit
    created_by = Column(String(30), nullable=False, doc="Created by user")
    created_date = Column(Integer, nullable=False, doc="Created date (YYYYMMDD)")


class CycleCountTaskRec(Base):
    """Cycle Count Task Record - Individual cycle count tasks"""
    __tablename__ = "cycle_count_tasks"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    task_id = Column(Integer, primary_key=True, autoincrement=True, doc="Task ID")
    task_number = Column(String(20), unique=True, nullable=False, doc="Task number")
    
    # Schedule Reference
    schedule_id = Column(Integer, ForeignKey("acas.cycle_count_schedules.schedule_id", ondelete="SET NULL"), doc="Schedule ID")
    
    # Count Information
    stock_key = Column(String(30), nullable=False, doc="Stock item code")
    warehouse = Column(String(10), nullable=False, doc="Warehouse code")
    location = Column(String(10), nullable=False, doc="Location code")
    bin_location = Column(String(20), doc="Bin location")
    
    # Count Details
    scheduled_date = Column(Integer, nullable=False, doc="Scheduled count date (YYYYMMDD)")
    count_type = Column(String(20), default='CYCLE', doc="Count type")
    priority = Column(String(10), default='NORMAL', doc="Count priority")
    
    # System Information
    system_quantity = Column(Numeric(15, 3), nullable=False, doc="System quantity")
    freeze_date = Column(Integer, doc="Freeze date (YYYYMMDD)")
    
    # Count Results
    counted_quantity = Column(Numeric(15, 3), doc="Counted quantity")
    variance_quantity = Column(Numeric(15, 3), doc="Variance quantity")
    variance_percentage = Column(Numeric(8, 4), doc="Variance percentage")
    
    # Assignment and Status
    assigned_to = Column(String(30), doc="Assigned to user")
    status = Column(String(20), default='PENDING', doc="Task status")
    
    # Count Execution
    counted_by = Column(String(30), doc="Counted by user")
    counted_date = Column(Integer, doc="Count date (YYYYMMDD)")
    count_time = Column(Integer, doc="Count time (HHMMSS)")
    
    # Approval and Adjustment
    requires_approval = Column(String(1), default='N', doc="Requires approval flag")
    approved_by = Column(String(30), doc="Approved by user")
    approved_date = Column(Integer, doc="Approval date (YYYYMMDD)")
    adjustment_created = Column(String(1), default='N', doc="Adjustment created flag")
    
    # Notes
    count_notes = Column(Text, doc="Count notes")
    variance_reason = Column(String(100), doc="Variance reason")


class CountVarianceRec(Base):
    """Count Variance Record - Count variance analysis"""
    __tablename__ = "count_variances"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    variance_id = Column(Integer, primary_key=True, autoincrement=True, doc="Variance ID")
    
    # Count Task Reference
    task_id = Column(Integer, ForeignKey("acas.cycle_count_tasks.task_id", ondelete="CASCADE"), nullable=False, doc="Task ID")
    
    # Variance Information
    variance_type = Column(String(20), nullable=False, doc="Variance type")
    variance_category = Column(String(30), doc="Variance category")
    
    # Variance Analysis
    root_cause = Column(String(100), doc="Root cause analysis")
    corrective_action = Column(Text, doc="Corrective action taken")
    
    # Financial Impact
    unit_cost = Column(Numeric(15, 4), doc="Unit cost")
    variance_value = Column(Numeric(15, 2), doc="Variance value")
    
    # Resolution
    resolved_by = Column(String(30), doc="Resolved by user")
    resolved_date = Column(Integer, doc="Resolution date (YYYYMMDD)")
    resolution_notes = Column(Text, doc="Resolution notes")
    
    # Status
    variance_status = Column(String(20), default='OPEN', doc="Variance status")
    
    # Relationships
    count_task = relationship("CycleCountTaskRec", foreign_keys=[task_id])