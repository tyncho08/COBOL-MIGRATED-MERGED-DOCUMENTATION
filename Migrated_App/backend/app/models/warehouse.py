"""
ACAS Warehouse Models
SQLAlchemy models for warehouse management and bin control
"""
from sqlalchemy import (
    Column, String, Integer, Numeric, DateTime, Text,
    ForeignKey, CheckConstraint, Index
)
from sqlalchemy.orm import relationship
from sqlalchemy.sql import func
from app.core.database import Base


class WarehouseRec(Base):
    """Warehouse Record - Warehouse master data"""
    __tablename__ = "warehouses"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    warehouse_id = Column(Integer, primary_key=True, autoincrement=True, doc="Warehouse ID")
    warehouse_code = Column(String(10), unique=True, nullable=False, doc="Warehouse code")
    
    # Warehouse Information
    warehouse_name = Column(String(50), nullable=False, doc="Warehouse name")
    warehouse_description = Column(String(100), doc="Warehouse description")
    warehouse_type = Column(String(20), default='STANDARD', doc="Warehouse type")
    
    # Address Information
    address_1 = Column(String(30), doc="Address line 1")
    address_2 = Column(String(30), doc="Address line 2")
    address_3 = Column(String(30), doc="Address line 3")
    city = Column(String(30), doc="City")
    postcode = Column(String(12), doc="Postcode")
    country = Column(String(24), doc="Country")
    
    # Contact Information
    contact_name = Column(String(50), doc="Contact person")
    phone = Column(String(20), doc="Phone number")
    fax = Column(String(20), doc="Fax number")
    email = Column(String(100), doc="Email address")
    
    # Physical Information
    total_area = Column(Numeric(12, 2), default=0.00, doc="Total warehouse area")
    storage_area = Column(Numeric(12, 2), default=0.00, doc="Storage area")
    height = Column(Numeric(8, 2), default=0.00, doc="Warehouse height")
    
    # Operational Information
    operating_hours = Column(String(50), doc="Operating hours")
    timezone = Column(String(50), default='UTC', doc="Timezone")
    
    # Status and Control
    is_active = Column(String(1), default='Y', doc="Active warehouse flag")
    is_default = Column(String(1), default='N', doc="Default warehouse flag")
    
    # Configuration
    allow_negative_stock = Column(String(1), default='N', doc="Allow negative stock flag")
    require_bin_control = Column(String(1), default='Y', doc="Require bin control flag")
    cycle_count_frequency = Column(Integer, default=0, doc="Cycle count frequency in days")
    
    # Integration
    external_warehouse_id = Column(String(20), doc="External system warehouse ID")
    
    # Notes
    notes = Column(Text, doc="Warehouse notes")
    
    # Audit Trail
    created_by = Column(String(30), nullable=False, doc="Created by user")
    created_at = Column(DateTime(timezone=True), server_default=func.current_timestamp())
    updated_at = Column(DateTime(timezone=True), server_default=func.current_timestamp(), onupdate=func.current_timestamp())
    
    # Relationships
    zones = relationship("WarehouseZoneRec", back_populates="warehouse", cascade="all, delete-orphan")
    bin_locations = relationship("BinLocationRec", back_populates="warehouse")


class WarehouseZoneRec(Base):
    """Warehouse Zone Record - Zones within warehouses"""
    __tablename__ = "warehouse_zones"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    zone_id = Column(Integer, primary_key=True, autoincrement=True, doc="Zone ID")
    
    # Zone Information
    warehouse_id = Column(Integer, ForeignKey("acas.warehouses.warehouse_id", ondelete="CASCADE"), nullable=False, doc="Warehouse ID")
    zone_code = Column(String(10), nullable=False, doc="Zone code")
    zone_name = Column(String(50), nullable=False, doc="Zone name")
    zone_type = Column(String(20), default='STORAGE', doc="Zone type")
    
    # Physical Layout
    start_aisle = Column(String(10), doc="Starting aisle")
    end_aisle = Column(String(10), doc="Ending aisle")
    start_bay = Column(String(10), doc="Starting bay")
    end_bay = Column(String(10), doc="Ending bay")
    start_level = Column(String(10), doc="Starting level")
    end_level = Column(String(10), doc="Ending level")
    
    # Capacity Information
    total_bins = Column(Integer, default=0, doc="Total bins in zone")
    available_bins = Column(Integer, default=0, doc="Available bins")
    max_capacity = Column(Numeric(15, 3), default=0.000, doc="Maximum capacity")
    current_utilization = Column(Numeric(15, 3), default=0.000, doc="Current utilization")
    
    # Configuration
    temperature_controlled = Column(String(1), default='N', doc="Temperature controlled flag")
    min_temperature = Column(Numeric(8, 2), doc="Minimum temperature")
    max_temperature = Column(Numeric(8, 2), doc="Maximum temperature")
    humidity_controlled = Column(String(1), default='N', doc="Humidity controlled flag")
    
    # Access Control
    security_level = Column(String(10), default='STANDARD', doc="Security level")
    restricted_access = Column(String(1), default='N', doc="Restricted access flag")
    
    # Status
    is_active = Column(String(1), default='Y', doc="Active zone flag")
    
    # Notes
    notes = Column(Text, doc="Zone notes")
    
    # Audit Trail
    created_at = Column(DateTime(timezone=True), server_default=func.current_timestamp())
    updated_at = Column(DateTime(timezone=True), server_default=func.current_timestamp(), onupdate=func.current_timestamp())
    
    # Relationships
    warehouse = relationship("WarehouseRec", back_populates="zones")
    bin_locations = relationship("BinLocationRec", back_populates="zone")


class BinTypeRec(Base):
    """Bin Type Record - Types of storage bins"""
    __tablename__ = "bin_types"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    bin_type_id = Column(Integer, primary_key=True, autoincrement=True, doc="Bin type ID")
    bin_type_code = Column(String(10), unique=True, nullable=False, doc="Bin type code")
    
    # Type Information
    bin_type_name = Column(String(50), nullable=False, doc="Bin type name")
    bin_type_description = Column(String(100), doc="Bin type description")
    
    # Physical Specifications
    length = Column(Numeric(8, 2), doc="Bin length")
    width = Column(Numeric(8, 2), doc="Bin width")
    height = Column(Numeric(8, 2), doc="Bin height")
    volume = Column(Numeric(12, 3), doc="Bin volume")
    weight_capacity = Column(Numeric(12, 2), doc="Weight capacity")
    
    # Storage Configuration
    allows_mixed_items = Column(String(1), default='Y', doc="Allows mixed items flag")
    requires_lot_control = Column(String(1), default='N', doc="Requires lot control flag")
    stackable = Column(String(1), default='Y', doc="Stackable flag")
    
    # Special Handling
    hazmat_approved = Column(String(1), default='N', doc="Hazmat approved flag")
    temperature_controlled = Column(String(1), default='N', doc="Temperature controlled flag")
    
    # Status
    is_active = Column(String(1), default='Y', doc="Active bin type flag")
    
    # Audit Trail
    created_at = Column(DateTime(timezone=True), server_default=func.current_timestamp())
    updated_at = Column(DateTime(timezone=True), server_default=func.current_timestamp(), onupdate=func.current_timestamp())
    
    # Relationships
    bin_locations = relationship("BinLocationRec", back_populates="bin_type")


class BinLocationRec(Base):
    """Bin Location Record - Individual bin locations"""
    __tablename__ = "bin_locations"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    bin_location_id = Column(Integer, primary_key=True, autoincrement=True, doc="Bin location ID")
    bin_code = Column(String(20), unique=True, nullable=False, doc="Bin location code")
    
    # Location Hierarchy
    warehouse_id = Column(Integer, ForeignKey("acas.warehouses.warehouse_id", ondelete="RESTRICT"), nullable=False, doc="Warehouse ID")
    zone_id = Column(Integer, ForeignKey("acas.warehouse_zones.zone_id", ondelete="RESTRICT"), doc="Zone ID")
    bin_type_id = Column(Integer, ForeignKey("acas.bin_types.bin_type_id", ondelete="RESTRICT"), doc="Bin type ID")
    
    # Physical Location
    aisle = Column(String(10), doc="Aisle")
    bay = Column(String(10), doc="Bay")
    level = Column(String(10), doc="Level")
    position = Column(String(10), doc="Position")
    
    # Capacity Information
    max_weight = Column(Numeric(12, 2), default=0.00, doc="Maximum weight capacity")
    max_volume = Column(Numeric(12, 3), default=0.000, doc="Maximum volume capacity")
    current_weight = Column(Numeric(12, 2), default=0.00, doc="Current weight")
    current_volume = Column(Numeric(12, 3), default=0.000, doc="Current volume")
    
    # Configuration
    pick_sequence = Column(Integer, doc="Pick sequence number")
    replenishment_sequence = Column(Integer, doc="Replenishment sequence")
    
    # Special Attributes
    is_pick_face = Column(String(1), default='N', doc="Pick face flag")
    is_bulk_location = Column(String(1), default='N', doc="Bulk location flag")
    is_receiving_location = Column(String(1), default='N', doc="Receiving location flag")
    is_shipping_location = Column(String(1), default='N', doc="Shipping location flag")
    
    # Environmental Control
    temperature_zone = Column(String(10), doc="Temperature zone")
    climate_controlled = Column(String(1), default='N', doc="Climate controlled flag")
    
    # Access Control
    requires_equipment = Column(String(20), doc="Required equipment")
    height_restriction = Column(Numeric(8, 2), doc="Height restriction")
    
    # Status
    bin_status = Column(String(20), default='AVAILABLE', doc="Bin status")
    is_blocked = Column(String(1), default='N', doc="Blocked flag")
    block_reason = Column(String(100), doc="Block reason")
    
    # Tracking
    last_movement_date = Column(Integer, doc="Last movement date (YYYYMMDD)")
    last_counted_date = Column(Integer, doc="Last count date (YYYYMMDD)")
    
    # Notes
    notes = Column(Text, doc="Bin location notes")
    
    # Audit Trail
    created_at = Column(DateTime(timezone=True), server_default=func.current_timestamp())
    updated_at = Column(DateTime(timezone=True), server_default=func.current_timestamp(), onupdate=func.current_timestamp())
    
    # Relationships
    warehouse = relationship("WarehouseRec", back_populates="bin_locations")
    zone = relationship("WarehouseZoneRec", back_populates="bin_locations")
    bin_type = relationship("BinTypeRec", back_populates="bin_locations")
    capacities = relationship("BinCapacityRec", back_populates="bin_location", cascade="all, delete-orphan")


class BinCapacityRec(Base):
    """Bin Capacity Record - Item-specific capacity constraints"""
    __tablename__ = "bin_capacities"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    capacity_id = Column(Integer, primary_key=True, autoincrement=True, doc="Capacity ID")
    
    # References
    bin_location_id = Column(Integer, ForeignKey("acas.bin_locations.bin_location_id", ondelete="CASCADE"), nullable=False, doc="Bin location ID")
    stock_key = Column(String(30), doc="Stock item code (optional)")
    item_category = Column(String(20), doc="Item category")
    
    # Capacity Constraints
    max_quantity = Column(Numeric(15, 3), default=0.000, doc="Maximum quantity")
    max_weight = Column(Numeric(12, 2), default=0.00, doc="Maximum weight")
    max_volume = Column(Numeric(12, 3), default=0.000, doc="Maximum volume")
    
    # Stacking Rules
    max_stack_height = Column(Integer, default=1, doc="Maximum stack height")
    stack_weight_limit = Column(Numeric(12, 2), doc="Stack weight limit")
    
    # Special Handling
    requires_equipment = Column(String(20), doc="Required handling equipment")
    segregation_code = Column(String(10), doc="Segregation code")
    
    # Status
    is_active = Column(String(1), default='Y', doc="Active capacity rule flag")
    effective_date = Column(Integer, doc="Effective date (YYYYMMDD)")
    expiry_date = Column(Integer, doc="Expiry date (YYYYMMDD)")
    
    # Notes
    notes = Column(Text, doc="Capacity rule notes")
    
    # Audit Trail
    created_at = Column(DateTime(timezone=True), server_default=func.current_timestamp())
    updated_at = Column(DateTime(timezone=True), server_default=func.current_timestamp(), onupdate=func.current_timestamp())
    
    # Relationships
    bin_location = relationship("BinLocationRec", back_populates="capacities")


class PickRouteRec(Base):
    """
    Pick Route Record - Optimized picking routes through warehouse
    """
    __tablename__ = "pick_routes"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    route_id = Column(Integer, primary_key=True, autoincrement=True, doc="Route ID")
    route_code = Column(String(10), unique=True, nullable=False, doc="Route code")
    
    # Route Information
    warehouse_id = Column(Integer, ForeignKey("acas.warehouses.warehouse_id", ondelete="RESTRICT"), nullable=False, doc="Warehouse ID")
    route_name = Column(String(50), nullable=False, doc="Route name")
    route_description = Column(String(100), doc="Route description")
    route_type = Column(String(20), default='PICKING', doc="Route type")
    
    # Route Configuration
    start_location = Column(String(20), doc="Starting location")
    end_location = Column(String(20), doc="Ending location")
    route_direction = Column(String(10), default='FORWARD', doc="Route direction: FORWARD, REVERSE, SERPENTINE")
    
    # Zone Coverage
    zones_covered = Column(Text, doc="Zones covered by this route (JSON array)")
    primary_zone = Column(String(10), doc="Primary zone for this route")
    
    # Route Metrics
    total_distance = Column(Numeric(10, 2), default=0.00, doc="Total route distance")
    estimated_time = Column(Integer, default=0, doc="Estimated completion time (seconds)")
    max_capacity = Column(Numeric(12, 2), default=0.00, doc="Maximum weight capacity")
    max_volume = Column(Numeric(12, 3), default=0.000, doc="Maximum volume capacity")
    
    # Optimization Settings
    optimize_for = Column(String(20), default='DISTANCE', doc="Optimization criteria: DISTANCE, TIME, SEQUENCE")
    allow_zone_skip = Column(String(1), default='N', doc="Allow zone skipping")
    require_sequence = Column(String(1), default='Y', doc="Require strict sequence")
    
    # Equipment Requirements
    equipment_required = Column(String(50), doc="Required picking equipment")
    vehicle_type = Column(String(20), doc="Required vehicle type")
    special_requirements = Column(Text, doc="Special requirements")
    
    # Performance Tracking
    avg_pick_time = Column(Integer, doc="Average pick time per location (seconds)")
    picks_per_hour = Column(Numeric(8, 2), doc="Average picks per hour")
    accuracy_rate = Column(Numeric(5, 2), doc="Picking accuracy rate percentage")
    
    # Status and Control
    is_active = Column(String(1), default='Y', doc="Active route flag")
    is_default = Column(String(1), default='N', doc="Default route flag")
    priority = Column(Integer, default=5, doc="Route priority (1-10)")
    
    # Shift and Time Restrictions
    shift_restrictions = Column(String(50), doc="Shift restrictions")
    time_restrictions = Column(String(100), doc="Time-based restrictions")
    
    # Notes
    notes = Column(Text, doc="Route notes and instructions")
    
    # Audit Trail
    created_by = Column(String(30), nullable=False, doc="Created by user")
    created_at = Column(DateTime(timezone=True), server_default=func.current_timestamp())
    updated_at = Column(DateTime(timezone=True), server_default=func.current_timestamp(), onupdate=func.current_timestamp())
    
    # Relationships
    warehouse = relationship("WarehouseRec", foreign_keys=[warehouse_id])
    route_steps = relationship("PickRouteStepRec", back_populates="route", cascade="all, delete-orphan")
    
    # Table constraints
    __table_args__ = (
        CheckConstraint("route_type IN ('PICKING', 'REPLENISHMENT', 'CYCLE_COUNT', 'RECEIVING')", name='ck_route_type'),
        CheckConstraint("route_direction IN ('FORWARD', 'REVERSE', 'SERPENTINE')", name='ck_route_direction'),
        CheckConstraint("optimize_for IN ('DISTANCE', 'TIME', 'SEQUENCE')", name='ck_route_optimize'),
        CheckConstraint("is_active IN ('Y', 'N')", name='ck_route_active'),
        CheckConstraint("is_default IN ('Y', 'N')", name='ck_route_default'),
        CheckConstraint("priority BETWEEN 1 AND 10", name='ck_route_priority'),
        Index('idx_route_warehouse', 'warehouse_id'),
        Index('idx_route_type', 'route_type'),
        Index('idx_route_active', 'is_active'),
        {'schema': 'acas'}
    )


class PickRouteStepRec(Base):
    """
    Pick Route Step Record - Individual steps within a picking route
    """
    __tablename__ = "pick_route_steps"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    step_id = Column(Integer, primary_key=True, autoincrement=True, doc="Step ID")
    
    # Route Reference
    route_id = Column(Integer, ForeignKey("acas.pick_routes.route_id", ondelete="CASCADE"), nullable=False, doc="Route ID")
    step_sequence = Column(Integer, nullable=False, doc="Step sequence number")
    
    # Location Information
    location_code = Column(String(20), nullable=False, doc="Location code")
    zone_code = Column(String(10), doc="Zone code")
    aisle = Column(String(10), doc="Aisle")
    bay = Column(String(10), doc="Bay")
    level = Column(String(10), doc="Level")
    
    # Step Details
    step_type = Column(String(20), default='PICK', doc="Step type")
    step_description = Column(String(100), doc="Step description")
    
    # Distance and Time
    distance_from_previous = Column(Numeric(8, 2), default=0.00, doc="Distance from previous step")
    estimated_time = Column(Integer, default=0, doc="Estimated time at this step (seconds)")
    travel_time = Column(Integer, default=0, doc="Travel time from previous step (seconds)")
    
    # Directional Information
    direction_from_previous = Column(String(20), doc="Direction from previous step")
    turn_type = Column(String(20), doc="Turn type: STRAIGHT, LEFT, RIGHT, U_TURN")
    
    # Equipment and Access
    equipment_change = Column(String(1), default='N', doc="Equipment change required")
    access_restrictions = Column(String(100), doc="Access restrictions")
    
    # Special Instructions
    special_instructions = Column(Text, doc="Special picking instructions")
    hazmat_flag = Column(String(1), default='N', doc="Hazmat handling required")
    
    # Performance Metrics
    avg_pick_time = Column(Integer, doc="Average pick time at this location")
    complexity_rating = Column(Integer, doc="Complexity rating (1-10)")
    
    # Status
    is_active = Column(String(1), default='Y', doc="Active step flag")
    is_mandatory = Column(String(1), default='Y', doc="Mandatory step flag")
    
    # Notes
    notes = Column(Text, doc="Step notes")
    
    # Audit Trail
    created_at = Column(DateTime(timezone=True), server_default=func.current_timestamp())
    updated_at = Column(DateTime(timezone=True), server_default=func.current_timestamp(), onupdate=func.current_timestamp())
    
    # Relationships
    route = relationship("PickRouteRec", back_populates="route_steps")
    
    # Table constraints
    __table_args__ = (
        CheckConstraint("step_type IN ('PICK', 'CHECKPOINT', 'EQUIPMENT_CHANGE', 'BREAK_POINT')", name='ck_step_type'),
        CheckConstraint("turn_type IN ('STRAIGHT', 'LEFT', 'RIGHT', 'U_TURN')", name='ck_turn_type'),
        CheckConstraint("equipment_change IN ('Y', 'N')", name='ck_equipment_change'),
        CheckConstraint("hazmat_flag IN ('Y', 'N')", name='ck_hazmat_flag'),
        CheckConstraint("is_active IN ('Y', 'N')", name='ck_step_active'),
        CheckConstraint("is_mandatory IN ('Y', 'N')", name='ck_step_mandatory'),
        CheckConstraint("complexity_rating BETWEEN 1 AND 10", name='ck_complexity_rating'),
        Index('idx_route_step_route', 'route_id'),
        Index('idx_route_step_sequence', 'step_sequence'),
        Index('idx_route_step_location', 'location_code'),
        {'schema': 'acas'}
    )