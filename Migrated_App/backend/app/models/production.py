"""
Production Models - Manufacturing and Work Order Management
Handles work orders, bill of materials, production planning, and shop floor control
"""
from sqlalchemy import Column, String, Integer, Numeric, Text, ForeignKey, Boolean, Date, DateTime
from sqlalchemy.orm import relationship
from app.core.database import Base


class WorkOrderRec(Base):
    """Work Order/Manufacturing Order Master Record"""
    __tablename__ = "work_order_rec"
    
    wo_no = Column(String(10), primary_key=True)
    wo_type = Column(String(10))  # PRODUCTION, ASSEMBLY, REWORK, REPAIR
    wo_status = Column(String(20))  # PLANNED, RELEASED, IN_PROGRESS, COMPLETED, CANCELLED
    wo_priority = Column(Integer)  # 1-5 (1=highest)
    wo_stock_code = Column(String(20))  # Product to manufacture
    wo_description = Column(String(50))
    wo_quantity_required = Column(Numeric(15, 3))
    wo_quantity_completed = Column(Numeric(15, 3))
    wo_quantity_scrapped = Column(Numeric(15, 3))
    wo_unit_of_measure = Column(String(5))
    wo_due_date = Column(Integer)
    wo_start_date_planned = Column(Integer)
    wo_start_date_actual = Column(Integer)
    wo_completion_date_planned = Column(Integer)
    wo_completion_date_actual = Column(Integer)
    wo_warehouse = Column(String(10))
    wo_work_center = Column(String(10))
    wo_routing_code = Column(String(10))
    wo_bom_version = Column(String(10))
    wo_batch_no = Column(String(20))
    wo_serial_no_from = Column(String(20))
    wo_serial_no_to = Column(String(20))
    wo_customer_order = Column(String(10))
    wo_customer_code = Column(String(10))
    wo_project_code = Column(String(10))
    wo_standard_cost = Column(Numeric(15, 2))
    wo_actual_cost = Column(Numeric(15, 2))
    wo_labor_hours_std = Column(Numeric(10, 2))
    wo_labor_hours_actual = Column(Numeric(10, 2))
    wo_machine_hours_std = Column(Numeric(10, 2))
    wo_machine_hours_actual = Column(Numeric(10, 2))
    wo_setup_hours_std = Column(Numeric(10, 2))
    wo_setup_hours_actual = Column(Numeric(10, 2))
    wo_notes = Column(Text)
    wo_created_date = Column(Integer)
    wo_created_by = Column(String(10))
    wo_released_date = Column(Integer)
    wo_released_by = Column(String(10))
    wo_closed_date = Column(Integer)
    wo_closed_by = Column(String(10))


class WorkOrderLineRec(Base):
    """Work Order Line/Component Requirements"""
    __tablename__ = "work_order_line_rec"
    
    wol_wo_no = Column(String(10), ForeignKey('work_order_rec.wo_no'), primary_key=True)
    wol_line_no = Column(Integer, primary_key=True)
    wol_type = Column(String(10))  # MATERIAL, LABOR, OVERHEAD
    wol_stock_code = Column(String(20))
    wol_description = Column(String(50))
    wol_quantity_required = Column(Numeric(15, 3))
    wol_quantity_issued = Column(Numeric(15, 3))
    wol_quantity_returned = Column(Numeric(15, 3))
    wol_unit_of_measure = Column(String(5))
    wol_unit_cost = Column(Numeric(15, 4))
    wol_extended_cost = Column(Numeric(15, 2))
    wol_warehouse = Column(String(10))
    wol_location = Column(String(10))
    wol_batch_no = Column(String(20))
    wol_serial_no = Column(String(20))
    wol_operation_seq = Column(Integer)
    wol_scrap_factor = Column(Numeric(5, 2))
    wol_issue_method = Column(String(10))  # BACKFLUSH, MANUAL, AUTO
    wol_issue_date = Column(Integer)
    wol_status = Column(String(10))  # PLANNED, ALLOCATED, ISSUED, COMPLETE
    
    # Relationship
    work_order = relationship("WorkOrderRec", backref="lines")


class BillOfMaterialsRec(Base):
    """Bill of Materials Master Record"""
    __tablename__ = "bom_rec"
    
    bom_parent_code = Column(String(20), primary_key=True)
    bom_version = Column(String(10), primary_key=True)
    bom_description = Column(String(50))
    bom_status = Column(String(10))  # ACTIVE, INACTIVE, PENDING
    bom_effective_date = Column(Integer)
    bom_expiry_date = Column(Integer)
    bom_quantity_basis = Column(Numeric(15, 3))  # Quantity this BOM produces
    bom_unit_of_measure = Column(String(5))
    bom_type = Column(String(10))  # STANDARD, PHANTOM, PLANNING
    bom_revision_notes = Column(Text)
    bom_approved_by = Column(String(10))
    bom_approved_date = Column(Integer)
    bom_created_date = Column(Integer)
    bom_created_by = Column(String(10))


class BillOfMaterialsLineRec(Base):
    """Bill of Materials Component Lines"""
    __tablename__ = "bom_line_rec"
    
    boml_parent_code = Column(String(20), primary_key=True)
    boml_version = Column(String(10), primary_key=True)
    boml_line_no = Column(Integer, primary_key=True)
    boml_component_code = Column(String(20))
    boml_component_desc = Column(String(50))
    boml_quantity_per = Column(Numeric(15, 6))
    boml_unit_of_measure = Column(String(5))
    boml_scrap_percent = Column(Numeric(5, 2))
    boml_operation_seq = Column(Integer)
    boml_effective_date = Column(Integer)
    boml_expiry_date = Column(Integer)
    boml_component_type = Column(String(10))  # MATERIAL, PHANTOM, BYPRODUCT
    boml_supply_type = Column(String(10))  # STOCK, PURCHASE, MANUFACTURE
    boml_reference_designator = Column(String(50))
    boml_critical_component = Column(Boolean, default=False)
    boml_notes = Column(Text)
    
    # Note: Composite foreign key relationships can be established at the database level
    # or handled via application logic


class WorkCenterRec(Base):
    """Work Center/Resource Master Record"""
    __tablename__ = "work_center_rec"
    
    wc_code = Column(String(10), primary_key=True)
    wc_description = Column(String(50))
    wc_type = Column(String(20))  # MACHINE, LABOR, BOTH
    wc_department = Column(String(10))
    wc_capacity_hours = Column(Numeric(10, 2))  # Per day
    wc_efficiency_percent = Column(Numeric(5, 2))
    wc_utilization_percent = Column(Numeric(5, 2))
    wc_queue_hours = Column(Numeric(10, 2))
    wc_setup_hours = Column(Numeric(10, 2))
    wc_labor_rate = Column(Numeric(10, 2))
    wc_overhead_rate = Column(Numeric(10, 2))
    wc_machine_rate = Column(Numeric(10, 2))
    wc_status = Column(String(10))  # ACTIVE, INACTIVE, MAINTENANCE
    wc_calendar_code = Column(String(10))
    wc_created_date = Column(Integer)
    wc_created_by = Column(String(10))


class RoutingRec(Base):
    """Production Routing Master Record"""
    __tablename__ = "routing_rec"
    
    routing_code = Column(String(10), primary_key=True)
    routing_stock_code = Column(String(20), primary_key=True)
    routing_version = Column(String(10), primary_key=True)
    routing_description = Column(String(50))
    routing_status = Column(String(10))  # ACTIVE, INACTIVE, PENDING
    routing_effective_date = Column(Integer)
    routing_expiry_date = Column(Integer)
    routing_approved_by = Column(String(10))
    routing_approved_date = Column(Integer)


class RoutingOperationRec(Base):
    """Routing Operation Details"""
    __tablename__ = "routing_operation_rec"
    
    ro_routing_code = Column(String(10), primary_key=True)
    ro_stock_code = Column(String(20), primary_key=True)
    ro_version = Column(String(10), primary_key=True)
    ro_operation_seq = Column(Integer, primary_key=True)
    ro_operation_code = Column(String(10))
    ro_description = Column(String(50))
    ro_work_center = Column(String(10), ForeignKey('work_center_rec.wc_code'))
    ro_setup_hours = Column(Numeric(10, 2))
    ro_run_hours = Column(Numeric(10, 2))
    ro_machine_hours = Column(Numeric(10, 2))
    ro_labor_hours = Column(Numeric(10, 2))
    ro_queue_hours = Column(Numeric(10, 2))
    ro_move_hours = Column(Numeric(10, 2))
    ro_overlap_percent = Column(Numeric(5, 2))
    ro_scrap_percent = Column(Numeric(5, 2))
    ro_inspection_required = Column(Boolean, default=False)
    ro_milestone_operation = Column(Boolean, default=False)
    ro_subcontract_operation = Column(Boolean, default=False)
    ro_instructions = Column(Text)
    
    # Note: Composite foreign key relationships can be established at the database level
    # or handled via application logic
    
    # Relationship
    work_center = relationship("WorkCenterRec")


class ProductionScheduleRec(Base):
    """Production Schedule/Planning Record"""
    __tablename__ = "production_schedule_rec"
    
    ps_id = Column(Integer, primary_key=True, autoincrement=True)
    ps_date = Column(Integer)
    ps_shift = Column(Integer)  # 1, 2, 3
    ps_work_center = Column(String(10), ForeignKey('work_center_rec.wc_code'))
    ps_work_order = Column(String(10), ForeignKey('work_order_rec.wo_no'))
    ps_operation_seq = Column(Integer)
    ps_start_time = Column(String(4))  # HHMM
    ps_end_time = Column(String(4))
    ps_quantity_planned = Column(Numeric(15, 3))
    ps_quantity_completed = Column(Numeric(15, 3))
    ps_status = Column(String(20))  # SCHEDULED, IN_PROGRESS, COMPLETED, DELAYED
    ps_operator = Column(String(10))
    ps_notes = Column(Text)
    
    # Relationships
    work_center = relationship("WorkCenterRec")
    work_order = relationship("WorkOrderRec")


class ProductionTransactionRec(Base):
    """Production Transaction/Shop Floor Data Collection"""
    __tablename__ = "production_transaction_rec"
    
    pt_id = Column(Integer, primary_key=True, autoincrement=True)
    pt_date = Column(Integer)
    pt_time = Column(Integer)
    pt_type = Column(String(20))  # START, STOP, COMPLETE, SCRAP, REWORK
    pt_work_order = Column(String(10), ForeignKey('work_order_rec.wo_no'))
    pt_operation_seq = Column(Integer)
    pt_work_center = Column(String(10), ForeignKey('work_center_rec.wc_code'))
    pt_operator = Column(String(10))
    pt_quantity_good = Column(Numeric(15, 3))
    pt_quantity_scrap = Column(Numeric(15, 3))
    pt_scrap_reason = Column(String(10))
    pt_labor_hours = Column(Numeric(10, 2))
    pt_machine_hours = Column(Numeric(10, 2))
    pt_setup_hours = Column(Numeric(10, 2))
    pt_downtime_hours = Column(Numeric(10, 2))
    pt_downtime_reason = Column(String(10))
    pt_notes = Column(Text)
    
    # Relationships
    work_order = relationship("WorkOrderRec")
    work_center = relationship("WorkCenterRec")