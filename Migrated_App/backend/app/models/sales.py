"""
ACAS Sales Models
SQLAlchemy models for sales orders, open items, receipts, and related sales processes
"""
from sqlalchemy import (
    Column, String, Integer, Numeric, DateTime, Text, Boolean,
    ForeignKey, CheckConstraint, Index
)
from sqlalchemy.orm import relationship
from sqlalchemy.sql import func
from app.core.database import Base


class SalesOpenItemRec(Base):
    """
    Sales Open Items - Outstanding customer transactions
    Tracks all open invoices, credit notes, payments, and their allocation
    """
    __tablename__ = "sales_open_items"
    
    # Primary Key
    item_id = Column(Integer, primary_key=True, autoincrement=True, doc="Open item ID")
    
    # Customer and Transaction Info
    sales_key = Column(String(10), ForeignKey("acas.saledger_rec.sales_key", ondelete="RESTRICT"), nullable=False, doc="Customer code")
    transaction_type = Column(String(2), nullable=False, doc="Transaction type: IN=Invoice, CN=Credit Note, PY=Payment")
    document_number = Column(String(20), nullable=False, doc="Document number")
    
    # Dates
    transaction_date = Column(Integer, nullable=False, doc="Transaction date (YYYYMMDD)")
    due_date = Column(Integer, nullable=False, doc="Due date (YYYYMMDD)")
    
    # Financial Information
    original_amount = Column(Numeric(12, 2), nullable=False, doc="Original transaction amount")
    outstanding_amount = Column(Numeric(12, 2), nullable=False, doc="Outstanding amount")
    allocated_amount = Column(Numeric(12, 2), default=0.00, doc="Allocated amount")
    
    # Status and Control
    status = Column(String(1), default='O', doc="Status: O=Open, A=Allocated, C=Closed")
    currency = Column(String(3), default='USD', doc="Currency code")
    exchange_rate = Column(Numeric(8, 4), default=1.0000, doc="Exchange rate")
    
    # Additional Information
    reference = Column(String(30), doc="Customer reference")
    narrative = Column(String(100), doc="Transaction narrative")
    
    # Audit Trail
    created_at = Column(DateTime(timezone=True), server_default=func.current_timestamp())
    updated_at = Column(DateTime(timezone=True), server_default=func.current_timestamp(), onupdate=func.current_timestamp())
    
    # Relationships
    customer = relationship("SalesLedgerRec", foreign_keys=[sales_key])
    payment_allocations = relationship("SalesAllocationRec", foreign_keys="SalesAllocationRec.payment_item_id", back_populates="payment_item")
    invoice_allocations = relationship("SalesAllocationRec", foreign_keys="SalesAllocationRec.invoice_item_id", back_populates="invoice_item")
    
    # Table constraints
    __table_args__ = (
        CheckConstraint("transaction_type IN ('IN', 'CN', 'PY', 'JE')", name='ck_sales_open_item_type'),
        CheckConstraint("status IN ('O', 'A', 'C')", name='ck_sales_open_item_status'),
        CheckConstraint('outstanding_amount >= 0', name='ck_sales_open_item_outstanding'),
        Index('ix_sales_open_customer', 'sales_key'),
        Index('ix_sales_open_status', 'status'),
        Index('ix_sales_open_due_date', 'due_date'),
        Index('ix_sales_open_doc_number', 'document_number'),
        {'schema': 'acas'}
    )


class SalesOrderRec(Base):
    """
    Sales Orders - Customer orders for goods/services
    """
    __tablename__ = "sales_orders"
    
    # Primary Key
    so_no = Column(String(20), primary_key=True, doc="Sales order number")
    
    # Customer Information
    sales_key = Column(String(10), ForeignKey("acas.saledger_rec.sales_key", ondelete="RESTRICT"), nullable=False, doc="Customer code")
    customer_po = Column(String(30), doc="Customer purchase order number")
    
    # Dates
    order_date = Column(Integer, nullable=False, doc="Order date (YYYYMMDD)")
    required_date = Column(Integer, doc="Required delivery date (YYYYMMDD)")
    promised_date = Column(Integer, doc="Promised delivery date (YYYYMMDD)")
    
    # Status and Control
    so_status = Column(String(20), default='OPEN', doc="Order status")
    priority = Column(String(10), default='NORMAL', doc="Order priority")
    so_warehouse = Column(String(10), doc="Warehouse code")
    
    # Financial Information
    order_value = Column(Numeric(15, 2), default=0.00, doc="Total order value")
    discount_percent = Column(Numeric(5, 2), default=0.00, doc="Order discount percentage")
    
    # Delivery Information
    delivery_address_1 = Column(String(30), doc="Delivery address line 1")
    delivery_address_2 = Column(String(30), doc="Delivery address line 2")
    delivery_address_3 = Column(String(30), doc="Delivery address line 3")
    delivery_postcode = Column(String(12), doc="Delivery postcode")
    delivery_contact = Column(String(30), doc="Delivery contact person")
    delivery_phone = Column(String(20), doc="Delivery contact phone")
    
    # Special Instructions
    notes = Column(Text, doc="Order notes and special instructions")
    
    # Audit Trail
    created_by = Column(String(30), nullable=False, doc="Created by user")
    created_at = Column(DateTime(timezone=True), server_default=func.current_timestamp())
    updated_at = Column(DateTime(timezone=True), server_default=func.current_timestamp(), onupdate=func.current_timestamp())
    
    # Relationships
    customer = relationship("SalesLedgerRec", foreign_keys=[sales_key])
    lines = relationship("SalesOrderLineRec", back_populates="order", cascade="all, delete-orphan")
    
    # Table constraints
    __table_args__ = (
        CheckConstraint("so_status IN ('OPEN', 'ALLOCATED', 'PICKED', 'DESPATCHED', 'INVOICED', 'CANCELLED')", name='ck_so_status'),
        CheckConstraint("priority IN ('LOW', 'NORMAL', 'HIGH', 'URGENT')", name='ck_so_priority'),
        Index('ix_so_customer', 'sales_key'),
        Index('ix_so_status', 'so_status'),
        Index('ix_so_date', 'order_date'),
        Index('ix_so_warehouse', 'so_warehouse'),
        {'schema': 'acas'}
    )


class SalesOrderLineRec(Base):
    """
    Sales Order Lines - Line items within sales orders
    """
    __tablename__ = "sales_order_lines"
    
    # Primary Key
    line_id = Column(Integer, primary_key=True, autoincrement=True, doc="Line ID")
    
    # Order Reference
    so_no = Column(String(20), ForeignKey("acas.sales_orders.so_no", ondelete="CASCADE"), nullable=False, doc="Sales order number")
    line_number = Column(Integer, nullable=False, doc="Line number within order")
    
    # Product Information
    stock_key = Column(String(30), nullable=False, doc="Stock item code")
    description = Column(String(100), nullable=False, doc="Item description")
    
    # Quantities
    ordered_qty = Column(Numeric(15, 3), nullable=False, doc="Ordered quantity")
    allocated_qty = Column(Numeric(15, 3), default=0.000, doc="Allocated quantity")
    picked_qty = Column(Numeric(15, 3), default=0.000, doc="Picked quantity")
    despatched_qty = Column(Numeric(15, 3), default=0.000, doc="Despatched quantity")
    invoiced_qty = Column(Numeric(15, 3), default=0.000, doc="Invoiced quantity")
    
    # Pricing
    unit_price = Column(Numeric(15, 4), nullable=False, doc="Unit selling price")
    discount_percent = Column(Numeric(5, 2), default=0.00, doc="Line discount percentage")
    line_value = Column(Numeric(15, 2), nullable=False, doc="Line total value")
    
    # Status and Control
    line_status = Column(String(20), default='OPEN', doc="Line status")
    warehouse = Column(String(10), doc="Warehouse for this line")
    
    # Special Instructions
    line_notes = Column(String(200), doc="Line-specific notes")
    
    # Relationships
    order = relationship("SalesOrderRec", back_populates="lines")
    
    # Table constraints
    __table_args__ = (
        CheckConstraint("line_status IN ('OPEN', 'ALLOCATED', 'PICKED', 'DESPATCHED', 'INVOICED', 'CANCELLED')", name='ck_sol_status'),
        CheckConstraint('ordered_qty > 0', name='ck_sol_positive_qty'),
        CheckConstraint('unit_price >= 0', name='ck_sol_positive_price'),
        Index('ix_sol_order', 'so_no'),
        Index('ix_sol_stock', 'stock_key'),
        Index('ix_sol_status', 'line_status'),
        {'schema': 'acas'}
    )


class SalesAllocationRec(Base):
    """
    Sales Allocations - Links payments to invoices
    """
    __tablename__ = "sales_allocations"
    
    # Primary Key
    allocation_id = Column(Integer, primary_key=True, autoincrement=True, doc="Allocation ID")
    
    # Related Items
    payment_item_id = Column(Integer, ForeignKey("acas.sales_open_items.item_id", ondelete="CASCADE"), nullable=False, doc="Payment open item ID")
    invoice_item_id = Column(Integer, ForeignKey("acas.sales_open_items.item_id", ondelete="CASCADE"), nullable=False, doc="Invoice open item ID")
    
    # Allocation Details
    allocated_amount = Column(Numeric(12, 2), nullable=False, doc="Amount allocated")
    allocation_date = Column(Integer, nullable=False, doc="Allocation date (YYYYMMDD)")
    
    # Control Information
    allocated_by = Column(String(30), nullable=False, doc="User who made allocation")
    allocation_ref = Column(String(20), doc="Allocation reference")
    
    # Audit Trail
    created_at = Column(DateTime(timezone=True), server_default=func.current_timestamp())
    
    # Relationships
    payment_item = relationship("SalesOpenItemRec", foreign_keys=[payment_item_id], back_populates="payment_allocations")
    invoice_item = relationship("SalesOpenItemRec", foreign_keys=[invoice_item_id], back_populates="invoice_allocations")
    
    # Table constraints
    __table_args__ = (
        CheckConstraint('allocated_amount > 0', name='ck_allocation_positive_amount'),
        Index('ix_allocation_payment', 'payment_item_id'),
        Index('ix_allocation_invoice', 'invoice_item_id'),
        Index('ix_allocation_date', 'allocation_date'),
        {'schema': 'acas'}
    )


class SalesReceiptRec(Base):
    """
    Sales Receipts - Customer payments and cash receipts
    """
    __tablename__ = "sales_receipts"
    __table_args__ = {"schema": "acas"}
    
    # Primary Key
    receipt_id = Column(Integer, primary_key=True, autoincrement=True, doc="Receipt ID")
    receipt_number = Column(String(20), unique=True, nullable=False, doc="Receipt number")
    
    # Customer Information
    sales_key = Column(String(10), ForeignKey("acas.saledger_rec.sales_key", ondelete="RESTRICT"), nullable=False, doc="Customer code")
    
    # Payment Details
    receipt_date = Column(Integer, nullable=False, doc="Receipt date (YYYYMMDD)")
    payment_method = Column(String(10), nullable=False, doc="Payment method")
    amount = Column(Numeric(12, 2), nullable=False, doc="Receipt amount")
    
    # Bank/Check Information
    bank_account = Column(String(20), doc="Bank account code")
    check_number = Column(String(20), doc="Check number")
    check_date = Column(Integer, doc="Check date (YYYYMMDD)")
    
    # Status and Control
    status = Column(String(1), default='U', doc="Status: U=Unallocated, A=Allocated, C=Cleared")
    currency = Column(String(3), default='USD', doc="Currency code")
    exchange_rate = Column(Numeric(8, 4), default=1.0000, doc="Exchange rate")
    
    # Additional Information
    customer_ref = Column(String(30), doc="Customer reference")
    narrative = Column(String(100), doc="Receipt narrative")
    
    # Audit Trail
    received_by = Column(String(30), nullable=False, doc="User who recorded receipt")
    created_at = Column(DateTime(timezone=True), server_default=func.current_timestamp())
    updated_at = Column(DateTime(timezone=True), server_default=func.current_timestamp(), onupdate=func.current_timestamp())
    
    # Relationships
    customer = relationship("SalesLedgerRec", foreign_keys=[sales_key])
    
    # Table constraints
    __table_args__ = (
        CheckConstraint("payment_method IN ('CASH', 'CHECK', 'TRANSFER', 'CARD', 'OTHER')", name='ck_receipt_payment_method'),
        CheckConstraint("status IN ('U', 'A', 'C')", name='ck_receipt_status'),
        CheckConstraint('amount > 0', name='ck_receipt_positive_amount'),
        Index('ix_receipt_customer', 'sales_key'),
        Index('ix_receipt_date', 'receipt_date'),
        Index('ix_receipt_status', 'status'),
        Index('ix_receipt_number', 'receipt_number'),
        {'schema': 'acas'}
    )


class SalesHistoryRec(Base):
    """
    Sales History - Historical sales transactions for analysis
    """
    __tablename__ = "sales_history"
    
    # Primary Key
    history_id = Column(Integer, primary_key=True, autoincrement=True, doc="History ID")
    
    # Transaction Information
    sales_key = Column(String(10), ForeignKey("acas.saledger_rec.sales_key", ondelete="RESTRICT"), nullable=False, doc="Customer code")
    stock_key = Column(String(30), nullable=False, doc="Stock item code")
    transaction_date = Column(Integer, nullable=False, doc="Transaction date (YYYYMMDD)")
    
    # Transaction Details
    transaction_type = Column(String(2), nullable=False, doc="Transaction type: SA=Sale, CR=Credit, RE=Return")
    document_number = Column(String(20), nullable=False, doc="Document number")
    
    # Quantities and Values
    quantity = Column(Numeric(15, 3), nullable=False, doc="Transaction quantity")
    unit_price = Column(Numeric(15, 4), nullable=False, doc="Unit price")
    cost_price = Column(Numeric(15, 4), default=0.0000, doc="Unit cost price")
    total_value = Column(Numeric(15, 2), nullable=False, doc="Total transaction value")
    
    # Additional Information
    warehouse = Column(String(10), doc="Warehouse code")
    salesperson = Column(String(10), doc="Salesperson code")
    
    # Audit Trail
    created_at = Column(DateTime(timezone=True), server_default=func.current_timestamp())
    
    # Relationships
    customer = relationship("SalesLedgerRec", foreign_keys=[sales_key])
    
    # Table constraints
    __table_args__ = (
        CheckConstraint("transaction_type IN ('SA', 'CR', 'RE')", name='ck_history_transaction_type'),
        Index('ix_history_customer', 'sales_key'),
        Index('ix_history_stock', 'stock_key'),
        Index('ix_history_date', 'transaction_date'),
        Index('ix_history_type', 'transaction_type'),
        {'schema': 'acas'}
    )


class DeliveryNoteRec(Base):
    """
    Delivery Notes - Goods despatch documentation
    """
    __tablename__ = "delivery_notes"
    
    # Primary Key
    delivery_note_number = Column(String(20), primary_key=True, doc="Delivery note number")
    
    # Order Reference
    so_no = Column(String(20), ForeignKey("acas.sales_orders.so_no", ondelete="RESTRICT"), nullable=False, doc="Sales order number")
    
    # Customer Information
    sales_key = Column(String(10), ForeignKey("acas.saledger_rec.sales_key", ondelete="RESTRICT"), nullable=False, doc="Customer code")
    
    # Delivery Details
    delivery_date = Column(Integer, nullable=False, doc="Delivery date (YYYYMMDD)")
    carrier = Column(String(30), doc="Carrier/courier name")
    tracking_number = Column(String(50), doc="Tracking number")
    
    # Status
    status = Column(String(20), default='CREATED', doc="Delivery note status")
    
    # Audit Trail
    created_by = Column(String(30), nullable=False, doc="Created by user")
    created_at = Column(DateTime(timezone=True), server_default=func.current_timestamp())
    
    # Relationships
    order = relationship("SalesOrderRec", foreign_keys=[so_no])
    customer = relationship("SalesLedgerRec", foreign_keys=[sales_key])
    lines = relationship("DeliveryNoteLineRec", back_populates="delivery_note", cascade="all, delete-orphan")
    
    # Table constraints
    __table_args__ = (
        CheckConstraint("status IN ('CREATED', 'PRINTED', 'DESPATCHED', 'DELIVERED', 'CANCELLED')", name='ck_dn_status'),
        Index('ix_dn_order', 'so_no'),
        Index('ix_dn_customer', 'sales_key'),
        Index('ix_dn_date', 'delivery_date'),
        {'schema': 'acas'}
    )


class DeliveryNoteLineRec(Base):
    """
    Delivery Note Lines - Items included in delivery
    """
    __tablename__ = "delivery_note_lines"
    
    # Primary Key
    line_id = Column(Integer, primary_key=True, autoincrement=True, doc="Line ID")
    
    # Delivery Note Reference
    delivery_note_number = Column(String(20), ForeignKey("acas.delivery_notes.delivery_note_number", ondelete="CASCADE"), nullable=False, doc="Delivery note number")
    line_number = Column(Integer, nullable=False, doc="Line number")
    
    # Product Information
    stock_key = Column(String(30), nullable=False, doc="Stock item code")
    description = Column(String(100), nullable=False, doc="Item description")
    
    # Quantities
    quantity_delivered = Column(Numeric(15, 3), nullable=False, doc="Quantity delivered")
    
    # Order Line Reference
    so_line_id = Column(Integer, doc="Sales order line ID reference")
    
    # Relationships
    delivery_note = relationship("DeliveryNoteRec", back_populates="lines")
    
    # Table constraints
    __table_args__ = (
        CheckConstraint('quantity_delivered > 0', name='ck_dnl_positive_qty'),
        Index('ix_dnl_delivery_note', 'delivery_note_number'),
        Index('ix_dnl_stock', 'stock_key'),
        {'schema': 'acas'}
    )


class DespatchRec(Base):
    """
    Despatch Records - Warehouse despatch documentation
    """
    __tablename__ = "despatch_records"
    
    # Primary Key
    despatch_id = Column(Integer, primary_key=True, autoincrement=True, doc="Despatch ID")
    despatch_number = Column(String(20), unique=True, nullable=False, doc="Despatch number")
    
    # References
    so_no = Column(String(20), ForeignKey("acas.sales_orders.so_no", ondelete="RESTRICT"), nullable=False, doc="Sales order number")
    warehouse = Column(String(10), nullable=False, doc="Despatch warehouse")
    
    # Despatch Details
    despatch_date = Column(Integer, nullable=False, doc="Despatch date (YYYYMMDD)")
    despatch_time = Column(String(8), doc="Despatch time (HH:MM:SS)")
    
    # Status
    status = Column(String(20), default='PENDING', doc="Despatch status")
    
    # Audit Trail
    created_by = Column(String(30), nullable=False, doc="Created by user")
    created_at = Column(DateTime(timezone=True), server_default=func.current_timestamp())
    
    # Relationships
    order = relationship("SalesOrderRec", foreign_keys=[so_no])
    lines = relationship("DespatchLineRec", back_populates="despatch", cascade="all, delete-orphan")
    
    # Table constraints
    __table_args__ = (
        CheckConstraint("status IN ('PENDING', 'PICKING', 'PACKED', 'DESPATCHED', 'CANCELLED')", name='ck_despatch_status'),
        Index('ix_despatch_order', 'so_no'),
        Index('ix_despatch_warehouse', 'warehouse'),
        Index('ix_despatch_date', 'despatch_date'),
        {'schema': 'acas'}
    )


class DespatchLineRec(Base):
    """
    Despatch Lines - Items in despatch
    """
    __tablename__ = "despatch_lines"
    
    # Primary Key
    line_id = Column(Integer, primary_key=True, autoincrement=True, doc="Line ID")
    
    # Despatch Reference
    despatch_id = Column(Integer, ForeignKey("acas.despatch_records.despatch_id", ondelete="CASCADE"), nullable=False, doc="Despatch ID")
    line_number = Column(Integer, nullable=False, doc="Line number")
    
    # Product Information
    stock_key = Column(String(30), nullable=False, doc="Stock item code")
    description = Column(String(100), nullable=False, doc="Item description")
    
    # Quantities
    quantity_despatched = Column(Numeric(15, 3), nullable=False, doc="Quantity despatched")
    
    # Order Line Reference
    so_line_id = Column(Integer, doc="Sales order line ID reference")
    
    # Relationships
    despatch = relationship("DespatchRec", back_populates="lines")
    
    # Table constraints
    __table_args__ = (
        CheckConstraint('quantity_despatched > 0', name='ck_dl_positive_qty'),
        Index('ix_dl_despatch', 'despatch_id'),
        Index('ix_dl_stock', 'stock_key'),
        {'schema': 'acas'}
    )


class LoadingAdviceRec(Base):
    """
    Loading Advice - Warehouse loading instructions
    """
    __tablename__ = "loading_advice"
    
    # Primary Key
    advice_id = Column(Integer, primary_key=True, autoincrement=True, doc="Advice ID")
    advice_number = Column(String(20), unique=True, nullable=False, doc="Loading advice number")
    
    # Load Information
    load_date = Column(Integer, nullable=False, doc="Loading date (YYYYMMDD)")
    vehicle_registration = Column(String(20), doc="Vehicle registration")
    driver_name = Column(String(50), doc="Driver name")
    carrier = Column(String(30), doc="Carrier name")
    
    # Route Information
    route_code = Column(String(10), doc="Delivery route code")
    estimated_delivery_time = Column(String(8), doc="Estimated delivery time (HH:MM:SS)")
    
    # Status
    status = Column(String(20), default='PLANNED', doc="Loading advice status")
    
    # Audit Trail
    created_by = Column(String(30), nullable=False, doc="Created by user")
    created_at = Column(DateTime(timezone=True), server_default=func.current_timestamp())
    
    # Table constraints
    __table_args__ = (
        CheckConstraint("status IN ('PLANNED', 'LOADING', 'LOADED', 'DEPARTED', 'DELIVERED', 'CANCELLED')", name='ck_la_status'),
        Index('ix_la_date', 'load_date'),
        Index('ix_la_carrier', 'carrier'),
        Index('ix_la_status', 'status'),
        {'schema': 'acas'}
    )


# Import the customer model to ensure relationships work
from .customer import SalesLedgerRec