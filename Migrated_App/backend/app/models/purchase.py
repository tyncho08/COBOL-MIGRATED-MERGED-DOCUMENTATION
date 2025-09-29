"""
Purchase Order Models
Handles purchase orders, lines, and related transactions
"""
from sqlalchemy import Column, String, Integer, DateTime, Text, Boolean, ForeignKey, Numeric, Date
from sqlalchemy.orm import relationship
from sqlalchemy.sql import func
from app.core.database import Base


class PurchaseOrderRec(Base):
    """Purchase order header record"""
    __tablename__ = "purchase_orders"
    
    order_id = Column(Integer, primary_key=True, autoincrement=True)
    order_number = Column(String(20), unique=True, nullable=False, doc="Purchase order number")
    order_date = Column(Integer, nullable=False, doc="Order date YYYYMMDD")
    
    # Supplier information
    supplier_key = Column(String(20), nullable=False, doc="Supplier code")
    supplier_name = Column(String(100), doc="Supplier name")
    supplier_reference = Column(String(50), doc="Supplier's reference/quote number")
    
    # Delivery information
    delivery_address_1 = Column(String(100))
    delivery_address_2 = Column(String(100))
    delivery_address_3 = Column(String(100))
    delivery_address_4 = Column(String(100))
    delivery_contact = Column(String(50))
    delivery_phone = Column(String(30))
    
    # Dates
    required_date = Column(Integer, doc="Required delivery date YYYYMMDD")
    promised_date = Column(Integer, doc="Supplier promised date YYYYMMDD")
    
    # Status
    status = Column(String(1), nullable=False, default='O', doc="O=Open, C=Complete, X=Cancelled, H=Hold")
    approval_status = Column(String(1), default='P', doc="P=Pending, A=Approved, R=Rejected")
    
    # Financial
    currency_code = Column(String(3), default='USD')
    exchange_rate = Column(Numeric(15, 6), default=1.0)
    
    # Totals (in order currency)
    goods_amount = Column(Numeric(15, 2), default=0, doc="Total goods amount")
    tax_amount = Column(Numeric(15, 2), default=0, doc="Total tax amount")
    freight_amount = Column(Numeric(15, 2), default=0, doc="Freight amount")
    other_amount = Column(Numeric(15, 2), default=0, doc="Other charges")
    total_amount = Column(Numeric(15, 2), default=0, doc="Total order amount")
    
    # Terms
    payment_terms = Column(String(20), doc="Payment terms code")
    payment_days = Column(Integer, doc="Payment days")
    discount_percent = Column(Numeric(5, 2), doc="Early payment discount %")
    discount_days = Column(Integer, doc="Days for discount")
    
    # Shipping
    ship_via = Column(String(30), doc="Shipping method")
    fob_point = Column(String(30), doc="FOB point")
    
    # Internal
    buyer_code = Column(String(20), doc="Buyer/purchaser code")
    department_code = Column(String(20), doc="Department code")
    project_code = Column(String(20), doc="Project code")
    
    # Receipt status
    receipt_status = Column(String(1), default='N', doc="N=Not received, P=Partial, F=Full")
    first_receipt_date = Column(Integer, doc="First receipt date")
    last_receipt_date = Column(Integer, doc="Last receipt date")
    
    # Notes
    internal_notes = Column(Text, doc="Internal notes")
    supplier_notes = Column(Text, doc="Notes to supplier")
    
    # Audit
    created_by = Column(String(30), nullable=False)
    created_date = Column(Integer, nullable=False)
    modified_by = Column(String(30))
    modified_date = Column(Integer)
    approved_by = Column(String(30))
    approved_date = Column(Integer)
    
    # Relationships
    lines = relationship("PurchaseOrderLineRec", back_populates="order")
    receipts = relationship("PurchaseReceiptRec", back_populates="order")


class PurchaseOrderLineRec(Base):
    """Purchase order line record"""
    __tablename__ = "purchase_order_lines"
    
    line_id = Column(Integer, primary_key=True, autoincrement=True)
    order_id = Column(Integer, ForeignKey("purchase_orders.order_id"), nullable=False)
    line_number = Column(Integer, nullable=False, doc="Line number")
    
    # Item information
    item_type = Column(String(1), default='S', doc="S=Stock, N=Non-stock, D=Description")
    stock_code = Column(String(20), doc="Stock code (for stock items)")
    supplier_item_code = Column(String(50), doc="Supplier's item code")
    description = Column(String(255), nullable=False, doc="Item description")
    extended_description = Column(Text, doc="Extended description")
    
    # Quantities
    quantity_ordered = Column(Numeric(15, 4), nullable=False, doc="Quantity ordered")
    quantity_received = Column(Numeric(15, 4), default=0, doc="Quantity received to date")
    quantity_invoiced = Column(Numeric(15, 4), default=0, doc="Quantity invoiced to date")
    quantity_cancelled = Column(Numeric(15, 4), default=0, doc="Quantity cancelled")
    unit_of_measure = Column(String(10), doc="Unit of measure")
    
    # Pricing (in order currency)
    unit_price = Column(Numeric(15, 6), nullable=False, doc="Unit price")
    discount_percent = Column(Numeric(5, 2), default=0, doc="Line discount %")
    line_amount = Column(Numeric(15, 2), nullable=False, doc="Line amount after discount")
    
    # Tax
    tax_code = Column(String(10), doc="Tax code")
    tax_rate = Column(Numeric(5, 2), doc="Tax rate %")
    tax_amount = Column(Numeric(15, 2), default=0, doc="Tax amount")
    
    # Dates
    required_date = Column(Integer, doc="Required date for this line")
    promised_date = Column(Integer, doc="Promised date for this line")
    
    # GL coding
    gl_account = Column(String(10), doc="GL account code")
    cost_center = Column(String(20), doc="Cost center")
    project_code = Column(String(20), doc="Project code")
    
    # Status
    line_status = Column(String(1), default='O', doc="O=Open, C=Complete, X=Cancelled")
    close_reason = Column(String(20), doc="Reason for closing")
    
    # Receipt matching
    match_on_receipt = Column(Boolean, default=True, doc="Match on receipt")
    tolerance_percent = Column(Numeric(5, 2), doc="Price tolerance %")
    
    # Notes
    line_notes = Column(Text)
    
    # Relationships
    order = relationship("PurchaseOrderRec", back_populates="lines")


class PurchaseRequisitionRec(Base):
    """Purchase requisition record"""
    __tablename__ = "purchase_requisitions"
    
    requisition_id = Column(Integer, primary_key=True, autoincrement=True)
    requisition_number = Column(String(20), unique=True, nullable=False)
    requisition_date = Column(Integer, nullable=False)
    
    # Requester
    requested_by = Column(String(30), nullable=False)
    department_code = Column(String(20))
    
    # Status
    status = Column(String(1), default='O', doc="O=Open, A=Approved, C=Converted, X=Cancelled")
    
    # Approval
    approval_required = Column(Boolean, default=True)
    approved_by = Column(String(30))
    approved_date = Column(Integer)
    
    # Conversion
    po_number = Column(String(20), doc="Converted to PO number")
    converted_date = Column(Integer)
    converted_by = Column(String(30))
    
    # Priority
    priority = Column(String(1), default='N', doc="U=Urgent, H=High, N=Normal, L=Low")
    
    # Notes
    justification = Column(Text)
    notes = Column(Text)


class PurchaseReceiptRec(Base):
    """Purchase receipt/goods received record"""
    __tablename__ = "purchase_receipts"
    
    receipt_id = Column(Integer, primary_key=True, autoincrement=True)
    receipt_number = Column(String(20), unique=True, nullable=False)
    receipt_date = Column(Integer, nullable=False)
    
    # Reference
    order_id = Column(Integer, ForeignKey("purchase_orders.order_id"))
    supplier_key = Column(String(20), nullable=False)
    
    # Delivery
    delivery_note = Column(String(50), doc="Supplier delivery note")
    carrier = Column(String(50))
    
    # Status
    status = Column(String(1), default='O', doc="O=Open, P=Posted, X=Cancelled")
    
    # Inspection
    inspection_required = Column(Boolean, default=False)
    inspection_status = Column(String(1), doc="P=Pending, A=Approved, R=Rejected")
    inspection_date = Column(Integer)
    inspection_by = Column(String(30))
    
    # Invoice matching
    invoice_matched = Column(Boolean, default=False)
    invoice_number = Column(String(50))
    
    # Audit
    received_by = Column(String(30), nullable=False)
    posted_by = Column(String(30))
    posted_date = Column(Integer)
    
    # Relationships
    order = relationship("PurchaseOrderRec", back_populates="receipts")


class PurchaseContractRec(Base):
    """Purchase contract/blanket order record"""
    __tablename__ = "purchase_contracts"
    
    contract_id = Column(Integer, primary_key=True, autoincrement=True)
    contract_number = Column(String(20), unique=True, nullable=False)
    contract_type = Column(String(20), doc="BLANKET, SCHEDULED, STANDING")
    
    # Supplier
    supplier_key = Column(String(20), nullable=False)
    
    # Validity
    start_date = Column(Integer, nullable=False)
    end_date = Column(Integer, nullable=False)
    
    # Limits
    min_order_value = Column(Numeric(15, 2))
    max_order_value = Column(Numeric(15, 2))
    total_contract_value = Column(Numeric(15, 2))
    
    # Usage
    value_used = Column(Numeric(15, 2), default=0)
    value_remaining = Column(Numeric(15, 2))
    
    # Terms
    payment_terms = Column(String(20))
    delivery_terms = Column(String(50))
    
    # Status
    status = Column(String(1), default='A', doc="A=Active, E=Expired, C=Closed, S=Suspended")
    
    # Renewal
    auto_renew = Column(Boolean, default=False)
    renewal_notice_days = Column(Integer)
    renewal_notified = Column(Boolean, default=False)
    
    # Audit
    created_by = Column(String(30), nullable=False)
    created_date = Column(Integer, nullable=False)


class SupplierPriceListRec(Base):
    """Supplier price list record"""
    __tablename__ = "supplier_price_lists"
    
    price_id = Column(Integer, primary_key=True, autoincrement=True)
    supplier_key = Column(String(20), nullable=False)
    stock_code = Column(String(20), nullable=False)
    supplier_item_code = Column(String(50))
    
    # Pricing
    unit_price = Column(Numeric(15, 6), nullable=False)
    currency_code = Column(String(3), default='USD')
    unit_of_measure = Column(String(10))
    
    # Quantity breaks
    min_quantity = Column(Numeric(15, 4), default=1)
    max_quantity = Column(Numeric(15, 4))
    
    # Validity
    effective_date = Column(Integer, nullable=False)
    expiry_date = Column(Integer)
    
    # Lead time
    lead_time_days = Column(Integer)
    
    # Status
    active = Column(Boolean, default=True)
    preferred = Column(Boolean, default=False)
    
    # Last purchase
    last_purchase_date = Column(Integer)
    last_purchase_price = Column(Numeric(15, 6))
    
    # Notes
    notes = Column(Text)


class PurchaseHistoryRec(Base):
    """Purchase history record"""
    __tablename__ = "purchase_history"
    
    history_id = Column(Integer, primary_key=True, autoincrement=True)
    
    # Reference
    order_number = Column(String(20), nullable=False)
    line_number = Column(Integer)
    receipt_number = Column(String(20))
    invoice_number = Column(String(50))
    
    # Item
    stock_code = Column(String(20))
    description = Column(String(255))
    
    # Supplier
    supplier_key = Column(String(20), nullable=False)
    
    # Transaction
    transaction_date = Column(Integer, nullable=False)
    transaction_type = Column(String(20), doc="ORDER, RECEIPT, INVOICE, RETURN")
    
    # Quantities and values
    quantity = Column(Numeric(15, 4))
    unit_price = Column(Numeric(15, 6))
    total_amount = Column(Numeric(15, 2))
    currency_code = Column(String(3))
    
    # Performance
    days_late = Column(Integer)
    quality_issue = Column(Boolean, default=False)
    
    # Audit
    created_by = Column(String(30))
    created_date = Column(Integer)


class PurchaseOpenItemRec(Base):
    """Purchase Open Items - Outstanding supplier transactions"""
    __tablename__ = "purchase_open_items"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    item_id = Column(Integer, primary_key=True, autoincrement=True, doc="Open item ID")
    
    # Supplier and Transaction Info
    supplier_key = Column(String(10), nullable=False, doc="Supplier code")
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
    reference = Column(String(30), doc="Supplier reference")
    narrative = Column(String(100), doc="Transaction narrative")
    
    # Purchase Order Reference
    purchase_order = Column(String(20), doc="Related purchase order")
    
    # Payment Terms
    payment_terms = Column(String(20), doc="Payment terms")
    discount_available = Column(Numeric(12, 2), default=0.00, doc="Early payment discount available")
    discount_date = Column(Integer, doc="Discount expiry date (YYYYMMDD)")
    
    # Audit Trail
    created_at = Column(DateTime(timezone=True), server_default=func.current_timestamp())
    updated_at = Column(DateTime(timezone=True), server_default=func.current_timestamp(), onupdate=func.current_timestamp())


class GoodsReceivedRec(Base):
    """Goods Received Record - Goods receipt header"""
    __tablename__ = "goods_received"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    receipt_id = Column(Integer, primary_key=True, autoincrement=True, doc="Receipt ID")
    receipt_number = Column(String(20), unique=True, nullable=False, doc="Receipt number")
    
    # Reference Information
    purchase_order = Column(String(20), doc="Purchase order number")
    supplier_key = Column(String(10), nullable=False, doc="Supplier code")
    supplier_delivery_note = Column(String(30), doc="Supplier delivery note")
    
    # Receipt Details
    receipt_date = Column(Integer, nullable=False, doc="Receipt date (YYYYMMDD)")
    receipt_time = Column(Integer, doc="Receipt time (HHMMSS)")
    warehouse = Column(String(10), nullable=False, doc="Receiving warehouse")
    location = Column(String(10), doc="Receiving location")
    
    # Delivery Information
    carrier = Column(String(50), doc="Carrier/delivery company")
    vehicle_registration = Column(String(20), doc="Vehicle registration")
    driver_name = Column(String(50), doc="Driver name")
    
    # Status and Control
    status = Column(String(20), default='RECEIVED', doc="Receipt status")
    inspection_required = Column(String(1), default='N', doc="Inspection required flag")
    inspection_status = Column(String(20), doc="Inspection status")
    
    # Quality Control
    quality_checked = Column(String(1), default='N', doc="Quality checked flag")
    quality_result = Column(String(10), doc="Quality check result")
    quarantine_required = Column(String(1), default='N', doc="Quarantine required flag")
    
    # Processing
    posted_to_stock = Column(String(1), default='N', doc="Posted to stock flag")
    posted_date = Column(Integer, doc="Posted date (YYYYMMDD)")
    invoice_matched = Column(String(1), default='N', doc="Invoice matched flag")
    
    # Personnel
    received_by = Column(String(30), nullable=False, doc="Received by user")
    checked_by = Column(String(30), doc="Checked by user")
    authorized_by = Column(String(30), doc="Authorized by user")
    
    # Totals
    total_packages = Column(Integer, default=0, doc="Total packages received")
    total_weight = Column(Numeric(12, 3), doc="Total weight")
    
    # Notes
    receipt_notes = Column(Text, doc="Receipt notes")
    damage_notes = Column(Text, doc="Damage notes")
    variance_notes = Column(Text, doc="Variance notes")
    
    # Relationships
    lines = relationship("GoodsReceivedLineRec", back_populates="receipt", cascade="all, delete-orphan")


class GoodsReceivedLineRec(Base):
    """Goods Received Line Record - Individual items received"""
    __tablename__ = "goods_received_lines"
    __table_args__ = {'schema': 'acas'}
    
    # Primary Key
    line_id = Column(Integer, primary_key=True, autoincrement=True, doc="Line ID")
    
    # Receipt Reference
    receipt_id = Column(Integer, ForeignKey("acas.goods_received.receipt_id", ondelete="CASCADE"), nullable=False, doc="Receipt ID")
    line_number = Column(Integer, nullable=False, doc="Line number")
    
    # Purchase Order Reference
    po_line_id = Column(Integer, doc="Purchase order line ID")
    
    # Item Information
    stock_key = Column(String(30), nullable=False, doc="Stock item code")
    description = Column(String(100), doc="Item description")
    supplier_item_code = Column(String(50), doc="Supplier item code")
    
    # Quantities
    quantity_ordered = Column(Numeric(15, 3), doc="Quantity ordered")
    quantity_received = Column(Numeric(15, 3), nullable=False, doc="Quantity received")
    quantity_accepted = Column(Numeric(15, 3), doc="Quantity accepted")
    quantity_rejected = Column(Numeric(15, 3), default=0.000, doc="Quantity rejected")
    unit_of_measure = Column(String(6), default='EA', doc="Unit of measure")
    
    # Location Information
    warehouse = Column(String(10), doc="Warehouse code")
    location = Column(String(10), doc="Storage location")
    bin_location = Column(String(20), doc="Bin location")
    
    # Lot/Serial Information
    lot_number = Column(String(30), doc="Lot number")
    serial_number = Column(String(50), doc="Serial number")
    expiry_date = Column(Integer, doc="Expiry date (YYYYMMDD)")
    
    # Quality Information
    quality_grade = Column(String(10), doc="Quality grade")
    reject_reason = Column(String(100), doc="Rejection reason")
    condition_on_receipt = Column(String(50), doc="Condition on receipt")
    
    # Cost Information
    unit_cost = Column(Numeric(15, 4), doc="Unit cost")
    total_cost = Column(Numeric(15, 2), doc="Total line cost")
    
    # Status
    line_status = Column(String(20), default='RECEIVED', doc="Line status")
    variance_type = Column(String(20), doc="Variance type if any")
    
    # Processing
    posted_to_stock = Column(String(1), default='N', doc="Posted to stock flag")
    stock_movement_id = Column(Integer, doc="Related stock movement ID")
    
    # Notes
    line_notes = Column(Text, doc="Line notes")
    variance_reason = Column(Text, doc="Variance reason")
    
    # Relationships
    receipt = relationship("GoodsReceivedRec", back_populates="lines")