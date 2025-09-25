"""
ACAS Stock Control Models
SQLAlchemy models for inventory management and stock control
"""
from sqlalchemy import (
    Column, String, Integer, Numeric, Boolean, DateTime, 
    ForeignKey, CheckConstraint, Index
)
from sqlalchemy.orm import relationship
from sqlalchemy.sql import func
from app.core.database import Base
from decimal import Decimal

class StockRec(Base):
    """
    Stock Record - Inventory Master
    
    Represents stock/inventory items with complete movement history.
    Mirrors COBOL STOCK_REC structure with 83 fields.
    """
    __tablename__ = "stock_rec"
    
    # Primary Key - Stock Code
    stock_key = Column(
        String(13), 
        primary_key=True,
        doc="Stock item code (13 characters)"
    )
    
    # Item Description and Classification
    stock_desc = Column(
        String(30), 
        nullable=False, 
        default='',
        doc="Stock item description"
    )
    stock_abrev_key = Column(
        String(10), 
        nullable=False, 
        default='',
        doc="Abbreviated key/search code"
    )
    stock_location = Column(
        String(3), 
        nullable=False, 
        default='',
        doc="Primary storage location"
    )
    
    # Inventory Quantities
    stock_qty_on_hand = Column(
        Numeric(12, 3), 
        nullable=False, 
        default=0.000,
        doc="Current quantity on hand"
    )
    stock_qty_allocated = Column(
        Numeric(12, 3), 
        nullable=False, 
        default=0.000,
        doc="Quantity allocated to orders"
    )
    stock_qty_available = Column(
        Numeric(12, 3), 
        nullable=False, 
        default=0.000,
        doc="Available quantity (on hand - allocated)"
    )
    stock_qty_on_order = Column(
        Numeric(12, 3), 
        nullable=False, 
        default=0.000,
        doc="Quantity on purchase orders"
    )
    
    # Reorder Management
    stock_reorder_point = Column(
        Numeric(12, 3), 
        nullable=False, 
        default=0.000,
        doc="Reorder point quantity"
    )
    stock_reorder_qty = Column(
        Numeric(12, 3), 
        nullable=False, 
        default=0.000,
        doc="Reorder quantity"
    )
    stock_lead_time_days = Column(
        Integer, 
        nullable=False, 
        default=0,
        doc="Lead time in days"
    )
    
    # Costing Information
    stock_std_cost = Column(
        Numeric(10, 4), 
        nullable=False, 
        default=0.0000,
        doc="Standard cost per unit"
    )
    stock_last_cost = Column(
        Numeric(10, 4), 
        nullable=False, 
        default=0.0000,
        doc="Last purchase cost per unit"
    )
    stock_avg_cost = Column(
        Numeric(10, 4), 
        nullable=False, 
        default=0.0000,
        doc="Weighted average cost per unit"
    )
    stock_list_price = Column(
        Numeric(10, 4), 
        nullable=False, 
        default=0.0000,
        doc="List selling price per unit"
    )
    stock_costing_method = Column(
        String(1), 
        nullable=False, 
        default='A',
        doc="Costing method: A=Average, F=FIFO, L=LIFO, S=Standard"
    )
    
    # Classification and Configuration
    stock_product_group = Column(
        String(3), 
        nullable=False, 
        default='',
        doc="Product group code"
    )
    stock_unit_of_measure = Column(
        String(3), 
        nullable=False, 
        default='',
        doc="Unit of measure code"
    )
    stock_tax_code = Column(
        String(6), 
        nullable=False, 
        default='',
        doc="Tax/VAT code for sales"
    )
    
    # Status Flags
    stock_active = Column(
        Boolean, 
        nullable=False, 
        default=True,
        doc="Item is active"
    )
    stock_sellable = Column(
        Boolean, 
        nullable=False, 
        default=True,
        doc="Item can be sold"
    )
    stock_purchasable = Column(
        Boolean, 
        nullable=False, 
        default=True,
        doc="Item can be purchased"
    )
    
    # Monthly Receipt Statistics (12 periods)
    stock_receipt_01 = Column(Numeric(12, 3), nullable=False, default=0.000, doc="Period 1 receipts")
    stock_receipt_02 = Column(Numeric(12, 3), nullable=False, default=0.000, doc="Period 2 receipts")
    stock_receipt_03 = Column(Numeric(12, 3), nullable=False, default=0.000, doc="Period 3 receipts")
    stock_receipt_04 = Column(Numeric(12, 3), nullable=False, default=0.000, doc="Period 4 receipts")
    stock_receipt_05 = Column(Numeric(12, 3), nullable=False, default=0.000, doc="Period 5 receipts")
    stock_receipt_06 = Column(Numeric(12, 3), nullable=False, default=0.000, doc="Period 6 receipts")
    stock_receipt_07 = Column(Numeric(12, 3), nullable=False, default=0.000, doc="Period 7 receipts")
    stock_receipt_08 = Column(Numeric(12, 3), nullable=False, default=0.000, doc="Period 8 receipts")
    stock_receipt_09 = Column(Numeric(12, 3), nullable=False, default=0.000, doc="Period 9 receipts")
    stock_receipt_10 = Column(Numeric(12, 3), nullable=False, default=0.000, doc="Period 10 receipts")
    stock_receipt_11 = Column(Numeric(12, 3), nullable=False, default=0.000, doc="Period 11 receipts")
    stock_receipt_12 = Column(Numeric(12, 3), nullable=False, default=0.000, doc="Period 12 receipts")
    
    # Monthly Issue Statistics (12 periods)
    stock_issue_01 = Column(Numeric(12, 3), nullable=False, default=0.000, doc="Period 1 issues")
    stock_issue_02 = Column(Numeric(12, 3), nullable=False, default=0.000, doc="Period 2 issues")
    stock_issue_03 = Column(Numeric(12, 3), nullable=False, default=0.000, doc="Period 3 issues")
    stock_issue_04 = Column(Numeric(12, 3), nullable=False, default=0.000, doc="Period 4 issues")
    stock_issue_05 = Column(Numeric(12, 3), nullable=False, default=0.000, doc="Period 5 issues")
    stock_issue_06 = Column(Numeric(12, 3), nullable=False, default=0.000, doc="Period 6 issues")
    stock_issue_07 = Column(Numeric(12, 3), nullable=False, default=0.000, doc="Period 7 issues")
    stock_issue_08 = Column(Numeric(12, 3), nullable=False, default=0.000, doc="Period 8 issues")
    stock_issue_09 = Column(Numeric(12, 3), nullable=False, default=0.000, doc="Period 9 issues")
    stock_issue_10 = Column(Numeric(12, 3), nullable=False, default=0.000, doc="Period 10 issues")
    stock_issue_11 = Column(Numeric(12, 3), nullable=False, default=0.000, doc="Period 11 issues")
    stock_issue_12 = Column(Numeric(12, 3), nullable=False, default=0.000, doc="Period 12 issues")
    
    # Audit and Maintenance
    last_movement_date = Column(
        Integer,
        nullable=True,
        doc="Last movement date (YYYYMMDD format)"
    )
    created_at = Column(
        DateTime(timezone=True), 
        server_default=func.now(),
        doc="Record creation timestamp"
    )
    updated_at = Column(
        DateTime(timezone=True), 
        server_default=func.now(),
        onupdate=func.now(),
        doc="Last update timestamp"
    )
    
    # Relationships
    audit_records = relationship("StockAuditRec", back_populates="stock_item", cascade="all, delete-orphan")
    
    # Table constraints
    __table_args__ = (
        CheckConstraint(
            "stock_costing_method IN ('A', 'F', 'L', 'S')", 
            name='ck_stock_valid_costing_method'
        ),
        CheckConstraint(
            'stock_qty_on_hand >= 0', 
            name='ck_stock_valid_qty_on_hand'
        ),
        CheckConstraint(
            'stock_qty_allocated >= 0', 
            name='ck_stock_valid_qty_allocated'
        ),
        CheckConstraint(
            'stock_qty_available = stock_qty_on_hand - stock_qty_allocated', 
            name='ck_stock_valid_qty_available'
        ),
        CheckConstraint(
            'stock_reorder_point >= 0', 
            name='ck_stock_valid_reorder_point'
        ),
        CheckConstraint(
            'stock_lead_time_days >= 0', 
            name='ck_stock_valid_lead_time'
        ),
        Index('ix_stock_abrev', 'stock_abrev_key'),
        Index('ix_stock_desc', 'stock_desc'),
        Index('ix_stock_location', 'stock_location'),
        Index('ix_stock_active', 'stock_active'),
        Index('ix_stock_group', 'stock_product_group'),
        {
            'comment': 'Stock/inventory master records with movement history'
        }
    )
    
    def __repr__(self):
        return f"<Stock(key='{self.stock_key}', desc='{self.stock_desc}')>"
    
    @property
    def current_value(self) -> Decimal:
        """Calculate current inventory value"""
        if self.stock_costing_method == 'S':
            return Decimal(str(self.stock_qty_on_hand)) * Decimal(str(self.stock_std_cost))
        else:
            return Decimal(str(self.stock_qty_on_hand)) * Decimal(str(self.stock_avg_cost))
    
    @property
    def is_below_reorder_point(self) -> bool:
        """Check if stock is below reorder point"""
        return self.stock_qty_available <= self.stock_reorder_point
    
    @property
    def is_active_item(self) -> bool:
        """Check if item is active for transactions"""
        return self.stock_active
    
    def can_allocate(self, quantity: Decimal) -> bool:
        """Check if quantity can be allocated"""
        return self.stock_qty_available >= quantity and self.stock_sellable
    
    def can_receive(self, quantity: Decimal) -> bool:
        """Check if quantity can be received"""
        return self.stock_active and self.stock_purchasable
    
    def get_period_receipts(self, period: int) -> Decimal:
        """Get receipts for specific period (1-12)"""
        if period < 1 or period > 12:
            return Decimal('0.000')
        return Decimal(str(getattr(self, f'stock_receipt_{period:02d}')))
    
    def get_period_issues(self, period: int) -> Decimal:
        """Get issues for specific period (1-12)"""
        if period < 1 or period > 12:
            return Decimal('0.000')
        return Decimal(str(getattr(self, f'stock_issue_{period:02d}')))
    
    def calculate_average_cost(self, receipt_qty: Decimal, receipt_cost: Decimal) -> Decimal:
        """Calculate new average cost after receipt"""
        if self.stock_qty_on_hand == 0:
            return receipt_cost
        
        current_value = Decimal(str(self.stock_qty_on_hand)) * Decimal(str(self.stock_avg_cost))
        receipt_value = receipt_qty * receipt_cost
        total_qty = Decimal(str(self.stock_qty_on_hand)) + receipt_qty
        
        if total_qty == 0:
            return Decimal('0.0000')
        
        return (current_value + receipt_value) / total_qty

class StockAuditRec(Base):
    """
    Stock Audit Record
    
    Complete audit trail of all stock movements.
    Provides tamper-proof record of inventory transactions.
    """
    __tablename__ = "stockaudit_rec"
    
    # Primary Key - Auto-increment
    audit_id = Column(
        Integer, 
        primary_key=True,
        autoincrement=True,
        doc="Unique audit record ID"
    )
    
    # Stock Item Reference
    stock_key = Column(
        String(13), 
        ForeignKey("stock_rec.stock_key", ondelete="CASCADE"),
        nullable=False,
        doc="Stock item code"
    )
    
    # Movement Information
    movement_date = Column(
        Integer, 
        nullable=False,
        doc="Movement date (YYYYMMDD format)"
    )
    movement_type = Column(
        String(2), 
        nullable=False,
        doc="Movement type: RR=Receipt, IS=Issue, AD=Adjustment, TR=Transfer"
    )
    document_ref = Column(
        String(15), 
        nullable=False, 
        default='',
        doc="Document reference (PO, invoice, etc.)"
    )
    
    # Quantity and Cost Information
    quantity = Column(
        Numeric(12, 3), 
        nullable=False, 
        default=0.000,
        doc="Movement quantity (positive for receipts, negative for issues)"
    )
    unit_cost = Column(
        Numeric(10, 4), 
        nullable=False, 
        default=0.0000,
        doc="Unit cost for this movement"
    )
    total_value = Column(
        Numeric(15, 2), 
        nullable=False, 
        default=0.00,
        doc="Total value of movement (quantity × unit cost)"
    )
    
    # Location and User Information
    location = Column(
        String(3), 
        nullable=False, 
        default='',
        doc="Location/warehouse code"
    )
    user_id = Column(
        String(10), 
        nullable=False, 
        default='',
        doc="User who created the movement"
    )
    
    # System Timestamp
    timestamp = Column(
        DateTime(timezone=True), 
        server_default=func.now(),
        doc="System timestamp (tamper-proof)"
    )
    
    # Relationships
    stock_item = relationship("StockRec", back_populates="audit_records")
    
    # Table constraints
    __table_args__ = (
        CheckConstraint(
            "movement_type IN ('RR', 'IS', 'AD', 'TR', 'SO', 'PO')", 
            name='ck_stockaudit_valid_movement_type'
        ),
        CheckConstraint(
            'movement_date > 0', 
            name='ck_stockaudit_valid_date'
        ),
        CheckConstraint(
            'total_value = quantity * unit_cost', 
            name='ck_stockaudit_valid_value_calculation'
        ),
        Index('ix_stockaudit_stock', 'stock_key'),
        Index('ix_stockaudit_date', 'movement_date'),
        Index('ix_stockaudit_type', 'movement_type'),
        Index('ix_stockaudit_document', 'document_ref'),
        Index('ix_stockaudit_timestamp', 'timestamp'),
        {
            'comment': 'Complete audit trail of all stock movements'
        }
    )
    
    def __repr__(self):
        return f"<StockAudit(id={self.audit_id}, stock='{self.stock_key}', type='{self.movement_type}')>"
    
    @property
    def is_receipt(self) -> bool:
        """Check if this is a receipt movement"""
        return self.movement_type in ['RR', 'AD'] and self.quantity > 0
    
    @property
    def is_issue(self) -> bool:
        """Check if this is an issue movement"""
        return self.movement_type in ['IS', 'SO'] and self.quantity < 0
    
    @property
    def movement_description(self) -> str:
        """Get human-readable movement description"""
        descriptions = {
            'RR': 'Receipt from Purchase',
            'IS': 'Issue to Sales',
            'AD': 'Stock Adjustment',
            'TR': 'Transfer Between Locations',
            'SO': 'Sales Order Allocation',
            'PO': 'Purchase Order Receipt'
        }
        return descriptions.get(self.movement_type, f"Unknown ({self.movement_type})")