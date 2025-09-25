"""
ACAS System Configuration Model
SQLAlchemy model for SYSTEM_REC table
"""
from sqlalchemy import Column, Integer, String, Boolean, Numeric, DateTime, CheckConstraint
from sqlalchemy.sql import func
from app.core.database import Base

class SystemRec(Base):
    """
    System Configuration Record
    
    Main system configuration table containing company information,
    system parameters, module settings, and control totals.
    
    This mirrors the COBOL SYSTEM_REC copybook structure.
    """
    __tablename__ = "system_rec"
    
    # Primary Key - Single record system
    system_rec_key = Column(
        Integer, 
        primary_key=True, 
        default=1,
        doc="System record key - always 1 (single record table)"
    )
    
    # Company Information
    company_name = Column(
        String(40), 
        nullable=False, 
        default='',
        doc="Company name (40 characters)"
    )
    company_address_1 = Column(
        String(40), 
        nullable=False, 
        default='',
        doc="Company address line 1"
    )
    company_address_2 = Column(
        String(40), 
        nullable=False, 
        default='',
        doc="Company address line 2"
    )
    company_address_3 = Column(
        String(40), 
        nullable=False, 
        default='',
        doc="Company address line 3"
    )
    company_address_4 = Column(
        String(40), 
        nullable=False, 
        default='',
        doc="Company address line 4"
    )
    company_address_5 = Column(
        String(40), 
        nullable=False, 
        default='',
        doc="Company address line 5 (usually postcode/zip)"
    )
    
    # System Parameters
    current_period = Column(
        Integer, 
        nullable=False, 
        default=1,
        doc="Current accounting period (1-13)"
    )
    year_end_date = Column(
        Integer, 
        nullable=False, 
        default=0,
        doc="Year end date in YYYYMMDD format"
    )
    base_currency = Column(
        String(3), 
        nullable=False, 
        default='GBP',
        doc="Base currency code (ISO 4217)"
    )
    
    # VAT/Tax Configuration
    vat_rate_1 = Column(
        Numeric(5, 3), 
        nullable=False, 
        default=0.000,
        doc="Primary VAT rate (e.g., 20.000 for 20%)"
    )
    vat_rate_2 = Column(
        Numeric(5, 3), 
        nullable=False, 
        default=0.000,
        doc="Secondary VAT rate"
    )
    vat_rate_3 = Column(
        Numeric(5, 3), 
        nullable=False, 
        default=0.000,
        doc="Tertiary VAT rate"
    )
    vat_code_1 = Column(
        String(6), 
        nullable=False, 
        default='',
        doc="Primary VAT code"
    )
    vat_code_2 = Column(
        String(6), 
        nullable=False, 
        default='',
        doc="Secondary VAT code"
    )
    vat_code_3 = Column(
        String(6), 
        nullable=False, 
        default='',
        doc="Tertiary VAT code"
    )
    
    # Module Activation Flags
    gl_active = Column(
        Boolean, 
        nullable=False, 
        default=True,
        doc="General Ledger module active"
    )
    sl_active = Column(
        Boolean, 
        nullable=False, 
        default=True,
        doc="Sales Ledger module active"
    )
    pl_active = Column(
        Boolean, 
        nullable=False, 
        default=True,
        doc="Purchase Ledger module active"
    )
    stock_active = Column(
        Boolean, 
        nullable=False, 
        default=True,
        doc="Stock Control module active"
    )
    irs_active = Column(
        Boolean, 
        nullable=False, 
        default=False,
        doc="IRS (Incomplete Records System) module active"
    )
    
    # Audit and Control Configuration
    audit_trail = Column(
        Boolean, 
        nullable=False, 
        default=True,
        doc="Audit trail enabled"
    )
    backup_retention_days = Column(
        Integer, 
        nullable=False, 
        default=90,
        doc="Backup retention period in days"
    )
    auto_post_gl = Column(
        Boolean, 
        nullable=False, 
        default=True,
        doc="Automatically post transactions to General Ledger"
    )
    period_locked = Column(
        Boolean, 
        nullable=False, 
        default=False,
        doc="Current period locked for posting"
    )
    
    # System Control Totals (for validation and reconciliation)
    gl_total_dr = Column(
        Numeric(15, 2), 
        nullable=False, 
        default=0.00,
        doc="General Ledger total debits"
    )
    gl_total_cr = Column(
        Numeric(15, 2), 
        nullable=False, 
        default=0.00,
        doc="General Ledger total credits"
    )
    sl_total_balance = Column(
        Numeric(15, 2), 
        nullable=False, 
        default=0.00,
        doc="Sales Ledger total balance (debtors control)"
    )
    pl_total_balance = Column(
        Numeric(15, 2), 
        nullable=False, 
        default=0.00,
        doc="Purchase Ledger total balance (creditors control)"
    )
    stock_total_value = Column(
        Numeric(15, 2), 
        nullable=False, 
        default=0.00,
        doc="Stock total value (inventory control)"
    )
    
    # Version and Maintenance
    version = Column(
        String(20), 
        nullable=False, 
        default='3.02',
        doc="System version"
    )
    last_update = Column(
        DateTime(timezone=True), 
        server_default=func.now(),
        onupdate=func.now(),
        doc="Last update timestamp"
    )
    created_at = Column(
        DateTime(timezone=True), 
        server_default=func.now(),
        doc="Record creation timestamp"
    )
    
    # Table constraints
    __table_args__ = (
        CheckConstraint(
            'system_rec_key = 1', 
            name='ck_system_rec_single_record'
        ),
        CheckConstraint(
            'current_period >= 1 AND current_period <= 13', 
            name='ck_system_rec_valid_period'
        ),
        CheckConstraint(
            'year_end_date >= 0', 
            name='ck_system_rec_valid_year_end'
        ),
        CheckConstraint(
            'vat_rate_1 >= 0 AND vat_rate_1 <= 100', 
            name='ck_system_rec_valid_vat_rate_1'
        ),
        CheckConstraint(
            'vat_rate_2 >= 0 AND vat_rate_2 <= 100', 
            name='ck_system_rec_valid_vat_rate_2'
        ),
        CheckConstraint(
            'vat_rate_3 >= 0 AND vat_rate_3 <= 100', 
            name='ck_system_rec_valid_vat_rate_3'
        ),
        CheckConstraint(
            'backup_retention_days > 0', 
            name='ck_system_rec_valid_backup_retention'
        ),
        {
            'comment': 'System configuration table - single record containing all system parameters'
        }
    )
    
    def __repr__(self):
        return f"<SystemRec(company='{self.company_name}', version='{self.version}')>"
    
    @property
    def is_period_open(self) -> bool:
        """Check if current period is open for posting"""
        return not self.period_locked
    
    @property
    def gl_in_balance(self) -> bool:
        """Check if General Ledger debits equal credits"""
        return self.gl_total_dr == self.gl_total_cr
    
    def get_vat_rate_by_code(self, vat_code: str) -> float:
        """Get VAT rate by VAT code"""
        if vat_code == self.vat_code_1:
            return float(self.vat_rate_1)
        elif vat_code == self.vat_code_2:
            return float(self.vat_rate_2)
        elif vat_code == self.vat_code_3:
            return float(self.vat_rate_3)
        else:
            return 0.0
    
    def get_company_address(self) -> str:
        """Get formatted company address"""
        address_lines = [
            self.company_address_1,
            self.company_address_2,
            self.company_address_3,
            self.company_address_4,
            self.company_address_5
        ]
        return '\n'.join(line for line in address_lines if line.strip())