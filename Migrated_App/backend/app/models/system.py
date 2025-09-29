"""
ACAS System Configuration Model
SQLAlchemy model for SYSTEM_REC table
"""
from sqlalchemy import Column, Integer, String, Numeric
from app.core.database import Base

class SystemRec(Base):
    """
    System Configuration Record
    
    Main system configuration table containing system parameters and settings.
    This matches the actual COBOL migrated database schema.
    """
    __tablename__ = "system_rec"
    __table_args__ = {"schema": "acas"}
    
    # Primary Key
    system_rec_key = Column(Integer, primary_key=True, default=1)
    
    # System parameters (based on actual database columns)
    current_period = Column(Integer, nullable=False, default=1)
    period_status = Column(String(1), nullable=False, default='O')  # O=Open, C=Closed
    
    # The actual database has many more columns, but we'll only map what we need
    # The database appears to have 170+ columns from the COBOL migration
    
    def __repr__(self):
        return f"<SystemRec(key={self.system_rec_key}, period={self.current_period})>"
    
    @property
    def is_period_open(self) -> bool:
        """Check if current period is open for posting"""
        return self.period_status == 'O'