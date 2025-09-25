"""
Audit Trail Model
Comprehensive audit logging for compliance
"""
from sqlalchemy import Column, String, Integer, DateTime, JSON, BigInteger, TIMESTAMP
from sqlalchemy.dialects.postgresql import INET

from app.core.database import Base
from datetime import datetime


class AuditLog(Base):
    """Comprehensive audit trail for all system changes"""
    __tablename__ = "audit_log"
    __table_args__ = {"schema": "acas"}
    
    audit_id = Column(BigInteger, primary_key=True, index=True)
    audit_timestamp = Column(TIMESTAMP(timezone=True), default=datetime.utcnow, index=True)
    audit_user = Column(String(30), nullable=False, index=True)
    audit_action = Column(String(20), nullable=False, index=True)  # INSERT, UPDATE, DELETE, LOGIN, etc
    audit_table = Column(String(50), index=True)
    audit_key = Column(String(100))
    audit_old_values = Column(JSON)
    audit_new_values = Column(JSON)
    audit_ip_address = Column(INET)
    audit_user_agent = Column(String)
    audit_session_id = Column(String(100))
    audit_module = Column(String(10))  # GL, SL, PL, STOCK, etc
    audit_program = Column(String(30))  # Which program/endpoint made the change
    
    @classmethod
    def log_action(cls, db_session, **kwargs):
        """Helper method to log an action"""
        audit_entry = cls(**kwargs)
        db_session.add(audit_entry)
        db_session.commit()
        return audit_entry