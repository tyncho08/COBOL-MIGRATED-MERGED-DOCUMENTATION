"""
Authentication and Authorization Models
Maps to users, roles, sessions tables
"""
from sqlalchemy import Column, String, Boolean, Integer, DateTime, JSON, ForeignKey, TIMESTAMP
from sqlalchemy.orm import relationship
from sqlalchemy.dialects.postgresql import UUID
import uuid

from app.core.database import Base
from datetime import datetime


class Role(Base):
    """User roles with module access levels"""
    __tablename__ = "roles"
    __table_args__ = {"schema": "acas"}
    
    role_id = Column(Integer, primary_key=True, index=True)
    role_name = Column(String(30), unique=True, nullable=False)
    role_description = Column(String(100))
    permissions = Column(JSON, default=dict)
    
    # Module access levels (0=None, 1=Read, 2=Write, 3=Full)
    gl_access = Column(Integer, default=0)
    sl_access = Column(Integer, default=0)
    pl_access = Column(Integer, default=0)
    stock_access = Column(Integer, default=0)
    reports_access = Column(Integer, default=0)
    system_access = Column(Integer, default=0)
    
    # Timestamps
    created_at = Column(TIMESTAMP(timezone=True), default=datetime.utcnow)
    updated_at = Column(TIMESTAMP(timezone=True), default=datetime.utcnow, onupdate=datetime.utcnow)
    
    # Relationships
    users = relationship("User", back_populates="role")


class User(Base):
    """System users"""
    __tablename__ = "users"
    __table_args__ = {"schema": "acas"}
    
    user_id = Column(UUID(as_uuid=True), primary_key=True, default=uuid.uuid4)
    username = Column(String(30), unique=True, nullable=False, index=True)
    email = Column(String(100), unique=True, nullable=False, index=True)
    password_hash = Column(String(255), nullable=False)
    full_name = Column(String(60), nullable=False)
    
    # Role and permissions
    role_id = Column(Integer, ForeignKey("acas.roles.role_id"), nullable=False)
    is_active = Column(Boolean, default=True)
    is_superuser = Column(Boolean, default=False)
    
    # Module access override (JSON)
    modules_access = Column(JSON, default=dict)
    
    # Security
    last_login = Column(TIMESTAMP(timezone=True))
    failed_logins = Column(Integer, default=0)
    locked_until = Column(TIMESTAMP(timezone=True))
    
    # Preferences
    language = Column(String(5), default="en-US")
    timezone = Column(String(50), default="UTC")
    theme = Column(String(20), default="light")
    
    # Timestamps
    created_at = Column(TIMESTAMP(timezone=True), default=datetime.utcnow)
    updated_at = Column(TIMESTAMP(timezone=True), default=datetime.utcnow, onupdate=datetime.utcnow)
    created_by = Column(String(30), default="system")
    
    # Relationships
    role = relationship("Role", back_populates="users")
    sessions = relationship("UserSession", back_populates="user", cascade="all, delete-orphan")
    preferences = relationship("UserPreference", back_populates="user", cascade="all, delete-orphan")
    
    @property
    def is_locked(self) -> bool:
        """Check if account is locked"""
        if self.locked_until:
            return datetime.utcnow() < self.locked_until
        return False
    
    def get_module_access(self, module: str) -> int:
        """Get effective module access level"""
        # Check override first
        if self.modules_access and module in self.modules_access:
            return self.modules_access[module]
        
        # Fall back to role
        if self.role:
            module_map = {
                "GL": self.role.gl_access,
                "SL": self.role.sl_access,
                "PL": self.role.pl_access,
                "STOCK": self.role.stock_access,
                "REPORTS": self.role.reports_access,
                "SYSTEM": self.role.system_access
            }
            return module_map.get(module, 0)
        
        return 0


class UserSession(Base):
    """Active user sessions"""
    __tablename__ = "sessions"
    __table_args__ = {"schema": "acas"}
    
    session_id = Column(String(100), primary_key=True)
    user_id = Column(UUID(as_uuid=True), ForeignKey("acas.users.user_id"), nullable=False)
    created_at = Column(TIMESTAMP(timezone=True), default=datetime.utcnow)
    last_activity = Column(TIMESTAMP(timezone=True), default=datetime.utcnow)
    expires_at = Column(TIMESTAMP(timezone=True), nullable=False)
    ip_address = Column(String(45))  # Support IPv6
    user_agent = Column(String)
    data = Column(JSON, default=dict)
    
    # Relationships
    user = relationship("User", back_populates="sessions")


class UserPreference(Base):
    """User preferences storage"""
    __tablename__ = "user_preferences"
    __table_args__ = {"schema": "acas"}
    
    pref_id = Column(Integer, primary_key=True)
    user_id = Column(UUID(as_uuid=True), ForeignKey("acas.users.user_id"), nullable=False)
    pref_key = Column(String(50), nullable=False)
    pref_value = Column(JSON, nullable=False)
    created_at = Column(TIMESTAMP(timezone=True), default=datetime.utcnow)
    updated_at = Column(TIMESTAMP(timezone=True), default=datetime.utcnow, onupdate=datetime.utcnow)
    
    # Relationships
    user = relationship("User", back_populates="preferences")


class Permission(Base):
    """System permissions"""
    __tablename__ = "permissions"
    __table_args__ = {"schema": "acas"}
    
    permission_id = Column(Integer, primary_key=True)
    name = Column(String(50), unique=True, nullable=False)
    description = Column(String(200))
    module = Column(String(20), nullable=False)  # GL, SL, PL, STOCK, etc.
    action = Column(String(20), nullable=False)  # read, write, delete, etc.
    resource = Column(String(50))  # specific resource if applicable
    
    # Timestamps
    created_at = Column(TIMESTAMP(timezone=True), default=datetime.utcnow)
    
    # Relationships
    role_permissions = relationship("RolePermission", back_populates="permission")


class UserRole(Base):
    """User-Role assignment table (many-to-many)"""
    __tablename__ = "user_roles"
    __table_args__ = {"schema": "acas"}
    
    user_id = Column(UUID(as_uuid=True), ForeignKey("acas.users.user_id"), primary_key=True)
    role_id = Column(Integer, ForeignKey("acas.roles.role_id"), primary_key=True)
    assigned_at = Column(TIMESTAMP(timezone=True), default=datetime.utcnow)
    assigned_by = Column(String(30), nullable=False)
    
    # Relationships
    user = relationship("User", backref="user_roles")
    role = relationship("Role", backref="user_roles")


class RolePermission(Base):
    """Role-Permission assignment table (many-to-many)"""
    __tablename__ = "role_permissions"
    __table_args__ = {"schema": "acas"}
    
    role_id = Column(Integer, ForeignKey("acas.roles.role_id"), primary_key=True)
    permission_id = Column(Integer, ForeignKey("acas.permissions.permission_id"), primary_key=True)
    granted_at = Column(TIMESTAMP(timezone=True), default=datetime.utcnow)
    granted_by = Column(String(30), nullable=False)
    
    # Relationships
    role = relationship("Role", backref="role_permissions")
    permission = relationship("Permission", back_populates="role_permissions")


class SystemLock(Base):
    """Record locking for concurrent access control"""
    __tablename__ = "system_locks"
    __table_args__ = {"schema": "acas"}
    
    lock_id = Column(Integer, primary_key=True)
    lock_table = Column(String(50), nullable=False)
    lock_key = Column(String(100), nullable=False)
    locked_by = Column(String(30), nullable=False)
    locked_at = Column(TIMESTAMP(timezone=True), default=datetime.utcnow)
    expires_at = Column(TIMESTAMP(timezone=True), nullable=False)
    lock_type = Column(String(20), default="EXCLUSIVE")
    session_id = Column(String(100))