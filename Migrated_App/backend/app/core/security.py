"""
Security utilities for ACAS
Authentication, authorization, and encryption functions
"""
from datetime import datetime, timedelta
from typing import Optional, Dict, Any
from jose import JWTError, jwt
from passlib.context import CryptContext
from sqlalchemy.orm import Session
from fastapi import Depends, HTTPException, status
from fastapi.security import OAuth2PasswordBearer

from app.core.config import settings
from app.models.auth import User
from app.core.database import get_db

# Password hashing
pwd_context = CryptContext(schemes=["bcrypt"], deprecated="auto")

# OAuth2 scheme
oauth2_scheme = OAuth2PasswordBearer(tokenUrl="/api/v1/auth/login")

# JWT settings
SECRET_KEY = settings.SECRET_KEY
ALGORITHM = "HS256"
ACCESS_TOKEN_EXPIRE_MINUTES = settings.ACCESS_TOKEN_EXPIRE_MINUTES


def verify_password(plain_password: str, hashed_password: str) -> bool:
    """Verify a password against a hash"""
    return pwd_context.verify(plain_password, hashed_password)


def get_password_hash(password: str) -> str:
    """Generate password hash"""
    return pwd_context.hash(password)


def hash_password(password: str) -> str:
    """Generate password hash (alias for get_password_hash)"""
    return pwd_context.hash(password)


def create_access_token(data: dict, expires_delta: Optional[timedelta] = None) -> str:
    """Create JWT access token"""
    to_encode = data.copy()
    if expires_delta:
        expire = datetime.utcnow() + expires_delta
    else:
        expire = datetime.utcnow() + timedelta(minutes=ACCESS_TOKEN_EXPIRE_MINUTES)
    
    to_encode.update({"exp": expire})
    encoded_jwt = jwt.encode(to_encode, SECRET_KEY, algorithm=ALGORITHM)
    return encoded_jwt


def verify_token(token: str, credentials_exception: HTTPException) -> Dict[str, Any]:
    """Verify JWT token and return payload"""
    try:
        payload = jwt.decode(token, SECRET_KEY, algorithms=[ALGORITHM])
        username: str = payload.get("sub")
        if username is None:
            raise credentials_exception
        return {"username": username}
    except JWTError:
        raise credentials_exception


async def get_current_user(
    token: str = Depends(oauth2_scheme),
    db: Session = Depends(get_db)
) -> User:
    """Get current authenticated user"""
    credentials_exception = HTTPException(
        status_code=status.HTTP_401_UNAUTHORIZED,
        detail="Could not validate credentials",
        headers={"WWW-Authenticate": "Bearer"},
    )
    
    token_data = verify_token(token, credentials_exception)
    
    user = db.query(User).filter(User.username == token_data["username"]).first()
    if user is None:
        raise credentials_exception
    
    if not user.is_active:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Inactive user"
        )
    
    return user


async def get_current_active_user(
    current_user: User = Depends(get_current_user)
) -> User:
    """Get current active user (alias for get_current_user)"""
    return current_user


async def get_current_active_superuser(
    current_user: User = Depends(get_current_user)
) -> User:
    """Get current active superuser"""
    if not current_user.is_superuser:
        raise HTTPException(
            status_code=status.HTTP_403_FORBIDDEN,
            detail="The user doesn't have enough privileges"
        )
    return current_user


async def require_admin(
    current_user: User = Depends(get_current_user)
) -> User:
    """Require admin privileges"""
    if not hasattr(current_user, 'is_admin'):
        # Check if user has admin role
        if hasattr(current_user, 'roles') and current_user.roles:
            is_admin = any(role.name.lower() in ['admin', 'administrator'] for role in current_user.roles)
        else:
            is_admin = getattr(current_user, 'is_superuser', False)
    else:
        is_admin = current_user.is_admin
    
    if not is_admin and not getattr(current_user, 'is_superuser', False):
        raise HTTPException(
            status_code=status.HTTP_403_FORBIDDEN,
            detail="Admin privileges required"
        )
    return current_user


def check_module_access(user: User, module: str, level: int = 1) -> bool:
    """
    Check if user has access to a specific module
    module: GL, SL, PL, STOCK, REPORTS, SYSTEM
    level: 0=None, 1=Read, 2=Write, 3=Full
    """
    if user.is_superuser:
        return True
    
    # Check module-specific access from role
    role = user.role
    if not role:
        return False
    
    module_access_map = {
        "GL": role.gl_access,
        "SL": role.sl_access,
        "PL": role.pl_access,
        "STOCK": role.stock_access,
        "REPORTS": role.reports_access,
        "SYSTEM": role.system_access
    }
    
    module_access = module_access_map.get(module.upper(), 0)
    return module_access >= level


def require_module_access(module: str, level: int = 1):
    """
    Dependency to require module access
    Use in routes like: current_user: User = Depends(require_module_access("GL", 2))
    """
    async def _require_module_access(
        current_user: User = Depends(get_current_user)
    ) -> User:
        if not check_module_access(current_user, module, level):
            raise HTTPException(
                status_code=status.HTTP_403_FORBIDDEN,
                detail=f"Insufficient privileges for {module} module (level {level} required)"
            )
        return current_user
    
    return _require_module_access


# COBOL-compatible password encoding/decoding (for migration)
def encode_cobol_password(password: str) -> str:
    """
    Encode password in COBOL-compatible format
    This is for migration purposes only
    """
    # Simple XOR encoding as used in legacy COBOL
    key = 0x5A  # Same key used in COBOL
    encoded = ""
    for char in password[:4]:  # COBOL uses 4-char passwords
        encoded += chr(ord(char) ^ key)
    return encoded.ljust(4)


def decode_cobol_password(encoded: str) -> str:
    """
    Decode COBOL-encoded password
    This is for migration purposes only
    """
    key = 0x5A
    decoded = ""
    for char in encoded[:4]:
        decoded += chr(ord(char) ^ key)
    return decoded.strip()


# Session management
class SessionManager:
    """Manage user sessions"""
    
    @staticmethod
    def create_session(db: Session, user_id: str, ip_address: str, user_agent: str) -> str:
        """Create a new session"""
        from app.models.auth import UserSession
        import uuid
        
        session_id = str(uuid.uuid4())
        expires_at = datetime.utcnow() + timedelta(hours=8)  # 8 hour sessions
        
        session = UserSession(
            session_id=session_id,
            user_id=user_id,
            ip_address=ip_address,
            user_agent=user_agent,
            expires_at=expires_at
        )
        
        db.add(session)
        db.commit()
        
        return session_id
    
    @staticmethod
    def validate_session(db: Session, session_id: str) -> Optional[Dict[str, Any]]:
        """Validate a session and return user info"""
        from app.models.auth import UserSession
        
        session = db.query(UserSession).filter(
            UserSession.session_id == session_id,
            UserSession.expires_at > datetime.utcnow()
        ).first()
        
        if not session:
            return None
        
        # Update last activity
        session.last_activity = datetime.utcnow()
        db.commit()
        
        return {
            "user_id": session.user_id,
            "session_id": session.session_id,
            "expires_at": session.expires_at
        }
    
    @staticmethod
    def end_session(db: Session, session_id: str) -> bool:
        """End a session"""
        from app.models.auth import UserSession
        
        session = db.query(UserSession).filter(
            UserSession.session_id == session_id
        ).first()
        
        if session:
            db.delete(session)
            db.commit()
            return True
        
        return False


# Audit logging
def log_user_action(
    db: Session,
    user: User,
    action: str,
    table: Optional[str] = None,
    key: Optional[str] = None,
    old_values: Optional[Dict] = None,
    new_values: Optional[Dict] = None,
    ip_address: Optional[str] = None,
    user_agent: Optional[str] = None,
    module: Optional[str] = None
) -> None:
    """Log user action to audit trail"""
    from app.models.audit import AuditLog
    import json
    
    audit_entry = AuditLog(
        audit_user=user.username,
        audit_action=action,
        audit_table=table,
        audit_key=key,
        audit_old_values=json.dumps(old_values) if old_values else None,
        audit_new_values=json.dumps(new_values) if new_values else None,
        audit_ip_address=ip_address,
        audit_user_agent=user_agent,
        audit_module=module
    )
    
    db.add(audit_entry)
    db.commit()


# Permission checking utilities
class Permissions:
    """Permission constants and checking"""
    
    # Actions
    READ = 1
    WRITE = 2
    DELETE = 3
    APPROVE = 4
    POST = 5
    
    # Special permissions
    VIEW_COST = "view_cost"
    VIEW_PROFIT = "view_profit"
    MODIFY_CLOSED_PERIOD = "modify_closed_period"
    OVERRIDE_CREDIT_LIMIT = "override_credit_limit"
    
    @staticmethod
    def has_permission(user: User, permission: str) -> bool:
        """Check if user has specific permission"""
        if user.is_superuser:
            return True
        
        if not user.role or not user.role.permissions:
            return False
        
        return permission in user.role.permissions.get("special_permissions", [])