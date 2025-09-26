"""
Authentication schemas for request/response validation
"""
from typing import Optional, Dict, Any
from pydantic import BaseModel, EmailStr, Field, UUID4, ConfigDict
from datetime import datetime


class Token(BaseModel):
    """JWT token response"""
    access_token: str
    token_type: str = "bearer"
    user: Optional[Dict[str, Any]] = None


class TokenData(BaseModel):
    """Token payload data"""
    username: Optional[str] = None
    session_id: Optional[str] = None


class UserLogin(BaseModel):
    """User login request"""
    username: str = Field(..., min_length=3, max_length=30)
    password: str = Field(..., min_length=4)


class PasswordChange(BaseModel):
    """Password change request"""
    old_password: str = Field(..., min_length=4)
    new_password: str = Field(..., min_length=8)
    confirm_password: str = Field(..., min_length=8)
    
    model_config = {
        "json_schema_extra": {
            "example": {
                "old_password": "current_password",
                "new_password": "new_secure_password",
                "confirm_password": "new_secure_password"
            }
    }
        }


class UserBase(BaseModel):
    """Base user properties"""
    username: str = Field(..., min_length=3, max_length=30)
    email: EmailStr
    full_name: str = Field(..., min_length=1, max_length=60)
    is_active: bool = True
    is_superuser: bool = False
    role_id: int
    language: str = "en-US"
    timezone: str = "UTC"
    theme: str = "light"


class UserCreate(UserBase):
    """User creation request"""
    password: str = Field(..., min_length=8)
    
    model_config = {
        "json_schema_extra": {
            "example": {
                "username": "john_doe",
                "email": "john.doe@example.com",
                "full_name": "John Doe",
                "password": "secure_password",
                "role_id": 3,
                "is_active": True,
                "is_superuser": False
            }
    }
        }


class UserUpdate(BaseModel):
    """User update request"""
    email: Optional[EmailStr] = None
    full_name: Optional[str] = Field(None, min_length=1, max_length=60)
    is_active: Optional[bool] = None
    role_id: Optional[int] = None
    language: Optional[str] = None
    timezone: Optional[str] = None
    theme: Optional[str] = None


class UserResponse(BaseModel):
    """User response"""
    user_id: UUID4
    username: str
    email: EmailStr
    full_name: str
    is_active: bool
    is_superuser: bool
    role_id: int
    role: Optional[Dict[str, Any]] = None
    language: str
    timezone: str
    theme: str
    last_login: Optional[datetime] = None
    created_at: datetime
    updated_at: datetime
    
    model_config = ConfigDict(from_attributes=True)


class RoleBase(BaseModel):
    """Base role properties"""
    role_name: str = Field(..., min_length=3, max_length=30)
    role_description: Optional[str] = Field(None, max_length=100)
    gl_access: int = Field(0, ge=0, le=3)
    sl_access: int = Field(0, ge=0, le=3)
    pl_access: int = Field(0, ge=0, le=3)
    stock_access: int = Field(0, ge=0, le=3)
    reports_access: int = Field(0, ge=0, le=3)
    system_access: int = Field(0, ge=0, le=3)
    permissions: Dict[str, Any] = {}


class RoleCreate(RoleBase):
    """Role creation request"""
    pass


class RoleUpdate(BaseModel):
    """Role update request"""
    role_description: Optional[str] = None
    gl_access: Optional[int] = Field(None, ge=0, le=3)
    sl_access: Optional[int] = Field(None, ge=0, le=3)
    pl_access: Optional[int] = Field(None, ge=0, le=3)
    stock_access: Optional[int] = Field(None, ge=0, le=3)
    reports_access: Optional[int] = Field(None, ge=0, le=3)
    system_access: Optional[int] = Field(None, ge=0, le=3)
    permissions: Optional[Dict[str, Any]] = None


class RoleResponse(RoleBase):
    """Role response"""
    role_id: int
    created_at: datetime
    updated_at: datetime
    
    model_config = ConfigDict(from_attributes=True)


class SessionInfo(BaseModel):
    """Session information"""
    session_id: str
    user_id: UUID4
    created_at: datetime
    last_activity: datetime
    expires_at: datetime
    ip_address: Optional[str] = None
    
    model_config = ConfigDict(from_attributes=True)


class PermissionBase(BaseModel):
    """Base permission properties"""
    permission_name: str = Field(..., min_length=3, max_length=50)
    permission_description: Optional[str] = Field(None, max_length=200)
    resource: str = Field(..., min_length=1, max_length=50)
    action: str = Field(..., min_length=1, max_length=50)
    conditions: Dict[str, Any] = {}


class PermissionCreate(PermissionBase):
    """Permission creation request"""
    pass


class PermissionUpdate(BaseModel):
    """Permission update request"""
    permission_description: Optional[str] = None
    conditions: Optional[Dict[str, Any]] = None


class PermissionResponse(PermissionBase):
    """Permission response"""
    permission_id: int
    created_at: datetime
    updated_at: datetime
    
    model_config = ConfigDict(from_attributes=True)


class UserRoleAssignment(BaseModel):
    """User role assignment request"""
    role_id: int = Field(..., gt=0)
    
    model_config = {
        "json_schema_extra": {
            "example": {
                "role_id": 3
            }
        }
    }