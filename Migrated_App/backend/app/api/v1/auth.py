"""
Authentication API endpoints
Login, logout, user management
"""
from datetime import timedelta, datetime
from typing import Any, Optional
from fastapi import APIRouter, Depends, HTTPException, status, Request
from fastapi.security import OAuth2PasswordRequestForm
from sqlalchemy.orm import Session

from app.core.database import get_db
from app.core.security import (
    verify_password,
    create_access_token,
    get_current_user,
    get_current_active_superuser,
    get_password_hash,
    SessionManager,
    log_user_action
)
from app.models.auth import User, Role
from app.schemas.auth import (
    Token,
    UserCreate,
    UserUpdate,
    UserResponse,
    UserLogin,
    PasswordChange
)
from app.core.config import settings

router = APIRouter(prefix="/auth", tags=["authentication"])


@router.post("/login", response_model=Token)
async def login(
    request: Request,
    form_data: OAuth2PasswordRequestForm = Depends(),
    db: Session = Depends(get_db)
) -> Any:
    """
    OAuth2 compatible token login
    """
    # Get user
    user = db.query(User).filter(User.username == form_data.username).first()
    
    if not user:
        # Log failed login attempt
        log_user_action(
            db=db,
            user=None,
            action="LOGIN_FAILED",
            ip_address=request.client.host,
            user_agent=request.headers.get("user-agent"),
            new_values={"username": form_data.username, "reason": "user_not_found"}
        )
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Incorrect username or password",
            headers={"WWW-Authenticate": "Bearer"},
        )
    
    # Check if account is locked
    if user.is_locked:
        raise HTTPException(
            status_code=status.HTTP_423_LOCKED,
            detail="Account is locked. Please contact administrator."
        )
    
    # Verify password
    if not verify_password(form_data.password, user.password_hash):
        # Increment failed login counter
        user.failed_logins += 1
        
        # Lock account after 5 failed attempts
        if user.failed_logins >= 5:
            user.locked_until = datetime.utcnow() + timedelta(minutes=30)
            db.commit()
            
        # Log failed attempt
        log_user_action(
            db=db,
            user=user,
            action="LOGIN_FAILED",
            ip_address=request.client.host,
            user_agent=request.headers.get("user-agent"),
            new_values={"reason": "incorrect_password", "failed_attempts": user.failed_logins}
        )
        
        db.commit()
        
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Incorrect username or password",
            headers={"WWW-Authenticate": "Bearer"},
        )
    
    if not user.is_active:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Inactive user"
        )
    
    # Reset failed login counter
    user.failed_logins = 0
    user.last_login = datetime.utcnow()
    db.commit()
    
    # Create session
    session_id = SessionManager.create_session(
        db=db,
        user_id=str(user.user_id),
        ip_address=request.client.host,
        user_agent=request.headers.get("user-agent", "")
    )
    
    # Create access token
    access_token_expires = timedelta(minutes=settings.ACCESS_TOKEN_EXPIRE_MINUTES)
    access_token = create_access_token(
        data={"sub": user.username, "session_id": session_id},
        expires_delta=access_token_expires
    )
    
    # Log successful login
    log_user_action(
        db=db,
        user=user,
        action="LOGIN",
        ip_address=request.client.host,
        user_agent=request.headers.get("user-agent"),
        new_values={"session_id": session_id}
    )
    
    return {
        "access_token": access_token,
        "token_type": "bearer",
        "user": {
            "username": user.username,
            "email": user.email,
            "full_name": user.full_name,
            "role": user.role.role_name if user.role else None,
            "is_superuser": user.is_superuser
        }
    }


@router.post("/logout")
async def logout(
    request: Request,
    current_user: User = Depends(get_current_user),
    db: Session = Depends(get_db)
) -> Any:
    """
    Logout current user
    """
    # End session (would need session_id from token)
    # For now, just log the action
    
    log_user_action(
        db=db,
        user=current_user,
        action="LOGOUT",
        ip_address=request.client.host,
        user_agent=request.headers.get("user-agent")
    )
    
    return {"message": "Successfully logged out"}


@router.get("/me", response_model=UserResponse)
async def read_current_user(
    current_user: User = Depends(get_current_user)
) -> Any:
    """
    Get current user info
    """
    return current_user


@router.post("/change-password")
async def change_password(
    password_data: PasswordChange,
    current_user: User = Depends(get_current_user),
    db: Session = Depends(get_db)
) -> Any:
    """
    Change current user's password
    """
    # Verify old password
    if not verify_password(password_data.old_password, current_user.password_hash):
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Incorrect password"
        )
    
    # Update password
    current_user.password_hash = get_password_hash(password_data.new_password)
    db.commit()
    
    # Log password change
    log_user_action(
        db=db,
        user=current_user,
        action="PASSWORD_CHANGE"
    )
    
    return {"message": "Password changed successfully"}


@router.get("/users", response_model=list[UserResponse])
async def list_users(
    skip: int = 0,
    limit: int = 100,
    current_user: User = Depends(get_current_active_superuser),
    db: Session = Depends(get_db)
) -> Any:
    """
    List all users (superuser only)
    """
    users = db.query(User).offset(skip).limit(limit).all()
    return users


@router.post("/users", response_model=UserResponse)
async def create_user(
    user_in: UserCreate,
    current_user: User = Depends(get_current_active_superuser),
    db: Session = Depends(get_db)
) -> Any:
    """
    Create new user (superuser only)
    """
    # Check if username exists
    if db.query(User).filter(User.username == user_in.username).first():
        raise HTTPException(
            status_code=400,
            detail="Username already registered"
        )
    
    # Check if email exists
    if db.query(User).filter(User.email == user_in.email).first():
        raise HTTPException(
            status_code=400,
            detail="Email already registered"
        )
    
    # Create user
    user = User(
        username=user_in.username,
        email=user_in.email,
        password_hash=get_password_hash(user_in.password),
        full_name=user_in.full_name,
        role_id=user_in.role_id,
        is_active=user_in.is_active,
        is_superuser=user_in.is_superuser,
        created_by=current_user.username
    )
    
    db.add(user)
    db.commit()
    db.refresh(user)
    
    # Log user creation
    log_user_action(
        db=db,
        user=current_user,
        action="CREATE_USER",
        table="users",
        key=str(user.user_id),
        new_values={"username": user.username, "email": user.email}
    )
    
    return user


@router.put("/users/{user_id}", response_model=UserResponse)
async def update_user(
    user_id: str,
    user_in: UserUpdate,
    current_user: User = Depends(get_current_active_superuser),
    db: Session = Depends(get_db)
) -> Any:
    """
    Update user (superuser only)
    """
    user = db.query(User).filter(User.user_id == user_id).first()
    if not user:
        raise HTTPException(
            status_code=404,
            detail="User not found"
        )
    
    # Track old values for audit
    old_values = {
        "email": user.email,
        "full_name": user.full_name,
        "role_id": user.role_id,
        "is_active": user.is_active
    }
    
    # Update fields
    update_data = user_in.dict(exclude_unset=True)
    for field, value in update_data.items():
        setattr(user, field, value)
    
    db.commit()
    db.refresh(user)
    
    # Log update
    log_user_action(
        db=db,
        user=current_user,
        action="UPDATE_USER",
        table="users",
        key=str(user.user_id),
        old_values=old_values,
        new_values=update_data
    )
    
    return user


@router.delete("/users/{user_id}")
async def delete_user(
    user_id: str,
    current_user: User = Depends(get_current_active_superuser),
    db: Session = Depends(get_db)
) -> Any:
    """
    Delete user (superuser only)
    """
    user = db.query(User).filter(User.user_id == user_id).first()
    if not user:
        raise HTTPException(
            status_code=404,
            detail="User not found"
        )
    
    # Don't allow deleting yourself
    if user.user_id == current_user.user_id:
        raise HTTPException(
            status_code=400,
            detail="Cannot delete your own account"
        )
    
    # Log deletion
    log_user_action(
        db=db,
        user=current_user,
        action="DELETE_USER",
        table="users",
        key=str(user.user_id),
        old_values={"username": user.username, "email": user.email}
    )
    
    db.delete(user)
    db.commit()
    
    return {"message": "User deleted successfully"}


@router.get("/roles")
async def list_roles(
    current_user: User = Depends(get_current_user),
    db: Session = Depends(get_db)
) -> Any:
    """
    List all available roles
    """
    roles = db.query(Role).all()
    return [
        {
            "role_id": role.role_id,
            "role_name": role.role_name,
            "role_description": role.role_description,
            "gl_access": role.gl_access,
            "sl_access": role.sl_access,
            "pl_access": role.pl_access,
            "stock_access": role.stock_access,
            "reports_access": role.reports_access,
            "system_access": role.system_access
        }
        for role in roles
    ]