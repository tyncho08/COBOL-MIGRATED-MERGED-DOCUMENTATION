"""
Admin Users API endpoints
User management, roles, permissions
"""

from typing import List, Optional
from fastapi import APIRouter, Depends, HTTPException, status, Query
from sqlalchemy.orm import Session
from app.core.database import get_db
from app.core.security import get_current_user, require_admin
from app.models.auth import User, Role, Permission
from app.schemas.auth import (
    UserResponse, UserCreate, UserUpdate, 
    RoleResponse, RoleCreate, RoleUpdate,
    PermissionResponse, UserRoleAssignment
)
from app.services.auth_service import AuthService
import logging

logger = logging.getLogger(__name__)

router = APIRouter()

# User Management Endpoints

@router.get("/", response_model=List[UserResponse])
async def list_users(
    skip: int = Query(0, ge=0),
    limit: int = Query(100, ge=1, le=1000),
    search: Optional[str] = Query(None),
    role_id: Optional[int] = Query(None),
    is_active: Optional[bool] = Query(None),
    db: Session = Depends(get_db),
    current_user: User = Depends(require_admin)
):
    """
    List all users with filtering and pagination
    Requires admin privileges
    """
    try:
        auth_service = AuthService(db)
        users = auth_service.get_users(
            skip=skip, 
            limit=limit, 
            search=search,
            role_id=role_id,
            is_active=is_active
        )
        return users
        
    except Exception as e:
        logger.error(f"Error listing users: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to retrieve users"
        )

@router.get("/{user_id}", response_model=UserResponse)
async def get_user(
    user_id: int,
    db: Session = Depends(get_db),
    current_user: User = Depends(require_admin)
):
    """
    Get user by ID
    Requires admin privileges
    """
    try:
        auth_service = AuthService(db)
        user = auth_service.get_user_by_id(user_id)
        
        if not user:
            raise HTTPException(
                status_code=status.HTTP_404_NOT_FOUND,
                detail="User not found"
            )
        
        return user
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error getting user {user_id}: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to retrieve user"
        )

@router.post("/", response_model=UserResponse, status_code=status.HTTP_201_CREATED)
async def create_user(
    user_data: UserCreate,
    db: Session = Depends(get_db),
    current_user: User = Depends(require_admin)
):
    """
    Create new user
    Requires admin privileges
    """
    try:
        auth_service = AuthService(db)
        
        # Check if user already exists
        if auth_service.get_user_by_username(user_data.username):
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail="Username already exists"
            )
        
        if auth_service.get_user_by_email(user_data.email):
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail="Email already exists"
            )
        
        user = auth_service.create_user(user_data)
        logger.info(f"User created: {user.username} by admin {current_user.username}")
        
        return user
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error creating user: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to create user"
        )

@router.put("/{user_id}", response_model=UserResponse)
async def update_user(
    user_id: int,
    user_data: UserUpdate,
    db: Session = Depends(get_db),
    current_user: User = Depends(require_admin)
):
    """
    Update user information
    Requires admin privileges
    """
    try:
        auth_service = AuthService(db)
        
        user = auth_service.get_user_by_id(user_id)
        if not user:
            raise HTTPException(
                status_code=status.HTTP_404_NOT_FOUND,
                detail="User not found"
            )
        
        # Check for duplicate username/email if changing
        if user_data.username and user_data.username != user.username:
            if auth_service.get_user_by_username(user_data.username):
                raise HTTPException(
                    status_code=status.HTTP_400_BAD_REQUEST,
                    detail="Username already exists"
                )
        
        if user_data.email and user_data.email != user.email:
            if auth_service.get_user_by_email(user_data.email):
                raise HTTPException(
                    status_code=status.HTTP_400_BAD_REQUEST,
                    detail="Email already exists"
                )
        
        updated_user = auth_service.update_user(user_id, user_data)
        logger.info(f"User updated: {updated_user.username} by admin {current_user.username}")
        
        return updated_user
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error updating user {user_id}: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to update user"
        )

@router.delete("/{user_id}", status_code=status.HTTP_204_NO_CONTENT)
async def delete_user(
    user_id: int,
    db: Session = Depends(get_db),
    current_user: User = Depends(require_admin)
):
    """
    Delete user (soft delete - deactivate)
    Requires admin privileges
    """
    try:
        auth_service = AuthService(db)
        
        if user_id == current_user.id:
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail="Cannot delete your own account"
            )
        
        user = auth_service.get_user_by_id(user_id)
        if not user:
            raise HTTPException(
                status_code=status.HTTP_404_NOT_FOUND,
                detail="User not found"
            )
        
        auth_service.deactivate_user(user_id)
        logger.info(f"User deactivated: {user.username} by admin {current_user.username}")
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error deleting user {user_id}: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to delete user"
        )

@router.post("/{user_id}/activate", response_model=UserResponse)
async def activate_user(
    user_id: int,
    db: Session = Depends(get_db),
    current_user: User = Depends(require_admin)
):
    """
    Activate user account
    Requires admin privileges
    """
    try:
        auth_service = AuthService(db)
        
        user = auth_service.get_user_by_id(user_id)
        if not user:
            raise HTTPException(
                status_code=status.HTTP_404_NOT_FOUND,
                detail="User not found"
            )
        
        activated_user = auth_service.activate_user(user_id)
        logger.info(f"User activated: {activated_user.username} by admin {current_user.username}")
        
        return activated_user
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error activating user {user_id}: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to activate user"
        )

@router.post("/{user_id}/reset-password")
async def reset_user_password(
    user_id: int,
    db: Session = Depends(get_db),
    current_user: User = Depends(require_admin)
):
    """
    Reset user password and send new password via email
    Requires admin privileges
    """
    try:
        auth_service = AuthService(db)
        
        user = auth_service.get_user_by_id(user_id)
        if not user:
            raise HTTPException(
                status_code=status.HTTP_404_NOT_FOUND,
                detail="User not found"
            )
        
        new_password = auth_service.reset_user_password(user_id)
        logger.info(f"Password reset for user: {user.username} by admin {current_user.username}")
        
        return {"message": "Password reset successfully. New password sent to user's email."}
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error resetting password for user {user_id}: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to reset password"
        )

# Role Management Endpoints

@router.get("/roles/", response_model=List[RoleResponse])
async def list_roles(
    db: Session = Depends(get_db),
    current_user: User = Depends(require_admin)
):
    """
    List all roles
    Requires admin privileges
    """
    try:
        auth_service = AuthService(db)
        roles = auth_service.get_all_roles()
        return roles
        
    except Exception as e:
        logger.error(f"Error listing roles: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to retrieve roles"
        )

@router.post("/roles/", response_model=RoleResponse, status_code=status.HTTP_201_CREATED)
async def create_role(
    role_data: RoleCreate,
    db: Session = Depends(get_db),
    current_user: User = Depends(require_admin)
):
    """
    Create new role
    Requires admin privileges
    """
    try:
        auth_service = AuthService(db)
        
        # Check if role already exists
        if auth_service.get_role_by_name(role_data.name):
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail="Role name already exists"
            )
        
        role = auth_service.create_role(role_data)
        logger.info(f"Role created: {role.name} by admin {current_user.username}")
        
        return role
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error creating role: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to create role"
        )

@router.put("/roles/{role_id}", response_model=RoleResponse)
async def update_role(
    role_id: int,
    role_data: RoleUpdate,
    db: Session = Depends(get_db),
    current_user: User = Depends(require_admin)
):
    """
    Update role information
    Requires admin privileges
    """
    try:
        auth_service = AuthService(db)
        
        role = auth_service.get_role_by_id(role_id)
        if not role:
            raise HTTPException(
                status_code=status.HTTP_404_NOT_FOUND,
                detail="Role not found"
            )
        
        updated_role = auth_service.update_role(role_id, role_data)
        logger.info(f"Role updated: {updated_role.name} by admin {current_user.username}")
        
        return updated_role
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error updating role {role_id}: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to update role"
        )

@router.delete("/roles/{role_id}", status_code=status.HTTP_204_NO_CONTENT)
async def delete_role(
    role_id: int,
    db: Session = Depends(get_db),
    current_user: User = Depends(require_admin)
):
    """
    Delete role
    Requires admin privileges
    """
    try:
        auth_service = AuthService(db)
        
        role = auth_service.get_role_by_id(role_id)
        if not role:
            raise HTTPException(
                status_code=status.HTTP_404_NOT_FOUND,
                detail="Role not found"
            )
        
        # Check if role is in use
        if auth_service.is_role_in_use(role_id):
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail="Cannot delete role that is assigned to users"
            )
        
        auth_service.delete_role(role_id)
        logger.info(f"Role deleted: {role.name} by admin {current_user.username}")
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error deleting role {role_id}: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to delete role"
        )

# User-Role Assignment Endpoints

@router.post("/{user_id}/roles", response_model=UserResponse)
async def assign_role_to_user(
    user_id: int,
    assignment: UserRoleAssignment,
    db: Session = Depends(get_db),
    current_user: User = Depends(require_admin)
):
    """
    Assign role to user
    Requires admin privileges
    """
    try:
        auth_service = AuthService(db)
        
        user = auth_service.get_user_by_id(user_id)
        if not user:
            raise HTTPException(
                status_code=status.HTTP_404_NOT_FOUND,
                detail="User not found"
            )
        
        role = auth_service.get_role_by_id(assignment.role_id)
        if not role:
            raise HTTPException(
                status_code=status.HTTP_404_NOT_FOUND,
                detail="Role not found"
            )
        
        updated_user = auth_service.assign_role_to_user(user_id, assignment.role_id)
        logger.info(f"Role {role.name} assigned to user {user.username} by admin {current_user.username}")
        
        return updated_user
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error assigning role to user {user_id}: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to assign role"
        )

@router.delete("/{user_id}/roles/{role_id}", response_model=UserResponse)
async def remove_role_from_user(
    user_id: int,
    role_id: int,
    db: Session = Depends(get_db),
    current_user: User = Depends(require_admin)
):
    """
    Remove role from user
    Requires admin privileges
    """
    try:
        auth_service = AuthService(db)
        
        user = auth_service.get_user_by_id(user_id)
        if not user:
            raise HTTPException(
                status_code=status.HTTP_404_NOT_FOUND,
                detail="User not found"
            )
        
        updated_user = auth_service.remove_role_from_user(user_id, role_id)
        logger.info(f"Role {role_id} removed from user {user.username} by admin {current_user.username}")
        
        return updated_user
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error removing role from user {user_id}: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to remove role"
        )

# Permission Management Endpoints

@router.get("/permissions/", response_model=List[PermissionResponse])
async def list_permissions(
    db: Session = Depends(get_db),
    current_user: User = Depends(require_admin)
):
    """
    List all permissions
    Requires admin privileges
    """
    try:
        auth_service = AuthService(db)
        permissions = auth_service.get_all_permissions()
        return permissions
        
    except Exception as e:
        logger.error(f"Error listing permissions: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to retrieve permissions"
        )

@router.get("/audit-log", response_model=List[dict])
async def get_user_audit_log(
    user_id: Optional[int] = Query(None),
    action: Optional[str] = Query(None),
    skip: int = Query(0, ge=0),
    limit: int = Query(100, ge=1, le=1000),
    db: Session = Depends(get_db),
    current_user: User = Depends(require_admin)
):
    """
    Get user audit log
    Requires admin privileges
    """
    try:
        auth_service = AuthService(db)
        audit_logs = auth_service.get_user_audit_log(
            user_id=user_id,
            action=action,
            skip=skip,
            limit=limit
        )
        return audit_logs
        
    except Exception as e:
        logger.error(f"Error getting audit log: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to retrieve audit log"
        )