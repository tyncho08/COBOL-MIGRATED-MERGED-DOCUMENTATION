"""
Authentication Service
User authentication, authorization, and session management
"""

from typing import List, Optional, Dict, Any
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_
from app.models.auth import User, Role, Permission, UserRole, RolePermission
from app.schemas.auth import UserCreate, UserUpdate, RoleCreate, RoleUpdate
from app.core.security import hash_password, verify_password, create_access_token
from datetime import datetime, timedelta
import secrets
import string
import logging

logger = logging.getLogger(__name__)


class AuthService:
    """Service for authentication and user management operations"""
    
    def __init__(self, db: Session):
        self.db = db
    
    # User Management Methods
    
    def get_users(
        self, 
        skip: int = 0, 
        limit: int = 100,
        search: Optional[str] = None,
        role_id: Optional[int] = None,
        is_active: Optional[bool] = None
    ) -> List[User]:
        """Get users with filtering"""
        query = self.db.query(User)
        
        if search:
            search_filter = f"%{search}%"
            query = query.filter(
                or_(
                    User.username.ilike(search_filter),
                    User.email.ilike(search_filter),
                    User.full_name.ilike(search_filter)
                )
            )
        
        if role_id:
            query = query.join(UserRole).filter(UserRole.role_id == role_id)
        
        if is_active is not None:
            query = query.filter(User.is_active == is_active)
        
        return query.offset(skip).limit(limit).all()
    
    def get_user_by_id(self, user_id: int) -> Optional[User]:
        """Get user by ID"""
        return self.db.query(User).filter(User.id == user_id).first()
    
    def get_user_by_username(self, username: str) -> Optional[User]:
        """Get user by username"""
        return self.db.query(User).filter(User.username == username).first()
    
    def get_user_by_email(self, email: str) -> Optional[User]:
        """Get user by email"""
        return self.db.query(User).filter(User.email == email).first()
    
    def create_user(self, user_data: UserCreate) -> User:
        """Create new user"""
        hashed_password = hash_password(user_data.password)
        
        db_user = User(
            username=user_data.username,
            email=user_data.email,
            full_name=user_data.full_name,
            hashed_password=hashed_password,
            is_active=True,
            created_at=datetime.utcnow(),
            updated_at=datetime.utcnow()
        )
        
        self.db.add(db_user)
        self.db.commit()
        self.db.refresh(db_user)
        
        # Assign default role if specified
        if hasattr(user_data, 'default_role_id') and user_data.default_role_id:
            self.assign_role_to_user(db_user.id, user_data.default_role_id)
        
        logger.info(f"User created: {db_user.username}")
        return db_user
    
    def update_user(self, user_id: int, user_data: UserUpdate) -> Optional[User]:
        """Update user information"""
        db_user = self.get_user_by_id(user_id)
        if not db_user:
            return None
        
        update_data = user_data.dict(exclude_unset=True)
        
        # Hash password if provided
        if 'password' in update_data:
            update_data['hashed_password'] = hash_password(update_data.pop('password'))
        
        # Update timestamp
        update_data['updated_at'] = datetime.utcnow()
        
        for field, value in update_data.items():
            setattr(db_user, field, value)
        
        self.db.commit()
        self.db.refresh(db_user)
        
        logger.info(f"User updated: {db_user.username}")
        return db_user
    
    def deactivate_user(self, user_id: int) -> bool:
        """Deactivate user (soft delete)"""
        db_user = self.get_user_by_id(user_id)
        if not db_user:
            return False
        
        db_user.is_active = False
        db_user.updated_at = datetime.utcnow()
        
        self.db.commit()
        
        logger.info(f"User deactivated: {db_user.username}")
        return True
    
    def activate_user(self, user_id: int) -> Optional[User]:
        """Activate user"""
        db_user = self.get_user_by_id(user_id)
        if not db_user:
            return None
        
        db_user.is_active = True
        db_user.updated_at = datetime.utcnow()
        
        self.db.commit()
        self.db.refresh(db_user)
        
        logger.info(f"User activated: {db_user.username}")
        return db_user
    
    def reset_user_password(self, user_id: int) -> str:
        """Reset user password and return new password"""
        db_user = self.get_user_by_id(user_id)
        if not db_user:
            raise ValueError("User not found")
        
        # Generate random password
        new_password = self._generate_password()
        hashed_password = hash_password(new_password)
        
        db_user.hashed_password = hashed_password
        db_user.password_reset_required = True
        db_user.updated_at = datetime.utcnow()
        
        self.db.commit()
        
        # TODO: Send email with new password
        logger.info(f"Password reset for user: {db_user.username}")
        
        return new_password
    
    # Authentication Methods
    
    def authenticate_user(self, username: str, password: str) -> Optional[User]:
        """Authenticate user credentials"""
        user = self.get_user_by_username(username)
        if not user:
            return None
        
        if not user.is_active:
            return None
        
        if not verify_password(password, user.hashed_password):
            return None
        
        # Update last login
        user.last_login = datetime.utcnow()
        self.db.commit()
        
        return user
    
    def create_user_session(self, user: User) -> Dict[str, Any]:
        """Create user session and return token"""
        access_token = create_access_token(
            data={"sub": user.username, "user_id": user.id}
        )
        
        return {
            "access_token": access_token,
            "token_type": "bearer",
            "user": {
                "id": user.id,
                "username": user.username,
                "email": user.email,
                "full_name": user.full_name,
                "roles": [role.name for role in user.roles]
            }
        }
    
    # Role Management Methods
    
    def get_all_roles(self) -> List[Role]:
        """Get all roles"""
        return self.db.query(Role).all()
    
    def get_role_by_id(self, role_id: int) -> Optional[Role]:
        """Get role by ID"""
        return self.db.query(Role).filter(Role.id == role_id).first()
    
    def get_role_by_name(self, name: str) -> Optional[Role]:
        """Get role by name"""
        return self.db.query(Role).filter(Role.name == name).first()
    
    def create_role(self, role_data: RoleCreate) -> Role:
        """Create new role"""
        db_role = Role(
            name=role_data.name,
            description=role_data.description,
            is_active=True,
            created_at=datetime.utcnow(),
            updated_at=datetime.utcnow()
        )
        
        self.db.add(db_role)
        self.db.commit()
        self.db.refresh(db_role)
        
        logger.info(f"Role created: {db_role.name}")
        return db_role
    
    def update_role(self, role_id: int, role_data: RoleUpdate) -> Optional[Role]:
        """Update role information"""
        db_role = self.get_role_by_id(role_id)
        if not db_role:
            return None
        
        update_data = role_data.dict(exclude_unset=True)
        update_data['updated_at'] = datetime.utcnow()
        
        for field, value in update_data.items():
            setattr(db_role, field, value)
        
        self.db.commit()
        self.db.refresh(db_role)
        
        logger.info(f"Role updated: {db_role.name}")
        return db_role
    
    def delete_role(self, role_id: int) -> bool:
        """Delete role"""
        db_role = self.get_role_by_id(role_id)
        if not db_role:
            return False
        
        # Remove role assignments first
        self.db.query(UserRole).filter(UserRole.role_id == role_id).delete()
        self.db.query(RolePermission).filter(RolePermission.role_id == role_id).delete()
        
        self.db.delete(db_role)
        self.db.commit()
        
        logger.info(f"Role deleted: {db_role.name}")
        return True
    
    def is_role_in_use(self, role_id: int) -> bool:
        """Check if role is assigned to any users"""
        count = self.db.query(UserRole).filter(UserRole.role_id == role_id).count()
        return count > 0
    
    # User-Role Assignment Methods
    
    def assign_role_to_user(self, user_id: int, role_id: int) -> Optional[User]:
        """Assign role to user"""
        # Check if assignment already exists
        existing = self.db.query(UserRole).filter(
            and_(UserRole.user_id == user_id, UserRole.role_id == role_id)
        ).first()
        
        if existing:
            return self.get_user_by_id(user_id)
        
        user_role = UserRole(
            user_id=user_id,
            role_id=role_id,
            assigned_at=datetime.utcnow()
        )
        
        self.db.add(user_role)
        self.db.commit()
        
        user = self.get_user_by_id(user_id)
        logger.info(f"Role {role_id} assigned to user {user.username if user else user_id}")
        
        return user
    
    def remove_role_from_user(self, user_id: int, role_id: int) -> Optional[User]:
        """Remove role from user"""
        self.db.query(UserRole).filter(
            and_(UserRole.user_id == user_id, UserRole.role_id == role_id)
        ).delete()
        
        self.db.commit()
        
        user = self.get_user_by_id(user_id)
        logger.info(f"Role {role_id} removed from user {user.username if user else user_id}")
        
        return user
    
    # Permission Management Methods
    
    def get_all_permissions(self) -> List[Permission]:
        """Get all permissions"""
        return self.db.query(Permission).all()
    
    def get_user_permissions(self, user_id: int) -> List[Permission]:
        """Get all permissions for a user through their roles"""
        permissions = self.db.query(Permission).join(RolePermission).join(Role).join(UserRole).filter(
            UserRole.user_id == user_id
        ).distinct().all()
        
        return permissions
    
    def user_has_permission(self, user_id: int, permission_name: str) -> bool:
        """Check if user has specific permission"""
        count = self.db.query(Permission).join(RolePermission).join(Role).join(UserRole).filter(
            and_(
                UserRole.user_id == user_id,
                Permission.name == permission_name
            )
        ).count()
        
        return count > 0
    
    # Audit Methods
    
    def get_user_audit_log(
        self,
        user_id: Optional[int] = None,
        action: Optional[str] = None,
        skip: int = 0,
        limit: int = 100
    ) -> List[Dict[str, Any]]:
        """Get user audit log"""
        # TODO: Implement actual audit log retrieval
        # For now, return sample data
        audit_logs = [
            {
                "id": 1,
                "user_id": user_id or 1,
                "action": "login",
                "timestamp": datetime.utcnow().isoformat(),
                "ip_address": "192.168.1.100",
                "user_agent": "Mozilla/5.0...",
                "details": {"successful": True}
            },
            {
                "id": 2,
                "user_id": user_id or 1,
                "action": "password_change",
                "timestamp": (datetime.utcnow() - timedelta(hours=2)).isoformat(),
                "ip_address": "192.168.1.100",
                "user_agent": "Mozilla/5.0...",
                "details": {"self_initiated": True}
            }
        ]
        
        return audit_logs[skip:skip+limit]
    
    # Utility Methods
    
    def _generate_password(self, length: int = 12) -> str:
        """Generate random password"""
        alphabet = string.ascii_letters + string.digits + "!@#$%^&*"
        password = ''.join(secrets.choice(alphabet) for _ in range(length))
        return password
    
    def validate_password_strength(self, password: str) -> Dict[str, Any]:
        """Validate password strength"""
        validation = {
            "valid": True,
            "errors": [],
            "score": 0
        }
        
        if len(password) < 8:
            validation["errors"].append("Password must be at least 8 characters long")
            validation["valid"] = False
        else:
            validation["score"] += 1
        
        if not any(c.isupper() for c in password):
            validation["errors"].append("Password must contain at least one uppercase letter")
            validation["valid"] = False
        else:
            validation["score"] += 1
        
        if not any(c.islower() for c in password):
            validation["errors"].append("Password must contain at least one lowercase letter")
            validation["valid"] = False
        else:
            validation["score"] += 1
        
        if not any(c.isdigit() for c in password):
            validation["errors"].append("Password must contain at least one number")
            validation["valid"] = False
        else:
            validation["score"] += 1
        
        if not any(c in "!@#$%^&*()_+-=[]{}|;:,.<>?" for c in password):
            validation["errors"].append("Password must contain at least one special character")
            validation["valid"] = False
        else:
            validation["score"] += 1
        
        return validation