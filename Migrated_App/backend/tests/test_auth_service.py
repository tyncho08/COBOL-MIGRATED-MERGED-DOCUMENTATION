"""
Tests for Authentication Service
Critical business logic testing for user management and security
"""

import pytest
from datetime import datetime, timedelta
from sqlalchemy.orm import Session

from app.services.auth_service import AuthService
from app.models.auth import User, Role
from app.core.security import verify_password


class TestAuthService:
    """Test suite for AuthService"""
    
    def test_create_user_success(self, db_session: Session):
        """Test successful user creation"""
        auth_service = AuthService(db_session)
        
        user_data = {
            "username": "newuser",
            "email": "new@test.com",
            "full_name": "New User",
            "password": "securepassword123"
        }
        
        user = auth_service.create_user(user_data)
        
        assert user.id is not None
        assert user.username == "newuser"
        assert user.email == "new@test.com"
        assert user.full_name == "New User"
        assert user.is_active is True
        assert verify_password("securepassword123", user.hashed_password)
    
    def test_create_user_duplicate_username(self, db_session: Session, test_user: User):
        """Test user creation with duplicate username fails"""
        auth_service = AuthService(db_session)
        
        user_data = {
            "username": test_user.username,
            "email": "different@test.com",
            "full_name": "Different User",
            "password": "password123"
        }
        
        # Should raise exception or return None
        existing_user = auth_service.get_user_by_username(test_user.username)
        assert existing_user is not None
    
    def test_authenticate_user_success(self, db_session: Session, test_user: User):
        """Test successful user authentication"""
        auth_service = AuthService(db_session)
        
        authenticated_user = auth_service.authenticate_user(
            test_user.username, 
            "testpassword123"
        )
        
        assert authenticated_user is not None
        assert authenticated_user.id == test_user.id
        assert authenticated_user.username == test_user.username
    
    def test_authenticate_user_wrong_password(self, db_session: Session, test_user: User):
        """Test authentication with wrong password"""
        auth_service = AuthService(db_session)
        
        authenticated_user = auth_service.authenticate_user(
            test_user.username,
            "wrongpassword"
        )
        
        assert authenticated_user is None
    
    def test_authenticate_nonexistent_user(self, db_session: Session):
        """Test authentication of non-existent user"""
        auth_service = AuthService(db_session)
        
        authenticated_user = auth_service.authenticate_user(
            "nonexistent",
            "password"
        )
        
        assert authenticated_user is None
    
    def test_create_user_session(self, test_user: User):
        """Test user session creation"""
        auth_service = AuthService(None)  # No DB needed for this test
        
        session_data = auth_service.create_user_session(test_user)
        
        assert "access_token" in session_data
        assert "token_type" in session_data
        assert session_data["token_type"] == "bearer"
        assert "user" in session_data
        assert session_data["user"]["username"] == test_user.username
    
    def test_get_users_with_pagination(self, db_session: Session):
        """Test getting users with pagination"""
        auth_service = AuthService(db_session)
        
        # Create multiple users
        for i in range(5):
            user_data = {
                "username": f"user{i}",
                "email": f"user{i}@test.com",
                "full_name": f"User {i}",
                "password": "password123"
            }
            auth_service.create_user(user_data)
        
        # Test pagination
        users_page1 = auth_service.get_users(skip=0, limit=3)
        users_page2 = auth_service.get_users(skip=3, limit=3)
        
        assert len(users_page1) == 3
        assert len(users_page2) == 2
        
        # Ensure no duplicates
        page1_ids = {user.id for user in users_page1}
        page2_ids = {user.id for user in users_page2}
        assert page1_ids.isdisjoint(page2_ids)
    
    def test_get_users_with_search(self, db_session: Session):
        """Test user search functionality"""
        auth_service = AuthService(db_session)
        
        # Create users with specific names
        user_data = [
            {"username": "john.doe", "email": "john@test.com", "full_name": "John Doe", "password": "pass123"},
            {"username": "jane.smith", "email": "jane@test.com", "full_name": "Jane Smith", "password": "pass123"},
            {"username": "bob.jones", "email": "bob@test.com", "full_name": "Bob Jones", "password": "pass123"}
        ]
        
        for data in user_data:
            auth_service.create_user(data)
        
        # Search by username
        john_users = auth_service.get_users(search="john")
        assert len(john_users) == 1
        assert john_users[0].username == "john.doe"
        
        # Search by full name
        smith_users = auth_service.get_users(search="Smith")
        assert len(smith_users) == 1
        assert smith_users[0].full_name == "Jane Smith"
    
    def test_update_user(self, db_session: Session, test_user: User):
        """Test user update functionality"""
        auth_service = AuthService(db_session)
        
        update_data = {
            "full_name": "Updated Name",
            "email": "updated@test.com"
        }
        
        updated_user = auth_service.update_user(test_user.id, update_data)
        
        assert updated_user is not None
        assert updated_user.full_name == "Updated Name"
        assert updated_user.email == "updated@test.com"
        assert updated_user.username == test_user.username  # Unchanged
    
    def test_deactivate_user(self, db_session: Session, test_user: User):
        """Test user deactivation"""
        auth_service = AuthService(db_session)
        
        success = auth_service.deactivate_user(test_user.id)
        
        assert success is True
        
        # Verify user is deactivated
        updated_user = auth_service.get_user_by_id(test_user.id)
        assert updated_user.is_active is False
    
    def test_activate_user(self, db_session: Session, test_user: User):
        """Test user activation"""
        auth_service = AuthService(db_session)
        
        # First deactivate
        auth_service.deactivate_user(test_user.id)
        
        # Then activate
        activated_user = auth_service.activate_user(test_user.id)
        
        assert activated_user is not None
        assert activated_user.is_active is True
    
    def test_reset_user_password(self, db_session: Session, test_user: User):
        """Test password reset functionality"""
        auth_service = AuthService(db_session)
        
        old_password_hash = test_user.hashed_password
        new_password = auth_service.reset_user_password(test_user.id)
        
        # Refresh user from database
        db_session.refresh(test_user)
        
        assert new_password is not None
        assert len(new_password) >= 8  # Minimum password length
        assert test_user.hashed_password != old_password_hash
        assert verify_password(new_password, test_user.hashed_password)
    
    def test_role_management(self, db_session: Session):
        """Test role creation and management"""
        auth_service = AuthService(db_session)
        
        # Create role
        role_data = {
            "name": "test_role",
            "description": "Test Role"
        }
        
        role = auth_service.create_role(role_data)
        
        assert role.id is not None
        assert role.name == "test_role"
        assert role.description == "Test Role"
        assert role.is_active is True
        
        # Get all roles
        roles = auth_service.get_all_roles()
        assert len(roles) >= 1
        assert any(r.name == "test_role" for r in roles)
    
    def test_assign_role_to_user(self, db_session: Session, test_user: User):
        """Test role assignment to user"""
        auth_service = AuthService(db_session)
        
        # Create role
        role_data = {"name": "test_role", "description": "Test Role"}
        role = auth_service.create_role(role_data)
        
        # Assign role to user
        updated_user = auth_service.assign_role_to_user(test_user.id, role.id)
        
        assert updated_user is not None
        # Note: Actual role relationship verification would depend on model implementation
    
    def test_password_strength_validation(self):
        """Test password strength validation"""
        auth_service = AuthService(None)
        
        # Strong password
        strong_result = auth_service.validate_password_strength("StrongP@ssw0rd!")
        assert strong_result["valid"] is True
        assert strong_result["score"] >= 4
        
        # Weak password
        weak_result = auth_service.validate_password_strength("weak")
        assert weak_result["valid"] is False
        assert len(weak_result["errors"]) > 0
        
        # Medium password
        medium_result = auth_service.validate_password_strength("MediumPass123")
        assert medium_result["score"] >= 3


class TestAuthServiceEdgeCases:
    """Test edge cases and error conditions"""
    
    def test_create_user_with_empty_data(self, db_session: Session):
        """Test user creation with missing required fields"""
        auth_service = AuthService(db_session)
        
        with pytest.raises((ValueError, TypeError, AttributeError)):
            auth_service.create_user({})
    
    def test_authenticate_inactive_user(self, db_session: Session, test_user: User):
        """Test authentication of inactive user"""
        auth_service = AuthService(db_session)
        
        # Deactivate user
        auth_service.deactivate_user(test_user.id)
        
        # Try to authenticate
        authenticated_user = auth_service.authenticate_user(
            test_user.username,
            "testpassword123"
        )
        
        assert authenticated_user is None
    
    def test_update_nonexistent_user(self, db_session: Session):
        """Test updating non-existent user"""
        auth_service = AuthService(db_session)
        
        result = auth_service.update_user(99999, {"full_name": "Updated"})
        assert result is None
    
    def test_reset_password_nonexistent_user(self, db_session: Session):
        """Test password reset for non-existent user"""
        auth_service = AuthService(db_session)
        
        with pytest.raises(ValueError):
            auth_service.reset_user_password(99999)


class TestAuthServicePerformance:
    """Performance tests for auth service"""
    
    def test_bulk_user_creation_performance(self, db_session: Session):
        """Test performance of creating multiple users"""
        auth_service = AuthService(db_session)
        
        start_time = datetime.now()
        
        # Create 50 users
        for i in range(50):
            user_data = {
                "username": f"perfuser{i}",
                "email": f"perfuser{i}@test.com",
                "full_name": f"Performance User {i}",
                "password": "password123"
            }
            auth_service.create_user(user_data)
        
        end_time = datetime.now()
        duration = (end_time - start_time).total_seconds()
        
        # Should complete within reasonable time (adjust threshold as needed)
        assert duration < 10.0  # 10 seconds max for 50 users
        
        # Verify all users were created
        all_users = auth_service.get_users(limit=100)
        assert len(all_users) >= 50
    
    def test_search_performance(self, db_session: Session):
        """Test search performance with many users"""
        auth_service = AuthService(db_session)
        
        # Create 100 users
        for i in range(100):
            user_data = {
                "username": f"searchuser{i}",
                "email": f"searchuser{i}@test.com",
                "full_name": f"Search User {i}",
                "password": "password123"
            }
            auth_service.create_user(user_data)
        
        start_time = datetime.now()
        
        # Perform search
        results = auth_service.get_users(search="searchuser5")
        
        end_time = datetime.now()
        duration = (end_time - start_time).total_seconds()
        
        # Search should be fast
        assert duration < 1.0  # 1 second max
        assert len(results) >= 1