"""
Test Configuration and Fixtures
Shared testing infrastructure for ACAS
"""

import pytest
import asyncio
from typing import Generator, Dict, Any
from fastapi.testclient import TestClient
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker, Session
from sqlalchemy.pool import StaticPool

from app.main import app
from app.core.database import get_db, Base
from app.core.config import settings
from app.models.auth import User, Role
from app.services.auth_service import AuthService

# Test database URL - using SQLite for tests
TEST_DATABASE_URL = "sqlite:///./test_acas.db"

# Create test engine
engine = create_engine(
    TEST_DATABASE_URL,
    connect_args={"check_same_thread": False},
    poolclass=StaticPool,
)

# Create test session
TestingSessionLocal = sessionmaker(autocommit=False, autoflush=False, bind=engine)

@pytest.fixture(scope="session")
def event_loop():
    """Create an instance of the default event loop for the test session."""
    loop = asyncio.get_event_loop_policy().new_event_loop()
    yield loop
    loop.close()

@pytest.fixture(scope="function")
def db_session() -> Generator[Session, None, None]:
    """Create a fresh database session for each test"""
    # Create all tables
    Base.metadata.create_all(bind=engine)
    
    session = TestingSessionLocal()
    try:
        yield session
    finally:
        session.close()
        # Drop all tables after test
        Base.metadata.drop_all(bind=engine)

@pytest.fixture(scope="function")
def client(db_session: Session) -> Generator[TestClient, None, None]:
    """Create a test client with database dependency override"""
    def override_get_db():
        try:
            yield db_session
        finally:
            pass
    
    app.dependency_overrides[get_db] = override_get_db
    with TestClient(app) as test_client:
        yield test_client
    app.dependency_overrides.clear()

@pytest.fixture
def test_user(db_session: Session) -> User:
    """Create a test user"""
    auth_service = AuthService(db_session)
    
    user_data = {
        "username": "testuser",
        "email": "test@acas.com",
        "full_name": "Test User",
        "password": "testpassword123"
    }
    
    user = auth_service.create_user(user_data)
    return user

@pytest.fixture
def test_admin_user(db_session: Session) -> User:
    """Create a test admin user"""
    auth_service = AuthService(db_session)
    
    # Create admin role
    admin_role = Role(
        name="admin",
        description="Administrator role",
        is_active=True
    )
    db_session.add(admin_role)
    db_session.commit()
    
    user_data = {
        "username": "admin",
        "email": "admin@acas.com", 
        "full_name": "Admin User",
        "password": "adminpassword123"
    }
    
    user = auth_service.create_user(user_data)
    user.is_superuser = True
    db_session.commit()
    
    return user

@pytest.fixture
def auth_headers(client: TestClient, test_user: User) -> Dict[str, str]:
    """Get authentication headers for test user"""
    login_data = {
        "username": test_user.username,
        "password": "testpassword123"
    }
    
    response = client.post("/api/v1/auth/login", data=login_data)
    assert response.status_code == 200
    
    token = response.json()["access_token"]
    return {"Authorization": f"Bearer {token}"}

@pytest.fixture
def admin_auth_headers(client: TestClient, test_admin_user: User) -> Dict[str, str]:
    """Get authentication headers for admin user"""
    login_data = {
        "username": test_admin_user.username,
        "password": "adminpassword123"
    }
    
    response = client.post("/api/v1/auth/login", data=login_data)
    assert response.status_code == 200
    
    token = response.json()["access_token"]
    return {"Authorization": f"Bearer {token}"}

@pytest.fixture
def sample_customer_data() -> Dict[str, Any]:
    """Sample customer data for testing"""
    return {
        "customer_code": "CUST01",
        "customer_name": "Test Customer Ltd",
        "address_line1": "123 Test Street",
        "city": "Test City",
        "postal_code": "TE5T 1NG",
        "country_code": "GB",
        "phone_number": "01234567890",
        "email_address": "test@customer.com",
        "credit_limit": 5000.00,
        "payment_terms": "30",
        "status": "active"
    }

@pytest.fixture
def sample_supplier_data() -> Dict[str, Any]:
    """Sample supplier data for testing"""
    return {
        "supplier_code": "SUPP01",
        "supplier_name": "Test Supplier Ltd",
        "address_line1": "456 Supplier Road",
        "city": "Supplier City", 
        "postal_code": "SU5P 1LR",
        "country_code": "GB",
        "phone_number": "01234567891",
        "email_address": "test@supplier.com",
        "payment_terms": "30",
        "status": "active"
    }

@pytest.fixture
def sample_stock_item_data() -> Dict[str, Any]:
    """Sample stock item data for testing"""
    return {
        "item_code": "ITEM01",
        "description": "Test Stock Item",
        "unit_of_measure": "EA",
        "standard_cost": 10.00,
        "selling_price": 15.00,
        "quantity_on_hand": 100,
        "reorder_level": 10,
        "reorder_quantity": 50,
        "costing_method": "FIFO",
        "status": "active"
    }

@pytest.fixture
def sample_invoice_data(sample_customer_data: Dict[str, Any]) -> Dict[str, Any]:
    """Sample invoice data for testing"""
    return {
        "customer_code": sample_customer_data["customer_code"],
        "invoice_date": "2024-01-26",
        "due_date": "2024-02-25",
        "subtotal": 100.00,
        "tax_amount": 20.00,
        "total_amount": 120.00,
        "lines": [
            {
                "item_code": "ITEM01",
                "description": "Test Item",
                "quantity": 2,
                "unit_price": 50.00,
                "line_total": 100.00
            }
        ]
    }

# Performance testing fixtures
@pytest.fixture
def performance_test_data():
    """Generate data for performance testing"""
    customers = []
    for i in range(100):
        customers.append({
            "customer_code": f"PERF{i:03d}",
            "customer_name": f"Performance Test Customer {i}",
            "address_line1": f"{i} Performance Street",
            "city": "Test City",
            "postal_code": "PE5F T5T",
            "country_code": "GB",
            "credit_limit": 1000.00 * (i + 1),
            "status": "active"
        })
    return customers

# Database test helpers
class DatabaseTestHelper:
    """Helper class for database operations in tests"""
    
    @staticmethod
    def count_records(db_session: Session, model_class) -> int:
        """Count records in a table"""
        return db_session.query(model_class).count()
    
    @staticmethod
    def create_test_gl_accounts(db_session: Session, count: int = 10):
        """Create test GL accounts"""
        from app.models.gl_accounts import GLAccount
        from decimal import Decimal
        
        accounts = []
        for i in range(count):
            account = GLAccount(
                account_code=f"1000{i:04d}",
                account_name=f"Test Account {i}",
                account_type="ASSET",
                account_category="CURRENT_ASSET",
                normal_balance="DEBIT",
                status="active",
                opening_balance=Decimal("1000.00"),
                ytd_debits=Decimal("0.00"),
                ytd_credits=Decimal("0.00")
            )
            accounts.append(account)
        
        db_session.add_all(accounts)
        db_session.commit()
        return accounts

# API test helpers
class APITestHelper:
    """Helper class for API testing"""
    
    @staticmethod
    def assert_error_response(response, expected_status: int, expected_detail: str = None):
        """Assert error response format"""
        assert response.status_code == expected_status
        data = response.json()
        assert "detail" in data
        if expected_detail:
            assert expected_detail in data["detail"]
    
    @staticmethod
    def assert_success_response(response, expected_keys: list = None):
        """Assert successful response format"""
        assert response.status_code in [200, 201]
        data = response.json()
        if expected_keys:
            for key in expected_keys:
                assert key in data

# Mock data generators
class MockDataGenerator:
    """Generate mock data for testing"""
    
    @staticmethod
    def generate_cobol_customer_record() -> bytes:
        """Generate mock COBOL customer record"""
        # Fixed-width COBOL record - 300 bytes
        record = b"CUST01" + b" " * 294  # Customer code + padding
        return record[:300]
    
    @staticmethod
    def generate_cobol_supplier_record() -> bytes:
        """Generate mock COBOL supplier record"""
        # Fixed-width COBOL record - 324 bytes  
        record = b"SUPP01" + b" " * 318  # Supplier code + padding
        return record[:324]