"""
Test COBOL File Handlers
Verify exact COBOL logic is preserved
"""
import pytest
from decimal import Decimal
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker

from app.core.database import Base
from app.services.file_handlers import (
    FileHandlerFactory, 
    FileFunction, 
    FileStatus,
    SystemFileHandler,
    CustomerFileHandler,
    StockFileHandler
)
from app.models.system import SystemRec
from app.models.customer import SalesLedgerRec
from app.models.stock import StockRec


@pytest.fixture
def db_session():
    """Create test database session"""
    engine = create_engine("sqlite:///:memory:")
    Base.metadata.create_all(engine)
    SessionLocal = sessionmaker(autocommit=False, autoflush=False, bind=engine)
    session = SessionLocal()
    
    # Create default system record
    system_rec = SystemRec(
        system_rec_key=1,
        vat_rate_1=Decimal("20.00"),
        period=1,
        next_invoice=1000
    )
    session.add(system_rec)
    session.commit()
    
    yield session
    session.close()


def test_system_file_handler(db_session):
    """Test system file handler operations"""
    handler = SystemFileHandler(db_session)
    
    # Test open
    _, status = handler.process(FileFunction.OPEN)
    assert status.fs_reply == FileStatus.SUCCESS
    
    # Test read system params
    record, status = handler.process(FileFunction.READ_INDEXED, key_value=1, key_type=1)
    assert status.fs_reply == FileStatus.SUCCESS
    assert record is not None
    assert record.system_rec_key == 1
    assert record.vat_rate_1 == Decimal("20.00")
    
    # Test invalid key type
    _, status = handler.process(FileFunction.READ_INDEXED, key_value=5, key_type=5)
    assert status.we_error == 998  # Invalid key type
    assert status.fs_reply == "99"
    
    # Test helper methods
    assert handler.get_current_period() == 1
    assert handler.get_vat_rate(1) == 20.0
    
    # Test invoice number increment
    next_num = handler.get_next_invoice_number()
    assert next_num == 1000
    next_num = handler.get_next_invoice_number()
    assert next_num == 1001


def test_customer_file_handler(db_session):
    """Test customer file handler operations"""
    handler = CustomerFileHandler(db_session)
    
    # Open file
    _, status = handler.process(FileFunction.OPEN)
    assert status.fs_reply == FileStatus.SUCCESS
    
    # Create test customer
    customer = SalesLedgerRec(
        sales_key="CUST001",
        sales_name="Test Customer Ltd",
        sales_credit_limit=Decimal("5000.00"),
        sales_balance=Decimal("1000.00"),
        sales_payment_terms="30",
        sales_credit_status="O"
    )
    
    # Write new customer
    _, status = handler.process(FileFunction.WRITE, record=customer)
    assert status.fs_reply == FileStatus.SUCCESS
    
    # Read back by key
    record, status = handler.process(FileFunction.READ_INDEXED, key_value="CUST001", key_type=1)
    assert status.fs_reply == FileStatus.SUCCESS
    assert record.sales_name == "Test Customer Ltd"
    
    # Test duplicate key
    _, status = handler.process(FileFunction.WRITE, record=customer)
    assert status.fs_reply == FileStatus.DUPLICATE_KEY
    assert status.we_error == 903
    
    # Test credit check
    within_limit, available = handler.check_credit_limit("CUST001", 1000)
    assert within_limit is True
    assert available == 3000.0  # 5000 limit - 1000 balance - 1000 additional
    
    # Test balance update
    status = handler.update_balance("CUST001", 500, "ADD")
    assert status.fs_reply == FileStatus.SUCCESS
    
    record, _ = handler.process(FileFunction.READ_INDEXED, key_value="CUST001")
    assert record.sales_balance == Decimal("1500.00")
    assert record.sales_ytd_turnover == Decimal("500.00")
    
    # Test partial key search
    customers = handler.find_by_partial_key("CUST")
    assert len(customers) == 1
    assert customers[0].sales_key == "CUST001"


def test_stock_file_handler(db_session):
    """Test stock file handler operations"""
    handler = StockFileHandler(db_session)
    
    # Open file
    _, status = handler.process(FileFunction.OPEN)
    assert status.fs_reply == FileStatus.SUCCESS
    
    # Create test stock item
    stock = StockRec(
        stock_key="ITEM001",
        stock_desc="Test Product",
        stock_qty_on_hand=Decimal("100.000"),
        stock_qty_allocated=Decimal("0.000"),
        stock_qty_available=Decimal("100.000"),
        stock_avg_cost=Decimal("10.0000"),
        stock_std_cost=Decimal("10.0000"),
        stock_costing_method="A"
    )
    
    # Write stock item
    _, status = handler.process(FileFunction.WRITE, record=stock)
    assert status.fs_reply == FileStatus.SUCCESS
    
    # Check availability
    is_available, qty = handler.check_available_quantity("ITEM001", 50)
    assert is_available is True
    assert qty == 100.0
    
    # Allocate stock
    status = handler.allocate_stock("ITEM001", 30)
    assert status.fs_reply == FileStatus.SUCCESS
    
    # Check updated quantities
    record, _ = handler.process(FileFunction.READ_INDEXED, key_value="ITEM001")
    assert record.stock_qty_allocated == Decimal("30.000")
    assert record.stock_qty_available == Decimal("70.000")
    
    # Process sale
    status = handler.process_sale("ITEM001", 20)
    assert status.fs_reply == FileStatus.SUCCESS
    
    record, _ = handler.process(FileFunction.READ_INDEXED, key_value="ITEM001")
    assert record.stock_qty_on_hand == Decimal("80.000")
    assert record.stock_qty_allocated == Decimal("10.000")
    assert record.stock_mtd_usage == Decimal("20.000")
    
    # Process receipt
    status = handler.process_receipt("ITEM001", 50, 12.00)
    assert status.fs_reply == FileStatus.SUCCESS
    
    record, _ = handler.process(FileFunction.READ_INDEXED, key_value="ITEM001")
    assert record.stock_qty_on_hand == Decimal("130.000")
    assert record.stock_last_cost == Decimal("12.0000")
    # New average cost: (80 * 10 + 50 * 12) / 130 = 10.7692
    assert round(record.stock_avg_cost, 4) == Decimal("10.7692")


def test_sequential_reading(db_session):
    """Test sequential read operations"""
    handler = CustomerFileHandler(db_session)
    handler.process(FileFunction.OPEN)
    
    # Create multiple customers
    for i in range(5):
        customer = SalesLedgerRec(
            sales_key=f"CUST{i:03d}",
            sales_name=f"Customer {i}",
            sales_balance=Decimal("0.00")
        )
        handler.process(FileFunction.WRITE, record=customer)
    
    # Start at beginning
    handler.process(FileFunction.START, key_value="CUST000")
    
    # Read next sequentially
    customers_read = []
    for _ in range(5):
        record, status = handler.process(FileFunction.READ_NEXT)
        if status.fs_reply == FileStatus.SUCCESS:
            customers_read.append(record.sales_key)
    
    assert customers_read == ["CUST001", "CUST002", "CUST003", "CUST004"]
    
    # Next read should hit EOF
    _, status = handler.process(FileFunction.READ_NEXT)
    assert status.fs_reply == FileStatus.END_OF_FILE
    
    # Test READ_GREATER
    record, status = handler.process(FileFunction.READ_GREATER, key_value="CUST002")
    assert status.fs_reply == FileStatus.SUCCESS
    assert record.sales_key == "CUST003"


def test_file_handler_factory(db_session):
    """Test file handler factory"""
    # Get system handler
    handler = FileHandlerFactory.get_handler(0, db_session)
    assert isinstance(handler, SystemFileHandler)
    
    # Get customer handler
    handler = FileHandlerFactory.get_handler(4, db_session)
    assert isinstance(handler, CustomerFileHandler)
    
    # Get stock handler
    handler = FileHandlerFactory.get_handler(11, db_session)
    assert isinstance(handler, StockFileHandler)
    
    # Invalid file number
    handler = FileHandlerFactory.get_handler(999, db_session)
    assert handler is None


if __name__ == "__main__":
    pytest.main([__file__, "-v"])