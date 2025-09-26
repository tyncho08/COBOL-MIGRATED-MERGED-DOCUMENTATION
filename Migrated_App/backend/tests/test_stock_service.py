"""
Tests for Stock Control Services
Critical business logic testing for inventory management
"""

import pytest
from decimal import Decimal
from datetime import datetime, date
from sqlalchemy.orm import Session

from app.services.stock.stock_master import StockMasterService
from app.services.stock.stock_movements import StockMovementService
from app.services.stock.stock_valuation import StockValuationService
from app.models.stock import StockItem, StockMovement


class TestStockMasterService:
    """Test suite for StockMasterService"""
    
    def test_create_stock_item_success(self, db_session: Session):
        """Test successful stock item creation"""
        service = StockMasterService(db_session)
        
        item_data = {
            "item_code": "TEST001",
            "description": "Test Item",
            "unit_of_measure": "EA",
            "standard_cost": Decimal("10.00"),
            "selling_price": Decimal("15.00"),
            "quantity_on_hand": 100,
            "reorder_level": 10,
            "reorder_quantity": 50,
            "costing_method": "FIFO",
            "status": "active"
        }
        
        item = service.create_item(item_data, user_id=1)
        
        assert item.id is not None
        assert item.item_code == "TEST001"
        assert item.description == "Test Item"
        assert item.standard_cost == Decimal("10.00")
        assert item.selling_price == Decimal("15.00")
        assert item.quantity_on_hand == 100
        assert item.costing_method == "FIFO"
        assert item.status == "active"
    
    def test_create_item_duplicate_code(self, db_session: Session, sample_stock_item_data):
        """Test creating item with duplicate code fails"""
        service = StockMasterService(db_session)
        
        # Create first item
        service.create_item(sample_stock_item_data, user_id=1)
        
        # Try to create duplicate
        with pytest.raises(ValueError, match="Item code .* already exists"):
            service.create_item(sample_stock_item_data, user_id=1)
    
    def test_get_item_by_code(self, db_session: Session, sample_stock_item_data):
        """Test retrieving item by code"""
        service = StockMasterService(db_session)
        
        # Create item
        created_item = service.create_item(sample_stock_item_data, user_id=1)
        
        # Retrieve by code
        retrieved_item = service.get_item_by_code(sample_stock_item_data["item_code"])
        
        assert retrieved_item is not None
        assert retrieved_item.id == created_item.id
        assert retrieved_item.item_code == sample_stock_item_data["item_code"]
    
    def test_update_stock_item(self, db_session: Session, sample_stock_item_data):
        """Test updating stock item"""
        service = StockMasterService(db_session)
        
        # Create item
        item = service.create_item(sample_stock_item_data, user_id=1)
        
        # Update item
        update_data = {
            "description": "Updated Description",
            "selling_price": Decimal("20.00"),
            "reorder_level": 15
        }
        
        updated_item = service.update_item(item.id, update_data, user_id=1)
        
        assert updated_item.description == "Updated Description"
        assert updated_item.selling_price == Decimal("20.00")
        assert updated_item.reorder_level == 15
        assert updated_item.item_code == sample_stock_item_data["item_code"]  # Unchanged
    
    def test_calculate_item_value(self, db_session: Session, sample_stock_item_data):
        """Test item value calculation"""
        service = StockMasterService(db_session)
        
        item_data = sample_stock_item_data.copy()
        item_data["quantity_on_hand"] = 100
        item_data["standard_cost"] = Decimal("10.00")
        
        item = service.create_item(item_data, user_id=1)
        
        total_value = service.calculate_item_value(item.id)
        expected_value = Decimal("100") * Decimal("10.00")
        
        assert total_value == expected_value
    
    def test_check_reorder_requirements(self, db_session: Session, sample_stock_item_data):
        """Test reorder level checking"""
        service = StockMasterService(db_session)
        
        # Create item below reorder level
        item_data = sample_stock_item_data.copy()
        item_data["quantity_on_hand"] = 5  # Below reorder level of 10
        item_data["reorder_level"] = 10
        
        item = service.create_item(item_data, user_id=1)
        
        reorder_items = service.get_items_requiring_reorder()
        
        assert len(reorder_items) >= 1
        assert any(item.id == reorder_item.id for reorder_item in reorder_items)
    
    def test_get_items_by_status(self, db_session: Session):
        """Test filtering items by status"""
        service = StockMasterService(db_session)
        
        # Create active item
        active_data = {
            "item_code": "ACTIVE01",
            "description": "Active Item",
            "unit_of_measure": "EA",
            "standard_cost": Decimal("10.00"),
            "status": "active"
        }
        service.create_item(active_data, user_id=1)
        
        # Create inactive item
        inactive_data = {
            "item_code": "INACTIVE01",
            "description": "Inactive Item", 
            "unit_of_measure": "EA",
            "standard_cost": Decimal("10.00"),
            "status": "inactive"
        }
        service.create_item(inactive_data, user_id=1)
        
        # Test filtering
        active_items = service.get_items_by_status("active")
        inactive_items = service.get_items_by_status("inactive")
        
        assert len(active_items) >= 1
        assert len(inactive_items) >= 1
        assert all(item.status == "active" for item in active_items)
        assert all(item.status == "inactive" for item in inactive_items)


class TestStockMovementService:
    """Test suite for StockMovementService"""
    
    def test_create_receipt_movement(self, db_session: Session, sample_stock_item_data):
        """Test creating stock receipt movement"""
        # First create a stock item
        stock_service = StockMasterService(db_session)
        item = stock_service.create_item(sample_stock_item_data, user_id=1)
        
        movement_service = StockMovementService(db_session)
        
        movement_data = {
            "item_id": item.id,
            "movement_type": "RECEIPT",
            "quantity": 50,
            "unit_cost": Decimal("12.00"),
            "reference": "PO001",
            "notes": "Purchase order receipt"
        }
        
        movement = movement_service.create_movement(movement_data, user_id=1)
        
        assert movement.id is not None
        assert movement.item_id == item.id
        assert movement.movement_type == "RECEIPT"
        assert movement.quantity == 50
        assert movement.unit_cost == Decimal("12.00")
        assert movement.reference == "PO001"
        
        # Check item quantity updated
        db_session.refresh(item)
        assert item.quantity_on_hand == 150  # Original 100 + 50
    
    def test_create_issue_movement(self, db_session: Session, sample_stock_item_data):
        """Test creating stock issue movement"""
        # Create item with stock
        stock_service = StockMasterService(db_session)
        item_data = sample_stock_item_data.copy()
        item_data["quantity_on_hand"] = 100
        item = stock_service.create_item(item_data, user_id=1)
        
        movement_service = StockMovementService(db_session)
        
        movement_data = {
            "item_id": item.id,
            "movement_type": "ISSUE",
            "quantity": 30,
            "unit_cost": Decimal("10.00"),
            "reference": "SO001",
            "notes": "Sales order issue"
        }
        
        movement = movement_service.create_movement(movement_data, user_id=1)
        
        assert movement.quantity == 30
        assert movement.movement_type == "ISSUE"
        
        # Check item quantity updated
        db_session.refresh(item)
        assert item.quantity_on_hand == 70  # Original 100 - 30
    
    def test_issue_insufficient_stock(self, db_session: Session, sample_stock_item_data):
        """Test issuing more stock than available"""
        # Create item with limited stock
        stock_service = StockMasterService(db_session)
        item_data = sample_stock_item_data.copy()
        item_data["quantity_on_hand"] = 10
        item = stock_service.create_item(item_data, user_id=1)
        
        movement_service = StockMovementService(db_session)
        
        movement_data = {
            "item_id": item.id,
            "movement_type": "ISSUE",
            "quantity": 20,  # More than available
            "unit_cost": Decimal("10.00"),
            "reference": "SO002"
        }
        
        with pytest.raises(ValueError, match="Insufficient stock"):
            movement_service.create_movement(movement_data, user_id=1)
    
    def test_get_movements_by_item(self, db_session: Session, sample_stock_item_data):
        """Test retrieving movements for specific item"""
        # Create item
        stock_service = StockMasterService(db_session)
        item = stock_service.create_item(sample_stock_item_data, user_id=1)
        
        movement_service = StockMovementService(db_session)
        
        # Create multiple movements
        movements_data = [
            {"item_id": item.id, "movement_type": "RECEIPT", "quantity": 50, "reference": "PO001"},
            {"item_id": item.id, "movement_type": "ISSUE", "quantity": 20, "reference": "SO001"},
            {"item_id": item.id, "movement_type": "ADJUSTMENT", "quantity": 5, "reference": "ADJ001"}
        ]
        
        for data in movements_data:
            movement_service.create_movement(data, user_id=1)
        
        # Retrieve movements
        item_movements = movement_service.get_movements_by_item(item.id)
        
        assert len(item_movements) == 3
        assert all(movement.item_id == item.id for movement in item_movements)
    
    def test_calculate_fifo_cost(self, db_session: Session, sample_stock_item_data):
        """Test FIFO costing calculation"""
        # Create item with FIFO costing
        stock_service = StockMasterService(db_session)
        item_data = sample_stock_item_data.copy()
        item_data["costing_method"] = "FIFO"
        item_data["quantity_on_hand"] = 0
        item = stock_service.create_item(item_data, user_id=1)
        
        movement_service = StockMovementService(db_session)
        
        # Create receipts at different costs
        receipts = [
            {"item_id": item.id, "movement_type": "RECEIPT", "quantity": 100, "unit_cost": Decimal("10.00")},
            {"item_id": item.id, "movement_type": "RECEIPT", "quantity": 100, "unit_cost": Decimal("12.00")},
        ]
        
        for receipt in receipts:
            movement_service.create_movement(receipt, user_id=1)
        
        # Issue 150 units (should use FIFO - first 100 at $10, next 50 at $12)
        issue_cost = movement_service.calculate_fifo_issue_cost(item.id, 150)
        expected_cost = (100 * Decimal("10.00")) + (50 * Decimal("12.00"))
        
        assert issue_cost == expected_cost


class TestStockValuationService:
    """Test suite for StockValuationService"""
    
    def test_calculate_total_inventory_value(self, db_session: Session):
        """Test total inventory value calculation"""
        valuation_service = StockValuationService(db_session)
        stock_service = StockMasterService(db_session)
        
        # Create multiple items
        items_data = [
            {"item_code": "VAL001", "description": "Item 1", "quantity_on_hand": 100, "standard_cost": Decimal("10.00")},
            {"item_code": "VAL002", "description": "Item 2", "quantity_on_hand": 50, "standard_cost": Decimal("20.00")},
            {"item_code": "VAL003", "description": "Item 3", "quantity_on_hand": 25, "standard_cost": Decimal("40.00")}
        ]
        
        total_expected = Decimal("0.00")
        for item_data in items_data:
            item_data.update({
                "unit_of_measure": "EA",
                "selling_price": Decimal("15.00"),
                "status": "active"
            })
            stock_service.create_item(item_data, user_id=1)
            total_expected += item_data["quantity_on_hand"] * item_data["standard_cost"]
        
        total_value = valuation_service.calculate_total_inventory_value()
        
        assert total_value == total_expected  # 1000 + 1000 + 1000 = 3000
    
    def test_calculate_item_valuation_fifo(self, db_session: Session, sample_stock_item_data):
        """Test FIFO item valuation"""
        # Create item with FIFO method
        stock_service = StockMasterService(db_session)
        item_data = sample_stock_item_data.copy()
        item_data["costing_method"] = "FIFO"
        item_data["quantity_on_hand"] = 0
        item = stock_service.create_item(item_data, user_id=1)
        
        # Create movements
        movement_service = StockMovementService(db_session)
        receipts = [
            {"item_id": item.id, "movement_type": "RECEIPT", "quantity": 100, "unit_cost": Decimal("10.00")},
            {"item_id": item.id, "movement_type": "RECEIPT", "quantity": 100, "unit_cost": Decimal("15.00")},
        ]
        
        for receipt in receipts:
            movement_service.create_movement(receipt, user_id=1)
        
        # Issue some stock
        movement_service.create_movement({
            "item_id": item.id,
            "movement_type": "ISSUE", 
            "quantity": 50,
            "unit_cost": Decimal("10.00")  # FIFO - first batch
        }, user_id=1)
        
        valuation_service = StockValuationService(db_session)
        valuation = valuation_service.calculate_item_valuation(item.id, "FIFO")
        
        # Should have 50 units @ $10 + 100 units @ $15 = $2000
        expected_value = (50 * Decimal("10.00")) + (100 * Decimal("15.00"))
        assert valuation["total_value"] == expected_value
        assert valuation["quantity"] == 150
    
    def test_get_slow_moving_items(self, db_session: Session):
        """Test identification of slow-moving items"""
        valuation_service = StockValuationService(db_session)
        stock_service = StockMasterService(db_session)
        
        # Create item with old last movement date
        old_date = datetime.now().replace(year=2023, month=1, day=1)
        
        item_data = {
            "item_code": "SLOW001",
            "description": "Slow Moving Item",
            "unit_of_measure": "EA",
            "standard_cost": Decimal("10.00"),
            "selling_price": Decimal("15.00"),
            "quantity_on_hand": 100,
            "last_movement_date": old_date,
            "status": "active"
        }
        
        item = stock_service.create_item(item_data, user_id=1)
        
        # Get slow moving items (90+ days)
        slow_items = valuation_service.get_slow_moving_items(days=90)
        
        assert len(slow_items) >= 1
        assert any(slow_item.id == item.id for slow_item in slow_items)
    
    def test_calculate_abc_analysis(self, db_session: Session):
        """Test ABC analysis calculation"""
        valuation_service = StockValuationService(db_session)
        stock_service = StockMasterService(db_session)
        
        # Create items with different values for ABC analysis
        items_data = [
            {"item_code": "HIGH001", "quantity": 100, "cost": Decimal("100.00")},  # High value
            {"item_code": "MED001", "quantity": 100, "cost": Decimal("50.00")},    # Medium value
            {"item_code": "LOW001", "quantity": 100, "cost": Decimal("10.00")},    # Low value
        ]
        
        for item_data in items_data:
            full_item_data = {
                "item_code": item_data["item_code"],
                "description": f"Item {item_data['item_code']}",
                "unit_of_measure": "EA",
                "standard_cost": item_data["cost"],
                "selling_price": item_data["cost"] * Decimal("1.5"),
                "quantity_on_hand": item_data["quantity"],
                "status": "active"
            }
            stock_service.create_item(full_item_data, user_id=1)
        
        abc_analysis = valuation_service.calculate_abc_analysis()
        
        assert "A" in abc_analysis
        assert "B" in abc_analysis
        assert "C" in abc_analysis
        assert len(abc_analysis["A"]) >= 1  # Should have at least the high-value item


class TestStockServiceIntegration:
    """Integration tests for stock services working together"""
    
    def test_complete_stock_workflow(self, db_session: Session):
        """Test complete stock management workflow"""
        stock_service = StockMasterService(db_session)
        movement_service = StockMovementService(db_session)
        valuation_service = StockValuationService(db_session)
        
        # 1. Create stock item
        item_data = {
            "item_code": "WORKFLOW01",
            "description": "Workflow Test Item",
            "unit_of_measure": "EA",
            "standard_cost": Decimal("20.00"),
            "selling_price": Decimal("30.00"),
            "quantity_on_hand": 0,
            "reorder_level": 20,
            "reorder_quantity": 100,
            "costing_method": "FIFO",
            "status": "active"
        }
        
        item = stock_service.create_item(item_data, user_id=1)
        
        # 2. Receive stock
        receipt_data = {
            "item_id": item.id,
            "movement_type": "RECEIPT",
            "quantity": 100,
            "unit_cost": Decimal("20.00"),
            "reference": "PO001",
            "notes": "Initial stock receipt"
        }
        
        movement_service.create_movement(receipt_data, user_id=1)
        
        # 3. Issue stock
        issue_data = {
            "item_id": item.id,
            "movement_type": "ISSUE",
            "quantity": 30,
            "unit_cost": Decimal("20.00"),
            "reference": "SO001",
            "notes": "Sales order issue"
        }
        
        movement_service.create_movement(issue_data, user_id=1)
        
        # 4. Verify final state
        db_session.refresh(item)
        assert item.quantity_on_hand == 70  # 100 - 30
        
        # 5. Check valuation
        valuation = valuation_service.calculate_item_valuation(item.id, "FIFO")
        expected_value = 70 * Decimal("20.00")
        assert valuation["total_value"] == expected_value
        
        # 6. Check movements history
        movements = movement_service.get_movements_by_item(item.id)
        assert len(movements) == 2
        assert movements[0].movement_type in ["RECEIPT", "ISSUE"]
        assert movements[1].movement_type in ["RECEIPT", "ISSUE"]