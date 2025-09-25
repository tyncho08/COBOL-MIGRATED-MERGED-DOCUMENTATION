"""
Customer File Handler - COBOL acas004 migration
Handles saledger_rec (Sales/Customer Ledger)
"""
from typing import Any, Tuple, Optional, List
from sqlalchemy.orm import Session
from sqlalchemy import or_, func

from app.services.file_handlers.base_handler import BaseFileHandler, FileAccess, FileFunction
from app.models.customer import SalesLedgerRec


class CustomerFileHandler(BaseFileHandler):
    """
    Customer File Handler (acas004)
    Handles customer master records
    """
    
    def __init__(self, db: Session):
        super().__init__(db, file_number=4, table_name="saledger_rec")
        
    def get_model_class(self):
        return SalesLedgerRec
        
    def get_key_field(self, key_type: int = 1):
        """
        Customer file keys:
        1 = Customer code (primary)
        2 = Customer name (for searches)
        """
        if key_type == 1:
            return "sales_key"
        elif key_type == 2:
            return "sales_name"
        return None
        
    def find_by_partial_key(self, partial_key: str, key_type: int = 1) -> List[SalesLedgerRec]:
        """Find customers by partial key match"""
        model = self.get_model_class()
        key_field = self.get_key_field(key_type)
        
        if not key_field:
            return []
            
        # COBOL-style partial key search (starts with)
        records = self.db.query(model).filter(
            getattr(model, key_field).like(f"{partial_key}%")
        ).order_by(getattr(model, key_field)).limit(50).all()
        
        return records
        
    def find_by_name_search(self, search_term: str) -> List[SalesLedgerRec]:
        """Search customers by name (anywhere in name)"""
        model = self.get_model_class()
        
        # COBOL-style name search
        records = self.db.query(model).filter(
            func.upper(model.sales_name).contains(search_term.upper())
        ).order_by(model.sales_name).limit(50).all()
        
        return records
        
    def get_active_customers(self) -> List[SalesLedgerRec]:
        """Get all active customers"""
        return self.db.query(SalesLedgerRec).filter(
            SalesLedgerRec.sales_account_status == 'A'
        ).order_by(SalesLedgerRec.sales_key).all()
        
    def get_customers_with_balance(self) -> List[SalesLedgerRec]:
        """Get customers with non-zero balance"""
        return self.db.query(SalesLedgerRec).filter(
            SalesLedgerRec.sales_balance != 0
        ).order_by(SalesLedgerRec.sales_key).all()
        
    def get_customers_on_hold(self) -> List[SalesLedgerRec]:
        """Get customers on credit hold"""
        return self.db.query(SalesLedgerRec).filter(
            or_(
                SalesLedgerRec.sales_credit_status == 'H',
                SalesLedgerRec.sales_credit_status == 'S'
            )
        ).order_by(SalesLedgerRec.sales_key).all()
        
    def check_credit_limit(self, customer_key: str, additional_amount: float = 0) -> Tuple[bool, float]:
        """
        Check if customer is within credit limit
        Returns (is_within_limit, available_credit)
        """
        customer, status = self._read_indexed(customer_key, 1)
        
        if status.fs_reply != "00" or not customer:
            return False, 0
            
        # Check credit status first
        if customer.sales_credit_status in ['H', 'S']:
            return False, 0
            
        # Calculate available credit
        current_exposure = float(customer.sales_balance) + additional_amount
        credit_limit = float(customer.sales_credit_limit)
        
        available = credit_limit - current_exposure
        is_within_limit = current_exposure <= credit_limit
        
        return is_within_limit, available
        
    def update_balance(self, customer_key: str, amount: float, operation: str = "ADD") -> FileAccess:
        """
        Update customer balance
        operation: ADD, SUBTRACT, SET
        """
        customer, status = self._read_indexed(customer_key, 1)
        
        if status.fs_reply != "00" or not customer:
            return status
            
        if operation == "ADD":
            customer.sales_balance += amount
        elif operation == "SUBTRACT":
            customer.sales_balance -= amount
        elif operation == "SET":
            customer.sales_balance = amount
            
        # Also update YTD turnover for sales
        if operation == "ADD" and amount > 0:
            customer.sales_ytd_turnover += amount
            customer.sales_transactions_ytd += 1
            customer.sales_invoices_ytd += 1
            
        _, status = self._rewrite_record(customer, customer_key, 1)
        return status
        
    def get_statement_customers(self) -> List[SalesLedgerRec]:
        """Get customers who should receive statements"""
        return self.db.query(SalesLedgerRec).filter(
            SalesLedgerRec.sales_statement_flag == 'Y',
            SalesLedgerRec.sales_account_status == 'A'
        ).order_by(SalesLedgerRec.sales_key).all()
        
    def get_dunning_customers(self) -> List[SalesLedgerRec]:
        """Get customers eligible for dunning letters"""
        return self.db.query(SalesLedgerRec).filter(
            SalesLedgerRec.sales_dunning_flag == 'Y',
            SalesLedgerRec.sales_balance > 0,
            SalesLedgerRec.sales_account_status == 'A'
        ).order_by(SalesLedgerRec.sales_key).all()
        
    def apply_settlement_discount(self, customer_key: str) -> float:
        """Get settlement discount rate for customer"""
        customer, status = self._read_indexed(customer_key, 1)
        
        if status.fs_reply == "00" and customer:
            return float(customer.sales_settlement_disc)
        return 0.0
        
    def get_price_level(self, customer_key: str) -> int:
        """Get customer's price level for pricing"""
        customer, status = self._read_indexed(customer_key, 1)
        
        if status.fs_reply == "00" and customer:
            return customer.sales_price_level
        return 1  # Default price level