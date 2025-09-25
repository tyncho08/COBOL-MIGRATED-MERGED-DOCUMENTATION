"""
Supplier File Handler - COBOL acas005 migration
Handles puledger_rec (Purchase/Supplier Ledger)
"""
from typing import Any, Tuple, Optional, List
from sqlalchemy.orm import Session
from sqlalchemy import or_, func

from app.services.file_handlers.base_handler import BaseFileHandler, FileAccess
from app.models.supplier import PurchaseLedgerRec


class SupplierFileHandler(BaseFileHandler):
    """
    Supplier File Handler (acas005)
    Handles supplier/vendor master records
    """
    
    def __init__(self, db: Session):
        super().__init__(db, file_number=5, table_name="puledger_rec")
        
    def get_model_class(self):
        return PurchaseLedgerRec
        
    def get_key_field(self, key_type: int = 1):
        """
        Supplier file keys:
        1 = Supplier code (primary)
        2 = Supplier name (for searches)
        """
        if key_type == 1:
            return "purch_key"
        elif key_type == 2:
            return "purch_name"
        return None
        
    def find_by_partial_key(self, partial_key: str, key_type: int = 1) -> List[PurchaseLedgerRec]:
        """Find suppliers by partial key match"""
        model = self.get_model_class()
        key_field = self.get_key_field(key_type)
        
        if not key_field:
            return []
            
        records = self.db.query(model).filter(
            getattr(model, key_field).like(f"{partial_key}%")
        ).order_by(getattr(model, key_field)).limit(50).all()
        
        return records
        
    def find_by_name_search(self, search_term: str) -> List[PurchaseLedgerRec]:
        """Search suppliers by name"""
        model = self.get_model_class()
        
        records = self.db.query(model).filter(
            func.upper(model.purch_name).contains(search_term.upper())
        ).order_by(model.purch_name).limit(50).all()
        
        return records
        
    def get_active_suppliers(self) -> List[PurchaseLedgerRec]:
        """Get all active suppliers"""
        return self.db.query(PurchaseLedgerRec).filter(
            PurchaseLedgerRec.purch_account_status == 'A'
        ).order_by(PurchaseLedgerRec.purch_key).all()
        
    def get_suppliers_with_balance(self) -> List[PurchaseLedgerRec]:
        """Get suppliers with non-zero balance"""
        return self.db.query(PurchaseLedgerRec).filter(
            PurchaseLedgerRec.purch_balance != 0
        ).order_by(PurchaseLedgerRec.purch_key).all()
        
    def get_suppliers_on_hold(self) -> List[PurchaseLedgerRec]:
        """Get suppliers on hold"""
        return self.db.query(PurchaseLedgerRec).filter(
            PurchaseLedgerRec.purch_account_status == 'H'
        ).order_by(PurchaseLedgerRec.purch_key).all()
        
    def requires_approval(self, supplier_key: str) -> bool:
        """Check if supplier requires purchase approval"""
        supplier, status = self._read_indexed(supplier_key, 1)
        
        if status.fs_reply == "00" and supplier:
            return supplier.purch_approval_required == 'Y'
        return False
        
    def update_balance(self, supplier_key: str, amount: float, operation: str = "ADD") -> FileAccess:
        """
        Update supplier balance
        operation: ADD, SUBTRACT, SET
        """
        supplier, status = self._read_indexed(supplier_key, 1)
        
        if status.fs_reply != "00" or not supplier:
            return status
            
        if operation == "ADD":
            supplier.purch_balance += amount
        elif operation == "SUBTRACT":
            supplier.purch_balance -= amount
        elif operation == "SET":
            supplier.purch_balance = amount
            
        # Update YTD turnover for purchases
        if operation == "ADD" and amount > 0:
            supplier.purch_ytd_turnover += amount
            supplier.purch_transactions_ytd += 1
            supplier.purch_invoices_ytd += 1
            
        _, status = self._rewrite_record(supplier, supplier_key, 1)
        return status
        
    def get_remittance_suppliers(self) -> List[PurchaseLedgerRec]:
        """Get suppliers who should receive remittance advice"""
        return self.db.query(PurchaseLedgerRec).filter(
            PurchaseLedgerRec.purch_remittance_flag == 'Y',
            PurchaseLedgerRec.purch_account_status == 'A'
        ).order_by(PurchaseLedgerRec.purch_key).all()
        
    def get_1099_suppliers(self) -> List[PurchaseLedgerRec]:
        """Get suppliers requiring 1099 reporting (US tax)"""
        return self.db.query(PurchaseLedgerRec).filter(
            PurchaseLedgerRec.purch_1099_required == 'Y'
        ).order_by(PurchaseLedgerRec.purch_key).all()
        
    def get_settlement_discount(self, supplier_key: str) -> float:
        """Get settlement discount rate for supplier"""
        supplier, status = self._read_indexed(supplier_key, 1)
        
        if status.fs_reply == "00" and supplier:
            return float(supplier.purch_settlement_disc)
        return 0.0
        
    def get_payment_terms(self, supplier_key: str) -> str:
        """Get payment terms for supplier"""
        supplier, status = self._read_indexed(supplier_key, 1)
        
        if status.fs_reply == "00" and supplier:
            return supplier.purch_payment_terms
        return "30"  # Default 30 days
        
    def get_default_nominal(self, supplier_key: str) -> int:
        """Get default GL account for supplier purchases"""
        supplier, status = self._read_indexed(supplier_key, 1)
        
        if status.fs_reply == "00" and supplier:
            return supplier.purch_default_nominal
        return 0