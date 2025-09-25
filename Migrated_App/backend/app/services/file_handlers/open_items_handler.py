"""
Open Items File Handlers - COBOL acas013/015 migration
Handles saitm3_rec and puitm5_rec (Open Items/Outstanding Transactions)
"""
from typing import Any, Tuple, Optional, List
from decimal import Decimal
from datetime import datetime, timedelta
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func

from app.services.file_handlers.base_handler import BaseFileHandler, FileAccess, COBOLError
from app.models.customer import SalesItemRec
from app.models.supplier import PurchaseItemRec


class SalesOpenItemsHandler(BaseFileHandler):
    """
    Sales Open Items Handler (acas013)
    Handles outstanding customer transactions
    """
    
    def __init__(self, db: Session):
        super().__init__(db, file_number=13, table_name="saitm3_rec")
        
    def get_model_class(self):
        return SalesItemRec
        
    def get_key_field(self, key_type: int = 1):
        """
        Open items use composite key (customer + invoice)
        For searches, we use customer as primary
        """
        if key_type == 1:
            return "soitm_customer"
        elif key_type == 2:
            return "soitm_invoice"
        return None
        
    def get_customer_open_items(self, customer_key: str) -> List[SalesItemRec]:
        """Get all open items for a customer"""
        return self.db.query(SalesItemRec).filter(
            and_(
                SalesItemRec.soitm_customer == customer_key,
                SalesItemRec.soitm_balance > 0
            )
        ).order_by(SalesItemRec.soitm_date).all()
        
    def get_overdue_items(self, days_overdue: int = 0) -> List[SalesItemRec]:
        """Get overdue items"""
        cutoff_date = int((datetime.now() - timedelta(days=days_overdue)).strftime("%Y%m%d"))
        
        return self.db.query(SalesItemRec).filter(
            and_(
                SalesItemRec.soitm_due_date < cutoff_date,
                SalesItemRec.soitm_balance > 0
            )
        ).order_by(SalesItemRec.soitm_customer, SalesItemRec.soitm_date).all()
        
    def create_open_item(self, customer_key: str, invoice_no: int, invoice_data: dict) -> FileAccess:
        """Create new open item from invoice"""
        item = SalesItemRec(
            soitm_customer=customer_key,
            soitm_invoice=invoice_no,
            soitm_date=invoice_data.get('date', int(datetime.now().strftime("%Y%m%d"))),
            soitm_due_date=invoice_data.get('due_date', 0),
            soitm_type=invoice_data.get('type', 'I'),
            soitm_reference=invoice_data.get('reference', ''),
            soitm_amount=Decimal(str(invoice_data.get('amount', 0))),
            soitm_paid=Decimal('0'),
            soitm_balance=Decimal(str(invoice_data.get('amount', 0))),
            soitm_period=invoice_data.get('period', 0),
            soitm_age_days=0,
            soitm_dispute='N'
        )
        
        return self._write_record(item)
        
    def apply_payment(self, customer_key: str, invoice_no: int, payment_amount: float) -> FileAccess:
        """Apply payment to open item"""
        item = self.db.query(SalesItemRec).filter(
            and_(
                SalesItemRec.soitm_customer == customer_key,
                SalesItemRec.soitm_invoice == invoice_no
            )
        ).first()
        
        if not item:
            status = FileAccess()
            status.we_error = COBOLError.KEY_NOT_FOUND
            status.fs_reply = "23"
            return status
            
        # Update amounts
        payment = Decimal(str(payment_amount))
        item.soitm_paid += payment
        item.soitm_balance -= payment
        
        # If fully paid, we might delete or keep for history
        if item.soitm_balance <= 0:
            item.soitm_balance = Decimal('0')
            
        self.db.merge(item)
        self.db.flush()
        
        status = FileAccess()
        status.fs_reply = "00"
        return status
        
    def get_aging_summary(self, customer_key: str) -> dict:
        """Get aging summary for customer"""
        items = self.get_customer_open_items(customer_key)
        
        current_date = int(datetime.now().strftime("%Y%m%d"))
        
        aging = {
            'current': Decimal('0'),
            '30_days': Decimal('0'),
            '60_days': Decimal('0'),
            '90_days': Decimal('0'),
            'over_90': Decimal('0'),
            'total': Decimal('0')
        }
        
        for item in items:
            if item.soitm_balance <= 0:
                continue
                
            days_old = self._calculate_days_between(item.soitm_due_date, current_date)
            
            if days_old <= 0:
                aging['current'] += item.soitm_balance
            elif days_old <= 30:
                aging['30_days'] += item.soitm_balance
            elif days_old <= 60:
                aging['60_days'] += item.soitm_balance
            elif days_old <= 90:
                aging['90_days'] += item.soitm_balance
            else:
                aging['over_90'] += item.soitm_balance
                
            aging['total'] += item.soitm_balance
            
        return {k: float(v) for k, v in aging.items()}
        
    def _calculate_days_between(self, date1: int, date2: int) -> int:
        """Calculate days between two YYYYMMDD dates"""
        d1 = datetime.strptime(str(date1), "%Y%m%d")
        d2 = datetime.strptime(str(date2), "%Y%m%d")
        return (d2 - d1).days


class PurchaseOpenItemsHandler(BaseFileHandler):
    """
    Purchase Open Items Handler (acas015)
    Handles outstanding supplier transactions
    """
    
    def __init__(self, db: Session):
        super().__init__(db, file_number=15, table_name="puitm5_rec")
        
    def get_model_class(self):
        return PurchaseItemRec
        
    def get_key_field(self, key_type: int = 1):
        """
        Open items use composite key (supplier + invoice)
        For searches, we use supplier as primary
        """
        if key_type == 1:
            return "poitm_supplier"
        elif key_type == 2:
            return "poitm_invoice"
        return None
        
    def get_supplier_open_items(self, supplier_key: str) -> List[PurchaseItemRec]:
        """Get all open items for a supplier"""
        return self.db.query(PurchaseItemRec).filter(
            and_(
                PurchaseItemRec.poitm_supplier == supplier_key,
                PurchaseItemRec.poitm_balance > 0
            )
        ).order_by(PurchaseItemRec.poitm_date).all()
        
    def get_items_for_payment(self, cutoff_date: Optional[int] = None) -> List[PurchaseItemRec]:
        """Get items due for payment"""
        query = self.db.query(PurchaseItemRec).filter(
            and_(
                PurchaseItemRec.poitm_balance > 0,
                PurchaseItemRec.poitm_held == 'N'
            )
        )
        
        if cutoff_date:
            query = query.filter(PurchaseItemRec.poitm_due_date <= cutoff_date)
            
        return query.order_by(
            PurchaseItemRec.poitm_supplier,
            PurchaseItemRec.poitm_due_date
        ).all()
        
    def create_open_item(self, supplier_key: str, invoice_no: str, invoice_data: dict) -> FileAccess:
        """Create new open item from invoice"""
        item = PurchaseItemRec(
            poitm_supplier=supplier_key,
            poitm_invoice=invoice_no,
            poitm_date=invoice_data.get('date', int(datetime.now().strftime("%Y%m%d"))),
            poitm_due_date=invoice_data.get('due_date', 0),
            poitm_type=invoice_data.get('type', 'I'),
            poitm_reference=invoice_data.get('reference', ''),
            poitm_amount=Decimal(str(invoice_data.get('amount', 0))),
            poitm_paid=Decimal('0'),
            poitm_balance=Decimal(str(invoice_data.get('amount', 0))),
            poitm_period=invoice_data.get('period', 0),
            poitm_age_days=0,
            poitm_held='N'
        )
        
        return self._write_record(item)
        
    def apply_payment(self, supplier_key: str, invoice_no: str, payment_amount: float) -> FileAccess:
        """Apply payment to open item"""
        item = self.db.query(PurchaseItemRec).filter(
            and_(
                PurchaseItemRec.poitm_supplier == supplier_key,
                PurchaseItemRec.poitm_invoice == invoice_no
            )
        ).first()
        
        if not item:
            status = FileAccess()
            status.we_error = COBOLError.KEY_NOT_FOUND
            status.fs_reply = "23"
            return status
            
        # Update amounts
        payment = Decimal(str(payment_amount))
        item.poitm_paid += payment
        item.poitm_balance -= payment
        
        # If fully paid, we might delete or keep for history
        if item.poitm_balance <= 0:
            item.poitm_balance = Decimal('0')
            
        self.db.merge(item)
        self.db.flush()
        
        status = FileAccess()
        status.fs_reply = "00"
        return status
        
    def hold_invoice(self, supplier_key: str, invoice_no: str, reason: str = "") -> FileAccess:
        """Put invoice on hold"""
        item = self.db.query(PurchaseItemRec).filter(
            and_(
                PurchaseItemRec.poitm_supplier == supplier_key,
                PurchaseItemRec.poitm_invoice == invoice_no
            )
        ).first()
        
        if not item:
            status = FileAccess()
            status.we_error = COBOLError.KEY_NOT_FOUND
            status.fs_reply = "23"
            return status
            
        item.poitm_held = 'Y'
        item.poitm_held_reason = reason[:100]
        
        self.db.merge(item)
        self.db.flush()
        
        status = FileAccess()
        status.fs_reply = "00"
        return status
        
    def release_hold(self, supplier_key: str, invoice_no: str) -> FileAccess:
        """Release invoice from hold"""
        item = self.db.query(PurchaseItemRec).filter(
            and_(
                PurchaseItemRec.poitm_supplier == supplier_key,
                PurchaseItemRec.poitm_invoice == invoice_no
            )
        ).first()
        
        if not item:
            status = FileAccess()
            status.we_error = COBOLError.KEY_NOT_FOUND
            status.fs_reply = "23"
            return status
            
        item.poitm_held = 'N'
        item.poitm_held_reason = ''
        
        self.db.merge(item)
        self.db.flush()
        
        status = FileAccess()
        status.fs_reply = "00"
        return status
        
    def get_cash_requirements(self, days_ahead: int = 30) -> float:
        """Calculate cash requirements for next N days"""
        future_date = int((datetime.now() + timedelta(days=days_ahead)).strftime("%Y%m%d"))
        
        items = self.db.query(PurchaseItemRec).filter(
            and_(
                PurchaseItemRec.poitm_balance > 0,
                PurchaseItemRec.poitm_held == 'N',
                PurchaseItemRec.poitm_due_date <= future_date
            )
        ).all()
        
        total = sum(float(item.poitm_balance) for item in items)
        return total