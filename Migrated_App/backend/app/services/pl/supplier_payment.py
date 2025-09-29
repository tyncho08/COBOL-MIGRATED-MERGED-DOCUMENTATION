"""
Supplier Payment Service - PL050 migration
Handles supplier payment processing and allocation
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from datetime import datetime
from sqlalchemy.orm import Session
from sqlalchemy import and_, func

from app.services.file_handlers.supplier_handler import SupplierFileHandler
from app.services.file_handlers.system_handler import SystemFileHandler
from app.services.file_handlers.open_items_handler import PurchaseOpenItemsHandler
from app.models.supplier import PurchaseLedgerRec, PurchaseOpenItemRec
from app.models.system import SystemRec
from app.services.gl.gl_integration import GLIntegrationService
from app.core.security import log_user_action
from app.models.auth import User


class SupplierPaymentService:
    """
    Supplier Payment functionality
    Implements PL050 - payment processing and allocation
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.supplier_handler = SupplierFileHandler(db)
        self.system_handler = SystemFileHandler(db)
        self.open_items_handler = PurchaseOpenItemsHandler(db)
        
    def process_payment(self, payment_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Process supplier payment
        Returns (success, error_message)
        """
        supplier_no = payment_data.get('supplier_no')
        amount = Decimal(str(payment_data.get('amount', 0)))
        payment_method = payment_data.get('method', 'BANK')
        reference = payment_data.get('reference', '')
        
        if amount <= 0:
            return False, "Payment amount must be positive"
            
        # Validate supplier
        supplier, status = self.supplier_handler.process(4, key_value=supplier_no)
        if status.fs_reply != "00":
            return False, "Supplier not found"
            
        try:
            # Get payment number
            payment_no = self._get_next_payment_number()
            
            # Update supplier balance
            supplier.purch_balance -= amount
            supplier.purch_date_last_pay = int(datetime.now().strftime("%Y%m%d"))
            self.supplier_handler.process(7, record=supplier)
            
            # Create payment open item
            payment_item = PurchaseOpenItemRec(
                purch_oi_supp=supplier_no,
                purch_oi_type='PAY',
                purch_oi_our_ref=payment_no,
                purch_oi_their_ref=reference,
                purch_oi_date=int(datetime.now().strftime("%Y%m%d")),
                purch_oi_gross=-amount,
                purch_oi_amount=-amount,
                purch_oi_posted='Y'
            )
            self.open_items_handler.process(5, record=payment_item)
            
            # Auto-allocate to oldest invoices (FIFO)
            if payment_data.get('auto_allocate', True):
                self._auto_allocate_payment(supplier_no, amount, payment_no)
                
            # Post to GL
            self._post_payment_to_gl(payment_data, supplier, payment_no)
            
            self.db.commit()
            
            # Log payment
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="PROCESS_PAYMENT",
                    table="purchase_open_item_rec",
                    key=payment_no,
                    new_values={
                        'supplier': supplier_no,
                        'amount': float(amount),
                        'method': payment_method
                    },
                    module="PL"
                )
                
            return True, None
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def _auto_allocate_payment(self, supplier_no: str, payment_amount: Decimal, payment_no: str):
        """Auto-allocate payment to oldest invoices"""
        # Get unpaid invoices ordered by date
        open_invoices = self.db.query(PurchaseOpenItemRec).filter(
            and_(
                PurchaseOpenItemRec.purch_oi_supp == supplier_no,
                PurchaseOpenItemRec.purch_oi_type == 'INV',
                PurchaseOpenItemRec.purch_oi_amount > 0
            )
        ).order_by(PurchaseOpenItemRec.purch_oi_date).all()
        
        remaining = payment_amount
        
        for invoice in open_invoices:
            if remaining <= 0:
                break
                
            if invoice.purch_oi_amount <= remaining:
                # Fully allocate this invoice
                allocation_amount = invoice.purch_oi_amount
                invoice.purch_oi_amount = Decimal('0')
            else:
                # Partially allocate
                allocation_amount = remaining
                invoice.purch_oi_amount -= allocation_amount
                
            remaining -= allocation_amount
            
            # Create allocation record (simplified for now)
            # In full implementation, would have a PurchaseAllocationRec table
            
    def _post_payment_to_gl(self, payment_data: Dict, supplier: PurchaseLedgerRec, payment_no: str):
        """Post payment to General Ledger"""
        gl_service = GLIntegrationService(self.db)
        
        # Prepare payment data for GL posting
        gl_payment_data = {
            'payment_number': payment_no,
            'supplier_name': supplier.purch_name,
            'payment_date': datetime.now().date(),
            'amount': Decimal(str(payment_data.get('amount', 0))),
            'discount_amount': Decimal(str(payment_data.get('discount', 0))),
            'bank_account': payment_data.get('bank_account', '1000'),  # Default bank account
            'ap_account': '2100',  # Default AP account
            'discount_received_account': '4200'  # Default discount received account
        }
        
        # Post to GL
        batch_id = gl_service.post_purchase_payment(gl_payment_data)
        
        # Store GL batch ID reference if needed
        if batch_id:
            # Would store this in the payment record
            pass
            
    def _get_next_payment_number(self) -> str:
        """Generate next payment number"""
        return f"PAY{datetime.now().strftime('%Y%m%d%H%M%S')}"