"""
GL Integration Service
Central service for posting transactions to General Ledger
"""
from typing import Dict, Any, List, Optional
from decimal import Decimal
from datetime import datetime, date
from sqlalchemy.orm import Session
from sqlalchemy import func

from app.models.gl_accounts import GLLedgerRec, GLTransactionRec
from app.models.system import SystemRec
from app.core.exceptions import BusinessError


class GLIntegrationService:
    """Service for integrating all modules with General Ledger"""
    
    def __init__(self, db: Session):
        self.db = db
        self._load_system_settings()
    
    def _load_system_settings(self):
        """Load system settings for GL integration"""
        system = self.db.query(SystemRec).filter(
            SystemRec.system_rec_key == 1
        ).first()
        
        if not system:
            raise BusinessError("System settings not found")
        
        self.current_period = system.current_period
        self.fiscal_year = system.fiscal_year
        self.gl_integration_enabled = system.gl_integration == 'Y'
    
    def post_sales_invoice(self, invoice_data: Dict[str, Any]) -> Optional[int]:
        """Post sales invoice to GL"""
        if not self.gl_integration_enabled:
            return None
        
        batch_id = self._create_batch("SALES", f"Invoice {invoice_data['invoice_number']}")
        
        # Debit: Accounts Receivable
        self._create_transaction(
            batch_id=batch_id,
            account_code=invoice_data.get('ar_account', '1100'),
            debit_amount=invoice_data['total_amount'],
            credit_amount=Decimal('0.00'),
            reference=f"INV-{invoice_data['invoice_number']}",
            description=f"Invoice {invoice_data['invoice_number']} - {invoice_data['customer_name']}",
            transaction_date=invoice_data['invoice_date']
        )
        
        # Credit: Sales Revenue
        self._create_transaction(
            batch_id=batch_id,
            account_code=invoice_data.get('sales_account', '4000'),
            debit_amount=Decimal('0.00'),
            credit_amount=invoice_data['net_amount'],
            reference=f"INV-{invoice_data['invoice_number']}",
            description=f"Sales Revenue - {invoice_data['customer_name']}",
            transaction_date=invoice_data['invoice_date']
        )
        
        # Credit: Sales Tax
        if invoice_data.get('tax_amount', 0) > 0:
            self._create_transaction(
                batch_id=batch_id,
                account_code=invoice_data.get('tax_account', '2300'),
                debit_amount=Decimal('0.00'),
                credit_amount=invoice_data['tax_amount'],
                reference=f"INV-{invoice_data['invoice_number']}",
                description=f"Sales Tax - {invoice_data['customer_name']}",
                transaction_date=invoice_data['invoice_date']
            )
        
        self._post_batch(batch_id)
        return batch_id
    
    def post_purchase_invoice(self, invoice_data: Dict[str, Any]) -> Optional[int]:
        """Post purchase invoice to GL"""
        if not self.gl_integration_enabled:
            return None
        
        batch_id = self._create_batch("PURCHASE", f"PI {invoice_data['invoice_number']}")
        
        # Debit: Purchase/Expense Account
        self._create_transaction(
            batch_id=batch_id,
            account_code=invoice_data.get('expense_account', '5000'),
            debit_amount=invoice_data['net_amount'],
            credit_amount=Decimal('0.00'),
            reference=f"PI-{invoice_data['invoice_number']}",
            description=f"Purchase - {invoice_data['supplier_name']}",
            transaction_date=invoice_data['invoice_date']
        )
        
        # Debit: Input Tax
        if invoice_data.get('tax_amount', 0) > 0:
            self._create_transaction(
                batch_id=batch_id,
                account_code=invoice_data.get('input_tax_account', '1300'),
                debit_amount=invoice_data['tax_amount'],
                credit_amount=Decimal('0.00'),
                reference=f"PI-{invoice_data['invoice_number']}",
                description=f"Input Tax - {invoice_data['supplier_name']}",
                transaction_date=invoice_data['invoice_date']
            )
        
        # Credit: Accounts Payable
        self._create_transaction(
            batch_id=batch_id,
            account_code=invoice_data.get('ap_account', '2100'),
            debit_amount=Decimal('0.00'),
            credit_amount=invoice_data['total_amount'],
            reference=f"PI-{invoice_data['invoice_number']}",
            description=f"Payable - {invoice_data['supplier_name']}",
            transaction_date=invoice_data['invoice_date']
        )
        
        self._post_batch(batch_id)
        return batch_id
    
    def post_sales_receipt(self, receipt_data: Dict[str, Any]) -> Optional[int]:
        """Post sales receipt to GL"""
        if not self.gl_integration_enabled:
            return None
        
        batch_id = self._create_batch("RECEIPT", f"RCT {receipt_data['receipt_number']}")
        
        # Debit: Bank/Cash
        self._create_transaction(
            batch_id=batch_id,
            account_code=receipt_data.get('bank_account', '1000'),
            debit_amount=receipt_data['amount'],
            credit_amount=Decimal('0.00'),
            reference=f"RCT-{receipt_data['receipt_number']}",
            description=f"Receipt from {receipt_data['customer_name']}",
            transaction_date=receipt_data['receipt_date']
        )
        
        # Credit: Accounts Receivable
        self._create_transaction(
            batch_id=batch_id,
            account_code=receipt_data.get('ar_account', '1100'),
            debit_amount=Decimal('0.00'),
            credit_amount=receipt_data['amount'],
            reference=f"RCT-{receipt_data['receipt_number']}",
            description=f"AR Payment - {receipt_data['customer_name']}",
            transaction_date=receipt_data['receipt_date']
        )
        
        # Handle discount if any
        if receipt_data.get('discount_amount', 0) > 0:
            self._create_transaction(
                batch_id=batch_id,
                account_code=receipt_data.get('discount_account', '4100'),
                debit_amount=receipt_data['discount_amount'],
                credit_amount=Decimal('0.00'),
                reference=f"RCT-{receipt_data['receipt_number']}",
                description=f"Settlement Discount - {receipt_data['customer_name']}",
                transaction_date=receipt_data['receipt_date']
            )
        
        self._post_batch(batch_id)
        return batch_id
    
    def post_purchase_payment(self, payment_data: Dict[str, Any]) -> Optional[int]:
        """Post purchase payment to GL"""
        if not self.gl_integration_enabled:
            return None
        
        batch_id = self._create_batch("PAYMENT", f"PAY {payment_data['payment_number']}")
        
        # Debit: Accounts Payable
        self._create_transaction(
            batch_id=batch_id,
            account_code=payment_data.get('ap_account', '2100'),
            debit_amount=payment_data['amount'],
            credit_amount=Decimal('0.00'),
            reference=f"PAY-{payment_data['payment_number']}",
            description=f"Payment to {payment_data['supplier_name']}",
            transaction_date=payment_data['payment_date']
        )
        
        # Credit: Bank/Cash
        self._create_transaction(
            batch_id=batch_id,
            account_code=payment_data.get('bank_account', '1000'),
            debit_amount=Decimal('0.00'),
            credit_amount=payment_data['amount'],
            reference=f"PAY-{payment_data['payment_number']}",
            description=f"Bank Payment - {payment_data['supplier_name']}",
            transaction_date=payment_data['payment_date']
        )
        
        # Handle discount received if any
        if payment_data.get('discount_amount', 0) > 0:
            self._create_transaction(
                batch_id=batch_id,
                account_code=payment_data.get('discount_received_account', '4200'),
                debit_amount=Decimal('0.00'),
                credit_amount=payment_data['discount_amount'],
                reference=f"PAY-{payment_data['payment_number']}",
                description=f"Discount Received - {payment_data['supplier_name']}",
                transaction_date=payment_data['payment_date']
            )
        
        self._post_batch(batch_id)
        return batch_id
    
    def post_stock_adjustment(self, adjustment_data: Dict[str, Any]) -> Optional[int]:
        """Post stock adjustment to GL"""
        if not self.gl_integration_enabled:
            return None
        
        batch_id = self._create_batch("STOCK", f"ADJ {adjustment_data['adjustment_number']}")
        
        # Calculate adjustment value
        adjustment_value = adjustment_data['quantity'] * adjustment_data['unit_cost']
        
        if adjustment_data['adjustment_type'] == 'INCREASE':
            # Debit: Stock Asset
            self._create_transaction(
                batch_id=batch_id,
                account_code=adjustment_data.get('stock_account', '1200'),
                debit_amount=adjustment_value,
                credit_amount=Decimal('0.00'),
                reference=f"ADJ-{adjustment_data['adjustment_number']}",
                description=f"Stock Increase - {adjustment_data['item_description']}",
                transaction_date=adjustment_data['adjustment_date']
            )
            
            # Credit: Stock Adjustment Account
            self._create_transaction(
                batch_id=batch_id,
                account_code=adjustment_data.get('adjustment_account', '5100'),
                debit_amount=Decimal('0.00'),
                credit_amount=adjustment_value,
                reference=f"ADJ-{adjustment_data['adjustment_number']}",
                description=f"Stock Adjustment - {adjustment_data['item_description']}",
                transaction_date=adjustment_data['adjustment_date']
            )
        else:  # DECREASE
            # Debit: Stock Adjustment Account
            self._create_transaction(
                batch_id=batch_id,
                account_code=adjustment_data.get('adjustment_account', '5100'),
                debit_amount=adjustment_value,
                credit_amount=Decimal('0.00'),
                reference=f"ADJ-{adjustment_data['adjustment_number']}",
                description=f"Stock Adjustment - {adjustment_data['item_description']}",
                transaction_date=adjustment_data['adjustment_date']
            )
            
            # Credit: Stock Asset
            self._create_transaction(
                batch_id=batch_id,
                account_code=adjustment_data.get('stock_account', '1200'),
                debit_amount=Decimal('0.00'),
                credit_amount=adjustment_value,
                reference=f"ADJ-{adjustment_data['adjustment_number']}",
                description=f"Stock Decrease - {adjustment_data['item_description']}",
                transaction_date=adjustment_data['adjustment_date']
            )
        
        self._post_batch(batch_id)
        return batch_id
    
    def post_journal_entry(self, journal_data: Dict[str, Any]) -> Optional[int]:
        """Post manual journal entry to GL"""
        if not self.gl_integration_enabled:
            return None
        
        batch_id = self._create_batch("JOURNAL", journal_data['description'])
        
        # Process all journal lines
        for line in journal_data['lines']:
            self._create_transaction(
                batch_id=batch_id,
                account_code=line['account_code'],
                debit_amount=line.get('debit_amount', Decimal('0.00')),
                credit_amount=line.get('credit_amount', Decimal('0.00')),
                reference=journal_data['reference'],
                description=line.get('description', journal_data['description']),
                transaction_date=journal_data['journal_date']
            )
        
        # Validate journal balance
        if not self._validate_batch_balance(batch_id):
            self._delete_batch(batch_id)
            raise BusinessError("Journal entry is not balanced")
        
        self._post_batch(batch_id)
        return batch_id
    
    def _create_batch(self, source: str, description: str) -> int:
        """Create a new GL batch"""
        # Implementation would create batch record
        # For now, return a dummy batch ID
        return int(datetime.now().timestamp())
    
    def _create_transaction(self, batch_id: int, account_code: str, 
                           debit_amount: Decimal, credit_amount: Decimal,
                           reference: str, description: str, 
                           transaction_date: date):
        """Create a GL transaction"""
        # Verify account exists
        account = self.db.query(GLLedgerRec).filter(
            GLLedgerRec.gl_account_key == account_code
        ).first()
        
        if not account:
            raise BusinessError(f"GL Account {account_code} not found")
        
        # Create transaction record
        transaction = GLTransactionRec(
            batch_id=batch_id,
            gl_account_key=account_code,
            transaction_date=int(transaction_date.strftime("%Y%m%d")),
            period=self.current_period,
            fiscal_year=self.fiscal_year,
            reference=reference,
            description=description,
            debit_amount=debit_amount,
            credit_amount=credit_amount,
            posted='N',
            created_at=datetime.now()
        )
        
        self.db.add(transaction)
    
    def _validate_batch_balance(self, batch_id: int) -> bool:
        """Validate that batch debits equal credits"""
        result = self.db.query(
            func.sum(GLTransactionRec.debit_amount).label('total_debits'),
            func.sum(GLTransactionRec.credit_amount).label('total_credits')
        ).filter(
            GLTransactionRec.batch_id == batch_id
        ).first()
        
        if not result:
            return False
        
        total_debits = result.total_debits or Decimal('0.00')
        total_credits = result.total_credits or Decimal('0.00')
        
        return total_debits == total_credits
    
    def _post_batch(self, batch_id: int):
        """Post all transactions in a batch to GL"""
        # Update all transactions as posted
        self.db.query(GLTransactionRec).filter(
            GLTransactionRec.batch_id == batch_id
        ).update({
            'posted': 'Y',
            'posted_date': int(datetime.now().strftime("%Y%m%d")),
            'posted_by': 'SYSTEM'
        })
        
        # Update GL account balances
        transactions = self.db.query(GLTransactionRec).filter(
            GLTransactionRec.batch_id == batch_id
        ).all()
        
        for trans in transactions:
            account = self.db.query(GLLedgerRec).filter(
                GLLedgerRec.gl_account_key == trans.gl_account_key
            ).first()
            
            if account:
                # Update period movements
                period_field = f"gl_period_{trans.period:02d}_movement"
                current_movement = getattr(account, period_field, Decimal('0.00'))
                new_movement = current_movement + trans.debit_amount - trans.credit_amount
                setattr(account, period_field, new_movement)
                
                # Update YTD balance
                account.gl_ytd_movement = (
                    (account.gl_ytd_movement or Decimal('0.00')) + 
                    trans.debit_amount - trans.credit_amount
                )
        
        self.db.commit()
    
    def _delete_batch(self, batch_id: int):
        """Delete a batch and all its transactions"""
        self.db.query(GLTransactionRec).filter(
            GLTransactionRec.batch_id == batch_id
        ).delete()
        self.db.commit()
    
    def get_account_balance(self, account_code: str, as_at_date: Optional[date] = None) -> Decimal:
        """Get GL account balance"""
        account = self.db.query(GLLedgerRec).filter(
            GLLedgerRec.gl_account_key == account_code
        ).first()
        
        if not account:
            raise BusinessError(f"GL Account {account_code} not found")
        
        # If no date specified, return current balance
        if not as_at_date:
            return account.gl_ytd_movement or Decimal('0.00')
        
        # Otherwise calculate balance as at specific date
        # This would involve summing transactions up to that date
        # For now, return current balance
        return account.gl_ytd_movement or Decimal('0.00')