"""
Cash Receipt Service - SL050 migration
Handles customer payment processing and allocation
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from datetime import datetime
from sqlalchemy.orm import Session
from sqlalchemy import and_, func

from app.services.file_handlers.customer_handler import CustomerFileHandler
from app.services.file_handlers.system_handler import SystemFileHandler
from app.services.file_handlers.open_items_handler import SalesOpenItemsHandler
from app.models.customer import SalesLedgerRec
from app.models.sales import SalesOpenItemRec, SalesReceiptRec, SalesAllocationRec
from app.models.system import SystemRec
from app.services.gl.journal_entry import JournalEntryService
from app.core.security import log_user_action
from app.models.auth import User


class CashReceiptService:
    """
    Cash Receipt Entry functionality
    Implements SL050 - payment processing and allocation
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.customer_handler = CustomerFileHandler(db)
        self.system_handler = SystemFileHandler(db)
        self.open_items_handler = SalesOpenItemsHandler(db)
        
    def create_receipt(self, receipt_data: Dict) -> Tuple[SalesReceiptRec, Optional[str]]:
        """
        Create new cash receipt
        Returns (receipt_record, error_message)
        """
        # Validate customer
        customer_no = receipt_data.get('customer_no')
        customer, status = self.customer_handler.process(4, key_value=customer_no)
        if status.fs_reply != "00":
            return None, "Customer not found"
            
        # Get next receipt number
        system_rec, _ = self.system_handler.read_system_params()
        receipt_no = self._get_next_receipt_number(system_rec)
        
        # Create receipt record
        receipt = SalesReceiptRec(
            receipt_no=receipt_no,
            receipt_customer=customer_no,
            receipt_date=receipt_data.get('receipt_date', int(datetime.now().strftime("%Y%m%d"))),
            receipt_period=system_rec.sl_period if system_rec else 1,
            receipt_type=receipt_data.get('type', 'PAYMENT'),
            receipt_method=receipt_data.get('method', 'CHEQUE'),
            receipt_reference=receipt_data.get('reference', ''),
            receipt_amount=Decimal(str(receipt_data.get('amount', 0))),
            receipt_bank_code=receipt_data.get('bank_code', system_rec.sl_pay_ac if system_rec else ''),
            receipt_currency=receipt_data.get('currency', customer.sales_currency),
            receipt_exchange_rate=Decimal(str(receipt_data.get('exchange_rate', 1))),
            receipt_discount=Decimal(str(receipt_data.get('discount', 0))),
            receipt_status='DRAFT',
            receipt_posted='N',
            receipt_allocated=Decimal('0'),
            receipt_unallocated=Decimal(str(receipt_data.get('amount', 0)))
        )
        
        # Calculate local amount
        receipt.receipt_local_amount = receipt.receipt_amount * receipt.receipt_exchange_rate
        
        self.db.add(receipt)
        self.db.flush()
        
        # Auto-allocate if requested
        if receipt_data.get('auto_allocate', False):
            self._auto_allocate_receipt(receipt)
            
        # Specific allocations if provided
        allocations = receipt_data.get('allocations', [])
        if allocations:
            for alloc in allocations:
                success, error = self._allocate_to_invoice(
                    receipt, 
                    alloc['invoice_no'], 
                    Decimal(str(alloc['amount']))
                )
                if not success:
                    self.db.rollback()
                    return None, error
                    
        self.db.flush()
        
        # Log creation
        if self.current_user:
            log_user_action(
                db=self.db,
                user=self.current_user,
                action="CREATE_RECEIPT",
                table="sales_receipt_rec",
                key=receipt_no,
                new_values={'customer': customer_no, 'amount': float(receipt.receipt_amount)},
                module="SL"
            )
            
        return receipt, None
        
    def post_receipt(self, receipt_no: str) -> Tuple[bool, Optional[str]]:
        """
        Post receipt to ledgers
        Returns (success, error_message)
        """
        receipt = self.db.query(SalesReceiptRec).filter(
            SalesReceiptRec.receipt_no == receipt_no
        ).first()
        
        if not receipt:
            return False, "Receipt not found"
            
        if receipt.receipt_posted == 'Y':
            return False, "Receipt already posted"
            
        try:
            # 1. Create open item record for payment
            open_item = self._create_open_item(receipt)
            
            # 2. Update customer balance
            customer, _ = self.customer_handler.process(4, key_value=receipt.receipt_customer)
            customer.sales_balance -= receipt.receipt_local_amount
            customer.sales_date_last_pay = int(datetime.now().strftime("%Y%m%d"))
            
            self.customer_handler.process(7, record=customer)
            
            # 3. Process allocations
            allocations = self.db.query(SalesAllocationRec).filter(
                SalesAllocationRec.alloc_receipt_no == receipt_no
            ).all()
            
            for alloc in allocations:
                self._post_allocation(alloc)
                
            # 4. Post to General Ledger
            self._post_receipt_to_gl(receipt)
            
            # 5. Mark receipt as posted
            receipt.receipt_posted = 'Y'
            receipt.receipt_posted_date = int(datetime.now().strftime("%Y%m%d"))
            receipt.receipt_posted_by = self.current_user.username if self.current_user else 'SYSTEM'
            
            # 6. Update payment open item with allocations
            open_item.sales_oi_amount = -receipt.receipt_unallocated
            
            self.db.commit()
            
            # Log posting
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="POST_RECEIPT",
                    table="sales_receipt_rec",
                    key=receipt_no,
                    new_values={'amount': float(receipt.receipt_amount)},
                    module="SL"
                )
                
            return True, None
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def allocate_receipt(self, receipt_no: str, allocations: List[Dict]) -> Tuple[bool, Optional[str]]:
        """
        Allocate receipt to invoices
        allocations: [{'invoice_no': 'INV123', 'amount': 100.00}, ...]
        """
        receipt = self.db.query(SalesReceiptRec).filter(
            SalesReceiptRec.receipt_no == receipt_no
        ).first()
        
        if not receipt:
            return False, "Receipt not found"
            
        if receipt.receipt_unallocated <= 0:
            return False, "Receipt fully allocated"
            
        total_to_allocate = Decimal('0')
        
        # Validate allocations
        for alloc in allocations:
            amount = Decimal(str(alloc['amount']))
            if amount <= 0:
                return False, "Allocation amount must be positive"
                
            total_to_allocate += amount
            
        if total_to_allocate > receipt.receipt_unallocated:
            return False, "Allocations exceed unallocated amount"
            
        # Apply allocations
        for alloc in allocations:
            success, error = self._allocate_to_invoice(
                receipt,
                alloc['invoice_no'],
                Decimal(str(alloc['amount']))
            )
            if not success:
                self.db.rollback()
                return False, error
                
        self.db.commit()
        return True, None
        
    def reverse_receipt(self, receipt_no: str, reason: str) -> Tuple[bool, Optional[str]]:
        """Reverse a posted receipt"""
        receipt = self.db.query(SalesReceiptRec).filter(
            SalesReceiptRec.receipt_no == receipt_no
        ).first()
        
        if not receipt:
            return False, "Receipt not found"
            
        if receipt.receipt_posted != 'Y':
            return False, "Receipt not posted"
            
        if receipt.receipt_reversed == 'Y':
            return False, "Receipt already reversed"
            
        try:
            # Create reversal receipt
            reversal_data = {
                'customer_no': receipt.receipt_customer,
                'amount': -float(receipt.receipt_amount),
                'type': 'REVERSAL',
                'reference': f"REV-{receipt.receipt_no}",
                'method': receipt.receipt_method
            }
            
            reversal, error = self.create_receipt(reversal_data)
            if error:
                return False, error
                
            # Post reversal
            success, error = self.post_receipt(reversal.receipt_no)
            if not success:
                return False, error
                
            # Mark original as reversed
            receipt.receipt_reversed = 'Y'
            receipt.receipt_reverse_date = int(datetime.now().strftime("%Y%m%d"))
            receipt.receipt_reverse_reason = reason
            receipt.receipt_reverse_ref = reversal.receipt_no
            
            # Reverse allocations
            allocations = self.db.query(SalesAllocationRec).filter(
                SalesAllocationRec.alloc_receipt_no == receipt_no
            ).all()
            
            for alloc in allocations:
                self._reverse_allocation(alloc)
                
            self.db.commit()
            
            return True, None
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def get_receipt_details(self, receipt_no: str) -> Dict:
        """Get full receipt details including allocations"""
        receipt = self.db.query(SalesReceiptRec).filter(
            SalesReceiptRec.receipt_no == receipt_no
        ).first()
        
        if not receipt:
            return {"error": "Receipt not found"}
            
        # Get allocations
        allocations = self.db.query(SalesAllocationRec).filter(
            SalesAllocationRec.alloc_receipt_no == receipt_no
        ).all()
        
        # Get customer
        customer, _ = self.customer_handler.process(4, key_value=receipt.receipt_customer)
        
        return {
            'receipt': {
                'receipt_no': receipt.receipt_no,
                'date': receipt.receipt_date,
                'customer': receipt.receipt_customer,
                'customer_name': customer.sales_name if customer else '',
                'amount': float(receipt.receipt_amount),
                'method': receipt.receipt_method,
                'reference': receipt.receipt_reference,
                'allocated': float(receipt.receipt_allocated),
                'unallocated': float(receipt.receipt_unallocated),
                'status': {
                    'posted': receipt.receipt_posted == 'Y',
                    'reversed': receipt.receipt_reversed == 'Y'
                }
            },
            'allocations': [
                {
                    'invoice_no': alloc.alloc_invoice_no,
                    'invoice_date': alloc.alloc_invoice_date,
                    'invoice_amount': float(alloc.alloc_invoice_amount),
                    'allocated': float(alloc.alloc_amount),
                    'discount': float(alloc.alloc_discount)
                }
                for alloc in allocations
            ]
        }
        
    def get_unallocated_receipts(self, customer_no: Optional[str] = None) -> List[Dict]:
        """Get list of receipts with unallocated amounts"""
        query = self.db.query(SalesReceiptRec).filter(
            and_(
                SalesReceiptRec.receipt_posted == 'Y',
                SalesReceiptRec.receipt_unallocated > 0
            )
        )
        
        if customer_no:
            query = query.filter(SalesReceiptRec.receipt_customer == customer_no)
            
        receipts = query.order_by(SalesReceiptRec.receipt_date.desc()).all()
        
        return [
            {
                'receipt_no': r.receipt_no,
                'customer': r.receipt_customer,
                'date': r.receipt_date,
                'total': float(r.receipt_amount),
                'allocated': float(r.receipt_allocated),
                'unallocated': float(r.receipt_unallocated),
                'method': r.receipt_method,
                'reference': r.receipt_reference
            }
            for r in receipts
        ]
        
    def suggest_allocations(self, receipt_no: str) -> List[Dict]:
        """Suggest allocations for a receipt based on FIFO"""
        receipt = self.db.query(SalesReceiptRec).filter(
            SalesReceiptRec.receipt_no == receipt_no
        ).first()
        
        if not receipt or receipt.receipt_unallocated <= 0:
            return []
            
        # Get unpaid invoices for customer, oldest first
        unpaid = self.db.query(SalesOpenItemRec).filter(
            and_(
                SalesOpenItemRec.sales_oi_cust == receipt.receipt_customer,
                SalesOpenItemRec.sales_oi_type == 'INV',
                SalesOpenItemRec.sales_oi_amount > 0
            )
        ).order_by(SalesOpenItemRec.sales_oi_date).all()
        
        suggestions = []
        remaining = receipt.receipt_unallocated
        
        for invoice in unpaid:
            if remaining <= 0:
                break
                
            # Calculate discount if within terms
            discount = self._calculate_settlement_discount(invoice)
            net_due = invoice.sales_oi_amount - discount
            
            if net_due <= remaining:
                # Full allocation
                suggestions.append({
                    'invoice_no': invoice.sales_oi_our_ref,
                    'invoice_date': invoice.sales_oi_date,
                    'invoice_amount': float(invoice.sales_oi_amount),
                    'suggested_amount': float(net_due),
                    'discount': float(discount),
                    'days_overdue': self._calculate_days_overdue(invoice.sales_oi_due_date)
                })
                remaining -= net_due
            else:
                # Partial allocation
                suggestions.append({
                    'invoice_no': invoice.sales_oi_our_ref,
                    'invoice_date': invoice.sales_oi_date,
                    'invoice_amount': float(invoice.sales_oi_amount),
                    'suggested_amount': float(remaining),
                    'discount': 0,  # No discount on partial
                    'days_overdue': self._calculate_days_overdue(invoice.sales_oi_due_date)
                })
                remaining = Decimal('0')
                
        return suggestions
        
    def _get_next_receipt_number(self, system_rec: SystemRec) -> str:
        """Generate next receipt number"""
        if system_rec:
            next_no = system_rec.sl_next_receipt + 1
            system_rec.sl_next_receipt = next_no
            return f"REC{next_no:06d}"
        return f"REC{datetime.now().strftime('%Y%m%d%H%M%S')}"
        
    def _create_open_item(self, receipt: SalesReceiptRec) -> SalesOpenItemRec:
        """Create open item record for payment"""
        open_item = SalesOpenItemRec(
            sales_oi_cust=receipt.receipt_customer,
            sales_oi_type='PAY',
            sales_oi_our_ref=receipt.receipt_no,
            sales_oi_their_ref=receipt.receipt_reference,
            sales_oi_date=receipt.receipt_date,
            sales_oi_due_date=receipt.receipt_date,
            sales_oi_goods=Decimal('0'),
            sales_oi_tax=Decimal('0'),
            sales_oi_gross=-receipt.receipt_local_amount,  # Negative for payment
            sales_oi_amount=-receipt.receipt_local_amount,
            sales_oi_period=receipt.receipt_period,
            sales_oi_posted='Y',
            sales_oi_pay_method=receipt.receipt_method
        )
        
        # Write using handler to get proper key
        self.open_items_handler.process(5, record=open_item)
        return open_item
        
    def _auto_allocate_receipt(self, receipt: SalesReceiptRec):
        """Automatically allocate receipt to oldest invoices"""
        suggestions = self.suggest_allocations(receipt.receipt_no)
        
        for suggestion in suggestions:
            self._allocate_to_invoice(
                receipt,
                suggestion['invoice_no'],
                Decimal(str(suggestion['suggested_amount'])),
                Decimal(str(suggestion['discount']))
            )
            
    def _allocate_to_invoice(self, receipt: SalesReceiptRec, invoice_no: str, 
                           amount: Decimal, discount: Decimal = Decimal('0')) -> Tuple[bool, Optional[str]]:
        """Allocate receipt amount to invoice"""
        # Get invoice open item
        invoice_oi = self.db.query(SalesOpenItemRec).filter(
            and_(
                SalesOpenItemRec.sales_oi_type == 'INV',
                SalesOpenItemRec.sales_oi_our_ref == invoice_no,
                SalesOpenItemRec.sales_oi_cust == receipt.receipt_customer
            )
        ).first()
        
        if not invoice_oi:
            return False, f"Invoice {invoice_no} not found"
            
        if invoice_oi.sales_oi_amount <= 0:
            return False, f"Invoice {invoice_no} already paid"
            
        # Check allocation doesn't exceed invoice balance
        total_allocation = amount + discount
        if total_allocation > invoice_oi.sales_oi_amount:
            return False, f"Allocation exceeds invoice {invoice_no} balance"
            
        # Create allocation record
        allocation = SalesAllocationRec(
            alloc_receipt_no=receipt.receipt_no,
            alloc_invoice_no=invoice_no,
            alloc_date=receipt.receipt_date,
            alloc_amount=amount,
            alloc_discount=discount,
            alloc_invoice_date=invoice_oi.sales_oi_date,
            alloc_invoice_amount=invoice_oi.sales_oi_gross
        )
        
        self.db.add(allocation)
        
        # Update receipt
        receipt.receipt_allocated += amount
        receipt.receipt_unallocated -= amount
        
        # Update invoice (if posted)
        if receipt.receipt_posted == 'Y':
            invoice_oi.sales_oi_amount -= total_allocation
            if abs(invoice_oi.sales_oi_amount) < Decimal('0.01'):
                invoice_oi.sales_oi_amount = Decimal('0')
                invoice_oi.sales_oi_paid_date = receipt.receipt_date
                
        return True, None
        
    def _post_allocation(self, allocation: SalesAllocationRec):
        """Post allocation when receipt is posted"""
        # Get invoice open item
        invoice_oi = self.db.query(SalesOpenItemRec).filter(
            and_(
                SalesOpenItemRec.sales_oi_type == 'INV',
                SalesOpenItemRec.sales_oi_our_ref == allocation.alloc_invoice_no
            )
        ).first()
        
        if invoice_oi:
            # Reduce invoice balance
            total_allocation = allocation.alloc_amount + allocation.alloc_discount
            invoice_oi.sales_oi_amount -= total_allocation
            
            if abs(invoice_oi.sales_oi_amount) < Decimal('0.01'):
                invoice_oi.sales_oi_amount = Decimal('0')
                invoice_oi.sales_oi_paid_date = allocation.alloc_date
                
            # Update last payment date
            invoice_oi.sales_oi_last_pay = allocation.alloc_date
            
            # Handle discount if any
            if allocation.alloc_discount > 0:
                self._post_settlement_discount(allocation)
                
    def _reverse_allocation(self, allocation: SalesAllocationRec):
        """Reverse allocation when receipt is reversed"""
        # Get invoice open item
        invoice_oi = self.db.query(SalesOpenItemRec).filter(
            and_(
                SalesOpenItemRec.sales_oi_type == 'INV',
                SalesOpenItemRec.sales_oi_our_ref == allocation.alloc_invoice_no
            )
        ).first()
        
        if invoice_oi:
            # Restore invoice balance
            total_allocation = allocation.alloc_amount + allocation.alloc_discount
            invoice_oi.sales_oi_amount += total_allocation
            
            # Clear paid date if now unpaid
            if invoice_oi.sales_oi_amount > 0:
                invoice_oi.sales_oi_paid_date = 0
                
    def _calculate_settlement_discount(self, invoice: SalesOpenItemRec) -> Decimal:
        """Calculate settlement discount if within terms"""
        # Get customer
        customer, _ = self.customer_handler.process(4, key_value=invoice.sales_oi_cust)
        if not customer:
            return Decimal('0')
            
        # Check if within discount terms
        # This is simplified - would need discount terms configuration
        today = datetime.now().date()
        invoice_date = datetime.strptime(str(invoice.sales_oi_date), "%Y%m%d").date()
        days_since_invoice = (today - invoice_date).days
        
        # Example: 2% discount if paid within 10 days
        if days_since_invoice <= 10:
            return invoice.sales_oi_amount * Decimal('0.02')
            
        return Decimal('0')
        
    def _calculate_days_overdue(self, due_date: int) -> int:
        """Calculate days overdue"""
        if due_date == 0:
            return 0
            
        today = datetime.now().date()
        due = datetime.strptime(str(due_date), "%Y%m%d").date()
        
        if due >= today:
            return 0
            
        return (today - due).days
        
    def _post_settlement_discount(self, allocation: SalesAllocationRec):
        """Post settlement discount to GL"""
        # This would create GL entries for the discount
        pass
        
    def _post_receipt_to_gl(self, receipt: SalesReceiptRec):
        """Post receipt to General Ledger"""
        system_rec, _ = self.system_handler.read_system_params()
        if not system_rec or system_rec.gl_interface != 'Y':
            return
            
        je_service = JournalEntryService(self.db, self.current_user)
        
        # Create batch
        batch = je_service.create_journal_batch(
            description=f"Customer Receipt {receipt.receipt_no}",
            source="SL"
        )
        
        # Debit: Bank Account
        je_service.add_journal_line(batch.batch_no, {
            'account': int(receipt.receipt_bank_code),
            'debit': float(receipt.receipt_local_amount),
            'credit': 0,
            'reference': receipt.receipt_no,
            'description': f"Receipt from {receipt.receipt_customer}"
        })
        
        # Credit: Customer Control Account
        je_service.add_journal_line(batch.batch_no, {
            'account': system_rec.s_debtors,
            'debit': 0,
            'credit': float(receipt.receipt_local_amount),
            'reference': receipt.receipt_no,
            'description': f"Receipt {receipt.receipt_customer}"
        })
        
        # Handle any settlement discount
        total_discount = self.db.query(
            func.sum(SalesAllocationRec.alloc_discount)
        ).filter(
            SalesAllocationRec.alloc_receipt_no == receipt.receipt_no
        ).scalar() or Decimal('0')
        
        if total_discount > 0:
            # Debit: Discount Allowed Account
            je_service.add_journal_line(batch.batch_no, {
                'account': system_rec.sl_discount_ac,
                'debit': float(total_discount),
                'credit': 0,
                'reference': receipt.receipt_no,
                'description': "Settlement discount"
            })
            
            # Credit: Customer Control Account (additional)
            je_service.add_journal_line(batch.batch_no, {
                'account': system_rec.s_debtors,
                'debit': 0,
                'credit': float(total_discount),
                'reference': receipt.receipt_no,
                'description': "Discount allowed"
            })
            
        # Post the batch
        je_service.post_batch(batch.batch_no)
        
    def batch_allocate_receipts(self, customer_no: Optional[str] = None) -> Dict:
        """Batch allocate all unallocated receipts"""
        # Get unallocated receipts
        receipts = self.get_unallocated_receipts(customer_no)
        
        results = {
            'processed': 0,
            'allocated': Decimal('0'),
            'errors': []
        }
        
        for receipt_data in receipts:
            receipt_no = receipt_data['receipt_no']
            
            # Get suggestions
            suggestions = self.suggest_allocations(receipt_no)
            
            if suggestions:
                # Apply allocations
                allocations = [
                    {
                        'invoice_no': s['invoice_no'],
                        'amount': s['suggested_amount']
                    }
                    for s in suggestions
                ]
                
                success, error = self.allocate_receipt(receipt_no, allocations)
                
                if success:
                    results['processed'] += 1
                    results['allocated'] += sum(
                        Decimal(str(a['amount'])) for a in allocations
                    )
                else:
                    results['errors'].append(f"{receipt_no}: {error}")
                    
        return results