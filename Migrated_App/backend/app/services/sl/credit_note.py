"""
Credit Note Service - SL040 migration
Handles credit note creation, validation, and posting
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from datetime import datetime
from sqlalchemy.orm import Session
from sqlalchemy import and_, func

from app.services.file_handlers.customer_handler import CustomerFileHandler
from app.services.file_handlers.system_handler import SystemFileHandler
from app.services.file_handlers.stock_handler import StockFileHandler
from app.services.file_handlers.open_items_handler import SalesOpenItemsHandler
from app.models.customer import SalesLedgerRec
from app.models.sales import SalesOpenItemRec, SalesInvoiceRec, SalesInvoiceLineRec
from app.models.stock import StockMasterRec
from app.models.system import SystemRec
from app.services.gl.journal_entry import JournalEntryService
from app.core.security import log_user_action
from app.models.auth import User


class CreditNoteService:
    """
    Credit Note Entry functionality
    Implements SL040 - credit note creation and posting
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.customer_handler = CustomerFileHandler(db)
        self.system_handler = SystemFileHandler(db)
        self.stock_handler = StockFileHandler(db)
        self.open_items_handler = SalesOpenItemsHandler(db)
        
    def create_credit_note(self, credit_data: Dict) -> Tuple[SalesInvoiceRec, Optional[str]]:
        """
        Create new credit note
        Returns (credit_note_record, error_message)
        """
        # Validate customer
        customer_no = credit_data.get('customer_no')
        customer, status = self.customer_handler.process(4, key_value=customer_no)
        if status.fs_reply != "00":
            return None, "Customer not found"
            
        # Get next credit note number
        system_rec, _ = self.system_handler.read_system_params()
        credit_no = self._get_next_credit_number(system_rec)
        
        # Create credit note header (using invoice table with type='CRN')
        credit_note = SalesInvoiceRec(
            invoice_no=credit_no,
            invoice_customer=customer_no,
            invoice_date=credit_data.get('invoice_date', int(datetime.now().strftime("%Y%m%d"))),
            invoice_period=system_rec.sl_period if system_rec else 1,
            invoice_type='CRN',
            
            # Customer details
            invoice_name=customer.sales_name,
            invoice_add1=customer.sales_add1,
            invoice_add2=customer.sales_add2,
            invoice_add3=customer.sales_add3,
            invoice_add4=customer.sales_add4,
            invoice_add5=customer.sales_add5,
            invoice_postcode=customer.sales_postcode,
            
            # References
            invoice_order_no=credit_data.get('original_invoice', ''),
            invoice_cust_ref=credit_data.get('customer_ref', ''),
            invoice_reason_code=credit_data.get('reason_code', ''),
            invoice_reason_desc=credit_data.get('reason', ''),
            
            # Financial
            invoice_currency=customer.sales_currency,
            invoice_vat_rate=customer.sales_vat_rate,
            invoice_payment_terms=customer.sales_payment_cd,
            
            # Status
            invoice_status='DRAFT',
            invoice_posted='N'
        )
        
        self.db.add(credit_note)
        self.db.flush()
        
        # Add credit note lines
        lines = credit_data.get('lines', [])
        if not lines and credit_data.get('original_invoice'):
            # If no lines provided but original invoice specified, copy from invoice
            lines = self._get_invoice_lines_for_credit(credit_data['original_invoice'])
            
        if not lines:
            return None, "Credit note must have at least one line"
            
        total_goods = Decimal('0')
        total_vat = Decimal('0')
        
        for line_data in lines:
            line, error = self._add_credit_line(credit_note, line_data)
            if error:
                self.db.rollback()
                return None, error
                
            total_goods += line.line_goods_val
            total_vat += line.line_vat_val
            
        # Update credit note totals (negative for credits)
        credit_note.invoice_goods_val = -total_goods
        credit_note.invoice_vat_val = -total_vat
        credit_note.invoice_total_val = -(total_goods + total_vat)
        
        self.db.flush()
        
        # Log creation
        if self.current_user:
            log_user_action(
                db=self.db,
                user=self.current_user,
                action="CREATE_CREDIT_NOTE",
                table="sales_invoice_rec",
                key=credit_no,
                new_values={'customer': customer_no, 'total': float(credit_note.invoice_total_val)},
                module="SL"
            )
            
        return credit_note, None
        
    def _add_credit_line(self, credit_note: SalesInvoiceRec, line_data: Dict) -> Tuple[SalesInvoiceLineRec, Optional[str]]:
        """Add line to credit note"""
        # Validate product if stock code provided
        stock_code = line_data.get('stock_code')
        stock_item = None
        
        if stock_code:
            stock_item, status = self.stock_handler.process(4, key_value=stock_code)
            if status.fs_reply != "00":
                return None, f"Stock item {stock_code} not found"
                
        # Create line
        line_no = self.db.query(func.count(SalesInvoiceLineRec.line_id)).filter(
            SalesInvoiceLineRec.line_invoice_no == credit_note.invoice_no
        ).scalar() + 1
        
        line = SalesInvoiceLineRec(
            line_invoice_no=credit_note.invoice_no,
            line_no=line_no,
            line_stock_code=stock_code or '',
            line_description=line_data.get('description', stock_item.stock_desc if stock_item else ''),
            line_quantity=Decimal(str(line_data.get('quantity', 1))),
            line_unit=line_data.get('unit', stock_item.stock_unit if stock_item else 'EA'),
            line_unit_price=Decimal(str(line_data.get('unit_price', 0))),
            line_discount_pct=Decimal(str(line_data.get('discount_pct', 0))),
            line_vat_code=line_data.get('vat_code', credit_note.invoice_vat_rate),
            line_reason_code=line_data.get('reason_code', '')
        )
        
        # Calculate line values
        gross_value = line.line_quantity * line.line_unit_price
        
        # Apply line discount
        if line.line_discount_pct > 0:
            discount = gross_value * line.line_discount_pct / 100
            line.line_discount_val = discount
            gross_value -= discount
            
        line.line_goods_val = gross_value
        
        # Calculate VAT
        vat_rate = self._get_vat_rate(line.line_vat_code)
        line.line_vat_rate = vat_rate
        line.line_vat_val = gross_value * vat_rate / 100
        line.line_total_val = gross_value + line.line_vat_val
        
        # Cost calculation if stock item
        if stock_item:
            line.line_cost_price = stock_item.stock_average_cost
            line.line_cost_val = line.line_quantity * stock_item.stock_average_cost
            
        self.db.add(line)
        return line, None
        
    def post_credit_note(self, credit_no: str) -> Tuple[bool, Optional[str]]:
        """
        Post credit note to ledgers
        Returns (success, error_message)
        """
        credit_note = self.db.query(SalesInvoiceRec).filter(
            SalesInvoiceRec.invoice_no == credit_no
        ).first()
        
        if not credit_note:
            return False, "Credit note not found"
            
        if credit_note.invoice_type != 'CRN':
            return False, "Not a credit note"
            
        if credit_note.invoice_posted == 'Y':
            return False, "Credit note already posted"
            
        try:
            # 1. Create open item record
            open_item = self._create_open_item(credit_note)
            
            # 2. Update customer balance (reduce by credit amount)
            customer, _ = self.customer_handler.process(4, key_value=credit_note.invoice_customer)
            customer.sales_balance += credit_note.invoice_total_val  # Negative value
            customer.sales_ytd_sales += credit_note.invoice_goods_val  # Negative value
            
            self.customer_handler.process(7, record=customer)
            
            # 3. Update stock quantities (return stock)
            self._update_stock_for_credit(credit_note)
            
            # 4. Check if this is against a specific invoice
            if credit_note.invoice_order_no:  # Original invoice reference
                self._allocate_credit_to_invoice(open_item, credit_note.invoice_order_no)
                
            # 5. Post to General Ledger
            self._post_credit_to_gl(credit_note)
            
            # 6. Mark credit note as posted
            credit_note.invoice_posted = 'Y'
            credit_note.invoice_posted_date = int(datetime.now().strftime("%Y%m%d"))
            credit_note.invoice_posted_by = self.current_user.username if self.current_user else 'SYSTEM'
            
            self.db.commit()
            
            # Log posting
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="POST_CREDIT_NOTE",
                    table="sales_invoice_rec",
                    key=credit_no,
                    new_values={'total': float(credit_note.invoice_total_val)},
                    module="SL"
                )
                
            return True, None
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def allocate_credit_note(self, credit_no: str, allocations: List[Dict]) -> Tuple[bool, Optional[str]]:
        """
        Allocate credit note to invoices
        allocations: [{'invoice_no': 'INV123', 'amount': 100.00}, ...]
        """
        # Get credit note open item
        credit_oi = self.db.query(SalesOpenItemRec).filter(
            and_(
                SalesOpenItemRec.sales_oi_type == 'CRN',
                SalesOpenItemRec.sales_oi_our_ref == credit_no
            )
        ).first()
        
        if not credit_oi:
            return False, "Credit note open item not found"
            
        if credit_oi.sales_oi_amount == 0:
            return False, "Credit note already fully allocated"
            
        total_allocated = Decimal('0')
        available = abs(credit_oi.sales_oi_amount)
        
        for alloc in allocations:
            invoice_no = alloc['invoice_no']
            amount = Decimal(str(alloc['amount']))
            
            if amount <= 0:
                return False, "Allocation amount must be positive"
                
            # Get invoice open item
            invoice_oi = self.db.query(SalesOpenItemRec).filter(
                and_(
                    SalesOpenItemRec.sales_oi_type == 'INV',
                    SalesOpenItemRec.sales_oi_our_ref == invoice_no,
                    SalesOpenItemRec.sales_oi_cust == credit_oi.sales_oi_cust
                )
            ).first()
            
            if not invoice_oi:
                return False, f"Invoice {invoice_no} not found"
                
            if invoice_oi.sales_oi_amount <= 0:
                return False, f"Invoice {invoice_no} already paid"
                
            # Check allocation doesn't exceed invoice balance
            if amount > invoice_oi.sales_oi_amount:
                return False, f"Allocation exceeds invoice {invoice_no} balance"
                
            total_allocated += amount
            
        # Check total doesn't exceed credit note
        if total_allocated > available:
            return False, "Total allocations exceed credit note amount"
            
        # Apply allocations
        for alloc in allocations:
            self._apply_allocation(credit_oi, alloc['invoice_no'], Decimal(str(alloc['amount'])))
            
        self.db.commit()
        return True, None
        
    def get_unallocated_credits(self, customer_no: Optional[str] = None) -> List[Dict]:
        """Get list of unallocated credit notes"""
        query = self.db.query(SalesOpenItemRec).filter(
            and_(
                SalesOpenItemRec.sales_oi_type == 'CRN',
                SalesOpenItemRec.sales_oi_amount != 0
            )
        )
        
        if customer_no:
            query = query.filter(SalesOpenItemRec.sales_oi_cust == customer_no)
            
        credits = query.order_by(SalesOpenItemRec.sales_oi_date.desc()).all()
        
        return [
            {
                'credit_no': c.sales_oi_our_ref,
                'customer': c.sales_oi_cust,
                'date': c.sales_oi_date,
                'total': float(abs(c.sales_oi_gross)),
                'unallocated': float(abs(c.sales_oi_amount)),
                'reason': self._get_credit_reason(c.sales_oi_our_ref)
            }
            for c in credits
        ]
        
    def _get_next_credit_number(self, system_rec: SystemRec) -> str:
        """Generate next credit note number"""
        if system_rec:
            next_no = system_rec.sl_next_credit + 1
            system_rec.sl_next_credit = next_no
            return f"CRN{next_no:06d}"
        return f"CRN{datetime.now().strftime('%Y%m%d%H%M%S')}"
        
    def _get_invoice_lines_for_credit(self, invoice_no: str) -> List[Dict]:
        """Get invoice lines to copy to credit note"""
        lines = self.db.query(SalesInvoiceLineRec).filter(
            SalesInvoiceLineRec.line_invoice_no == invoice_no
        ).all()
        
        return [
            {
                'stock_code': line.line_stock_code,
                'description': line.line_description,
                'quantity': float(line.line_quantity),
                'unit_price': float(line.line_unit_price),
                'discount_pct': float(line.line_discount_pct),
                'vat_code': line.line_vat_code
            }
            for line in lines
        ]
        
    def _get_vat_rate(self, vat_code: str) -> Decimal:
        """Get VAT rate for code"""
        rates = {
            'S': Decimal('20.0'),  # Standard
            'R': Decimal('5.0'),   # Reduced
            'Z': Decimal('0.0'),   # Zero
            'E': Decimal('0.0')    # Exempt
        }
        return rates.get(vat_code, Decimal('20.0'))
        
    def _create_open_item(self, credit_note: SalesInvoiceRec) -> SalesOpenItemRec:
        """Create open item record for credit note"""
        open_item = SalesOpenItemRec(
            sales_oi_cust=credit_note.invoice_customer,
            sales_oi_type='CRN',
            sales_oi_our_ref=credit_note.invoice_no,
            sales_oi_their_ref=credit_note.invoice_cust_ref,
            sales_oi_date=credit_note.invoice_date,
            sales_oi_due_date=credit_note.invoice_date,  # Credits due immediately
            sales_oi_goods=credit_note.invoice_goods_val,
            sales_oi_tax=credit_note.invoice_vat_val,
            sales_oi_gross=credit_note.invoice_total_val,
            sales_oi_amount=credit_note.invoice_total_val,  # Outstanding amount
            sales_oi_period=credit_note.invoice_period,
            sales_oi_posted='Y'
        )
        
        # Write using handler to get proper key
        self.open_items_handler.process(5, record=open_item)
        return open_item
        
    def _update_stock_for_credit(self, credit_note: SalesInvoiceRec):
        """Update stock quantities for credit note (return stock)"""
        lines = self.db.query(SalesInvoiceLineRec).filter(
            SalesInvoiceLineRec.line_invoice_no == credit_note.invoice_no
        ).all()
        
        for line in lines:
            if line.line_stock_code:
                stock, status = self.stock_handler.process(4, key_value=line.line_stock_code)
                if status.fs_reply == "00":
                    # Return stock
                    stock.stock_on_hand += line.line_quantity
                    stock.stock_ytd_returns_qty += line.line_quantity
                    stock.stock_ytd_returns_val += line.line_goods_val
                    
                    self.stock_handler.process(7, record=stock)
                    
    def _allocate_credit_to_invoice(self, credit_oi: SalesOpenItemRec, invoice_no: str):
        """Automatically allocate credit to specific invoice"""
        # Get invoice open item
        invoice_oi = self.db.query(SalesOpenItemRec).filter(
            and_(
                SalesOpenItemRec.sales_oi_type == 'INV',
                SalesOpenItemRec.sales_oi_our_ref == invoice_no,
                SalesOpenItemRec.sales_oi_cust == credit_oi.sales_oi_cust
            )
        ).first()
        
        if invoice_oi and invoice_oi.sales_oi_amount > 0:
            # Allocate up to the smaller of credit amount or invoice balance
            alloc_amount = min(abs(credit_oi.sales_oi_amount), invoice_oi.sales_oi_amount)
            self._apply_allocation(credit_oi, invoice_no, alloc_amount)
            
    def _apply_allocation(self, credit_oi: SalesOpenItemRec, invoice_no: str, amount: Decimal):
        """Apply allocation between credit note and invoice"""
        # Get invoice open item
        invoice_oi = self.db.query(SalesOpenItemRec).filter(
            and_(
                SalesOpenItemRec.sales_oi_type == 'INV',
                SalesOpenItemRec.sales_oi_our_ref == invoice_no
            )
        ).first()
        
        if invoice_oi:
            # Reduce invoice balance
            invoice_oi.sales_oi_amount -= amount
            if abs(invoice_oi.sales_oi_amount) < Decimal('0.01'):
                invoice_oi.sales_oi_amount = Decimal('0')
                invoice_oi.sales_oi_paid_date = int(datetime.now().strftime("%Y%m%d"))
                
            # Reduce credit balance (increase as it's negative)
            credit_oi.sales_oi_amount += amount
            if abs(credit_oi.sales_oi_amount) < Decimal('0.01'):
                credit_oi.sales_oi_amount = Decimal('0')
                credit_oi.sales_oi_paid_date = int(datetime.now().strftime("%Y%m%d"))
                
            # Create allocation record (would be in separate table)
            # For now, just update the records
            self.db.flush()
            
    def _post_credit_to_gl(self, credit_note: SalesInvoiceRec):
        """Post credit note to General Ledger"""
        system_rec, _ = self.system_handler.read_system_params()
        if not system_rec or system_rec.gl_interface != 'Y':
            return
            
        je_service = JournalEntryService(self.db, self.current_user)
        
        # Create batch
        batch = je_service.create_journal_batch(
            description=f"Credit Note {credit_note.invoice_no}",
            source="SL"
        )
        
        # Credit: Customer Control Account (reduce debtors)
        je_service.add_journal_line(batch.batch_no, {
            'account': system_rec.s_debtors,
            'debit': 0,
            'credit': float(abs(credit_note.invoice_total_val)),
            'reference': credit_note.invoice_no,
            'description': f"Credit {credit_note.invoice_customer}"
        })
        
        # Debit: Sales Returns Account
        je_service.add_journal_line(batch.batch_no, {
            'account': system_rec.sl_returns_ac,
            'debit': float(abs(credit_note.invoice_goods_val)),
            'credit': 0,
            'reference': credit_note.invoice_no,
            'description': "Sales Returns"
        })
        
        # Debit: VAT Account
        if credit_note.invoice_vat_val != 0:
            je_service.add_journal_line(batch.batch_no, {
                'account': system_rec.gl_vat_ac,
                'debit': float(abs(credit_note.invoice_vat_val)),
                'credit': 0,
                'reference': credit_note.invoice_no,
                'description': "VAT on Returns"
            })
            
        # Post the batch
        je_service.post_batch(batch.batch_no)
        
    def _get_credit_reason(self, credit_no: str) -> str:
        """Get reason for credit note"""
        credit = self.db.query(SalesInvoiceRec).filter(
            SalesInvoiceRec.invoice_no == credit_no
        ).first()
        
        return credit.invoice_reason_desc if credit else ''
        
    def get_credit_note_details(self, credit_no: str) -> Dict:
        """Get full credit note details"""
        credit = self.db.query(SalesInvoiceRec).filter(
            and_(
                SalesInvoiceRec.invoice_no == credit_no,
                SalesInvoiceRec.invoice_type == 'CRN'
            )
        ).first()
        
        if not credit:
            return {"error": "Credit note not found"}
            
        # Get lines
        lines = self.db.query(SalesInvoiceLineRec).filter(
            SalesInvoiceLineRec.line_invoice_no == credit_no
        ).order_by(SalesInvoiceLineRec.line_no).all()
        
        # Get allocation status
        open_item = self.db.query(SalesOpenItemRec).filter(
            and_(
                SalesOpenItemRec.sales_oi_type == 'CRN',
                SalesOpenItemRec.sales_oi_our_ref == credit_no
            )
        ).first()
        
        allocated = float(abs(open_item.sales_oi_gross) - abs(open_item.sales_oi_amount)) if open_item else 0
        
        return {
            'header': {
                'credit_no': credit.invoice_no,
                'date': credit.invoice_date,
                'customer': credit.invoice_customer,
                'customer_name': credit.invoice_name,
                'original_invoice': credit.invoice_order_no,
                'reason': credit.invoice_reason_desc,
                'totals': {
                    'goods': float(credit.invoice_goods_val),
                    'vat': float(credit.invoice_vat_val),
                    'total': float(credit.invoice_total_val)
                },
                'status': {
                    'posted': credit.invoice_posted == 'Y',
                    'allocated': allocated,
                    'unallocated': float(abs(open_item.sales_oi_amount)) if open_item else 0
                }
            },
            'lines': [
                {
                    'line_no': line.line_no,
                    'stock_code': line.line_stock_code,
                    'description': line.line_description,
                    'quantity': float(line.line_quantity),
                    'unit_price': float(line.line_unit_price),
                    'goods_value': float(line.line_goods_val),
                    'vat_value': float(line.line_vat_val),
                    'total_value': float(line.line_total_val),
                    'reason_code': line.line_reason_code
                }
                for line in lines
            ]
        }