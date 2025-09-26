"""
Sales Invoice Service - SL030 migration
Handles sales invoice creation, validation, and posting
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
from app.models.sales import SalesOpenItemRec
from app.models.customer import SalesInvoiceRec, SalesInvoiceLineRec
from app.models.stock import StockMasterRec
from app.models.system import SystemRec
from app.services.gl.journal_entry import JournalEntryService
from app.core.security import log_user_action
from app.models.auth import User


class SalesInvoiceService:
    """
    Sales Invoice Entry functionality
    Implements SL030 - invoice creation and posting
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.customer_handler = CustomerFileHandler(db)
        self.system_handler = SystemFileHandler(db)
        self.stock_handler = StockFileHandler(db)
        self.open_items_handler = SalesOpenItemsHandler(db)
        
    def create_invoice(self, invoice_data: Dict) -> Tuple[SalesInvoiceRec, Optional[str]]:
        """
        Create new sales invoice
        Returns (invoice_record, error_message)
        """
        # Validate customer
        customer_no = invoice_data.get('customer_no')
        customer, status = self.customer_handler.process(4, key_value=customer_no)
        if status.fs_reply != "00":
            return None, "Customer not found"
            
        # Check customer status
        if customer.sales_on_stop == 'Y':
            return None, "Customer is on stop"
            
        # Get next invoice number
        system_rec, _ = self.system_handler.read_system_params()
        invoice_no = self._get_next_invoice_number(system_rec)
        
        # Create invoice header
        invoice = SalesInvoiceRec(
            invoice_no=invoice_no,
            invoice_customer=customer_no,
            invoice_date=invoice_data.get('invoice_date', int(datetime.now().strftime("%Y%m%d"))),
            invoice_period=system_rec.sl_period if system_rec else 1,
            invoice_type='INV',
            
            # Customer details (denormalized for historical record)
            invoice_name=customer.sales_name,
            invoice_add1=customer.sales_add1,
            invoice_add2=customer.sales_add2,
            invoice_add3=customer.sales_add3,
            invoice_add4=customer.sales_add4,
            invoice_add5=customer.sales_add5,
            invoice_postcode=customer.sales_postcode,
            
            # Delivery details
            invoice_del_add1=invoice_data.get('del_add1', customer.sales_add1),
            invoice_del_add2=invoice_data.get('del_add2', customer.sales_add2),
            invoice_del_add3=invoice_data.get('del_add3', customer.sales_add3),
            invoice_del_add4=invoice_data.get('del_add4', customer.sales_add4),
            invoice_del_postcode=invoice_data.get('del_postcode', customer.sales_postcode),
            
            # References
            invoice_order_no=invoice_data.get('order_no', ''),
            invoice_cust_ref=invoice_data.get('customer_ref', ''),
            invoice_del_note=invoice_data.get('delivery_note', ''),
            
            # Financial
            invoice_currency=customer.sales_currency,
            invoice_vat_rate=customer.sales_vat_rate,
            invoice_payment_terms=customer.sales_payment_cd,
            invoice_discount_pct=Decimal(str(invoice_data.get('discount_pct', 0))),
            
            # Status
            invoice_status='DRAFT',
            invoice_posted='N',
            invoice_printed='N',
            invoice_emailed='N'
        )
        
        # Calculate due date based on payment terms
        invoice.invoice_due_date = self._calculate_due_date(
            invoice.invoice_date,
            customer.sales_payment_cd
        )
        
        self.db.add(invoice)
        self.db.flush()
        
        # Add invoice lines
        lines = invoice_data.get('lines', [])
        if not lines:
            return None, "Invoice must have at least one line"
            
        total_goods = Decimal('0')
        total_vat = Decimal('0')
        
        for line_data in lines:
            line, error = self._add_invoice_line(invoice, line_data)
            if error:
                self.db.rollback()
                return None, error
                
            total_goods += line.line_goods_val
            total_vat += line.line_vat_val
            
        # Update invoice totals
        invoice.invoice_goods_val = total_goods
        invoice.invoice_vat_val = total_vat
        invoice.invoice_total_val = total_goods + total_vat
        
        # Apply header discount if any
        if invoice.invoice_discount_pct > 0:
            discount_amt = invoice.invoice_goods_val * invoice.invoice_discount_pct / 100
            invoice.invoice_discount_val = discount_amt
            invoice.invoice_goods_val -= discount_amt
            invoice.invoice_total_val = invoice.invoice_goods_val + invoice.invoice_vat_val
            
        self.db.flush()
        
        # Log creation
        if self.current_user:
            log_user_action(
                db=self.db,
                user=self.current_user,
                action="CREATE_INVOICE",
                table="sales_invoice_rec",
                key=invoice_no,
                new_values={'customer': customer_no, 'total': float(invoice.invoice_total_val)},
                module="SL"
            )
            
        return invoice, None
        
    def _add_invoice_line(self, invoice: SalesInvoiceRec, line_data: Dict) -> Tuple[SalesInvoiceLineRec, Optional[str]]:
        """Add line to invoice"""
        # Validate product if stock code provided
        stock_code = line_data.get('stock_code')
        stock_item = None
        
        if stock_code:
            stock_item, status = self.stock_handler.process(4, key_value=stock_code)
            if status.fs_reply != "00":
                return None, f"Stock item {stock_code} not found"
                
        # Create line
        line_no = self.db.query(func.count(SalesInvoiceLineRec.line_id)).filter(
            SalesInvoiceLineRec.line_invoice_no == invoice.invoice_no
        ).scalar() + 1
        
        line = SalesInvoiceLineRec(
            line_invoice_no=invoice.invoice_no,
            line_no=line_no,
            line_stock_code=stock_code or '',
            line_description=line_data.get('description', stock_item.stock_desc if stock_item else ''),
            line_quantity=Decimal(str(line_data.get('quantity', 1))),
            line_unit=line_data.get('unit', stock_item.stock_unit if stock_item else 'EA'),
            line_unit_price=Decimal(str(line_data.get('unit_price', 0))),
            line_discount_pct=Decimal(str(line_data.get('discount_pct', 0))),
            line_vat_code=line_data.get('vat_code', invoice.invoice_vat_rate),
            line_analysis_code=line_data.get('analysis_code', '')
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
        
    def update_invoice(self, invoice_no: str, updates: Dict) -> Tuple[SalesInvoiceRec, Optional[str]]:
        """Update draft invoice"""
        invoice = self.db.query(SalesInvoiceRec).filter(
            SalesInvoiceRec.invoice_no == invoice_no
        ).first()
        
        if not invoice:
            return None, "Invoice not found"
            
        if invoice.invoice_posted == 'Y':
            return None, "Cannot update posted invoice"
            
        # Apply updates
        for field, value in updates.items():
            if hasattr(invoice, field) and field not in ['invoice_no', 'invoice_posted']:
                setattr(invoice, field, value)
                
        self.db.flush()
        return invoice, None
        
    def post_invoice(self, invoice_no: str) -> Tuple[bool, Optional[str]]:
        """
        Post invoice to ledgers
        Returns (success, error_message)
        """
        invoice = self.db.query(SalesInvoiceRec).filter(
            SalesInvoiceRec.invoice_no == invoice_no
        ).first()
        
        if not invoice:
            return False, "Invoice not found"
            
        if invoice.invoice_posted == 'Y':
            return False, "Invoice already posted"
            
        # Validate invoice
        validation_errors = self._validate_invoice(invoice)
        if validation_errors:
            return False, "; ".join(validation_errors)
            
        try:
            # 1. Create open item record
            open_item = self._create_open_item(invoice)
            
            # 2. Update customer balance
            customer, _ = self.customer_handler.process(4, key_value=invoice.invoice_customer)
            customer.sales_balance += invoice.invoice_total_val
            customer.sales_ytd_sales += invoice.invoice_goods_val
            customer.sales_date_last_inv = int(datetime.now().strftime("%Y%m%d"))
            customer.sales_invoice_cnt += 1
            
            # Update highest balance
            if customer.sales_balance > customer.sales_highest_bal:
                customer.sales_highest_bal = customer.sales_balance
                
            self.customer_handler.process(7, record=customer)
            
            # 3. Update stock quantities and costs
            self._update_stock_for_invoice(invoice)
            
            # 4. Post to General Ledger
            self._post_invoice_to_gl(invoice)
            
            # 5. Mark invoice as posted
            invoice.invoice_posted = 'Y'
            invoice.invoice_posted_date = int(datetime.now().strftime("%Y%m%d"))
            invoice.invoice_posted_by = self.current_user.username if self.current_user else 'SYSTEM'
            
            self.db.commit()
            
            # Log posting
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="POST_INVOICE",
                    table="sales_invoice_rec",
                    key=invoice_no,
                    new_values={'total': float(invoice.invoice_total_val)},
                    module="SL"
                )
                
            return True, None
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def void_invoice(self, invoice_no: str, reason: str) -> Tuple[bool, Optional[str]]:
        """Void posted invoice by creating credit note"""
        invoice = self.db.query(SalesInvoiceRec).filter(
            SalesInvoiceRec.invoice_no == invoice_no
        ).first()
        
        if not invoice:
            return False, "Invoice not found"
            
        if invoice.invoice_posted != 'Y':
            return False, "Invoice not posted"
            
        # Create reversing credit note
        credit_note_data = {
            'customer_no': invoice.invoice_customer,
            'invoice_date': int(datetime.now().strftime("%Y%m%d")),
            'original_invoice': invoice_no,
            'reason': reason,
            'lines': []
        }
        
        # Copy all lines as negative
        lines = self.db.query(SalesInvoiceLineRec).filter(
            SalesInvoiceLineRec.line_invoice_no == invoice_no
        ).all()
        
        for line in lines:
            credit_note_data['lines'].append({
                'stock_code': line.line_stock_code,
                'description': line.line_description,
                'quantity': float(line.line_quantity),
                'unit_price': float(line.line_unit_price),
                'discount_pct': float(line.line_discount_pct),
                'vat_code': line.line_vat_code
            })
            
        # Create credit note
        from app.services.sl.credit_note import CreditNoteService
        cn_service = CreditNoteService(self.db, self.current_user)
        credit_note, error = cn_service.create_credit_note(credit_note_data)
        
        if error:
            return False, error
            
        # Post credit note
        success, error = cn_service.post_credit_note(credit_note.invoice_no)
        
        if not success:
            return False, error
            
        # Mark original invoice as voided
        invoice.invoice_voided = 'Y'
        invoice.invoice_void_date = int(datetime.now().strftime("%Y%m%d"))
        invoice.invoice_void_reason = reason
        invoice.invoice_void_credit = credit_note.invoice_no
        
        self.db.flush()
        
        return True, None
        
    def get_invoice_details(self, invoice_no: str) -> Dict:
        """Get full invoice details"""
        invoice = self.db.query(SalesInvoiceRec).filter(
            SalesInvoiceRec.invoice_no == invoice_no
        ).first()
        
        if not invoice:
            return {"error": "Invoice not found"}
            
        # Get lines
        lines = self.db.query(SalesInvoiceLineRec).filter(
            SalesInvoiceLineRec.line_invoice_no == invoice_no
        ).order_by(SalesInvoiceLineRec.line_no).all()
        
        # Get customer details
        customer, _ = self.customer_handler.process(4, key_value=invoice.invoice_customer)
        
        return {
            'header': {
                'invoice_no': invoice.invoice_no,
                'date': invoice.invoice_date,
                'due_date': invoice.invoice_due_date,
                'customer': {
                    'number': invoice.invoice_customer,
                    'name': invoice.invoice_name,
                    'address': [
                        invoice.invoice_add1,
                        invoice.invoice_add2,
                        invoice.invoice_add3,
                        invoice.invoice_add4,
                        invoice.invoice_postcode
                    ]
                },
                'delivery': {
                    'address': [
                        invoice.invoice_del_add1,
                        invoice.invoice_del_add2,
                        invoice.invoice_del_add3,
                        invoice.invoice_del_add4,
                        invoice.invoice_del_postcode
                    ]
                },
                'references': {
                    'order_no': invoice.invoice_order_no,
                    'customer_ref': invoice.invoice_cust_ref,
                    'delivery_note': invoice.invoice_del_note
                },
                'totals': {
                    'goods': float(invoice.invoice_goods_val),
                    'discount': float(invoice.invoice_discount_val),
                    'vat': float(invoice.invoice_vat_val),
                    'total': float(invoice.invoice_total_val)
                },
                'status': {
                    'posted': invoice.invoice_posted == 'Y',
                    'printed': invoice.invoice_printed == 'Y',
                    'emailed': invoice.invoice_emailed == 'Y',
                    'voided': invoice.invoice_voided == 'Y'
                }
            },
            'lines': [
                {
                    'line_no': line.line_no,
                    'stock_code': line.line_stock_code,
                    'description': line.line_description,
                    'quantity': float(line.line_quantity),
                    'unit': line.line_unit,
                    'unit_price': float(line.line_unit_price),
                    'discount_pct': float(line.line_discount_pct),
                    'goods_value': float(line.line_goods_val),
                    'vat_rate': float(line.line_vat_rate),
                    'vat_value': float(line.line_vat_val),
                    'total_value': float(line.line_total_val)
                }
                for line in lines
            ]
        }
        
    def _get_next_invoice_number(self, system_rec: SystemRec) -> str:
        """Generate next invoice number"""
        if system_rec:
            next_no = system_rec.sl_next_invoice + 1
            system_rec.sl_next_invoice = next_no
            return f"INV{next_no:06d}"
        return f"INV{datetime.now().strftime('%Y%m%d%H%M%S')}"
        
    def _calculate_due_date(self, invoice_date: int, payment_terms: str) -> int:
        """Calculate due date based on payment terms"""
        # Parse payment terms (e.g., "C30" = Current + 30 days)
        if not payment_terms:
            return invoice_date
            
        try:
            if payment_terms.startswith('C'):
                days = int(payment_terms[1:])
            else:
                days = 30  # Default
                
            invoice_dt = datetime.strptime(str(invoice_date), "%Y%m%d")
            due_dt = invoice_dt + timedelta(days=days)
            return int(due_dt.strftime("%Y%m%d"))
        except:
            return invoice_date
            
    def _get_vat_rate(self, vat_code: str) -> Decimal:
        """Get VAT rate for code"""
        # This would lookup VAT rates table
        # For now, use standard rates
        rates = {
            'S': Decimal('20.0'),  # Standard
            'R': Decimal('5.0'),   # Reduced
            'Z': Decimal('0.0'),   # Zero
            'E': Decimal('0.0')    # Exempt
        }
        return rates.get(vat_code, Decimal('20.0'))
        
    def _validate_invoice(self, invoice: SalesInvoiceRec) -> List[str]:
        """Validate invoice before posting"""
        errors = []
        
        # Check totals
        if invoice.invoice_total_val <= 0:
            errors.append("Invoice total must be greater than zero")
            
        # Check customer
        customer, status = self.customer_handler.process(4, key_value=invoice.invoice_customer)
        if status.fs_reply != "00":
            errors.append("Customer not found")
        elif customer.sales_on_stop == 'Y':
            errors.append("Customer is on stop")
            
        # Check lines exist
        line_count = self.db.query(func.count(SalesInvoiceLineRec.line_id)).filter(
            SalesInvoiceLineRec.line_invoice_no == invoice.invoice_no
        ).scalar()
        
        if line_count == 0:
            errors.append("Invoice has no lines")
            
        return errors
        
    def _create_open_item(self, invoice: SalesInvoiceRec) -> SalesOpenItemRec:
        """Create open item record for invoice"""
        open_item = SalesOpenItemRec(
            sales_oi_cust=invoice.invoice_customer,
            sales_oi_type='INV',
            sales_oi_our_ref=invoice.invoice_no,
            sales_oi_their_ref=invoice.invoice_cust_ref,
            sales_oi_date=invoice.invoice_date,
            sales_oi_due_date=invoice.invoice_due_date,
            sales_oi_goods=invoice.invoice_goods_val,
            sales_oi_tax=invoice.invoice_vat_val,
            sales_oi_gross=invoice.invoice_total_val,
            sales_oi_amount=invoice.invoice_total_val,  # Outstanding amount
            sales_oi_period=invoice.invoice_period,
            sales_oi_posted='Y',
            sales_oi_analysis_1=invoice.invoice_analysis_1,
            sales_oi_analysis_2=invoice.invoice_analysis_2
        )
        
        # Write using handler to get proper key
        self.open_items_handler.process(5, record=open_item)
        return open_item
        
    def _update_stock_for_invoice(self, invoice: SalesInvoiceRec):
        """Update stock quantities and costs"""
        lines = self.db.query(SalesInvoiceLineRec).filter(
            SalesInvoiceLineRec.line_invoice_no == invoice.invoice_no
        ).all()
        
        for line in lines:
            if line.line_stock_code:
                stock, status = self.stock_handler.process(4, key_value=line.line_stock_code)
                if status.fs_reply == "00":
                    # Update quantity
                    stock.stock_on_hand -= line.line_quantity
                    stock.stock_ytd_sales_qty += line.line_quantity
                    stock.stock_ytd_sales_val += line.line_goods_val
                    
                    # Update customer YTD cost
                    customer, _ = self.customer_handler.process(4, key_value=invoice.invoice_customer)
                    if customer and line.line_cost_val:
                        customer.sales_ytd_cost += line.line_cost_val
                        self.customer_handler.process(7, record=customer)
                        
                    self.stock_handler.process(7, record=stock)
                    
    def _post_invoice_to_gl(self, invoice: SalesInvoiceRec):
        """Post invoice to General Ledger"""
        system_rec, _ = self.system_handler.read_system_params()
        if not system_rec or system_rec.gl_interface != 'Y':
            return
            
        je_service = JournalEntryService(self.db, self.current_user)
        
        # Create batch
        batch = je_service.create_journal_batch(
            description=f"Sales Invoice {invoice.invoice_no}",
            source="SL"
        )
        
        # Debit: Customer Control Account
        je_service.add_journal_line(batch.batch_no, {
            'account': system_rec.s_debtors,
            'debit': float(invoice.invoice_total_val),
            'credit': 0,
            'reference': invoice.invoice_no,
            'description': f"Invoice {invoice.invoice_customer}"
        })
        
        # Credit: Sales Account(s)
        # This would be more complex with multiple sales accounts per line
        je_service.add_journal_line(batch.batch_no, {
            'account': system_rec.sl_sales_ac,
            'debit': 0,
            'credit': float(invoice.invoice_goods_val),
            'reference': invoice.invoice_no,
            'description': "Sales"
        })
        
        # Credit: VAT Account
        if invoice.invoice_vat_val > 0:
            je_service.add_journal_line(batch.batch_no, {
                'account': system_rec.gl_vat_ac,
                'debit': 0,
                'credit': float(invoice.invoice_vat_val),
                'reference': invoice.invoice_no,
                'description': "VAT"
            })
            
        # Post the batch
        je_service.post_batch(batch.batch_no)
        
    def generate_invoice_pdf(self, invoice_no: str) -> bytes:
        """Generate PDF for invoice"""
        # This would use a PDF generation library
        # For now, return empty bytes
        return b''
        
    def email_invoice(self, invoice_no: str, email_data: Dict) -> Tuple[bool, Optional[str]]:
        """Email invoice to customer"""
        invoice = self.db.query(SalesInvoiceRec).filter(
            SalesInvoiceRec.invoice_no == invoice_no
        ).first()
        
        if not invoice:
            return False, "Invoice not found"
            
        # Generate PDF
        pdf_data = self.generate_invoice_pdf(invoice_no)
        
        # Send email (would integrate with email service)
        # For now, just mark as emailed
        invoice.invoice_emailed = 'Y'
        invoice.invoice_email_date = int(datetime.now().strftime("%Y%m%d"))
        
        self.db.flush()
        
        return True, None