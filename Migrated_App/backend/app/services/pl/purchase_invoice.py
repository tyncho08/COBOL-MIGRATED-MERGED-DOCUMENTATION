"""
Purchase Invoice Service - PL030 migration
Handles purchase invoice entry, validation, and posting
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from datetime import datetime
from sqlalchemy.orm import Session
from sqlalchemy import and_, func

from app.services.file_handlers.supplier_handler import SupplierFileHandler
from app.services.file_handlers.system_handler import SystemFileHandler
from app.services.file_handlers.stock_handler import StockFileHandler
from app.services.file_handlers.open_items_handler import PurchaseOpenItemsHandler
from app.models.supplier import PurchaseLedgerRec
from app.models.purchase import PurchaseOpenItemRec, PurchaseInvoiceRec, PurchaseInvoiceLineRec
from app.models.stock import StockMasterRec
from app.models.system import SystemRec
from app.services.gl.journal_entry import JournalEntryService
from app.core.security import log_user_action
from app.models.auth import User


class PurchaseInvoiceService:
    """
    Purchase Invoice Entry functionality
    Implements PL030 - invoice creation and posting
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.supplier_handler = SupplierFileHandler(db)
        self.system_handler = SystemFileHandler(db)
        self.stock_handler = StockFileHandler(db)
        self.open_items_handler = PurchaseOpenItemsHandler(db)
        
    def create_invoice(self, invoice_data: Dict) -> Tuple[PurchaseInvoiceRec, Optional[str]]:
        """
        Create new purchase invoice
        Returns (invoice_record, error_message)
        """
        # Validate supplier
        supplier_no = invoice_data.get('supplier_no')
        supplier, status = self.supplier_handler.process(4, key_value=supplier_no)
        if status.fs_reply != "00":
            return None, "Supplier not found"
            
        # Check supplier status
        if supplier.purch_on_stop == 'Y':
            return None, "Supplier is on stop"
            
        # Get next invoice number
        system_rec, _ = self.system_handler.read_system_params()
        invoice_no = self._get_next_invoice_number(system_rec)
        
        # Create invoice header
        invoice = PurchaseInvoiceRec(
            invoice_no=invoice_no,
            invoice_supplier=supplier_no,
            invoice_date=invoice_data.get('invoice_date', int(datetime.now().strftime("%Y%m%d"))),
            invoice_period=system_rec.pl_period if system_rec else 1,
            invoice_type='INV',
            
            # Supplier details (denormalized for historical record)
            invoice_name=supplier.purch_name,
            invoice_add1=supplier.purch_add1,
            invoice_add2=supplier.purch_add2,
            invoice_add3=supplier.purch_add3,
            invoice_add4=supplier.purch_add4,
            invoice_add5=supplier.purch_add5,
            invoice_postcode=supplier.purch_postcode,
            
            # References
            invoice_supp_ref=invoice_data.get('supplier_ref', ''),
            invoice_order_no=invoice_data.get('order_no', ''),
            invoice_del_note=invoice_data.get('delivery_note', ''),
            
            # Financial
            invoice_currency=supplier.purch_currency,
            invoice_vat_rate=supplier.purch_vat_rate,
            invoice_payment_terms=supplier.purch_payment_cd,
            invoice_discount_pct=Decimal(str(invoice_data.get('discount_pct', 0))),
            
            # Status
            invoice_status='DRAFT',
            invoice_posted='N',
            invoice_approved='N',
            invoice_approval_ref='',
            invoice_disputed='N'
        )
        
        # Calculate due date based on payment terms
        invoice.invoice_due_date = self._calculate_due_date(
            invoice.invoice_date,
            supplier.purch_payment_cd
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
                action="CREATE_PURCHASE_INVOICE",
                table="purchase_invoice_rec",
                key=invoice_no,
                new_values={'supplier': supplier_no, 'total': float(invoice.invoice_total_val)},
                module="PL"
            )
            
        return invoice, None
        
    def _add_invoice_line(self, invoice: PurchaseInvoiceRec, line_data: Dict) -> Tuple[PurchaseInvoiceLineRec, Optional[str]]:
        """Add line to invoice"""
        # Validate product if stock code provided
        stock_code = line_data.get('stock_code')
        stock_item = None
        
        if stock_code:
            stock_item, status = self.stock_handler.process(4, key_value=stock_code)
            if status.fs_reply != "00":
                return None, f"Stock item {stock_code} not found"
                
        # Create line
        line_no = self.db.query(func.count(PurchaseInvoiceLineRec.line_id)).filter(
            PurchaseInvoiceLineRec.line_invoice_no == invoice.invoice_no
        ).scalar() + 1
        
        line = PurchaseInvoiceLineRec(
            line_invoice_no=invoice.invoice_no,
            line_no=line_no,
            line_stock_code=stock_code or '',
            line_description=line_data.get('description', stock_item.stock_desc if stock_item else ''),
            line_quantity=Decimal(str(line_data.get('quantity', 1))),
            line_unit=line_data.get('unit', stock_item.stock_unit if stock_item else 'EA'),
            line_unit_price=Decimal(str(line_data.get('unit_price', 0))),
            line_discount_pct=Decimal(str(line_data.get('discount_pct', 0))),
            line_vat_code=line_data.get('vat_code', invoice.invoice_vat_rate),
            line_analysis_code=line_data.get('analysis_code', ''),
            line_gl_code=line_data.get('gl_code', '')
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
        
        self.db.add(line)
        return line, None
        
    def approve_invoice(self, invoice_no: str, approval_ref: str = '') -> Tuple[bool, Optional[str]]:
        """Approve invoice for posting"""
        invoice = self.db.query(PurchaseInvoiceRec).filter(
            PurchaseInvoiceRec.invoice_no == invoice_no
        ).first()
        
        if not invoice:
            return False, "Invoice not found"
            
        if invoice.invoice_posted == 'Y':
            return False, "Cannot approve posted invoice"
            
        if invoice.invoice_approved == 'Y':
            return False, "Invoice already approved"
            
        # Validate invoice
        validation_errors = self._validate_invoice(invoice)
        if validation_errors:
            return False, "; ".join(validation_errors)
            
        # Approve invoice
        invoice.invoice_approved = 'Y'
        invoice.invoice_approved_by = self.current_user.username if self.current_user else 'SYSTEM'
        invoice.invoice_approved_date = int(datetime.now().strftime("%Y%m%d"))
        invoice.invoice_approval_ref = approval_ref
        
        self.db.flush()
        
        # Log approval
        if self.current_user:
            log_user_action(
                db=self.db,
                user=self.current_user,
                action="APPROVE_PURCHASE_INVOICE",
                table="purchase_invoice_rec",
                key=invoice_no,
                new_values={'approval_ref': approval_ref},
                module="PL"
            )
            
        return True, None
        
    def post_invoice(self, invoice_no: str) -> Tuple[bool, Optional[str]]:
        """
        Post invoice to ledgers
        Returns (success, error_message)
        """
        invoice = self.db.query(PurchaseInvoiceRec).filter(
            PurchaseInvoiceRec.invoice_no == invoice_no
        ).first()
        
        if not invoice:
            return False, "Invoice not found"
            
        if invoice.invoice_posted == 'Y':
            return False, "Invoice already posted"
            
        if invoice.invoice_approved != 'Y':
            return False, "Invoice not approved"
            
        try:
            # 1. Create open item record
            open_item = self._create_open_item(invoice)
            
            # 2. Update supplier balance
            supplier, _ = self.supplier_handler.process(4, key_value=invoice.invoice_supplier)
            supplier.purch_balance += invoice.invoice_total_val
            supplier.purch_ytd_purch += invoice.invoice_goods_val
            supplier.purch_date_last_inv = int(datetime.now().strftime("%Y%m%d"))
            supplier.purch_invoice_cnt += 1
            
            # Update highest balance
            if supplier.purch_balance > supplier.purch_highest_bal:
                supplier.purch_highest_bal = supplier.purch_balance
                
            self.supplier_handler.process(7, record=supplier)
            
            # 3. Update stock quantities and values
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
                    action="POST_PURCHASE_INVOICE",
                    table="purchase_invoice_rec",
                    key=invoice_no,
                    new_values={'total': float(invoice.invoice_total_val)},
                    module="PL"
                )
                
            return True, None
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def dispute_invoice(self, invoice_no: str, reason: str) -> Tuple[bool, Optional[str]]:
        """Mark invoice as disputed"""
        invoice = self.db.query(PurchaseInvoiceRec).filter(
            PurchaseInvoiceRec.invoice_no == invoice_no
        ).first()
        
        if not invoice:
            return False, "Invoice not found"
            
        invoice.invoice_disputed = 'Y'
        invoice.invoice_dispute_date = int(datetime.now().strftime("%Y%m%d"))
        invoice.invoice_dispute_reason = reason
        invoice.invoice_dispute_by = self.current_user.username if self.current_user else 'SYSTEM'
        
        # Update open item if posted
        if invoice.invoice_posted == 'Y':
            open_item = self.db.query(PurchaseOpenItemRec).filter(
                and_(
                    PurchaseOpenItemRec.purch_oi_type == 'INV',
                    PurchaseOpenItemRec.purch_oi_our_ref == invoice_no
                )
            ).first()
            
            if open_item:
                open_item.purch_oi_disputed = 'Y'
                
        self.db.flush()
        
        # Log dispute
        if self.current_user:
            log_user_action(
                db=self.db,
                user=self.current_user,
                action="DISPUTE_PURCHASE_INVOICE",
                table="purchase_invoice_rec",
                key=invoice_no,
                new_values={'reason': reason},
                module="PL"
            )
            
        return True, None
        
    def get_invoice_details(self, invoice_no: str) -> Dict:
        """Get full invoice details"""
        invoice = self.db.query(PurchaseInvoiceRec).filter(
            PurchaseInvoiceRec.invoice_no == invoice_no
        ).first()
        
        if not invoice:
            return {"error": "Invoice not found"}
            
        # Get lines
        lines = self.db.query(PurchaseInvoiceLineRec).filter(
            PurchaseInvoiceLineRec.line_invoice_no == invoice_no
        ).order_by(PurchaseInvoiceLineRec.line_no).all()
        
        # Get supplier details
        supplier, _ = self.supplier_handler.process(4, key_value=invoice.invoice_supplier)
        
        return {
            'header': {
                'invoice_no': invoice.invoice_no,
                'date': invoice.invoice_date,
                'due_date': invoice.invoice_due_date,
                'supplier': {
                    'number': invoice.invoice_supplier,
                    'name': invoice.invoice_name,
                    'address': [
                        invoice.invoice_add1,
                        invoice.invoice_add2,
                        invoice.invoice_add3,
                        invoice.invoice_add4,
                        invoice.invoice_postcode
                    ]
                },
                'references': {
                    'supplier_ref': invoice.invoice_supp_ref,
                    'order_no': invoice.invoice_order_no,
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
                    'approved': invoice.invoice_approved == 'Y',
                    'disputed': invoice.invoice_disputed == 'Y',
                    'approval_ref': invoice.invoice_approval_ref
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
                    'total_value': float(line.line_total_val),
                    'gl_code': line.line_gl_code
                }
                for line in lines
            ]
        }
        
    def get_unapproved_invoices(self, supplier_no: Optional[str] = None) -> List[Dict]:
        """Get list of invoices pending approval"""
        query = self.db.query(PurchaseInvoiceRec).filter(
            and_(
                PurchaseInvoiceRec.invoice_approved != 'Y',
                PurchaseInvoiceRec.invoice_posted != 'Y'
            )
        )
        
        if supplier_no:
            query = query.filter(PurchaseInvoiceRec.invoice_supplier == supplier_no)
            
        invoices = query.order_by(PurchaseInvoiceRec.invoice_date.desc()).all()
        
        return [
            {
                'invoice_no': inv.invoice_no,
                'supplier': inv.invoice_supplier,
                'supplier_name': inv.invoice_name,
                'date': inv.invoice_date,
                'supplier_ref': inv.invoice_supp_ref,
                'total': float(inv.invoice_total_val),
                'disputed': inv.invoice_disputed == 'Y'
            }
            for inv in invoices
        ]
        
    def _get_next_invoice_number(self, system_rec: SystemRec) -> str:
        """Generate next invoice number"""
        if system_rec:
            next_no = system_rec.pl_next_invoice + 1
            system_rec.pl_next_invoice = next_no
            return f"PI{next_no:06d}"
        return f"PI{datetime.now().strftime('%Y%m%d%H%M%S')}"
        
    def _calculate_due_date(self, invoice_date: int, payment_terms: str) -> int:
        """Calculate due date based on payment terms"""
        # Parse payment terms (e.g., "C30" = Current + 30 days)
        if not payment_terms:
            return invoice_date
            
        try:
            if payment_terms.startswith('C'):
                days = int(payment_terms[1:])
            elif payment_terms.startswith('EOM'):
                # End of month
                invoice_dt = datetime.strptime(str(invoice_date), "%Y%m%d")
                # Get last day of month
                if invoice_dt.month == 12:
                    due_dt = invoice_dt.replace(day=31)
                else:
                    due_dt = invoice_dt.replace(month=invoice_dt.month + 1, day=1) - timedelta(days=1)
                return int(due_dt.strftime("%Y%m%d"))
            else:
                days = 30  # Default
                
            invoice_dt = datetime.strptime(str(invoice_date), "%Y%m%d")
            due_dt = invoice_dt + timedelta(days=days)
            return int(due_dt.strftime("%Y%m%d"))
        except:
            return invoice_date
            
    def _get_vat_rate(self, vat_code: str) -> Decimal:
        """Get VAT rate for code"""
        rates = {
            'S': Decimal('20.0'),  # Standard
            'R': Decimal('5.0'),   # Reduced
            'Z': Decimal('0.0'),   # Zero
            'E': Decimal('0.0')    # Exempt
        }
        return rates.get(vat_code, Decimal('20.0'))
        
    def _validate_invoice(self, invoice: PurchaseInvoiceRec) -> List[str]:
        """Validate invoice before approval/posting"""
        errors = []
        
        # Check totals
        if invoice.invoice_total_val <= 0:
            errors.append("Invoice total must be greater than zero")
            
        # Check supplier
        supplier, status = self.supplier_handler.process(4, key_value=invoice.invoice_supplier)
        if status.fs_reply != "00":
            errors.append("Supplier not found")
        elif supplier.purch_on_stop == 'Y':
            errors.append("Supplier is on stop")
            
        # Check lines exist
        line_count = self.db.query(func.count(PurchaseInvoiceLineRec.line_id)).filter(
            PurchaseInvoiceLineRec.line_invoice_no == invoice.invoice_no
        ).scalar()
        
        if line_count == 0:
            errors.append("Invoice has no lines")
            
        # Check GL codes on lines
        lines = self.db.query(PurchaseInvoiceLineRec).filter(
            PurchaseInvoiceLineRec.line_invoice_no == invoice.invoice_no
        ).all()
        
        for line in lines:
            if not line.line_gl_code and not line.line_stock_code:
                errors.append(f"Line {line.line_no} missing GL code")
                
        return errors
        
    def _create_open_item(self, invoice: PurchaseInvoiceRec) -> PurchaseOpenItemRec:
        """Create open item record for invoice"""
        open_item = PurchaseOpenItemRec(
            purch_oi_supp=invoice.invoice_supplier,
            purch_oi_type='INV',
            purch_oi_our_ref=invoice.invoice_no,
            purch_oi_their_ref=invoice.invoice_supp_ref,
            purch_oi_date=invoice.invoice_date,
            purch_oi_due_date=invoice.invoice_due_date,
            purch_oi_goods=invoice.invoice_goods_val,
            purch_oi_tax=invoice.invoice_vat_val,
            purch_oi_gross=invoice.invoice_total_val,
            purch_oi_amount=invoice.invoice_total_val,  # Outstanding amount
            purch_oi_period=invoice.invoice_period,
            purch_oi_posted='Y',
            purch_oi_approved=invoice.invoice_approved,
            purch_oi_disputed=invoice.invoice_disputed
        )
        
        # Write using handler to get proper key
        self.open_items_handler.process(5, record=open_item)
        return open_item
        
    def _update_stock_for_invoice(self, invoice: PurchaseInvoiceRec):
        """Update stock quantities and values"""
        lines = self.db.query(PurchaseInvoiceLineRec).filter(
            PurchaseInvoiceLineRec.line_invoice_no == invoice.invoice_no
        ).all()
        
        for line in lines:
            if line.line_stock_code:
                stock, status = self.stock_handler.process(4, key_value=line.line_stock_code)
                if status.fs_reply == "00":
                    # Update quantity
                    stock.stock_on_hand += line.line_quantity
                    stock.stock_on_order -= line.line_quantity  # Reduce on order
                    
                    # Update value and average cost
                    total_value = (stock.stock_on_hand * stock.stock_average_cost) + line.line_goods_val
                    new_qty = stock.stock_on_hand + line.line_quantity
                    if new_qty > 0:
                        stock.stock_average_cost = total_value / new_qty
                        
                    # Update YTD
                    stock.stock_ytd_receipts_qty += line.line_quantity
                    stock.stock_ytd_receipts_val += line.line_goods_val
                    
                    self.stock_handler.process(7, record=stock)
                    
    def _post_invoice_to_gl(self, invoice: PurchaseInvoiceRec):
        """Post invoice to General Ledger"""
        system_rec, _ = self.system_handler.read_system_params()
        if not system_rec or system_rec.gl_interface != 'Y':
            return
            
        je_service = JournalEntryService(self.db, self.current_user)
        
        # Create batch
        batch = je_service.create_journal_batch(
            description=f"Purchase Invoice {invoice.invoice_no}",
            source="PL"
        )
        
        # Credit: Supplier Control Account
        je_service.add_journal_line(batch.batch_no, {
            'account': system_rec.p_creditors,
            'debit': 0,
            'credit': float(invoice.invoice_total_val),
            'reference': invoice.invoice_no,
            'description': f"Invoice {invoice.invoice_supplier}"
        })
        
        # Debit: Expense/Asset Account(s)
        lines = self.db.query(PurchaseInvoiceLineRec).filter(
            PurchaseInvoiceLineRec.line_invoice_no == invoice.invoice_no
        ).all()
        
        # Group by GL code
        gl_totals = {}
        for line in lines:
            gl_code = line.line_gl_code or system_rec.bl_purch_ac
            if gl_code not in gl_totals:
                gl_totals[gl_code] = Decimal('0')
            gl_totals[gl_code] += line.line_goods_val
            
        for gl_code, amount in gl_totals.items():
            je_service.add_journal_line(batch.batch_no, {
                'account': int(gl_code),
                'debit': float(amount),
                'credit': 0,
                'reference': invoice.invoice_no,
                'description': "Purchases"
            })
            
        # Debit: VAT Account
        if invoice.invoice_vat_val > 0:
            je_service.add_journal_line(batch.batch_no, {
                'account': system_rec.gl_vat_ac,
                'debit': float(invoice.invoice_vat_val),
                'credit': 0,
                'reference': invoice.invoice_no,
                'description': "VAT"
            })
            
        # Post the batch
        je_service.post_batch(batch.batch_no)
        
    def match_to_order(self, invoice_no: str, order_no: str) -> Tuple[bool, Optional[str]]:
        """Match invoice to purchase order"""
        invoice = self.db.query(PurchaseInvoiceRec).filter(
            PurchaseInvoiceRec.invoice_no == invoice_no
        ).first()
        
        if not invoice:
            return False, "Invoice not found"
            
        # This would match against purchase order table
        # For now, just update reference
        invoice.invoice_order_no = order_no
        invoice.invoice_matched = 'Y'
        invoice.invoice_match_date = int(datetime.now().strftime("%Y%m%d"))
        
        self.db.flush()
        
        return True, None
        
    def import_invoice(self, import_data: Dict) -> Tuple[PurchaseInvoiceRec, Optional[str]]:
        """Import invoice from external source (EDI, OCR, etc.)"""
        # Map import data to invoice format
        invoice_data = {
            'supplier_no': import_data.get('supplier_no'),
            'supplier_ref': import_data.get('supplier_ref'),
            'invoice_date': import_data.get('invoice_date'),
            'lines': []
        }
        
        # Map lines
        for import_line in import_data.get('lines', []):
            line_data = {
                'description': import_line.get('description'),
                'quantity': import_line.get('quantity'),
                'unit_price': import_line.get('unit_price'),
                'vat_code': import_line.get('vat_code', 'S'),
                'gl_code': import_line.get('gl_code', '')
            }
            
            # Try to match to stock item
            if import_line.get('supplier_product_code'):
                # Would lookup cross-reference table
                pass
                
            invoice_data['lines'].append(line_data)
            
        # Create invoice
        return self.create_invoice(invoice_data)