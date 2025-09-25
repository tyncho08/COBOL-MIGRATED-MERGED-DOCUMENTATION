"""
Customer Master Service - SL010 migration
Handles customer creation, maintenance, and validation
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from datetime import datetime
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func

from app.services.file_handlers.customer_handler import CustomerFileHandler
from app.services.file_handlers.system_handler import SystemFileHandler
from app.models.customer import SalesLedgerRec, CustomerContactRec, CustomerCreditRec
from app.core.security import log_user_action
from app.models.auth import User


class CustomerMasterService:
    """
    Customer Master maintenance
    Implements SL010 functionality
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.customer_handler = CustomerFileHandler(db)
        self.system_handler = SystemFileHandler(db)
        
    def create_customer(self, customer_data: Dict) -> Tuple[SalesLedgerRec, Optional[str]]:
        """
        Create new customer
        Returns (customer_record, error_message)
        """
        # Validate customer number
        customer_no = customer_data.get('sales_cust', '')
        if not self._validate_customer_number(customer_no):
            return None, "Invalid customer number format"
            
        # Check if already exists
        existing, status = self.customer_handler.process(4, key_value=customer_no)
        if status.fs_reply == "00":
            return None, "Customer already exists"
            
        # Get system defaults
        system_rec, _ = self.system_handler.read_system_params()
        
        # Create customer record
        customer = SalesLedgerRec(
            sales_cust=customer_no,
            sales_name=customer_data.get('sales_name', ''),
            sales_add1=customer_data.get('sales_add1', ''),
            sales_add2=customer_data.get('sales_add2', ''),
            sales_add3=customer_data.get('sales_add3', ''),
            sales_add4=customer_data.get('sales_add4', ''),
            sales_add5=customer_data.get('sales_add5', ''),
            sales_postcode=customer_data.get('sales_postcode', ''),
            sales_country=customer_data.get('sales_country', 'GB'),
            sales_telno=customer_data.get('sales_telno', ''),
            sales_faxno=customer_data.get('sales_faxno', ''),
            sales_email=customer_data.get('sales_email', ''),
            sales_contact=customer_data.get('sales_contact', ''),
            
            # Financial info
            sales_balance=Decimal('0'),
            sales_os_orders=Decimal('0'),
            sales_credit_lim=Decimal(str(customer_data.get('sales_credit_lim', 0))),
            sales_payment_cd=customer_data.get('sales_payment_cd', 'C30'),
            sales_vat_no=customer_data.get('sales_vat_no', ''),
            sales_vat_rate=customer_data.get('sales_vat_rate', system_rec.s_vat_rate if system_rec else ''),
            
            # Status fields
            sales_active='Y',
            sales_on_stop='N',
            sales_stop_date=0,
            sales_account_type=customer_data.get('sales_account_type', 'TRADE'),
            
            # Dates
            sales_date_opened=int(datetime.now().strftime("%Y%m%d")),
            sales_date_last_inv=0,
            sales_date_last_pay=0,
            
            # Defaults
            sales_price_list=customer_data.get('sales_price_list', '01'),
            sales_discount_cd=customer_data.get('sales_discount_cd', ''),
            sales_analysis_cd=customer_data.get('sales_analysis_cd', ''),
            sales_currency=customer_data.get('sales_currency', 'GBP'),
            sales_delivery_cd=customer_data.get('sales_delivery_cd', ''),
            
            # Statistics
            sales_ytd_sales=Decimal('0'),
            sales_ytd_cost=Decimal('0'),
            sales_last_yr_sales=Decimal('0'),
            sales_highest_bal=Decimal('0'),
            sales_invoice_cnt=0,
            sales_average_days=0
        )
        
        # Set statement preferences
        customer.sales_stmt_required = customer_data.get('sales_stmt_required', 'Y')
        customer.sales_stmt_type = customer_data.get('sales_stmt_type', 'OPEN')
        customer.sales_stmt_freq = customer_data.get('sales_stmt_freq', 'M')
        
        # Write customer
        _, status = self.customer_handler.process(5, record=customer)
        if status.fs_reply != "00":
            return None, f"Failed to create customer: {status.fs_reply}"
            
        # Create initial credit record if credit limit set
        if customer.sales_credit_lim > 0:
            self._create_credit_record(customer)
            
        # Log creation
        if self.current_user:
            log_user_action(
                db=self.db,
                user=self.current_user,
                action="CREATE_CUSTOMER",
                table="salesledger_rec",
                key=customer_no,
                new_values=customer_data,
                module="SL"
            )
            
        return customer, None
        
    def update_customer(self, customer_no: str, updates: Dict) -> Tuple[SalesLedgerRec, Optional[str]]:
        """
        Update existing customer
        Returns (customer_record, error_message)
        """
        # Read existing customer
        customer, status = self.customer_handler.process(4, key_value=customer_no)
        if status.fs_reply != "00":
            return None, "Customer not found"
            
        # Store old values for audit
        old_values = {
            'sales_name': customer.sales_name,
            'sales_credit_lim': float(customer.sales_credit_lim),
            'sales_on_stop': customer.sales_on_stop,
            'sales_active': customer.sales_active
        }
        
        # Validate updates
        if 'sales_cust' in updates and updates['sales_cust'] != customer_no:
            return None, "Cannot change customer number"
            
        # Special handling for credit limit changes
        old_credit_limit = customer.sales_credit_lim
        
        # Apply updates
        for field, value in updates.items():
            if hasattr(customer, field):
                if field in ['sales_balance', 'sales_credit_lim', 'sales_ytd_sales']:
                    value = Decimal(str(value))
                setattr(customer, field, value)
                
        # Update timestamps
        customer.sales_last_updated = int(datetime.now().strftime("%Y%m%d"))
        customer.sales_updated_by = self.current_user.username if self.current_user else 'SYSTEM'
        
        # Rewrite customer
        _, status = self.customer_handler.process(7, record=customer)
        if status.fs_reply != "00":
            return None, f"Failed to update customer: {status.fs_reply}"
            
        # Handle credit limit change
        if 'sales_credit_lim' in updates and customer.sales_credit_lim != old_credit_limit:
            self._update_credit_history(customer, old_credit_limit)
            
        # Check if put on stop
        if 'sales_on_stop' in updates and updates['sales_on_stop'] == 'Y':
            customer.sales_stop_date = int(datetime.now().strftime("%Y%m%d"))
            customer.sales_stop_reason = updates.get('stop_reason', 'MANUAL')
            
        # Log update
        if self.current_user:
            log_user_action(
                db=self.db,
                user=self.current_user,
                action="UPDATE_CUSTOMER",
                table="salesledger_rec",
                key=customer_no,
                old_values=old_values,
                new_values=updates,
                module="SL"
            )
            
        return customer, None
        
    def delete_customer(self, customer_no: str) -> Optional[str]:
        """
        Delete customer (mark as inactive)
        Returns error message or None on success
        """
        # Check if customer exists
        customer, status = self.customer_handler.process(4, key_value=customer_no)
        if status.fs_reply != "00":
            return "Customer not found"
            
        # Check balance
        if customer.sales_balance != 0:
            return "Cannot delete customer with outstanding balance"
            
        # Check for transactions in current period
        if self._has_current_period_transactions(customer_no):
            return "Cannot delete customer with current period transactions"
            
        # Check open orders
        if customer.sales_os_orders > 0:
            return "Cannot delete customer with open orders"
            
        # Mark as inactive instead of deleting
        customer.sales_active = 'N'
        customer.sales_date_closed = int(datetime.now().strftime("%Y%m%d"))
        _, status = self.customer_handler.process(7, record=customer)
        
        if status.fs_reply != "00":
            return f"Failed to delete customer: {status.fs_reply}"
            
        # Log deletion
        if self.current_user:
            log_user_action(
                db=self.db,
                user=self.current_user,
                action="DELETE_CUSTOMER",
                table="salesledger_rec",
                key=customer_no,
                module="SL"
            )
            
        return None
        
    def get_customer_list(self, filters: Optional[Dict] = None) -> List[Dict]:
        """Get list of customers with optional filters"""
        query = self.db.query(SalesLedgerRec)
        
        if filters:
            if filters.get('active_only'):
                query = query.filter(SalesLedgerRec.sales_active == 'Y')
                
            if filters.get('name_search'):
                search_term = f"%{filters['name_search']}%"
                query = query.filter(
                    or_(
                        SalesLedgerRec.sales_name.ilike(search_term),
                        SalesLedgerRec.sales_cust.ilike(search_term)
                    )
                )
                
            if filters.get('on_stop'):
                query = query.filter(SalesLedgerRec.sales_on_stop == 'Y')
                
            if filters.get('balance_from'):
                query = query.filter(
                    SalesLedgerRec.sales_balance >= Decimal(str(filters['balance_from']))
                )
                
            if filters.get('credit_exceeded'):
                query = query.filter(
                    SalesLedgerRec.sales_balance > SalesLedgerRec.sales_credit_lim
                )
                
        customers = query.order_by(SalesLedgerRec.sales_cust).all()
        
        return [
            {
                'customer_no': c.sales_cust,
                'name': c.sales_name,
                'balance': float(c.sales_balance),
                'credit_limit': float(c.sales_credit_lim),
                'on_stop': c.sales_on_stop,
                'active': c.sales_active,
                'payment_terms': c.sales_payment_cd,
                'phone': c.sales_telno,
                'email': c.sales_email,
                'last_invoice': c.sales_date_last_inv,
                'last_payment': c.sales_date_last_pay
            }
            for c in customers
        ]
        
    def add_customer_contact(self, customer_no: str, contact_data: Dict) -> Tuple[CustomerContactRec, Optional[str]]:
        """Add contact to customer"""
        # Verify customer exists
        customer, status = self.customer_handler.process(4, key_value=customer_no)
        if status.fs_reply != "00":
            return None, "Customer not found"
            
        # Create contact record
        contact = CustomerContactRec(
            contact_customer=customer_no,
            contact_name=contact_data.get('contact_name'),
            contact_title=contact_data.get('contact_title', ''),
            contact_position=contact_data.get('contact_position', ''),
            contact_phone=contact_data.get('contact_phone', ''),
            contact_mobile=contact_data.get('contact_mobile', ''),
            contact_email=contact_data.get('contact_email', ''),
            contact_type=contact_data.get('contact_type', 'GENERAL'),
            contact_primary=contact_data.get('contact_primary', 'N'),
            contact_active='Y',
            contact_created_date=int(datetime.now().strftime("%Y%m%d"))
        )
        
        self.db.add(contact)
        self.db.flush()
        
        return contact, None
        
    def get_customer_contacts(self, customer_no: str) -> List[CustomerContactRec]:
        """Get all contacts for a customer"""
        return self.db.query(CustomerContactRec).filter(
            and_(
                CustomerContactRec.contact_customer == customer_no,
                CustomerContactRec.contact_active == 'Y'
            )
        ).order_by(CustomerContactRec.contact_primary.desc()).all()
        
    def validate_customer_credit(self, customer_no: str, order_value: Decimal) -> Dict:
        """
        Validate customer credit for new order
        Returns validation result with details
        """
        customer, status = self.customer_handler.process(4, key_value=customer_no)
        if status.fs_reply != "00":
            return {
                'approved': False,
                'reason': 'Customer not found'
            }
            
        # Check if on stop
        if customer.sales_on_stop == 'Y':
            return {
                'approved': False,
                'reason': 'Customer on stop',
                'stop_date': customer.sales_stop_date,
                'stop_reason': customer.sales_stop_reason
            }
            
        # Check if active
        if customer.sales_active != 'Y':
            return {
                'approved': False,
                'reason': 'Customer inactive'
            }
            
        # Calculate available credit
        total_exposure = customer.sales_balance + customer.sales_os_orders + order_value
        available_credit = customer.sales_credit_lim - total_exposure
        
        # Check credit limit
        if customer.sales_credit_lim > 0 and total_exposure > customer.sales_credit_lim:
            return {
                'approved': False,
                'reason': 'Credit limit exceeded',
                'credit_limit': float(customer.sales_credit_lim),
                'current_balance': float(customer.sales_balance),
                'open_orders': float(customer.sales_os_orders),
                'order_value': float(order_value),
                'total_exposure': float(total_exposure),
                'available_credit': float(available_credit)
            }
            
        # Check overdue invoices
        overdue_count = self._get_overdue_invoice_count(customer_no)
        if overdue_count > 0:
            return {
                'approved': False,
                'reason': 'Overdue invoices exist',
                'overdue_count': overdue_count
            }
            
        return {
            'approved': True,
            'credit_limit': float(customer.sales_credit_lim),
            'current_balance': float(customer.sales_balance),
            'open_orders': float(customer.sales_os_orders),
            'available_credit': float(available_credit)
        }
        
    def merge_customers(self, from_customer: str, to_customer: str) -> Tuple[bool, Optional[str]]:
        """
        Merge one customer into another
        Returns (success, error_message)
        """
        # Validate both customers exist
        from_cust, status = self.customer_handler.process(4, key_value=from_customer)
        if status.fs_reply != "00":
            return False, "Source customer not found"
            
        to_cust, status = self.customer_handler.process(4, key_value=to_customer)
        if status.fs_reply != "00":
            return False, "Target customer not found"
            
        # Check if source has transactions
        if from_cust.sales_balance != 0:
            return False, "Cannot merge customer with outstanding balance"
            
        try:
            # Update all related records to point to target customer
            # This would update invoices, orders, etc.
            # For now, just mark source as merged
            from_cust.sales_active = 'N'
            from_cust.sales_merged_to = to_customer
            from_cust.sales_merge_date = int(datetime.now().strftime("%Y%m%d"))
            
            self.customer_handler.process(7, record=from_cust)
            
            # Log merge
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="MERGE_CUSTOMER",
                    module="SL",
                    new_values={
                        'from_customer': from_customer,
                        'to_customer': to_customer
                    }
                )
                
            return True, None
            
        except Exception as e:
            return False, str(e)
            
    def export_customer_data(self, customer_no: str, format: str = 'json') -> Dict:
        """Export all customer data"""
        customer, status = self.customer_handler.process(4, key_value=customer_no)
        if status.fs_reply != "00":
            return {"error": "Customer not found"}
            
        # Get contacts
        contacts = self.get_customer_contacts(customer_no)
        
        # Get credit history
        credit_history = self.db.query(CustomerCreditRec).filter(
            CustomerCreditRec.credit_customer == customer_no
        ).order_by(CustomerCreditRec.credit_date.desc()).all()
        
        data = {
            'customer': {
                'customer_no': customer.sales_cust,
                'name': customer.sales_name,
                'address': {
                    'line1': customer.sales_add1,
                    'line2': customer.sales_add2,
                    'line3': customer.sales_add3,
                    'line4': customer.sales_add4,
                    'line5': customer.sales_add5,
                    'postcode': customer.sales_postcode,
                    'country': customer.sales_country
                },
                'contact': {
                    'phone': customer.sales_telno,
                    'fax': customer.sales_faxno,
                    'email': customer.sales_email
                },
                'financial': {
                    'balance': float(customer.sales_balance),
                    'credit_limit': float(customer.sales_credit_lim),
                    'payment_terms': customer.sales_payment_cd,
                    'vat_no': customer.sales_vat_no,
                    'currency': customer.sales_currency
                },
                'status': {
                    'active': customer.sales_active,
                    'on_stop': customer.sales_on_stop,
                    'account_type': customer.sales_account_type
                },
                'statistics': {
                    'ytd_sales': float(customer.sales_ytd_sales),
                    'last_year_sales': float(customer.sales_last_yr_sales),
                    'highest_balance': float(customer.sales_highest_bal),
                    'average_days': customer.sales_average_days
                }
            },
            'contacts': [
                {
                    'name': c.contact_name,
                    'title': c.contact_title,
                    'position': c.contact_position,
                    'phone': c.contact_phone,
                    'email': c.contact_email,
                    'type': c.contact_type,
                    'primary': c.contact_primary == 'Y'
                }
                for c in contacts
            ],
            'credit_history': [
                {
                    'date': c.credit_date,
                    'old_limit': float(c.credit_old_limit),
                    'new_limit': float(c.credit_new_limit),
                    'reason': c.credit_reason,
                    'approved_by': c.credit_approved_by
                }
                for c in credit_history
            ]
        }
        
        return data
        
    def _validate_customer_number(self, customer_no: str) -> bool:
        """Validate customer number format"""
        if not customer_no:
            return False
            
        # Check length and format based on system settings
        # Standard format is alphanumeric up to 10 characters
        if len(customer_no) > 10:
            return False
            
        # Must start with letter or number
        if not customer_no[0].isalnum():
            return False
            
        return True
        
    def _create_credit_record(self, customer: SalesLedgerRec):
        """Create initial credit history record"""
        credit_rec = CustomerCreditRec(
            credit_customer=customer.sales_cust,
            credit_date=int(datetime.now().strftime("%Y%m%d")),
            credit_old_limit=Decimal('0'),
            credit_new_limit=customer.sales_credit_lim,
            credit_reason='Initial setup',
            credit_approved_by=self.current_user.username if self.current_user else 'SYSTEM'
        )
        
        self.db.add(credit_rec)
        self.db.flush()
        
    def _update_credit_history(self, customer: SalesLedgerRec, old_limit: Decimal):
        """Update credit history when limit changes"""
        credit_rec = CustomerCreditRec(
            credit_customer=customer.sales_cust,
            credit_date=int(datetime.now().strftime("%Y%m%d")),
            credit_old_limit=old_limit,
            credit_new_limit=customer.sales_credit_lim,
            credit_reason='Credit limit update',
            credit_approved_by=self.current_user.username if self.current_user else 'SYSTEM'
        )
        
        self.db.add(credit_rec)
        self.db.flush()
        
    def _has_current_period_transactions(self, customer_no: str) -> bool:
        """Check if customer has transactions in current period"""
        from app.models.sales import SalesOpenItemRec
        
        system_rec, _ = self.system_handler.read_system_params()
        current_period = system_rec.sl_period if system_rec else 0
        
        count = self.db.query(SalesOpenItemRec).filter(
            and_(
                SalesOpenItemRec.sales_oi_cust == customer_no,
                SalesOpenItemRec.sales_oi_period == current_period
            )
        ).count()
        
        return count > 0
        
    def _get_overdue_invoice_count(self, customer_no: str) -> int:
        """Get count of overdue invoices"""
        from app.models.sales import SalesOpenItemRec
        
        today = int(datetime.now().strftime("%Y%m%d"))
        
        return self.db.query(SalesOpenItemRec).filter(
            and_(
                SalesOpenItemRec.sales_oi_cust == customer_no,
                SalesOpenItemRec.sales_oi_type == 'INV',
                SalesOpenItemRec.sales_oi_due_date < today,
                SalesOpenItemRec.sales_oi_amount != 0
            )
        ).count()