"""
Supplier Master Service - PL010 migration
Handles supplier creation, maintenance, and validation
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from datetime import datetime
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func

from app.services.file_handlers.supplier_handler import SupplierFileHandler
from app.services.file_handlers.system_handler import SystemFileHandler
from app.models.supplier import PurchaseLedgerRec, SupplierContactRec, SupplierBankRec
from app.core.security import log_user_action
from app.models.auth import User


class SupplierMasterService:
    """
    Supplier Master maintenance
    Implements PL010 functionality
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.supplier_handler = SupplierFileHandler(db)
        self.system_handler = SystemFileHandler(db)
        
    def create_supplier(self, supplier_data: Dict) -> Tuple[PurchaseLedgerRec, Optional[str]]:
        """
        Create new supplier
        Returns (supplier_record, error_message)
        """
        # Validate supplier number
        supplier_no = supplier_data.get('purch_supp', '')
        if not self._validate_supplier_number(supplier_no):
            return None, "Invalid supplier number format"
            
        # Check if already exists
        existing, status = self.supplier_handler.process(4, key_value=supplier_no)
        if status.fs_reply == "00":
            return None, "Supplier already exists"
            
        # Get system defaults
        system_rec, _ = self.system_handler.read_system_params()
        
        # Create supplier record
        supplier = PurchaseLedgerRec(
            purch_supp=supplier_no,
            purch_name=supplier_data.get('purch_name', ''),
            purch_add1=supplier_data.get('purch_add1', ''),
            purch_add2=supplier_data.get('purch_add2', ''),
            purch_add3=supplier_data.get('purch_add3', ''),
            purch_add4=supplier_data.get('purch_add4', ''),
            purch_add5=supplier_data.get('purch_add5', ''),
            purch_postcode=supplier_data.get('purch_postcode', ''),
            purch_country=supplier_data.get('purch_country', 'GB'),
            purch_telno=supplier_data.get('purch_telno', ''),
            purch_faxno=supplier_data.get('purch_faxno', ''),
            purch_email=supplier_data.get('purch_email', ''),
            purch_contact=supplier_data.get('purch_contact', ''),
            
            # Financial info
            purch_balance=Decimal('0'),
            purch_os_orders=Decimal('0'),
            purch_credit_lim=Decimal(str(supplier_data.get('purch_credit_lim', 0))),
            purch_payment_cd=supplier_data.get('purch_payment_cd', 'C30'),
            purch_vat_no=supplier_data.get('purch_vat_no', ''),
            purch_vat_rate=supplier_data.get('purch_vat_rate', system_rec.p_vat_rate if system_rec else ''),
            
            # Status fields
            purch_active='Y',
            purch_on_stop='N',
            purch_stop_date=0,
            purch_account_type=supplier_data.get('purch_account_type', 'TRADE'),
            
            # Dates
            purch_date_opened=int(datetime.now().strftime("%Y%m%d")),
            purch_date_last_inv=0,
            purch_date_last_pay=0,
            
            # Defaults
            purch_price_list=supplier_data.get('purch_price_list', '01'),
            purch_discount_cd=supplier_data.get('purch_discount_cd', ''),
            purch_analysis_cd=supplier_data.get('purch_analysis_cd', ''),
            purch_currency=supplier_data.get('purch_currency', 'GBP'),
            purch_lead_time=supplier_data.get('purch_lead_time', 0),
            
            # Bank details
            purch_bank_name=supplier_data.get('purch_bank_name', ''),
            purch_bank_account=supplier_data.get('purch_bank_account', ''),
            purch_bank_sort_code=supplier_data.get('purch_bank_sort_code', ''),
            
            # Statistics
            purch_ytd_purch=Decimal('0'),
            purch_ytd_returns=Decimal('0'),
            purch_last_yr_purch=Decimal('0'),
            purch_highest_bal=Decimal('0'),
            purch_invoice_cnt=0,
            purch_average_days=0,
            
            # Payment preferences
            purch_payment_method=supplier_data.get('purch_payment_method', 'CHQ'),
            purch_remit_advice='Y',
            purch_self_billing='N'
        )
        
        # Write supplier
        _, status = self.supplier_handler.process(5, record=supplier)
        if status.fs_reply != "00":
            return None, f"Failed to create supplier: {status.fs_reply}"
            
        # Create bank record if bank details provided
        if supplier.purch_bank_account:
            self._create_bank_record(supplier)
            
        # Log creation
        if self.current_user:
            log_user_action(
                db=self.db,
                user=self.current_user,
                action="CREATE_SUPPLIER",
                table="purchaseledger_rec",
                key=supplier_no,
                new_values=supplier_data,
                module="PL"
            )
            
        return supplier, None
        
    def update_supplier(self, supplier_no: str, updates: Dict) -> Tuple[PurchaseLedgerRec, Optional[str]]:
        """
        Update existing supplier
        Returns (supplier_record, error_message)
        """
        # Read existing supplier
        supplier, status = self.supplier_handler.process(4, key_value=supplier_no)
        if status.fs_reply != "00":
            return None, "Supplier not found"
            
        # Store old values for audit
        old_values = {
            'purch_name': supplier.purch_name,
            'purch_credit_lim': float(supplier.purch_credit_lim),
            'purch_on_stop': supplier.purch_on_stop,
            'purch_active': supplier.purch_active
        }
        
        # Validate updates
        if 'purch_supp' in updates and updates['purch_supp'] != supplier_no:
            return None, "Cannot change supplier number"
            
        # Apply updates
        for field, value in updates.items():
            if hasattr(supplier, field):
                if field in ['purch_balance', 'purch_credit_lim', 'purch_ytd_purch']:
                    value = Decimal(str(value))
                setattr(supplier, field, value)
                
        # Update timestamps
        supplier.purch_last_updated = int(datetime.now().strftime("%Y%m%d"))
        supplier.purch_updated_by = self.current_user.username if self.current_user else 'SYSTEM'
        
        # Rewrite supplier
        _, status = self.supplier_handler.process(7, record=supplier)
        if status.fs_reply != "00":
            return None, f"Failed to update supplier: {status.fs_reply}"
            
        # Check if put on stop
        if 'purch_on_stop' in updates and updates['purch_on_stop'] == 'Y':
            supplier.purch_stop_date = int(datetime.now().strftime("%Y%m%d"))
            supplier.purch_stop_reason = updates.get('stop_reason', 'MANUAL')
            
        # Log update
        if self.current_user:
            log_user_action(
                db=self.db,
                user=self.current_user,
                action="UPDATE_SUPPLIER",
                table="purchaseledger_rec",
                key=supplier_no,
                old_values=old_values,
                new_values=updates,
                module="PL"
            )
            
        return supplier, None
        
    def delete_supplier(self, supplier_no: str) -> Optional[str]:
        """
        Delete supplier (mark as inactive)
        Returns error message or None on success
        """
        # Check if supplier exists
        supplier, status = self.supplier_handler.process(4, key_value=supplier_no)
        if status.fs_reply != "00":
            return "Supplier not found"
            
        # Check balance
        if supplier.purch_balance != 0:
            return "Cannot delete supplier with outstanding balance"
            
        # Check for transactions in current period
        if self._has_current_period_transactions(supplier_no):
            return "Cannot delete supplier with current period transactions"
            
        # Check open orders
        if supplier.purch_os_orders > 0:
            return "Cannot delete supplier with open orders"
            
        # Mark as inactive instead of deleting
        supplier.purch_active = 'N'
        supplier.purch_date_closed = int(datetime.now().strftime("%Y%m%d"))
        _, status = self.supplier_handler.process(7, record=supplier)
        
        if status.fs_reply != "00":
            return f"Failed to delete supplier: {status.fs_reply}"
            
        # Log deletion
        if self.current_user:
            log_user_action(
                db=self.db,
                user=self.current_user,
                action="DELETE_SUPPLIER",
                table="purchaseledger_rec",
                key=supplier_no,
                module="PL"
            )
            
        return None
        
    def get_supplier_list(self, filters: Optional[Dict] = None) -> List[Dict]:
        """Get list of suppliers with optional filters"""
        query = self.db.query(PurchaseLedgerRec)
        
        if filters:
            if filters.get('active_only'):
                query = query.filter(PurchaseLedgerRec.purch_active == 'Y')
                
            if filters.get('name_search'):
                search_term = f"%{filters['name_search']}%"
                query = query.filter(
                    or_(
                        PurchaseLedgerRec.purch_name.ilike(search_term),
                        PurchaseLedgerRec.purch_supp.ilike(search_term)
                    )
                )
                
            if filters.get('on_stop'):
                query = query.filter(PurchaseLedgerRec.purch_on_stop == 'Y')
                
            if filters.get('balance_from'):
                query = query.filter(
                    PurchaseLedgerRec.purch_balance >= Decimal(str(filters['balance_from']))
                )
                
            if filters.get('payment_method'):
                query = query.filter(
                    PurchaseLedgerRec.purch_payment_method == filters['payment_method']
                )
                
        suppliers = query.order_by(PurchaseLedgerRec.purch_supp).all()
        
        return [
            {
                'supplier_no': s.purch_supp,
                'name': s.purch_name,
                'balance': float(s.purch_balance),
                'credit_limit': float(s.purch_credit_lim),
                'on_stop': s.purch_on_stop,
                'active': s.purch_active,
                'payment_terms': s.purch_payment_cd,
                'payment_method': s.purch_payment_method,
                'phone': s.purch_telno,
                'email': s.purch_email,
                'last_invoice': s.purch_date_last_inv,
                'last_payment': s.purch_date_last_pay
            }
            for s in suppliers
        ]
        
    def add_supplier_contact(self, supplier_no: str, contact_data: Dict) -> Tuple[SupplierContactRec, Optional[str]]:
        """Add contact to supplier"""
        # Verify supplier exists
        supplier, status = self.supplier_handler.process(4, key_value=supplier_no)
        if status.fs_reply != "00":
            return None, "Supplier not found"
            
        # Create contact record
        contact = SupplierContactRec(
            contact_supplier=supplier_no,
            contact_name=contact_data.get('contact_name'),
            contact_title=contact_data.get('contact_title', ''),
            contact_position=contact_data.get('contact_position', ''),
            contact_department=contact_data.get('contact_department', ''),
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
        
    def add_supplier_bank(self, supplier_no: str, bank_data: Dict) -> Tuple[SupplierBankRec, Optional[str]]:
        """Add bank account to supplier"""
        # Verify supplier exists
        supplier, status = self.supplier_handler.process(4, key_value=supplier_no)
        if status.fs_reply != "00":
            return None, "Supplier not found"
            
        # Create bank record
        bank = SupplierBankRec(
            bank_supplier=supplier_no,
            bank_name=bank_data.get('bank_name'),
            bank_branch=bank_data.get('bank_branch', ''),
            bank_account_no=bank_data.get('bank_account_no'),
            bank_sort_code=bank_data.get('bank_sort_code', ''),
            bank_iban=bank_data.get('bank_iban', ''),
            bank_swift=bank_data.get('bank_swift', ''),
            bank_currency=bank_data.get('bank_currency', 'GBP'),
            bank_primary=bank_data.get('bank_primary', 'Y'),
            bank_active='Y',
            bank_verified='N',
            bank_created_date=int(datetime.now().strftime("%Y%m%d"))
        )
        
        # If this is primary, unset other primary banks
        if bank.bank_primary == 'Y':
            self.db.query(SupplierBankRec).filter(
                SupplierBankRec.bank_supplier == supplier_no
            ).update({'bank_primary': 'N'})
            
        self.db.add(bank)
        self.db.flush()
        
        return bank, None
        
    def verify_supplier_bank(self, supplier_no: str, bank_id: int) -> Tuple[bool, Optional[str]]:
        """Verify supplier bank details"""
        bank = self.db.query(SupplierBankRec).filter(
            and_(
                SupplierBankRec.bank_id == bank_id,
                SupplierBankRec.bank_supplier == supplier_no
            )
        ).first()
        
        if not bank:
            return False, "Bank record not found"
            
        # Here would integrate with bank verification service
        # For now, just mark as verified
        bank.bank_verified = 'Y'
        bank.bank_verified_date = int(datetime.now().strftime("%Y%m%d"))
        bank.bank_verified_by = self.current_user.username if self.current_user else 'SYSTEM'
        
        self.db.flush()
        
        # Log verification
        if self.current_user:
            log_user_action(
                db=self.db,
                user=self.current_user,
                action="VERIFY_SUPPLIER_BANK",
                module="PL",
                new_values={
                    'supplier': supplier_no,
                    'bank_id': bank_id,
                    'bank_name': bank.bank_name
                }
            )
            
        return True, None
        
    def get_supplier_contacts(self, supplier_no: str) -> List[SupplierContactRec]:
        """Get all contacts for a supplier"""
        return self.db.query(SupplierContactRec).filter(
            and_(
                SupplierContactRec.contact_supplier == supplier_no,
                SupplierContactRec.contact_active == 'Y'
            )
        ).order_by(SupplierContactRec.contact_primary.desc()).all()
        
    def get_supplier_banks(self, supplier_no: str) -> List[SupplierBankRec]:
        """Get all bank accounts for a supplier"""
        return self.db.query(SupplierBankRec).filter(
            and_(
                SupplierBankRec.bank_supplier == supplier_no,
                SupplierBankRec.bank_active == 'Y'
            )
        ).order_by(SupplierBankRec.bank_primary.desc()).all()
        
    def check_supplier_compliance(self, supplier_no: str) -> Dict:
        """
        Check supplier compliance status
        Returns compliance details
        """
        supplier, status = self.supplier_handler.process(4, key_value=supplier_no)
        if status.fs_reply != "00":
            return {"error": "Supplier not found"}
            
        compliance = {
            'supplier_no': supplier_no,
            'compliant': True,
            'issues': []
        }
        
        # Check VAT registration
        if supplier.purch_vat_no == '' and supplier.purch_account_type == 'TRADE':
            compliance['issues'].append("Missing VAT registration number")
            compliance['compliant'] = False
            
        # Check bank details verified
        primary_bank = self.db.query(SupplierBankRec).filter(
            and_(
                SupplierBankRec.bank_supplier == supplier_no,
                SupplierBankRec.bank_primary == 'Y',
                SupplierBankRec.bank_active == 'Y'
            )
        ).first()
        
        if not primary_bank:
            compliance['issues'].append("No primary bank account on file")
            compliance['compliant'] = False
        elif primary_bank.bank_verified != 'Y':
            compliance['issues'].append("Bank details not verified")
            compliance['compliant'] = False
            
        # Check insurance/certificates if required
        if supplier.purch_insurance_expiry > 0:
            if supplier.purch_insurance_expiry < int(datetime.now().strftime("%Y%m%d")):
                compliance['issues'].append("Insurance certificate expired")
                compliance['compliant'] = False
                
        # Check payment performance
        if supplier.purch_average_days > 60:
            compliance['issues'].append(f"Poor payment performance: {supplier.purch_average_days} days average")
            
        return compliance
        
    def merge_suppliers(self, from_supplier: str, to_supplier: str) -> Tuple[bool, Optional[str]]:
        """
        Merge one supplier into another
        Returns (success, error_message)
        """
        # Validate both suppliers exist
        from_supp, status = self.supplier_handler.process(4, key_value=from_supplier)
        if status.fs_reply != "00":
            return False, "Source supplier not found"
            
        to_supp, status = self.supplier_handler.process(4, key_value=to_supplier)
        if status.fs_reply != "00":
            return False, "Target supplier not found"
            
        # Check if source has transactions
        if from_supp.purch_balance != 0:
            return False, "Cannot merge supplier with outstanding balance"
            
        try:
            # Update all related records to point to target supplier
            # Merge contacts
            self.db.query(SupplierContactRec).filter(
                SupplierContactRec.contact_supplier == from_supplier
            ).update({'contact_supplier': to_supplier})
            
            # Merge bank accounts
            self.db.query(SupplierBankRec).filter(
                SupplierBankRec.bank_supplier == from_supplier
            ).update({'bank_supplier': to_supplier})
            
            # Mark source as merged
            from_supp.purch_active = 'N'
            from_supp.purch_merged_to = to_supplier
            from_supp.purch_merge_date = int(datetime.now().strftime("%Y%m%d"))
            
            self.supplier_handler.process(7, record=from_supp)
            
            # Log merge
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="MERGE_SUPPLIER",
                    module="PL",
                    new_values={
                        'from_supplier': from_supplier,
                        'to_supplier': to_supplier
                    }
                )
                
            self.db.commit()
            return True, None
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def calculate_supplier_rating(self, supplier_no: str) -> Dict:
        """Calculate supplier performance rating"""
        supplier, status = self.supplier_handler.process(4, key_value=supplier_no)
        if status.fs_reply != "00":
            return {"error": "Supplier not found"}
            
        # Rating components (0-100 scale)
        rating = {
            'overall': 0,
            'payment_performance': 0,
            'quality': 0,
            'delivery': 0,
            'compliance': 0,
            'volume': 0
        }
        
        # Payment performance (based on average days to pay)
        if supplier.purch_average_days == 0:
            rating['payment_performance'] = 100
        elif supplier.purch_average_days <= 30:
            rating['payment_performance'] = 90
        elif supplier.purch_average_days <= 45:
            rating['payment_performance'] = 70
        elif supplier.purch_average_days <= 60:
            rating['payment_performance'] = 50
        else:
            rating['payment_performance'] = 30
            
        # Quality (simplified - would use quality incident tracking)
        quality_score = 85  # Default
        if supplier.purch_ytd_returns > 0 and supplier.purch_ytd_purch > 0:
            return_rate = float(supplier.purch_ytd_returns / supplier.purch_ytd_purch)
            if return_rate > 0.05:  # More than 5% returns
                quality_score = 60
            elif return_rate > 0.02:  # More than 2% returns
                quality_score = 75
                
        rating['quality'] = quality_score
        
        # Delivery performance (simplified)
        rating['delivery'] = 80  # Would track on-time delivery
        
        # Compliance
        compliance = self.check_supplier_compliance(supplier_no)
        rating['compliance'] = 100 if compliance['compliant'] else 50
        
        # Volume (based on YTD purchases)
        if supplier.purch_ytd_purch > 100000:
            rating['volume'] = 100
        elif supplier.purch_ytd_purch > 50000:
            rating['volume'] = 80
        elif supplier.purch_ytd_purch > 10000:
            rating['volume'] = 60
        else:
            rating['volume'] = 40
            
        # Calculate overall rating (weighted average)
        weights = {
            'payment_performance': 0.25,
            'quality': 0.30,
            'delivery': 0.20,
            'compliance': 0.15,
            'volume': 0.10
        }
        
        rating['overall'] = int(sum(
            rating[component] * weight 
            for component, weight in weights.items()
        ))
        
        # Determine rating category
        if rating['overall'] >= 85:
            category = 'PREFERRED'
        elif rating['overall'] >= 70:
            category = 'APPROVED'
        elif rating['overall'] >= 50:
            category = 'CONDITIONAL'
        else:
            category = 'RESTRICTED'
            
        return {
            'supplier_no': supplier_no,
            'supplier_name': supplier.purch_name,
            'rating': rating,
            'category': category,
            'evaluated_date': datetime.now().strftime("%Y-%m-%d")
        }
        
    def export_supplier_data(self, supplier_no: str, format: str = 'json') -> Dict:
        """Export all supplier data"""
        supplier, status = self.supplier_handler.process(4, key_value=supplier_no)
        if status.fs_reply != "00":
            return {"error": "Supplier not found"}
            
        # Get related data
        contacts = self.get_supplier_contacts(supplier_no)
        banks = self.get_supplier_banks(supplier_no)
        rating = self.calculate_supplier_rating(supplier_no)
        
        data = {
            'supplier': {
                'supplier_no': supplier.purch_supp,
                'name': supplier.purch_name,
                'address': {
                    'line1': supplier.purch_add1,
                    'line2': supplier.purch_add2,
                    'line3': supplier.purch_add3,
                    'line4': supplier.purch_add4,
                    'line5': supplier.purch_add5,
                    'postcode': supplier.purch_postcode,
                    'country': supplier.purch_country
                },
                'contact': {
                    'phone': supplier.purch_telno,
                    'fax': supplier.purch_faxno,
                    'email': supplier.purch_email
                },
                'financial': {
                    'balance': float(supplier.purch_balance),
                    'credit_limit': float(supplier.purch_credit_lim),
                    'payment_terms': supplier.purch_payment_cd,
                    'payment_method': supplier.purch_payment_method,
                    'vat_no': supplier.purch_vat_no,
                    'currency': supplier.purch_currency
                },
                'status': {
                    'active': supplier.purch_active,
                    'on_stop': supplier.purch_on_stop,
                    'account_type': supplier.purch_account_type
                },
                'statistics': {
                    'ytd_purchases': float(supplier.purch_ytd_purch),
                    'ytd_returns': float(supplier.purch_ytd_returns),
                    'last_year_purchases': float(supplier.purch_last_yr_purch),
                    'highest_balance': float(supplier.purch_highest_bal),
                    'average_days': supplier.purch_average_days
                }
            },
            'contacts': [
                {
                    'name': c.contact_name,
                    'title': c.contact_title,
                    'position': c.contact_position,
                    'department': c.contact_department,
                    'phone': c.contact_phone,
                    'email': c.contact_email,
                    'type': c.contact_type,
                    'primary': c.contact_primary == 'Y'
                }
                for c in contacts
            ],
            'bank_accounts': [
                {
                    'bank_name': b.bank_name,
                    'branch': b.bank_branch,
                    'account_no': b.bank_account_no,
                    'sort_code': b.bank_sort_code,
                    'iban': b.bank_iban,
                    'swift': b.bank_swift,
                    'currency': b.bank_currency,
                    'primary': b.bank_primary == 'Y',
                    'verified': b.bank_verified == 'Y'
                }
                for b in banks
            ],
            'rating': rating
        }
        
        return data
        
    def _validate_supplier_number(self, supplier_no: str) -> bool:
        """Validate supplier number format"""
        if not supplier_no:
            return False
            
        # Check length and format based on system settings
        # Standard format is alphanumeric up to 10 characters
        if len(supplier_no) > 10:
            return False
            
        # Must start with letter or number
        if not supplier_no[0].isalnum():
            return False
            
        return True
        
    def _create_bank_record(self, supplier: PurchaseLedgerRec):
        """Create initial bank record from supplier master"""
        if supplier.purch_bank_account:
            bank = SupplierBankRec(
                bank_supplier=supplier.purch_supp,
                bank_name=supplier.purch_bank_name,
                bank_account_no=supplier.purch_bank_account,
                bank_sort_code=supplier.purch_bank_sort_code,
                bank_currency=supplier.purch_currency,
                bank_primary='Y',
                bank_active='Y',
                bank_verified='N',
                bank_created_date=int(datetime.now().strftime("%Y%m%d"))
            )
            
            self.db.add(bank)
            self.db.flush()
            
    def _has_current_period_transactions(self, supplier_no: str) -> bool:
        """Check if supplier has transactions in current period"""
        from app.models.purchase import PurchaseOpenItemRec
        
        system_rec, _ = self.system_handler.read_system_params()
        current_period = system_rec.pl_period if system_rec else 0
        
        count = self.db.query(PurchaseOpenItemRec).filter(
            and_(
                PurchaseOpenItemRec.purch_oi_supp == supplier_no,
                PurchaseOpenItemRec.purch_oi_period == current_period
            )
        ).count()
        
        return count > 0