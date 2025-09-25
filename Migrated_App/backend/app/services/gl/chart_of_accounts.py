"""
Chart of Accounts Service - GL010/GL020 migration
Handles GL account setup, maintenance and validation
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func

from app.services.file_handlers.gl_handler import GLFileHandler
from app.services.file_handlers.system_handler import SystemFileHandler
from app.models.gl_accounts import GLLedgerRec
from app.core.security import log_user_action
from app.models.auth import User


class ChartOfAccountsService:
    """
    Chart of Accounts management
    Implements GL010/GL020 functionality
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.gl_handler = GLFileHandler(db)
        self.system_handler = SystemFileHandler(db)
        
    def create_account(self, account_data: Dict) -> Tuple[GLLedgerRec, str]:
        """
        Create new GL account
        Returns (account, error_message)
        """
        # Validate account number format
        account_no = account_data.get('ledger_key', 0)
        if not self._validate_account_number_format(account_no):
            return None, "Invalid account number format"
            
        # Check if already exists
        existing, status = self.gl_handler.process(4, key_value=account_no)  # READ_INDEXED
        if status.fs_reply == "00":
            return None, "Account already exists"
            
        # Validate account type
        account_type = account_data.get('ledger_type', 0)
        if account_type not in [1, 2, 3, 4, 5]:
            return None, "Invalid account type (1-5)"
            
        # Validate parent if hierarchical
        parent = account_data.get('ledger_parent', 0)
        if parent > 0:
            parent_acct, status = self.gl_handler.process(4, key_value=parent)
            if status.fs_reply != "00":
                return None, f"Parent account {parent} does not exist"
                
        # Create account
        account = GLLedgerRec(
            ledger_key=account_no,
            ledger_name=account_data.get('ledger_name', ''),
            ledger_type=account_type,
            ledger_place=self._determine_account_place(account_type),
            ledger_level=self._determine_account_level(account_no),
            ledger_parent=parent,
            ledger_active='Y',
            ledger_control=account_data.get('ledger_control', 'N'),
            ledger_reconcilable=account_data.get('ledger_reconcilable', 'N'),
            ledger_analysis_required=account_data.get('ledger_analysis_required', 'N'),
            ledger_vat_code=account_data.get('ledger_vat_code', ''),
            ledger_currency=account_data.get('ledger_currency', 'GBP')
        )
        
        # Write account
        _, status = self.gl_handler.process(5, record=account)  # WRITE
        if status.fs_reply != "00":
            return None, f"Failed to create account: {status.fs_reply}"
            
        # Log creation
        if self.current_user:
            log_user_action(
                db=self.db,
                user=self.current_user,
                action="CREATE_GL_ACCOUNT",
                table="glledger_rec",
                key=str(account_no),
                new_values=account_data,
                module="GL"
            )
            
        return account, None
        
    def update_account(self, account_no: int, updates: Dict) -> Tuple[GLLedgerRec, str]:
        """
        Update existing GL account
        Returns (account, error_message)
        """
        # Read existing account
        account, status = self.gl_handler.process(4, key_value=account_no)
        if status.fs_reply != "00":
            return None, "Account not found"
            
        # Store old values for audit
        old_values = {
            'ledger_name': account.ledger_name,
            'ledger_active': account.ledger_active,
            'ledger_control': account.ledger_control
        }
        
        # Validate updates
        if 'ledger_type' in updates:
            return None, "Cannot change account type"
            
        if 'ledger_key' in updates and updates['ledger_key'] != account_no:
            return None, "Cannot change account number"
            
        # Apply updates
        for field, value in updates.items():
            if hasattr(account, field):
                setattr(account, field, value)
                
        # Rewrite account
        _, status = self.gl_handler.process(7, record=account)  # REWRITE
        if status.fs_reply != "00":
            return None, f"Failed to update account: {status.fs_reply}"
            
        # Log update
        if self.current_user:
            log_user_action(
                db=self.db,
                user=self.current_user,
                action="UPDATE_GL_ACCOUNT",
                table="glledger_rec",
                key=str(account_no),
                old_values=old_values,
                new_values=updates,
                module="GL"
            )
            
        return account, None
        
    def delete_account(self, account_no: int) -> Optional[str]:
        """
        Delete GL account (mark as inactive)
        Returns error message or None on success
        """
        # Check if account exists
        account, status = self.gl_handler.process(4, key_value=account_no)
        if status.fs_reply != "00":
            return "Account not found"
            
        # Check balance
        if account.ledger_balance != 0:
            return "Cannot delete account with non-zero balance"
            
        # Check for child accounts
        children = self.gl_handler.get_account_hierarchy(account_no)
        if children:
            return "Cannot delete account with sub-accounts"
            
        # Check for transactions in current period
        if self._has_current_period_transactions(account_no):
            return "Cannot delete account with current period transactions"
            
        # Mark as inactive instead of deleting
        account.ledger_active = 'N'
        _, status = self.gl_handler.process(7, record=account)
        
        if status.fs_reply != "00":
            return f"Failed to delete account: {status.fs_reply}"
            
        # Log deletion
        if self.current_user:
            log_user_action(
                db=self.db,
                user=self.current_user,
                action="DELETE_GL_ACCOUNT",
                table="glledger_rec",
                key=str(account_no),
                module="GL"
            )
            
        return None
        
    def get_account_list(self, filters: Optional[Dict] = None) -> List[GLLedgerRec]:
        """Get list of accounts with optional filters"""
        query = self.db.query(GLLedgerRec)
        
        if filters:
            if 'active_only' in filters and filters['active_only']:
                query = query.filter(GLLedgerRec.ledger_active == 'Y')
                
            if 'account_type' in filters:
                query = query.filter(GLLedgerRec.ledger_type == filters['account_type'])
                
            if 'control_only' in filters and filters['control_only']:
                query = query.filter(GLLedgerRec.ledger_control == 'Y')
                
            if 'parent' in filters:
                query = query.filter(GLLedgerRec.ledger_parent == filters['parent'])
                
        return query.order_by(GLLedgerRec.ledger_key).all()
        
    def validate_account_structure(self) -> List[str]:
        """
        Validate chart of accounts structure
        Returns list of validation errors
        """
        errors = []
        
        # Check for required control accounts
        system_rec, _ = self.system_handler.read_system_params()
        if system_rec:
            # Debtors control
            if system_rec.sl_exists == 'Y':
                debtors_ac = system_rec.s_debtors
                if debtors_ac > 0:
                    account, status = self.gl_handler.process(4, key_value=debtors_ac)
                    if status.fs_reply != "00":
                        errors.append(f"Sales Debtors control account {debtors_ac} not found")
                    elif account.ledger_control != 'Y':
                        errors.append(f"Account {debtors_ac} must be marked as control account")
                        
            # Creditors control  
            if system_rec.pl_exists == 'Y':
                creditors_ac = system_rec.p_creditors
                if creditors_ac > 0:
                    account, status = self.gl_handler.process(4, key_value=creditors_ac)
                    if status.fs_reply != "00":
                        errors.append(f"Purchase Creditors control account {creditors_ac} not found")
                    elif account.ledger_control != 'Y':
                        errors.append(f"Account {creditors_ac} must be marked as control account")
                        
        # Check hierarchical consistency
        accounts = self.get_account_list()
        for account in accounts:
            if account.ledger_parent > 0:
                parent = next((a for a in accounts if a.ledger_key == account.ledger_parent), None)
                if not parent:
                    errors.append(f"Account {account.ledger_key} has invalid parent {account.ledger_parent}")
                elif parent.ledger_level >= account.ledger_level:
                    errors.append(f"Account {account.ledger_key} level must be greater than parent level")
                    
        return errors
        
    def _validate_account_number_format(self, account_no: int) -> bool:
        """Validate account number format based on system settings"""
        # Basic validation - must be positive
        if account_no <= 0:
            return False
            
        # Check length based on system configuration
        # Standard ACAS uses 8 digit accounts
        if account_no > 99999999:
            return False
            
        return True
        
    def _determine_account_place(self, account_type: int) -> str:
        """
        Determine if account belongs to Balance Sheet or P&L
        1,2,3 = Balance Sheet
        4,5 = P&L
        """
        if account_type in [1, 2, 3]:  # Assets, Liabilities, Capital
            return 'B'
        else:  # Income, Expenses
            return 'P'
            
    def _determine_account_level(self, account_no: int) -> int:
        """
        Determine account hierarchy level based on number
        This is a simplified version - real implementation would
        use configured ranges
        """
        if account_no < 1000000:
            return 1  # Top level
        elif account_no < 10000000:
            return 2  # Sub-account
        else:
            return 3  # Detail account
            
    def _has_current_period_transactions(self, account_no: int) -> bool:
        """Check if account has transactions in current period"""
        from app.models.gl_accounts import GLPostingRec
        
        current_period = self.system_handler.get_current_period()
        
        count = self.db.query(GLPostingRec).filter(
            and_(
                GLPostingRec.posting_account == account_no,
                GLPostingRec.posting_period == current_period
            )
        ).count()
        
        return count > 0
        
    def get_account_defaults(self) -> Dict[str, int]:
        """
        Get default GL accounts from system settings
        Used for automatic posting
        """
        system_rec, _ = self.system_handler.read_system_params()
        if not system_rec:
            return {}
            
        defaults = {
            'sales_control': system_rec.s_debtors,
            'purchase_control': system_rec.p_creditors,
            'sales_account': system_rec.sl_sales_ac,
            'purchase_account': system_rec.bl_purch_ac,
            'bank_account': system_rec.sl_pay_ac,
            'vat_account': system_rec.gl_vat_ac,
            'retained_earnings': 30020000  # Standard retained earnings account
        }
        
        return defaults
        
    def create_standard_chart(self) -> List[str]:
        """
        Create standard chart of accounts
        Returns list of created account numbers
        """
        created = []
        
        # Standard chart structure
        standard_accounts = [
            # Assets
            {'ledger_key': 10000000, 'ledger_name': 'ASSETS', 'ledger_type': 1, 'ledger_level': 1},
            {'ledger_key': 10010000, 'ledger_name': 'Petty Cash', 'ledger_type': 1, 'ledger_level': 2, 'ledger_parent': 10000000},
            {'ledger_key': 10020000, 'ledger_name': 'Bank Current Account', 'ledger_type': 1, 'ledger_level': 2, 'ledger_parent': 10000000},
            {'ledger_key': 11010000, 'ledger_name': 'Trade Debtors Control', 'ledger_type': 1, 'ledger_level': 2, 'ledger_parent': 10000000, 'ledger_control': 'Y'},
            {'ledger_key': 12010000, 'ledger_name': 'Stock on Hand', 'ledger_type': 1, 'ledger_level': 2, 'ledger_parent': 10000000},
            
            # Liabilities
            {'ledger_key': 20000000, 'ledger_name': 'LIABILITIES', 'ledger_type': 2, 'ledger_level': 1},
            {'ledger_key': 20010000, 'ledger_name': 'Trade Creditors Control', 'ledger_type': 2, 'ledger_level': 2, 'ledger_parent': 20000000, 'ledger_control': 'Y'},
            {'ledger_key': 20020000, 'ledger_name': 'VAT Control', 'ledger_type': 2, 'ledger_level': 2, 'ledger_parent': 20000000},
            
            # Capital
            {'ledger_key': 30000000, 'ledger_name': 'CAPITAL & RESERVES', 'ledger_type': 3, 'ledger_level': 1},
            {'ledger_key': 30010000, 'ledger_name': 'Share Capital', 'ledger_type': 3, 'ledger_level': 2, 'ledger_parent': 30000000},
            {'ledger_key': 30020000, 'ledger_name': 'Retained Earnings', 'ledger_type': 3, 'ledger_level': 2, 'ledger_parent': 30000000},
            
            # Revenue
            {'ledger_key': 40000000, 'ledger_name': 'REVENUE', 'ledger_type': 4, 'ledger_level': 1},
            {'ledger_key': 40010000, 'ledger_name': 'Sales Revenue', 'ledger_type': 4, 'ledger_level': 2, 'ledger_parent': 40000000},
            
            # Expenses
            {'ledger_key': 50000000, 'ledger_name': 'COST OF SALES', 'ledger_type': 5, 'ledger_level': 1},
            {'ledger_key': 50010000, 'ledger_name': 'Purchases', 'ledger_type': 5, 'ledger_level': 2, 'ledger_parent': 50000000},
            {'ledger_key': 60000000, 'ledger_name': 'EXPENSES', 'ledger_type': 5, 'ledger_level': 1},
            {'ledger_key': 60010000, 'ledger_name': 'Salaries', 'ledger_type': 5, 'ledger_level': 2, 'ledger_parent': 60000000},
            {'ledger_key': 60020000, 'ledger_name': 'Rent', 'ledger_type': 5, 'ledger_level': 2, 'ledger_parent': 60000000},
        ]
        
        for account_data in standard_accounts:
            account, error = self.create_account(account_data)
            if not error:
                created.append(str(account.ledger_key))
                
        return created