"""
General Ledger File Handler - COBOL acas010 migration
Handles glledger_rec (Chart of Accounts)
"""
from typing import Any, Tuple, Optional, List
from decimal import Decimal
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func

from app.services.file_handlers.base_handler import BaseFileHandler, FileAccess, COBOLError
from app.models.gl_accounts import GLLedgerRec, GLPostingRec, GLBatchRec
from datetime import datetime


class GLFileHandler(BaseFileHandler):
    """
    General Ledger File Handler (acas010)
    Handles GL account master records
    """
    
    def __init__(self, db: Session):
        super().__init__(db, file_number=10, table_name="glledger_rec")
        
    def get_model_class(self):
        return GLLedgerRec
        
    def get_key_field(self, key_type: int = 1):
        """GL uses account number as primary key"""
        return "ledger_key"
        
    def get_account_by_type(self, account_type: int) -> List[GLLedgerRec]:
        """
        Get accounts by type:
        1 = Asset
        2 = Liability  
        3 = Capital/Equity
        4 = Income/Revenue
        5 = Expense
        """
        return self.db.query(GLLedgerRec).filter(
            GLLedgerRec.ledger_type == account_type
        ).order_by(GLLedgerRec.ledger_key).all()
        
    def get_balance_sheet_accounts(self) -> List[GLLedgerRec]:
        """Get all balance sheet accounts"""
        return self.db.query(GLLedgerRec).filter(
            GLLedgerRec.ledger_place == 'B'
        ).order_by(GLLedgerRec.ledger_key).all()
        
    def get_pl_accounts(self) -> List[GLLedgerRec]:
        """Get all P&L (income statement) accounts"""
        return self.db.query(GLLedgerRec).filter(
            GLLedgerRec.ledger_place == 'P'
        ).order_by(GLLedgerRec.ledger_key).all()
        
    def get_control_accounts(self) -> List[GLLedgerRec]:
        """Get control accounts (AR, AP, etc)"""
        return self.db.query(GLLedgerRec).filter(
            GLLedgerRec.ledger_control == 'Y'
        ).order_by(GLLedgerRec.ledger_key).all()
        
    def post_journal_entry(self, batch_no: int, entries: List[dict]) -> FileAccess:
        """
        Post journal entry to GL
        entries = [{"account": 12345, "debit": 100.00, "credit": 0.00, "description": "..."}, ...]
        """
        status = FileAccess()
        
        # Validate batch exists
        batch = self.db.query(GLBatchRec).filter(
            GLBatchRec.batch_no == batch_no
        ).first()
        
        if not batch or batch.batch_status != 'O':
            status.we_error = COBOLError.GENERAL_ERROR
            status.fs_reply = "99"
            return status
            
        # Validate balanced entry
        total_debits = sum(e.get('debit', 0) for e in entries)
        total_credits = sum(e.get('credit', 0) for e in entries)
        
        if abs(total_debits - total_credits) > 0.01:  # Allow for rounding
            status.we_error = COBOLError.GENERAL_ERROR
            status.fs_reply = "99"
            return status
            
        # Post each line
        for entry in entries:
            account_no = entry['account']
            debit = Decimal(str(entry.get('debit', 0)))
            credit = Decimal(str(entry.get('credit', 0)))
            
            # Get account
            account, acc_status = self._read_indexed(account_no, 1)
            if acc_status.fs_reply != "00" or not account:
                status.we_error = COBOLError.KEY_NOT_FOUND
                status.fs_reply = "23"
                return status
                
            # Update account balance
            if debit > 0:
                if account.ledger_type in [1, 5]:  # Assets and Expenses
                    account.ledger_balance += debit
                else:  # Liabilities, Capital, Income
                    account.ledger_balance -= debit
            else:
                if account.ledger_type in [1, 5]:  # Assets and Expenses
                    account.ledger_balance -= credit
                else:  # Liabilities, Capital, Income
                    account.ledger_balance += credit
                    
            # Update period balances
            period = self._get_current_period()
            period_field = f"ledger_period_{period}"
            if hasattr(account, period_field):
                current_period_bal = getattr(account, period_field)
                if debit > 0:
                    setattr(account, period_field, current_period_bal + debit)
                else:
                    setattr(account, period_field, current_period_bal + credit)
                    
            # Update MTD/YTD
            if debit > 0:
                account.ledger_mtd_actual += debit
                account.ledger_ytd_actual += debit
            else:
                account.ledger_mtd_actual += credit
                account.ledger_ytd_actual += credit
                
            self.db.merge(account)
            
            # Create posting record
            posting = GLPostingRec(
                posting_batch=batch_no,
                posting_date=int(datetime.now().strftime("%Y%m%d")),
                posting_period=period,
                posting_type='JE',
                posting_reference=entry.get('reference', ''),
                posting_description=entry.get('description', ''),
                posting_account=account_no,
                posting_debit=debit,
                posting_credit=credit,
                posting_status='P',
                posting_user='SYSTEM'
            )
            self.db.add(posting)
            
        # Update batch totals
        batch.batch_debits = Decimal(str(total_debits))
        batch.batch_credits = Decimal(str(total_credits))
        batch.batch_entries = len(entries)
        self.db.merge(batch)
        
        self.db.flush()
        
        status.fs_reply = "00"
        return status
        
    def get_account_balance(self, account_no: int, balance_type: str = "CURRENT") -> float:
        """
        Get account balance
        balance_type: CURRENT, MTD, YTD, LAST_YEAR
        """
        account, status = self._read_indexed(account_no, 1)
        
        if status.fs_reply != "00" or not account:
            return 0.0
            
        if balance_type == "CURRENT":
            return float(account.ledger_balance)
        elif balance_type == "MTD":
            return float(account.ledger_mtd_actual)
        elif balance_type == "YTD":
            return float(account.ledger_ytd_actual)
        elif balance_type == "LAST_YEAR":
            return float(account.ledger_last_year_actual)
        else:
            return 0.0
            
    def get_trial_balance(self) -> List[dict]:
        """Get trial balance data"""
        accounts = self.db.query(GLLedgerRec).filter(
            GLLedgerRec.ledger_active == 'Y'
        ).order_by(GLLedgerRec.ledger_key).all()
        
        trial_balance = []
        total_debits = Decimal('0')
        total_credits = Decimal('0')
        
        for account in accounts:
            balance = account.ledger_balance
            if balance != 0:
                entry = {
                    'account': account.ledger_key,
                    'name': account.ledger_name,
                    'type': account.ledger_type,
                    'debit': balance if balance > 0 and account.ledger_type in [1, 5] else Decimal('0'),
                    'credit': abs(balance) if balance < 0 and account.ledger_type in [1, 5] else 
                             balance if balance > 0 and account.ledger_type in [2, 3, 4] else Decimal('0')
                }
                trial_balance.append(entry)
                total_debits += entry['debit']
                total_credits += entry['credit']
                
        return {
            'entries': trial_balance,
            'total_debits': float(total_debits),
            'total_credits': float(total_credits),
            'in_balance': abs(total_debits - total_credits) < 0.01
        }
        
    def close_period(self, period: int) -> FileAccess:
        """Close accounting period"""
        status = FileAccess()
        
        # Get all P&L accounts
        pl_accounts = self.get_pl_accounts()
        
        net_income = Decimal('0')
        
        # Calculate net income
        for account in pl_accounts:
            if account.ledger_type == 4:  # Income
                net_income += account.ledger_balance
            elif account.ledger_type == 5:  # Expense
                net_income -= account.ledger_balance
                
        # TODO: Post net income to retained earnings account
        # This would need the retained earnings account number from system settings
        
        # Zero out P&L accounts for new period
        for account in pl_accounts:
            account.ledger_balance = Decimal('0')
            account.ledger_mtd_actual = Decimal('0')
            self.db.merge(account)
            
        self.db.flush()
        
        status.fs_reply = "00"
        return status
        
    def _get_current_period(self) -> int:
        """Get current accounting period from system"""
        # This would normally read from system file
        # For now, return based on current month
        return datetime.now().month
        
    def validate_account_number(self, account_no: int) -> bool:
        """Validate if account exists and is active"""
        account, status = self._read_indexed(account_no, 1)
        return status.fs_reply == "00" and account and account.ledger_active == 'Y'
        
    def get_account_hierarchy(self, parent_account: int = 0) -> List[GLLedgerRec]:
        """Get account hierarchy starting from parent"""
        return self.db.query(GLLedgerRec).filter(
            GLLedgerRec.ledger_parent == parent_account
        ).order_by(GLLedgerRec.ledger_key).all()
        
    def search_accounts(self, search_term: str) -> List[GLLedgerRec]:
        """Search accounts by name or number"""
        search_upper = search_term.upper()
        
        # Try exact account number first
        try:
            account_no = int(search_term)
            account, status = self._read_indexed(account_no, 1)
            if status.fs_reply == "00" and account:
                return [account]
        except ValueError:
            pass
            
        # Search by name
        return self.db.query(GLLedgerRec).filter(
            func.upper(GLLedgerRec.ledger_name).contains(search_upper)
        ).order_by(GLLedgerRec.ledger_key).limit(50).all()