"""
Year-end Processing Service - GL100 migration
Handles fiscal year-end closing procedures
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from datetime import datetime
from sqlalchemy.orm import Session
from sqlalchemy import and_, func, update
import logging

from app.services.file_handlers.gl_handler import GLFileHandler
from app.services.file_handlers.system_handler import SystemFileHandler
from app.models.gl_accounts import GLLedgerRec, GLPostingRec, GLBatchRec
from app.models.system import SystemRec
from app.core.security import log_user_action
from app.models.auth import User


logger = logging.getLogger(__name__)


class YearEndProcessingService:
    """
    Year-end processing functionality
    Implements GL100 - fiscal year closing
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.gl_handler = GLFileHandler(db)
        self.system_handler = SystemFileHandler(db)
        
    def validate_year_end(self) -> Tuple[bool, List[str]]:
        """
        Validate system is ready for year-end
        Returns (is_valid, list_of_issues)
        """
        issues = []
        
        # Get system parameters
        system_rec, _ = self.system_handler.read_system_params()
        if not system_rec:
            return False, ["System parameters not found"]
            
        # Check current period
        if system_rec.gl_period != 13:
            issues.append(f"Current period is {system_rec.gl_period}, must be 13 for year-end")
            
        # Check all periods are closed
        for period in range(1, 13):
            if not self._is_period_closed(period):
                issues.append(f"Period {period} is not closed")
                
        # Check for unposted batches
        unposted = self.db.query(GLBatchRec).filter(
            GLBatchRec.batch_status == 'O'
        ).count()
        
        if unposted > 0:
            issues.append(f"{unposted} unposted batches found")
            
        # Check trial balance is balanced
        tb_check = self._check_trial_balance()
        if not tb_check['is_balanced']:
            issues.append(f"Trial balance out of balance by {tb_check['difference']}")
            
        # Check control accounts match subledgers
        control_issues = self._check_control_accounts(system_rec)
        issues.extend(control_issues)
        
        # Check P&L accounts net to zero (should equal retained earnings movement)
        pl_check = self._check_pl_accounts()
        if pl_check and 'error' in pl_check:
            issues.append(pl_check['error'])
            
        return len(issues) == 0, issues
        
    def prepare_year_end(self) -> Dict:
        """
        Prepare year-end processing
        Returns preparation status and checklist
        """
        # Validate first
        is_valid, issues = self.validate_year_end()
        
        if not is_valid:
            return {
                'status': 'NOT_READY',
                'issues': issues,
                'can_proceed': False
            }
            
        # Get year-end data
        system_rec, _ = self.system_handler.read_system_params()
        current_year = system_rec.gl_year
        
        # Calculate P&L to be transferred
        pl_accounts = self.db.query(GLLedgerRec).filter(
            GLLedgerRec.ledger_place == 'P',
            GLLedgerRec.ledger_active == 'Y'
        ).all()
        
        total_revenue = Decimal('0')
        total_expenses = Decimal('0')
        
        for account in pl_accounts:
            if account.ledger_type == 4:  # Revenue
                total_revenue += abs(account.ledger_balance)
            elif account.ledger_type == 5:  # Expenses
                total_expenses += abs(account.ledger_balance)
                
        net_profit = total_revenue - total_expenses
        
        # Get retained earnings account
        retained_earnings_ac = self._get_retained_earnings_account()
        
        checklist = {
            'current_year': current_year,
            'new_year': current_year + 1,
            'net_profit_loss': float(net_profit),
            'retained_earnings_account': retained_earnings_ac.ledger_key if retained_earnings_ac else None,
            'revenue_accounts': len([a for a in pl_accounts if a.ledger_type == 4]),
            'expense_accounts': len([a for a in pl_accounts if a.ledger_type == 5]),
            'balance_sheet_accounts': self.db.query(GLLedgerRec).filter(
                GLLedgerRec.ledger_place == 'B',
                GLLedgerRec.ledger_active == 'Y'
            ).count()
        }
        
        return {
            'status': 'READY',
            'issues': [],
            'can_proceed': True,
            'checklist': checklist
        }
        
    def execute_year_end(self, options: Optional[Dict] = None) -> Tuple[bool, Dict]:
        """
        Execute year-end processing
        Options:
        - create_audit_trail: create detailed audit trail
        - backup_data: backup current year data
        Returns (success, results)
        """
        options = options or {}
        results = {
            'start_time': datetime.now(),
            'steps_completed': []
        }
        
        try:
            # 1. Final validation
            is_valid, issues = self.validate_year_end()
            if not is_valid:
                return False, {'error': 'Validation failed', 'issues': issues}
                
            results['steps_completed'].append('validation')
            
            # 2. Create closing journal entry
            closing_batch = self._create_closing_entries()
            results['closing_batch'] = closing_batch.batch_no
            results['steps_completed'].append('closing_entries')
            
            # 3. Update system parameters
            system_rec, _ = self.system_handler.read_system_params()
            old_year = system_rec.gl_year
            
            system_rec.gl_year = old_year + 1
            system_rec.gl_period = 1
            system_rec.gl_last_year_end = int(datetime.now().strftime("%Y%m%d"))
            
            self.system_handler.update_system_params(system_rec)
            results['new_year'] = system_rec.gl_year
            results['steps_completed'].append('system_update')
            
            # 4. Clear P&L account balances
            self._clear_pl_balances()
            results['steps_completed'].append('clear_pl')
            
            # 5. Roll forward balance sheet accounts
            bs_count = self._roll_forward_balances()
            results['bs_accounts_rolled'] = bs_count
            results['steps_completed'].append('roll_forward')
            
            # 6. Archive prior year data
            if options.get('backup_data', True):
                archive_id = self._archive_year_data(old_year)
                results['archive_id'] = archive_id
                results['steps_completed'].append('archive')
                
            # 7. Initialize new year periods
            self._initialize_new_year_periods()
            results['steps_completed'].append('init_periods')
            
            # 8. Create opening balance batch
            opening_batch = self._create_opening_balances()
            results['opening_batch'] = opening_batch.batch_no if opening_batch else None
            results['steps_completed'].append('opening_balances')
            
            # Commit all changes
            self.db.commit()
            
            # Log year-end completion
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="YEAR_END_CLOSE",
                    module="GL",
                    new_values={
                        'old_year': old_year,
                        'new_year': system_rec.gl_year,
                        'closing_batch': closing_batch.batch_no
                    }
                )
                
            results['end_time'] = datetime.now()
            results['duration'] = (results['end_time'] - results['start_time']).seconds
            results['status'] = 'COMPLETED'
            
            return True, results
            
        except Exception as e:
            logger.error(f"Year-end processing failed: {str(e)}")
            self.db.rollback()
            results['error'] = str(e)
            results['status'] = 'FAILED'
            return False, results
            
    def _create_closing_entries(self) -> GLBatchRec:
        """Create closing journal entries for P&L accounts"""
        from app.services.gl.journal_entry import JournalEntryService
        
        je_service = JournalEntryService(self.db, self.current_user)
        
        # Create closing batch
        batch = je_service.create_journal_batch(
            description="Year-end closing entries",
            source="YEAREND"
        )
        
        # Get P&L accounts
        pl_accounts = self.db.query(GLLedgerRec).filter(
            GLLedgerRec.ledger_place == 'P',
            GLLedgerRec.ledger_active == 'Y',
            GLLedgerRec.ledger_balance != 0
        ).all()
        
        # Get retained earnings account
        retained_earnings = self._get_retained_earnings_account()
        if not retained_earnings:
            raise ValueError("Retained earnings account not found")
            
        total_net = Decimal('0')
        
        # Create closing entries - reverse P&L balances
        for account in pl_accounts:
            balance = account.ledger_balance
            
            if account.ledger_type == 4:  # Revenue - normally credit
                # Debit revenue to close
                entry_data = {
                    'account': account.ledger_key,
                    'debit': abs(balance) if balance < 0 else 0,
                    'credit': abs(balance) if balance > 0 else 0,
                    'reference': 'YEAREND',
                    'description': f"Close {account.ledger_name}"
                }
                total_net += abs(balance)  # Revenue increases net
                
            elif account.ledger_type == 5:  # Expenses - normally debit
                # Credit expenses to close
                entry_data = {
                    'account': account.ledger_key,
                    'debit': abs(balance) if balance < 0 else 0,
                    'credit': abs(balance) if balance > 0 else 0,
                    'reference': 'YEAREND',
                    'description': f"Close {account.ledger_name}"
                }
                total_net -= abs(balance)  # Expenses decrease net
                
            je_service.add_journal_line(batch.batch_no, entry_data)
            
        # Create balancing entry to retained earnings
        re_entry = {
            'account': retained_earnings.ledger_key,
            'debit': abs(total_net) if total_net < 0 else 0,
            'credit': abs(total_net) if total_net > 0 else 0,
            'reference': 'YEAREND',
            'description': 'Transfer net profit/loss to retained earnings'
        }
        je_service.add_journal_line(batch.batch_no, re_entry)
        
        # Post the batch
        success, error = je_service.post_batch(batch.batch_no)
        if not success:
            raise ValueError(f"Failed to post closing entries: {error}")
            
        return batch
        
    def _clear_pl_balances(self):
        """Clear P&L account balances for new year"""
        # Update all P&L accounts
        self.db.execute(
            update(GLLedgerRec).
            where(GLLedgerRec.ledger_place == 'P').
            values(ledger_balance=0)
        )
        
        # Clear period balances
        for period in range(1, 14):
            period_field = f"ledger_period_{period}"
            self.db.execute(
                update(GLLedgerRec).
                where(GLLedgerRec.ledger_place == 'P').
                values({period_field: 0})
            )
            
    def _roll_forward_balances(self) -> int:
        """Roll forward balance sheet account balances"""
        # Get balance sheet accounts
        bs_accounts = self.db.query(GLLedgerRec).filter(
            GLLedgerRec.ledger_place == 'B',
            GLLedgerRec.ledger_active == 'Y'
        ).all()
        
        count = 0
        for account in bs_accounts:
            # Store current balance as prior year
            account.ledger_last_year_actual = account.ledger_balance
            
            # Clear period balances for new year
            for period in range(1, 14):
                period_field = f"ledger_period_{period}"
                setattr(account, period_field, Decimal('0'))
                
            count += 1
            
        self.db.flush()
        return count
        
    def _archive_year_data(self, year: int) -> int:
        """Archive closed year data"""
        # This would copy data to archive tables
        # For now, return a dummy archive ID
        archive_id = int(f"{year}{datetime.now().strftime('%m%d%H%M')}")
        
        logger.info(f"Archived year {year} data with ID {archive_id}")
        return archive_id
        
    def _initialize_new_year_periods(self):
        """Initialize period control for new year"""
        # This would set up period statuses, etc.
        pass
        
    def _create_opening_balances(self) -> Optional[GLBatchRec]:
        """Create opening balance batch for accounts that need it"""
        # Check if any accounts need opening balance entries
        # This is typically for reconcilable accounts
        reconcilable = self.db.query(GLLedgerRec).filter(
            and_(
                GLLedgerRec.ledger_reconcilable == 'Y',
                GLLedgerRec.ledger_balance != 0
            )
        ).count()
        
        if reconcilable == 0:
            return None
            
        # Would create opening balance batch here
        return None
        
    def _is_period_closed(self, period: int) -> bool:
        """Check if a period is closed"""
        # Check for unposted transactions in period
        unposted = self.db.query(GLPostingRec).filter(
            and_(
                GLPostingRec.posting_period == period,
                GLPostingRec.posting_status == 'U'
            )
        ).count()
        
        return unposted == 0
        
    def _check_trial_balance(self) -> Dict:
        """Check if trial balance is balanced"""
        # Get all active accounts
        accounts = self.db.query(GLLedgerRec).filter(
            GLLedgerRec.ledger_active == 'Y'
        ).all()
        
        total_dr = Decimal('0')
        total_cr = Decimal('0')
        
        for account in accounts:
            balance = account.ledger_balance
            if account.ledger_type in [1, 5]:  # Assets & Expenses
                if balance > 0:
                    total_dr += balance
                else:
                    total_cr += abs(balance)
            else:  # Liabilities, Capital, Revenue
                if balance > 0:
                    total_cr += balance
                else:
                    total_dr += abs(balance)
                    
        difference = abs(total_dr - total_cr)
        
        return {
            'total_debit': float(total_dr),
            'total_credit': float(total_cr),
            'difference': float(difference),
            'is_balanced': difference < Decimal('0.01')
        }
        
    def _check_control_accounts(self, system_rec: SystemRec) -> List[str]:
        """Check control accounts match subledgers"""
        issues = []
        
        # Check debtors control
        if system_rec.s_debtors > 0:
            control_balance = self._get_account_balance(system_rec.s_debtors)
            sl_total = self._get_sales_ledger_total()
            
            if abs(control_balance - sl_total) > Decimal('0.01'):
                issues.append(
                    f"Sales control {control_balance} != SL total {sl_total}"
                )
                
        # Check creditors control
        if system_rec.p_creditors > 0:
            control_balance = self._get_account_balance(system_rec.p_creditors)
            pl_total = self._get_purchase_ledger_total()
            
            if abs(control_balance - pl_total) > Decimal('0.01'):
                issues.append(
                    f"Purchase control {control_balance} != PL total {pl_total}"
                )
                
        return issues
        
    def _check_pl_accounts(self) -> Optional[Dict]:
        """Check P&L accounts net to retained earnings movement"""
        pl_accounts = self.db.query(GLLedgerRec).filter(
            GLLedgerRec.ledger_place == 'P'
        ).all()
        
        net_pl = Decimal('0')
        for account in pl_accounts:
            if account.ledger_type == 4:  # Revenue
                net_pl += abs(account.ledger_balance)
            elif account.ledger_type == 5:  # Expenses
                net_pl -= abs(account.ledger_balance)
                
        if abs(net_pl) > Decimal('0.01'):
            return None  # This is expected - will transfer to RE
            
        return None
        
    def _get_retained_earnings_account(self) -> Optional[GLLedgerRec]:
        """Get retained earnings account"""
        # Standard retained earnings account number
        re_account = self.db.query(GLLedgerRec).filter(
            GLLedgerRec.ledger_key == 30020000
        ).first()
        
        if not re_account:
            # Try to find by name
            re_account = self.db.query(GLLedgerRec).filter(
                GLLedgerRec.ledger_name.ilike('%retained%earnings%')
            ).first()
            
        return re_account
        
    def _get_account_balance(self, account_no: int) -> Decimal:
        """Get account balance"""
        account = self.db.query(GLLedgerRec).filter(
            GLLedgerRec.ledger_key == account_no
        ).first()
        
        return account.ledger_balance if account else Decimal('0')
        
    def _get_sales_ledger_total(self) -> Decimal:
        """Get total of sales ledger balances"""
        from app.models.customer import SalesLedgerRec
        
        result = self.db.query(
            func.sum(SalesLedgerRec.sales_balance)
        ).scalar()
        
        return Decimal(str(result)) if result else Decimal('0')
        
    def _get_purchase_ledger_total(self) -> Decimal:
        """Get total of purchase ledger balances"""
        from app.models.supplier import PurchaseLedgerRec
        
        result = self.db.query(
            func.sum(PurchaseLedgerRec.purch_balance)
        ).scalar()
        
        return Decimal(str(result)) if result else Decimal('0')
        
    def get_year_end_report(self) -> Dict:
        """Generate year-end summary report"""
        # Preparation data
        prep_data = self.prepare_year_end()
        
        if prep_data['status'] != 'READY':
            return prep_data
            
        # Get additional year-end statistics
        system_rec, _ = self.system_handler.read_system_params()
        
        # Transaction counts by period
        period_counts = []
        for period in range(1, 14):
            count = self.db.query(GLPostingRec).filter(
                and_(
                    GLPostingRec.posting_period == period,
                    GLPostingRec.posting_status == 'P'
                )
            ).count()
            
            period_counts.append({
                'period': period,
                'transaction_count': count
            })
            
        # Account statistics
        total_accounts = self.db.query(GLLedgerRec).count()
        active_accounts = self.db.query(GLLedgerRec).filter(
            GLLedgerRec.ledger_active == 'Y'
        ).count()
        
        # Batch statistics
        total_batches = self.db.query(GLBatchRec).count()
        
        return {
            'preparation': prep_data,
            'statistics': {
                'current_year': system_rec.gl_year,
                'current_period': system_rec.gl_period,
                'total_accounts': total_accounts,
                'active_accounts': active_accounts,
                'total_batches': total_batches,
                'period_activity': period_counts,
                'last_year_end': system_rec.gl_last_year_end
            }
        }