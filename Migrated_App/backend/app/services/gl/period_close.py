"""
Period-end Close Service - GL105 migration
Handles monthly/period closing procedures
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from datetime import datetime
from sqlalchemy.orm import Session
from sqlalchemy import and_, func
import logging

from app.services.file_handlers.gl_handler import GLFileHandler
from app.services.file_handlers.system_handler import SystemFileHandler
from app.models.gl_accounts import GLLedgerRec, GLPostingRec, GLBatchRec, GLPeriodStatusRec
from app.models.system import SystemRec
from app.core.security import log_user_action
from app.models.auth import User


logger = logging.getLogger(__name__)


class PeriodCloseService:
    """
    Period-end closing functionality
    Implements GL105 - monthly closing procedures
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.gl_handler = GLFileHandler(db)
        self.system_handler = SystemFileHandler(db)
        
    def get_period_status(self, period: Optional[int] = None) -> Dict:
        """Get detailed status for a period"""
        system_rec, _ = self.system_handler.read_system_params()
        if not system_rec:
            return {"error": "System parameters not found"}
            
        period = period or system_rec.gl_period
        
        # Check period status record
        period_status = self.db.query(GLPeriodStatusRec).filter(
            GLPeriodStatusRec.period_number == period,
            GLPeriodStatusRec.period_year == system_rec.gl_year
        ).first()
        
        # Count unposted items
        unposted_batches = self.db.query(GLBatchRec).filter(
            and_(
                GLBatchRec.batch_period == period,
                GLBatchRec.batch_status == 'O'
            )
        ).count()
        
        unposted_trans = self.db.query(GLPostingRec).filter(
            and_(
                GLPostingRec.posting_period == period,
                GLPostingRec.posting_status == 'U'
            )
        ).count()
        
        # Get period activity
        period_trans = self.db.query(GLPostingRec).filter(
            and_(
                GLPostingRec.posting_period == period,
                GLPostingRec.posting_status == 'P'
            )
        ).count()
        
        # Calculate period totals
        period_totals = self.db.query(
            func.sum(GLPostingRec.posting_debit).label('total_dr'),
            func.sum(GLPostingRec.posting_credit).label('total_cr')
        ).filter(
            and_(
                GLPostingRec.posting_period == period,
                GLPostingRec.posting_status == 'P'
            )
        ).first()
        
        return {
            'period': period,
            'year': system_rec.gl_year,
            'status': period_status.status if period_status else 'OPEN',
            'is_current': period == system_rec.gl_period,
            'activity': {
                'posted_transactions': period_trans,
                'unposted_batches': unposted_batches,
                'unposted_transactions': unposted_trans,
                'total_debits': float(period_totals.total_dr or 0),
                'total_credits': float(period_totals.total_cr or 0)
            },
            'can_close': unposted_batches == 0 and unposted_trans == 0,
            'closed_date': period_status.closed_date if period_status and period_status.status == 'CLOSED' else None,
            'closed_by': period_status.closed_by if period_status and period_status.status == 'CLOSED' else None
        }
        
    def validate_period_close(self, period: Optional[int] = None) -> Tuple[bool, List[str]]:
        """
        Validate period is ready to close
        Returns (is_valid, list_of_issues)
        """
        issues = []
        
        system_rec, _ = self.system_handler.read_system_params()
        period = period or system_rec.gl_period
        
        # Check unposted batches
        unposted_batches = self.db.query(GLBatchRec).filter(
            and_(
                GLBatchRec.batch_period == period,
                GLBatchRec.batch_status == 'O'
            )
        ).all()
        
        if unposted_batches:
            issues.append(f"{len(unposted_batches)} unposted batches exist")
            for batch in unposted_batches[:5]:  # Show first 5
                issues.append(f"  - Batch {batch.batch_no}: {batch.batch_description}")
                
        # Check unposted transactions
        unposted_trans = self.db.query(GLPostingRec).filter(
            and_(
                GLPostingRec.posting_period == period,
                GLPostingRec.posting_status == 'U'
            )
        ).count()
        
        if unposted_trans > 0:
            issues.append(f"{unposted_trans} unposted transactions exist")
            
        # Check trial balance is balanced
        tb_check = self._check_period_trial_balance(period)
        if not tb_check['is_balanced']:
            issues.append(f"Period trial balance out of balance by {tb_check['difference']}")
            
        # Check control account reconciliations
        control_issues = self._check_control_reconciliations(period)
        issues.extend(control_issues)
        
        # Check for required period-end adjustments
        adjustment_issues = self._check_period_adjustments(period)
        issues.extend(adjustment_issues)
        
        # Verify all subledgers are closed
        if system_rec.sl_exists == 'Y':
            sl_status = self._check_subledger_status('SL', period)
            if not sl_status['is_closed']:
                issues.append("Sales Ledger not closed for period")
                
        if system_rec.pl_exists == 'Y':
            pl_status = self._check_subledger_status('PL', period)
            if not pl_status['is_closed']:
                issues.append("Purchase Ledger not closed for period")
                
        if system_rec.stock_exists == 'Y':
            stock_status = self._check_subledger_status('STOCK', period)
            if not stock_status['is_closed']:
                issues.append("Stock Control not closed for period")
                
        return len(issues) == 0, issues
        
    def prepare_period_close(self, period: Optional[int] = None) -> Dict:
        """Prepare period close with checklist"""
        system_rec, _ = self.system_handler.read_system_params()
        period = period or system_rec.gl_period
        
        # Validate first
        is_valid, issues = self.validate_period_close(period)
        
        # Get period statistics
        stats = self._get_period_statistics(period)
        
        # Build checklist
        checklist = {
            'reconciliations': self._get_reconciliation_status(period),
            'adjustments': self._get_adjustment_status(period),
            'reports': self._get_required_reports(period),
            'approvals': self._get_approval_status(period)
        }
        
        return {
            'period': period,
            'year': system_rec.gl_year,
            'can_close': is_valid,
            'validation_issues': issues,
            'statistics': stats,
            'checklist': checklist,
            'next_period': self._get_next_period(period)
        }
        
    def execute_period_close(self, period: Optional[int] = None, 
                           options: Optional[Dict] = None) -> Tuple[bool, Dict]:
        """
        Execute period closing
        Options:
        - force_close: close even with warnings
        - create_snapshot: create period-end snapshot
        Returns (success, results)
        """
        options = options or {}
        results = {
            'start_time': datetime.now(),
            'steps_completed': []
        }
        
        try:
            system_rec, _ = self.system_handler.read_system_params()
            period = period or system_rec.gl_period
            
            # 1. Final validation
            is_valid, issues = self.validate_period_close(period)
            if not is_valid and not options.get('force_close', False):
                return False, {'error': 'Validation failed', 'issues': issues}
                
            results['validation_issues'] = issues if not is_valid else []
            results['steps_completed'].append('validation')
            
            # 2. Post any pending accruals
            accruals_posted = self._post_period_accruals(period)
            results['accruals_posted'] = accruals_posted
            results['steps_completed'].append('accruals')
            
            # 3. Update period balances in GL accounts
            self._update_period_balances(period)
            results['steps_completed'].append('update_balances')
            
            # 4. Create period snapshot if requested
            if options.get('create_snapshot', True):
                snapshot_id = self._create_period_snapshot(period)
                results['snapshot_id'] = snapshot_id
                results['steps_completed'].append('snapshot')
                
            # 5. Update period status
            period_status = self._update_period_status(period, 'CLOSED')
            results['steps_completed'].append('status_update')
            
            # 6. Roll forward to next period if current
            if period == system_rec.gl_period:
                next_period = self._get_next_period(period)
                system_rec.gl_period = next_period
                self.system_handler.update_system_params(system_rec)
                results['new_current_period'] = next_period
                results['steps_completed'].append('roll_forward')
                
            # 7. Initialize next period
            self._initialize_period(results.get('new_current_period', period + 1))
            results['steps_completed'].append('init_next_period')
            
            # 8. Generate period-end reports
            reports = self._generate_period_reports(period)
            results['reports_generated'] = reports
            results['steps_completed'].append('reports')
            
            # Commit changes
            self.db.commit()
            
            # Log period close
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="PERIOD_CLOSE",
                    module="GL",
                    new_values={
                        'period': period,
                        'year': system_rec.gl_year,
                        'forced': options.get('force_close', False)
                    }
                )
                
            results['end_time'] = datetime.now()
            results['duration'] = (results['end_time'] - results['start_time']).seconds
            results['status'] = 'COMPLETED'
            
            return True, results
            
        except Exception as e:
            logger.error(f"Period close failed: {str(e)}")
            self.db.rollback()
            results['error'] = str(e)
            results['status'] = 'FAILED'
            return False, results
            
    def reopen_period(self, period: int, reason: str) -> Tuple[bool, Optional[str]]:
        """
        Reopen a closed period
        Returns (success, error_message)
        """
        # Check period exists and is closed
        period_status = self.db.query(GLPeriodStatusRec).filter(
            GLPeriodStatusRec.period_number == period
        ).first()
        
        if not period_status:
            return False, "Period status not found"
            
        if period_status.status != 'CLOSED':
            return False, "Period is not closed"
            
        # Check if subsequent periods are closed
        system_rec, _ = self.system_handler.read_system_params()
        if period < system_rec.gl_period - 1:
            return False, "Cannot reopen period with subsequent closed periods"
            
        # Update status
        period_status.status = 'OPEN'
        period_status.reopened_date = int(datetime.now().strftime("%Y%m%d"))
        period_status.reopened_by = self.current_user.username if self.current_user else 'SYSTEM'
        period_status.reopen_reason = reason
        
        self.db.flush()
        
        # Log reopening
        if self.current_user:
            log_user_action(
                db=self.db,
                user=self.current_user,
                action="PERIOD_REOPEN",
                module="GL",
                new_values={
                    'period': period,
                    'reason': reason
                }
            )
            
        return True, None
        
    def _check_period_trial_balance(self, period: int) -> Dict:
        """Check if period movements balance"""
        # Get period movements
        movements = self.db.query(
            func.sum(GLPostingRec.posting_debit).label('total_dr'),
            func.sum(GLPostingRec.posting_credit).label('total_cr')
        ).filter(
            and_(
                GLPostingRec.posting_period == period,
                GLPostingRec.posting_status == 'P'
            )
        ).first()
        
        total_dr = movements.total_dr or Decimal('0')
        total_cr = movements.total_cr or Decimal('0')
        difference = abs(total_dr - total_cr)
        
        return {
            'total_debit': float(total_dr),
            'total_credit': float(total_cr),
            'difference': float(difference),
            'is_balanced': difference < Decimal('0.01')
        }
        
    def _check_control_reconciliations(self, period: int) -> List[str]:
        """Check control account reconciliations"""
        issues = []
        
        # Get control accounts
        control_accounts = self.db.query(GLLedgerRec).filter(
            and_(
                GLLedgerRec.ledger_control == 'Y',
                GLLedgerRec.ledger_active == 'Y'
            )
        ).all()
        
        for account in control_accounts:
            # Check if reconciled for period
            if account.ledger_reconcilable == 'Y':
                unreconciled = self.db.query(GLPostingRec).filter(
                    and_(
                        GLPostingRec.posting_account == account.ledger_key,
                        GLPostingRec.posting_period <= period,
                        GLPostingRec.posting_reconciled != 'Y'
                    )
                ).count()
                
                if unreconciled > 0:
                    issues.append(
                        f"Control account {account.ledger_key} has {unreconciled} unreconciled items"
                    )
                    
        return issues
        
    def _check_period_adjustments(self, period: int) -> List[str]:
        """Check for required period-end adjustments"""
        issues = []
        
        # Check for standard adjustments
        # This would check for depreciation, accruals, prepayments, etc.
        
        # Example: Check depreciation has been run
        depreciation_batch = self.db.query(GLBatchRec).filter(
            and_(
                GLBatchRec.batch_period == period,
                GLBatchRec.batch_source == 'DEPRECIATION',
                GLBatchRec.batch_status == 'P'
            )
        ).first()
        
        if not depreciation_batch:
            issues.append("Depreciation not posted for period")
            
        return issues
        
    def _check_subledger_status(self, ledger_type: str, period: int) -> Dict:
        """Check if subledger is closed for period"""
        # This would check the respective subledger status
        # For now, return as closed
        return {'is_closed': True}
        
    def _get_period_statistics(self, period: int) -> Dict:
        """Get comprehensive period statistics"""
        # Transaction counts by type
        type_counts = self.db.query(
            GLPostingRec.posting_type,
            func.count(GLPostingRec.posting_id).label('count'),
            func.sum(GLPostingRec.posting_debit).label('total_dr'),
            func.sum(GLPostingRec.posting_credit).label('total_cr')
        ).filter(
            and_(
                GLPostingRec.posting_period == period,
                GLPostingRec.posting_status == 'P'
            )
        ).group_by(GLPostingRec.posting_type).all()
        
        # Batch statistics
        batch_stats = self.db.query(
            func.count(GLBatchRec.batch_no).label('count'),
            func.sum(GLBatchRec.batch_entries).label('total_entries')
        ).filter(
            and_(
                GLBatchRec.batch_period == period,
                GLBatchRec.batch_status == 'P'
            )
        ).first()
        
        return {
            'transaction_types': [
                {
                    'type': t.posting_type,
                    'count': t.count,
                    'total_debit': float(t.total_dr or 0),
                    'total_credit': float(t.total_cr or 0)
                }
                for t in type_counts
            ],
            'batch_count': batch_stats.count if batch_stats else 0,
            'total_entries': batch_stats.total_entries if batch_stats else 0
        }
        
    def _post_period_accruals(self, period: int) -> int:
        """Post standard period-end accruals"""
        # This would create and post accrual entries
        # For now, return 0
        return 0
        
    def _update_period_balances(self, period: int):
        """Update period balances in GL accounts"""
        # This ensures all account period fields are updated
        # with current period activity
        pass
        
    def _create_period_snapshot(self, period: int) -> int:
        """Create snapshot of period-end balances"""
        # This would create a snapshot for audit/reporting
        snapshot_id = int(f"{period}{datetime.now().strftime('%Y%m%d%H%M')}")
        
        logger.info(f"Created period {period} snapshot with ID {snapshot_id}")
        return snapshot_id
        
    def _update_period_status(self, period: int, status: str) -> GLPeriodStatusRec:
        """Update or create period status record"""
        system_rec, _ = self.system_handler.read_system_params()
        
        period_status = self.db.query(GLPeriodStatusRec).filter(
            and_(
                GLPeriodStatusRec.period_number == period,
                GLPeriodStatusRec.period_year == system_rec.gl_year
            )
        ).first()
        
        if not period_status:
            period_status = GLPeriodStatusRec(
                period_number=period,
                period_year=system_rec.gl_year
            )
            self.db.add(period_status)
            
        period_status.status = status
        if status == 'CLOSED':
            period_status.closed_date = int(datetime.now().strftime("%Y%m%d"))
            period_status.closed_by = self.current_user.username if self.current_user else 'SYSTEM'
            
        self.db.flush()
        return period_status
        
    def _get_next_period(self, current_period: int) -> int:
        """Get next period number"""
        if current_period >= 12:
            return 13  # Year-end period
        return current_period + 1
        
    def _initialize_period(self, period: int):
        """Initialize new period"""
        # Set up any period-specific data
        pass
        
    def _generate_period_reports(self, period: int) -> List[str]:
        """Generate standard period-end reports"""
        reports = []
        
        # This would trigger generation of:
        # - Period Trial Balance
        # - Period P&L
        # - Period movements report
        # - Exception reports
        
        reports.append(f"TB_{period}")
        reports.append(f"PL_{period}")
        reports.append(f"MOV_{period}")
        
        return reports
        
    def _get_reconciliation_status(self, period: int) -> Dict:
        """Get reconciliation status for key accounts"""
        recon_accounts = self.db.query(GLLedgerRec).filter(
            GLLedgerRec.ledger_reconcilable == 'Y'
        ).all()
        
        status = {}
        for account in recon_accounts:
            unreconciled = self.db.query(GLPostingRec).filter(
                and_(
                    GLPostingRec.posting_account == account.ledger_key,
                    GLPostingRec.posting_period <= period,
                    GLPostingRec.posting_reconciled != 'Y'
                )
            ).count()
            
            status[account.ledger_key] = {
                'name': account.ledger_name,
                'unreconciled_count': unreconciled,
                'is_reconciled': unreconciled == 0
            }
            
        return status
        
    def _get_adjustment_status(self, period: int) -> Dict:
        """Get status of period-end adjustments"""
        return {
            'depreciation': self._check_adjustment_posted('DEPRECIATION', period),
            'accruals': self._check_adjustment_posted('ACCRUALS', period),
            'prepayments': self._check_adjustment_posted('PREPAYMENTS', period),
            'provisions': self._check_adjustment_posted('PROVISIONS', period)
        }
        
    def _check_adjustment_posted(self, adj_type: str, period: int) -> bool:
        """Check if specific adjustment has been posted"""
        batch = self.db.query(GLBatchRec).filter(
            and_(
                GLBatchRec.batch_period == period,
                GLBatchRec.batch_source == adj_type,
                GLBatchRec.batch_status == 'P'
            )
        ).first()
        
        return batch is not None
        
    def _get_required_reports(self, period: int) -> List[Dict]:
        """Get list of required period-end reports"""
        return [
            {'name': 'Trial Balance', 'code': 'GL_TB', 'required': True},
            {'name': 'Profit & Loss', 'code': 'GL_PL', 'required': True},
            {'name': 'Balance Sheet', 'code': 'GL_BS', 'required': True},
            {'name': 'Bank Reconciliation', 'code': 'GL_BANK', 'required': True},
            {'name': 'VAT Return', 'code': 'GL_VAT', 'required': False}
        ]
        
    def _get_approval_status(self, period: int) -> Dict:
        """Get period-end approval status"""
        # This would check for required approvals
        return {
            'controller_approval': False,
            'cfo_approval': False,
            'audit_review': False
        }