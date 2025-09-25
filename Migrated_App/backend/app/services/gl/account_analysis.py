"""
Account Analysis Service - GL080 migration
Provides detailed transaction analysis for GL accounts
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from datetime import datetime, date
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func

from app.services.file_handlers.gl_handler import GLFileHandler
from app.services.file_handlers.system_handler import SystemFileHandler
from app.models.gl_accounts import GLLedgerRec, GLPostingRec
from app.core.security import log_user_action
from app.models.auth import User


class AccountAnalysisService:
    """
    Account Analysis functionality
    Implements GL080 - detailed movement analysis
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.gl_handler = GLFileHandler(db)
        self.system_handler = SystemFileHandler(db)
        
    def analyze_account(self, account_no: int, options: Optional[Dict] = None) -> Dict:
        """
        Analyze account movements
        Options:
        - date_from/date_to: date range
        - period_from/period_to: period range
        - include_unposted: include unposted entries
        - analysis_code: filter by analysis code
        - reference_pattern: filter by reference
        - amount_min/amount_max: amount range
        """
        options = options or {}
        
        # Validate account exists
        account, status = self.gl_handler.process(4, key_value=account_no)
        if status.fs_reply != "00":
            return {"error": "Account not found"}
            
        # Build query
        query = self.db.query(GLPostingRec).filter(
            GLPostingRec.posting_account == account_no
        )
        
        # Apply filters
        if 'date_from' in options:
            query = query.filter(GLPostingRec.posting_date >= options['date_from'])
        if 'date_to' in options:
            query = query.filter(GLPostingRec.posting_date <= options['date_to'])
            
        if 'period_from' in options:
            query = query.filter(GLPostingRec.posting_period >= options['period_from'])
        if 'period_to' in options:
            query = query.filter(GLPostingRec.posting_period <= options['period_to'])
            
        if not options.get('include_unposted', False):
            query = query.filter(GLPostingRec.posting_status == 'P')
            
        if 'analysis_code' in options:
            query = query.filter(
                or_(
                    GLPostingRec.posting_analysis_1 == options['analysis_code'],
                    GLPostingRec.posting_analysis_2 == options['analysis_code'],
                    GLPostingRec.posting_analysis_3 == options['analysis_code']
                )
            )
            
        if 'reference_pattern' in options:
            query = query.filter(
                GLPostingRec.posting_reference.ilike(f"%{options['reference_pattern']}%")
            )
            
        if 'amount_min' in options:
            min_amt = Decimal(str(options['amount_min']))
            query = query.filter(
                or_(
                    GLPostingRec.posting_debit >= min_amt,
                    GLPostingRec.posting_credit >= min_amt
                )
            )
            
        if 'amount_max' in options:
            max_amt = Decimal(str(options['amount_max']))
            query = query.filter(
                or_(
                    GLPostingRec.posting_debit <= max_amt,
                    GLPostingRec.posting_credit <= max_amt
                )
            )
            
        # Get transactions
        transactions = query.order_by(
            GLPostingRec.posting_date,
            GLPostingRec.posting_id
        ).all()
        
        # Calculate running balance
        opening_balance = self._get_opening_balance(account, options)
        running_balance = opening_balance
        
        transaction_details = []
        for trans in transactions:
            # Calculate transaction amount
            if trans.posting_debit > 0:
                amount = trans.posting_debit
                trans_type = 'DR'
                if account.ledger_type in [1, 5]:  # Assets & Expenses
                    running_balance += amount
                else:
                    running_balance -= amount
            else:
                amount = trans.posting_credit
                trans_type = 'CR'
                if account.ledger_type in [1, 5]:  # Assets & Expenses
                    running_balance -= amount
                else:
                    running_balance += amount
                    
            transaction_details.append({
                'posting_id': trans.posting_id,
                'date': trans.posting_date,
                'period': trans.posting_period,
                'type': trans.posting_type,
                'reference': trans.posting_reference,
                'description': trans.posting_description,
                'debit': float(trans.posting_debit),
                'credit': float(trans.posting_credit),
                'amount': float(amount),
                'trans_type': trans_type,
                'balance': float(running_balance),
                'analysis_1': trans.posting_analysis_1,
                'analysis_2': trans.posting_analysis_2,
                'analysis_3': trans.posting_analysis_3,
                'batch_no': trans.posting_batch,
                'user': trans.posting_user,
                'status': trans.posting_status
            })
            
        # Summary statistics
        total_debits = sum(t['debit'] for t in transaction_details)
        total_credits = sum(t['credit'] for t in transaction_details)
        transaction_count = len(transaction_details)
        
        # Period activity
        period_activity = self._get_period_activity(account_no, options)
        
        # Analysis by type
        type_analysis = self._analyze_by_transaction_type(transaction_details)
        
        # Analysis by reference
        reference_analysis = self._analyze_by_reference(transaction_details)
        
        result = {
            'account': {
                'number': account.ledger_key,
                'name': account.ledger_name,
                'type': account.ledger_type,
                'active': account.ledger_active == 'Y',
                'currency': account.ledger_currency
            },
            'summary': {
                'opening_balance': float(opening_balance),
                'closing_balance': float(running_balance),
                'total_debits': total_debits,
                'total_credits': total_credits,
                'net_movement': total_debits - total_credits,
                'transaction_count': transaction_count
            },
            'transactions': transaction_details,
            'period_activity': period_activity,
            'type_analysis': type_analysis,
            'reference_analysis': reference_analysis,
            'filters_applied': options
        }
        
        # Log analysis
        if self.current_user:
            log_user_action(
                db=self.db,
                user=self.current_user,
                action="ACCOUNT_ANALYSIS",
                module="GL",
                new_values={
                    'account': account_no,
                    'transactions': transaction_count,
                    'filters': options
                }
            )
            
        return result
        
    def get_account_reconciliation(self, account_no: int, period: Optional[int] = None) -> Dict:
        """
        Get reconciliation data for reconcilable accounts
        """
        # Validate account
        account, status = self.gl_handler.process(4, key_value=account_no)
        if status.fs_reply != "00":
            return {"error": "Account not found"}
            
        if account.ledger_reconcilable != 'Y':
            return {"error": "Account is not marked as reconcilable"}
            
        period = period or self.system_handler.get_current_period()
        
        # Get unreconciled transactions
        unreconciled = self.db.query(GLPostingRec).filter(
            and_(
                GLPostingRec.posting_account == account_no,
                GLPostingRec.posting_status == 'P',
                GLPostingRec.posting_reconciled != 'Y'
            )
        ).order_by(GLPostingRec.posting_date).all()
        
        # Get reconciled transactions for period
        reconciled = self.db.query(GLPostingRec).filter(
            and_(
                GLPostingRec.posting_account == account_no,
                GLPostingRec.posting_status == 'P',
                GLPostingRec.posting_reconciled == 'Y',
                GLPostingRec.posting_recon_period == period
            )
        ).order_by(GLPostingRec.posting_date).all()
        
        # Calculate totals
        unrec_debits = sum(t.posting_debit for t in unreconciled)
        unrec_credits = sum(t.posting_credit for t in unreconciled)
        rec_debits = sum(t.posting_debit for t in reconciled)
        rec_credits = sum(t.posting_credit for t in reconciled)
        
        return {
            'account': {
                'number': account.ledger_key,
                'name': account.ledger_name
            },
            'period': period,
            'unreconciled': {
                'items': [self._format_posting(p) for p in unreconciled],
                'total_debits': float(unrec_debits),
                'total_credits': float(unrec_credits),
                'net': float(unrec_debits - unrec_credits),
                'count': len(unreconciled)
            },
            'reconciled': {
                'items': [self._format_posting(p) for p in reconciled],
                'total_debits': float(rec_debits),
                'total_credits': float(rec_credits),
                'net': float(rec_debits - rec_credits),
                'count': len(reconciled)
            },
            'gl_balance': float(account.ledger_balance),
            'reconciled_balance': float(account.ledger_recon_balance)
        }
        
    def export_account_analysis(self, account_no: int, options: Dict, 
                              format: str = 'excel') -> bytes:
        """
        Export account analysis to Excel/CSV/PDF
        """
        # Get analysis data
        analysis = self.analyze_account(account_no, options)
        
        if 'error' in analysis:
            return None
            
        if format == 'csv':
            import csv
            import io
            
            output = io.StringIO()
            writer = csv.writer(output)
            
            # Header info
            writer.writerow(['Account Analysis Report'])
            writer.writerow([f"Account: {analysis['account']['number']} - {analysis['account']['name']}"])
            writer.writerow([f"Generated: {datetime.now().strftime('%Y-%m-%d %H:%M')}"])
            writer.writerow([])
            
            # Column headers
            writer.writerow([
                'Date', 'Reference', 'Description', 
                'Debit', 'Credit', 'Balance', 
                'Analysis 1', 'Analysis 2', 'Analysis 3'
            ])
            
            # Transactions
            for trans in analysis['transactions']:
                writer.writerow([
                    trans['date'],
                    trans['reference'],
                    trans['description'],
                    trans['debit'] if trans['debit'] > 0 else '',
                    trans['credit'] if trans['credit'] > 0 else '',
                    trans['balance'],
                    trans['analysis_1'],
                    trans['analysis_2'],
                    trans['analysis_3']
                ])
                
            return output.getvalue().encode('utf-8')
            
        elif format == 'excel':
            # Would use openpyxl for Excel generation
            pass
            
        return b''
        
    def _get_opening_balance(self, account: GLLedgerRec, options: Dict) -> Decimal:
        """Calculate opening balance based on filters"""
        if 'date_from' in options:
            # Calculate balance up to date_from
            prior_trans = self.db.query(
                func.sum(GLPostingRec.posting_debit).label('total_dr'),
                func.sum(GLPostingRec.posting_credit).label('total_cr')
            ).filter(
                and_(
                    GLPostingRec.posting_account == account.ledger_key,
                    GLPostingRec.posting_date < options['date_from'],
                    GLPostingRec.posting_status == 'P'
                )
            ).first()
            
            if prior_trans and (prior_trans.total_dr or prior_trans.total_cr):
                total_dr = prior_trans.total_dr or Decimal('0')
                total_cr = prior_trans.total_cr or Decimal('0')
                
                if account.ledger_type in [1, 5]:  # Assets & Expenses
                    return total_dr - total_cr
                else:
                    return total_cr - total_dr
                    
        return Decimal('0')
        
    def _get_period_activity(self, account_no: int, options: Dict) -> List[Dict]:
        """Get activity summary by period"""
        # Build period range
        system_rec, _ = self.system_handler.read_system_params()
        current_period = self.system_handler.get_current_period()
        
        period_from = options.get('period_from', 1)
        period_to = options.get('period_to', current_period)
        
        activity = []
        for period in range(period_from, period_to + 1):
            period_trans = self.db.query(
                func.count(GLPostingRec.posting_id).label('count'),
                func.sum(GLPostingRec.posting_debit).label('total_dr'),
                func.sum(GLPostingRec.posting_credit).label('total_cr')
            ).filter(
                and_(
                    GLPostingRec.posting_account == account_no,
                    GLPostingRec.posting_period == period,
                    GLPostingRec.posting_status == 'P'
                )
            ).first()
            
            if period_trans and period_trans.count > 0:
                activity.append({
                    'period': period,
                    'transaction_count': period_trans.count,
                    'total_debits': float(period_trans.total_dr or 0),
                    'total_credits': float(period_trans.total_cr or 0),
                    'net_movement': float((period_trans.total_dr or 0) - (period_trans.total_cr or 0))
                })
                
        return activity
        
    def _analyze_by_transaction_type(self, transactions: List[Dict]) -> Dict:
        """Analyze transactions by type"""
        type_summary = {}
        
        for trans in transactions:
            trans_type = trans['type']
            if trans_type not in type_summary:
                type_summary[trans_type] = {
                    'count': 0,
                    'total_debits': 0.0,
                    'total_credits': 0.0
                }
                
            type_summary[trans_type]['count'] += 1
            type_summary[trans_type]['total_debits'] += trans['debit']
            type_summary[trans_type]['total_credits'] += trans['credit']
            
        return type_summary
        
    def _analyze_by_reference(self, transactions: List[Dict]) -> List[Dict]:
        """Analyze transactions by reference pattern"""
        ref_patterns = {}
        
        for trans in transactions:
            ref = trans['reference']
            if ref:
                # Extract prefix (first word or pattern)
                prefix = ref.split('-')[0].split(' ')[0]
                if prefix not in ref_patterns:
                    ref_patterns[prefix] = {
                        'pattern': prefix,
                        'count': 0,
                        'total_debits': 0.0,
                        'total_credits': 0.0
                    }
                    
                ref_patterns[prefix]['count'] += 1
                ref_patterns[prefix]['total_debits'] += trans['debit']
                ref_patterns[prefix]['total_credits'] += trans['credit']
                
        # Sort by count descending
        return sorted(ref_patterns.values(), key=lambda x: x['count'], reverse=True)[:10]
        
    def _format_posting(self, posting: GLPostingRec) -> Dict:
        """Format posting record for output"""
        return {
            'posting_id': posting.posting_id,
            'date': posting.posting_date,
            'reference': posting.posting_reference,
            'description': posting.posting_description,
            'debit': float(posting.posting_debit),
            'credit': float(posting.posting_credit),
            'batch_no': posting.posting_batch,
            'reconciled': posting.posting_reconciled == 'Y',
            'recon_date': posting.posting_recon_date if posting.posting_reconciled == 'Y' else None
        }
        
    def get_drill_down(self, account_no: int, period: int) -> Dict:
        """
        Drill down from trial balance to transaction detail
        """
        # Get account
        account, status = self.gl_handler.process(4, key_value=account_no)
        if status.fs_reply != "00":
            return {"error": "Account not found"}
            
        # Get period balance from account record
        period_field = f"ledger_period_{period}"
        period_balance = getattr(account, period_field, Decimal('0')) if hasattr(account, period_field) else Decimal('0')
        
        # Get transactions for period
        transactions = self.db.query(GLPostingRec).filter(
            and_(
                GLPostingRec.posting_account == account_no,
                GLPostingRec.posting_period == period,
                GLPostingRec.posting_status == 'P'
            )
        ).order_by(GLPostingRec.posting_date).all()
        
        # Calculate totals
        total_dr = sum(t.posting_debit for t in transactions)
        total_cr = sum(t.posting_credit for t in transactions)
        
        return {
            'account': {
                'number': account.ledger_key,
                'name': account.ledger_name
            },
            'period': period,
            'period_balance': float(period_balance),
            'transactions': [self._format_posting(t) for t in transactions],
            'totals': {
                'debits': float(total_dr),
                'credits': float(total_cr),
                'net': float(total_dr - total_cr),
                'count': len(transactions)
            }
        }