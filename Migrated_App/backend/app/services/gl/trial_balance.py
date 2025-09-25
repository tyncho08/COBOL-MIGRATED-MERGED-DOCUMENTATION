"""
Trial Balance Service - GL030 migration
Generates trial balance reports with various formats
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from datetime import datetime
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func

from app.services.file_handlers.gl_handler import GLFileHandler
from app.services.file_handlers.system_handler import SystemFileHandler
from app.models.gl_accounts import GLLedgerRec
from app.core.security import log_user_action
from app.models.auth import User


class TrialBalanceService:
    """
    Trial Balance generation
    Implements GL030 functionality with multiple report formats
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.gl_handler = GLFileHandler(db)
        self.system_handler = SystemFileHandler(db)
        
    def generate_trial_balance(self, options: Optional[Dict] = None) -> Dict:
        """
        Generate trial balance with options
        Options:
        - period: accounting period (default: current)
        - format: 'summary', 'detailed', 'comparative'
        - include_zero: include zero balance accounts
        - group_by_type: group by account type
        """
        options = options or {}
        period = options.get('period', self.system_handler.get_current_period())
        format_type = options.get('format', 'detailed')
        include_zero = options.get('include_zero', False)
        group_by_type = options.get('group_by_type', False)
        
        # Get accounts
        accounts = self.db.query(GLLedgerRec).filter(
            GLLedgerRec.ledger_active == 'Y'
        ).order_by(GLLedgerRec.ledger_key).all()
        
        # Build trial balance data
        tb_data = []
        totals = {
            'debit': Decimal('0'),
            'credit': Decimal('0')
        }
        
        for account in accounts:
            balance = account.ledger_balance
            
            # Skip zero balances if requested
            if not include_zero and balance == 0:
                continue
                
            # Determine debit/credit based on account type
            debit = Decimal('0')
            credit = Decimal('0')
            
            if account.ledger_type in [1, 5]:  # Assets and Expenses
                if balance > 0:
                    debit = balance
                else:
                    credit = abs(balance)
            else:  # Liabilities, Capital, Income
                if balance > 0:
                    credit = balance
                else:
                    debit = abs(balance)
                    
            entry = {
                'account': account.ledger_key,
                'name': account.ledger_name,
                'type': account.ledger_type,
                'type_name': self._get_account_type_name(account.ledger_type),
                'level': account.ledger_level,
                'debit': debit,
                'credit': credit,
                'balance': balance
            }
            
            tb_data.append(entry)
            totals['debit'] += debit
            totals['credit'] += credit
            
        # Apply formatting
        if format_type == 'summary':
            tb_data = self._summarize_by_type(tb_data)
        elif format_type == 'comparative':
            tb_data = self._add_comparative_data(tb_data, period)
            
        # Group by type if requested
        if group_by_type:
            tb_data = self._group_by_account_type(tb_data)
            
        # Check if balanced
        is_balanced = abs(totals['debit'] - totals['credit']) < Decimal('0.01')
        
        # Log report generation
        if self.current_user:
            log_user_action(
                db=self.db,
                user=self.current_user,
                action="GENERATE_TRIAL_BALANCE",
                module="GL",
                new_values={
                    'period': period,
                    'format': format_type,
                    'accounts': len(tb_data)
                }
            )
            
        return {
            'period': period,
            'generated_date': datetime.now().strftime("%Y-%m-%d"),
            'format': format_type,
            'entries': tb_data,
            'totals': {
                'debit': float(totals['debit']),
                'credit': float(totals['credit'])
            },
            'is_balanced': is_balanced,
            'difference': float(abs(totals['debit'] - totals['credit']))
        }
        
    def generate_working_trial_balance(self, period: Optional[int] = None) -> Dict:
        """
        Generate working trial balance with adjustments columns
        Used for period-end processing
        """
        period = period or self.system_handler.get_current_period()
        
        # Get trial balance
        tb_result = self.generate_trial_balance({
            'period': period,
            'format': 'detailed',
            'include_zero': True
        })
        
        # Add adjustment columns
        entries_with_adj = []
        for entry in tb_result['entries']:
            extended_entry = entry.copy()
            extended_entry.update({
                'adjustments_debit': Decimal('0'),
                'adjustments_credit': Decimal('0'),
                'adjusted_debit': entry['debit'],
                'adjusted_credit': entry['credit']
            })
            entries_with_adj.append(extended_entry)
            
        tb_result['entries'] = entries_with_adj
        tb_result['has_adjustments'] = False
        
        return tb_result
        
    def export_trial_balance(self, tb_data: Dict, export_format: str = 'csv') -> bytes:
        """
        Export trial balance to various formats
        Supports: csv, excel, pdf
        """
        import io
        import csv
        
        if export_format == 'csv':
            output = io.StringIO()
            writer = csv.writer(output)
            
            # Headers
            writer.writerow([
                'Account', 'Description', 'Type', 
                'Debit', 'Credit', 'Balance'
            ])
            
            # Data
            for entry in tb_data['entries']:
                writer.writerow([
                    entry['account'],
                    entry['name'],
                    entry['type_name'],
                    entry['debit'],
                    entry['credit'],
                    entry['balance']
                ])
                
            # Totals
            writer.writerow([])
            writer.writerow([
                '', 'TOTALS', '',
                tb_data['totals']['debit'],
                tb_data['totals']['credit'],
                ''
            ])
            
            return output.getvalue().encode('utf-8')
            
        elif export_format == 'excel':
            # Would use openpyxl here
            pass
            
        elif export_format == 'pdf':
            # Would use reportlab here
            pass
            
        return b''
        
    def _get_account_type_name(self, account_type: int) -> str:
        """Get readable name for account type"""
        type_names = {
            1: 'Asset',
            2: 'Liability',
            3: 'Capital',
            4: 'Income',
            5: 'Expense'
        }
        return type_names.get(account_type, 'Unknown')
        
    def _summarize_by_type(self, tb_data: List[Dict]) -> List[Dict]:
        """Summarize trial balance by account type"""
        summary = {}
        
        for entry in tb_data:
            type_key = entry['type']
            if type_key not in summary:
                summary[type_key] = {
                    'account': f"{type_key}0000000",
                    'name': entry['type_name'],
                    'type': type_key,
                    'type_name': entry['type_name'],
                    'level': 1,
                    'debit': Decimal('0'),
                    'credit': Decimal('0'),
                    'balance': Decimal('0')
                }
                
            summary[type_key]['debit'] += entry['debit']
            summary[type_key]['credit'] += entry['credit']
            summary[type_key]['balance'] += entry['balance']
            
        return list(summary.values())
        
    def _add_comparative_data(self, tb_data: List[Dict], current_period: int) -> List[Dict]:
        """Add comparative data from previous period"""
        prev_period = current_period - 1 if current_period > 1 else 13
        
        for entry in tb_data:
            account = self.db.query(GLLedgerRec).filter(
                GLLedgerRec.ledger_key == entry['account']
            ).first()
            
            if account:
                # Get previous period balance
                period_field = f"ledger_period_{prev_period}"
                if hasattr(account, period_field):
                    prev_balance = getattr(account, period_field)
                else:
                    prev_balance = Decimal('0')
                    
                entry['prev_period_balance'] = float(prev_balance)
                entry['variance'] = float(entry['balance'] - prev_balance)
                entry['variance_pct'] = (
                    float((entry['balance'] - prev_balance) / prev_balance * 100)
                    if prev_balance != 0 else 0
                )
                
        return tb_data
        
    def _group_by_account_type(self, tb_data: List[Dict]) -> List[Dict]:
        """Group accounts by type with subtotals"""
        grouped = []
        current_type = None
        type_totals = {
            'debit': Decimal('0'),
            'credit': Decimal('0')
        }
        
        for entry in sorted(tb_data, key=lambda x: (x['type'], x['account'])):
            # Add type header and subtotal
            if current_type != entry['type']:
                if current_type is not None:
                    # Add subtotal for previous type
                    grouped.append({
                        'account': '',
                        'name': f"Total {self._get_account_type_name(current_type)}",
                        'type': current_type,
                        'is_subtotal': True,
                        'debit': type_totals['debit'],
                        'credit': type_totals['credit']
                    })
                    grouped.append({})  # Blank line
                    
                current_type = entry['type']
                type_totals = {
                    'debit': Decimal('0'),
                    'credit': Decimal('0')
                }
                
                # Add type header
                grouped.append({
                    'account': '',
                    'name': entry['type_name'].upper(),
                    'type': current_type,
                    'is_header': True
                })
                
            grouped.append(entry)
            type_totals['debit'] += entry['debit']
            type_totals['credit'] += entry['credit']
            
        # Add final subtotal
        if current_type is not None:
            grouped.append({
                'account': '',
                'name': f"Total {self._get_account_type_name(current_type)}",
                'type': current_type,
                'is_subtotal': True,
                'debit': type_totals['debit'],
                'credit': type_totals['credit']
            })
            
        return grouped
        
    def check_trial_balance_integrity(self) -> List[str]:
        """
        Check trial balance integrity
        Returns list of issues found
        """
        issues = []
        
        # Generate trial balance
        tb_result = self.generate_trial_balance({
            'include_zero': True
        })
        
        # Check if balanced
        if not tb_result['is_balanced']:
            issues.append(
                f"Trial balance out of balance by {tb_result['difference']}"
            )
            
        # Check control accounts match subledgers
        system_rec, _ = self.system_handler.read_system_params()
        if system_rec:
            # Check debtors control
            if system_rec.s_debtors > 0:
                control_balance = self._get_account_balance(system_rec.s_debtors)
                subledger_total = self._get_sales_ledger_total()
                
                if abs(control_balance - subledger_total) > Decimal('0.01'):
                    issues.append(
                        f"Sales control account {control_balance} does not match "
                        f"subledger total {subledger_total}"
                    )
                    
            # Check creditors control
            if system_rec.p_creditors > 0:
                control_balance = self._get_account_balance(system_rec.p_creditors)
                subledger_total = self._get_purchase_ledger_total()
                
                if abs(control_balance - subledger_total) > Decimal('0.01'):
                    issues.append(
                        f"Purchase control account {control_balance} does not match "
                        f"subledger total {subledger_total}"
                    )
                    
        return issues
        
    def _get_account_balance(self, account_no: int) -> Decimal:
        """Get current balance for an account"""
        account = self.db.query(GLLedgerRec).filter(
            GLLedgerRec.ledger_key == account_no
        ).first()
        
        return account.ledger_balance if account else Decimal('0')
        
    def _get_sales_ledger_total(self) -> Decimal:
        """Get total of all customer balances"""
        from app.models.customer import SalesLedgerRec
        
        result = self.db.query(
            func.sum(SalesLedgerRec.sales_balance)
        ).scalar()
        
        return Decimal(str(result)) if result else Decimal('0')
        
    def _get_purchase_ledger_total(self) -> Decimal:
        """Get total of all supplier balances"""
        from app.models.supplier import PurchaseLedgerRec
        
        result = self.db.query(
            func.sum(PurchaseLedgerRec.purch_balance)
        ).scalar()
        
        return Decimal(str(result)) if result else Decimal('0')