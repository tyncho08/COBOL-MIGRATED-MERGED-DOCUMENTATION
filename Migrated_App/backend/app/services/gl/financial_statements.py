"""
Financial Statements Service - GL050/GL051/GL060 migration
Generates P&L and Balance Sheet reports
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from datetime import datetime
from sqlalchemy.orm import Session
from sqlalchemy import and_, func

from app.services.file_handlers.gl_handler import GLFileHandler
from app.services.file_handlers.system_handler import SystemFileHandler
from app.models.gl_accounts import GLLedgerRec
from app.core.security import log_user_action
from app.models.auth import User


class FinancialStatementsService:
    """
    Financial Statements generation
    Implements:
    - GL050: Profit & Loss Statement
    - GL051: Comparative P&L
    - GL060: Balance Sheet
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.gl_handler = GLFileHandler(db)
        self.system_handler = SystemFileHandler(db)
        
    def generate_profit_loss(self, options: Optional[Dict] = None) -> Dict:
        """
        Generate Profit & Loss Statement (GL050)
        Options:
        - period: single period or range
        - format: 'standard', 'detailed', 'summarized'
        - include_percentages: show % of revenue
        - comparative: include comparative period
        """
        options = options or {}
        period = options.get('period', self.system_handler.get_current_period())
        format_type = options.get('format', 'standard')
        include_pct = options.get('include_percentages', True)
        comparative = options.get('comparative', False)
        
        # Get P&L accounts
        pl_accounts = self.db.query(GLLedgerRec).filter(
            GLLedgerRec.ledger_place == 'P',
            GLLedgerRec.ledger_active == 'Y'
        ).order_by(GLLedgerRec.ledger_type, GLLedgerRec.ledger_key).all()
        
        # Build P&L structure
        revenue_accounts = []
        cos_accounts = []  # Cost of Sales
        expense_accounts = []
        
        total_revenue = Decimal('0')
        total_cos = Decimal('0')
        total_expenses = Decimal('0')
        
        for account in pl_accounts:
            if account.ledger_type == 4:  # Revenue
                balance = abs(account.ledger_balance)  # Revenue is credit, show as positive
                revenue_accounts.append({
                    'account': account.ledger_key,
                    'name': account.ledger_name,
                    'balance': balance,
                    'level': account.ledger_level
                })
                total_revenue += balance
                
            elif account.ledger_type == 5:  # Expenses
                balance = abs(account.ledger_balance)
                
                # Separate Cost of Sales from Operating Expenses
                if self._is_cost_of_sales(account.ledger_key):
                    cos_accounts.append({
                        'account': account.ledger_key,
                        'name': account.ledger_name,
                        'balance': balance,
                        'level': account.ledger_level
                    })
                    total_cos += balance
                else:
                    expense_accounts.append({
                        'account': account.ledger_key,
                        'name': account.ledger_name,
                        'balance': balance,
                        'level': account.ledger_level
                    })
                    total_expenses += balance
                    
        # Calculate metrics
        gross_profit = total_revenue - total_cos
        net_profit = gross_profit - total_expenses
        
        # Add percentages if requested
        if include_pct and total_revenue > 0:
            for acc in revenue_accounts:
                acc['percentage'] = float(acc['balance'] / total_revenue * 100)
            for acc in cos_accounts:
                acc['percentage'] = float(acc['balance'] / total_revenue * 100)
            for acc in expense_accounts:
                acc['percentage'] = float(acc['balance'] / total_revenue * 100)
                
        # Format based on type
        if format_type == 'summarized':
            revenue_accounts = self._summarize_accounts(revenue_accounts)
            cos_accounts = self._summarize_accounts(cos_accounts)
            expense_accounts = self._summarize_accounts(expense_accounts)
            
        # Add comparative if requested
        if comparative:
            self._add_comparative_pl_data(
                revenue_accounts + cos_accounts + expense_accounts,
                period
            )
            
        # Build result
        result = {
            'period': period,
            'generated_date': datetime.now().strftime("%Y-%m-%d"),
            'format': format_type,
            'revenue': {
                'accounts': revenue_accounts,
                'total': float(total_revenue)
            },
            'cost_of_sales': {
                'accounts': cos_accounts,
                'total': float(total_cos)
            },
            'gross_profit': float(gross_profit),
            'gross_profit_margin': float(gross_profit / total_revenue * 100) if total_revenue > 0 else 0,
            'operating_expenses': {
                'accounts': expense_accounts,
                'total': float(total_expenses)
            },
            'net_profit': float(net_profit),
            'net_profit_margin': float(net_profit / total_revenue * 100) if total_revenue > 0 else 0
        }
        
        # Log generation
        if self.current_user:
            log_user_action(
                db=self.db,
                user=self.current_user,
                action="GENERATE_P&L",
                module="GL",
                new_values={
                    'period': period,
                    'format': format_type,
                    'net_profit': float(net_profit)
                }
            )
            
        return result
        
    def generate_balance_sheet(self, options: Optional[Dict] = None) -> Dict:
        """
        Generate Balance Sheet (GL060)
        Options:
        - as_of_date: balance sheet date
        - format: 'standard', 'detailed', 'condensed'
        - include_notes: include account notes
        """
        options = options or {}
        as_of_date = options.get('as_of_date', datetime.now().strftime("%Y%m%d"))
        format_type = options.get('format', 'standard')
        
        # Get Balance Sheet accounts
        bs_accounts = self.db.query(GLLedgerRec).filter(
            GLLedgerRec.ledger_place == 'B',
            GLLedgerRec.ledger_active == 'Y'
        ).order_by(GLLedgerRec.ledger_type, GLLedgerRec.ledger_key).all()
        
        # Build Balance Sheet structure
        assets = {
            'current': [],
            'non_current': [],
            'total': Decimal('0')
        }
        
        liabilities = {
            'current': [],
            'non_current': [],
            'total': Decimal('0')
        }
        
        equity = {
            'accounts': [],
            'total': Decimal('0')
        }
        
        # Get current period P&L for retained earnings
        pl_result = self.generate_profit_loss()
        current_year_earnings = Decimal(str(pl_result['net_profit']))
        
        for account in bs_accounts:
            account_data = {
                'account': account.ledger_key,
                'name': account.ledger_name,
                'balance': abs(account.ledger_balance),
                'level': account.ledger_level
            }
            
            if account.ledger_type == 1:  # Assets
                if self._is_current_asset(account.ledger_key):
                    assets['current'].append(account_data)
                else:
                    assets['non_current'].append(account_data)
                assets['total'] += account_data['balance']
                
            elif account.ledger_type == 2:  # Liabilities
                if self._is_current_liability(account.ledger_key):
                    liabilities['current'].append(account_data)
                else:
                    liabilities['non_current'].append(account_data)
                liabilities['total'] += account_data['balance']
                
            elif account.ledger_type == 3:  # Capital/Equity
                # Add current year earnings to retained earnings account
                if self._is_retained_earnings(account.ledger_key):
                    account_data['balance'] += current_year_earnings
                    account_data['includes_current_earnings'] = True
                    
                equity['accounts'].append(account_data)
                equity['total'] += account_data['balance']
                
        # Calculate totals
        total_assets = assets['total']
        total_liabilities = liabilities['total']
        total_equity = equity['total']
        
        # Verify balance
        is_balanced = abs(total_assets - (total_liabilities + total_equity)) < Decimal('0.01')
        
        # Format based on type
        if format_type == 'condensed':
            assets['current'] = self._summarize_accounts(assets['current'])
            assets['non_current'] = self._summarize_accounts(assets['non_current'])
            liabilities['current'] = self._summarize_accounts(liabilities['current'])
            liabilities['non_current'] = self._summarize_accounts(liabilities['non_current'])
            
        # Build result
        result = {
            'as_of_date': as_of_date,
            'generated_date': datetime.now().strftime("%Y-%m-%d"),
            'format': format_type,
            'assets': {
                'current': {
                    'accounts': assets['current'],
                    'total': float(sum(a['balance'] for a in assets['current']))
                },
                'non_current': {
                    'accounts': assets['non_current'],
                    'total': float(sum(a['balance'] for a in assets['non_current']))
                },
                'total': float(assets['total'])
            },
            'liabilities': {
                'current': {
                    'accounts': liabilities['current'],
                    'total': float(sum(a['balance'] for a in liabilities['current']))
                },
                'non_current': {
                    'accounts': liabilities['non_current'],
                    'total': float(sum(a['balance'] for a in liabilities['non_current']))
                },
                'total': float(liabilities['total'])
            },
            'equity': {
                'accounts': equity['accounts'],
                'total': float(equity['total'])
            },
            'is_balanced': is_balanced,
            'current_ratio': float(
                sum(a['balance'] for a in assets['current']) / 
                sum(l['balance'] for l in liabilities['current'])
            ) if liabilities['current'] else 0,
            'debt_to_equity': float(liabilities['total'] / equity['total']) if equity['total'] > 0 else 0
        }
        
        # Log generation
        if self.current_user:
            log_user_action(
                db=self.db,
                user=self.current_user,
                action="GENERATE_BALANCE_SHEET",
                module="GL",
                new_values={
                    'as_of_date': as_of_date,
                    'format': format_type,
                    'total_assets': float(total_assets)
                }
            )
            
        return result
        
    def generate_cash_flow_statement(self, options: Optional[Dict] = None) -> Dict:
        """
        Generate Cash Flow Statement
        Uses indirect method
        """
        options = options or {}
        period = options.get('period', self.system_handler.get_current_period())
        
        # Get P&L for the period
        pl_result = self.generate_profit_loss({'period': period})
        net_profit = Decimal(str(pl_result['net_profit']))
        
        # This is simplified - full implementation would track actual cash movements
        cash_flow = {
            'operating_activities': {
                'net_profit': float(net_profit),
                'adjustments': {
                    'depreciation': 0.0,
                    'working_capital_changes': {
                        'receivables': 0.0,
                        'inventory': 0.0,
                        'payables': 0.0
                    }
                },
                'total': float(net_profit)
            },
            'investing_activities': {
                'items': [],
                'total': 0.0
            },
            'financing_activities': {
                'items': [],
                'total': 0.0
            },
            'net_cash_flow': float(net_profit),
            'opening_cash': 0.0,
            'closing_cash': float(net_profit)
        }
        
        return cash_flow
        
    def _is_cost_of_sales(self, account_no: int) -> bool:
        """Determine if account is Cost of Sales"""
        # Simplified logic - would use configuration
        return 50000000 <= account_no < 60000000
        
    def _is_current_asset(self, account_no: int) -> bool:
        """Determine if account is current asset"""
        # Cash, receivables, inventory
        return account_no < 15000000
        
    def _is_current_liability(self, account_no: int) -> bool:
        """Determine if account is current liability"""
        # Payables, short-term debt
        return account_no < 25000000
        
    def _is_retained_earnings(self, account_no: int) -> bool:
        """Check if account is retained earnings"""
        # Standard retained earnings account
        return account_no == 30020000
        
    def _summarize_accounts(self, accounts: List[Dict]) -> List[Dict]:
        """Summarize accounts at higher level"""
        summary = {}
        
        for account in accounts:
            # Group by first 4 digits
            group_key = account['account'] // 10000
            group_name = account['name'].split('-')[0].strip()
            
            if group_key not in summary:
                summary[group_key] = {
                    'account': group_key * 10000,
                    'name': group_name,
                    'balance': Decimal('0'),
                    'level': 2
                }
                
            summary[group_key]['balance'] += account['balance']
            
        return list(summary.values())
        
    def _add_comparative_pl_data(self, accounts: List[Dict], current_period: int):
        """Add comparative P&L data"""
        # Get previous year same period
        for account in accounts:
            gl_account = self.db.query(GLLedgerRec).filter(
                GLLedgerRec.ledger_key == account['account']
            ).first()
            
            if gl_account:
                prev_balance = gl_account.ledger_last_year_actual
                account['prior_year'] = float(abs(prev_balance))
                account['variance'] = account['balance'] - float(abs(prev_balance))
                account['variance_pct'] = (
                    account['variance'] / float(abs(prev_balance)) * 100
                    if prev_balance != 0 else 0
                )
                
    def export_financial_statements(self, statement_type: str, data: Dict, 
                                   export_format: str = 'pdf') -> bytes:
        """Export financial statements to various formats"""
        # This would implement PDF generation using reportlab
        # or Excel generation using openpyxl
        return b''