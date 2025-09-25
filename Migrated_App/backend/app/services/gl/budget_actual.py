"""
Budget vs Actual Service - GL090 migration
Handles budget management and variance analysis
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from datetime import datetime
from sqlalchemy.orm import Session
from sqlalchemy import and_, func

from app.services.file_handlers.gl_handler import GLFileHandler
from app.services.file_handlers.system_handler import SystemFileHandler
from app.models.gl_accounts import GLLedgerRec, GLBudgetRec
from app.core.security import log_user_action
from app.models.auth import User


class BudgetActualService:
    """
    Budget vs Actual analysis
    Implements GL090 functionality
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.gl_handler = GLFileHandler(db)
        self.system_handler = SystemFileHandler(db)
        
    def create_budget(self, budget_data: Dict) -> Tuple[GLBudgetRec, Optional[str]]:
        """
        Create new budget
        Returns (budget_record, error_message)
        """
        # Validate account
        account_no = budget_data.get('budget_account', 0)
        account, status = self.gl_handler.process(4, key_value=account_no)
        if status.fs_reply != "00":
            return None, f"Account {account_no} not found"
            
        # Check if budget already exists
        existing = self.db.query(GLBudgetRec).filter(
            and_(
                GLBudgetRec.budget_account == account_no,
                GLBudgetRec.budget_year == budget_data.get('budget_year', 0),
                GLBudgetRec.budget_version == budget_data.get('budget_version', 1)
            )
        ).first()
        
        if existing:
            return None, "Budget already exists for this account/year/version"
            
        # Create budget record
        budget = GLBudgetRec(
            budget_account=account_no,
            budget_year=budget_data.get('budget_year'),
            budget_version=budget_data.get('budget_version', 1),
            budget_type=budget_data.get('budget_type', 'A'),  # Annual
            budget_status='A',  # Active
            budget_created_by=self.current_user.username if self.current_user else 'SYSTEM',
            budget_created_date=int(datetime.now().strftime("%Y%m%d"))
        )
        
        # Set period budgets
        total_budget = Decimal('0')
        for period in range(1, 14):  # Periods 1-13
            period_field = f"budget_period_{period}"
            period_value = Decimal(str(budget_data.get(f'period_{period}', 0)))
            setattr(budget, period_field, period_value)
            total_budget += period_value
            
        budget.budget_total = total_budget
        
        # Calculate distribution method
        if budget_data.get('distribution_method'):
            self._apply_distribution_method(budget, budget_data)
            
        self.db.add(budget)
        self.db.flush()
        
        # Log creation
        if self.current_user:
            log_user_action(
                db=self.db,
                user=self.current_user,
                action="CREATE_BUDGET",
                module="GL",
                new_values={
                    'account': account_no,
                    'year': budget.budget_year,
                    'total': float(budget.budget_total)
                }
            )
            
        return budget, None
        
    def update_budget(self, budget_id: int, updates: Dict) -> Tuple[GLBudgetRec, Optional[str]]:
        """Update existing budget"""
        budget = self.db.query(GLBudgetRec).filter(
            GLBudgetRec.budget_id == budget_id
        ).first()
        
        if not budget:
            return None, "Budget not found"
            
        if budget.budget_status == 'L':  # Locked
            return None, "Cannot update locked budget"
            
        # Update period values
        total_budget = Decimal('0')
        for period in range(1, 14):
            if f'period_{period}' in updates:
                period_field = f"budget_period_{period}"
                period_value = Decimal(str(updates[f'period_{period}']))
                setattr(budget, period_field, period_value)
                total_budget += period_value
            else:
                # Keep existing value
                period_field = f"budget_period_{period}"
                total_budget += getattr(budget, period_field)
                
        budget.budget_total = total_budget
        budget.budget_updated_by = self.current_user.username if self.current_user else 'SYSTEM'
        budget.budget_updated_date = int(datetime.now().strftime("%Y%m%d"))
        
        self.db.flush()
        
        # Log update
        if self.current_user:
            log_user_action(
                db=self.db,
                user=self.current_user,
                action="UPDATE_BUDGET",
                module="GL",
                new_values={
                    'budget_id': budget_id,
                    'total': float(budget.budget_total)
                }
            )
            
        return budget, None
        
    def copy_budget(self, source_year: int, target_year: int, 
                   adjustment_pct: Optional[float] = None,
                   version: int = 1) -> Tuple[int, Optional[str]]:
        """
        Copy budget from one year to another with optional adjustment
        Returns (created_count, error_message)
        """
        # Get source budgets
        source_budgets = self.db.query(GLBudgetRec).filter(
            and_(
                GLBudgetRec.budget_year == source_year,
                GLBudgetRec.budget_version == 1  # Copy from version 1
            )
        ).all()
        
        if not source_budgets:
            return 0, "No budgets found for source year"
            
        created = 0
        adjustment = Decimal(str(1 + (adjustment_pct or 0) / 100))
        
        for source in source_budgets:
            # Check if target exists
            existing = self.db.query(GLBudgetRec).filter(
                and_(
                    GLBudgetRec.budget_account == source.budget_account,
                    GLBudgetRec.budget_year == target_year,
                    GLBudgetRec.budget_version == version
                )
            ).first()
            
            if existing:
                continue
                
            # Create new budget
            target_budget = GLBudgetRec(
                budget_account=source.budget_account,
                budget_year=target_year,
                budget_version=version,
                budget_type=source.budget_type,
                budget_status='A',
                budget_created_by=self.current_user.username if self.current_user else 'SYSTEM',
                budget_created_date=int(datetime.now().strftime("%Y%m%d"))
            )
            
            # Copy and adjust period values
            total = Decimal('0')
            for period in range(1, 14):
                source_field = f"budget_period_{period}"
                source_value = getattr(source, source_field)
                adjusted_value = source_value * adjustment
                setattr(target_budget, source_field, adjusted_value)
                total += adjusted_value
                
            target_budget.budget_total = total
            
            self.db.add(target_budget)
            created += 1
            
        self.db.flush()
        
        # Log copy operation
        if self.current_user:
            log_user_action(
                db=self.db,
                user=self.current_user,
                action="COPY_BUDGET",
                module="GL",
                new_values={
                    'source_year': source_year,
                    'target_year': target_year,
                    'adjustment_pct': adjustment_pct,
                    'created_count': created
                }
            )
            
        return created, None
        
    def generate_variance_report(self, options: Optional[Dict] = None) -> Dict:
        """
        Generate budget vs actual variance report
        Options:
        - year: budget year
        - period_from/period_to: period range
        - account_type: filter by account type
        - variance_threshold: only show variances above threshold %
        - include_zero_budget: include accounts without budget
        """
        options = options or {}
        year = options.get('year', datetime.now().year)
        period_from = options.get('period_from', 1)
        period_to = options.get('period_to', self.system_handler.get_current_period())
        variance_threshold = options.get('variance_threshold', 0)
        
        # Get accounts with budgets
        query = self.db.query(
            GLLedgerRec,
            GLBudgetRec
        ).outerjoin(
            GLBudgetRec,
            and_(
                GLLedgerRec.ledger_key == GLBudgetRec.budget_account,
                GLBudgetRec.budget_year == year,
                GLBudgetRec.budget_version == 1,
                GLBudgetRec.budget_status == 'A'
            )
        ).filter(
            GLLedgerRec.ledger_active == 'Y'
        )
        
        # Filter by account type if specified
        if 'account_type' in options:
            query = query.filter(GLLedgerRec.ledger_type == options['account_type'])
            
        # Filter to include only P&L accounts
        query = query.filter(GLLedgerRec.ledger_place == 'P')
        
        # Execute query
        results = query.all()
        
        variance_data = []
        total_budget = Decimal('0')
        total_actual = Decimal('0')
        
        for account, budget in results:
            # Skip if no budget and not including zero budget
            if not budget and not options.get('include_zero_budget', False):
                continue
                
            # Calculate budget for period range
            period_budget = Decimal('0')
            if budget:
                for period in range(period_from, period_to + 1):
                    period_field = f"budget_period_{period}"
                    period_budget += getattr(budget, period_field, Decimal('0'))
                    
            # Calculate actual for period range
            period_actual = Decimal('0')
            for period in range(period_from, period_to + 1):
                period_field = f"ledger_period_{period}"
                if hasattr(account, period_field):
                    period_actual += abs(getattr(account, period_field, Decimal('0')))
                    
            # Calculate variance
            if period_budget > 0:
                variance_amt = period_actual - period_budget
                variance_pct = (variance_amt / period_budget * 100)
            else:
                variance_amt = period_actual
                variance_pct = 100 if period_actual > 0 else 0
                
            # Apply threshold filter
            if variance_threshold > 0 and abs(variance_pct) < variance_threshold:
                continue
                
            variance_data.append({
                'account': account.ledger_key,
                'name': account.ledger_name,
                'type': account.ledger_type,
                'budget': float(period_budget),
                'actual': float(period_actual),
                'variance': float(variance_amt),
                'variance_pct': float(variance_pct),
                'favorable': variance_amt < 0 if account.ledger_type == 5 else variance_amt > 0
            })
            
            total_budget += period_budget
            total_actual += period_actual
            
        # Sort by variance percentage
        variance_data.sort(key=lambda x: abs(x['variance_pct']), reverse=True)
        
        # Summary by account type
        type_summary = self._summarize_by_type(variance_data)
        
        result = {
            'year': year,
            'period_range': f"{period_from}-{period_to}",
            'generated_date': datetime.now().strftime("%Y-%m-%d"),
            'accounts': variance_data,
            'summary': {
                'total_budget': float(total_budget),
                'total_actual': float(total_actual),
                'total_variance': float(total_actual - total_budget),
                'variance_pct': float(
                    (total_actual - total_budget) / total_budget * 100
                ) if total_budget > 0 else 0,
                'account_count': len(variance_data)
            },
            'type_summary': type_summary
        }
        
        # Log report generation
        if self.current_user:
            log_user_action(
                db=self.db,
                user=self.current_user,
                action="GENERATE_VARIANCE_REPORT",
                module="GL",
                new_values={
                    'year': year,
                    'periods': f"{period_from}-{period_to}",
                    'accounts': len(variance_data)
                }
            )
            
        return result
        
    def get_budget_performance(self, account_no: int, year: Optional[int] = None) -> Dict:
        """
        Get detailed budget performance for specific account
        """
        year = year or datetime.now().year
        
        # Get account
        account, status = self.gl_handler.process(4, key_value=account_no)
        if status.fs_reply != "00":
            return {"error": "Account not found"}
            
        # Get budget
        budget = self.db.query(GLBudgetRec).filter(
            and_(
                GLBudgetRec.budget_account == account_no,
                GLBudgetRec.budget_year == year,
                GLBudgetRec.budget_version == 1,
                GLBudgetRec.budget_status == 'A'
            )
        ).first()
        
        if not budget:
            return {"error": "No budget found for account/year"}
            
        # Build period comparison
        current_period = self.system_handler.get_current_period()
        period_data = []
        cumulative_budget = Decimal('0')
        cumulative_actual = Decimal('0')
        
        for period in range(1, 14):
            budget_field = f"budget_period_{period}"
            actual_field = f"ledger_period_{period}"
            
            period_budget = getattr(budget, budget_field, Decimal('0'))
            period_actual = abs(getattr(account, actual_field, Decimal('0'))) if hasattr(account, actual_field) else Decimal('0')
            
            cumulative_budget += period_budget
            cumulative_actual += period_actual if period <= current_period else 0
            
            period_info = {
                'period': period,
                'budget': float(period_budget),
                'actual': float(period_actual) if period <= current_period else None,
                'variance': float(period_actual - period_budget) if period <= current_period else None,
                'variance_pct': float(
                    (period_actual - period_budget) / period_budget * 100
                ) if period_budget > 0 and period <= current_period else None,
                'cumulative_budget': float(cumulative_budget),
                'cumulative_actual': float(cumulative_actual) if period <= current_period else None
            }
            
            period_data.append(period_info)
            
        # YTD performance
        ytd_budget = sum(getattr(budget, f"budget_period_{p}", Decimal('0')) for p in range(1, current_period + 1))
        ytd_actual = sum(abs(getattr(account, f"ledger_period_{p}", Decimal('0'))) for p in range(1, current_period + 1) if hasattr(account, f"ledger_period_{p}"))
        
        # Forecast
        if current_period < 13:
            avg_actual_per_period = ytd_actual / current_period if current_period > 0 else Decimal('0')
            forecast_total = ytd_actual + (avg_actual_per_period * (13 - current_period))
        else:
            forecast_total = ytd_actual
            
        return {
            'account': {
                'number': account.ledger_key,
                'name': account.ledger_name,
                'type': account.ledger_type
            },
            'year': year,
            'current_period': current_period,
            'periods': period_data,
            'ytd_performance': {
                'budget': float(ytd_budget),
                'actual': float(ytd_actual),
                'variance': float(ytd_actual - ytd_budget),
                'variance_pct': float(
                    (ytd_actual - ytd_budget) / ytd_budget * 100
                ) if ytd_budget > 0 else 0,
                'utilization': float(ytd_actual / ytd_budget * 100) if ytd_budget > 0 else 0
            },
            'full_year': {
                'budget': float(budget.budget_total),
                'forecast': float(forecast_total),
                'forecast_variance': float(forecast_total - budget.budget_total),
                'forecast_variance_pct': float(
                    (forecast_total - budget.budget_total) / budget.budget_total * 100
                ) if budget.budget_total > 0 else 0
            }
        }
        
    def _apply_distribution_method(self, budget: GLBudgetRec, budget_data: Dict):
        """Apply distribution method to spread annual budget across periods"""
        method = budget_data.get('distribution_method')
        annual_amount = Decimal(str(budget_data.get('annual_amount', 0)))
        
        if method == 'EQUAL':
            # Equal distribution across 12 periods
            period_amount = annual_amount / 12
            for period in range(1, 13):
                setattr(budget, f"budget_period_{period}", period_amount)
            setattr(budget, 'budget_period_13', Decimal('0'))  # Period 13 for adjustments
            
        elif method == 'SEASONAL':
            # Seasonal distribution based on percentages
            seasonal_pcts = budget_data.get('seasonal_percentages', {})
            for period in range(1, 14):
                pct = Decimal(str(seasonal_pcts.get(f'period_{period}', 0)))
                period_amount = annual_amount * pct / 100
                setattr(budget, f"budget_period_{period}", period_amount)
                
        elif method == 'PRIOR_YEAR':
            # Based on prior year actual
            prior_year = budget_data.get('budget_year', 0) - 1
            account = self.db.query(GLLedgerRec).filter(
                GLLedgerRec.ledger_key == budget.budget_account
            ).first()
            
            if account:
                # Get prior year total
                prior_total = Decimal('0')
                for period in range(1, 14):
                    # This would need access to prior year data
                    prior_total += Decimal('0')  # Placeholder
                    
                if prior_total > 0:
                    # Apply growth factor
                    growth = Decimal(str(1 + budget_data.get('growth_pct', 0) / 100))
                    for period in range(1, 14):
                        # Distribute based on prior year pattern
                        setattr(budget, f"budget_period_{period}", Decimal('0'))  # Placeholder
                        
    def _summarize_by_type(self, variance_data: List[Dict]) -> Dict:
        """Summarize variance by account type"""
        summary = {}
        
        for item in variance_data:
            type_key = item['type']
            if type_key not in summary:
                summary[type_key] = {
                    'type': type_key,
                    'name': self._get_type_name(type_key),
                    'budget': 0.0,
                    'actual': 0.0,
                    'variance': 0.0,
                    'count': 0
                }
                
            summary[type_key]['budget'] += item['budget']
            summary[type_key]['actual'] += item['actual']
            summary[type_key]['variance'] += item['variance']
            summary[type_key]['count'] += 1
            
        # Calculate percentages
        for type_data in summary.values():
            if type_data['budget'] > 0:
                type_data['variance_pct'] = (
                    type_data['variance'] / type_data['budget'] * 100
                )
            else:
                type_data['variance_pct'] = 0
                
        return summary
        
    def _get_type_name(self, account_type: int) -> str:
        """Get readable account type name"""
        type_names = {
            1: 'Assets',
            2: 'Liabilities',
            3: 'Capital',
            4: 'Revenue',
            5: 'Expenses'
        }
        return type_names.get(account_type, 'Unknown')
        
    def export_variance_report(self, report_data: Dict, format: str = 'excel') -> bytes:
        """Export variance report to various formats"""
        if format == 'csv':
            import csv
            import io
            
            output = io.StringIO()
            writer = csv.writer(output)
            
            # Header
            writer.writerow(['Budget vs Actual Variance Report'])
            writer.writerow([f"Year: {report_data['year']}"])
            writer.writerow([f"Periods: {report_data['period_range']}"])
            writer.writerow([])
            
            # Column headers
            writer.writerow([
                'Account', 'Description', 'Type',
                'Budget', 'Actual', 'Variance', 'Variance %', 'Status'
            ])
            
            # Data rows
            for item in report_data['accounts']:
                writer.writerow([
                    item['account'],
                    item['name'],
                    self._get_type_name(item['type']),
                    item['budget'],
                    item['actual'],
                    item['variance'],
                    f"{item['variance_pct']:.1f}%",
                    'Favorable' if item['favorable'] else 'Unfavorable'
                ])
                
            # Summary
            writer.writerow([])
            writer.writerow(['TOTALS', '', '',
                           report_data['summary']['total_budget'],
                           report_data['summary']['total_actual'],
                           report_data['summary']['total_variance'],
                           f"{report_data['summary']['variance_pct']:.1f}%",
                           ''])
                           
            return output.getvalue().encode('utf-8')
            
        elif format == 'excel':
            # Would implement Excel export using openpyxl
            pass
            
        return b''