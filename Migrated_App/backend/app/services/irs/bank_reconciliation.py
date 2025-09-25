"""
IRS Bank Reconciliation Service - IRS030 migration
Handles bank reconciliation for tax purposes
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from datetime import datetime, timedelta
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func, desc

from app.services.file_handlers.system_handler import SystemFileHandler
from app.models.irs import IrsBankReconciliationRec, IrsCompanyConfigRec
from app.core.security import log_user_action
from app.models.auth import User


class IrsBankReconciliationService:
    """
    IRS Bank Reconciliation Service
    Implements IRS030 - bank reconciliation for tax compliance
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.system_handler = SystemFileHandler(db)
        
    def create_reconciliation(self, recon_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Create bank reconciliation record
        Returns (success, error_message or reconciliation_data)
        """
        company_code = recon_data.get('company_code')
        bank_account = recon_data.get('bank_account')
        statement_date = recon_data.get('statement_date')
        statement_balance = Decimal(str(recon_data.get('statement_balance', 0)))
        book_balance = Decimal(str(recon_data.get('book_balance', 0)))
        
        if not all([company_code, bank_account, statement_date]):
            return False, "Company code, bank account, and statement date are required"
            
        # Validate company exists
        company = self.db.query(IrsCompanyConfigRec).filter(
            IrsCompanyConfigRec.config_company_code == company_code
        ).first()
        
        if not company:
            return False, f"Company {company_code} not configured"
            
        try:
            # Parse date if string
            if isinstance(statement_date, str):
                statement_date = int(statement_date.replace('-', ''))
                
            # Check for existing reconciliation
            existing = self.db.query(IrsBankReconciliationRec).filter(
                and_(
                    IrsBankReconciliationRec.recon_company_code == company_code,
                    IrsBankReconciliationRec.recon_bank_account == bank_account,
                    IrsBankReconciliationRec.recon_statement_date == statement_date
                )
            ).first()
            
            if existing:
                return False, f"Reconciliation already exists for {bank_account} on {statement_date}"
                
            # Calculate reconciliation items
            outstanding_checks = Decimal(str(recon_data.get('outstanding_checks', 0)))
            deposits_in_transit = Decimal(str(recon_data.get('deposits_in_transit', 0)))
            bank_charges = Decimal(str(recon_data.get('bank_charges', 0)))
            interest_earned = Decimal(str(recon_data.get('interest_earned', 0)))
            nsf_items = Decimal(str(recon_data.get('nsf_items', 0)))
            other_adjustments = Decimal(str(recon_data.get('other_adjustments', 0)))
            
            # Calculate reconciled balance
            reconciled_balance = (statement_balance 
                                + deposits_in_transit 
                                - outstanding_checks 
                                + interest_earned 
                                - bank_charges 
                                - nsf_items 
                                + other_adjustments)
            
            reconciliation = IrsBankReconciliationRec(
                recon_company_code=company_code,
                recon_bank_account=bank_account,
                recon_statement_date=statement_date,
                recon_statement_balance=statement_balance,
                recon_book_balance=book_balance,
                recon_outstanding_checks=outstanding_checks,
                recon_deposits_in_transit=deposits_in_transit,
                recon_bank_charges=bank_charges,
                recon_interest_earned=interest_earned,
                recon_nsf_items=nsf_items,
                recon_other_adjustments=other_adjustments,
                recon_reconciled_balance=reconciled_balance,
                recon_status='OPEN',
                recon_notes=recon_data.get('notes', ''),
                recon_created_date=int(datetime.now().strftime("%Y%m%d")),
                recon_created_by=self.current_user.username if self.current_user else 'SYSTEM'
            )
            
            self.db.add(reconciliation)
            self.db.commit()
            
            # Log creation
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="CREATE_BANK_RECONCILIATION",
                    table="irs_bank_reconciliation_rec",
                    key=str(reconciliation.recon_id),
                    new_values={
                        'company_code': company_code,
                        'bank_account': bank_account,
                        'statement_date': statement_date,
                        'statement_balance': float(statement_balance),
                        'reconciled_balance': float(reconciled_balance)
                    },
                    module="IRS"
                )
                
            return True, {
                'recon_id': reconciliation.recon_id,
                'company_code': company_code,
                'bank_account': bank_account,
                'statement_date': statement_date,
                'statement_balance': float(statement_balance),
                'book_balance': float(book_balance),
                'reconciled_balance': float(reconciled_balance),
                'variance': float(abs(reconciled_balance - book_balance)),
                'status': 'OPEN'
            }
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def complete_reconciliation(self, recon_id: int, completion_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Complete bank reconciliation
        Returns (success, error_message)
        """
        reconciliation = self.db.query(IrsBankReconciliationRec).filter(
            IrsBankReconciliationRec.recon_id == recon_id
        ).first()
        
        if not reconciliation:
            return False, f"Reconciliation {recon_id} not found"
            
        if reconciliation.recon_status != 'OPEN':
            return False, f"Cannot complete reconciliation with status: {reconciliation.recon_status}"
            
        try:
            # Check if reconciliation balances
            variance_threshold = Decimal(str(completion_data.get('variance_threshold', 0.01)))
            variance = abs(reconciliation.recon_reconciled_balance - reconciliation.recon_book_balance)
            
            if variance > variance_threshold:
                return False, f"Reconciliation variance of ${variance} exceeds threshold of ${variance_threshold}"
                
            # Complete reconciliation
            reconciliation.recon_status = 'RECONCILED'
            reconciliation.recon_reconciled_by = self.current_user.username if self.current_user else 'SYSTEM'
            reconciliation.recon_reconciled_date = int(datetime.now().strftime("%Y%m%d"))
            
            if completion_data.get('notes'):
                reconciliation.recon_notes = completion_data['notes']
                
            self.db.commit()
            
            # Log completion
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="COMPLETE_BANK_RECONCILIATION",
                    table="irs_bank_reconciliation_rec",
                    key=str(recon_id),
                    new_values={
                        'status': 'RECONCILED',
                        'variance': float(variance),
                        'reconciled_by': reconciliation.recon_reconciled_by
                    },
                    module="IRS"
                )
                
            return True, f"Reconciliation {recon_id} completed successfully"
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def get_reconciliation_summary(self, company_code: str, year: int = None) -> Dict:
        """Get bank reconciliation summary"""
        try:
            if year is None:
                year = datetime.now().year
                
            start_date = int(f"{year}0101")
            end_date = int(f"{year}1231")
            
            reconciliations = self.db.query(IrsBankReconciliationRec).filter(
                and_(
                    IrsBankReconciliationRec.recon_company_code == company_code,
                    IrsBankReconciliationRec.recon_statement_date >= start_date,
                    IrsBankReconciliationRec.recon_statement_date <= end_date
                )
            ).order_by(
                IrsBankReconciliationRec.recon_bank_account,
                IrsBankReconciliationRec.recon_statement_date
            ).all()
            
            if not reconciliations:
                return {
                    'company_code': company_code,
                    'year': year,
                    'message': 'No reconciliations found'
                }
                
            # Group by bank account
            accounts_summary = {}
            total_reconciliations = 0
            total_completed = 0
            
            for recon in reconciliations:
                account = recon.recon_bank_account
                
                if account not in accounts_summary:
                    accounts_summary[account] = {
                        'reconciliations': [],
                        'total_count': 0,
                        'completed_count': 0,
                        'average_variance': 0,
                        'total_interest_earned': 0,
                        'total_bank_charges': 0
                    }
                    
                variance = float(abs(recon.recon_reconciled_balance - recon.recon_book_balance))
                
                recon_data = {
                    'recon_id': recon.recon_id,
                    'statement_date': recon.recon_statement_date,
                    'statement_date_formatted': self._format_date(recon.recon_statement_date),
                    'statement_balance': float(recon.recon_statement_balance),
                    'book_balance': float(recon.recon_book_balance),
                    'reconciled_balance': float(recon.recon_reconciled_balance),
                    'variance': variance,
                    'status': recon.recon_status,
                    'outstanding_checks': float(recon.recon_outstanding_checks),
                    'deposits_in_transit': float(recon.recon_deposits_in_transit),
                    'interest_earned': float(recon.recon_interest_earned),
                    'bank_charges': float(recon.recon_bank_charges),
                    'reconciled_by': recon.recon_reconciled_by,
                    'reconciled_date': recon.recon_reconciled_date
                }
                
                accounts_summary[account]['reconciliations'].append(recon_data)
                accounts_summary[account]['total_count'] += 1
                accounts_summary[account]['total_interest_earned'] += float(recon.recon_interest_earned)
                accounts_summary[account]['total_bank_charges'] += float(recon.recon_bank_charges)
                
                if recon.recon_status == 'RECONCILED':
                    accounts_summary[account]['completed_count'] += 1
                    total_completed += 1
                    
                total_reconciliations += 1
                
            # Calculate average variances
            for account_data in accounts_summary.values():
                if account_data['completed_count'] > 0:
                    total_variance = sum(r['variance'] for r in account_data['reconciliations'] 
                                       if r['status'] == 'RECONCILED')
                    account_data['average_variance'] = total_variance / account_data['completed_count']
                    
            return {
                'company_code': company_code,
                'year': year,
                'summary': {
                    'total_reconciliations': total_reconciliations,
                    'completed_reconciliations': total_completed,
                    'completion_rate': (total_completed / total_reconciliations * 100) if total_reconciliations > 0 else 0,
                    'accounts_count': len(accounts_summary)
                },
                'by_account': accounts_summary,
                'compliance_status': self._assess_compliance_status(accounts_summary, year)
            }
            
        except Exception as e:
            return {'error': str(e)}
            
    def generate_reconciliation_report(self, report_filters: Dict) -> Dict:
        """Generate detailed reconciliation report"""
        try:
            company_code = report_filters.get('company_code')
            date_from = report_filters.get('date_from')
            date_to = report_filters.get('date_to')
            bank_account = report_filters.get('bank_account')
            status = report_filters.get('status')
            
            if not company_code:
                return {'error': 'Company code is required'}
                
            # Build query
            query = self.db.query(IrsBankReconciliationRec).filter(
                IrsBankReconciliationRec.recon_company_code == company_code
            )
            
            if date_from:
                query = query.filter(IrsBankReconciliationRec.recon_statement_date >= int(str(date_from).replace('-', '')))
            if date_to:
                query = query.filter(IrsBankReconciliationRec.recon_statement_date <= int(str(date_to).replace('-', '')))
            if bank_account:
                query = query.filter(IrsBankReconciliationRec.recon_bank_account == bank_account)
            if status:
                query = query.filter(IrsBankReconciliationRec.recon_status == status)
                
            reconciliations = query.order_by(
                IrsBankReconciliationRec.recon_statement_date.desc()
            ).all()
            
            # Format report data
            report_data = []
            total_statement_balance = Decimal('0')
            total_book_balance = Decimal('0')
            total_variance = Decimal('0')
            total_interest = Decimal('0')
            total_charges = Decimal('0')
            
            for recon in reconciliations:
                variance = abs(recon.recon_reconciled_balance - recon.recon_book_balance)
                
                report_data.append({
                    'recon_id': recon.recon_id,
                    'bank_account': recon.recon_bank_account,
                    'statement_date': self._format_date(recon.recon_statement_date),
                    'statement_balance': float(recon.recon_statement_balance),
                    'book_balance': float(recon.recon_book_balance),
                    'reconciled_balance': float(recon.recon_reconciled_balance),
                    'variance': float(variance),
                    'outstanding_checks': float(recon.recon_outstanding_checks),
                    'deposits_in_transit': float(recon.recon_deposits_in_transit),
                    'interest_earned': float(recon.recon_interest_earned),
                    'bank_charges': float(recon.recon_bank_charges),
                    'nsf_items': float(recon.recon_nsf_items),
                    'other_adjustments': float(recon.recon_other_adjustments),
                    'status': recon.recon_status,
                    'reconciled_by': recon.recon_reconciled_by,
                    'reconciled_date': self._format_date(recon.recon_reconciled_date) if recon.recon_reconciled_date else '',
                    'notes': recon.recon_notes
                })
                
                total_statement_balance += recon.recon_statement_balance
                total_book_balance += recon.recon_book_balance
                total_variance += variance
                total_interest += recon.recon_interest_earned
                total_charges += recon.recon_bank_charges
                
            return {
                'report_filters': report_filters,
                'report_date': datetime.now().isoformat(),
                'reconciliations': report_data,
                'summary_totals': {
                    'total_reconciliations': len(report_data),
                    'total_statement_balance': float(total_statement_balance),
                    'total_book_balance': float(total_book_balance),
                    'total_variance': float(total_variance),
                    'total_interest_earned': float(total_interest),
                    'total_bank_charges': float(total_charges),
                    'average_variance': float(total_variance / len(report_data)) if report_data else 0
                }
            }
            
        except Exception as e:
            return {'error': str(e)}
            
    def _assess_compliance_status(self, accounts_summary: Dict, year: int) -> Dict:
        """Assess reconciliation compliance status"""
        compliance_issues = []
        compliance_score = 100
        
        for account, data in accounts_summary.items():
            completion_rate = (data['completed_count'] / data['total_count'] * 100) if data['total_count'] > 0 else 0
            
            # Check completion rate
            if completion_rate < 90:
                compliance_issues.append(f"Account {account}: Low completion rate ({completion_rate:.1f}%)")
                compliance_score -= 10
                
            # Check average variance
            if data['average_variance'] > 100:  # $100 threshold
                compliance_issues.append(f"Account {account}: High average variance (${data['average_variance']:.2f})")
                compliance_score -= 5
                
            # Check for missing monthly reconciliations
            if data['total_count'] < 12:  # Should have monthly reconciliations
                compliance_issues.append(f"Account {account}: Missing reconciliations (only {data['total_count']} of 12)")
                compliance_score -= 15
                
        compliance_score = max(0, compliance_score)
        
        if compliance_score >= 95:
            compliance_level = 'EXCELLENT'
        elif compliance_score >= 85:
            compliance_level = 'GOOD'
        elif compliance_score >= 70:
            compliance_level = 'FAIR'
        else:
            compliance_level = 'POOR'
            
        return {
            'compliance_score': compliance_score,
            'compliance_level': compliance_level,
            'issues': compliance_issues,
            'recommendations': self._generate_compliance_recommendations(compliance_issues)
        }
        
    def _generate_compliance_recommendations(self, issues: List[str]) -> List[str]:
        """Generate compliance improvement recommendations"""
        recommendations = []
        
        if any('completion rate' in issue for issue in issues):
            recommendations.append("Complete all monthly bank reconciliations promptly")
            
        if any('variance' in issue for issue in issues):
            recommendations.append("Investigate and resolve reconciliation variances promptly")
            recommendations.append("Review bank reconciliation procedures for accuracy")
            
        if any('Missing reconciliations' in issue for issue in issues):
            recommendations.append("Establish monthly reconciliation schedule and controls")
            recommendations.append("Set up automated reminders for reconciliation due dates")
            
        if not recommendations:
            recommendations.append("Maintain current reconciliation practices")
            
        return recommendations
        
    def _format_date(self, date_int: int) -> str:
        """Format integer date to readable format"""
        if not date_int:
            return ''
        try:
            date_str = str(date_int)
            return f"{date_str[4:6]}/{date_str[6:8]}/{date_str[:4]}"
        except:
            return str(date_int)