"""
IRS Fiscal Close Service - IRS090 migration
Handles fiscal year closing procedures
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from datetime import datetime, timedelta
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func, desc

from app.services.file_handlers.system_handler import SystemFileHandler
from app.models.irs import (
    IrsFiscalCloseRec, IrsCompanyConfigRec, IrsTaxCalculationRec,
    IrsTransactionRec, IrsTaxReturnRec
)
from app.core.security import log_user_action
from app.models.auth import User


class IrsFiscalCloseService:
    """
    IRS Fiscal Close Service
    Implements IRS090 - fiscal year closing procedures
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.system_handler = SystemFileHandler(db)
        
    def initiate_fiscal_close(self, close_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Initiate fiscal year close process
        Returns (success, error_message or close_data)
        """
        company_code = close_data.get('company_code')
        tax_year = close_data.get('tax_year')
        
        if not company_code or not tax_year:
            return False, "Company code and tax year are required"
            
        # Validate company exists
        company = self.db.query(IrsCompanyConfigRec).filter(
            IrsCompanyConfigRec.config_company_code == company_code
        ).first()
        
        if not company:
            return False, f"Company {company_code} not configured"
            
        try:
            # Check if fiscal year already closed
            existing_close = self.db.query(IrsFiscalCloseRec).filter(
                and_(
                    IrsFiscalCloseRec.close_company_code == company_code,
                    IrsFiscalCloseRec.close_tax_year == tax_year
                )
            ).first()
            
            if existing_close:
                return False, f"Fiscal year {tax_year} already closed for {company_code}"
                
            # Calculate fiscal year dates
            fiscal_dates = self._calculate_fiscal_year_dates(tax_year, company.config_fiscal_year_end)
            
            # Get year-end financial data
            financial_summary = self._calculate_year_end_financials(
                company_code, fiscal_dates['start'], fiscal_dates['end']
            )
            
            # Create fiscal close record
            fiscal_close = IrsFiscalCloseRec(
                close_company_code=company_code,
                close_tax_year=tax_year,
                close_start_date=fiscal_dates['start'],
                close_end_date=fiscal_dates['end'],
                close_status='OPEN',
                close_total_income=financial_summary['total_income'],
                close_total_expenses=financial_summary['total_expenses'],
                close_net_income=financial_summary['net_income'],
                close_depreciation_taken=financial_summary['depreciation'],
                close_section179_deduction=financial_summary['section179'],
                close_bonus_depreciation=financial_summary['bonus_depreciation'],
                close_carryforward_losses=Decimal('0'),  # Would be calculated from prior years
                close_carryforward_credits=Decimal('0'), # Would be calculated from prior years
                close_adjustments_made=0,
                close_final_return_prepared='N',
                close_books_closed='N',
                close_audit_trail_locked='N',
                close_backup_completed='N',
                close_notes='Fiscal year close initiated',
                close_created_date=int(datetime.now().strftime("%Y%m%d")),
                close_created_by=self.current_user.username if self.current_user else 'SYSTEM'
            )
            
            self.db.add(fiscal_close)
            self.db.commit()
            
            # Log initiation
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="INITIATE_FISCAL_CLOSE",
                    table="irs_fiscal_close_rec",
                    key=str(fiscal_close.close_id),
                    new_values={
                        'company_code': company_code,
                        'tax_year': tax_year,
                        'net_income': float(financial_summary['net_income']),
                        'total_income': float(financial_summary['total_income'])
                    },
                    module="IRS"
                )
                
            return True, {
                'close_id': fiscal_close.close_id,
                'company_code': company_code,
                'tax_year': tax_year,
                'fiscal_start_date': fiscal_dates['start'],
                'fiscal_end_date': fiscal_dates['end'],
                'total_income': float(financial_summary['total_income']),
                'total_expenses': float(financial_summary['total_expenses']),
                'net_income': float(financial_summary['net_income']),
                'status': 'OPEN'
            }
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def complete_fiscal_close(self, close_id: int, completion_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Complete fiscal year close
        Returns (success, error_message)
        """
        fiscal_close = self.db.query(IrsFiscalCloseRec).filter(
            IrsFiscalCloseRec.close_id == close_id
        ).first()
        
        if not fiscal_close:
            return False, f"Fiscal close {close_id} not found"
            
        if fiscal_close.close_status != 'OPEN':
            return False, f"Cannot complete fiscal close with status: {fiscal_close.close_status}"
            
        # Validate prerequisites
        validation_result = self._validate_close_prerequisites(fiscal_close)
        if not validation_result['valid']:
            return False, validation_result['error']
            
        try:
            # Update fiscal close record
            fiscal_close.close_status = 'CLOSED'
            fiscal_close.close_closed_date = int(datetime.now().strftime("%Y%m%d"))
            fiscal_close.close_closed_by = self.current_user.username if self.current_user else 'SYSTEM'
            fiscal_close.close_books_closed = 'Y'
            fiscal_close.close_audit_trail_locked = 'Y'
            fiscal_close.close_backup_completed = completion_data.get('backup_completed', 'Y')
            
            # Update completion flags
            if completion_data.get('final_return_prepared'):
                fiscal_close.close_final_return_prepared = 'Y'
                
            # Add closing notes
            closing_notes = completion_data.get('notes', '')
            if closing_notes:
                fiscal_close.close_notes = f"{fiscal_close.close_notes}\nClosure: {closing_notes}"
                
            self.db.commit()
            
            # Log completion
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="COMPLETE_FISCAL_CLOSE",
                    table="irs_fiscal_close_rec",
                    key=str(close_id),
                    new_values={
                        'status': 'CLOSED',
                        'closed_by': fiscal_close.close_closed_by,
                        'books_closed': 'Y'
                    },
                    module="IRS"
                )
                
            return True, f"Fiscal year {fiscal_close.close_tax_year} successfully closed"
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def reopen_fiscal_year(self, close_id: int, reopen_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Reopen closed fiscal year
        Returns (success, error_message)
        """
        fiscal_close = self.db.query(IrsFiscalCloseRec).filter(
            IrsFiscalCloseRec.close_id == close_id
        ).first()
        
        if not fiscal_close:
            return False, f"Fiscal close {close_id} not found"
            
        if fiscal_close.close_status != 'CLOSED':
            return False, f"Can only reopen closed fiscal years"
            
        reopen_reason = reopen_data.get('reason', '')
        if not reopen_reason:
            return False, "Reason for reopening is required"
            
        try:
            # Update fiscal close record
            fiscal_close.close_status = 'REOPENED'
            fiscal_close.close_books_closed = 'N'
            fiscal_close.close_audit_trail_locked = 'N'
            
            # Add reopen notes
            reopen_notes = f"Reopened on {datetime.now().strftime('%Y-%m-%d')}: {reopen_reason}"
            fiscal_close.close_notes = f"{fiscal_close.close_notes}\n{reopen_notes}"
            
            self.db.commit()
            
            # Log reopening
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="REOPEN_FISCAL_YEAR",
                    table="irs_fiscal_close_rec",
                    key=str(close_id),
                    new_values={
                        'status': 'REOPENED',
                        'reason': reopen_reason
                    },
                    module="IRS"
                )
                
            return True, f"Fiscal year {fiscal_close.close_tax_year} reopened"
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def get_fiscal_close_status(self, company_code: str, tax_year: int = None) -> Dict:
        """Get fiscal close status and details"""
        try:
            query = self.db.query(IrsFiscalCloseRec).filter(
                IrsFiscalCloseRec.close_company_code == company_code
            )
            
            if tax_year:
                query = query.filter(IrsFiscalCloseRec.close_tax_year == tax_year)
            else:
                # Get last 3 years
                current_year = datetime.now().year
                query = query.filter(
                    IrsFiscalCloseRec.close_tax_year >= current_year - 2
                )
                
            fiscal_closes = query.order_by(
                desc(IrsFiscalCloseRec.close_tax_year)
            ).all()
            
            if not fiscal_closes:
                return {
                    'company_code': company_code,
                    'tax_year': tax_year,
                    'message': 'No fiscal close records found'
                }
                
            close_data = []
            for close in fiscal_closes:
                close_info = {
                    'close_id': close.close_id,
                    'tax_year': close.close_tax_year,
                    'start_date': self._format_date(close.close_start_date),
                    'end_date': self._format_date(close.close_end_date),
                    'status': close.close_status,
                    'closed_date': self._format_date(close.close_closed_date) if close.close_closed_date else '',
                    'closed_by': close.close_closed_by or '',
                    'total_income': float(close.close_total_income),
                    'total_expenses': float(close.close_total_expenses),
                    'net_income': float(close.close_net_income),
                    'depreciation_taken': float(close.close_depreciation_taken),
                    'section179_deduction': float(close.close_section179_deduction),
                    'bonus_depreciation': float(close.close_bonus_depreciation),
                    'carryforward_losses': float(close.close_carryforward_losses),
                    'carryforward_credits': float(close.close_carryforward_credits),
                    'adjustments_made': close.close_adjustments_made,
                    'final_return_prepared': close.close_final_return_prepared == 'Y',
                    'books_closed': close.close_books_closed == 'Y',
                    'audit_trail_locked': close.close_audit_trail_locked == 'Y',
                    'backup_completed': close.close_backup_completed == 'Y',
                    'notes': close.close_notes
                }
                
                # Add validation status
                if close.close_status == 'OPEN':
                    close_info['prerequisites'] = self._validate_close_prerequisites(close)
                    
                close_data.append(close_info)
                
            return {
                'company_code': company_code,
                'fiscal_closes': close_data,
                'summary': {
                    'total_years': len(close_data),
                    'closed_years': len([c for c in close_data if c['status'] == 'CLOSED']),
                    'open_years': len([c for c in close_data if c['status'] == 'OPEN']),
                    'reopened_years': len([c for c in close_data if c['status'] == 'REOPENED'])
                }
            }
            
        except Exception as e:
            return {'error': str(e)}
            
    def generate_year_end_report(self, close_id: int) -> Dict:
        """Generate comprehensive year-end report"""
        try:
            fiscal_close = self.db.query(IrsFiscalCloseRec).filter(
                IrsFiscalCloseRec.close_id == close_id
            ).first()
            
            if not fiscal_close:
                return {'error': 'Fiscal close record not found'}
                
            # Get company information
            company = self.db.query(IrsCompanyConfigRec).filter(
                IrsCompanyConfigRec.config_company_code == fiscal_close.close_company_code
            ).first()
            
            # Get detailed transaction breakdown
            transaction_summary = self._get_transaction_summary(
                fiscal_close.close_company_code,
                fiscal_close.close_start_date,
                fiscal_close.close_end_date
            )
            
            # Get tax calculations
            tax_calculation = self.db.query(IrsTaxCalculationRec).filter(
                and_(
                    IrsTaxCalculationRec.calc_company_code == fiscal_close.close_company_code,
                    IrsTaxCalculationRec.calc_tax_year == fiscal_close.close_tax_year,
                    IrsTaxCalculationRec.calc_tax_period == 'YR'
                )
            ).first()
            
            # Get tax return status
            tax_return = self.db.query(IrsTaxReturnRec).filter(
                and_(
                    IrsTaxReturnRec.return_company_code == fiscal_close.close_company_code,
                    IrsTaxReturnRec.return_tax_year == fiscal_close.close_tax_year
                )
            ).first()
            
            report = {
                'report_date': datetime.now().isoformat(),
                'company_information': {
                    'company_code': fiscal_close.close_company_code,
                    'company_name': company.config_company_name if company else '',
                    'ein': company.config_ein if company else '',
                    'business_type': company.config_business_type if company else '',
                    'fiscal_year_end': company.config_fiscal_year_end if company else ''
                },
                'fiscal_year_summary': {
                    'tax_year': fiscal_close.close_tax_year,
                    'start_date': self._format_date(fiscal_close.close_start_date),
                    'end_date': self._format_date(fiscal_close.close_end_date),
                    'status': fiscal_close.close_status,
                    'closed_date': self._format_date(fiscal_close.close_closed_date) if fiscal_close.close_closed_date else None
                },
                'financial_summary': {
                    'total_income': float(fiscal_close.close_total_income),
                    'total_expenses': float(fiscal_close.close_total_expenses),
                    'net_income': float(fiscal_close.close_net_income),
                    'gross_profit_margin': (float(fiscal_close.close_net_income / fiscal_close.close_total_income * 100) 
                                          if fiscal_close.close_total_income > 0 else 0)
                },
                'depreciation_summary': {
                    'depreciation_taken': float(fiscal_close.close_depreciation_taken),
                    'section179_deduction': float(fiscal_close.close_section179_deduction),
                    'bonus_depreciation': float(fiscal_close.close_bonus_depreciation),
                    'total_depreciation': float(
                        fiscal_close.close_depreciation_taken +
                        fiscal_close.close_section179_deduction +
                        fiscal_close.close_bonus_depreciation
                    )
                },
                'carryforward_items': {
                    'net_operating_losses': float(fiscal_close.close_carryforward_losses),
                    'tax_credits': float(fiscal_close.close_carryforward_credits)
                },
                'transaction_breakdown': transaction_summary,
                'tax_information': {
                    'tax_calculation_completed': tax_calculation is not None,
                    'taxable_income': float(tax_calculation.calc_taxable_income) if tax_calculation else 0,
                    'total_tax': float(tax_calculation.calc_federal_tax + tax_calculation.calc_state_tax) if tax_calculation else 0,
                    'effective_tax_rate': float(tax_calculation.calc_effective_rate) if tax_calculation else 0
                },
                'compliance_status': {
                    'final_return_prepared': fiscal_close.close_final_return_prepared == 'Y',
                    'return_status': tax_return.return_status if tax_return else 'NOT_PREPARED',
                    'return_filed': tax_return.return_filed_date is not None if tax_return else False,
                    'books_closed': fiscal_close.close_books_closed == 'Y',
                    'audit_trail_locked': fiscal_close.close_audit_trail_locked == 'Y',
                    'backup_completed': fiscal_close.close_backup_completed == 'Y'
                },
                'recommendations': self._generate_year_end_recommendations(fiscal_close, tax_calculation, tax_return)
            }
            
            return report
            
        except Exception as e:
            return {'error': str(e)}
            
    def _calculate_fiscal_year_dates(self, tax_year: int, fiscal_year_end: str) -> Dict:
        """Calculate fiscal year start and end dates"""
        # fiscal_year_end is in MMDD format (e.g., "1231" for December 31)
        end_month = int(fiscal_year_end[:2])
        end_day = int(fiscal_year_end[2:])
        
        if fiscal_year_end == '1231':
            # Calendar year
            start_date = int(f"{tax_year}0101")
            end_date = int(f"{tax_year}1231")
        else:
            # Fiscal year ending in different month
            start_date = int(f"{tax_year}{fiscal_year_end[:2]}{fiscal_year_end[2:]}")
            end_date = int(f"{tax_year + 1}{fiscal_year_end[:2]}{fiscal_year_end[2:]}")
            
        return {
            'start': start_date,
            'end': end_date
        }
        
    def _calculate_year_end_financials(self, company_code: str, start_date: int, end_date: int) -> Dict:
        """Calculate year-end financial summary"""
        # Get all transactions for the fiscal year
        transactions = self.db.query(IrsTransactionRec).filter(
            and_(
                IrsTransactionRec.trans_company_code == company_code,
                IrsTransactionRec.trans_date >= start_date,
                IrsTransactionRec.trans_date <= end_date
            )
        ).all()
        
        total_income = Decimal('0')
        total_expenses = Decimal('0')
        depreciation = Decimal('0')
        section179 = Decimal('0')
        bonus_depreciation = Decimal('0')
        
        for trans in transactions:
            if trans.trans_type == 'INCOME':
                total_income += trans.trans_amount
            else:
                total_expenses += trans.trans_deductible_amount or trans.trans_amount
                
            # Track depreciation-related transactions
            if trans.trans_category in ['DEPRECIATION', 'REGULAR_DEPRECIATION']:
                depreciation += trans.trans_deductible_amount or trans.trans_amount
            elif trans.trans_section_code == '179':
                section179 += trans.trans_deductible_amount or trans.trans_amount
            elif trans.trans_category == 'BONUS_DEPRECIATION':
                bonus_depreciation += trans.trans_deductible_amount or trans.trans_amount
                
        net_income = total_income - total_expenses
        
        return {
            'total_income': total_income,
            'total_expenses': total_expenses,
            'net_income': net_income,
            'depreciation': depreciation,
            'section179': section179,
            'bonus_depreciation': bonus_depreciation
        }
        
    def _validate_close_prerequisites(self, fiscal_close: IrsFiscalCloseRec) -> Dict:
        """Validate prerequisites for closing fiscal year"""
        issues = []
        
        # Check if tax calculation is complete
        tax_calc = self.db.query(IrsTaxCalculationRec).filter(
            and_(
                IrsTaxCalculationRec.calc_company_code == fiscal_close.close_company_code,
                IrsTaxCalculationRec.calc_tax_year == fiscal_close.close_tax_year,
                IrsTaxCalculationRec.calc_tax_period == 'YR'
            )
        ).first()
        
        if not tax_calc:
            issues.append("Annual tax calculation not completed")
        elif tax_calc.calc_status != 'FINAL':
            issues.append("Tax calculation not finalized")
            
        # Check if tax return is prepared
        tax_return = self.db.query(IrsTaxReturnRec).filter(
            and_(
                IrsTaxReturnRec.return_company_code == fiscal_close.close_company_code,
                IrsTaxReturnRec.return_tax_year == fiscal_close.close_tax_year
            )
        ).first()
        
        if not tax_return:
            issues.append("Tax return not prepared")
        elif tax_return.return_status not in ['READY', 'FILED']:
            issues.append("Tax return not ready for filing")
            
        # Check for unreconciled items (placeholder - would check actual reconciliation records)
        # if self._has_unreconciled_items(fiscal_close.close_company_code, fiscal_close.close_tax_year):
        #     issues.append("Unreconciled bank or account items exist")
            
        return {
            'valid': len(issues) == 0,
            'issues': issues,
            'error': f"Prerequisites not met: {'; '.join(issues)}" if issues else None
        }
        
    def _get_transaction_summary(self, company_code: str, start_date: int, end_date: int) -> Dict:
        """Get detailed transaction summary for year-end report"""
        transactions = self.db.query(IrsTransactionRec).filter(
            and_(
                IrsTransactionRec.trans_company_code == company_code,
                IrsTransactionRec.trans_date >= start_date,
                IrsTransactionRec.trans_date <= end_date
            )
        ).all()
        
        income_by_category = {}
        expense_by_category = {}
        
        for trans in transactions:
            category = trans.trans_category
            amount = float(trans.trans_amount)
            
            if trans.trans_type == 'INCOME':
                income_by_category[category] = income_by_category.get(category, 0) + amount
            else:
                expense_by_category[category] = expense_by_category.get(category, 0) + amount
                
        return {
            'total_transactions': len(transactions),
            'income_by_category': income_by_category,
            'expense_by_category': expense_by_category
        }
        
    def _generate_year_end_recommendations(self, fiscal_close: IrsFiscalCloseRec, 
                                         tax_calculation: IrsTaxCalculationRec, 
                                         tax_return: IrsTaxReturnRec) -> List[str]:
        """Generate year-end recommendations"""
        recommendations = []
        
        # Financial performance recommendations
        if fiscal_close.close_net_income < 0:
            recommendations.append("Consider reviewing expenses and implementing cost reduction measures")
        elif fiscal_close.close_net_income > 0:
            recommendations.append("Consider tax planning strategies for next year")
            
        # Tax planning recommendations
        if tax_calculation and float(tax_calculation.calc_effective_rate) > 25:
            recommendations.append("High effective tax rate - review available deductions and credits")
            
        # Depreciation recommendations
        total_depreciation = (fiscal_close.close_depreciation_taken + 
                            fiscal_close.close_section179_deduction + 
                            fiscal_close.close_bonus_depreciation)
        
        if total_depreciation == 0 and fiscal_close.close_total_income > 100000:
            recommendations.append("Consider Section 179 or bonus depreciation for qualifying assets")
            
        # Compliance recommendations
        if not tax_return or tax_return.return_status != 'FILED':
            recommendations.append("File tax return by due date to avoid penalties")
            
        if fiscal_close.close_backup_completed != 'Y':
            recommendations.append("Complete data backup before final close")
            
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