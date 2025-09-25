"""
IRS Tax Return Service - IRS060 migration
Handles tax return preparation and management
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from datetime import datetime, timedelta
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func, desc

from app.services.file_handlers.system_handler import SystemFileHandler
from app.models.irs import (
    IrsTaxReturnRec, IrsTaxCalculationRec, IrsCompanyConfigRec,
    IrsTransactionRec, IrsScheduleRec
)
from app.core.security import log_user_action
from app.models.auth import User


class IrsTaxReturnService:
    """
    IRS Tax Return Service
    Implements IRS060 - tax return preparation and filing
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.system_handler = SystemFileHandler(db)
        
    def create_tax_return(self, return_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Create new tax return
        Returns (success, error_message or return_data)
        """
        company_code = return_data.get('company_code')
        tax_year = return_data.get('tax_year')
        form_type = return_data.get('form_type')
        
        if not all([company_code, tax_year, form_type]):
            return False, "Company code, tax year, and form type are required"
            
        # Validate company exists
        company = self.db.query(IrsCompanyConfigRec).filter(
            IrsCompanyConfigRec.config_company_code == company_code
        ).first()
        
        if not company:
            return False, f"Company {company_code} not configured"
            
        # Validate form type matches business type
        if not self._validate_form_type(company.config_business_type, form_type):
            return False, f"Form {form_type} not valid for business type {company.config_business_type}"
            
        try:
            # Check if return already exists
            existing = self.db.query(IrsTaxReturnRec).filter(
                and_(
                    IrsTaxReturnRec.return_company_code == company_code,
                    IrsTaxReturnRec.return_tax_year == tax_year,
                    IrsTaxReturnRec.return_form_type == form_type
                )
            ).first()
            
            if existing:
                return False, f"Tax return already exists for {company_code} {tax_year} {form_type}"
                
            # Get due dates
            due_dates = self._calculate_due_dates(tax_year, form_type, company.config_fiscal_year_end)
            
            # Get tax calculation data
            tax_calculation = self._get_annual_tax_calculation(company_code, tax_year)
            
            tax_return = IrsTaxReturnRec(
                return_company_code=company_code,
                return_tax_year=tax_year,
                return_form_type=form_type,
                return_filing_status=return_data.get('filing_status', ''),
                return_preparation_date=int(datetime.now().strftime("%Y%m%d")),
                return_due_date=due_dates['due_date'],
                return_extended_due_date=due_dates['extended_due_date'],
                return_gross_receipts=tax_calculation.get('gross_receipts', Decimal('0')),
                return_total_income=tax_calculation.get('total_income', Decimal('0')),
                return_total_deductions=tax_calculation.get('total_deductions', Decimal('0')),
                return_taxable_income=tax_calculation.get('taxable_income', Decimal('0')),
                return_total_tax=tax_calculation.get('total_tax', Decimal('0')),
                return_total_payments=tax_calculation.get('total_payments', Decimal('0')),
                return_refund_amount=max(Decimal('0'), tax_calculation.get('total_payments', Decimal('0')) - tax_calculation.get('total_tax', Decimal('0'))),
                return_balance_due=max(Decimal('0'), tax_calculation.get('total_tax', Decimal('0')) - tax_calculation.get('total_payments', Decimal('0'))),
                return_amended_return=return_data.get('amended_return', 'N'),
                return_original_return_date=return_data.get('original_return_date'),
                return_status='DRAFT',
                return_preparer_name=company.config_preparer_name or '',
                return_created_date=int(datetime.now().strftime("%Y%m%d")),
                return_created_by=self.current_user.username if self.current_user else 'SYSTEM'
            )
            
            self.db.add(tax_return)
            self.db.commit()
            
            # Log creation
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="CREATE_TAX_RETURN",
                    table="irs_tax_return_rec",
                    key=str(tax_return.return_id),
                    new_values={
                        'company_code': company_code,
                        'tax_year': tax_year,
                        'form_type': form_type,
                        'taxable_income': float(tax_return.return_taxable_income),
                        'total_tax': float(tax_return.return_total_tax)
                    },
                    module="IRS"
                )
                
            return True, {
                'return_id': tax_return.return_id,
                'company_code': company_code,
                'tax_year': tax_year,
                'form_type': form_type,
                'due_date': due_dates['due_date'],
                'taxable_income': float(tax_return.return_taxable_income),
                'total_tax': float(tax_return.return_total_tax),
                'balance_due': float(tax_return.return_balance_due),
                'refund_amount': float(tax_return.return_refund_amount),
                'status': 'DRAFT'
            }
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def update_return_status(self, return_id: int, status_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Update tax return status
        Returns (success, error_message)
        """
        tax_return = self.db.query(IrsTaxReturnRec).filter(
            IrsTaxReturnRec.return_id == return_id
        ).first()
        
        if not tax_return:
            return False, f"Tax return {return_id} not found"
            
        new_status = status_data.get('status')
        valid_statuses = ['DRAFT', 'READY', 'FILED', 'AMENDED']
        
        if new_status not in valid_statuses:
            return False, f"Status must be one of: {', '.join(valid_statuses)}"
            
        # Validate status transitions
        if not self._validate_status_transition(tax_return.return_status, new_status):
            return False, f"Cannot change status from {tax_return.return_status} to {new_status}"
            
        try:
            old_status = tax_return.return_status
            tax_return.return_status = new_status
            
            # Set additional fields based on status
            if new_status == 'FILED':
                tax_return.return_filed_date = int(datetime.now().strftime("%Y%m%d"))
                tax_return.return_confirmation_number = status_data.get('confirmation_number', '')
                
            if new_status == 'READY' and not tax_return.return_preparer_signature_date:
                tax_return.return_preparer_signature_date = int(datetime.now().strftime("%Y%m%d"))
                
            self.db.commit()
            
            # Log status change
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="UPDATE_RETURN_STATUS",
                    table="irs_tax_return_rec",
                    key=str(return_id),
                    old_values={'status': old_status},
                    new_values={
                        'status': new_status,
                        'confirmation_number': status_data.get('confirmation_number', '')
                    },
                    module="IRS"
                )
                
            return True, f"Tax return status updated to {new_status}"
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def get_tax_return_summary(self, company_code: str, tax_year: int = None) -> Dict:
        """Get tax return summary"""
        try:
            query = self.db.query(IrsTaxReturnRec).filter(
                IrsTaxReturnRec.return_company_code == company_code
            )
            
            if tax_year:
                query = query.filter(IrsTaxReturnRec.return_tax_year == tax_year)
            else:
                # Default to current and previous 2 years
                current_year = datetime.now().year
                query = query.filter(
                    IrsTaxReturnRec.return_tax_year >= current_year - 2
                )
                
            returns = query.order_by(
                desc(IrsTaxReturnRec.return_tax_year),
                IrsTaxReturnRec.return_form_type
            ).all()
            
            if not returns:
                return {
                    'company_code': company_code,
                    'tax_year': tax_year,
                    'message': 'No tax returns found'
                }
                
            returns_data = []
            total_tax_all_years = Decimal('0')
            total_refunds = Decimal('0')
            total_balance_due = Decimal('0')
            
            for tax_return in returns:
                return_summary = {
                    'return_id': tax_return.return_id,
                    'tax_year': tax_return.return_tax_year,
                    'form_type': tax_return.return_form_type,
                    'filing_status': tax_return.return_filing_status,
                    'preparation_date': self._format_date(tax_return.return_preparation_date),
                    'due_date': self._format_date(tax_return.return_due_date),
                    'extended_due_date': self._format_date(tax_return.return_extended_due_date) if tax_return.return_extended_due_date else '',
                    'gross_receipts': float(tax_return.return_gross_receipts),
                    'total_income': float(tax_return.return_total_income),
                    'total_deductions': float(tax_return.return_total_deductions),
                    'taxable_income': float(tax_return.return_taxable_income),
                    'total_tax': float(tax_return.return_total_tax),
                    'total_payments': float(tax_return.return_total_payments),
                    'refund_amount': float(tax_return.return_refund_amount),
                    'balance_due': float(tax_return.return_balance_due),
                    'status': tax_return.return_status,
                    'filed_date': self._format_date(tax_return.return_filed_date) if tax_return.return_filed_date else '',
                    'confirmation_number': tax_return.return_confirmation_number or '',
                    'amended_return': tax_return.return_amended_return == 'Y',
                    'preparer_name': tax_return.return_preparer_name,
                    'effective_tax_rate': (float(tax_return.return_total_tax / tax_return.return_taxable_income * 100) 
                                         if tax_return.return_taxable_income > 0 else 0)
                }
                
                returns_data.append(return_summary)
                
                total_tax_all_years += tax_return.return_total_tax
                total_refunds += tax_return.return_refund_amount
                total_balance_due += tax_return.return_balance_due
                
            return {
                'company_code': company_code,
                'tax_year_filter': tax_year,
                'returns': returns_data,
                'summary_totals': {
                    'total_returns': len(returns_data),
                    'total_tax_all_years': float(total_tax_all_years),
                    'total_refunds': float(total_refunds),
                    'total_balance_due': float(total_balance_due),
                    'filed_returns': len([r for r in returns_data if r['status'] == 'FILED']),
                    'draft_returns': len([r for r in returns_data if r['status'] == 'DRAFT'])
                }
            }
            
        except Exception as e:
            return {'error': str(e)}
            
    def generate_return_checklist(self, return_id: int) -> Dict:
        """Generate tax return preparation checklist"""
        try:
            tax_return = self.db.query(IrsTaxReturnRec).filter(
                IrsTaxReturnRec.return_id == return_id
            ).first()
            
            if not tax_return:
                return {'error': 'Tax return not found'}
                
            # Get company configuration
            company = self.db.query(IrsCompanyConfigRec).filter(
                IrsCompanyConfigRec.config_company_code == tax_return.return_company_code
            ).first()
            
            checklist = {
                'return_id': return_id,
                'company_code': tax_return.return_company_code,
                'tax_year': tax_return.return_tax_year,
                'form_type': tax_return.return_form_type,
                'checklist_items': [],
                'completion_percentage': 0,
                'ready_to_file': False
            }
            
            # Basic company information
            checklist['checklist_items'].append({
                'category': 'Company Information',
                'item': 'Company name and EIN verified',
                'completed': bool(company and company.config_company_name and company.config_ein),
                'required': True,
                'notes': 'Verify company name and EIN match IRS records'
            })
            
            # Address information
            checklist['checklist_items'].append({
                'category': 'Company Information',
                'item': 'Complete business address',
                'completed': bool(company and company.config_address_line1 and company.config_city and company.config_state),
                'required': True,
                'notes': 'Complete address required for filing'
            })
            
            # Financial data verification
            checklist['checklist_items'].append({
                'category': 'Financial Data',
                'item': 'Gross receipts/income verified',
                'completed': tax_return.return_gross_receipts > 0,
                'required': True,
                'notes': 'Verify all income sources included'
            })
            
            checklist['checklist_items'].append({
                'category': 'Financial Data',
                'item': 'Deductions properly categorized',
                'completed': tax_return.return_total_deductions > 0,
                'required': True,
                'notes': 'Ensure all deductions are properly supported'
            })
            
            # Supporting schedules
            schedules = self.db.query(IrsScheduleRec).filter(
                IrsScheduleRec.schedule_return_id == return_id
            ).all()
            
            checklist['checklist_items'].append({
                'category': 'Supporting Schedules',
                'item': 'Required schedules completed',
                'completed': len(schedules) > 0,
                'required': self._requires_schedules(tax_return.return_form_type),
                'notes': f'{len(schedules)} schedules attached'
            })
            
            # Preparer information
            checklist['checklist_items'].append({
                'category': 'Preparer Information',
                'item': 'Preparer signature and date',
                'completed': bool(tax_return.return_preparer_signature_date),
                'required': True,
                'notes': 'Required for professional preparation'
            })
            
            # Electronic filing setup
            if company and company.config_electronic_filing == 'Y':
                checklist['checklist_items'].append({
                    'category': 'Electronic Filing',
                    'item': 'E-file setup verified',
                    'completed': bool(company.config_preparer_ein),
                    'required': True,
                    'notes': 'PTIN/EFIN required for e-filing'
                })
                
            # Payment arrangements
            if tax_return.return_balance_due > 0:
                checklist['checklist_items'].append({
                    'category': 'Payment',
                    'item': 'Payment arrangements made',
                    'completed': False,  # This would be tracked separately
                    'required': True,
                    'notes': f'Balance due: ${float(tax_return.return_balance_due):,.2f}'
                })
                
            # Calculate completion
            required_items = [item for item in checklist['checklist_items'] if item['required']]
            completed_items = [item for item in required_items if item['completed']]
            
            if required_items:
                checklist['completion_percentage'] = len(completed_items) / len(required_items) * 100
                checklist['ready_to_file'] = len(completed_items) == len(required_items)
            else:
                checklist['completion_percentage'] = 100
                checklist['ready_to_file'] = True
                
            checklist['summary'] = {
                'total_items': len(checklist['checklist_items']),
                'required_items': len(required_items),
                'completed_items': len(completed_items),
                'remaining_items': len(required_items) - len(completed_items)
            }
            
            return checklist
            
        except Exception as e:
            return {'error': str(e)}
            
    def calculate_amended_return_impact(self, return_id: int, amendments: Dict) -> Dict:
        """Calculate impact of amendments to filed return"""
        try:
            original_return = self.db.query(IrsTaxReturnRec).filter(
                IrsTaxReturnRec.return_id == return_id
            ).first()
            
            if not original_return:
                return {'error': 'Original return not found'}
                
            if original_return.return_status != 'FILED':
                return {'error': 'Can only amend filed returns'}
                
            # Calculate amended amounts
            amended_income = Decimal(str(amendments.get('amended_income', original_return.return_total_income)))
            amended_deductions = Decimal(str(amendments.get('amended_deductions', original_return.return_total_deductions)))
            amended_taxable = amended_income - amended_deductions
            
            # Recalculate tax (simplified - would use actual tax calculation service)
            # This is a placeholder for the actual tax calculation
            tax_rate = (original_return.return_total_tax / original_return.return_taxable_income 
                       if original_return.return_taxable_income > 0 else Decimal('0.21'))
            amended_tax = amended_taxable * tax_rate
            
            # Calculate differences
            income_difference = amended_income - original_return.return_total_income
            deduction_difference = amended_deductions - original_return.return_total_deductions
            taxable_difference = amended_taxable - original_return.return_taxable_income
            tax_difference = amended_tax - original_return.return_total_tax
            
            # Determine refund/payment impact
            if tax_difference > 0:
                additional_tax_due = tax_difference
                additional_refund = Decimal('0')
            else:
                additional_tax_due = Decimal('0')
                additional_refund = abs(tax_difference)
                
            # Calculate penalties and interest (simplified)
            filing_date = datetime.strptime(str(original_return.return_filed_date), "%Y%m%d") if original_return.return_filed_date else datetime.now()
            days_since_filing = (datetime.now() - filing_date).days
            
            penalty = Decimal('0')
            interest = Decimal('0')
            
            if additional_tax_due > 0:
                # Simplified penalty calculation
                penalty = additional_tax_due * Decimal('0.05')  # 5% penalty
                interest = additional_tax_due * Decimal(str(days_since_filing / 365 * 0.06))  # 6% annual interest
                
            return {
                'return_id': return_id,
                'original_return': {
                    'total_income': float(original_return.return_total_income),
                    'total_deductions': float(original_return.return_total_deductions),
                    'taxable_income': float(original_return.return_taxable_income),
                    'total_tax': float(original_return.return_total_tax)
                },
                'amended_return': {
                    'total_income': float(amended_income),
                    'total_deductions': float(amended_deductions),
                    'taxable_income': float(amended_taxable),
                    'total_tax': float(amended_tax)
                },
                'differences': {
                    'income_difference': float(income_difference),
                    'deduction_difference': float(deduction_difference),
                    'taxable_difference': float(taxable_difference),
                    'tax_difference': float(tax_difference)
                },
                'financial_impact': {
                    'additional_tax_due': float(additional_tax_due),
                    'additional_refund': float(additional_refund),
                    'penalty': float(penalty),
                    'interest': float(interest),
                    'net_amount_due': float(additional_tax_due + penalty + interest - additional_refund)
                },
                'recommendations': self._generate_amendment_recommendations(tax_difference, penalty, interest)
            }
            
        except Exception as e:
            return {'error': str(e)}
            
    def _validate_form_type(self, business_type: str, form_type: str) -> bool:
        """Validate form type matches business entity type"""
        valid_forms = {
            'CORP': ['1120', '1120S'],
            'SCORP': ['1120S'],
            'LLC': ['1065', '1040'],
            'PARTNERSHIP': ['1065'],
            'SOLE_PROP': ['1040']
        }
        
        return form_type in valid_forms.get(business_type, [])
        
    def _calculate_due_dates(self, tax_year: int, form_type: str, fiscal_year_end: str) -> Dict:
        """Calculate tax return due dates"""
        # Standard due dates (simplified - actual dates depend on weekends/holidays)
        if form_type in ['1120', '1120S']:
            # Corporate returns - 15th day of 4th month
            due_date = int(f"{tax_year + 1}0415")
            extended_due_date = int(f"{tax_year + 1}1015")
        elif form_type == '1065':
            # Partnership returns - 15th day of 3rd month
            due_date = int(f"{tax_year + 1}0315")
            extended_due_date = int(f"{tax_year + 1}0915")
        else:
            # Individual returns - April 15th
            due_date = int(f"{tax_year + 1}0415")
            extended_due_date = int(f"{tax_year + 1}1015")
            
        return {
            'due_date': due_date,
            'extended_due_date': extended_due_date
        }
        
    def _get_annual_tax_calculation(self, company_code: str, tax_year: int) -> Dict:
        """Get annual tax calculation data"""
        # Get from tax calculation service
        calc = self.db.query(IrsTaxCalculationRec).filter(
            and_(
                IrsTaxCalculationRec.calc_company_code == company_code,
                IrsTaxCalculationRec.calc_tax_year == tax_year,
                IrsTaxCalculationRec.calc_tax_period == 'YR'
            )
        ).first()
        
        if calc:
            return {
                'gross_receipts': calc.calc_gross_income,
                'total_income': calc.calc_gross_income,
                'total_deductions': calc.calc_total_deductions,
                'taxable_income': calc.calc_taxable_income,
                'total_tax': calc.calc_federal_tax + calc.calc_state_tax,
                'total_payments': calc.calc_estimated_tax_payments + calc.calc_withholding_tax
            }
        else:
            # Default empty values
            return {
                'gross_receipts': Decimal('0'),
                'total_income': Decimal('0'),
                'total_deductions': Decimal('0'),
                'taxable_income': Decimal('0'),
                'total_tax': Decimal('0'),
                'total_payments': Decimal('0')
            }
            
    def _validate_status_transition(self, current_status: str, new_status: str) -> bool:
        """Validate status transition is allowed"""
        valid_transitions = {
            'DRAFT': ['READY', 'FILED'],
            'READY': ['DRAFT', 'FILED', 'AMENDED'],
            'FILED': ['AMENDED'],
            'AMENDED': ['FILED']
        }
        
        return new_status in valid_transitions.get(current_status, [])
        
    def _requires_schedules(self, form_type: str) -> bool:
        """Check if form type typically requires supporting schedules"""
        return form_type in ['1120', '1120S', '1065']
        
    def _generate_amendment_recommendations(self, tax_difference: Decimal, penalty: Decimal, interest: Decimal) -> List[str]:
        """Generate recommendations for amended return"""
        recommendations = []
        
        if tax_difference > 0:
            recommendations.append("File amended return promptly to minimize penalties and interest")
            recommendations.append("Consider making payment with amended return to reduce interest")
        elif tax_difference < 0:
            recommendations.append("File amended return to claim additional refund")
            
        if penalty > 0:
            recommendations.append("Review qualification for penalty abatement")
            
        if abs(tax_difference) < 100:
            recommendations.append("Consider if amendment is worth the administrative effort")
            
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