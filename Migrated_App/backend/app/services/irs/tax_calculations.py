"""
IRS Tax Calculations Service - IRS040 migration
Handles tax calculations, rates, and compliance computations
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal, ROUND_HALF_UP
from datetime import datetime
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func, desc

from app.services.file_handlers.system_handler import SystemFileHandler
from app.models.irs import (
    IrsTaxCalculationRec, IrsTaxTableRec, IrsTransactionRec, 
    IrsCompanyConfigRec, IrsEstimatedPaymentRec
)
from app.core.security import log_user_action
from app.models.auth import User


class IrsTaxCalculationService:
    """
    IRS Tax Calculation Service
    Implements IRS040 - comprehensive tax calculations
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.system_handler = SystemFileHandler(db)
        
    def calculate_quarterly_taxes(self, calculation_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Calculate quarterly tax liability
        Returns (success, error_message or calculation_results)
        """
        company_code = calculation_data.get('company_code')
        tax_year = calculation_data.get('tax_year', datetime.now().year)
        quarter = calculation_data.get('quarter')  # Q1, Q2, Q3, Q4
        
        if not company_code or not quarter:
            return False, "Company code and quarter are required"
            
        if quarter not in ['Q1', 'Q2', 'Q3', 'Q4']:
            return False, "Quarter must be Q1, Q2, Q3, or Q4"
            
        # Get company configuration
        company = self.db.query(IrsCompanyConfigRec).filter(
            IrsCompanyConfigRec.config_company_code == company_code
        ).first()
        
        if not company:
            return False, f"Company {company_code} not found"
            
        try:
            # Determine date range for quarter
            quarter_dates = self._get_quarter_date_range(tax_year, quarter)
            
            # Get transactions for the quarter
            quarterly_income, quarterly_expenses = self._get_quarterly_transactions(
                company_code, quarter_dates['start'], quarter_dates['end']
            )
            
            # Get year-to-date totals
            ytd_income, ytd_expenses = self._get_ytd_transactions(
                company_code, tax_year, quarter
            )
            
            # Calculate taxable income
            quarterly_taxable = quarterly_income - quarterly_expenses
            ytd_taxable = ytd_income - ytd_expenses
            
            # Calculate tax liability based on business type
            tax_calculation = self._calculate_business_tax(
                company.config_business_type,
                quarterly_taxable,
                ytd_taxable,
                tax_year
            )
            
            # Calculate estimated tax payments made
            estimated_payments = self._get_estimated_payments_ytd(company_code, tax_year, quarter)
            
            # Calculate withholding tax (if applicable)
            withholding_tax = self._calculate_withholding_tax(company_code, quarter_dates)
            
            # Calculate penalties and interest
            penalty_interest = self._calculate_penalty_interest(
                tax_calculation['quarterly_tax'],
                estimated_payments,
                quarter
            )
            
            # Create or update calculation record
            calc_record = self._save_tax_calculation(
                company_code,
                tax_year,
                quarter,
                {
                    'gross_income': quarterly_income,
                    'total_deductions': quarterly_expenses,
                    'taxable_income': quarterly_taxable,
                    'federal_tax': tax_calculation['federal_tax'],
                    'state_tax': tax_calculation['state_tax'],
                    'self_employment_tax': tax_calculation['self_employment_tax'],
                    'estimated_tax_payments': estimated_payments,
                    'withholding_tax': withholding_tax,
                    'penalties': penalty_interest['penalties'],
                    'interest': penalty_interest['interest'],
                    'tax_due': tax_calculation['total_tax'] - estimated_payments - withholding_tax + penalty_interest['penalties'],
                    'effective_rate': tax_calculation['effective_rate'],
                    'marginal_rate': tax_calculation['marginal_rate']
                }
            )
            
            result = {
                'calculation_id': calc_record.calc_id,
                'company_code': company_code,
                'tax_year': tax_year,
                'quarter': quarter,
                'quarterly_summary': {
                    'gross_income': float(quarterly_income),
                    'total_deductions': float(quarterly_expenses),
                    'taxable_income': float(quarterly_taxable)
                },
                'ytd_summary': {
                    'gross_income': float(ytd_income),
                    'total_deductions': float(ytd_expenses),
                    'taxable_income': float(ytd_taxable)
                },
                'tax_calculation': {
                    'federal_tax': float(tax_calculation['federal_tax']),
                    'state_tax': float(tax_calculation['state_tax']),
                    'self_employment_tax': float(tax_calculation['self_employment_tax']),
                    'total_tax': float(tax_calculation['total_tax']),
                    'effective_rate': float(tax_calculation['effective_rate']),
                    'marginal_rate': float(tax_calculation['marginal_rate'])
                },
                'payments_and_credits': {
                    'estimated_payments': float(estimated_payments),
                    'withholding_tax': float(withholding_tax),
                    'penalties': float(penalty_interest['penalties']),
                    'interest': float(penalty_interest['interest'])
                },
                'balance_due': float(calc_record.calc_tax_due)
            }
            
            return True, result
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def calculate_annual_taxes(self, calculation_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Calculate annual tax liability
        Returns (success, error_message or calculation_results)
        """
        company_code = calculation_data.get('company_code')
        tax_year = calculation_data.get('tax_year', datetime.now().year)
        
        if not company_code:
            return False, "Company code is required"
            
        try:
            # Get company configuration
            company = self.db.query(IrsCompanyConfigRec).filter(
                IrsCompanyConfigRec.config_company_code == company_code
            ).first()
            
            if not company:
                return False, f"Company {company_code} not found"
                
            # Get annual transactions
            annual_income, annual_expenses = self._get_annual_transactions(company_code, tax_year)
            
            # Calculate taxable income
            taxable_income = annual_income - annual_expenses
            
            # Calculate comprehensive tax liability
            tax_calculation = self._calculate_comprehensive_tax(
                company.config_business_type,
                taxable_income,
                tax_year,
                company_code
            )
            
            # Calculate total payments made during year
            total_payments = self._get_total_payments(company_code, tax_year)
            
            # Create final calculation record
            calc_record = self._save_tax_calculation(
                company_code,
                tax_year,
                'YR',
                tax_calculation
            )
            
            # Generate detailed breakdown
            result = {
                'calculation_id': calc_record.calc_id,
                'company_code': company_code,
                'tax_year': tax_year,
                'income_summary': {
                    'gross_receipts': float(annual_income),
                    'total_deductions': float(annual_expenses),
                    'taxable_income': float(taxable_income)
                },
                'tax_breakdown': {
                    'federal_income_tax': float(tax_calculation['federal_tax']),
                    'state_income_tax': float(tax_calculation['state_tax']),
                    'self_employment_tax': float(tax_calculation['self_employment_tax']),
                    'alternative_minimum_tax': float(tax_calculation.get('alternative_minimum_tax', 0)),
                    'total_tax': float(tax_calculation['total_tax'])
                },
                'payments_and_credits': {
                    'estimated_payments': float(total_payments['estimated']),
                    'withholding_tax': float(total_payments['withholding']),
                    'credits': float(tax_calculation.get('credits', 0)),
                    'total_payments': float(total_payments['total'])
                },
                'final_calculation': {
                    'tax_due': max(0, float(tax_calculation['total_tax'] - total_payments['total'])),
                    'refund_due': max(0, float(total_payments['total'] - tax_calculation['total_tax'])),
                    'effective_tax_rate': float(tax_calculation['effective_rate']),
                    'marginal_tax_rate': float(tax_calculation['marginal_rate'])
                },
                'compliance_status': self._check_compliance_status(company_code, tax_year, calc_record)
            }
            
            return True, result
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def calculate_estimated_payments(self, payment_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Calculate required estimated tax payments
        Returns (success, error_message or payment_schedule)
        """
        company_code = payment_data.get('company_code')
        tax_year = payment_data.get('tax_year', datetime.now().year)
        projected_income = Decimal(str(payment_data.get('projected_income', 0)))
        
        if not company_code or projected_income <= 0:
            return False, "Company code and projected income are required"
            
        try:
            # Get prior year tax liability for safe harbor calculation
            prior_year_tax = self._get_prior_year_tax(company_code, tax_year - 1)
            
            # Calculate projected tax for current year
            projected_tax = self._estimate_annual_tax(company_code, projected_income, tax_year)
            
            # Determine required annual payment (safe harbor rule)
            required_annual_payment = self._calculate_safe_harbor_payment(
                projected_tax,
                prior_year_tax,
                projected_income
            )
            
            # Calculate quarterly payment amounts
            quarterly_payment = required_annual_payment / 4
            
            # Get due dates for estimated payments
            payment_schedule = self._get_estimated_payment_due_dates(tax_year)
            
            # Create payment schedule
            schedule = []
            for quarter, due_date in payment_schedule.items():
                schedule.append({
                    'quarter': quarter,
                    'due_date': due_date,
                    'payment_amount': float(quarterly_payment),
                    'status': 'DUE'
                })
                
            result = {
                'company_code': company_code,
                'tax_year': tax_year,
                'projected_income': float(projected_income),
                'projected_tax': float(projected_tax),
                'prior_year_tax': float(prior_year_tax),
                'required_annual_payment': float(required_annual_payment),
                'quarterly_payment': float(quarterly_payment),
                'payment_schedule': schedule,
                'safe_harbor_rules': {
                    'applies': required_annual_payment == prior_year_tax,
                    'percentage_required': 100 if projected_income <= 150000 else 110
                }
            }
            
            return True, result
            
        except Exception as e:
            return False, str(e)
            
    def get_tax_calculation_history(self, company_code: str, years: int = 5) -> Dict:
        """Get tax calculation history for analysis"""
        try:
            current_year = datetime.now().year
            start_year = current_year - years + 1
            
            calculations = self.db.query(IrsTaxCalculationRec).filter(
                and_(
                    IrsTaxCalculationRec.calc_company_code == company_code,
                    IrsTaxCalculationRec.calc_tax_year >= start_year,
                    IrsTaxCalculationRec.calc_tax_year <= current_year
                )
            ).order_by(
                IrsTaxCalculationRec.calc_tax_year,
                IrsTaxCalculationRec.calc_tax_period
            ).all()
            
            history = {}
            for calc in calculations:
                year = calc.calc_tax_year
                if year not in history:
                    history[year] = {
                        'quarters': {},
                        'annual': None
                    }
                    
                calc_data = {
                    'calculation_id': calc.calc_id,
                    'period': calc.calc_tax_period,
                    'gross_income': float(calc.calc_gross_income),
                    'total_deductions': float(calc.calc_total_deductions),
                    'taxable_income': float(calc.calc_taxable_income),
                    'federal_tax': float(calc.calc_federal_tax),
                    'state_tax': float(calc.calc_state_tax),
                    'self_employment_tax': float(calc.calc_self_employment_tax),
                    'total_tax': float(calc.calc_federal_tax + calc.calc_state_tax + calc.calc_self_employment_tax),
                    'effective_rate': float(calc.calc_effective_rate),
                    'status': calc.calc_status,
                    'calculation_date': calc.calc_calculation_date
                }
                
                if calc.calc_tax_period == 'YR':
                    history[year]['annual'] = calc_data
                else:
                    history[year]['quarters'][calc.calc_tax_period] = calc_data
                    
            return {
                'company_code': company_code,
                'years_included': years,
                'history': history,
                'summary_statistics': self._calculate_historical_statistics(history)
            }
            
        except Exception as e:
            return {'error': str(e)}
            
    def _get_quarter_date_range(self, year: int, quarter: str) -> Dict:
        """Get date range for tax quarter"""
        quarter_ranges = {
            'Q1': {'start': f"{year}0101", 'end': f"{year}0331"},
            'Q2': {'start': f"{year}0401", 'end': f"{year}0630"},
            'Q3': {'start': f"{year}0701", 'end': f"{year}0930"},
            'Q4': {'start': f"{year}1001", 'end': f"{year}1231"}
        }
        
        dates = quarter_ranges[quarter]
        return {
            'start': int(dates['start']),
            'end': int(dates['end'])
        }
        
    def _get_quarterly_transactions(self, company_code: str, start_date: int, end_date: int) -> Tuple[Decimal, Decimal]:
        """Get income and expenses for quarter"""
        transactions = self.db.query(IrsTransactionRec).filter(
            and_(
                IrsTransactionRec.trans_company_code == company_code,
                IrsTransactionRec.trans_date >= start_date,
                IrsTransactionRec.trans_date <= end_date
            )
        ).all()
        
        income = Decimal('0')
        expenses = Decimal('0')
        
        for trans in transactions:
            if trans.trans_type == 'INCOME':
                income += trans.trans_amount
            else:
                expenses += trans.trans_deductible_amount or trans.trans_amount
                
        return income, expenses
        
    def _get_ytd_transactions(self, company_code: str, tax_year: int, through_quarter: str) -> Tuple[Decimal, Decimal]:
        """Get year-to-date transactions through specified quarter"""
        quarter_end_dates = {
            'Q1': f"{tax_year}0331",
            'Q2': f"{tax_year}0630", 
            'Q3': f"{tax_year}0930",
            'Q4': f"{tax_year}1231"
        }
        
        end_date = int(quarter_end_dates[through_quarter])
        start_date = int(f"{tax_year}0101")
        
        return self._get_quarterly_transactions(company_code, start_date, end_date)
        
    def _get_annual_transactions(self, company_code: str, tax_year: int) -> Tuple[Decimal, Decimal]:
        """Get annual transactions"""
        start_date = int(f"{tax_year}0101")
        end_date = int(f"{tax_year}1231")
        
        return self._get_quarterly_transactions(company_code, start_date, end_date)
        
    def _calculate_business_tax(self, business_type: str, quarterly_income: Decimal, 
                              ytd_income: Decimal, tax_year: int) -> Dict:
        """Calculate tax based on business entity type"""
        
        if business_type == 'CORP':
            # C-Corporation flat rate (21% for 2018+)
            federal_rate = Decimal('0.21')
            federal_tax = quarterly_income * federal_rate
            
            # State tax (estimate 6%)
            state_tax = quarterly_income * Decimal('0.06')
            
            # No self-employment tax for corporations
            se_tax = Decimal('0')
            
        elif business_type == 'SCORP':
            # S-Corporation - pass-through entity, no entity-level tax
            federal_tax = Decimal('0')
            state_tax = Decimal('0')
            se_tax = Decimal('0')
            
        elif business_type in ['LLC', 'PARTNERSHIP']:
            # Pass-through entities
            federal_tax = Decimal('0')
            state_tax = Decimal('0')
            
            # Self-employment tax on net earnings
            se_tax = self._calculate_self_employment_tax(quarterly_income)
            
        else:  # Sole Proprietorship
            # Individual tax rates apply
            federal_tax = self._calculate_individual_tax(quarterly_income, tax_year)
            state_tax = quarterly_income * Decimal('0.05')  # Estimate
            se_tax = self._calculate_self_employment_tax(quarterly_income)
            
        total_tax = federal_tax + state_tax + se_tax
        
        effective_rate = (total_tax / quarterly_income * 100) if quarterly_income > 0 else Decimal('0')
        marginal_rate = self._get_marginal_rate(business_type, quarterly_income, tax_year)
        
        return {
            'federal_tax': federal_tax,
            'state_tax': state_tax,
            'self_employment_tax': se_tax,
            'total_tax': total_tax,
            'effective_rate': effective_rate,
            'marginal_rate': marginal_rate
        }
        
    def _calculate_self_employment_tax(self, net_earnings: Decimal) -> Decimal:
        """Calculate self-employment tax (Social Security + Medicare)"""
        if net_earnings <= 0:
            return Decimal('0')
            
        # 92.35% of net earnings subject to SE tax
        se_income = net_earnings * Decimal('0.9235')
        
        # 2024 Social Security wage base
        ss_wage_base = Decimal('160200')
        
        # Social Security tax (12.4% up to wage base)
        ss_tax = min(se_income, ss_wage_base) * Decimal('0.124')
        
        # Medicare tax (2.9% on all income)
        medicare_tax = se_income * Decimal('0.029')
        
        # Additional Medicare tax (0.9% on income over $200K)
        additional_medicare = max(Decimal('0'), se_income - Decimal('200000')) * Decimal('0.009')
        
        return ss_tax + medicare_tax + additional_medicare
        
    def _calculate_individual_tax(self, taxable_income: Decimal, tax_year: int) -> Decimal:
        """Calculate individual federal income tax using tax brackets"""
        if taxable_income <= 0:
            return Decimal('0')
            
        # 2024 tax brackets for single filer (simplified)
        tax_brackets = [
            (Decimal('11000'), Decimal('0.10')),
            (Decimal('44725'), Decimal('0.12')),
            (Decimal('95375'), Decimal('0.22')),
            (Decimal('182050'), Decimal('0.24')),
            (Decimal('231250'), Decimal('0.32')),
            (Decimal('578125'), Decimal('0.35')),
            (Decimal('99999999'), Decimal('0.37'))
        ]
        
        tax = Decimal('0')
        previous_bracket = Decimal('0')
        
        for bracket_limit, rate in tax_brackets:
            if taxable_income <= previous_bracket:
                break
                
            taxable_in_bracket = min(taxable_income, bracket_limit) - previous_bracket
            tax += taxable_in_bracket * rate
            previous_bracket = bracket_limit
            
            if taxable_income <= bracket_limit:
                break
                
        return tax
        
    def _calculate_comprehensive_tax(self, business_type: str, taxable_income: Decimal, 
                                   tax_year: int, company_code: str) -> Dict:
        """Calculate comprehensive annual tax with all components"""
        
        # Base tax calculation
        base_calc = self._calculate_business_tax(business_type, taxable_income, taxable_income, tax_year)
        
        # Alternative Minimum Tax (AMT) calculation
        amt = self._calculate_amt(business_type, taxable_income, tax_year)
        
        # Tax credits
        credits = self._calculate_tax_credits(company_code, tax_year)
        
        # Final tax calculation
        total_tax = max(base_calc['total_tax'], amt) - credits
        
        return {
            **base_calc,
            'alternative_minimum_tax': amt,
            'credits': credits,
            'total_tax': max(Decimal('0'), total_tax)
        }
        
    def _calculate_amt(self, business_type: str, taxable_income: Decimal, tax_year: int) -> Decimal:
        """Calculate Alternative Minimum Tax"""
        # Simplified AMT calculation - not applicable to most small businesses
        if business_type == 'CORP' and taxable_income > 7500000:
            # Corporate AMT was repealed but may apply to very large corporations
            return taxable_income * Decimal('0.20')
        return Decimal('0')
        
    def _calculate_tax_credits(self, company_code: str, tax_year: int) -> Decimal:
        """Calculate available tax credits"""
        # This would integrate with detailed credit calculations
        # For now, return simplified estimate
        return Decimal('0')
        
    def _get_marginal_rate(self, business_type: str, income: Decimal, tax_year: int) -> Decimal:
        """Get marginal tax rate"""
        if business_type == 'CORP':
            return Decimal('21.0')  # Flat corporate rate
        else:
            # Individual rates - simplified
            if income > 578125:
                return Decimal('37.0')
            elif income > 231250:
                return Decimal('32.0')
            elif income > 182050:
                return Decimal('24.0')
            else:
                return Decimal('22.0')
                
    def _save_tax_calculation(self, company_code: str, tax_year: int, period: str, calc_data: Dict) -> IrsTaxCalculationRec:
        """Save tax calculation to database"""
        
        # Check if calculation already exists
        existing = self.db.query(IrsTaxCalculationRec).filter(
            and_(
                IrsTaxCalculationRec.calc_company_code == company_code,
                IrsTaxCalculationRec.calc_tax_year == tax_year,
                IrsTaxCalculationRec.calc_tax_period == period
            )
        ).first()
        
        if existing:
            # Update existing calculation
            for key, value in calc_data.items():
                if hasattr(existing, f'calc_{key}'):
                    setattr(existing, f'calc_{key}', value)
            calc_record = existing
        else:
            # Create new calculation
            calc_record = IrsTaxCalculationRec(
                calc_company_code=company_code,
                calc_tax_year=tax_year,
                calc_tax_period=period,
                calc_calculation_date=int(datetime.now().strftime("%Y%m%d")),
                calc_status='DRAFT',
                calc_created_date=int(datetime.now().strftime("%Y%m%d")),
                calc_created_by=self.current_user.username if self.current_user else 'SYSTEM'
            )
            
            for key, value in calc_data.items():
                if hasattr(calc_record, f'calc_{key}'):
                    setattr(calc_record, f'calc_{key}', value)
                    
            self.db.add(calc_record)
            
        self.db.commit()
        return calc_record