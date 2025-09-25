"""
IRS Estimated Payment Service - IRS085 migration
Handles estimated tax payments and quarterly calculations
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from datetime import datetime, timedelta
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func, desc

from app.services.file_handlers.system_handler import SystemFileHandler
from app.models.irs import (
    IrsEstimatedPaymentRec, IrsTaxCalculationRec, IrsCompanyConfigRec,
    IrsTaxReturnRec
)
from app.core.security import log_user_action
from app.models.auth import User


class IrsEstimatedPaymentService:
    """
    IRS Estimated Payment Service
    Implements IRS085 - estimated tax payments and safe harbor rules
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.system_handler = SystemFileHandler(db)
        
        # Safe harbor rules for 2024
        self.safe_harbor_rules = {
            'individual': {
                'prior_year_percentage': Decimal('1.10'),  # 110% if AGI > $150K, 100% otherwise
                'current_year_percentage': Decimal('0.90'),  # 90% of current year
                'agi_threshold': Decimal('150000')
            },
            'corporation': {
                'prior_year_percentage': Decimal('1.00'),  # 100% of prior year
                'current_year_percentage': Decimal('0.90'),  # 90% of current year
                'large_corp_threshold': Decimal('1000000')  # $1M threshold for large corps
            }
        }
        
    def create_estimated_payment(self, payment_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Create estimated tax payment record
        Returns (success, error_message or payment_data)
        """
        company_code = payment_data.get('company_code')
        tax_year = payment_data.get('tax_year')
        quarter = payment_data.get('quarter')
        payment_amount = Decimal(str(payment_data.get('payment_amount', 0)))
        
        if not all([company_code, tax_year, quarter]):
            return False, "Company code, tax year, and quarter are required"
            
        if quarter not in ['Q1', 'Q2', 'Q3', 'Q4']:
            return False, "Quarter must be Q1, Q2, Q3, or Q4"
            
        # Validate company exists
        company = self.db.query(IrsCompanyConfigRec).filter(
            IrsCompanyConfigRec.config_company_code == company_code
        ).first()
        
        if not company:
            return False, f"Company {company_code} not configured"
            
        try:
            # Check if payment already exists
            existing = self.db.query(IrsEstimatedPaymentRec).filter(
                and_(
                    IrsEstimatedPaymentRec.payment_company_code == company_code,
                    IrsEstimatedPaymentRec.payment_tax_year == tax_year,
                    IrsEstimatedPaymentRec.payment_quarter == quarter
                )
            ).first()
            
            if existing:
                return False, f"Estimated payment already exists for {company_code} {tax_year} {quarter}"
                
            # Calculate due date for quarter
            due_dates = self._calculate_quarterly_due_dates(tax_year, company.config_business_type)
            due_date = due_dates[quarter]
            
            # Get required payment amount
            required_amount = self._calculate_required_payment(company_code, tax_year, quarter)
            
            payment = IrsEstimatedPaymentRec(
                payment_company_code=company_code,
                payment_tax_year=tax_year,
                payment_quarter=quarter,
                payment_due_date=due_date,
                payment_amount=payment_amount,
                payment_required_amount=required_amount,
                payment_method=payment_data.get('payment_method', 'CHECK'),
                payment_date=payment_data.get('payment_date'),
                payment_confirmation=payment_data.get('confirmation_number', ''),
                payment_penalty_amount=Decimal('0'),  # Calculate if late
                payment_interest_amount=Decimal('0'),  # Calculate if late
                payment_status='PENDING' if not payment_data.get('payment_date') else 'PAID',
                payment_safe_harbor_met=self._check_safe_harbor_compliance(
                    company_code, tax_year, quarter, payment_amount
                ),
                payment_notes=payment_data.get('notes', ''),
                payment_created_date=int(datetime.now().strftime("%Y%m%d")),
                payment_created_by=self.current_user.username if self.current_user else 'SYSTEM'
            )
            
            # Calculate penalties if payment is late or insufficient
            if payment_data.get('payment_date'):
                penalty_info = self._calculate_penalty_and_interest(
                    payment_amount, required_amount, due_date, 
                    int(str(payment_data.get('payment_date')).replace('-', ''))
                )
                payment.payment_penalty_amount = penalty_info['penalty']
                payment.payment_interest_amount = penalty_info['interest']
                
            self.db.add(payment)
            self.db.commit()
            
            # Log creation
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="CREATE_ESTIMATED_PAYMENT",
                    table="irs_estimated_payment_rec",
                    key=str(payment.payment_id),
                    new_values={
                        'company_code': company_code,
                        'tax_year': tax_year,
                        'quarter': quarter,
                        'payment_amount': float(payment_amount),
                        'required_amount': float(required_amount)
                    },
                    module="IRS"
                )
                
            return True, {
                'payment_id': payment.payment_id,
                'company_code': company_code,
                'tax_year': tax_year,
                'quarter': quarter,
                'due_date': due_date,
                'payment_amount': float(payment_amount),
                'required_amount': float(required_amount),
                'safe_harbor_met': payment.payment_safe_harbor_met == 'Y',
                'penalty_amount': float(payment.payment_penalty_amount),
                'interest_amount': float(payment.payment_interest_amount),
                'status': payment.payment_status
            }
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def calculate_quarterly_requirements(self, company_code: str, tax_year: int) -> Dict:
        """Calculate estimated payment requirements for all quarters"""
        try:
            company = self.db.query(IrsCompanyConfigRec).filter(
                IrsCompanyConfigRec.config_company_code == company_code
            ).first()
            
            if not company:
                return {'error': 'Company not found'}
                
            # Get prior year tax liability
            prior_year_return = self.db.query(IrsTaxReturnRec).filter(
                and_(
                    IrsTaxReturnRec.return_company_code == company_code,
                    IrsTaxReturnRec.return_tax_year == tax_year - 1
                )
            ).first()
            
            prior_year_tax = prior_year_return.return_total_tax if prior_year_return else Decimal('0')
            
            # Get current year tax calculation if available
            current_year_calc = self.db.query(IrsTaxCalculationRec).filter(
                and_(
                    IrsTaxCalculationRec.calc_company_code == company_code,
                    IrsTaxCalculationRec.calc_tax_year == tax_year,
                    IrsTaxCalculationRec.calc_tax_period == 'YR'
                )
            ).first()
            
            current_year_tax = (current_year_calc.calc_federal_tax + current_year_calc.calc_state_tax 
                              if current_year_calc else Decimal('0'))
            
            # Determine safe harbor amounts
            business_type = company.config_business_type.lower()
            is_corporation = business_type in ['corp', 'scorp']
            
            if is_corporation:
                rules = self.safe_harbor_rules['corporation']
                safe_harbor_prior = prior_year_tax * rules['prior_year_percentage']
                safe_harbor_current = current_year_tax * rules['current_year_percentage']
                
                # Large corporation rules
                if prior_year_tax > rules['large_corp_threshold']:
                    safe_harbor_prior = prior_year_tax  # 100% for large corps
                    
            else:
                rules = self.safe_harbor_rules['individual']
                # Check AGI for 110% rule
                agi = current_year_calc.calc_gross_income if current_year_calc else Decimal('0')
                prior_percentage = (rules['prior_year_percentage'] if agi > rules['agi_threshold'] 
                                 else Decimal('1.00'))
                
                safe_harbor_prior = prior_year_tax * prior_percentage
                safe_harbor_current = current_year_tax * rules['current_year_percentage']
                
            # Use the lower safe harbor amount
            safe_harbor_amount = min(safe_harbor_prior, safe_harbor_current) if current_year_tax > 0 else safe_harbor_prior
            
            # Calculate quarterly amounts
            quarterly_amount = safe_harbor_amount / 4
            
            # Get due dates
            due_dates = self._calculate_quarterly_due_dates(tax_year, company.config_business_type)
            
            # Get existing payments
            existing_payments = self.db.query(IrsEstimatedPaymentRec).filter(
                and_(
                    IrsEstimatedPaymentRec.payment_company_code == company_code,
                    IrsEstimatedPaymentRec.payment_tax_year == tax_year
                )
            ).all()
            
            payments_made = {p.payment_quarter: float(p.payment_amount) for p in existing_payments}
            
            quarterly_requirements = []
            cumulative_required = Decimal('0')
            cumulative_paid = Decimal('0')
            
            for quarter in ['Q1', 'Q2', 'Q3', 'Q4']:
                cumulative_required += quarterly_amount
                paid_amount = Decimal(str(payments_made.get(quarter, 0)))
                cumulative_paid += paid_amount
                
                quarterly_requirements.append({
                    'quarter': quarter,
                    'due_date': self._format_date(due_dates[quarter]),
                    'required_amount': float(quarterly_amount),
                    'cumulative_required': float(cumulative_required),
                    'paid_amount': float(paid_amount),
                    'cumulative_paid': float(cumulative_paid),
                    'balance_due': float(max(Decimal('0'), cumulative_required - cumulative_paid)),
                    'status': 'PAID' if paid_amount >= quarterly_amount else 'DUE'
                })
                
            return {
                'company_code': company_code,
                'tax_year': tax_year,
                'business_type': company.config_business_type,
                'prior_year_tax': float(prior_year_tax),
                'current_year_estimated_tax': float(current_year_tax),
                'safe_harbor_calculation': {
                    'prior_year_safe_harbor': float(safe_harbor_prior),
                    'current_year_safe_harbor': float(safe_harbor_current) if current_year_tax > 0 else 0,
                    'recommended_total': float(safe_harbor_amount),
                    'quarterly_amount': float(quarterly_amount)
                },
                'quarterly_requirements': quarterly_requirements,
                'annual_summary': {
                    'total_required': float(safe_harbor_amount),
                    'total_paid': float(cumulative_paid),
                    'remaining_balance': float(max(Decimal('0'), safe_harbor_amount - cumulative_paid))
                }
            }
            
        except Exception as e:
            return {'error': str(e)}
            
    def process_payment(self, payment_id: int, payment_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Process estimated tax payment
        Returns (success, error_message)
        """
        payment = self.db.query(IrsEstimatedPaymentRec).filter(
            IrsEstimatedPaymentRec.payment_id == payment_id
        ).first()
        
        if not payment:
            return False, f"Payment {payment_id} not found"
            
        if payment.payment_status == 'PAID':
            return False, "Payment already processed"
            
        try:
            # Update payment details
            payment_date = payment_data.get('payment_date')
            if payment_date:
                payment.payment_date = int(str(payment_date).replace('-', ''))
                
            payment.payment_method = payment_data.get('payment_method', payment.payment_method)
            payment.payment_confirmation = payment_data.get('confirmation_number', '')
            payment.payment_status = 'PAID'
            
            # Recalculate penalties if payment date provided
            if payment_date:
                penalty_info = self._calculate_penalty_and_interest(
                    payment.payment_amount,
                    payment.payment_required_amount,
                    payment.payment_due_date,
                    payment.payment_date
                )
                payment.payment_penalty_amount = penalty_info['penalty']
                payment.payment_interest_amount = penalty_info['interest']
                
            # Update safe harbor status
            payment.payment_safe_harbor_met = self._check_safe_harbor_compliance(
                payment.payment_company_code,
                payment.payment_tax_year,
                payment.payment_quarter,
                payment.payment_amount
            )
            
            self.db.commit()
            
            # Log processing
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="PROCESS_ESTIMATED_PAYMENT",
                    table="irs_estimated_payment_rec",
                    key=str(payment_id),
                    new_values={
                        'status': 'PAID',
                        'payment_date': payment.payment_date,
                        'confirmation': payment.payment_confirmation,
                        'penalty': float(payment.payment_penalty_amount),
                        'interest': float(payment.payment_interest_amount)
                    },
                    module="IRS"
                )
                
            return True, f"Payment {payment_id} processed successfully"
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def get_payment_history(self, company_code: str, tax_year: int = None) -> Dict:
        """Get estimated payment history"""
        try:
            query = self.db.query(IrsEstimatedPaymentRec).filter(
                IrsEstimatedPaymentRec.payment_company_code == company_code
            )
            
            if tax_year:
                query = query.filter(IrsEstimatedPaymentRec.payment_tax_year == tax_year)
            else:
                # Default to current and previous 2 years
                current_year = datetime.now().year
                query = query.filter(
                    IrsEstimatedPaymentRec.payment_tax_year >= current_year - 2
                )
                
            payments = query.order_by(
                desc(IrsEstimatedPaymentRec.payment_tax_year),
                IrsEstimatedPaymentRec.payment_quarter
            ).all()
            
            if not payments:
                return {
                    'company_code': company_code,
                    'tax_year': tax_year,
                    'message': 'No estimated payments found'
                }
                
            payment_data = []
            total_paid = Decimal('0')
            total_penalties = Decimal('0')
            total_interest = Decimal('0')
            
            for payment in payments:
                payment_info = {
                    'payment_id': payment.payment_id,
                    'tax_year': payment.payment_tax_year,
                    'quarter': payment.payment_quarter,
                    'due_date': self._format_date(payment.payment_due_date),
                    'payment_date': self._format_date(payment.payment_date) if payment.payment_date else '',
                    'required_amount': float(payment.payment_required_amount),
                    'payment_amount': float(payment.payment_amount),
                    'penalty_amount': float(payment.payment_penalty_amount),
                    'interest_amount': float(payment.payment_interest_amount),
                    'total_cost': float(payment.payment_amount + payment.payment_penalty_amount + payment.payment_interest_amount),
                    'payment_method': payment.payment_method,
                    'confirmation_number': payment.payment_confirmation,
                    'status': payment.payment_status,
                    'safe_harbor_met': payment.payment_safe_harbor_met == 'Y',
                    'days_late': self._calculate_days_late(payment.payment_due_date, payment.payment_date) if payment.payment_date else 0,
                    'notes': payment.payment_notes
                }
                
                payment_data.append(payment_info)
                total_paid += payment.payment_amount
                total_penalties += payment.payment_penalty_amount
                total_interest += payment.payment_interest_amount
                
            return {
                'company_code': company_code,
                'tax_year_filter': tax_year,
                'payments': payment_data,
                'summary': {
                    'total_payments': len(payment_data),
                    'total_paid': float(total_paid),
                    'total_penalties': float(total_penalties),
                    'total_interest': float(total_interest),
                    'total_cost': float(total_paid + total_penalties + total_interest),
                    'paid_payments': len([p for p in payment_data if p['status'] == 'PAID']),
                    'pending_payments': len([p for p in payment_data if p['status'] == 'PENDING']),
                    'safe_harbor_met': len([p for p in payment_data if p['safe_harbor_met']])
                }
            }
            
        except Exception as e:
            return {'error': str(e)}
            
    def generate_voucher(self, payment_id: int) -> Dict:
        """Generate Form 1040ES payment voucher"""
        try:
            payment = self.db.query(IrsEstimatedPaymentRec).filter(
                IrsEstimatedPaymentRec.payment_id == payment_id
            ).first()
            
            if not payment:
                return {'error': 'Payment not found'}
                
            company = self.db.query(IrsCompanyConfigRec).filter(
                IrsCompanyConfigRec.config_company_code == payment.payment_company_code
            ).first()
            
            voucher = {
                'voucher_type': 'Form 1040ES' if company.config_business_type == 'SOLE_PROP' else 'Form 1120W',
                'tax_year': payment.payment_tax_year,
                'quarter': payment.payment_quarter,
                'due_date': self._format_date(payment.payment_due_date),
                'company_information': {
                    'name': company.config_company_name if company else '',
                    'ein': company.config_ein if company else '',
                    'address': {
                        'line1': company.config_address_line1 if company else '',
                        'line2': company.config_address_line2 if company else '',
                        'city': company.config_city if company else '',
                        'state': company.config_state if company else '',
                        'zip': company.config_zip_code if company else ''
                    }
                },
                'payment_details': {
                    'payment_amount': float(payment.payment_amount),
                    'quarter_text': {
                        'Q1': 'First Quarter',
                        'Q2': 'Second Quarter', 
                        'Q3': 'Third Quarter',
                        'Q4': 'Fourth Quarter'
                    }.get(payment.payment_quarter, payment.payment_quarter),
                    'amount_words': self._convert_amount_to_words(payment.payment_amount)
                },
                'mailing_instructions': {
                    'make_check_payable_to': 'United States Treasury',
                    'memo_line': f'Form {voucher["voucher_type"]} {payment.payment_tax_year} {payment.payment_quarter}',
                    'mail_to_address': self._get_irs_mailing_address(company.config_state if company else 'NY')
                },
                'generated_date': datetime.now().isoformat(),
                'payment_id': payment_id
            }
            
            return voucher
            
        except Exception as e:
            return {'error': str(e)}
            
    def _calculate_required_payment(self, company_code: str, tax_year: int, quarter: str) -> Decimal:
        """Calculate required payment amount for quarter"""
        # Get annual requirement and divide by quarters remaining
        requirements = self.calculate_quarterly_requirements(company_code, tax_year)
        
        if 'error' in requirements:
            return Decimal('0')
            
        return Decimal(str(requirements['safe_harbor_calculation']['quarterly_amount']))
        
    def _calculate_quarterly_due_dates(self, tax_year: int, business_type: str) -> Dict:
        """Calculate quarterly due dates"""
        if business_type in ['CORP', 'SCORP']:
            # Corporate due dates - 15th of 4th, 6th, 9th, 12th month
            return {
                'Q1': int(f"{tax_year}0415"),
                'Q2': int(f"{tax_year}0615"),
                'Q3': int(f"{tax_year}0915"),
                'Q4': int(f"{tax_year}1215")
            }
        else:
            # Individual due dates - 15th of 4th, 6th, 9th month, and Jan 15th of next year
            return {
                'Q1': int(f"{tax_year}0415"),
                'Q2': int(f"{tax_year}0615"),
                'Q3': int(f"{tax_year}0915"),
                'Q4': int(f"{tax_year + 1}0115")
            }
            
    def _check_safe_harbor_compliance(self, company_code: str, tax_year: int, quarter: str, payment_amount: Decimal) -> str:
        """Check if payment meets safe harbor requirements"""
        requirements = self.calculate_quarterly_requirements(company_code, tax_year)
        
        if 'error' in requirements:
            return 'N'
            
        quarterly_required = Decimal(str(requirements['safe_harbor_calculation']['quarterly_amount']))
        
        return 'Y' if payment_amount >= quarterly_required else 'N'
        
    def _calculate_penalty_and_interest(self, payment_amount: Decimal, required_amount: Decimal, 
                                      due_date: int, payment_date: int) -> Dict:
        """Calculate penalty and interest for late or insufficient payments"""
        penalty = Decimal('0')
        interest = Decimal('0')
        
        # Check if payment is late
        if payment_date > due_date:
            days_late = self._calculate_days_late(due_date, payment_date)
            
            # Calculate underpayment penalty (simplified)
            underpayment = max(Decimal('0'), required_amount - payment_amount)
            
            if underpayment > 0:
                # Penalty rate (varies by year - using 8% as example)
                annual_penalty_rate = Decimal('0.08')
                penalty = underpayment * annual_penalty_rate * Decimal(str(days_late)) / Decimal('365')
                
            # Interest on late payments
            if days_late > 0:
                annual_interest_rate = Decimal('0.06')
                interest = payment_amount * annual_interest_rate * Decimal(str(days_late)) / Decimal('365')
                
        return {
            'penalty': penalty,
            'interest': interest
        }
        
    def _calculate_days_late(self, due_date: int, payment_date: int) -> int:
        """Calculate days between due date and payment date"""
        try:
            due = datetime.strptime(str(due_date), "%Y%m%d")
            paid = datetime.strptime(str(payment_date), "%Y%m%d")
            return max(0, (paid - due).days)
        except:
            return 0
            
    def _convert_amount_to_words(self, amount: Decimal) -> str:
        """Convert dollar amount to words for voucher"""
        # Simplified implementation - would use full number-to-words conversion
        return f"{float(amount):,.2f} dollars"
        
    def _get_irs_mailing_address(self, state: str) -> str:
        """Get IRS mailing address for state"""
        # Simplified - would have full address mapping
        return "Internal Revenue Service, P.O. Box 1300, Charlotte, NC 28201-1300"
        
    def _format_date(self, date_int: int) -> str:
        """Format integer date to readable format"""
        if not date_int:
            return ''
        try:
            date_str = str(date_int)
            return f"{date_str[4:6]}/{date_str[6:8]}/{date_str[:4]}"
        except:
            return str(date_int)