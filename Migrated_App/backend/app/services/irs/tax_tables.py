"""
IRS Tax Tables Service - IRS045 migration
Handles tax rate tables and tax bracket management
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from datetime import datetime
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func, desc

from app.services.file_handlers.system_handler import SystemFileHandler
from app.models import IrsTaxTableRec
from app.core.security import log_user_action
from app.models.auth import User


class IrsTaxTableService:
    """
    IRS Tax Table Service
    Implements IRS045 - tax rate tables and bracket management
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.system_handler = SystemFileHandler(db)
        
    def create_tax_table(self, table_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Create tax rate table
        Returns (success, error_message or table_data)
        """
        tax_year = table_data.get('tax_year')
        filing_status = table_data.get('filing_status')
        brackets = table_data.get('brackets', [])
        
        if not tax_year or not filing_status or not brackets:
            return False, "Tax year, filing status, and tax brackets are required"
            
        valid_statuses = ['SINGLE', 'MFJ', 'MFS', 'HOH', 'QW']  # Married Filing Joint, Married Filing Separate, Head of Household, Qualifying Widow(er)
        if filing_status not in valid_statuses:
            return False, f"Filing status must be one of: {', '.join(valid_statuses)}"
            
        try:
            # Delete existing brackets for this year and filing status
            self.db.query(IrsTaxTableRec).filter(
                and_(
                    IrsTaxTableRec.table_tax_year == tax_year,
                    IrsTaxTableRec.table_filing_status == filing_status
                )
            ).delete()
            
            # Create new tax brackets
            brackets_created = 0
            for i, bracket in enumerate(brackets):
                income_min = Decimal(str(bracket.get('income_min', 0)))
                income_max = Decimal(str(bracket.get('income_max', 999999999)))
                tax_rate = Decimal(str(bracket.get('tax_rate', 0)))
                tax_base = Decimal(str(bracket.get('tax_base', 0)))
                
                tax_table = IrsTaxTableRec(
                    table_tax_year=tax_year,
                    table_filing_status=filing_status,
                    table_income_min=income_min,
                    table_income_max=income_max,
                    table_tax_base=tax_base,
                    table_tax_rate=tax_rate,
                    table_bracket_number=i + 1,
                    table_standard_deduction=Decimal(str(bracket.get('standard_deduction', 0))),
                    table_personal_exemption=Decimal(str(bracket.get('personal_exemption', 0))),
                    table_created_date=int(datetime.now().strftime("%Y%m%d")),
                    table_created_by=self.current_user.username if self.current_user else 'SYSTEM'
                )
                
                self.db.add(tax_table)
                brackets_created += 1
                
            self.db.commit()
            
            # Log creation
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="CREATE_TAX_TABLE",
                    table="irs_tax_table_rec",
                    key=f"{tax_year}-{filing_status}",
                    new_values={
                        'tax_year': tax_year,
                        'filing_status': filing_status,
                        'brackets_created': brackets_created
                    },
                    module="IRS"
                )
                
            return True, {
                'tax_year': tax_year,
                'filing_status': filing_status,
                'brackets_created': brackets_created
            }
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def get_tax_brackets(self, tax_year: int, filing_status: str) -> List[Dict]:
        """Get tax brackets for specific year and filing status"""
        brackets = self.db.query(IrsTaxTableRec).filter(
            and_(
                IrsTaxTableRec.table_tax_year == tax_year,
                IrsTaxTableRec.table_filing_status == filing_status
            )
        ).order_by(IrsTaxTableRec.table_bracket_number).all()
        
        return [
            {
                'bracket_number': bracket.table_bracket_number,
                'income_min': float(bracket.table_income_min),
                'income_max': float(bracket.table_income_max),
                'tax_base': float(bracket.table_tax_base),
                'tax_rate': float(bracket.table_tax_rate),
                'standard_deduction': float(bracket.table_standard_deduction),
                'personal_exemption': float(bracket.table_personal_exemption)
            }
            for bracket in brackets
        ]
        
    def calculate_tax_from_table(self, taxable_income: Decimal, tax_year: int, filing_status: str) -> Dict:
        """
        Calculate tax using tax table brackets
        Returns detailed tax calculation
        """
        try:
            brackets = self.db.query(IrsTaxTableRec).filter(
                and_(
                    IrsTaxTableRec.table_tax_year == tax_year,
                    IrsTaxTableRec.table_filing_status == filing_status
                )
            ).order_by(IrsTaxTableRec.table_bracket_number).all()
            
            if not brackets:
                return {
                    'error': f'No tax table found for {tax_year} {filing_status}',
                    'tax_due': 0,
                    'effective_rate': 0,
                    'marginal_rate': 0
                }
                
            total_tax = Decimal('0')
            marginal_rate = Decimal('0')
            bracket_calculations = []
            
            for bracket in brackets:
                if taxable_income <= bracket.table_income_min:
                    break
                    
                # Calculate taxable amount in this bracket
                bracket_min = bracket.table_income_min
                bracket_max = min(bracket.table_income_max, taxable_income)
                taxable_in_bracket = bracket_max - bracket_min
                
                if taxable_in_bracket > 0:
                    bracket_tax = taxable_in_bracket * bracket.table_tax_rate
                    total_tax += bracket_tax
                    marginal_rate = bracket.table_tax_rate
                    
                    bracket_calculations.append({
                        'bracket_number': bracket.table_bracket_number,
                        'income_range': f"${float(bracket_min):,.2f} - ${float(bracket_max):,.2f}",
                        'taxable_amount': float(taxable_in_bracket),
                        'tax_rate': float(bracket.table_tax_rate * 100),
                        'tax_in_bracket': float(bracket_tax)
                    })
                    
            # Calculate effective rate
            effective_rate = (total_tax / taxable_income * 100) if taxable_income > 0 else Decimal('0')
            
            return {
                'taxable_income': float(taxable_income),
                'tax_year': tax_year,
                'filing_status': filing_status,
                'total_tax': float(total_tax),
                'effective_rate': float(effective_rate),
                'marginal_rate': float(marginal_rate * 100),
                'bracket_calculations': bracket_calculations
            }
            
        except Exception as e:
            return {'error': str(e)}
            
    def load_standard_tax_tables(self, tax_year: int) -> Tuple[bool, Optional[str]]:
        """
        Load standard IRS tax tables for a tax year
        Returns (success, error_message or summary)
        """
        try:
            # 2024 Tax Brackets (example - would be updated annually)
            standard_tables = {
                'SINGLE': [
                    {'income_min': 0, 'income_max': 11000, 'tax_rate': 0.10, 'tax_base': 0, 'standard_deduction': 14600},
                    {'income_min': 11000, 'income_max': 44725, 'tax_rate': 0.12, 'tax_base': 1100, 'standard_deduction': 14600},
                    {'income_min': 44725, 'income_max': 95375, 'tax_rate': 0.22, 'tax_base': 5147, 'standard_deduction': 14600},
                    {'income_min': 95375, 'income_max': 182050, 'tax_rate': 0.24, 'tax_base': 16290, 'standard_deduction': 14600},
                    {'income_min': 182050, 'income_max': 231250, 'tax_rate': 0.32, 'tax_base': 37104, 'standard_deduction': 14600},
                    {'income_min': 231250, 'income_max': 578125, 'tax_rate': 0.35, 'tax_base': 52832, 'standard_deduction': 14600},
                    {'income_min': 578125, 'income_max': 999999999, 'tax_rate': 0.37, 'tax_base': 174238, 'standard_deduction': 14600}
                ],
                'MFJ': [  # Married Filing Jointly
                    {'income_min': 0, 'income_max': 22000, 'tax_rate': 0.10, 'tax_base': 0, 'standard_deduction': 29200},
                    {'income_min': 22000, 'income_max': 89450, 'tax_rate': 0.12, 'tax_base': 2200, 'standard_deduction': 29200},
                    {'income_min': 89450, 'income_max': 190750, 'tax_rate': 0.22, 'tax_base': 10294, 'standard_deduction': 29200},
                    {'income_min': 190750, 'income_max': 364200, 'tax_rate': 0.24, 'tax_base': 32580, 'standard_deduction': 29200},
                    {'income_min': 364200, 'income_max': 462500, 'tax_rate': 0.32, 'tax_base': 74208, 'standard_deduction': 29200},
                    {'income_min': 462500, 'income_max': 693750, 'tax_rate': 0.35, 'tax_base': 105664, 'standard_deduction': 29200},
                    {'income_min': 693750, 'income_max': 999999999, 'tax_rate': 0.37, 'tax_base': 186601, 'standard_deduction': 29200}
                ],
                'MFS': [  # Married Filing Separately
                    {'income_min': 0, 'income_max': 11000, 'tax_rate': 0.10, 'tax_base': 0, 'standard_deduction': 14600},
                    {'income_min': 11000, 'income_max': 44725, 'tax_rate': 0.12, 'tax_base': 1100, 'standard_deduction': 14600},
                    {'income_min': 44725, 'income_max': 95375, 'tax_rate': 0.22, 'tax_base': 5147, 'standard_deduction': 14600},
                    {'income_min': 95375, 'income_max': 182100, 'tax_rate': 0.24, 'tax_base': 16290, 'standard_deduction': 14600},
                    {'income_min': 182100, 'income_max': 231250, 'tax_rate': 0.32, 'tax_base': 37104, 'standard_deduction': 14600},
                    {'income_min': 231250, 'income_max': 346875, 'tax_rate': 0.35, 'tax_base': 52832, 'standard_deduction': 14600},
                    {'income_min': 346875, 'income_max': 999999999, 'tax_rate': 0.37, 'tax_base': 93301, 'standard_deduction': 14600}
                ],
                'HOH': [  # Head of Household
                    {'income_min': 0, 'income_max': 15700, 'tax_rate': 0.10, 'tax_base': 0, 'standard_deduction': 21900},
                    {'income_min': 15700, 'income_max': 59850, 'tax_rate': 0.12, 'tax_base': 1570, 'standard_deduction': 21900},
                    {'income_min': 59850, 'income_max': 95350, 'tax_rate': 0.22, 'tax_base': 6868, 'standard_deduction': 21900},
                    {'income_min': 95350, 'income_max': 182050, 'tax_rate': 0.24, 'tax_base': 14678, 'standard_deduction': 21900},
                    {'income_min': 182050, 'income_max': 231250, 'tax_rate': 0.32, 'tax_base': 35486, 'standard_deduction': 21900},
                    {'income_min': 231250, 'income_max': 578100, 'tax_rate': 0.35, 'tax_base': 51222, 'standard_deduction': 21900},
                    {'income_min': 578100, 'income_max': 999999999, 'tax_rate': 0.37, 'tax_base': 172623, 'standard_deduction': 21900}
                ]
            }
            
            total_brackets_created = 0
            
            for filing_status, brackets in standard_tables.items():
                success, result = self.create_tax_table({
                    'tax_year': tax_year,
                    'filing_status': filing_status,
                    'brackets': brackets
                })
                
                if success:
                    total_brackets_created += len(brackets)
                else:
                    return False, f"Failed to create {filing_status} tax table: {result}"
                    
            return True, {
                'tax_year': tax_year,
                'filing_statuses_created': len(standard_tables),
                'total_brackets_created': total_brackets_created
            }
            
        except Exception as e:
            return False, str(e)
            
    def get_standard_deduction(self, tax_year: int, filing_status: str) -> Decimal:
        """Get standard deduction amount for tax year and filing status"""
        bracket = self.db.query(IrsTaxTableRec).filter(
            and_(
                IrsTaxTableRec.table_tax_year == tax_year,
                IrsTaxTableRec.table_filing_status == filing_status,
                IrsTaxTableRec.table_bracket_number == 1  # Standard deduction is in first bracket
            )
        ).first()
        
        return bracket.table_standard_deduction if bracket else Decimal('0')
        
    def compare_tax_years(self, tax_year1: int, tax_year2: int, filing_status: str) -> Dict:
        """Compare tax brackets between two years"""
        try:
            brackets_year1 = self.get_tax_brackets(tax_year1, filing_status)
            brackets_year2 = self.get_tax_brackets(tax_year2, filing_status)
            
            if not brackets_year1 or not brackets_year2:
                return {'error': 'Tax brackets not found for one or both years'}
                
            comparison = []
            max_brackets = max(len(brackets_year1), len(brackets_year2))
            
            for i in range(max_brackets):
                bracket1 = brackets_year1[i] if i < len(brackets_year1) else {}
                bracket2 = brackets_year2[i] if i < len(brackets_year2) else {}
                
                if bracket1 and bracket2:
                    rate_change = bracket2['tax_rate'] - bracket1['tax_rate']
                    min_change = bracket2['income_min'] - bracket1['income_min']
                    max_change = bracket2['income_max'] - bracket1['income_max']
                    
                    comparison.append({
                        'bracket': i + 1,
                        f'{tax_year1}_rate': bracket1['tax_rate'],
                        f'{tax_year2}_rate': bracket2['tax_rate'],
                        'rate_change': rate_change,
                        f'{tax_year1}_min': bracket1['income_min'],
                        f'{tax_year2}_min': bracket2['income_min'],
                        'min_change': min_change,
                        f'{tax_year1}_max': bracket1['income_max'],
                        f'{tax_year2}_max': bracket2['income_max'],
                        'max_change': max_change
                    })
                    
            # Compare standard deductions
            std_deduction1 = brackets_year1[0]['standard_deduction'] if brackets_year1 else 0
            std_deduction2 = brackets_year2[0]['standard_deduction'] if brackets_year2 else 0
            std_deduction_change = std_deduction2 - std_deduction1
            
            return {
                'tax_year1': tax_year1,
                'tax_year2': tax_year2,
                'filing_status': filing_status,
                'bracket_comparison': comparison,
                'standard_deduction_comparison': {
                    f'{tax_year1}_amount': std_deduction1,
                    f'{tax_year2}_amount': std_deduction2,
                    'change': std_deduction_change,
                    'percent_change': (std_deduction_change / std_deduction1 * 100) if std_deduction1 > 0 else 0
                }
            }
            
        except Exception as e:
            return {'error': str(e)}
            
    def get_available_tax_years(self) -> List[int]:
        """Get list of available tax years in the system"""
        years = self.db.query(IrsTaxTableRec.table_tax_year).distinct().order_by(
            desc(IrsTaxTableRec.table_tax_year)
        ).all()
        
        return [year[0] for year in years]
        
    def get_marginal_rate(self, taxable_income: Decimal, tax_year: int, filing_status: str) -> Decimal:
        """Get marginal tax rate for specific income level"""
        brackets = self.db.query(IrsTaxTableRec).filter(
            and_(
                IrsTaxTableRec.table_tax_year == tax_year,
                IrsTaxTableRec.table_filing_status == filing_status,
                IrsTaxTableRec.table_income_min <= taxable_income,
                IrsTaxTableRec.table_income_max >= taxable_income
            )
        ).first()
        
        return brackets.table_tax_rate if brackets else Decimal('0')
        
    def validate_tax_table_integrity(self, tax_year: int) -> Dict:
        """Validate tax table data integrity"""
        issues = []
        
        try:
            filing_statuses = ['SINGLE', 'MFJ', 'MFS', 'HOH']
            
            for status in filing_statuses:
                brackets = self.db.query(IrsTaxTableRec).filter(
                    and_(
                        IrsTaxTableRec.table_tax_year == tax_year,
                        IrsTaxTableRec.table_filing_status == status
                    )
                ).order_by(IrsTaxTableRec.table_bracket_number).all()
                
                if not brackets:
                    issues.append(f"No tax brackets found for {status}")
                    continue
                    
                # Check for gaps or overlaps in income ranges
                for i, bracket in enumerate(brackets):
                    if i > 0:
                        prev_bracket = brackets[i-1]
                        if bracket.table_income_min != prev_bracket.table_income_max:
                            issues.append(f"{status} bracket {i+1}: Gap or overlap in income range")
                            
                    # Check for negative or zero rates
                    if bracket.table_tax_rate <= 0:
                        issues.append(f"{status} bracket {i+1}: Invalid tax rate ({bracket.table_tax_rate})")
                        
                    # Check for logical income progression
                    if bracket.table_income_min >= bracket.table_income_max:
                        issues.append(f"{status} bracket {i+1}: Income min >= max")
                        
            validation_status = 'VALID' if not issues else 'INVALID'
            
            return {
                'tax_year': tax_year,
                'validation_status': validation_status,
                'issues': issues,
                'filing_statuses_checked': len(filing_statuses)
            }
            
        except Exception as e:
            return {'error': str(e)}