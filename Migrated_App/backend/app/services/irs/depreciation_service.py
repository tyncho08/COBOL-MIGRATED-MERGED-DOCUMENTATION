"""
IRS Depreciation Service
Handles depreciation calculations and asset management for tax purposes
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from datetime import datetime, timedelta
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func, desc

from app.services.file_handlers.system_handler import SystemFileHandler
from app.models.irs import IrsDepreciationRec, IrsCompanyConfigRec, IrsTransactionRec
from app.core.security import log_user_action
from app.models.auth import User


class IrsDepreciationService:
    """
    IRS Depreciation Service
    Implements depreciation calculations including MACRS, Section 179, and Bonus Depreciation
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.system_handler = SystemFileHandler(db)
        
        # MACRS depreciation tables (simplified - full implementation would have complete tables)
        self.macrs_tables = {
            '3-year': [0.3333, 0.4445, 0.1481, 0.0741],
            '5-year': [0.2000, 0.3200, 0.1920, 0.1152, 0.1152, 0.0576],
            '7-year': [0.1429, 0.2449, 0.1749, 0.1249, 0.0893, 0.0892, 0.0893, 0.0446],
            '10-year': [0.1000, 0.1800, 0.1440, 0.1152, 0.0922, 0.0737, 0.0655, 0.0655, 0.0656, 0.0655, 0.0328],
            '15-year': [0.0500, 0.0950, 0.0855, 0.0770, 0.0693, 0.0623, 0.0590, 0.0590, 0.0591, 0.0590, 0.0591, 0.0590, 0.0591, 0.0590, 0.0591, 0.0295],
            '20-year': [0.0375, 0.0722, 0.0668, 0.0618, 0.0571, 0.0528, 0.0489, 0.0452, 0.0447, 0.0447, 0.0447, 0.0447, 0.0447, 0.0447, 0.0447, 0.0447, 0.0447, 0.0447, 0.0447, 0.0447, 0.0223]
        }
        
        # Section 179 limits (2024)
        self.section_179_limits = {
            'max_deduction': Decimal('1160000'),
            'phase_out_threshold': Decimal('2890000'),
            'suv_limit': Decimal('30500')  # For SUVs over 6000 lbs
        }
        
        # Bonus depreciation rates by year
        self.bonus_depreciation_rates = {
            2023: Decimal('0.80'),  # 80%
            2024: Decimal('0.60'),  # 60%
            2025: Decimal('0.40'),  # 40%
            2026: Decimal('0.20'),  # 20%
            2027: Decimal('0.00')   # 0%
        }
        
    def create_depreciation_record(self, asset_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Create depreciation record for an asset
        Returns (success, error_message or depreciation_data)
        """
        company_code = asset_data.get('company_code')
        asset_description = asset_data.get('asset_description')
        asset_cost = Decimal(str(asset_data.get('asset_cost', 0)))
        placed_in_service_date = asset_data.get('placed_in_service_date')
        
        if not all([company_code, asset_description, asset_cost, placed_in_service_date]):
            return False, "Company code, asset description, cost, and placed in service date are required"
            
        # Validate company exists
        company = self.db.query(IrsCompanyConfigRec).filter(
            IrsCompanyConfigRec.config_company_code == company_code
        ).first()
        
        if not company:
            return False, f"Company {company_code} not configured"
            
        try:
            # Parse placed in service date
            if isinstance(placed_in_service_date, str):
                placed_in_service_date = int(placed_in_service_date.replace('-', ''))
                
            # Determine tax year from placed in service date
            tax_year = int(str(placed_in_service_date)[:4])
            
            # Determine depreciation method and life
            depreciation_info = self._determine_depreciation_method(asset_data)
            
            # Calculate depreciation amounts
            depreciation_calc = self._calculate_depreciation_amounts(
                asset_cost, 
                depreciation_info['method'],
                depreciation_info['life_years'],
                tax_year,
                asset_data
            )
            
            depreciation_record = IrsDepreciationRec(
                depr_company_code=company_code,
                depr_tax_year=tax_year,
                depr_asset_description=asset_description,
                depr_asset_category=asset_data.get('asset_category', 'OTHER'),
                depr_asset_cost=asset_cost,
                depr_placed_in_service_date=placed_in_service_date,
                depr_method=depreciation_info['method'],
                depr_life_years=depreciation_info['life_years'],
                depr_convention=depreciation_info['convention'],
                depr_basis_for_depreciation=depreciation_calc['depreciable_basis'],
                depr_section179_amount=depreciation_calc['section179_amount'],
                depr_section179_carryforward=depreciation_calc['section179_carryforward'],
                depr_bonus_amount=depreciation_calc['bonus_amount'],
                depr_regular_amount=depreciation_calc['regular_amount'],
                depr_total_current_year=depreciation_calc['total_current_year'],
                depr_accumulated_depreciation=depreciation_calc['total_current_year'],
                depr_remaining_basis=asset_cost - depreciation_calc['total_current_year'],
                depr_election_section179=asset_data.get('elect_section179', 'N'),
                depr_election_bonus=asset_data.get('elect_bonus', 'Y'),
                depr_business_use_percentage=Decimal(str(asset_data.get('business_use_percentage', 100))),
                depr_notes=asset_data.get('notes', ''),
                depr_created_date=int(datetime.now().strftime("%Y%m%d")),
                depr_created_by=self.current_user.username if self.current_user else 'SYSTEM'
            )
            
            self.db.add(depreciation_record)
            self.db.commit()
            
            # Log creation
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="CREATE_DEPRECIATION_RECORD",
                    table="irs_depreciation_rec",
                    key=str(depreciation_record.depr_id),
                    new_values={
                        'company_code': company_code,
                        'asset_description': asset_description,
                        'asset_cost': float(asset_cost),
                        'total_depreciation': float(depreciation_calc['total_current_year'])
                    },
                    module="IRS"
                )
                
            return True, {
                'depreciation_id': depreciation_record.depr_id,
                'company_code': company_code,
                'tax_year': tax_year,
                'asset_description': asset_description,
                'asset_cost': float(asset_cost),
                'depreciation_method': depreciation_info['method'],
                'life_years': depreciation_info['life_years'],
                'section179_amount': float(depreciation_calc['section179_amount']),
                'bonus_amount': float(depreciation_calc['bonus_amount']),
                'regular_amount': float(depreciation_calc['regular_amount']),
                'total_current_year': float(depreciation_calc['total_current_year']),
                'remaining_basis': float(depreciation_record.depr_remaining_basis)
            }
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def calculate_annual_depreciation(self, company_code: str, tax_year: int) -> Dict:
        """Calculate total depreciation for all assets in a tax year"""
        try:
            # Get all depreciation records for the company and year
            depreciation_records = self.db.query(IrsDepreciationRec).filter(
                and_(
                    IrsDepreciationRec.depr_company_code == company_code,
                    IrsDepreciationRec.depr_tax_year <= tax_year,
                    IrsDepreciationRec.depr_remaining_basis > 0
                )
            ).all()
            
            if not depreciation_records:
                return {
                    'company_code': company_code,
                    'tax_year': tax_year,
                    'message': 'No depreciation records found'
                }
                
            total_section179 = Decimal('0')
            total_bonus = Decimal('0')
            total_regular = Decimal('0')
            total_depreciation = Decimal('0')
            
            asset_details = []
            
            for record in depreciation_records:
                # Calculate current year depreciation for existing assets
                if record.depr_tax_year < tax_year:
                    # Calculate depreciation for subsequent years
                    years_in_service = tax_year - record.depr_tax_year + 1
                    current_year_depreciation = self._calculate_subsequent_year_depreciation(
                        record, years_in_service
                    )
                else:
                    # First year depreciation (already calculated)
                    current_year_depreciation = record.depr_regular_amount
                    
                # Update accumulated depreciation
                if record.depr_tax_year < tax_year:
                    record.depr_accumulated_depreciation += current_year_depreciation
                    record.depr_remaining_basis = max(Decimal('0'), 
                        record.depr_asset_cost - record.depr_accumulated_depreciation)
                    
                asset_detail = {
                    'depreciation_id': record.depr_id,
                    'asset_description': record.depr_asset_description,
                    'asset_category': record.depr_asset_category,
                    'asset_cost': float(record.depr_asset_cost),
                    'placed_in_service': self._format_date(record.depr_placed_in_service_date),
                    'method': record.depr_method,
                    'life_years': record.depr_life_years,
                    'business_use_percentage': float(record.depr_business_use_percentage),
                    'depreciable_basis': float(record.depr_basis_for_depreciation),
                    'section179_amount': float(record.depr_section179_amount) if record.depr_tax_year == tax_year else 0,
                    'bonus_amount': float(record.depr_bonus_amount) if record.depr_tax_year == tax_year else 0,
                    'regular_depreciation': float(current_year_depreciation),
                    'accumulated_depreciation': float(record.depr_accumulated_depreciation),
                    'remaining_basis': float(record.depr_remaining_basis)
                }
                
                asset_details.append(asset_detail)
                
                # Add to totals (only for current year assets for Section 179 and Bonus)
                if record.depr_tax_year == tax_year:
                    total_section179 += record.depr_section179_amount
                    total_bonus += record.depr_bonus_amount
                    
                total_regular += current_year_depreciation
                
            total_depreciation = total_section179 + total_bonus + total_regular
            
            # Apply business income limitations for Section 179
            business_income = self._get_business_income(company_code, tax_year)
            section179_limitation = min(total_section179, business_income) if business_income > 0 else Decimal('0')
            section179_carryforward = max(Decimal('0'), total_section179 - section179_limitation)
            
            self.db.commit()  # Commit accumulated depreciation updates
            
            return {
                'company_code': company_code,
                'tax_year': tax_year,
                'depreciation_summary': {
                    'total_assets': len(asset_details),
                    'section179_deduction': float(section179_limitation),
                    'section179_carryforward': float(section179_carryforward),
                    'bonus_depreciation': float(total_bonus),
                    'regular_depreciation': float(total_regular),
                    'total_depreciation': float(section179_limitation + total_bonus + total_regular)
                },
                'limitations': {
                    'section179_max': float(self.section_179_limits['max_deduction']),
                    'section179_used': float(section179_limitation),
                    'section179_remaining': float(self.section_179_limits['max_deduction'] - section179_limitation),
                    'business_income': float(business_income),
                    'bonus_rate': float(self.bonus_depreciation_rates.get(tax_year, Decimal('0')))
                },
                'asset_details': asset_details
            }
            
        except Exception as e:
            return {'error': str(e)}
            
    def dispose_asset(self, depreciation_id: int, disposal_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Record asset disposal and calculate gain/loss
        Returns (success, error_message or disposal_data)
        """
        depreciation_record = self.db.query(IrsDepreciationRec).filter(
            IrsDepreciationRec.depr_id == depreciation_id
        ).first()
        
        if not depreciation_record:
            return False, f"Depreciation record {depreciation_id} not found"
            
        disposal_date = disposal_data.get('disposal_date')
        sale_price = Decimal(str(disposal_data.get('sale_price', 0)))
        
        if not disposal_date:
            return False, "Disposal date is required"
            
        try:
            if isinstance(disposal_date, str):
                disposal_date = int(disposal_date.replace('-', ''))
                
            # Calculate gain/loss on disposal
            adjusted_basis = depreciation_record.depr_remaining_basis
            gain_loss = sale_price - adjusted_basis
            
            # Determine if depreciation recapture applies
            depreciation_taken = (depreciation_record.depr_asset_cost - 
                                depreciation_record.depr_remaining_basis)
            
            # Section 1245 recapture (personal property)
            section_1245_recapture = Decimal('0')
            if depreciation_record.depr_asset_category in ['EQUIPMENT', 'MACHINERY', 'FURNITURE']:
                section_1245_recapture = min(gain_loss, depreciation_taken) if gain_loss > 0 else Decimal('0')
                
            # Capital gain/loss (remaining gain after recapture)
            capital_gain_loss = gain_loss - section_1245_recapture
            
            # Update depreciation record
            depreciation_record.depr_disposal_date = disposal_date
            depreciation_record.depr_sale_price = sale_price
            depreciation_record.depr_gain_loss = gain_loss
            depreciation_record.depr_recapture_1245 = section_1245_recapture
            depreciation_record.depr_capital_gain_loss = capital_gain_loss
            depreciation_record.depr_remaining_basis = Decimal('0')
            
            self.db.commit()
            
            # Log disposal
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="DISPOSE_ASSET",
                    table="irs_depreciation_rec",
                    key=str(depreciation_id),
                    new_values={
                        'disposal_date': disposal_date,
                        'sale_price': float(sale_price),
                        'gain_loss': float(gain_loss),
                        'section_1245_recapture': float(section_1245_recapture)
                    },
                    module="IRS"
                )
                
            return True, {
                'depreciation_id': depreciation_id,
                'asset_description': depreciation_record.depr_asset_description,
                'original_cost': float(depreciation_record.depr_asset_cost),
                'accumulated_depreciation': float(depreciation_taken),
                'adjusted_basis': float(adjusted_basis),
                'sale_price': float(sale_price),
                'total_gain_loss': float(gain_loss),
                'section_1245_recapture': float(section_1245_recapture),
                'capital_gain_loss': float(capital_gain_loss),
                'disposal_date': self._format_date(disposal_date)
            }
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def _determine_depreciation_method(self, asset_data: Dict) -> Dict:
        """Determine depreciation method and life based on asset category"""
        category = asset_data.get('asset_category', 'OTHER').upper()
        
        # Asset category to depreciation life mapping
        category_mapping = {
            'COMPUTER': {'life': 5, 'method': 'MACRS'},
            'EQUIPMENT': {'life': 7, 'method': 'MACRS'},
            'MACHINERY': {'life': 7, 'method': 'MACRS'},
            'FURNITURE': {'life': 7, 'method': 'MACRS'},
            'VEHICLE': {'life': 5, 'method': 'MACRS'},
            'BUILDING': {'life': 39, 'method': 'SL'},  # Straight line for buildings
            'LEASEHOLD_IMPROVEMENT': {'life': 15, 'method': 'MACRS'},
            'OTHER': {'life': 7, 'method': 'MACRS'}
        }
        
        mapping = category_mapping.get(category, category_mapping['OTHER'])
        
        return {
            'method': asset_data.get('depreciation_method', mapping['method']),
            'life_years': asset_data.get('life_years', mapping['life']),
            'convention': asset_data.get('convention', 'HY')  # Half-year convention
        }
        
    def _calculate_depreciation_amounts(self, asset_cost: Decimal, method: str, life_years: int, 
                                     tax_year: int, asset_data: Dict) -> Dict:
        """Calculate all depreciation amounts for first year"""
        business_use_pct = Decimal(str(asset_data.get('business_use_percentage', 100))) / 100
        depreciable_basis = asset_cost * business_use_pct
        
        section179_amount = Decimal('0')
        section179_carryforward = Decimal('0')
        bonus_amount = Decimal('0')
        regular_amount = Decimal('0')
        
        # Section 179 calculation
        if asset_data.get('elect_section179', 'N') == 'Y':
            max_section179 = min(depreciable_basis, self.section_179_limits['max_deduction'])
            section179_amount = max_section179
            
        # Remaining basis after Section 179
        remaining_basis = depreciable_basis - section179_amount
        
        # Bonus depreciation calculation
        if asset_data.get('elect_bonus', 'Y') == 'Y' and remaining_basis > 0:
            bonus_rate = self.bonus_depreciation_rates.get(tax_year, Decimal('0'))
            bonus_amount = remaining_basis * bonus_rate
            
        # Regular depreciation on remaining basis
        final_basis = remaining_basis - bonus_amount
        
        if final_basis > 0:
            if method == 'MACRS':
                # MACRS first year calculation with half-year convention
                life_key = f"{life_years}-year"
                if life_key in self.macrs_tables:
                    first_year_rate = Decimal(str(self.macrs_tables[life_key][0]))
                    regular_amount = final_basis * first_year_rate
                else:
                    # Straight line fallback
                    regular_amount = final_basis / life_years / 2  # Half-year convention
            else:
                # Straight line method
                regular_amount = final_basis / life_years / 2  # Half-year convention
                
        total_current_year = section179_amount + bonus_amount + regular_amount
        
        return {
            'depreciable_basis': depreciable_basis,
            'section179_amount': section179_amount,
            'section179_carryforward': section179_carryforward,
            'bonus_amount': bonus_amount,
            'regular_amount': regular_amount,
            'total_current_year': total_current_year
        }
        
    def _calculate_subsequent_year_depreciation(self, record: IrsDepreciationRec, year_number: int) -> Decimal:
        """Calculate depreciation for subsequent years"""
        if record.depr_remaining_basis <= 0:
            return Decimal('0')
            
        # Only regular depreciation applies in subsequent years
        basis_for_regular_depreciation = (record.depr_basis_for_depreciation - 
                                        record.depr_section179_amount - 
                                        record.depr_bonus_amount)
        
        if basis_for_regular_depreciation <= 0:
            return Decimal('0')
            
        if record.depr_method == 'MACRS':
            life_key = f"{record.depr_life_years}-year"
            if life_key in self.macrs_tables and year_number <= len(self.macrs_tables[life_key]):
                rate = Decimal(str(self.macrs_tables[life_key][year_number - 1]))
                return basis_for_regular_depreciation * rate
                
        # Straight line fallback
        annual_depreciation = basis_for_regular_depreciation / record.depr_life_years
        remaining_years = record.depr_life_years - year_number + 1
        
        return min(annual_depreciation, record.depr_remaining_basis)
        
    def _get_business_income(self, company_code: str, tax_year: int) -> Decimal:
        """Get business income for Section 179 limitation"""
        # Get net income from transactions
        transactions = self.db.query(IrsTransactionRec).filter(
            and_(
                IrsTransactionRec.trans_company_code == company_code,
                IrsTransactionRec.trans_date >= int(f"{tax_year}0101"),
                IrsTransactionRec.trans_date <= int(f"{tax_year}1231")
            )
        ).all()
        
        income = Decimal('0')
        expenses = Decimal('0')
        
        for trans in transactions:
            if trans.trans_type == 'INCOME':
                income += trans.trans_amount
            else:
                expenses += trans.trans_deductible_amount or trans.trans_amount
                
        return max(Decimal('0'), income - expenses)
        
    def _format_date(self, date_int: int) -> str:
        """Format integer date to readable format"""
        if not date_int:
            return ''
        try:
            date_str = str(date_int)
            return f"{date_str[4:6]}/{date_str[6:8]}/{date_str[:4]}"
        except:
            return str(date_int)