"""
IRS Schedule Service - IRS065 migration
Handles tax return schedules and supporting forms
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from datetime import datetime, timedelta
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func, desc

from app.services.file_handlers.system_handler import SystemFileHandler
from app.models.irs import (
    IrsScheduleRec, IrsTaxReturnRec, IrsCompanyConfigRec,
    IrsTransactionRec, IrsDepreciationRec
)
from app.core.security import log_user_action
from app.models.auth import User


class IrsScheduleService:
    """
    IRS Schedule Service
    Implements IRS065 - tax return schedules and supporting forms
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.system_handler = SystemFileHandler(db)
        
        # Define schedule types and their requirements
        self.schedule_types = {
            'Schedule C': {
                'description': 'Profit or Loss from Business (Sole Proprietorship)',
                'form_types': ['1040'],
                'required_fields': ['gross_receipts', 'total_expenses', 'net_profit']
            },
            'Schedule E': {
                'description': 'Supplemental Income and Loss',
                'form_types': ['1040'],
                'required_fields': ['rental_income', 'rental_expenses', 'net_income']
            },
            'Schedule K-1': {
                'description': 'Partner/Shareholder Share of Income',
                'form_types': ['1065', '1120S'],
                'required_fields': ['partner_name', 'share_percentage', 'ordinary_income']
            },
            'Form 4562': {
                'description': 'Depreciation and Amortization',
                'form_types': ['1040', '1120', '1120S', '1065'],
                'required_fields': ['depreciation_method', 'depreciation_amount', 'section_179']
            },
            'Form 8825': {
                'description': 'Rental Real Estate Income and Expenses',
                'form_types': ['1065'],
                'required_fields': ['rental_income', 'operating_expenses', 'net_income']
            }
        }
        
    def create_schedule(self, schedule_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Create tax schedule
        Returns (success, error_message or schedule_data)
        """
        return_id = schedule_data.get('return_id')
        schedule_type = schedule_data.get('schedule_type')
        
        if not return_id or not schedule_type:
            return False, "Return ID and schedule type are required"
            
        # Validate return exists
        tax_return = self.db.query(IrsTaxReturnRec).filter(
            IrsTaxReturnRec.return_id == return_id
        ).first()
        
        if not tax_return:
            return False, f"Tax return {return_id} not found"
            
        # Validate schedule type
        if schedule_type not in self.schedule_types:
            return False, f"Invalid schedule type: {schedule_type}"
            
        # Validate schedule is applicable to form type
        schedule_info = self.schedule_types[schedule_type]
        if tax_return.return_form_type not in schedule_info['form_types']:
            return False, f"{schedule_type} not applicable to form {tax_return.return_form_type}"
            
        try:
            # Check if schedule already exists
            existing = self.db.query(IrsScheduleRec).filter(
                and_(
                    IrsScheduleRec.schedule_return_id == return_id,
                    IrsScheduleRec.schedule_type == schedule_type
                )
            ).first()
            
            if existing:
                return False, f"{schedule_type} already exists for this return"
                
            # Generate schedule from data
            schedule_content = self._generate_schedule_content(schedule_type, schedule_data, tax_return)
            
            schedule = IrsScheduleRec(
                schedule_return_id=return_id,
                schedule_type=schedule_type,
                schedule_description=schedule_info['description'],
                schedule_line_items=schedule_content['line_items'],
                schedule_total_income=schedule_content.get('total_income', Decimal('0')),
                schedule_total_expenses=schedule_content.get('total_expenses', Decimal('0')),
                schedule_net_amount=schedule_content.get('net_amount', Decimal('0')),
                schedule_depreciation_amount=schedule_content.get('depreciation_amount', Decimal('0')),
                schedule_section179_amount=schedule_content.get('section179_amount', Decimal('0')),
                schedule_other_deductions=schedule_content.get('other_deductions', Decimal('0')),
                schedule_status='DRAFT',
                schedule_notes=schedule_data.get('notes', ''),
                schedule_created_date=int(datetime.now().strftime("%Y%m%d")),
                schedule_created_by=self.current_user.username if self.current_user else 'SYSTEM'
            )
            
            self.db.add(schedule)
            self.db.commit()
            
            # Log creation
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="CREATE_TAX_SCHEDULE",
                    table="irs_schedule_rec",
                    key=str(schedule.schedule_id),
                    new_values={
                        'return_id': return_id,
                        'schedule_type': schedule_type,
                        'net_amount': float(schedule.schedule_net_amount),
                        'total_income': float(schedule.schedule_total_income)
                    },
                    module="IRS"
                )
                
            return True, {
                'schedule_id': schedule.schedule_id,
                'return_id': return_id,
                'schedule_type': schedule_type,
                'description': schedule_info['description'],
                'total_income': float(schedule.schedule_total_income),
                'total_expenses': float(schedule.schedule_total_expenses),
                'net_amount': float(schedule.schedule_net_amount),
                'status': 'DRAFT'
            }
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def update_schedule(self, schedule_id: int, update_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Update tax schedule
        Returns (success, error_message)
        """
        schedule = self.db.query(IrsScheduleRec).filter(
            IrsScheduleRec.schedule_id == schedule_id
        ).first()
        
        if not schedule:
            return False, f"Schedule {schedule_id} not found"
            
        try:
            # Update allowed fields
            if 'line_items' in update_data:
                schedule.schedule_line_items = update_data['line_items']
                
            if 'total_income' in update_data:
                schedule.schedule_total_income = Decimal(str(update_data['total_income']))
                
            if 'total_expenses' in update_data:
                schedule.schedule_total_expenses = Decimal(str(update_data['total_expenses']))
                
            if 'net_amount' in update_data:
                schedule.schedule_net_amount = Decimal(str(update_data['net_amount']))
                
            if 'depreciation_amount' in update_data:
                schedule.schedule_depreciation_amount = Decimal(str(update_data['depreciation_amount']))
                
            if 'section179_amount' in update_data:
                schedule.schedule_section179_amount = Decimal(str(update_data['section179_amount']))
                
            if 'other_deductions' in update_data:
                schedule.schedule_other_deductions = Decimal(str(update_data['other_deductions']))
                
            if 'status' in update_data:
                new_status = update_data['status']
                if new_status in ['DRAFT', 'COMPLETE', 'FILED']:
                    schedule.schedule_status = new_status
                    if new_status == 'COMPLETE':
                        schedule.schedule_completed_date = int(datetime.now().strftime("%Y%m%d"))
                        schedule.schedule_completed_by = self.current_user.username if self.current_user else 'SYSTEM'
                        
            if 'notes' in update_data:
                schedule.schedule_notes = update_data['notes']
                
            self.db.commit()
            
            # Log update
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="UPDATE_TAX_SCHEDULE",
                    table="irs_schedule_rec",
                    key=str(schedule_id),
                    new_values=update_data,
                    module="IRS"
                )
                
            return True, f"Schedule {schedule_id} updated successfully"
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def get_schedules_for_return(self, return_id: int) -> List[Dict]:
        """Get all schedules for a tax return"""
        schedules = self.db.query(IrsScheduleRec).filter(
            IrsScheduleRec.schedule_return_id == return_id
        ).order_by(IrsScheduleRec.schedule_type).all()
        
        return [
            {
                'schedule_id': schedule.schedule_id,
                'schedule_type': schedule.schedule_type,
                'description': schedule.schedule_description,
                'total_income': float(schedule.schedule_total_income),
                'total_expenses': float(schedule.schedule_total_expenses),
                'net_amount': float(schedule.schedule_net_amount),
                'depreciation_amount': float(schedule.schedule_depreciation_amount),
                'section179_amount': float(schedule.schedule_section179_amount),
                'other_deductions': float(schedule.schedule_other_deductions),
                'status': schedule.schedule_status,
                'created_date': self._format_date(schedule.schedule_created_date),
                'completed_date': self._format_date(schedule.schedule_completed_date) if schedule.schedule_completed_date else '',
                'created_by': schedule.schedule_created_by,
                'completed_by': schedule.schedule_completed_by or '',
                'notes': schedule.schedule_notes
            }
            for schedule in schedules
        ]
        
    def generate_schedule_c(self, return_id: int, business_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Generate Schedule C (Profit or Loss from Business)
        Returns (success, error_message or schedule_data)
        """
        try:
            # Get tax return
            tax_return = self.db.query(IrsTaxReturnRec).filter(
                IrsTaxReturnRec.return_id == return_id
            ).first()
            
            if not tax_return:
                return False, f"Tax return {return_id} not found"
                
            # Get company information
            company = self.db.query(IrsCompanyConfigRec).filter(
                IrsCompanyConfigRec.config_company_code == tax_return.return_company_code
            ).first()
            
            # Get business transactions for the tax year
            transactions = self.db.query(IrsTransactionRec).filter(
                and_(
                    IrsTransactionRec.trans_company_code == tax_return.return_company_code,
                    IrsTransactionRec.trans_date >= int(f"{tax_return.return_tax_year}0101"),
                    IrsTransactionRec.trans_date <= int(f"{tax_return.return_tax_year}1231")
                )
            ).all()
            
            # Calculate Schedule C amounts
            gross_receipts = Decimal('0')
            returns_allowances = Decimal('0')
            cost_of_goods_sold = Decimal('0')
            
            # Business expenses by category
            expenses = {
                'advertising': Decimal('0'),
                'car_truck': Decimal('0'),
                'commissions': Decimal('0'),
                'contract_labor': Decimal('0'),
                'depletion': Decimal('0'),
                'depreciation': Decimal('0'),
                'employee_benefit_programs': Decimal('0'),
                'insurance': Decimal('0'),
                'interest_mortgage': Decimal('0'),
                'interest_other': Decimal('0'),
                'legal_professional': Decimal('0'),
                'office_expense': Decimal('0'),
                'pension_profit_sharing': Decimal('0'),
                'rent_vehicles': Decimal('0'),
                'rent_other': Decimal('0'),
                'repairs_maintenance': Decimal('0'),
                'supplies': Decimal('0'),
                'taxes_licenses': Decimal('0'),
                'travel': Decimal('0'),
                'meals_entertainment': Decimal('0'),
                'utilities': Decimal('0'),
                'wages': Decimal('0'),
                'other_expenses': Decimal('0')
            }
            
            # Categorize transactions
            for trans in transactions:
                if trans.trans_type == 'INCOME':
                    if trans.trans_category == 'SALES_REVENUE':
                        gross_receipts += trans.trans_amount
                    elif trans.trans_category == 'RETURNS_ALLOWANCES':
                        returns_allowances += trans.trans_amount
                else:
                    # Map expense categories to Schedule C lines
                    category = trans.trans_category.lower()
                    if 'advertising' in category:
                        expenses['advertising'] += trans.trans_deductible_amount or trans.trans_amount
                    elif 'vehicle' in category or 'car' in category:
                        expenses['car_truck'] += trans.trans_deductible_amount or trans.trans_amount
                    elif 'commission' in category:
                        expenses['commissions'] += trans.trans_deductible_amount or trans.trans_amount
                    elif 'contract' in category:
                        expenses['contract_labor'] += trans.trans_deductible_amount or trans.trans_amount
                    elif 'depreciation' in category:
                        expenses['depreciation'] += trans.trans_deductible_amount or trans.trans_amount
                    elif 'insurance' in category:
                        expenses['insurance'] += trans.trans_deductible_amount or trans.trans_amount
                    elif 'interest' in category:
                        expenses['interest_other'] += trans.trans_deductible_amount or trans.trans_amount
                    elif 'legal' in category or 'professional' in category:
                        expenses['legal_professional'] += trans.trans_deductible_amount or trans.trans_amount
                    elif 'office' in category:
                        expenses['office_expense'] += trans.trans_deductible_amount or trans.trans_amount
                    elif 'rent' in category:
                        expenses['rent_other'] += trans.trans_deductible_amount or trans.trans_amount
                    elif 'repair' in category or 'maintenance' in category:
                        expenses['repairs_maintenance'] += trans.trans_deductible_amount or trans.trans_amount
                    elif 'supplies' in category:
                        expenses['supplies'] += trans.trans_deductible_amount or trans.trans_amount
                    elif 'tax' in category or 'license' in category:
                        expenses['taxes_licenses'] += trans.trans_deductible_amount or trans.trans_amount
                    elif 'travel' in category:
                        expenses['travel'] += trans.trans_deductible_amount or trans.trans_amount
                    elif 'meal' in category or 'entertainment' in category:
                        expenses['meals_entertainment'] += (trans.trans_deductible_amount or trans.trans_amount) * Decimal('0.5')  # 50% limit
                    elif 'utilities' in category:
                        expenses['utilities'] += trans.trans_deductible_amount or trans.trans_amount
                    elif 'wage' in category or 'salary' in category:
                        expenses['wages'] += trans.trans_deductible_amount or trans.trans_amount
                    else:
                        expenses['other_expenses'] += trans.trans_deductible_amount or trans.trans_amount
                        
            # Calculate totals
            net_receipts = gross_receipts - returns_allowances
            gross_income = net_receipts - cost_of_goods_sold
            total_expenses = sum(expenses.values())
            net_profit = gross_income - total_expenses
            
            # Create Schedule C data
            schedule_data = {
                'return_id': return_id,
                'schedule_type': 'Schedule C',
                'notes': f"Schedule C for {company.config_company_name if company else 'Business'} - {tax_return.return_tax_year}",
                'line_items': {
                    'principal_business_code': business_data.get('business_code', ''),
                    'business_name': company.config_company_name if company else '',
                    'business_address': f"{company.config_address_line1}, {company.config_city}, {company.config_state}" if company else '',
                    'accounting_method': company.config_accounting_method if company else 'CASH',
                    'gross_receipts': float(gross_receipts),
                    'returns_allowances': float(returns_allowances),
                    'net_receipts': float(net_receipts),
                    'cost_of_goods_sold': float(cost_of_goods_sold),
                    'gross_income': float(gross_income),
                    'expenses': {k: float(v) for k, v in expenses.items()},
                    'total_expenses': float(total_expenses),
                    'net_profit_loss': float(net_profit)
                },
                'total_income': gross_income,
                'total_expenses': total_expenses,
                'net_amount': net_profit
            }
            
            # Create the schedule
            success, result = self.create_schedule(schedule_data)
            return success, result
            
        except Exception as e:
            return False, str(e)
            
    def generate_form_4562(self, return_id: int) -> Tuple[bool, Optional[str]]:
        """
        Generate Form 4562 (Depreciation and Amortization)
        Returns (success, error_message or schedule_data)
        """
        try:
            # Get tax return
            tax_return = self.db.query(IrsTaxReturnRec).filter(
                IrsTaxReturnRec.return_id == return_id
            ).first()
            
            if not tax_return:
                return False, f"Tax return {return_id} not found"
                
            # Get depreciation records for the tax year
            depreciation_records = self.db.query(IrsDepreciationRec).filter(
                and_(
                    IrsDepreciationRec.depr_company_code == tax_return.return_company_code,
                    IrsDepreciationRec.depr_tax_year == tax_return.return_tax_year
                )
            ).all()
            
            # Calculate depreciation amounts
            regular_depreciation = Decimal('0')
            section_179_deduction = Decimal('0')
            bonus_depreciation = Decimal('0')
            amortization = Decimal('0')
            
            asset_details = []
            
            for depr in depreciation_records:
                regular_depreciation += depr.depr_regular_amount or Decimal('0')
                section_179_deduction += depr.depr_section179_amount or Decimal('0')
                bonus_depreciation += depr.depr_bonus_amount or Decimal('0')
                amortization += depr.depr_amortization_amount or Decimal('0')
                
                asset_details.append({
                    'asset_description': depr.depr_asset_description,
                    'date_placed_in_service': self._format_date(depr.depr_placed_in_service_date),
                    'cost': float(depr.depr_asset_cost),
                    'section_179_amount': float(depr.depr_section179_amount or 0),
                    'basis_for_depreciation': float(depr.depr_basis_for_depreciation),
                    'method': depr.depr_method,
                    'life_years': depr.depr_life_years,
                    'current_year_depreciation': float(depr.depr_regular_amount or 0)
                })
                
            total_depreciation = regular_depreciation + section_179_deduction + bonus_depreciation + amortization
            
            # Create Form 4562 data
            schedule_data = {
                'return_id': return_id,
                'schedule_type': 'Form 4562',
                'notes': f"Depreciation and Amortization for {tax_return.return_tax_year}",
                'line_items': {
                    'section_179_election': {
                        'total_cost_elected': float(section_179_deduction),
                        'threshold_reduction': 0,  # Would calculate based on asset purchases
                        'reduction_in_limitation': 0,
                        'deduction_allowed': float(section_179_deduction)
                    },
                    'special_depreciation_allowance': {
                        'bonus_depreciation': float(bonus_depreciation)
                    },
                    'macrs_depreciation': {
                        'regular_depreciation': float(regular_depreciation)
                    },
                    'amortization': {
                        'current_year': float(amortization)
                    },
                    'asset_details': asset_details,
                    'total_depreciation': float(total_depreciation)
                },
                'depreciation_amount': total_depreciation,
                'section179_amount': section_179_deduction,
                'net_amount': total_depreciation
            }
            
            # Create the schedule
            success, result = self.create_schedule(schedule_data)
            return success, result
            
        except Exception as e:
            return False, str(e)
            
    def validate_schedule_completeness(self, return_id: int) -> Dict:
        """Validate schedule completeness for tax return"""
        try:
            # Get tax return
            tax_return = self.db.query(IrsTaxReturnRec).filter(
                IrsTaxReturnRec.return_id == return_id
            ).first()
            
            if not tax_return:
                return {'error': 'Tax return not found'}
                
            # Get existing schedules
            schedules = self.db.query(IrsScheduleRec).filter(
                IrsScheduleRec.schedule_return_id == return_id
            ).all()
            
            existing_schedules = [s.schedule_type for s in schedules]
            
            # Determine required schedules based on form type and business activities
            required_schedules = []
            recommended_schedules = []
            
            if tax_return.return_form_type == '1040':
                # Individual return schedules
                if tax_return.return_gross_receipts > 0:
                    required_schedules.append('Schedule C')
                    
            elif tax_return.return_form_type == '1120':
                # Corporate return schedules
                recommended_schedules.append('Form 4562')  # Depreciation
                
            elif tax_return.return_form_type == '1120S':
                # S-Corp return schedules
                required_schedules.append('Schedule K-1')
                recommended_schedules.append('Form 4562')
                
            elif tax_return.return_form_type == '1065':
                # Partnership return schedules
                required_schedules.append('Schedule K-1')
                recommended_schedules.append('Form 4562')
                
            # Check for depreciation requirements
            has_depreciation = self.db.query(IrsDepreciationRec).filter(
                and_(
                    IrsDepreciationRec.depr_company_code == tax_return.return_company_code,
                    IrsDepreciationRec.depr_tax_year == tax_return.return_tax_year
                )
            ).first()
            
            if has_depreciation and 'Form 4562' not in required_schedules:
                required_schedules.append('Form 4562')
                
            # Determine missing schedules
            missing_required = [s for s in required_schedules if s not in existing_schedules]
            missing_recommended = [s for s in recommended_schedules if s not in existing_schedules]
            
            # Check schedule status
            incomplete_schedules = [
                s.schedule_type for s in schedules 
                if s.schedule_status == 'DRAFT'
            ]
            
            validation_result = {
                'return_id': return_id,
                'form_type': tax_return.return_form_type,
                'existing_schedules': existing_schedules,
                'required_schedules': required_schedules,
                'recommended_schedules': recommended_schedules,
                'missing_required': missing_required,
                'missing_recommended': missing_recommended,
                'incomplete_schedules': incomplete_schedules,
                'is_complete': len(missing_required) == 0 and len(incomplete_schedules) == 0,
                'completion_percentage': (
                    len(existing_schedules) / len(required_schedules + recommended_schedules) * 100
                    if required_schedules or recommended_schedules else 100
                )
            }
            
            return validation_result
            
        except Exception as e:
            return {'error': str(e)}
            
    def _generate_schedule_content(self, schedule_type: str, schedule_data: Dict, tax_return: IrsTaxReturnRec) -> Dict:
        """Generate schedule content based on type"""
        content = {
            'line_items': schedule_data.get('line_items', {}),
            'total_income': Decimal(str(schedule_data.get('total_income', 0))),
            'total_expenses': Decimal(str(schedule_data.get('total_expenses', 0))),
            'net_amount': Decimal(str(schedule_data.get('net_amount', 0)))
        }
        
        if schedule_type == 'Form 4562':
            content['depreciation_amount'] = Decimal(str(schedule_data.get('depreciation_amount', 0)))
            content['section179_amount'] = Decimal(str(schedule_data.get('section179_amount', 0)))
            
        elif schedule_type == 'Schedule C':
            content['other_deductions'] = Decimal(str(schedule_data.get('other_deductions', 0)))
            
        return content
        
    def _format_date(self, date_int: int) -> str:
        """Format integer date to readable format"""
        if not date_int:
            return ''
        try:
            date_str = str(date_int)
            return f"{date_str[4:6]}/{date_str[6:8]}/{date_str[:4]}"
        except:
            return str(date_int)