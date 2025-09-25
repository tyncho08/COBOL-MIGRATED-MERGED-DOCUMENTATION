"""
IRS Company Configuration Service - IRS010 migration
Handles IRS company setup and configuration
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from datetime import datetime
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func, desc

from app.services.file_handlers.system_handler import SystemFileHandler
from app.models.irs import IrsCompanyConfigRec
from app.core.security import log_user_action
from app.models.auth import User


class IrsCompanyConfigService:
    """
    IRS Company Configuration Service
    Implements IRS010 - company configuration for IRS compliance
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.system_handler = SystemFileHandler(db)
        
    def create_company_config(self, config_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Create IRS company configuration
        Returns (success, error_message or config_data)
        """
        company_code = config_data.get('company_code')
        company_name = config_data.get('company_name')
        ein = config_data.get('ein')
        tax_year = config_data.get('tax_year', datetime.now().year)
        accounting_method = config_data.get('accounting_method', 'ACCRUAL')
        business_type = config_data.get('business_type', 'CORP')
        
        if not company_code or not company_name or not ein:
            return False, "Company code, name, and EIN are required"
            
        # Validate EIN format (XX-XXXXXXX)
        if not self._validate_ein(ein):
            return False, "Invalid EIN format (must be XX-XXXXXXX)"
            
        try:
            # Check if company already exists
            existing = self.db.query(IrsCompanyConfigRec).filter(
                IrsCompanyConfigRec.config_company_code == company_code
            ).first()
            
            if existing:
                return False, f"Company {company_code} already configured"
                
            # Create configuration record
            config = IrsCompanyConfigRec(
                config_company_code=company_code,
                config_company_name=company_name,
                config_ein=ein,
                config_tax_year=tax_year,
                config_accounting_method=accounting_method,
                config_business_type=business_type,
                config_fiscal_year_end=config_data.get('fiscal_year_end', '1231'),
                config_address_line1=config_data.get('address_line1', ''),
                config_address_line2=config_data.get('address_line2', ''),
                config_city=config_data.get('city', ''),
                config_state=config_data.get('state', ''),
                config_zip_code=config_data.get('zip_code', ''),
                config_phone=config_data.get('phone', ''),
                config_fax=config_data.get('fax', ''),
                config_email=config_data.get('email', ''),
                config_preparer_name=config_data.get('preparer_name', ''),
                config_preparer_ein=config_data.get('preparer_ein', ''),
                config_preparer_phone=config_data.get('preparer_phone', ''),
                config_quarterly_filing=config_data.get('quarterly_filing', 'Y'),
                config_electronic_filing=config_data.get('electronic_filing', 'Y'),
                config_estimated_payments=config_data.get('estimated_payments', 'Y'),
                config_created_date=int(datetime.now().strftime("%Y%m%d")),
                config_created_by=self.current_user.username if self.current_user else 'SYSTEM'
            )
            
            self.db.add(config)
            self.db.commit()
            
            # Log creation
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="CREATE_IRS_CONFIG",
                    table="irs_company_config_rec",
                    key=company_code,
                    new_values={
                        'company_name': company_name,
                        'ein': ein,
                        'business_type': business_type,
                        'tax_year': tax_year
                    },
                    module="IRS"
                )
                
            return True, {
                'company_code': company_code,
                'company_name': company_name,
                'ein': ein,
                'tax_year': tax_year,
                'accounting_method': accounting_method,
                'business_type': business_type
            }
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def update_company_config(self, company_code: str, update_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Update IRS company configuration
        Returns (success, error_message)
        """
        config = self.db.query(IrsCompanyConfigRec).filter(
            IrsCompanyConfigRec.config_company_code == company_code
        ).first()
        
        if not config:
            return False, f"Company {company_code} not found"
            
        try:
            # Store old values for audit
            old_values = {
                'company_name': config.config_company_name,
                'ein': config.config_ein,
                'accounting_method': config.config_accounting_method,
                'business_type': config.config_business_type
            }
            
            # Update allowed fields
            updateable_fields = [
                'company_name', 'address_line1', 'address_line2', 'city', 'state',
                'zip_code', 'phone', 'fax', 'email', 'preparer_name', 'preparer_ein',
                'preparer_phone', 'quarterly_filing', 'electronic_filing', 'estimated_payments',
                'accounting_method', 'fiscal_year_end'
            ]
            
            new_values = {}
            for field, value in update_data.items():
                if field in updateable_fields:
                    setattr(config, f'config_{field}', value)
                    new_values[field] = value
                    
            # Validate EIN if being updated
            if 'ein' in update_data:
                if not self._validate_ein(update_data['ein']):
                    return False, "Invalid EIN format (must be XX-XXXXXXX)"
                config.config_ein = update_data['ein']
                new_values['ein'] = update_data['ein']
                
            config.config_updated_date = int(datetime.now().strftime("%Y%m%d"))
            config.config_updated_by = self.current_user.username if self.current_user else 'SYSTEM'
            
            self.db.commit()
            
            # Log update
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="UPDATE_IRS_CONFIG",
                    table="irs_company_config_rec",
                    key=company_code,
                    old_values=old_values,
                    new_values=new_values,
                    module="IRS"
                )
                
            return True, f"Company {company_code} configuration updated"
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def get_company_config(self, company_code: str) -> Optional[Dict]:
        """Get IRS company configuration"""
        config = self.db.query(IrsCompanyConfigRec).filter(
            IrsCompanyConfigRec.config_company_code == company_code
        ).first()
        
        if not config:
            return None
            
        return {
            'company_code': config.config_company_code,
            'company_name': config.config_company_name,
            'ein': config.config_ein,
            'tax_year': config.config_tax_year,
            'accounting_method': config.config_accounting_method,
            'business_type': config.config_business_type,
            'fiscal_year_end': config.config_fiscal_year_end,
            'address': {
                'line1': config.config_address_line1,
                'line2': config.config_address_line2,
                'city': config.config_city,
                'state': config.config_state,
                'zip_code': config.config_zip_code
            },
            'contact': {
                'phone': config.config_phone,
                'fax': config.config_fax,
                'email': config.config_email
            },
            'preparer': {
                'name': config.config_preparer_name,
                'ein': config.config_preparer_ein,
                'phone': config.config_preparer_phone
            },
            'settings': {
                'quarterly_filing': config.config_quarterly_filing == 'Y',
                'electronic_filing': config.config_electronic_filing == 'Y',
                'estimated_payments': config.config_estimated_payments == 'Y'
            },
            'audit_info': {
                'created_date': config.config_created_date,
                'created_by': config.config_created_by,
                'updated_date': config.config_updated_date,
                'updated_by': config.config_updated_by
            }
        }
        
    def get_all_companies(self) -> List[Dict]:
        """Get all configured companies"""
        configs = self.db.query(IrsCompanyConfigRec).order_by(
            IrsCompanyConfigRec.config_company_code
        ).all()
        
        return [
            {
                'company_code': config.config_company_code,
                'company_name': config.config_company_name,
                'ein': config.config_ein,
                'tax_year': config.config_tax_year,
                'business_type': config.config_business_type,
                'accounting_method': config.config_accounting_method,
                'electronic_filing': config.config_electronic_filing == 'Y',
                'created_date': config.config_created_date
            }
            for config in configs
        ]
        
    def validate_tax_year_setup(self, company_code: str, tax_year: int) -> Tuple[bool, Dict]:
        """
        Validate company setup for tax year processing
        Returns (is_valid, validation_results)
        """
        config = self.db.query(IrsCompanyConfigRec).filter(
            IrsCompanyConfigRec.config_company_code == company_code
        ).first()
        
        if not config:
            return False, {'error': 'Company not found'}
            
        validation_results = {
            'company_info_complete': True,
            'address_complete': True,
            'ein_valid': True,
            'preparer_info_complete': True,
            'ready_for_processing': True,
            'warnings': [],
            'errors': []
        }
        
        # Validate required company information
        if not all([config.config_company_name, config.config_ein, config.config_business_type]):
            validation_results['company_info_complete'] = False
            validation_results['errors'].append('Company name, EIN, and business type are required')
            
        # Validate address information
        if not all([config.config_address_line1, config.config_city, config.config_state, config.config_zip_code]):
            validation_results['address_complete'] = False
            validation_results['warnings'].append('Complete address information recommended for tax filings')
            
        # Validate EIN format
        if not self._validate_ein(config.config_ein):
            validation_results['ein_valid'] = False
            validation_results['errors'].append('Invalid EIN format')
            
        # Check preparer information if electronic filing enabled
        if config.config_electronic_filing == 'Y':
            if not config.config_preparer_name:
                validation_results['preparer_info_complete'] = False
                validation_results['warnings'].append('Preparer information recommended for electronic filing')
                
        # Check tax year consistency
        if config.config_tax_year != tax_year:
            validation_results['warnings'].append(f'Configuration tax year ({config.config_tax_year}) differs from requested year ({tax_year})')
            
        # Overall readiness
        validation_results['ready_for_processing'] = (
            validation_results['company_info_complete'] and 
            validation_results['ein_valid'] and
            len(validation_results['errors']) == 0
        )
        
        return True, validation_results
        
    def generate_company_summary_report(self, company_code: str = None) -> Dict:
        """Generate company configuration summary report"""
        try:
            if company_code:
                configs = self.db.query(IrsCompanyConfigRec).filter(
                    IrsCompanyConfigRec.config_company_code == company_code
                ).all()
            else:
                configs = self.db.query(IrsCompanyConfigRec).all()
                
            if not configs:
                return {'error': 'No companies found'}
                
            summary = {
                'report_date': datetime.now().isoformat(),
                'total_companies': len(configs),
                'companies': [],
                'statistics': {
                    'by_business_type': {},
                    'by_accounting_method': {},
                    'electronic_filing_enabled': 0,
                    'quarterly_filing_enabled': 0,
                    'estimated_payments_enabled': 0
                }
            }
            
            for config in configs:
                company_data = {
                    'company_code': config.config_company_code,
                    'company_name': config.config_company_name,
                    'ein': config.config_ein,
                    'tax_year': config.config_tax_year,
                    'business_type': config.config_business_type,
                    'accounting_method': config.config_accounting_method,
                    'fiscal_year_end': config.config_fiscal_year_end,
                    'electronic_filing': config.config_electronic_filing == 'Y',
                    'quarterly_filing': config.config_quarterly_filing == 'Y',
                    'estimated_payments': config.config_estimated_payments == 'Y',
                    'created_date': config.config_created_date,
                    'last_updated': config.config_updated_date
                }
                
                summary['companies'].append(company_data)
                
                # Update statistics
                business_type = config.config_business_type
                summary['statistics']['by_business_type'][business_type] = \
                    summary['statistics']['by_business_type'].get(business_type, 0) + 1
                    
                accounting_method = config.config_accounting_method
                summary['statistics']['by_accounting_method'][accounting_method] = \
                    summary['statistics']['by_accounting_method'].get(accounting_method, 0) + 1
                    
                if config.config_electronic_filing == 'Y':
                    summary['statistics']['electronic_filing_enabled'] += 1
                if config.config_quarterly_filing == 'Y':
                    summary['statistics']['quarterly_filing_enabled'] += 1
                if config.config_estimated_payments == 'Y':
                    summary['statistics']['estimated_payments_enabled'] += 1
                    
            return summary
            
        except Exception as e:
            return {'error': str(e)}
            
    def _validate_ein(self, ein: str) -> bool:
        """Validate EIN format (XX-XXXXXXX)"""
        if not ein or len(ein) != 10:
            return False
            
        if ein[2] != '-':
            return False
            
        # Check if first 2 and last 7 characters are digits
        return ein[:2].isdigit() and ein[3:].isdigit()
        
    def _validate_zip_code(self, zip_code: str) -> bool:
        """Validate US ZIP code format"""
        if not zip_code:
            return True  # Optional field
            
        # Standard ZIP (XXXXX) or ZIP+4 (XXXXX-XXXX)
        if len(zip_code) == 5:
            return zip_code.isdigit()
        elif len(zip_code) == 10:
            return zip_code[:5].isdigit() and zip_code[5] == '-' and zip_code[6:].isdigit()
        else:
            return False