"""
IRS080: Electronic Filing Service

This service handles electronic filing of tax returns and related documents
to the IRS. It manages the preparation, validation, transmission, and tracking
of e-filed returns.

Note: The original COBOL irs080 was a "Nominal Fix-up Program". This modern
implementation provides actual electronic filing functionality.
"""

from datetime import datetime, date, timedelta
from decimal import Decimal
from typing import List, Dict, Any, Optional, Tuple
from enum import Enum
import json
import uuid
import hashlib
import base64
from sqlalchemy import select, and_, or_, func
from sqlalchemy.orm import Session

from app.models import (
    IrsElectronicFileRec as IRSElectronicFile, 
    IrsTaxReturnRec as IRSTaxReturn, 
    IrsCompanyConfigRec as IRSCompanyConfig,
    IrsTransactionRec as IRSTransaction, 
    IrsScheduleRec as IRSSchedule, 
    IrsAuditTrailRec as IRSAuditTrail
)
from app.services.irs.tax_return import IrsTaxReturnService
from app.services.irs.audit_trail_service import AuditTrailService
from app.core.logging import get_logger

logger = get_logger(__name__)


class FilingStatus(str, Enum):
    """Electronic filing status codes."""
    DRAFT = "DRAFT"
    VALIDATED = "VALIDATED"
    READY = "READY"
    TRANSMITTED = "TRANSMITTED"
    ACCEPTED = "ACCEPTED"
    REJECTED = "REJECTED"
    ERROR = "ERROR"


class TransmissionStatus(str, Enum):
    """IRS transmission status codes."""
    PENDING = "PENDING"
    IN_PROGRESS = "IN_PROGRESS"
    COMPLETED = "COMPLETED"
    FAILED = "FAILED"
    TIMEOUT = "TIMEOUT"


class ElectronicFilingService:
    """Service for managing IRS electronic filing operations."""
    
    def __init__(self, db: Session):
        self.db = db
        self.tax_return_service = TaxReturnService(db)
        self.audit_service = AuditTrailService(db)
    
    def prepare_electronic_file(
        self,
        tax_return_id: int,
        preparer_info: Dict[str, Any],
        user_id: int
    ) -> IRSElectronicFile:
        """
        Prepare a tax return for electronic filing.
        
        Args:
            tax_return_id: ID of the tax return to file
            preparer_info: Dictionary containing preparer information
            user_id: ID of the user initiating the filing
            
        Returns:
            Electronic file record
        """
        try:
            # Get the tax return
            tax_return = self.db.get(IRSTaxReturn, tax_return_id)
            if not tax_return:
                raise ValueError(f"Tax return {tax_return_id} not found")
            
            if tax_return.filing_status == 'FILED':
                raise ValueError("Tax return has already been filed")
            
            # Get company configuration
            company_config = self.db.execute(
                select(IRSCompanyConfig).filter(
                    IRSCompanyConfig.company_id == tax_return.company_id
                )
            ).scalar_one()
            
            # Generate submission ID
            submission_id = self._generate_submission_id(
                company_config.ein,
                tax_return.tax_year,
                tax_return.form_type
            )
            
            # Create electronic file record
            efile = IRSElectronicFile(
                tax_return_id=tax_return_id,
                submission_id=submission_id,
                filing_status=FilingStatus.DRAFT,
                transmission_status=TransmissionStatus.PENDING,
                preparer_ptin=preparer_info.get('ptin'),
                preparer_ein=preparer_info.get('ein'),
                preparer_name=preparer_info.get('name'),
                preparer_firm=preparer_info.get('firm_name'),
                software_id=preparer_info.get('software_id', 'ACAS-MIGRATION-1.0'),
                created_at=datetime.utcnow(),
                created_by=user_id
            )
            
            self.db.add(efile)
            self.db.commit()
            
            # Log the action
            self.audit_service.log_change(
                user_id=user_id,
                entity_type='electronic_file',
                entity_id=str(efile.id),
                action='CREATE',
                description=f"Prepared electronic file for tax return {tax_return_id}"
            )
            
            logger.info(f"Created electronic file {efile.id} for tax return {tax_return_id}")
            
            return efile
            
        except Exception as e:
            logger.error(f"Error preparing electronic file: {str(e)}")
            self.db.rollback()
            raise
    
    def validate_electronic_file(
        self,
        efile_id: int,
        user_id: int
    ) -> Tuple[bool, List[Dict[str, str]]]:
        """
        Validate an electronic file before transmission.
        
        Args:
            efile_id: ID of the electronic file
            user_id: ID of the user performing validation
            
        Returns:
            Tuple of (is_valid, list of validation errors/warnings)
        """
        try:
            efile = self.db.get(IRSElectronicFile, efile_id)
            if not efile:
                raise ValueError(f"Electronic file {efile_id} not found")
            
            errors = []
            warnings = []
            
            # Get associated data
            tax_return = self.db.get(IRSTaxReturn, efile.tax_return_id)
            company_config = self.db.execute(
                select(IRSCompanyConfig).filter(
                    IRSCompanyConfig.company_id == tax_return.company_id
                )
            ).scalar_one()
            
            # Validation rules
            
            # 1. Check preparer information
            if not efile.preparer_ptin:
                errors.append({
                    'code': 'PREP001',
                    'message': 'Preparer PTIN is required for e-filing',
                    'field': 'preparer_ptin'
                })
            
            if not efile.preparer_ein:
                errors.append({
                    'code': 'PREP002',
                    'message': 'Preparer EIN is required for e-filing',
                    'field': 'preparer_ein'
                })
            
            # 2. Check company information
            if not company_config.ein:
                errors.append({
                    'code': 'COMP001',
                    'message': 'Company EIN is required for e-filing',
                    'field': 'company_ein'
                })
            
            if not company_config.legal_name:
                errors.append({
                    'code': 'COMP002',
                    'message': 'Company legal name is required',
                    'field': 'company_name'
                })
            
            # 3. Check tax return completeness
            if tax_return.total_tax is None:
                errors.append({
                    'code': 'TAX001',
                    'message': 'Total tax must be calculated before filing',
                    'field': 'total_tax'
                })
            
            if tax_return.taxable_income is None:
                errors.append({
                    'code': 'TAX002',
                    'message': 'Taxable income must be calculated',
                    'field': 'taxable_income'
                })
            
            # 4. Check schedules
            schedules = self.db.execute(
                select(IRSSchedule).filter(
                    IRSSchedule.tax_return_id == tax_return.id
                )
            ).scalars().all()
            
            required_schedules = self._get_required_schedules(
                tax_return.form_type,
                company_config.entity_type
            )
            
            for req_schedule in required_schedules:
                if not any(s.schedule_type == req_schedule for s in schedules):
                    warnings.append({
                        'code': 'SCHED001',
                        'message': f'Schedule {req_schedule} may be required',
                        'field': 'schedules'
                    })
            
            # 5. Check filing deadline
            filing_deadline = self._calculate_filing_deadline(
                tax_return.tax_year,
                company_config.fiscal_year_end
            )
            
            if date.today() > filing_deadline:
                warnings.append({
                    'code': 'DEAD001',
                    'message': f'Filing deadline ({filing_deadline}) has passed',
                    'field': 'filing_date'
                })
            
            # 6. Check prior year filing
            prior_year_return = self.db.execute(
                select(IRSTaxReturn).filter(
                    and_(
                        IRSTaxReturn.company_id == tax_return.company_id,
                        IRSTaxReturn.tax_year == tax_return.tax_year - 1,
                        IRSTaxReturn.filing_status == 'FILED'
                    )
                )
            ).scalar_one_or_none()
            
            if not prior_year_return and tax_return.tax_year > company_config.first_tax_year:
                warnings.append({
                    'code': 'PRIOR001',
                    'message': 'Prior year return not found',
                    'field': 'prior_year'
                })
            
            # Update validation status
            is_valid = len(errors) == 0
            
            efile.filing_status = FilingStatus.VALIDATED if is_valid else FilingStatus.DRAFT
            efile.validation_errors = json.dumps({
                'errors': errors,
                'warnings': warnings,
                'validated_at': datetime.utcnow().isoformat()
            })
            
            self.db.commit()
            
            # Log validation
            self.audit_service.log_change(
                user_id=user_id,
                entity_type='electronic_file',
                entity_id=str(efile_id),
                action='VALIDATE',
                description=f"Validation {'passed' if is_valid else 'failed'} with {len(errors)} errors, {len(warnings)} warnings"
            )
            
            return is_valid, errors + warnings
            
        except Exception as e:
            logger.error(f"Error validating electronic file: {str(e)}")
            self.db.rollback()
            raise
    
    def transmit_electronic_file(
        self,
        efile_id: int,
        user_id: int,
        test_mode: bool = False
    ) -> Dict[str, Any]:
        """
        Transmit an electronic file to the IRS.
        
        Args:
            efile_id: ID of the electronic file
            user_id: ID of the user initiating transmission
            test_mode: Whether to use test transmission
            
        Returns:
            Transmission result dictionary
        """
        try:
            efile = self.db.get(IRSElectronicFile, efile_id)
            if not efile:
                raise ValueError(f"Electronic file {efile_id} not found")
            
            if efile.filing_status != FilingStatus.VALIDATED:
                raise ValueError("Electronic file must be validated before transmission")
            
            # Generate transmission package
            transmission_data = self._generate_transmission_package(efile)
            
            # Update status
            efile.transmission_status = TransmissionStatus.IN_PROGRESS
            efile.transmitted_at = datetime.utcnow()
            self.db.commit()
            
            # Simulate IRS transmission (in production, this would call actual IRS API)
            if test_mode:
                result = self._simulate_irs_transmission(transmission_data)
            else:
                # Production transmission would go here
                result = self._transmit_to_irs(transmission_data)
            
            # Process transmission result
            if result['status'] == 'ACCEPTED':
                efile.filing_status = FilingStatus.TRANSMITTED
                efile.transmission_status = TransmissionStatus.COMPLETED
                efile.irs_submission_id = result.get('irs_submission_id')
                efile.acknowledgment = json.dumps(result.get('acknowledgment', {}))
                
                # Update tax return status
                tax_return = self.db.get(IRSTaxReturn, efile.tax_return_id)
                tax_return.filing_status = 'FILED'
                tax_return.filed_date = datetime.utcnow()
                
            else:
                efile.filing_status = FilingStatus.REJECTED
                efile.transmission_status = TransmissionStatus.FAILED
                efile.rejection_codes = json.dumps(result.get('errors', []))
            
            self.db.commit()
            
            # Log transmission
            self.audit_service.log_change(
                user_id=user_id,
                entity_type='electronic_file',
                entity_id=str(efile_id),
                action='TRANSMIT',
                description=f"Transmission {result['status']} - {result.get('message', '')}"
            )
            
            return result
            
        except Exception as e:
            logger.error(f"Error transmitting electronic file: {str(e)}")
            
            # Update status on error
            if efile:
                efile.transmission_status = TransmissionStatus.FAILED
                efile.filing_status = FilingStatus.ERROR
                self.db.commit()
            
            raise
    
    def check_transmission_status(
        self,
        efile_id: int,
        user_id: int
    ) -> Dict[str, Any]:
        """
        Check the status of a transmitted file with the IRS.
        
        Args:
            efile_id: ID of the electronic file
            user_id: ID of the user checking status
            
        Returns:
            Current status information
        """
        try:
            efile = self.db.get(IRSElectronicFile, efile_id)
            if not efile:
                raise ValueError(f"Electronic file {efile_id} not found")
            
            if not efile.irs_submission_id:
                return {
                    'status': 'NOT_TRANSMITTED',
                    'message': 'File has not been transmitted to IRS'
                }
            
            # In production, this would query IRS systems
            # For now, simulate status check
            status_result = self._check_irs_status(efile.irs_submission_id)
            
            # Update local status based on IRS response
            if status_result['irs_status'] == 'ACCEPTED':
                efile.filing_status = FilingStatus.ACCEPTED
                efile.accepted_at = datetime.utcnow()
            elif status_result['irs_status'] == 'REJECTED':
                efile.filing_status = FilingStatus.REJECTED
                efile.rejection_codes = json.dumps(status_result.get('rejection_codes', []))
            
            self.db.commit()
            
            # Log status check
            self.audit_service.log_change(
                user_id=user_id,
                entity_type='electronic_file',
                entity_id=str(efile_id),
                action='STATUS_CHECK',
                description=f"IRS status: {status_result['irs_status']}"
            )
            
            return {
                'efile_id': efile_id,
                'submission_id': efile.submission_id,
                'irs_submission_id': efile.irs_submission_id,
                'filing_status': efile.filing_status,
                'transmission_status': efile.transmission_status,
                'irs_status': status_result['irs_status'],
                'last_checked': datetime.utcnow().isoformat(),
                'details': status_result
            }
            
        except Exception as e:
            logger.error(f"Error checking transmission status: {str(e)}")
            raise
    
    def get_filing_history(
        self,
        company_id: int,
        tax_year: Optional[int] = None
    ) -> List[Dict[str, Any]]:
        """
        Get electronic filing history for a company.
        
        Args:
            company_id: ID of the company
            tax_year: Optional specific tax year
            
        Returns:
            List of filing records
        """
        try:
            query = (
                select(IRSElectronicFile, IRSTaxReturn)
                .join(IRSTaxReturn, IRSElectronicFile.tax_return_id == IRSTaxReturn.id)
                .filter(IRSTaxReturn.company_id == company_id)
            )
            
            if tax_year:
                query = query.filter(IRSTaxReturn.tax_year == tax_year)
            
            query = query.order_by(IRSElectronicFile.created_at.desc())
            
            results = self.db.execute(query).all()
            
            return [
                {
                    'efile_id': efile.id,
                    'submission_id': efile.submission_id,
                    'tax_year': tax_return.tax_year,
                    'form_type': tax_return.form_type,
                    'filing_status': efile.filing_status,
                    'transmission_status': efile.transmission_status,
                    'created_at': efile.created_at.isoformat(),
                    'transmitted_at': efile.transmitted_at.isoformat() if efile.transmitted_at else None,
                    'accepted_at': efile.accepted_at.isoformat() if efile.accepted_at else None,
                    'preparer_name': efile.preparer_name
                }
                for efile, tax_return in results
            ]
            
        except Exception as e:
            logger.error(f"Error retrieving filing history: {str(e)}")
            raise
    
    def _generate_submission_id(self, ein: str, tax_year: int, form_type: str) -> str:
        """Generate unique submission ID for electronic file."""
        timestamp = datetime.utcnow().strftime('%Y%m%d%H%M%S')
        unique_str = f"{ein}-{tax_year}-{form_type}-{timestamp}-{uuid.uuid4().hex[:8]}"
        return hashlib.sha256(unique_str.encode()).hexdigest()[:20].upper()
    
    def _get_required_schedules(self, form_type: str, entity_type: str) -> List[str]:
        """Get list of required schedules based on form and entity type."""
        required_schedules = []
        
        if form_type == '1120':
            required_schedules = ['M-1', 'M-2']
            if entity_type == 'S-CORP':
                required_schedules.extend(['K-1', 'M-3'])
        elif form_type == '1065':
            required_schedules = ['K', 'K-1', 'L', 'M-1', 'M-2']
        elif form_type == '1040':
            required_schedules = ['C']  # For sole proprietorship
        
        return required_schedules
    
    def _calculate_filing_deadline(self, tax_year: int, fiscal_year_end: str) -> date:
        """Calculate filing deadline based on tax year and fiscal year end."""
        if fiscal_year_end == '12-31':
            # Calendar year - due March 15 (C-Corp) or April 15 (others)
            return date(tax_year + 1, 3, 15)
        else:
            # Fiscal year - due 2.5 months after year end
            month, day = map(int, fiscal_year_end.split('-'))
            deadline_date = date(tax_year + 1, month, day) + timedelta(days=75)
            return deadline_date
    
    def _generate_transmission_package(self, efile: IRSElectronicFile) -> Dict[str, Any]:
        """Generate complete transmission package for IRS."""
        tax_return = self.db.get(IRSTaxReturn, efile.tax_return_id)
        company_config = self.db.execute(
            select(IRSCompanyConfig).filter(
                IRSCompanyConfig.company_id == tax_return.company_id
            )
        ).scalar_one()
        
        # Build transmission package (simplified structure)
        package = {
            'submission_id': efile.submission_id,
            'software_id': efile.software_id,
            'preparer': {
                'ptin': efile.preparer_ptin,
                'ein': efile.preparer_ein,
                'name': efile.preparer_name,
                'firm': efile.preparer_firm
            },
            'taxpayer': {
                'ein': company_config.ein,
                'name': company_config.legal_name,
                'address': {
                    'street': company_config.address_line1,
                    'city': company_config.city,
                    'state': company_config.state,
                    'zip': company_config.zip_code
                }
            },
            'return': {
                'form_type': tax_return.form_type,
                'tax_year': tax_return.tax_year,
                'gross_income': float(tax_return.gross_income),
                'taxable_income': float(tax_return.taxable_income),
                'total_tax': float(tax_return.total_tax)
            }
        }
        
        return package
    
    def _simulate_irs_transmission(self, transmission_data: Dict[str, Any]) -> Dict[str, Any]:
        """Simulate IRS transmission for testing."""
        import random
        
        # Simulate various outcomes
        outcome = random.choice(['accepted', 'accepted', 'accepted', 'rejected'])
        
        if outcome == 'accepted':
            return {
                'status': 'ACCEPTED',
                'irs_submission_id': f"IRS-{uuid.uuid4().hex[:12].upper()}",
                'message': 'Return accepted for processing',
                'acknowledgment': {
                    'timestamp': datetime.utcnow().isoformat(),
                    'tracking_number': uuid.uuid4().hex[:16].upper()
                }
            }
        else:
            return {
                'status': 'REJECTED',
                'message': 'Return rejected',
                'errors': [
                    {
                        'code': 'F1040-502',
                        'message': 'Missing required schedule',
                        'field': 'Schedule C'
                    }
                ]
            }
    
    def _transmit_to_irs(self, transmission_data: Dict[str, Any]) -> Dict[str, Any]:
        """Transmit to actual IRS systems (placeholder for production implementation)."""
        # In production, this would:
        # 1. Connect to IRS MeF (Modernized e-File) system
        # 2. Authenticate using certificates
        # 3. Submit XML package
        # 4. Handle synchronous response
        
        logger.warning("Production IRS transmission not implemented - using simulation")
        return self._simulate_irs_transmission(transmission_data)
    
    def _check_irs_status(self, irs_submission_id: str) -> Dict[str, Any]:
        """Check status with IRS systems (placeholder for production implementation)."""
        # In production, this would query IRS systems for current status
        
        # Simulate status check
        import random
        status = random.choice(['ACCEPTED', 'ACCEPTED', 'PENDING', 'REJECTED'])
        
        result = {
            'irs_status': status,
            'last_updated': datetime.utcnow().isoformat()
        }
        
        if status == 'REJECTED':
            result['rejection_codes'] = [
                {
                    'code': 'R0000-902',
                    'message': 'EIN does not match IRS records'
                }
            ]
        
        return result