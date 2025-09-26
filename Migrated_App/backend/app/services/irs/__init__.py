"""
IRS Module Services - Phase 8 migration
Tax processing, compliance and reporting services
"""

from .company_config import IrsCompanyConfigService
from .transaction_entry import IrsTransactionService
from .bank_reconciliation import IrsBankReconciliationService
from .tax_calculations import IrsTaxCalculationService
from .tax_tables import IrsTaxTableService
from .tax_return import IrsTaxReturnService
from .schedule_service import IrsScheduleService
from .estimated_payment_service import IrsEstimatedPaymentService
from .fiscal_close import IrsFiscalCloseService
from .depreciation_service import IrsDepreciationService
from .audit_trail_service import AuditTrailService
from .electronic_filing_service import ElectronicFilingService

__all__ = [
    'IrsCompanyConfigService',
    'IrsTransactionService', 
    'IrsBankReconciliationService',
    'IrsTaxCalculationService',
    'IrsTaxTableService',
    'TaxReturnService',
    'ScheduleService',
    'EstimatedPaymentService',
    'IrsFiscalCloseService',
    'DepreciationService',
    'AuditTrailService',
    'ElectronicFilingService'
]