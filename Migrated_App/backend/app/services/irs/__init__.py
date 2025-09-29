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

# Export service instances/modules for API compatibility
company_config = IrsCompanyConfigService
transaction_entry = IrsTransactionService
bank_reconciliation = IrsBankReconciliationService
tax_calculations = IrsTaxCalculationService
tax_tables = IrsTaxTableService
tax_return = IrsTaxReturnService
schedule_service = IrsScheduleService
estimated_payment_service = IrsEstimatedPaymentService
fiscal_close = IrsFiscalCloseService
depreciation_service = IrsDepreciationService
audit_trail_service = AuditTrailService
electronic_filing_service = ElectronicFilingService

__all__ = [
    'IrsCompanyConfigService',
    'IrsTransactionService', 
    'IrsBankReconciliationService',
    'IrsTaxCalculationService',
    'IrsTaxTableService',
    'IrsTaxReturnService',
    'IrsScheduleService',
    'IrsEstimatedPaymentService',
    'IrsFiscalCloseService',
    'IrsDepreciationService',
    'AuditTrailService',
    'ElectronicFilingService',
    'company_config',
    'transaction_entry',
    'bank_reconciliation',
    'tax_calculations',
    'tax_tables',
    'tax_return',
    'schedule_service',
    'estimated_payment_service',
    'fiscal_close',
    'depreciation_service',
    'audit_trail_service',
    'electronic_filing_service'
]