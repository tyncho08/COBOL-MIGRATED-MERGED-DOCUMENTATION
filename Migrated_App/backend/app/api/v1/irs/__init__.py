"""IRS Module API endpoints"""

from .company_config import router as company_router
from .transaction_entry import router as transactions_router
from .bank_reconciliation import router as bank_reconciliation_router
from .tax_calculations import router as tax_calculations_router
from .tax_return import router as tax_returns_router
from .electronic_filing import router as electronic_filing_router
from .audit_trail import router as audit_trail_router

__all__ = [
    "company_router",
    "transactions_router",
    "bank_reconciliation_router", 
    "tax_calculations_router",
    "tax_returns_router",
    "electronic_filing_router",
    "audit_trail_router"
]