"""
ACAS Business Services
Core business logic services for the ACAS migration
"""

from .business_logic import (
    TaxCalculationService,
    DiscountCalculationService, 
    CreditControlService,
    StockCostingService
)
from .auth_service import AuthService
from .system_service import SystemService

# Import services from organized modules
try:
    from .sl.customer_master import CustomerService
except ImportError:
    CustomerService = None

try:
    from .pl.supplier_master import SupplierService  
except ImportError:
    SupplierService = None

try:
    from .stock.stock_master import StockService
except ImportError:
    StockService = None

try:
    from .gl.chart_of_accounts import GLService
except ImportError:
    GLService = None

__all__ = [
    "TaxCalculationService",
    "DiscountCalculationService", 
    "CreditControlService",
    "StockCostingService",
    "AuthService",
    "SystemService"
]

# Add available services to __all__
if CustomerService:
    __all__.append("CustomerService")
if SupplierService:
    __all__.append("SupplierService") 
if StockService:
    __all__.append("StockService")
if GLService:
    __all__.append("GLService")