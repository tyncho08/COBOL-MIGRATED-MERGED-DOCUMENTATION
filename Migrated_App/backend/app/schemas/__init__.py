"""
ACAS Pydantic Schemas
Request/Response models for the ACAS migration API
"""

# Import all schemas to make them available
from .system import SystemConfig, SystemConfigResponse, SystemConfigUpdate
from .customer import (
    CustomerBase, CustomerCreate, CustomerUpdate, CustomerResponse,
    SalesInvoiceBase, SalesInvoiceCreate, SalesInvoiceResponse,
    SalesInvoiceLineBase, SalesInvoiceLineCreate, SalesInvoiceLineResponse
)
from .supplier import (
    SupplierBase, SupplierCreate, SupplierUpdate, SupplierResponse,
    PurchaseInvoiceBase, PurchaseInvoiceCreate, PurchaseInvoiceResponse,
    PurchaseInvoiceLineBase, PurchaseInvoiceLineCreate, PurchaseInvoiceLineResponse
)
from .stock import (
    StockItemBase, StockItemCreate, StockItemUpdate, StockItem,
    StockMovementBase, StockMovementCreate, StockMovement
)
from .gl_accounts import (
    GLAccountBase, GLAccountCreate, GLAccountUpdate, GLAccountResponse,
    GLPostingBase, GLPostingCreate, GLPostingResponse
)
from .common import (
    PaginatedResponse, ErrorResponse, SuccessResponse,
    DateRange, AmountRange, StatusFilter
)

__all__ = [
    # System Configuration
    "SystemConfig",
    "SystemConfigResponse", 
    "SystemConfigUpdate",
    
    # Customer/Sales Ledger
    "CustomerBase",
    "CustomerCreate",
    "CustomerUpdate",
    "CustomerResponse",
    "SalesInvoiceBase",
    "SalesInvoiceCreate", 
    "SalesInvoiceResponse",
    "SalesInvoiceLineBase",
    "SalesInvoiceLineCreate",
    "SalesInvoiceLineResponse",
    
    # Supplier/Purchase Ledger
    "SupplierBase",
    "SupplierCreate",
    "SupplierUpdate",
    "SupplierResponse",
    "PurchaseInvoiceBase",
    "PurchaseInvoiceCreate",
    "PurchaseInvoiceResponse",
    "PurchaseInvoiceLineBase",
    "PurchaseInvoiceLineCreate", 
    "PurchaseInvoiceLineResponse",
    
    # Stock Control
    "StockItemBase",
    "StockItemCreate",
    "StockItemUpdate",
    "StockItemResponse",
    "StockMovementBase",
    "StockMovementCreate",
    "StockMovementResponse",
    
    # General Ledger
    "GLAccountBase",
    "GLAccountCreate",
    "GLAccountUpdate",
    "GLAccountResponse",
    "GLPostingBase",
    "GLPostingCreate",
    "GLPostingResponse",
    
    # Common/Utility
    "PaginatedResponse",
    "ErrorResponse",
    "SuccessResponse",
    "DateRange",
    "AmountRange",
    "StatusFilter"
]