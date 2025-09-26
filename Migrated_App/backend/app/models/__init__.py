"""
ACAS SQLAlchemy Models
Database models for the ACAS migration
"""

# Import all models to ensure they are registered with SQLAlchemy
from .system import SystemRec
from .customer import SalesLedgerRec, SalesInvoiceRec, SalesInvoiceLineRec, SalesItemRec
from .sales import (
    SalesOpenItemRec, SalesOrderRec, SalesOrderLineRec, SalesAllocationRec,
    SalesReceiptRec, SalesHistoryRec, DeliveryNoteRec, DeliveryNoteLineRec,
    DespatchRec, DespatchLineRec, LoadingAdviceRec
)
from .supplier import PurchaseLedgerRec, PurchaseInvoiceRec, PurchaseInvoiceLineRec, PurchaseItemRec, SupplierContactRec, SupplierBankRec, PurchaseOpenItemRec, PurchaseOrderRec, PurchaseOrderLineRec
from .stock import (
    StockRec, StockAuditRec, StockMasterRec, StockLocationRec, StockMovementRec,
    StockBinRec, PickListRec, PickListLineRec, StockSupplierRec, StockPriceRec, 
    StockBarcodeRec, StockTransferRec, StockAdjustmentRec, StockCountRec,
    StockValuationRec, StockRevalRec, StockItem, StockLocation, StockMovement
)
from .gl_accounts import GLLedgerRec, GLPostingRec, GLBatchRec, GLPeriodStatusRec, GLBudgetRec
from .payments import PurchasePaymentRec, PurchasePaymentLineRec
from .auth import User, Role, UserSession, UserPreference, SystemLock, Permission, UserRole, RolePermission
from .audit import AuditLog
from .gl import GLAccount, JournalEntry, JournalLine, GLPosting, AccountingPeriod, Budget, BudgetLine, RecurringJournal
from .irs import (
    IrsCompanyConfigRec, IrsTransactionRec, IrsBankReconciliationRec, IrsTaxCalculationRec,
    IrsTaxTableRec, IrsTaxReturnRec, IrsScheduleRec, IrsEstimatedPaymentRec,
    IrsFiscalCloseRec, IrsDepreciationRec, IrsAuditTrailRec, IrsElectronicFileRec
)
from .warehouse import (
    WarehouseRec, WarehouseZoneRec, BinTypeRec, BinLocationRec, BinCapacityRec
)

__all__ = [
    "SystemRec",
    "SalesLedgerRec", 
    "SalesInvoiceRec",
    "SalesInvoiceLineRec", 
    "SalesItemRec",
    "SalesOpenItemRec",
    "SalesOrderRec", 
    "SalesOrderLineRec",
    "SalesAllocationRec",
    "SalesReceiptRec",
    "SalesHistoryRec",
    "DeliveryNoteRec",
    "DeliveryNoteLineRec",
    "DespatchRec",
    "DespatchLineRec", 
    "LoadingAdviceRec",
    "PurchaseLedgerRec",
    "PurchaseInvoiceRec",
    "PurchaseInvoiceLineRec",
    "PurchaseItemRec",
    "SupplierContactRec", 
    "SupplierBankRec",
    "PurchaseOpenItemRec",
    "PurchaseOrderRec",
    "PurchaseOrderLineRec", 
    "StockRec",
    "StockAuditRec", 
    "StockMasterRec",
    "StockLocationRec",
    "StockMovementRec",
    "StockBinRec",
    "PickListRec",
    "PickListLineRec",
    "StockSupplierRec",
    "StockPriceRec", 
    "StockBarcodeRec",
    "StockTransferRec",
    "StockAdjustmentRec", 
    "StockCountRec",
    "StockValuationRec",
    "StockRevalRec",
    "StockItem",
    "StockLocation", 
    "StockMovement",
    "GLLedgerRec",
    "GLPostingRec", 
    "GLBatchRec",
    "GLPeriodStatusRec",
    "GLBudgetRec",
    "PurchasePaymentRec",
    "PurchasePaymentLineRec",
    "User",
    "Role",
    "Permission",
    "UserRole", 
    "RolePermission",
    "UserSession",
    "UserPreference",
    "SystemLock",
    "AuditLog",
    "GLAccount",
    "JournalEntry",
    "JournalLine", 
    "GLPosting",
    "AccountingPeriod",
    "Budget",
    "BudgetLine",
    "RecurringJournal",
    # IRS Models
    "IrsCompanyConfigRec",
    "IrsTransactionRec", 
    "IrsBankReconciliationRec",
    "IrsTaxCalculationRec",
    "IrsTaxTableRec",
    "IrsTaxReturnRec",
    "IrsScheduleRec",
    "IrsEstimatedPaymentRec",
    "IrsFiscalCloseRec",
    "IrsDepreciationRec",
    "IrsAuditTrailRec",
    "IrsElectronicFileRec",
    # Warehouse Models
    "WarehouseRec",
    "WarehouseZoneRec",
    "BinTypeRec",
    "BinLocationRec",
    "BinCapacityRec"
]