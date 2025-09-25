"""
COBOL File Handlers
Exact migration of acas000-032 file handling logic
"""
from typing import Optional, Dict, Type
from sqlalchemy.orm import Session

from .base_handler import BaseFileHandler, FileFunction, FileStatus, COBOLError, FileAccess
from .system_handler import SystemFileHandler
from .customer_handler import CustomerFileHandler
from .stock_handler import StockFileHandler
from .gl_handler import GLFileHandler
from .supplier_handler import SupplierFileHandler
from .open_items_handler import SalesOpenItemsHandler, PurchaseOpenItemsHandler


class FileHandlerFactory:
    """Factory to get the appropriate file handler based on file number"""
    
    # Map of file numbers to handler classes
    _handlers: Dict[int, Type[BaseFileHandler]] = {
        0: SystemFileHandler,     # acas000 - System file (was file-10)
        4: CustomerFileHandler,   # acas004 - Customer/Sales Ledger
        5: SupplierFileHandler,   # acas005 - Supplier/Purchase Ledger
        10: GLFileHandler,        # acas010 - General Ledger
        11: StockFileHandler,     # acas011 - Stock/Inventory
        13: SalesOpenItemsHandler,    # acas013 - Sales Open Items
        15: PurchaseOpenItemsHandler, # acas015 - Purchase Open Items
    }
    
    @classmethod
    def get_handler(cls, file_number: int, db: Session) -> Optional[BaseFileHandler]:
        """
        Get file handler instance for given file number
        
        Args:
            file_number: COBOL file number
            db: Database session
            
        Returns:
            File handler instance or None if not found
        """
        handler_class = cls._handlers.get(file_number)
        if handler_class:
            return handler_class(db)
        return None
        
    @classmethod
    def register_handler(cls, file_number: int, handler_class: Type[BaseFileHandler]):
        """Register a new file handler"""
        cls._handlers[file_number] = handler_class


# Export main classes and constants
__all__ = [
    'BaseFileHandler',
    'SystemFileHandler', 
    'CustomerFileHandler',
    'SupplierFileHandler',
    'StockFileHandler',
    'GLFileHandler',
    'SalesOpenItemsHandler',
    'PurchaseOpenItemsHandler',
    'FileHandlerFactory',
    'FileFunction',
    'FileStatus',
    'COBOLError',
    'FileAccess'
]