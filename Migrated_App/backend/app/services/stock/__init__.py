"""Stock Control Services - COBOL Stock programs migration with WMS"""

from .stock_master import StockMasterService
from .stock_movements import StockMovementsService
from .stock_inquiry import StockInquiryService
from .stock_valuation import StockValuationService
from .bin_management import BinManagementService

# Create aliases for API compatibility
movement_service = StockMovementsService
master_service = StockMasterService
inquiry_service = StockInquiryService
valuation_service = StockValuationService
location_service = BinManagementService
order_service = StockMasterService  # Using stock master for order-related functions

__all__ = [
    "StockMasterService",
    "StockMovementsService", 
    "StockInquiryService",
    "StockValuationService",
    "BinManagementService",
    "movement_service",
    "master_service",
    "inquiry_service",
    "valuation_service",
    "location_service",
    "order_service"
]