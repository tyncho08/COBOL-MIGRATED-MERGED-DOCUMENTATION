"""
Stock Services Module
Provides compatibility layer for field mappings
"""

def create_stock_wrapper(stock_rec):
    """
    Create a wrapper for StockRec that provides backward-compatible field names
    """
    if not stock_rec:
        return None
        
    # Create a wrapper class dynamically
    class StockWrapper:
        def __init__(self, rec):
            self._rec = rec
            
        # Map old field names to new field names
        @property
        def stock_code(self):
            return self._rec.stock_key
            
        @property
        def stock_on_hand(self):
            return self._rec.stock_qty_on_hand
            
        @property
        def stock_allocated(self):
            return self._rec.stock_qty_allocated
            
        @property
        def stock_on_order(self):
            return self._rec.stock_qty_on_order
            
        @property
        def stock_back_order(self):
            return self._rec.stock_qty_back_order
            
        @property
        def stock_available(self):
            return self._rec.stock_qty_available
            
        @property
        def stock_average_cost(self):
            return self._rec.stock_avg_cost
            
        @property
        def stock_standard_cost(self):
            return self._rec.stock_std_cost
            
        @property
        def stock_last_cost(self):
            return self._rec.stock_last_cost
            
        # Pass through all other attributes
        def __getattr__(self, name):
            return getattr(self._rec, name)
            
        def __setattr__(self, name, value):
            if name == '_rec':
                super().__setattr__(name, value)
            elif name == 'stock_code':
                self._rec.stock_key = value
            elif name == 'stock_on_hand':
                self._rec.stock_qty_on_hand = value
            elif name == 'stock_allocated':
                self._rec.stock_qty_allocated = value
            elif name == 'stock_on_order':
                self._rec.stock_qty_on_order = value
            elif name == 'stock_back_order':
                self._rec.stock_qty_back_order = value
            elif name == 'stock_available':
                self._rec.stock_qty_available = value
            elif name == 'stock_average_cost':
                self._rec.stock_avg_cost = value
            elif name == 'stock_standard_cost':
                self._rec.stock_std_cost = value
            elif name == 'stock_last_cost':
                self._rec.stock_last_cost = value
            else:
                setattr(self._rec, name, value)
    
    return StockWrapper(stock_rec)