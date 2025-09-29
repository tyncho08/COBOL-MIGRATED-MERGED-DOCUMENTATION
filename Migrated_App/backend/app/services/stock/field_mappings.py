"""
Stock Module Field Mappings
Maps service field names to actual model field names
"""

# StockRec field mappings (service name -> model name)
STOCK_FIELD_MAP = {
    'stock_code': 'stock_key',
    'stock_on_hand': 'stock_qty_on_hand',
    'stock_allocated': 'stock_qty_allocated',
    'stock_on_order': 'stock_qty_on_order',
    'stock_back_order': 'stock_qty_back_order',
    'stock_available': 'stock_qty_available',
}

# StockLocationRec field mappings
LOCATION_FIELD_MAP = {
    'loc_stock_code': 'stock_key',
    'loc_warehouse': 'warehouse_code',
    'loc_location': 'location_code',
    'loc_qty_on_hand': 'quantity_on_hand',
    'loc_qty_allocated': 'quantity_allocated',
    'loc_primary': 'is_primary',
}

# StockMovementRec field mappings
MOVEMENT_FIELD_MAP = {
    'move_stock_code': 'stock_key',
    'move_type': 'movement_type',
    'move_date': 'movement_date',
    'move_qty_in': 'quantity',  # Note: single quantity field
    'move_qty_out': 'quantity',  # Will need logic to handle +/-
}

# StockAdjustmentRec field mappings
ADJUSTMENT_FIELD_MAP = {
    'adj_no': 'adjustment_number',
    'adj_stock_code': 'stock_key',
    'adj_warehouse': 'warehouse_code',
    'adj_location': 'location_code',
    'adj_date': 'adjustment_date',
    'adj_qty': 'adjustment_quantity',
    'adj_reason': 'reason_code',
}

# StockTransferRec field mappings
TRANSFER_FIELD_MAP = {
    'trans_no': 'transfer_number',
    'trans_stock_code': 'stock_key',
    'trans_from_warehouse': 'from_warehouse',
    'trans_from_location': 'from_location',
    'trans_to_warehouse': 'to_warehouse',
    'trans_to_location': 'to_location',
    'trans_qty': 'transfer_quantity',
    'trans_date': 'transfer_date',
}

# StockCountRec field mappings
COUNT_FIELD_MAP = {
    'count_no': 'count_number',
    'count_stock_code': 'stock_key',
    'count_warehouse': 'warehouse_code',
    'count_location': 'location_code',
    'count_date': 'count_date',
    'count_qty': 'counted_quantity',
    'count_variance': 'variance_quantity',
}

def map_field(obj, field_map, field_name, default=None):
    """
    Map a field name from service to model
    
    Args:
        obj: The model object
        field_map: Dictionary mapping service names to model names
        field_name: The service field name
        default: Default value if field not found
        
    Returns:
        The field value from the model
    """
    model_field = field_map.get(field_name, field_name)
    return getattr(obj, model_field, default)

def set_mapped_field(obj, field_map, field_name, value):
    """
    Set a field value using field mapping
    
    Args:
        obj: The model object
        field_map: Dictionary mapping service names to model names
        field_name: The service field name
        value: The value to set
    """
    model_field = field_map.get(field_name, field_name)
    if hasattr(obj, model_field):
        setattr(obj, model_field, value)

class FieldMapper:
    """Helper class to wrap model objects with field mapping"""
    
    def __init__(self, obj, field_map):
        self._obj = obj
        self._field_map = field_map
        
    def __getattr__(self, name):
        """Get field value with mapping"""
        model_field = self._field_map.get(name, name)
        return getattr(self._obj, model_field)
        
    def __setattr__(self, name, value):
        """Set field value with mapping"""
        if name.startswith('_'):
            super().__setattr__(name, value)
        else:
            model_field = self._field_map.get(name, name)
            setattr(self._obj, model_field, value)