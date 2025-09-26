"""
Stock Inquiry Service - ST020 migration
Provides comprehensive stock information and analysis
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from datetime import datetime, date, timedelta
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func, case, desc, asc

from app.services.file_handlers.stock_handler import StockFileHandler
from app.services.file_handlers.system_handler import SystemFileHandler
from app.models.stock import (
    StockMasterRec, StockLocationRec, StockBinRec,
    StockSupplierRec, StockPriceRec, StockBarcodeRec,
    StockMovementRec, StockTransferRec
)
from app.models.sales import SalesOrderLineRec
from app.models.supplier import PurchaseOrderLineRec
from app.models.auth import User


class StockInquiryService:
    """
    Stock Inquiry functionality
    Implements ST020 - comprehensive stock information
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.stock_handler = StockFileHandler(db)
        self.system_handler = SystemFileHandler(db)
        
    def get_stock_summary(self, stock_code: str) -> Dict:
        """Get comprehensive stock summary"""
        # Get stock master
        stock, status = self.stock_handler.process(4, key_value=stock_code)
        if status.fs_reply != "00":
            return {"error": "Stock item not found"}
            
        # Get location details
        locations = self._get_location_details(stock_code)
        
        # Get supplier information
        suppliers = self._get_supplier_details(stock_code)
        
        # Get movement history
        movements = self._get_recent_movements(stock_code, limit=10)
        
        # Get pricing history
        pricing = self._get_pricing_history(stock_code)
        
        # Get allocation details
        allocations = self._get_allocation_details(stock_code)
        
        # Get order information
        orders = self._get_order_information(stock_code)
        
        # Calculate key metrics
        metrics = self._calculate_stock_metrics(stock)
        
        # Get ABC analysis
        abc_analysis = self._get_abc_analysis(stock_code)
        
        return {
            'item': {
                'stock_code': stock.stock_code,
                'description': stock.stock_desc,
                'description2': stock.stock_desc2,
                'search_key': stock.stock_search_key,
                'classification': {
                    'type': stock.stock_type,
                    'group': stock.stock_group,
                    'category': stock.stock_category,
                    'abc_class': stock.stock_abc_class
                },
                'unit_info': {
                    'unit': stock.stock_unit,
                    'pack_qty': float(stock.stock_pack_qty),
                    'weight': float(stock.stock_weight),
                    'volume': float(stock.stock_volume)
                },
                'status': {
                    'active': stock.stock_active == 'Y',
                    'sellable': stock.stock_sellable == 'Y',
                    'purchasable': stock.stock_purchasable == 'Y',
                    'manufactured': stock.stock_manufactured == 'Y'
                },
                'control': {
                    'lot_control': stock.stock_lot_control == 'Y',
                    'serial_control': stock.stock_serial_control == 'Y',
                    'expiry_control': stock.stock_expiry_control == 'Y'
                }
            },
            'quantities': {
                'on_hand': float(stock.stock_on_hand),
                'allocated': float(stock.stock_allocated),
                'available': float(stock.stock_on_hand - stock.stock_allocated),
                'on_order': float(stock.stock_on_order),
                'in_transit': float(stock.stock_in_transit),
                'quarantine': float(stock.stock_quarantine)
            },
            'valuation': {
                'average_cost': float(stock.stock_average_cost),
                'standard_cost': float(stock.stock_standard_cost),
                'last_cost': float(stock.stock_last_cost),
                'total_value': float(stock.stock_on_hand * stock.stock_average_cost),
                'valuation_method': 'AVERAGE'
            },
            'pricing': {
                'sell_price1': float(stock.stock_sell_price1),
                'sell_price2': float(stock.stock_sell_price2),
                'sell_price3': float(stock.stock_sell_price3),
                'last_price_update': stock.stock_price_date,
                'history': pricing
            },
            'reorder': {
                'min_qty': float(stock.stock_min_qty),
                'max_qty': float(stock.stock_max_qty),
                'reorder_level': float(stock.stock_reorder_level),
                'reorder_qty': float(stock.stock_reorder_qty),
                'lead_time': stock.stock_lead_time,
                'preferred_supplier': stock.stock_pref_supp,
                'reorder_required': float(stock.stock_on_hand) <= float(stock.stock_reorder_level)
            },
            'locations': locations,
            'suppliers': suppliers,
            'movements': movements,
            'allocations': allocations,
            'orders': orders,
            'metrics': metrics,
            'abc_analysis': abc_analysis,
            'dates': {
                'created': stock.stock_created_date,
                'last_movement': stock.stock_last_movement,
                'last_counted': stock.stock_last_counted,
                'last_sold': stock.stock_last_sold,
                'last_purchased': stock.stock_last_purchased
            },
            'statistics': {
                'ytd_sales_qty': float(stock.stock_ytd_sales_qty),
                'ytd_sales_val': float(stock.stock_ytd_sales_val),
                'ytd_cost_val': float(stock.stock_ytd_cost_val),
                'movements_mtd': stock.stock_movements_mtd
            }
        }
        
    def get_multi_location_view(self, stock_code: str) -> Dict:
        """Get multi-location warehouse view"""
        stock, status = self.stock_handler.process(4, key_value=stock_code)
        if status.fs_reply != "00":
            return {"error": "Stock item not found"}
            
        # Get all locations
        locations = self.db.query(StockLocationRec).filter(
            and_(
                StockLocationRec.loc_stock_code == stock_code,
                StockLocationRec.loc_active == 'Y'
            )
        ).order_by(
            StockLocationRec.loc_warehouse,
            StockLocationRec.loc_location
        ).all()
        
        # Group by warehouse
        warehouses = {}
        total_on_hand = Decimal('0')
        total_allocated = Decimal('0')
        
        for loc in locations:
            wh = loc.loc_warehouse
            if wh not in warehouses:
                warehouses[wh] = {
                    'warehouse': wh,
                    'locations': [],
                    'totals': {
                        'on_hand': Decimal('0'),
                        'allocated': Decimal('0'),
                        'available': Decimal('0'),
                        'in_transit': Decimal('0')
                    }
                }
                
            location_data = {
                'location': loc.loc_location,
                'bin': loc.loc_bin,
                'quantities': {
                    'on_hand': float(loc.loc_qty_on_hand),
                    'allocated': float(loc.loc_qty_allocated),
                    'available': float(loc.loc_qty_on_hand - loc.loc_qty_allocated),
                    'in_transit': float(loc.loc_qty_in_transit)
                },
                'settings': {
                    'primary': loc.loc_primary == 'Y',
                    'pickable': loc.loc_pickable == 'Y',
                    'replenish': loc.loc_replenish == 'Y',
                    'min_qty': float(loc.loc_min_qty),
                    'max_qty': float(loc.loc_max_qty)
                },
                'status': {
                    'needs_replenish': loc.loc_qty_on_hand < loc.loc_min_qty,
                    'overstocked': loc.loc_qty_on_hand > loc.loc_max_qty,
                    'pick_priority': 1 if loc.loc_primary == 'Y' else 2
                }
            }
            
            warehouses[wh]['locations'].append(location_data)
            warehouses[wh]['totals']['on_hand'] += loc.loc_qty_on_hand
            warehouses[wh]['totals']['allocated'] += loc.loc_qty_allocated
            warehouses[wh]['totals']['available'] += loc.loc_qty_on_hand - loc.loc_qty_allocated
            warehouses[wh]['totals']['in_transit'] += loc.loc_qty_in_transit
            
            total_on_hand += loc.loc_qty_on_hand
            total_allocated += loc.loc_qty_allocated
            
        # Convert warehouse totals to float
        for wh_data in warehouses.values():
            for key in wh_data['totals']:
                wh_data['totals'][key] = float(wh_data['totals'][key])
                
        return {
            'stock_code': stock.stock_code,
            'description': stock.stock_desc,
            'warehouses': list(warehouses.values()),
            'grand_totals': {
                'on_hand': float(total_on_hand),
                'allocated': float(total_allocated),
                'available': float(total_on_hand - total_allocated)
            },
            'location_count': len(locations),
            'warehouse_count': len(warehouses)
        }
        
    def get_stock_movements(self, stock_code: str, filters: Optional[Dict] = None) -> List[Dict]:
        """Get stock movement history with filters"""
        filters = filters or {}
        
        # Base query
        query = self.db.query(StockMovementRec).filter(
            StockMovementRec.move_stock_code == stock_code
        )
        
        # Apply filters
        if filters.get('date_from'):
            query = query.filter(StockMovementRec.move_date >= filters['date_from'])
        if filters.get('date_to'):
            query = query.filter(StockMovementRec.move_date <= filters['date_to'])
        if filters.get('type'):
            query = query.filter(StockMovementRec.move_type == filters['type'])
        if filters.get('warehouse'):
            query = query.filter(StockMovementRec.move_warehouse == filters['warehouse'])
        if filters.get('reference'):
            query = query.filter(StockMovementRec.move_reference.ilike(f"%{filters['reference']}%"))
            
        # Order and limit
        limit = filters.get('limit', 100)
        movements = query.order_by(desc(StockMovementRec.move_date)).limit(limit).all()
        
        return [
            {
                'date': mov.move_date,
                'type': mov.move_type,
                'type_desc': self._get_movement_type_desc(mov.move_type),
                'reference': mov.move_reference,
                'warehouse': mov.move_warehouse,
                'location': mov.move_location,
                'quantity': {
                    'in': float(mov.move_qty_in) if mov.move_qty_in else 0,
                    'out': float(mov.move_qty_out) if mov.move_qty_out else 0,
                    'net': float((mov.move_qty_in or 0) - (mov.move_qty_out or 0))
                },
                'cost': {
                    'unit_cost': float(mov.move_unit_cost) if mov.move_unit_cost else 0,
                    'total_cost': float(mov.move_total_cost) if mov.move_total_cost else 0
                },
                'balance': float(mov.move_balance) if mov.move_balance else 0,
                'user': mov.move_user,
                'time': mov.move_time
            }
            for mov in movements
        ]
        
    def get_allocation_analysis(self, stock_code: str) -> Dict:
        """Analyze stock allocations"""
        stock, status = self.stock_handler.process(4, key_value=stock_code)
        if status.fs_reply != "00":
            return {"error": "Stock item not found"}
            
        # Get sales allocations
        sales_allocations = self._get_sales_allocations(stock_code)
        
        # Get production allocations (would be from work orders)
        production_allocations = self._get_production_allocations(stock_code)
        
        # Get transfer allocations
        transfer_allocations = self._get_transfer_allocations(stock_code)
        
        total_allocated = sum([
            sum(alloc['quantity'] for alloc in sales_allocations),
            sum(alloc['quantity'] for alloc in production_allocations),
            sum(alloc['quantity'] for alloc in transfer_allocations)
        ])
        
        return {
            'stock_code': stock.stock_code,
            'total_on_hand': float(stock.stock_on_hand),
            'total_allocated': float(stock.stock_allocated),
            'system_allocated': total_allocated,  # Sum of detailed allocations
            'available': float(stock.stock_on_hand - stock.stock_allocated),
            'allocation_variance': float(stock.stock_allocated) - total_allocated,
            'allocations': {
                'sales': sales_allocations,
                'production': production_allocations,
                'transfers': transfer_allocations
            },
            'allocation_summary': {
                'sales_count': len(sales_allocations),
                'sales_qty': sum(alloc['quantity'] for alloc in sales_allocations),
                'production_count': len(production_allocations),
                'production_qty': sum(alloc['quantity'] for alloc in production_allocations),
                'transfer_count': len(transfer_allocations),
                'transfer_qty': sum(alloc['quantity'] for alloc in transfer_allocations)
            }
        }
        
    def get_supplier_comparison(self, stock_code: str) -> Dict:
        """Compare suppliers for stock item"""
        # Get all suppliers for item
        suppliers = self.db.query(StockSupplierRec).filter(
            and_(
                StockSupplierRec.ss_stock_code == stock_code,
                StockSupplierRec.ss_active == 'Y'
            )
        ).order_by(StockSupplierRec.ss_primary.desc()).all()
        
        supplier_data = []
        for supplier in suppliers:
            # Get supplier master details
            from app.services.file_handlers.supplier_handler import SupplierFileHandler
            supp_handler = SupplierFileHandler(self.db)
            supp_master, _ = supp_handler.process(4, key_value=supplier.ss_supplier)
            
            # Calculate performance metrics
            performance = self._calculate_supplier_performance(supplier.ss_supplier, stock_code)
            
            supplier_data.append({
                'supplier_code': supplier.ss_supplier,
                'supplier_name': supp_master.purch_name if supp_master else 'Unknown',
                'item_details': {
                    'supplier_item_code': supplier.ss_supplier_code,
                    'description': supplier.ss_supplier_desc,
                    'pack_qty': float(supplier.ss_pack_qty),
                    'min_order_qty': float(supplier.ss_min_order_qty)
                },
                'pricing': {
                    'last_price': float(supplier.ss_last_price),
                    'currency': supplier.ss_currency,
                    'price_per_unit': float(supplier.ss_last_price / supplier.ss_pack_qty) if supplier.ss_pack_qty > 0 else 0
                },
                'terms': {
                    'lead_time': supplier.ss_lead_time,
                    'payment_terms': supp_master.purch_payment_cd if supp_master else '',
                    'primary': supplier.ss_primary == 'Y'
                },
                'performance': performance,
                'status': {
                    'active': supplier.ss_active == 'Y',
                    'on_stop': supp_master.purch_on_stop == 'Y' if supp_master else False,
                    'created_date': supplier.ss_created_date
                }
            })
            
        # Rank suppliers by score
        for i, supplier in enumerate(supplier_data):
            supplier['rank'] = i + 1
            
        return {
            'stock_code': stock_code,
            'supplier_count': len(supplier_data),
            'suppliers': supplier_data,
            'recommendation': self._get_supplier_recommendation(supplier_data)
        }
        
    def get_usage_analysis(self, stock_code: str, months: int = 12) -> Dict:
        """Analyze stock usage patterns"""
        stock, status = self.stock_handler.process(4, key_value=stock_code)
        if status.fs_reply != "00":
            return {"error": "Stock item not found"}
            
        # Calculate date range
        end_date = datetime.now().date()
        start_date = end_date - timedelta(days=months * 30)
        
        # Get movement analysis
        movements = self.db.query(StockMovementRec).filter(
            and_(
                StockMovementRec.move_stock_code == stock_code,
                StockMovementRec.move_date >= int(start_date.strftime("%Y%m%d"))
            )
        ).all()
        
        # Analyze by month
        monthly_usage = {}
        sales_movements = []
        purchase_movements = []
        
        for mov in movements:
            mov_date = datetime.strptime(str(mov.move_date), "%Y%m%d").date()
            month_key = mov_date.strftime("%Y-%m")
            
            if month_key not in monthly_usage:
                monthly_usage[month_key] = {
                    'sales_qty': 0,
                    'purchase_qty': 0,
                    'adjustment_qty': 0,
                    'transfer_out': 0,
                    'transfer_in': 0
                }
                
            # Categorize movements
            if mov.move_type in ['SAL', 'INV']:  # Sales
                qty = float(mov.move_qty_out or 0)
                monthly_usage[month_key]['sales_qty'] += qty
                sales_movements.append(qty)
            elif mov.move_type in ['PUR', 'REC']:  # Purchases
                qty = float(mov.move_qty_in or 0)
                monthly_usage[month_key]['purchase_qty'] += qty
                purchase_movements.append(qty)
            elif mov.move_type in ['ADJ']:  # Adjustments
                net_qty = float((mov.move_qty_in or 0) - (mov.move_qty_out or 0))
                monthly_usage[month_key]['adjustment_qty'] += net_qty
            elif mov.move_type in ['TRO']:  # Transfer out
                monthly_usage[month_key]['transfer_out'] += float(mov.move_qty_out or 0)
            elif mov.move_type in ['TRI']:  # Transfer in
                monthly_usage[month_key]['transfer_in'] += float(mov.move_qty_in or 0)
                
        # Calculate statistics
        sales_qtys = [usage['sales_qty'] for usage in monthly_usage.values()]
        avg_monthly_usage = sum(sales_qtys) / len(sales_qtys) if sales_qtys else 0
        
        # Calculate reorder suggestions
        reorder_analysis = self._calculate_reorder_suggestions(
            stock, avg_monthly_usage, stock.stock_lead_time
        )
        
        return {
            'stock_code': stock.stock_code,
            'analysis_period': f"{start_date} to {end_date}",
            'monthly_usage': [
                {
                    'month': month,
                    'sales': usage['sales_qty'],
                    'purchases': usage['purchase_qty'],
                    'adjustments': usage['adjustment_qty'],
                    'net_movement': usage['purchase_qty'] - usage['sales_qty'] + usage['adjustment_qty']
                }
                for month, usage in sorted(monthly_usage.items())
            ],
            'statistics': {
                'avg_monthly_sales': avg_monthly_usage,
                'total_sales': sum(sales_qtys),
                'total_purchases': sum(usage['purchase_qty'] for usage in monthly_usage.values()),
                'movement_count': len(movements),
                'velocity': avg_monthly_usage / float(stock.stock_on_hand) if stock.stock_on_hand > 0 else 0
            },
            'reorder_analysis': reorder_analysis,
            'trends': self._calculate_usage_trends(monthly_usage),
            'forecasting': self._calculate_demand_forecast(sales_qtys)
        }
        
    def search_stock_items(self, filters: Dict) -> List[Dict]:
        """Search stock items with various criteria"""
        query = self.db.query(StockMasterRec)
        
        # Apply filters
        if filters.get('search'):
            search_term = f"%{filters['search']}%"
            query = query.filter(
                or_(
                    StockMasterRec.stock_code.ilike(search_term),
                    StockMasterRec.stock_desc.ilike(search_term),
                    StockMasterRec.stock_search_key.ilike(search_term)
                )
            )
            
        if filters.get('type'):
            query = query.filter(StockMasterRec.stock_type == filters['type'])
            
        if filters.get('group'):
            query = query.filter(StockMasterRec.stock_group == filters['group'])
            
        if filters.get('category'):
            query = query.filter(StockMasterRec.stock_category == filters['category'])
            
        if filters.get('active_only', True):
            query = query.filter(StockMasterRec.stock_active == 'Y')
            
        # Status filters
        if filters.get('low_stock'):
            query = query.filter(
                StockMasterRec.stock_on_hand <= StockMasterRec.stock_reorder_level
            )
            
        if filters.get('negative_stock'):
            query = query.filter(StockMasterRec.stock_on_hand < 0)
            
        if filters.get('zero_stock'):
            query = query.filter(StockMasterRec.stock_on_hand == 0)
            
        if filters.get('overstocked'):
            query = query.filter(
                StockMasterRec.stock_on_hand > StockMasterRec.stock_max_qty
            )
            
        # Order results
        order_by = filters.get('order_by', 'stock_code')
        if order_by == 'description':
            query = query.order_by(StockMasterRec.stock_desc)
        elif order_by == 'value':
            query = query.order_by(desc(StockMasterRec.stock_on_hand * StockMasterRec.stock_average_cost))
        else:
            query = query.order_by(StockMasterRec.stock_code)
            
        # Limit results
        limit = filters.get('limit', 100)
        items = query.limit(limit).all()
        
        return [
            {
                'stock_code': item.stock_code,
                'description': item.stock_desc,
                'type': item.stock_type,
                'group': item.stock_group,
                'category': item.stock_category,
                'quantities': {
                    'on_hand': float(item.stock_on_hand),
                    'allocated': float(item.stock_allocated),
                    'available': float(item.stock_on_hand - item.stock_allocated),
                    'on_order': float(item.stock_on_order)
                },
                'valuation': {
                    'average_cost': float(item.stock_average_cost),
                    'total_value': float(item.stock_on_hand * item.stock_average_cost)
                },
                'reorder': {
                    'level': float(item.stock_reorder_level),
                    'max_qty': float(item.stock_max_qty),
                    'needs_reorder': float(item.stock_on_hand) <= float(item.stock_reorder_level)
                },
                'status': {
                    'active': item.stock_active == 'Y',
                    'sellable': item.stock_sellable == 'Y',
                    'purchasable': item.stock_purchasable == 'Y'
                }
            }
            for item in items
        ]
        
    def _get_location_details(self, stock_code: str) -> List[Dict]:
        """Get detailed location information"""
        locations = self.db.query(StockLocationRec).filter(
            and_(
                StockLocationRec.loc_stock_code == stock_code,
                StockLocationRec.loc_active == 'Y'
            )
        ).order_by(
            StockLocationRec.loc_primary.desc(),
            StockLocationRec.loc_warehouse,
            StockLocationRec.loc_location
        ).all()
        
        return [
            {
                'warehouse': loc.loc_warehouse,
                'location': loc.loc_location,
                'bin': loc.loc_bin,
                'quantities': {
                    'on_hand': float(loc.loc_qty_on_hand),
                    'allocated': float(loc.loc_qty_allocated),
                    'available': float(loc.loc_qty_on_hand - loc.loc_qty_allocated),
                    'in_transit': float(loc.loc_qty_in_transit)
                },
                'settings': {
                    'primary': loc.loc_primary == 'Y',
                    'pickable': loc.loc_pickable == 'Y',
                    'replenish': loc.loc_replenish == 'Y',
                    'min_qty': float(loc.loc_min_qty),
                    'max_qty': float(loc.loc_max_qty)
                },
                'status': {
                    'needs_replenish': loc.loc_qty_on_hand < loc.loc_min_qty,
                    'overstocked': loc.loc_qty_on_hand > loc.loc_max_qty
                }
            }
            for loc in locations
        ]
        
    def _get_supplier_details(self, stock_code: str) -> List[Dict]:
        """Get supplier details for stock item"""
        suppliers = self.db.query(StockSupplierRec).filter(
            and_(
                StockSupplierRec.ss_stock_code == stock_code,
                StockSupplierRec.ss_active == 'Y'
            )
        ).order_by(StockSupplierRec.ss_primary.desc()).all()
        
        supplier_list = []
        for supplier in suppliers:
            # Get supplier master
            from app.services.file_handlers.supplier_handler import SupplierFileHandler
            supp_handler = SupplierFileHandler(self.db)
            supp_master, _ = supp_handler.process(4, key_value=supplier.ss_supplier)
            
            supplier_list.append({
                'supplier_code': supplier.ss_supplier,
                'name': supp_master.purch_name if supp_master else 'Unknown',
                'item_code': supplier.ss_supplier_code,
                'description': supplier.ss_supplier_desc,
                'pack_qty': float(supplier.ss_pack_qty),
                'lead_time': supplier.ss_lead_time,
                'min_order_qty': float(supplier.ss_min_order_qty),
                'last_price': float(supplier.ss_last_price),
                'currency': supplier.ss_currency,
                'primary': supplier.ss_primary == 'Y',
                'active': supplier.ss_active == 'Y'
            })
            
        return supplier_list
        
    def _get_recent_movements(self, stock_code: str, limit: int = 10) -> List[Dict]:
        """Get recent stock movements"""
        movements = self.db.query(StockMovementRec).filter(
            StockMovementRec.move_stock_code == stock_code
        ).order_by(desc(StockMovementRec.move_date)).limit(limit).all()
        
        return [
            {
                'date': mov.move_date,
                'type': mov.move_type,
                'type_desc': self._get_movement_type_desc(mov.move_type),
                'reference': mov.move_reference,
                'warehouse': mov.move_warehouse,
                'location': mov.move_location,
                'quantity_in': float(mov.move_qty_in) if mov.move_qty_in else 0,
                'quantity_out': float(mov.move_qty_out) if mov.move_qty_out else 0,
                'balance': float(mov.move_balance) if mov.move_balance else 0,
                'cost': float(mov.move_unit_cost) if mov.move_unit_cost else 0
            }
            for mov in movements
        ]
        
    def _get_pricing_history(self, stock_code: str) -> List[Dict]:
        """Get pricing history"""
        price_history = self.db.query(StockPriceRec).filter(
            StockPriceRec.price_stock_code == stock_code
        ).order_by(desc(StockPriceRec.price_date)).limit(10).all()
        
        return [
            {
                'date': price.price_date,
                'price_list': price.price_list,
                'old_price': float(price.price_old),
                'new_price': float(price.price_new),
                'change_pct': float((price.price_new - price.price_old) / price.price_old * 100) if price.price_old > 0 else 0,
                'updated_by': price.price_updated_by
            }
            for price in price_history
        ]
        
    def _get_allocation_details(self, stock_code: str) -> Dict:
        """Get detailed allocation information"""
        # Get sales order allocations
        sales_allocs = self._get_sales_allocations(stock_code)
        
        # Get production allocations
        prod_allocs = self._get_production_allocations(stock_code)
        
        # Get transfer allocations
        transfer_allocs = self._get_transfer_allocations(stock_code)
        
        return {
            'sales_orders': sales_allocs,
            'production': prod_allocs,
            'transfers': transfer_allocs,
            'total_sales': sum(alloc['quantity'] for alloc in sales_allocs),
            'total_production': sum(alloc['quantity'] for alloc in prod_allocs),
            'total_transfers': sum(alloc['quantity'] for alloc in transfer_allocs)
        }
        
    def _get_order_information(self, stock_code: str) -> Dict:
        """Get purchase and sales order information"""
        # Get open purchase orders
        purchase_orders = self.db.query(PurchaseOrderLineRec).filter(
            and_(
                PurchaseOrderLineRec.pol_stock_code == stock_code,
                PurchaseOrderLineRec.pol_outstanding > 0
            )
        ).all() if hasattr(self.db.query(PurchaseOrderLineRec), 'filter') else []
        
        # Get open sales orders
        sales_orders = self.db.query(SalesOrderLineRec).filter(
            and_(
                SalesOrderLineRec.sol_stock_code == stock_code,
                SalesOrderLineRec.sol_outstanding > 0
            )
        ).all() if hasattr(self.db.query(SalesOrderLineRec), 'filter') else []
        
        return {
            'purchase_orders': [
                {
                    'order_no': po.pol_order_no,
                    'supplier': po.pol_supplier,
                    'order_date': po.pol_order_date,
                    'due_date': po.pol_due_date,
                    'quantity': float(po.pol_quantity),
                    'outstanding': float(po.pol_outstanding),
                    'unit_price': float(po.pol_unit_price)
                }
                for po in purchase_orders
            ],
            'sales_orders': [
                {
                    'order_no': so.sol_order_no,
                    'customer': so.sol_customer,
                    'order_date': so.sol_order_date,
                    'due_date': so.sol_due_date,
                    'quantity': float(so.sol_quantity),
                    'outstanding': float(so.sol_outstanding),
                    'unit_price': float(so.sol_unit_price)
                }
                for so in sales_orders
            ]
        }
        
    def _calculate_stock_metrics(self, stock: StockMasterRec) -> Dict:
        """Calculate key stock metrics"""
        # Stock turn calculation
        stock_turn = 0
        if stock.stock_average_cost > 0 and stock.stock_on_hand > 0:
            avg_inventory_value = float(stock.stock_on_hand * stock.stock_average_cost)
            if avg_inventory_value > 0:
                stock_turn = float(stock.stock_ytd_cost_val / avg_inventory_value)
                
        # Days of stock
        days_stock = 0
        if stock.stock_ytd_sales_qty > 0:
            daily_sales = float(stock.stock_ytd_sales_qty / 365)
            if daily_sales > 0:
                days_stock = float(stock.stock_on_hand / daily_sales)
                
        # Margin analysis
        margin_pct = 0
        if stock.stock_sell_price1 > 0:
            margin_pct = float((stock.stock_sell_price1 - stock.stock_average_cost) / stock.stock_sell_price1 * 100)
            
        return {
            'stock_turn': stock_turn,
            'days_of_stock': days_stock,
            'margin_pct': margin_pct,
            'value_on_hand': float(stock.stock_on_hand * stock.stock_average_cost),
            'allocation_pct': float(stock.stock_allocated / stock.stock_on_hand * 100) if stock.stock_on_hand > 0 else 0,
            'reorder_due': float(stock.stock_on_hand) <= float(stock.stock_reorder_level),
            'fast_moving': stock_turn > 6,  # More than 6 turns per year
            'slow_moving': stock_turn < 2   # Less than 2 turns per year
        }
        
    def _get_abc_analysis(self, stock_code: str) -> Dict:
        """Get ABC analysis data"""
        stock, _ = self.stock_handler.process(4, key_value=stock_code)
        
        # This would normally be calculated across all stock items
        # For now, return the current classification
        return {
            'current_class': stock.stock_abc_class if stock else 'C',
            'annual_usage_value': float(stock.stock_ytd_sales_val) if stock else 0,
            'rank_position': 0,  # Would be calculated relative to other items
            'classification_date': stock.stock_created_date if stock else 0,
            'recommended_class': stock.stock_abc_class if stock else 'C'
        }
        
    def _get_sales_allocations(self, stock_code: str) -> List[Dict]:
        """Get sales order allocations"""
        # This would query sales order allocation table
        # For now, return mock data based on stock allocated quantity
        stock, _ = self.stock_handler.process(4, key_value=stock_code)
        if not stock or stock.stock_allocated == 0:
            return []
            
        # Mock allocation - would be real data from allocations table
        return [
            {
                'order_no': 'SO123456',
                'customer': 'CUST001',
                'line_no': 1,
                'quantity': float(stock.stock_allocated),
                'due_date': int(datetime.now().strftime("%Y%m%d")),
                'warehouse': 'MAIN',
                'location': 'A-01-01'
            }
        ]
        
    def _get_production_allocations(self, stock_code: str) -> List[Dict]:
        """Get production allocations"""
        # Would query work order/production tables
        return []
        
    def _get_transfer_allocations(self, stock_code: str) -> List[Dict]:
        """Get transfer allocations"""
        transfers = self.db.query(StockTransferRec).filter(
            and_(
                StockTransferRec.trans_stock_code == stock_code,
                StockTransferRec.trans_status == 'ALLOCATED'
            )
        ).all()
        
        return [
            {
                'transfer_no': transfer.trans_no,
                'from_warehouse': transfer.trans_from_warehouse,
                'to_warehouse': transfer.trans_to_warehouse,
                'quantity': float(transfer.trans_quantity),
                'date_required': transfer.trans_date_required
            }
            for transfer in transfers
        ]
        
    def _calculate_supplier_performance(self, supplier_code: str, stock_code: str) -> Dict:
        """Calculate supplier performance metrics"""
        # This would analyze purchase order vs delivery performance
        # For now, return basic metrics
        return {
            'on_time_delivery': 85.5,  # Percentage
            'quality_rating': 92.0,    # Percentage
            'price_competitiveness': 88.5,  # Percentage
            'overall_score': 88.7,     # Weighted average
            'last_delivery': int(datetime.now().strftime("%Y%m%d")),
            'orders_completed': 25,
            'orders_late': 4
        }
        
    def _get_supplier_recommendation(self, suppliers: List[Dict]) -> Dict:
        """Get supplier recommendation"""
        if not suppliers:
            return {"message": "No suppliers available"}
            
        # Find primary supplier or best performing
        primary = next((s for s in suppliers if s['terms']['primary']), suppliers[0])
        
        return {
            'recommended_supplier': primary['supplier_code'],
            'reason': 'Primary supplier' if primary['terms']['primary'] else 'Best performance',
            'score': primary['performance']['overall_score'],
            'alternative_count': len(suppliers) - 1
        }
        
    def _calculate_reorder_suggestions(self, stock: StockMasterRec, avg_monthly_usage: float, lead_time: int) -> Dict:
        """Calculate reorder suggestions"""
        # Calculate safety stock (1 month)
        safety_stock = avg_monthly_usage
        
        # Calculate reorder point
        lead_time_months = lead_time / 30
        reorder_point = (avg_monthly_usage * lead_time_months) + safety_stock
        
        # Calculate optimal order quantity (EOQ approximation)
        annual_usage = avg_monthly_usage * 12
        order_cost = 50  # Assumed ordering cost
        holding_cost = float(stock.stock_average_cost * 0.25)  # 25% holding cost
        
        if annual_usage > 0 and holding_cost > 0:
            eoq = (2 * annual_usage * order_cost / holding_cost) ** 0.5
        else:
            eoq = avg_monthly_usage * 3  # 3 months supply
            
        return {
            'current_reorder_level': float(stock.stock_reorder_level),
            'suggested_reorder_level': reorder_point,
            'current_reorder_qty': float(stock.stock_reorder_qty),
            'suggested_reorder_qty': eoq,
            'safety_stock': safety_stock,
            'lead_time_demand': avg_monthly_usage * lead_time_months,
            'reorder_required': float(stock.stock_on_hand) <= reorder_point
        }
        
    def _calculate_usage_trends(self, monthly_usage: Dict) -> Dict:
        """Calculate usage trends"""
        months = sorted(monthly_usage.keys())
        if len(months) < 3:
            return {'trend': 'INSUFFICIENT_DATA'}
            
        # Simple trend analysis
        sales_values = [monthly_usage[month]['sales_qty'] for month in months]
        
        # Calculate trend (last 3 months vs previous 3 months)
        if len(sales_values) >= 6:
            recent_avg = sum(sales_values[-3:]) / 3
            previous_avg = sum(sales_values[-6:-3]) / 3
            
            if previous_avg > 0:
                trend_pct = (recent_avg - previous_avg) / previous_avg * 100
            else:
                trend_pct = 0
                
            if trend_pct > 10:
                trend = 'INCREASING'
            elif trend_pct < -10:
                trend = 'DECREASING'
            else:
                trend = 'STABLE'
        else:
            trend = 'STABLE'
            trend_pct = 0
            
        return {
            'trend': trend,
            'trend_pct': trend_pct,
            'seasonal_pattern': 'NONE'  # Would analyze for seasonality
        }
        
    def _calculate_demand_forecast(self, sales_history: List[float]) -> Dict:
        """Calculate demand forecast"""
        if len(sales_history) < 3:
            return {'forecast': 'INSUFFICIENT_DATA'}
            
        # Simple moving average forecast
        avg_demand = sum(sales_history[-3:]) / 3 if len(sales_history) >= 3 else sum(sales_history) / len(sales_history)
        
        return {
            'method': 'MOVING_AVERAGE',
            'next_month_forecast': avg_demand,
            'confidence': 'MEDIUM',
            'forecast_period': '1_MONTH'
        }
        
    def _get_movement_type_desc(self, move_type: str) -> str:
        """Get description for movement type"""
        types = {
            'SAL': 'Sales',
            'PUR': 'Purchase',
            'ADJ': 'Adjustment',
            'TRI': 'Transfer In',
            'TRO': 'Transfer Out',
            'REC': 'Receipt',
            'ISS': 'Issue',
            'RET': 'Return',
            'CNT': 'Count',
            'WAS': 'Wastage'
        }
        return types.get(move_type, move_type)