"""
Stock Valuation Service - ST040 migration
Handles stock valuation calculations and reporting
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from datetime import datetime, date
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func, case, text

from app.services.file_handlers.stock_handler import StockFileHandler
from app.services.file_handlers.system_handler import SystemFileHandler
from app.models.stock import (
    StockMasterRec, StockLocationRec, StockMovementRec,
    StockValuationRec, StockRevalRec
)
from app.models.system import SystemRec
from app.services.gl.journal_entry import JournalEntryService
from app.core.security import log_user_action
from app.models.auth import User


class StockValuationService:
    """
    Stock Valuation functionality
    Implements ST040 - stock valuation and revaluation
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.stock_handler = StockFileHandler(db)
        self.system_handler = SystemFileHandler(db)
        
    def calculate_stock_valuation(self, options: Optional[Dict] = None) -> Dict:
        """
        Calculate comprehensive stock valuation
        Returns valuation report data
        """
        options = options or {}
        valuation_date = options.get('valuation_date', int(datetime.now().strftime("%Y%m%d")))
        method = options.get('method', 'AVERAGE')  # AVERAGE, STANDARD, LAST, FIFO
        warehouse = options.get('warehouse')  # Optional warehouse filter
        category = options.get('category')    # Optional category filter
        
        # Build query for stock items
        query = self.db.query(StockMasterRec).filter(
            and_(
                StockMasterRec.stock_active == 'Y',
                StockMasterRec.stock_on_hand != 0  # Only items with stock
            )
        )
        
        # Apply filters
        if category:
            query = query.filter(StockMasterRec.stock_category == category)
            
        stock_items = query.order_by(StockMasterRec.stock_code).all()
        
        # Calculate valuation for each item
        valuation_data = []
        total_value = Decimal('0')
        total_quantity = Decimal('0')
        category_totals = {}
        warehouse_totals = {}
        
        for stock in stock_items:
            # Get unit cost based on method
            unit_cost = self._get_valuation_cost(stock, method)
            
            # Get quantities by warehouse if specified
            if warehouse:
                warehouse_qty = self._get_warehouse_quantity(stock.stock_code, warehouse)
                if warehouse_qty == 0:
                    continue
                quantity = warehouse_qty
            else:
                quantity = stock.stock_on_hand
                
            # Calculate value
            item_value = quantity * unit_cost
            
            # Get additional item details
            item_data = {
                'stock_code': stock.stock_code,
                'description': stock.stock_desc,
                'category': stock.stock_category,
                'type': stock.stock_type,
                'group': stock.stock_group,
                'abc_class': stock.stock_abc_class,
                'unit': stock.stock_unit,
                'quantity': float(quantity),
                'unit_cost': float(unit_cost),
                'total_value': float(item_value),
                'valuation_method': method,
                'last_movement': stock.stock_last_movement,
                'last_counted': stock.stock_last_counted
            }
            
            # Add warehouse breakdown if not filtered
            if not warehouse:
                item_data['warehouses'] = self._get_warehouse_breakdown(stock.stock_code, unit_cost)
                
            valuation_data.append(item_data)
            
            # Update totals
            total_value += item_value
            total_quantity += quantity
            
            # Category totals
            cat = stock.stock_category or 'UNCATEGORIZED'
            if cat not in category_totals:
                category_totals[cat] = {'quantity': Decimal('0'), 'value': Decimal('0'), 'count': 0}
            category_totals[cat]['quantity'] += quantity
            category_totals[cat]['value'] += item_value
            category_totals[cat]['count'] += 1
            
        # Convert category totals to float
        for cat_data in category_totals.values():
            cat_data['quantity'] = float(cat_data['quantity'])
            cat_data['value'] = float(cat_data['value'])
            
        # Create valuation summary
        summary = {
            'valuation_date': valuation_date,
            'method': method,
            'warehouse_filter': warehouse,
            'category_filter': category,
            'total_items': len(valuation_data),
            'total_quantity': float(total_quantity),
            'total_value': float(total_value),
            'average_unit_cost': float(total_value / total_quantity) if total_quantity > 0 else 0
        }
        
        return {
            'summary': summary,
            'items': valuation_data,
            'category_analysis': category_totals,
            'abc_analysis': self._calculate_abc_analysis(valuation_data),
            'variance_analysis': self._calculate_variance_analysis(valuation_data, method)
        }
        
    def process_revaluation(self, revaluation_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Process stock revaluation
        Returns (success, error_message)
        """
        stock_code = revaluation_data.get('stock_code')
        new_cost = Decimal(str(revaluation_data.get('new_cost', 0)))
        effective_date = revaluation_data.get('effective_date', int(datetime.now().strftime("%Y%m%d")))
        reason = revaluation_data.get('reason', '')
        reference = revaluation_data.get('reference', '')
        
        if new_cost <= 0:
            return False, "New cost must be greater than zero"
            
        # Get stock item
        stock, status = self.stock_handler.process(4, key_value=stock_code)
        if status.fs_reply != "00":
            return False, "Stock item not found"
            
        if stock.stock_on_hand <= 0:
            return False, "Cannot revalue item with zero stock"
            
        try:
            # Get revaluation number
            reval_no = self._get_next_revaluation_number()
            
            # Calculate revaluation amounts
            old_cost = stock.stock_average_cost
            old_value = stock.stock_on_hand * old_cost
            new_value = stock.stock_on_hand * new_cost
            variance = new_value - old_value
            
            # Create revaluation record
            revaluation = StockRevalRec(
                reval_no=reval_no,
                reval_stock_code=stock_code,
                reval_date=effective_date,
                reval_quantity=stock.stock_on_hand,
                reval_old_cost=old_cost,
                reval_new_cost=new_cost,
                reval_old_value=old_value,
                reval_new_value=new_value,
                reval_variance=variance,
                reval_reason=reason,
                reval_reference=reference,
                reval_user=self.current_user.username if self.current_user else 'SYSTEM',
                reval_approved='N',
                reval_posted='N'
            )
            
            self.db.add(revaluation)
            self.db.flush()
            
            # Check if auto-approval is enabled
            system_rec, _ = self.system_handler.read_system_params()
            auto_approve = system_rec and system_rec.stock_reval_auto_approve == 'Y'
            
            if auto_approve:
                return self.approve_revaluation(reval_no)
            else:
                self.db.commit()
                return True, f"Revaluation {reval_no} created - pending approval"
                
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def approve_revaluation(self, reval_no: str) -> Tuple[bool, Optional[str]]:
        """
        Approve and post stock revaluation
        Returns (success, error_message)
        """
        # Get revaluation record
        revaluation = self.db.query(StockRevalRec).filter(
            StockRevalRec.reval_no == reval_no
        ).first()
        
        if not revaluation:
            return False, "Revaluation not found"
            
        if revaluation.reval_posted == 'Y':
            return False, "Revaluation already posted"
            
        try:
            stock_code = revaluation.reval_stock_code
            
            # Get stock item
            stock, status = self.stock_handler.process(4, key_value=stock_code)
            if status.fs_reply != "00":
                return False, "Stock item not found"
                
            # Update stock master cost
            stock.stock_average_cost = revaluation.reval_new_cost
            stock.stock_last_reval_date = revaluation.reval_date
            
            self.stock_handler.process(7, record=stock)
            
            # Update revaluation record
            revaluation.reval_approved = 'Y'
            revaluation.reval_posted = 'Y'
            revaluation.reval_approved_by = self.current_user.username if self.current_user else 'SYSTEM'
            revaluation.reval_approved_date = int(datetime.now().strftime("%Y%m%d"))
            
            # Post to GL
            self._post_revaluation_to_gl(revaluation, stock)
            
            self.db.commit()
            
            # Log approval
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="APPROVE_STOCK_REVALUATION",
                    table="stock_reval_rec",
                    key=reval_no,
                    new_values={
                        'new_cost': float(revaluation.reval_new_cost),
                        'variance': float(revaluation.reval_variance)
                    },
                    module="STOCK"
                )
                
            return True, None
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def calculate_inventory_snapshot(self, snapshot_date: int, warehouse: Optional[str] = None) -> Dict:
        """
        Calculate inventory snapshot as at specific date
        Returns snapshot data
        """
        # Get stock items with movements up to snapshot date
        base_query = """
        SELECT 
            sm.stock_code,
            sm.stock_desc,
            sm.stock_category,
            sm.stock_type,
            sm.stock_unit,
            COALESCE(SUM(CASE WHEN mov.move_date <= %s THEN 
                COALESCE(mov.move_qty_in, 0) - COALESCE(mov.move_qty_out, 0) 
            ELSE 0 END), 0) as snapshot_qty,
            sm.stock_average_cost
        FROM stockmaster_rec sm
        LEFT JOIN stock_movement_rec mov ON sm.stock_code = mov.move_stock_code
        WHERE sm.stock_active = 'Y'
        """
        
        params = [snapshot_date]
        
        if warehouse:
            base_query += " AND mov.move_warehouse = %s"
            params.append(warehouse)
            
        base_query += """
        GROUP BY sm.stock_code, sm.stock_desc, sm.stock_category, 
                 sm.stock_type, sm.stock_unit, sm.stock_average_cost
        HAVING COALESCE(SUM(CASE WHEN mov.move_date <= %s THEN 
            COALESCE(mov.move_qty_in, 0) - COALESCE(mov.move_qty_out, 0) 
        ELSE 0 END), 0) != 0
        ORDER BY sm.stock_code
        """
        
        params.append(snapshot_date)
        
        # Execute query
        result = self.db.execute(text(base_query), params).fetchall()
        
        # Process results
        snapshot_data = []
        total_value = Decimal('0')
        total_items = 0
        
        for row in result:
            quantity = Decimal(str(row.snapshot_qty))
            unit_cost = Decimal(str(row.stock_average_cost))
            item_value = quantity * unit_cost
            
            snapshot_data.append({
                'stock_code': row.stock_code,
                'description': row.stock_desc,
                'category': row.stock_category,
                'type': row.stock_type,
                'unit': row.stock_unit,
                'quantity': float(quantity),
                'unit_cost': float(unit_cost),
                'total_value': float(item_value)
            })
            
            total_value += item_value
            total_items += 1
            
        return {
            'snapshot_date': snapshot_date,
            'warehouse': warehouse,
            'total_items': total_items,
            'total_value': float(total_value),
            'items': snapshot_data
        }
        
    def analyze_slow_moving_stock(self, days_threshold: int = 90, value_threshold: float = 100) -> Dict:
        """
        Analyze slow-moving stock items
        Returns analysis data
        """
        cutoff_date = int((datetime.now() - timedelta(days=days_threshold)).strftime("%Y%m%d"))
        
        # Get items with no movement in specified period
        slow_moving = self.db.query(StockMasterRec).filter(
            and_(
                StockMasterRec.stock_active == 'Y',
                StockMasterRec.stock_on_hand > 0,
                or_(
                    StockMasterRec.stock_last_movement < cutoff_date,
                    StockMasterRec.stock_last_movement == 0
                )
            )
        ).all()
        
        # Analyze each item
        analysis_data = []
        total_value = Decimal('0')
        
        for stock in slow_moving:
            item_value = stock.stock_on_hand * stock.stock_average_cost
            
            if float(item_value) < value_threshold:
                continue
                
            # Calculate days since last movement
            if stock.stock_last_movement > 0:
                last_move_date = datetime.strptime(str(stock.stock_last_movement), "%Y%m%d").date()
                days_static = (datetime.now().date() - last_move_date).days
            else:
                days_static = 9999  # Never moved
                
            # Calculate holding cost
            annual_holding_rate = 0.25  # 25% holding cost
            holding_cost = float(item_value) * annual_holding_rate * (days_static / 365)
            
            analysis_data.append({
                'stock_code': stock.stock_code,
                'description': stock.stock_desc,
                'category': stock.stock_category,
                'abc_class': stock.stock_abc_class,
                'quantity': float(stock.stock_on_hand),
                'unit_cost': float(stock.stock_average_cost),
                'total_value': float(item_value),
                'days_static': days_static,
                'last_movement': stock.stock_last_movement,
                'holding_cost': holding_cost,
                'recommendations': self._get_slow_moving_recommendations(stock, days_static, float(item_value))
            })
            
            total_value += item_value
            
        # Sort by value descending
        analysis_data.sort(key=lambda x: x['total_value'], reverse=True)
        
        return {
            'analysis_date': datetime.now().strftime("%Y-%m-%d"),
            'days_threshold': days_threshold,
            'value_threshold': value_threshold,
            'total_items': len(analysis_data),
            'total_value': float(total_value),
            'items': analysis_data,
            'summary': self._summarize_slow_moving(analysis_data)
        }
        
    def calculate_dead_stock(self, months_threshold: int = 12) -> Dict:
        """
        Calculate dead stock (no movement for extended period)
        Returns dead stock analysis
        """
        cutoff_date = int((datetime.now() - timedelta(days=months_threshold * 30)).strftime("%Y%m%d"))
        
        dead_stock = self.db.query(StockMasterRec).filter(
            and_(
                StockMasterRec.stock_active == 'Y',
                StockMasterRec.stock_on_hand > 0,
                or_(
                    StockMasterRec.stock_last_movement < cutoff_date,
                    StockMasterRec.stock_last_movement == 0
                ),
                StockMasterRec.stock_last_sold < cutoff_date
            )
        ).all()
        
        dead_stock_data = []
        total_value = Decimal('0')
        
        for stock in dead_stock:
            item_value = stock.stock_on_hand * stock.stock_average_cost
            
            # Calculate obsolescence provision
            months_static = months_threshold if stock.stock_last_movement == 0 else \
                           max((datetime.now() - datetime.strptime(str(stock.stock_last_movement), "%Y%m%d")).days // 30, months_threshold)
            
            provision_rate = min(months_static * 0.1, 1.0)  # 10% per month up to 100%
            provision_amount = float(item_value) * provision_rate
            
            dead_stock_data.append({
                'stock_code': stock.stock_code,
                'description': stock.stock_desc,
                'category': stock.stock_category,
                'quantity': float(stock.stock_on_hand),
                'unit_cost': float(stock.stock_average_cost),
                'total_value': float(item_value),
                'months_static': months_static,
                'provision_rate': provision_rate * 100,
                'provision_amount': provision_amount,
                'net_value': float(item_value) - provision_amount,
                'last_movement': stock.stock_last_movement,
                'last_sold': stock.stock_last_sold
            })
            
            total_value += item_value
            
        return {
            'analysis_date': datetime.now().strftime("%Y-%m-%d"),
            'months_threshold': months_threshold,
            'total_items': len(dead_stock_data),
            'gross_value': float(total_value),
            'total_provision': sum(item['provision_amount'] for item in dead_stock_data),
            'net_value': sum(item['net_value'] for item in dead_stock_data),
            'items': dead_stock_data
        }
        
    def generate_valuation_report(self, report_type: str, options: Dict) -> Dict:
        """
        Generate various valuation reports
        Returns formatted report data
        """
        if report_type == 'SUMMARY':
            return self._generate_summary_report(options)
        elif report_type == 'DETAILED':
            return self._generate_detailed_report(options)
        elif report_type == 'VARIANCE':
            return self._generate_variance_report(options)
        elif report_type == 'MOVEMENT':
            return self._generate_movement_report(options)
        elif report_type == 'COMPARISON':
            return self._generate_comparison_report(options)
        else:
            return {"error": f"Unknown report type: {report_type}"}
            
    def get_pending_revaluations(self) -> List[Dict]:
        """Get pending revaluations requiring approval"""
        revaluations = self.db.query(StockRevalRec).filter(
            and_(
                StockRevalRec.reval_approved == 'N',
                StockRevalRec.reval_posted == 'N'
            )
        ).order_by(StockRevalRec.reval_date).all()
        
        return [
            {
                'revaluation_no': reval.reval_no,
                'stock_code': reval.reval_stock_code,
                'date': reval.reval_date,
                'quantity': float(reval.reval_quantity),
                'old_cost': float(reval.reval_old_cost),
                'new_cost': float(reval.reval_new_cost),
                'old_value': float(reval.reval_old_value),
                'new_value': float(reval.reval_new_value),
                'variance': float(reval.reval_variance),
                'variance_pct': float((reval.reval_new_cost - reval.reval_old_cost) / reval.reval_old_cost * 100) if reval.reval_old_cost > 0 else 0,
                'reason': reval.reval_reason,
                'reference': reval.reval_reference,
                'user': reval.reval_user
            }
            for reval in revaluations
        ]
        
    def _get_valuation_cost(self, stock: StockMasterRec, method: str) -> Decimal:
        """Get valuation cost based on method"""
        if method == 'AVERAGE':
            return stock.stock_average_cost
        elif method == 'STANDARD':
            return stock.stock_standard_cost
        elif method == 'LAST':
            return stock.stock_last_cost
        elif method == 'FIFO':
            # Would implement FIFO cost calculation
            return stock.stock_average_cost
        else:
            return stock.stock_average_cost
            
    def _get_warehouse_quantity(self, stock_code: str, warehouse: str) -> Decimal:
        """Get quantity for specific warehouse"""
        result = self.db.query(
            func.sum(StockLocationRec.loc_qty_on_hand)
        ).filter(
            and_(
                StockLocationRec.loc_stock_code == stock_code,
                StockLocationRec.loc_warehouse == warehouse,
                StockLocationRec.loc_active == 'Y'
            )
        ).scalar()
        
        return result or Decimal('0')
        
    def _get_warehouse_breakdown(self, stock_code: str, unit_cost: Decimal) -> List[Dict]:
        """Get warehouse breakdown for stock item"""
        locations = self.db.query(StockLocationRec).filter(
            and_(
                StockLocationRec.loc_stock_code == stock_code,
                StockLocationRec.loc_active == 'Y',
                StockLocationRec.loc_qty_on_hand > 0
            )
        ).all()
        
        warehouse_data = {}
        for loc in locations:
            wh = loc.loc_warehouse
            if wh not in warehouse_data:
                warehouse_data[wh] = Decimal('0')
            warehouse_data[wh] += loc.loc_qty_on_hand
            
        return [
            {
                'warehouse': wh,
                'quantity': float(qty),
                'value': float(qty * unit_cost)
            }
            for wh, qty in warehouse_data.items()
        ]
        
    def _calculate_abc_analysis(self, valuation_data: List[Dict]) -> Dict:
        """Calculate ABC analysis from valuation data"""
        # Sort by value descending
        sorted_items = sorted(valuation_data, key=lambda x: x['total_value'], reverse=True)
        total_value = sum(item['total_value'] for item in sorted_items)
        
        # Calculate cumulative percentages
        cumulative_value = 0
        a_items = []
        b_items = []
        c_items = []
        
        for item in sorted_items:
            cumulative_value += item['total_value']
            cumulative_pct = (cumulative_value / total_value * 100) if total_value > 0 else 0
            
            if cumulative_pct <= 80:
                a_items.append(item)
            elif cumulative_pct <= 95:
                b_items.append(item)
            else:
                c_items.append(item)
                
        return {
            'class_a': {
                'items': len(a_items),
                'value': sum(item['total_value'] for item in a_items),
                'percentage': (sum(item['total_value'] for item in a_items) / total_value * 100) if total_value > 0 else 0
            },
            'class_b': {
                'items': len(b_items),
                'value': sum(item['total_value'] for item in b_items),
                'percentage': (sum(item['total_value'] for item in b_items) / total_value * 100) if total_value > 0 else 0
            },
            'class_c': {
                'items': len(c_items),
                'value': sum(item['total_value'] for item in c_items),
                'percentage': (sum(item['total_value'] for item in c_items) / total_value * 100) if total_value > 0 else 0
            }
        }
        
    def _calculate_variance_analysis(self, valuation_data: List[Dict], method: str) -> Dict:
        """Calculate variance between different valuation methods"""
        if method == 'AVERAGE':
            compare_method = 'STANDARD'
        else:
            compare_method = 'AVERAGE'
            
        variances = []
        total_variance = Decimal('0')
        
        for item in valuation_data[:50]:  # Limit to top 50 for performance
            stock_code = item['stock_code']
            stock, _ = self.stock_handler.process(4, key_value=stock_code)
            
            if stock:
                current_cost = Decimal(str(item['unit_cost']))
                compare_cost = self._get_valuation_cost(stock, compare_method)
                
                if compare_cost != current_cost:
                    variance_amount = (compare_cost - current_cost) * Decimal(str(item['quantity']))
                    variance_pct = float((compare_cost - current_cost) / current_cost * 100) if current_cost > 0 else 0
                    
                    variances.append({
                        'stock_code': stock_code,
                        'current_cost': float(current_cost),
                        'compare_cost': float(compare_cost),
                        'variance_amount': float(variance_amount),
                        'variance_pct': variance_pct
                    })
                    
                    total_variance += abs(variance_amount)
                    
        return {
            'comparison_method': compare_method,
            'total_variance': float(total_variance),
            'item_count': len(variances),
            'items': variances[:20]  # Top 20 variances
        }
        
    def _get_slow_moving_recommendations(self, stock: StockMasterRec, days_static: int, item_value: float) -> List[str]:
        """Get recommendations for slow-moving stock"""
        recommendations = []
        
        if days_static > 180:
            recommendations.append("Consider obsolescence provision")
        if days_static > 90:
            recommendations.append("Review stock levels")
        if item_value > 1000:
            recommendations.append("High value - priority review")
        if stock.stock_on_hand > stock.stock_max_qty:
            recommendations.append("Quantity exceeds maximum")
        if stock.stock_abc_class == 'C':
            recommendations.append("Low value class - consider disposal")
            
        return recommendations
        
    def _summarize_slow_moving(self, analysis_data: List[Dict]) -> Dict:
        """Summarize slow-moving analysis"""
        if not analysis_data:
            return {}
            
        # Group by days static ranges
        ranges = {
            '90-180 days': [],
            '180-365 days': [],
            'Over 1 year': []
        }
        
        for item in analysis_data:
            days = item['days_static']
            if days <= 180:
                ranges['90-180 days'].append(item)
            elif days <= 365:
                ranges['180-365 days'].append(item)
            else:
                ranges['Over 1 year'].append(item)
                
        return {
            range_name: {
                'items': len(items),
                'value': sum(item['total_value'] for item in items)
            }
            for range_name, items in ranges.items()
        }
        
    def _post_revaluation_to_gl(self, revaluation: StockRevalRec, stock: StockMasterRec):
        """Post revaluation to General Ledger"""
        system_rec, _ = self.system_handler.read_system_params()
        if not system_rec or system_rec.gl_interface != 'Y':
            return
            
        je_service = JournalEntryService(self.db, self.current_user)
        
        # Create batch
        batch = je_service.create_journal_batch(
            description=f"Stock Revaluation {revaluation.reval_no}",
            source="STOCK"
        )
        
        if revaluation.reval_variance > 0:
            # Positive revaluation: Dr Stock, Cr Revaluation Reserve
            je_service.add_journal_line(batch.batch_no, {
                'account': system_rec.bl_stock_ac,
                'debit': float(abs(revaluation.reval_variance)),
                'credit': 0,
                'reference': revaluation.reval_reference,
                'description': f"Reval+ {stock.stock_code}"
            })
            
            je_service.add_journal_line(batch.batch_no, {
                'account': system_rec.bl_reval_res_ac,
                'debit': 0,
                'credit': float(abs(revaluation.reval_variance)),
                'reference': revaluation.reval_reference,
                'description': f"Reval+ {stock.stock_code}"
            })
        else:
            # Negative revaluation: Dr Revaluation Reserve, Cr Stock
            je_service.add_journal_line(batch.batch_no, {
                'account': system_rec.bl_reval_res_ac,
                'debit': float(abs(revaluation.reval_variance)),
                'credit': 0,
                'reference': revaluation.reval_reference,
                'description': f"Reval- {stock.stock_code}"
            })
            
            je_service.add_journal_line(batch.batch_no, {
                'account': system_rec.bl_stock_ac,
                'debit': 0,
                'credit': float(abs(revaluation.reval_variance)),
                'reference': revaluation.reval_reference,
                'description': f"Reval- {stock.stock_code}"
            })
            
        # Post the batch
        je_service.post_batch(batch.batch_no)
        
    def _get_next_revaluation_number(self) -> str:
        """Generate next revaluation number"""
        return f"REV{datetime.now().strftime('%Y%m%d%H%M%S')}"
        
    def _generate_summary_report(self, options: Dict) -> Dict:
        """Generate summary valuation report"""
        return self.calculate_stock_valuation(options)
        
    def _generate_detailed_report(self, options: Dict) -> Dict:
        """Generate detailed valuation report"""
        valuation = self.calculate_stock_valuation(options)
        
        # Add additional details like location breakdown
        for item in valuation['items']:
            stock_code = item['stock_code']
            item['locations'] = self._get_warehouse_breakdown(stock_code, Decimal(str(item['unit_cost'])))
            
        return valuation
        
    def _generate_variance_report(self, options: Dict) -> Dict:
        """Generate variance report"""
        base_valuation = self.calculate_stock_valuation(options)
        
        # Compare with different method
        alt_method = 'STANDARD' if options.get('method', 'AVERAGE') == 'AVERAGE' else 'AVERAGE'
        alt_options = options.copy()
        alt_options['method'] = alt_method
        alt_valuation = self.calculate_stock_valuation(alt_options)
        
        # Calculate variances
        variances = []
        for base_item in base_valuation['items']:
            alt_item = next((item for item in alt_valuation['items'] if item['stock_code'] == base_item['stock_code']), None)
            if alt_item:
                variance = alt_item['total_value'] - base_item['total_value']
                if abs(variance) > 0.01:  # Only significant variances
                    variances.append({
                        'stock_code': base_item['stock_code'],
                        'description': base_item['description'],
                        'base_value': base_item['total_value'],
                        'alt_value': alt_item['total_value'],
                        'variance': variance,
                        'variance_pct': (variance / base_item['total_value'] * 100) if base_item['total_value'] > 0 else 0
                    })
                    
        return {
            'base_method': options.get('method', 'AVERAGE'),
            'comparison_method': alt_method,
            'total_base_value': base_valuation['summary']['total_value'],
            'total_alt_value': alt_valuation['summary']['total_value'],
            'total_variance': alt_valuation['summary']['total_value'] - base_valuation['summary']['total_value'],
            'variances': sorted(variances, key=lambda x: abs(x['variance']), reverse=True)
        }
        
    def _generate_movement_report(self, options: Dict) -> Dict:
        """Generate movement-based valuation report"""
        # This would analyze valuation based on recent movements
        return {"report": "movement_valuation", "data": "Not implemented"}
        
    def _generate_comparison_report(self, options: Dict) -> Dict:
        """Generate comparison report between periods"""
        # This would compare valuations between different dates
        return {"report": "period_comparison", "data": "Not implemented"}