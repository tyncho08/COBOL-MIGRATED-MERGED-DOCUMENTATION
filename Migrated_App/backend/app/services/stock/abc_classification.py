"""
ABC Classification Service - ST140 migration
Handles automatic ABC classification analysis and updates
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from datetime import datetime, timedelta
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func, desc, asc

from app.services.file_handlers.stock_handler import StockFileHandler
from app.services.file_handlers.system_handler import SystemFileHandler
from app.models.stock import (
    StockMasterRec, StockMovementRec, AbcClassificationRec,
    AbcAnalysisRec, StockVelocityRec
)
from app.core.security import log_user_action
from app.models.auth import User


class ABCClassificationService:
    """
    ABC Classification functionality
    Implements ST140 - ABC classification analysis and updates
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.stock_handler = StockFileHandler(db)
        self.system_handler = SystemFileHandler(db)
        
    def run_abc_analysis(self, analysis_request: Dict) -> Tuple[bool, Optional[str]]:
        """
        Run ABC classification analysis
        Returns (success, error_message or analysis_results)
        """
        warehouse = analysis_request.get('warehouse', 'MAIN')
        analysis_period = analysis_request.get('period_months', 12)
        criteria = analysis_request.get('criteria', 'VALUE')  # VALUE, MOVEMENT, USAGE
        abc_percentages = analysis_request.get('abc_percentages', {'A': 80, 'B': 95, 'C': 100})
        update_master = analysis_request.get('update_master', True)
        
        try:
            # Get analysis period dates
            end_date = datetime.now()
            start_date = end_date - timedelta(days=analysis_period * 30)
            start_date_int = int(start_date.strftime("%Y%m%d"))
            end_date_int = int(end_date.strftime("%Y%m%d"))
            
            # Create analysis header
            analysis_no = self._get_next_analysis_number()
            
            analysis = AbcAnalysisRec(
                analysis_no=analysis_no,
                analysis_warehouse=warehouse,
                analysis_criteria=criteria,
                analysis_period_months=analysis_period,
                analysis_start_date=start_date_int,
                analysis_end_date=end_date_int,
                analysis_status='RUNNING',
                analysis_created_date=int(datetime.now().strftime("%Y%m%d")),
                analysis_created_time=int(datetime.now().strftime("%H%M%S")),
                analysis_created_by=self.current_user.username if self.current_user else 'SYSTEM',
                analysis_a_percent=abc_percentages.get('A', 80),
                analysis_b_percent=abc_percentages.get('B', 95),
                analysis_c_percent=abc_percentages.get('C', 100)
            )
            
            self.db.add(analysis)
            self.db.flush()
            
            # Analyze stock items
            if criteria == 'VALUE':
                stock_data = self._analyze_by_value(warehouse, start_date_int, end_date_int)
            elif criteria == 'MOVEMENT':
                stock_data = self._analyze_by_movement(warehouse, start_date_int, end_date_int)
            elif criteria == 'USAGE':
                stock_data = self._analyze_by_usage(warehouse, start_date_int, end_date_int)
            else:
                return False, f"Unknown analysis criteria: {criteria}"
                
            if not stock_data:
                return False, "No stock data found for analysis"
                
            # Calculate ABC classifications
            classified_items = self._calculate_abc_classes(stock_data, abc_percentages)
            
            # Create classification records
            a_count = 0
            b_count = 0
            c_count = 0
            
            for item in classified_items:
                classification = AbcClassificationRec(
                    class_analysis_no=analysis_no,
                    class_stock_code=item['stock_code'],
                    class_warehouse=warehouse,
                    class_abc_class=item['abc_class'],
                    class_value=item['value'],
                    class_quantity=item['quantity'],
                    class_transactions=item['transactions'],
                    class_percentage=item['percentage'],
                    class_cumulative_percentage=item['cumulative_percentage'],
                    class_ranking=item['ranking'],
                    class_previous_class=item['previous_class'],
                    class_changed='Y' if item['abc_class'] != item['previous_class'] else 'N'
                )
                
                self.db.add(classification)
                
                if item['abc_class'] == 'A':
                    a_count += 1
                elif item['abc_class'] == 'B':
                    b_count += 1
                else:
                    c_count += 1
                    
            # Update analysis totals
            analysis.analysis_status = 'COMPLETE'
            analysis.analysis_total_items = len(classified_items)
            analysis.analysis_a_items = a_count
            analysis.analysis_b_items = b_count
            analysis.analysis_c_items = c_count
            analysis.analysis_completed_date = int(datetime.now().strftime("%Y%m%d"))
            analysis.analysis_completed_time = int(datetime.now().strftime("%H%M%S"))
            
            # Update stock master if requested
            if update_master:
                self._update_stock_master_abc_classes(classified_items)
                
            self.db.commit()
            
            # Generate summary
            summary = {
                'analysis_no': analysis_no,
                'warehouse': warehouse,
                'criteria': criteria,
                'period_months': analysis_period,
                'total_items': len(classified_items),
                'classifications': {
                    'A': a_count,
                    'B': b_count,
                    'C': c_count
                },
                'changes': len([i for i in classified_items if i['abc_class'] != i['previous_class']]),
                'updated_master': update_master
            }
            
            # Log analysis
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="RUN_ABC_ANALYSIS",
                    table="abc_analysis_rec",
                    key=analysis_no,
                    new_values=summary,
                    module="STOCK"
                )
                
            return True, summary
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def get_abc_analysis_results(self, analysis_no: str, filters: Optional[Dict] = None) -> List[Dict]:
        """Get ABC analysis results with optional filters"""
        filters = filters or {}
        
        query = self.db.query(AbcClassificationRec).filter(
            AbcClassificationRec.class_analysis_no == analysis_no
        )
        
        # Apply filters
        if filters.get('abc_class'):
            query = query.filter(AbcClassificationRec.class_abc_class == filters['abc_class'])
        if filters.get('changed_only'):
            query = query.filter(AbcClassificationRec.class_changed == 'Y')
        if filters.get('min_value'):
            query = query.filter(AbcClassificationRec.class_value >= filters['min_value'])
            
        # Order by ranking
        classifications = query.order_by(AbcClassificationRec.class_ranking).all()
        
        results = []
        for classification in classifications:
            # Get stock details
            stock, _ = self.stock_handler.process(4, key_value=classification.class_stock_code)
            
            results.append({
                'stock_code': classification.class_stock_code,
                'description': stock.stock_desc if stock else '',
                'abc_class': classification.class_abc_class,
                'previous_class': classification.class_previous_class,
                'changed': classification.class_changed == 'Y',
                'value': float(classification.class_value),
                'quantity': float(classification.class_quantity),
                'transactions': classification.class_transactions,
                'percentage': float(classification.class_percentage),
                'cumulative_percentage': float(classification.class_cumulative_percentage),
                'ranking': classification.class_ranking,
                'unit_cost': stock.stock_average_cost if stock else Decimal('0')
            })
            
        return results
        
    def calculate_stock_velocity(self, velocity_request: Dict) -> Tuple[bool, Optional[str]]:
        """
        Calculate stock velocity metrics
        Returns (success, error_message or velocity_data)
        """
        warehouse = velocity_request.get('warehouse', 'MAIN')
        analysis_period = velocity_request.get('period_months', 12)
        stock_codes = velocity_request.get('stock_codes', [])
        
        try:
            # Get analysis period
            end_date = datetime.now()
            start_date = end_date - timedelta(days=analysis_period * 30)
            start_date_int = int(start_date.strftime("%Y%m%d"))
            end_date_int = int(end_date.strftime("%Y%m%d"))
            
            # Build query for stock items
            if stock_codes:
                stock_items = self.db.query(StockMasterRec).filter(
                    StockMasterRec.stock_code.in_(stock_codes)
                ).all()
            else:
                stock_items = self.db.query(StockMasterRec).filter(
                    StockMasterRec.stock_active == 'Y'
                ).all()
                
            velocity_data = []
            
            for stock in stock_items:
                # Calculate velocity metrics
                velocity_metrics = self._calculate_item_velocity(
                    stock, warehouse, start_date_int, end_date_int
                )
                
                if velocity_metrics:
                    # Create/update velocity record
                    existing_velocity = self.db.query(StockVelocityRec).filter(
                        and_(
                            StockVelocityRec.vel_stock_code == stock.stock_code,
                            StockVelocityRec.vel_warehouse == warehouse
                        )
                    ).first()
                    
                    if existing_velocity:
                        # Update existing record
                        for key, value in velocity_metrics.items():
                            if hasattr(existing_velocity, f"vel_{key}"):
                                setattr(existing_velocity, f"vel_{key}", value)
                        existing_velocity.vel_last_calculated = int(datetime.now().strftime("%Y%m%d"))
                    else:
                        # Create new record
                        velocity_record = StockVelocityRec(
                            vel_stock_code=stock.stock_code,
                            vel_warehouse=warehouse,
                            vel_turnover_rate=velocity_metrics.get('turnover_rate', Decimal('0')),
                            vel_velocity_class=velocity_metrics.get('velocity_class', 'SLOW'),
                            vel_avg_monthly_usage=velocity_metrics.get('avg_monthly_usage', Decimal('0')),
                            vel_days_cover=velocity_metrics.get('days_cover', 0),
                            vel_reorder_frequency=velocity_metrics.get('reorder_frequency', 0),
                            vel_last_movement_date=velocity_metrics.get('last_movement_date', 0),
                            vel_last_calculated=int(datetime.now().strftime("%Y%m%d"))
                        )
                        self.db.add(velocity_record)
                        
                    velocity_data.append({
                        'stock_code': stock.stock_code,
                        'description': stock.stock_desc,
                        **velocity_metrics
                    })
                    
            self.db.commit()
            
            return True, {
                'warehouse': warehouse,
                'period_months': analysis_period,
                'items_analyzed': len(velocity_data),
                'velocity_data': velocity_data
            }
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def get_velocity_analysis(self, warehouse: str, velocity_class: Optional[str] = None) -> List[Dict]:
        """Get stock velocity analysis"""
        query = self.db.query(StockVelocityRec).filter(
            StockVelocityRec.vel_warehouse == warehouse
        )
        
        if velocity_class:
            query = query.filter(StockVelocityRec.vel_velocity_class == velocity_class)
            
        velocity_records = query.order_by(desc(StockVelocityRec.vel_turnover_rate)).all()
        
        results = []
        for velocity in velocity_records:
            # Get stock details
            stock, _ = self.stock_handler.process(4, key_value=velocity.vel_stock_code)
            
            results.append({
                'stock_code': velocity.vel_stock_code,
                'description': stock.stock_desc if stock else '',
                'abc_class': stock.stock_abc_class if stock else '',
                'velocity_class': velocity.vel_velocity_class,
                'turnover_rate': float(velocity.vel_turnover_rate),
                'avg_monthly_usage': float(velocity.vel_avg_monthly_usage),
                'days_cover': velocity.vel_days_cover,
                'reorder_frequency': velocity.vel_reorder_frequency,
                'last_movement_date': velocity.vel_last_movement_date,
                'on_hand_qty': stock.stock_on_hand if stock else Decimal('0'),
                'unit_cost': stock.stock_average_cost if stock else Decimal('0'),
                'total_value': float((stock.stock_on_hand * stock.stock_average_cost) if stock else 0)
            })
            
        return results
        
    def generate_slow_moving_report(self, report_request: Dict) -> Dict:
        """Generate slow moving stock report"""
        warehouse = report_request.get('warehouse', 'MAIN')
        days_threshold = report_request.get('days_threshold', 90)
        min_value = report_request.get('min_value', 0)
        
        try:
            # Calculate cutoff date
            cutoff_date = int((datetime.now() - timedelta(days=days_threshold)).strftime("%Y%m%d"))
            
            # Get slow moving items
            slow_moving_query = self.db.query(StockMasterRec).filter(
                and_(
                    StockMasterRec.stock_active == 'Y',
                    StockMasterRec.stock_on_hand > 0,
                    or_(
                        StockMasterRec.stock_last_issue_date < cutoff_date,
                        StockMasterRec.stock_last_issue_date == 0
                    )
                )
            )
            
            if min_value > 0:
                slow_moving_query = slow_moving_query.filter(
                    StockMasterRec.stock_on_hand * StockMasterRec.stock_average_cost >= min_value
                )
                
            slow_moving_items = slow_moving_query.all()
            
            # Categorize by age and value
            categories = {
                'critical': [],  # > 365 days
                'very_slow': [],  # 181-365 days
                'slow': [],      # 91-180 days
                'attention': []  # threshold to 90 days
            }
            
            total_value = Decimal('0')
            
            for stock in slow_moving_items:
                days_since_movement = self._calculate_days_since_last_movement(stock)
                item_value = stock.stock_on_hand * stock.stock_average_cost
                total_value += item_value
                
                item_data = {
                    'stock_code': stock.stock_code,
                    'description': stock.stock_desc,
                    'abc_class': stock.stock_abc_class,
                    'on_hand_qty': float(stock.stock_on_hand),
                    'unit_cost': float(stock.stock_average_cost),
                    'total_value': float(item_value),
                    'days_since_movement': days_since_movement,
                    'last_issue_date': stock.stock_last_issue_date,
                    'last_receipt_date': stock.stock_last_receipt_date
                }
                
                if days_since_movement > 365:
                    categories['critical'].append(item_data)
                elif days_since_movement > 180:
                    categories['very_slow'].append(item_data)
                elif days_since_movement > 90:
                    categories['slow'].append(item_data)
                else:
                    categories['attention'].append(item_data)
                    
            # Sort each category by value descending
            for category in categories.values():
                category.sort(key=lambda x: x['total_value'], reverse=True)
                
            return {
                'warehouse': warehouse,
                'days_threshold': days_threshold,
                'report_date': datetime.now().strftime("%Y-%m-%d"),
                'summary': {
                    'total_items': len(slow_moving_items),
                    'total_value': float(total_value),
                    'critical_items': len(categories['critical']),
                    'very_slow_items': len(categories['very_slow']),
                    'slow_items': len(categories['slow']),
                    'attention_items': len(categories['attention'])
                },
                'categories': categories,
                'recommendations': self._generate_slow_moving_recommendations(categories)
            }
            
        except Exception as e:
            return {"error": str(e)}
            
    def _analyze_by_value(self, warehouse: str, start_date: int, end_date: int) -> List[Dict]:
        """Analyze stock by value (usage value)"""
        # Get stock movements for the period
        movements = self.db.query(
            StockMovementRec.move_stock_code,
            func.sum(StockMovementRec.move_qty_out * StockMovementRec.move_unit_cost).label('total_value'),
            func.sum(StockMovementRec.move_qty_out).label('total_quantity'),
            func.count(StockMovementRec.move_id).label('transactions')
        ).filter(
            and_(
                StockMovementRec.move_warehouse == warehouse,
                StockMovementRec.move_date >= start_date,
                StockMovementRec.move_date <= end_date,
                StockMovementRec.move_qty_out > 0
            )
        ).group_by(StockMovementRec.move_stock_code).all()
        
        return [
            {
                'stock_code': move.move_stock_code,
                'value': float(move.total_value or 0),
                'quantity': float(move.total_quantity or 0),
                'transactions': move.transactions
            }
            for move in movements
        ]
        
    def _analyze_by_movement(self, warehouse: str, start_date: int, end_date: int) -> List[Dict]:
        """Analyze stock by movement quantity"""
        movements = self.db.query(
            StockMovementRec.move_stock_code,
            func.sum(StockMovementRec.move_qty_out).label('total_quantity'),
            func.sum(StockMovementRec.move_qty_out * StockMovementRec.move_unit_cost).label('total_value'),
            func.count(StockMovementRec.move_id).label('transactions')
        ).filter(
            and_(
                StockMovementRec.move_warehouse == warehouse,
                StockMovementRec.move_date >= start_date,
                StockMovementRec.move_date <= end_date,
                StockMovementRec.move_qty_out > 0
            )
        ).group_by(StockMovementRec.move_stock_code).all()
        
        return [
            {
                'stock_code': move.move_stock_code,
                'value': float(move.total_quantity or 0),  # Use quantity as "value" for sorting
                'quantity': float(move.total_quantity or 0),
                'transactions': move.transactions
            }
            for move in movements
        ]
        
    def _analyze_by_usage(self, warehouse: str, start_date: int, end_date: int) -> List[Dict]:
        """Analyze stock by usage frequency (transaction count)"""
        movements = self.db.query(
            StockMovementRec.move_stock_code,
            func.count(StockMovementRec.move_id).label('transactions'),
            func.sum(StockMovementRec.move_qty_out).label('total_quantity'),
            func.sum(StockMovementRec.move_qty_out * StockMovementRec.move_unit_cost).label('total_value')
        ).filter(
            and_(
                StockMovementRec.move_warehouse == warehouse,
                StockMovementRec.move_date >= start_date,
                StockMovementRec.move_date <= end_date,
                StockMovementRec.move_qty_out > 0
            )
        ).group_by(StockMovementRec.move_stock_code).all()
        
        return [
            {
                'stock_code': move.move_stock_code,
                'value': float(move.transactions),  # Use transaction count as "value" for sorting
                'quantity': float(move.total_quantity or 0),
                'transactions': move.transactions
            }
            for move in movements
        ]
        
    def _calculate_abc_classes(self, stock_data: List[Dict], abc_percentages: Dict) -> List[Dict]:
        """Calculate ABC classes based on cumulative value"""
        # Sort by value descending
        sorted_data = sorted(stock_data, key=lambda x: x['value'], reverse=True)
        
        # Calculate total value
        total_value = sum(item['value'] for item in sorted_data)
        
        # Add rankings and cumulative percentages
        cumulative_value = 0
        
        for i, item in enumerate(sorted_data):
            cumulative_value += item['value']
            cumulative_percentage = (cumulative_value / total_value * 100) if total_value > 0 else 0
            
            # Determine ABC class
            if cumulative_percentage <= abc_percentages.get('A', 80):
                abc_class = 'A'
            elif cumulative_percentage <= abc_percentages.get('B', 95):
                abc_class = 'B'
            else:
                abc_class = 'C'
                
            # Get previous class
            stock, _ = self.stock_handler.process(4, key_value=item['stock_code'])
            previous_class = stock.stock_abc_class if stock else 'C'
            
            item.update({
                'ranking': i + 1,
                'percentage': (item['value'] / total_value * 100) if total_value > 0 else 0,
                'cumulative_percentage': cumulative_percentage,
                'abc_class': abc_class,
                'previous_class': previous_class
            })
            
        return sorted_data
        
    def _update_stock_master_abc_classes(self, classified_items: List[Dict]):
        """Update ABC classes in stock master"""
        for item in classified_items:
            stock = self.db.query(StockMasterRec).filter(
                StockMasterRec.stock_code == item['stock_code']
            ).first()
            
            if stock:
                stock.stock_abc_class = item['abc_class']
                stock.stock_abc_last_updated = int(datetime.now().strftime("%Y%m%d"))
                
    def _calculate_item_velocity(self, stock: StockMasterRec, warehouse: str, 
                               start_date: int, end_date: int) -> Optional[Dict]:
        """Calculate velocity metrics for a stock item"""
        # Get total issues for period
        total_issues = self.db.query(
            func.sum(StockMovementRec.move_qty_out)
        ).filter(
            and_(
                StockMovementRec.move_stock_code == stock.stock_code,
                StockMovementRec.move_warehouse == warehouse,
                StockMovementRec.move_date >= start_date,
                StockMovementRec.move_date <= end_date,
                StockMovementRec.move_qty_out > 0
            )
        ).scalar() or Decimal('0')
        
        if total_issues == 0:
            return None
            
        # Calculate metrics
        period_months = (datetime.now() - datetime.strptime(str(start_date), "%Y%m%d")).days / 30
        avg_monthly_usage = total_issues / Decimal(str(period_months)) if period_months > 0 else Decimal('0')
        
        # Calculate turnover rate
        avg_inventory = stock.stock_on_hand  # Simplified - could use average over period
        turnover_rate = total_issues / avg_inventory if avg_inventory > 0 else Decimal('0')
        
        # Calculate days cover
        daily_usage = avg_monthly_usage / 30 if avg_monthly_usage > 0 else Decimal('0')
        days_cover = int(stock.stock_on_hand / daily_usage) if daily_usage > 0 else 999
        
        # Determine velocity class
        if float(turnover_rate) >= 12:  # 12+ turns per year
            velocity_class = 'FAST'
        elif float(turnover_rate) >= 6:  # 6-12 turns per year
            velocity_class = 'MEDIUM'
        elif float(turnover_rate) >= 2:  # 2-6 turns per year
            velocity_class = 'SLOW'
        else:
            velocity_class = 'DEAD'
            
        # Get last movement date
        last_movement = self.db.query(
            func.max(StockMovementRec.move_date)
        ).filter(
            and_(
                StockMovementRec.move_stock_code == stock.stock_code,
                StockMovementRec.move_warehouse == warehouse,
                StockMovementRec.move_qty_out > 0
            )
        ).scalar() or 0
        
        return {
            'turnover_rate': turnover_rate,
            'velocity_class': velocity_class,
            'avg_monthly_usage': avg_monthly_usage,
            'days_cover': days_cover,
            'reorder_frequency': int(12 / float(turnover_rate)) if turnover_rate > 0 else 12,
            'last_movement_date': last_movement
        }
        
    def _calculate_days_since_last_movement(self, stock: StockMasterRec) -> int:
        """Calculate days since last movement"""
        last_date = max(stock.stock_last_issue_date or 0, stock.stock_last_receipt_date or 0)
        
        if last_date == 0:
            return 999  # No movement recorded
            
        last_movement_date = datetime.strptime(str(last_date), "%Y%m%d")
        return (datetime.now() - last_movement_date).days
        
    def _generate_slow_moving_recommendations(self, categories: Dict) -> List[Dict]:
        """Generate recommendations for slow moving stock"""
        recommendations = []
        
        if categories['critical']:
            recommendations.append({
                'type': 'DISPOSAL',
                'priority': 'HIGH',
                'description': f"Consider disposal of {len(categories['critical'])} critical items (>365 days)",
                'action': 'Review for obsolescence and disposal'
            })
            
        if categories['very_slow']:
            recommendations.append({
                'type': 'PRICE_REDUCTION',
                'priority': 'HIGH',
                'description': f"Consider price reduction for {len(categories['very_slow'])} very slow items",
                'action': 'Implement clearance pricing strategy'
            })
            
        if categories['slow']:
            recommendations.append({
                'type': 'PROMOTION',
                'priority': 'MEDIUM',
                'description': f"Consider promotion for {len(categories['slow'])} slow moving items",
                'action': 'Bundle with fast-moving items or special offers'
            })
            
        return recommendations
        
    def _get_next_analysis_number(self) -> str:
        """Generate next analysis number"""
        return f"ABC{datetime.now().strftime('%Y%m%d%H%M%S')}"