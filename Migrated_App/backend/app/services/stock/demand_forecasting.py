"""
Demand Forecasting Service - ST210 migration
Handles demand forecasting and planning algorithms
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from datetime import datetime, timedelta
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func, desc
import statistics
import math

from app.services.file_handlers.stock_handler import StockFileHandler
from app.services.file_handlers.system_handler import SystemFileHandler
from app.models.stock import (
    StockMasterRec, DemandForecastRec, ForecastModelRec, 
    ForecastAccuracyRec, SeasonalPatternRec, StockMovementRec
)
from app.models.sales import SalesOrderLineRec
from app.core.security import log_user_action
from app.models.auth import User


class DemandForecastingService:
    """
    Demand Forecasting functionality
    Implements ST210 - demand forecasting and planning
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.stock_handler = StockFileHandler(db)
        self.system_handler = SystemFileHandler(db)
        
    def generate_forecast(self, forecast_request: Dict) -> Tuple[bool, Optional[str]]:
        """
        Generate demand forecast for stock items
        Returns (success, error_message or forecast_data)
        """
        stock_codes = forecast_request.get('stock_codes', [])
        warehouse = forecast_request.get('warehouse', 'MAIN')
        forecast_periods = forecast_request.get('periods', 12)  # Number of periods to forecast
        period_type = forecast_request.get('period_type', 'MONTH')  # WEEK, MONTH, QUARTER
        model_type = forecast_request.get('model', 'AUTO')  # AUTO, MOVING_AVG, EXP_SMOOTH, LINEAR_TREND
        history_periods = forecast_request.get('history_periods', 24)
        
        if not stock_codes:
            # Get all active stock items if none specified
            stock_items = self.db.query(StockMasterRec).filter(
                StockMasterRec.stock_active == 'Y'
            ).all()
            stock_codes = [stock.stock_code for stock in stock_items]
            
        try:
            forecast_run_id = self._get_next_forecast_run_id()
            forecasts_created = 0
            
            for stock_code in stock_codes:
                # Get historical demand data
                historical_data = self._get_historical_demand(
                    stock_code, warehouse, history_periods, period_type
                )
                
                if not historical_data or len(historical_data) < 3:
                    continue  # Need minimum history for forecasting
                    
                # Determine best model if AUTO
                if model_type == 'AUTO':
                    best_model = self._select_best_model(historical_data)
                else:
                    best_model = model_type
                    
                # Generate forecast using selected model
                forecast_values = self._generate_forecast_values(
                    historical_data, forecast_periods, best_model
                )
                
                if not forecast_values:
                    continue
                    
                # Detect seasonality
                seasonal_factors = self._detect_seasonality(historical_data, period_type)
                
                # Apply seasonal adjustments
                if seasonal_factors:
                    forecast_values = self._apply_seasonal_adjustment(
                        forecast_values, seasonal_factors, period_type
                    )
                    
                # Create forecast records
                for i, forecast_value in enumerate(forecast_values):
                    period_date = self._calculate_period_date(i + 1, period_type)
                    
                    forecast_record = DemandForecastRec(
                        forecast_run_id=forecast_run_id,
                        forecast_stock_code=stock_code,
                        forecast_warehouse=warehouse,
                        forecast_period=i + 1,
                        forecast_period_date=period_date,
                        forecast_period_type=period_type,
                        forecast_model=best_model,
                        forecast_quantity=Decimal(str(max(0, forecast_value))),
                        forecast_confidence=self._calculate_confidence_level(historical_data, best_model),
                        forecast_lower_bound=Decimal(str(max(0, forecast_value * 0.8))),
                        forecast_upper_bound=Decimal(str(forecast_value * 1.2)),
                        forecast_seasonal_factor=Decimal(str(seasonal_factors.get(i % len(seasonal_factors), 1.0) if seasonal_factors else 1.0)),
                        forecast_created_date=int(datetime.now().strftime("%Y%m%d")),
                        forecast_created_by=self.current_user.username if self.current_user else 'SYSTEM'
                    )
                    
                    self.db.add(forecast_record)
                    
                forecasts_created += 1
                
                # Update/create model performance record
                self._update_model_performance(stock_code, warehouse, best_model, historical_data)
                
            self.db.commit()
            
            # Log forecast generation
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="GENERATE_DEMAND_FORECAST",
                    table="demand_forecast_rec",
                    key=forecast_run_id,
                    new_values={
                        'warehouse': warehouse,
                        'items_forecast': forecasts_created,
                        'periods': forecast_periods,
                        'model': model_type
                    },
                    module="STOCK"
                )
                
            return True, {
                'forecast_run_id': forecast_run_id,
                'items_forecast': forecasts_created,
                'periods': forecast_periods,
                'model_used': model_type,
                'warehouse': warehouse
            }
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def calculate_safety_stock(self, safety_stock_request: Dict) -> Tuple[bool, Optional[str]]:
        """
        Calculate safety stock levels based on forecast variability
        Returns (success, error_message or calculation_results)
        """
        stock_codes = safety_stock_request.get('stock_codes', [])
        warehouse = safety_stock_request.get('warehouse', 'MAIN')
        service_level = safety_stock_request.get('service_level', 95.0)  # Percentage
        lead_time_days = safety_stock_request.get('lead_time_days', 7)
        
        try:
            safety_stock_calculations = []
            
            for stock_code in stock_codes:
                # Get latest forecast data
                forecasts = self.db.query(DemandForecastRec).filter(
                    and_(
                        DemandForecastRec.forecast_stock_code == stock_code,
                        DemandForecastRec.forecast_warehouse == warehouse
                    )
                ).order_by(desc(DemandForecastRec.forecast_run_id)).limit(12).all()
                
                if not forecasts:
                    continue
                    
                # Get historical demand variability
                historical_data = self._get_historical_demand(stock_code, warehouse, 12, 'MONTH')
                
                if len(historical_data) < 3:
                    continue
                    
                # Calculate demand variability (standard deviation)
                demand_values = [data['quantity'] for data in historical_data]
                mean_demand = statistics.mean(demand_values)
                std_deviation = statistics.stdev(demand_values) if len(demand_values) > 1 else 0
                
                # Calculate average forecast
                avg_forecast = statistics.mean([float(f.forecast_quantity) for f in forecasts])
                
                # Calculate lead time demand
                daily_demand = avg_forecast / 30  # Assuming monthly forecasts
                lead_time_demand = daily_demand * lead_time_days
                
                # Calculate safety stock using statistical formula
                # Safety Stock = Z-score * sqrt(lead_time) * demand_std_dev
                z_scores = {90.0: 1.28, 95.0: 1.65, 97.5: 1.96, 99.0: 2.33, 99.5: 2.58}
                z_score = z_scores.get(service_level, 1.65)
                
                # Adjust std deviation for lead time
                lead_time_std = std_deviation * math.sqrt(lead_time_days / 30)
                
                # Calculate safety stock
                safety_stock = z_score * lead_time_std
                
                # Calculate reorder point
                reorder_point = lead_time_demand + safety_stock
                
                # Calculate economic order quantity (simplified EOQ)
                annual_demand = avg_forecast * 12
                holding_cost_rate = 0.25  # 25% annual holding cost
                ordering_cost = 50  # Fixed ordering cost
                
                if annual_demand > 0:
                    eoq = math.sqrt((2 * annual_demand * ordering_cost) / 
                                   (float(self._get_stock_cost(stock_code)) * holding_cost_rate))
                else:
                    eoq = 0
                    
                safety_stock_calculations.append({
                    'stock_code': stock_code,
                    'warehouse': warehouse,
                    'current_safety_stock': self._get_current_safety_stock(stock_code, warehouse),
                    'recommended_safety_stock': round(safety_stock, 2),
                    'reorder_point': round(reorder_point, 2),
                    'economic_order_qty': round(eoq, 0),
                    'service_level': service_level,
                    'lead_time_days': lead_time_days,
                    'avg_demand': round(mean_demand, 2),
                    'demand_variability': round(std_deviation, 2),
                    'forecast_accuracy': self._calculate_forecast_accuracy(stock_code, warehouse)
                })
                
            return True, {
                'calculations': safety_stock_calculations,
                'service_level': service_level,
                'total_items': len(safety_stock_calculations)
            }
            
        except Exception as e:
            return False, str(e)
            
    def analyze_forecast_accuracy(self, accuracy_request: Dict) -> Dict:
        """Analyze forecast accuracy and model performance"""
        try:
            warehouse = accuracy_request.get('warehouse', 'MAIN')
            analysis_periods = accuracy_request.get('periods', 6)
            stock_code = accuracy_request.get('stock_code')
            
            # Build query
            query = self.db.query(DemandForecastRec)
            
            if warehouse:
                query = query.filter(DemandForecastRec.forecast_warehouse == warehouse)
            if stock_code:
                query = query.filter(DemandForecastRec.forecast_stock_code == stock_code)
                
            # Get forecasts from previous periods
            cutoff_date = int((datetime.now() - timedelta(days=analysis_periods * 30)).strftime("%Y%m%d"))
            forecasts = query.filter(
                DemandForecastRec.forecast_period_date >= cutoff_date
            ).all()
            
            if not forecasts:
                return {"message": "No forecast data found for analysis"}
                
            accuracy_analysis = {}
            model_performance = {}
            
            for forecast in forecasts:
                # Get actual demand for the forecast period
                actual_demand = self._get_actual_demand_for_period(
                    forecast.forecast_stock_code,
                    forecast.forecast_warehouse,
                    forecast.forecast_period_date,
                    forecast.forecast_period_type
                )
                
                if actual_demand is None:
                    continue
                    
                forecast_qty = float(forecast.forecast_quantity)
                
                # Calculate accuracy metrics
                error = actual_demand - forecast_qty
                abs_error = abs(error)
                pct_error = (error / actual_demand * 100) if actual_demand != 0 else 0
                abs_pct_error = abs(pct_error)
                
                stock_code = forecast.forecast_stock_code
                if stock_code not in accuracy_analysis:
                    accuracy_analysis[stock_code] = {
                        'forecasts': 0,
                        'total_error': 0,
                        'total_abs_error': 0,
                        'total_pct_error': 0,
                        'total_abs_pct_error': 0,
                        'forecast_values': [],
                        'actual_values': []
                    }
                    
                accuracy_data = accuracy_analysis[stock_code]
                accuracy_data['forecasts'] += 1
                accuracy_data['total_error'] += error
                accuracy_data['total_abs_error'] += abs_error
                accuracy_data['total_pct_error'] += pct_error
                accuracy_data['total_abs_pct_error'] += abs_pct_error
                accuracy_data['forecast_values'].append(forecast_qty)
                accuracy_data['actual_values'].append(actual_demand)
                
                # Model performance
                model = forecast.forecast_model
                if model not in model_performance:
                    model_performance[model] = {
                        'forecasts': 0,
                        'total_abs_pct_error': 0,
                        'items': set()
                    }
                    
                model_performance[model]['forecasts'] += 1
                model_performance[model]['total_abs_pct_error'] += abs_pct_error
                model_performance[model]['items'].add(stock_code)
                
            # Calculate summary statistics
            summary_stats = {}
            for stock_code, data in accuracy_analysis.items():
                if data['forecasts'] > 0:
                    summary_stats[stock_code] = {
                        'mean_error': data['total_error'] / data['forecasts'],
                        'mean_abs_error': data['total_abs_error'] / data['forecasts'],
                        'mean_pct_error': data['total_pct_error'] / data['forecasts'],
                        'mean_abs_pct_error': data['total_abs_pct_error'] / data['forecasts'],
                        'forecast_bias': 'OVER' if data['total_error'] / data['forecasts'] > 5 else 'UNDER' if data['total_error'] / data['forecasts'] < -5 else 'NEUTRAL',
                        'accuracy_grade': self._calculate_accuracy_grade(data['total_abs_pct_error'] / data['forecasts']),
                        'forecasts_analyzed': data['forecasts']
                    }
                    
            # Model comparison
            model_comparison = {}
            for model, data in model_performance.items():
                if data['forecasts'] > 0:
                    model_comparison[model] = {
                        'avg_abs_pct_error': data['total_abs_pct_error'] / data['forecasts'],
                        'forecasts_count': data['forecasts'],
                        'items_count': len(data['items'])
                    }
                    
            return {
                'analysis_period': analysis_periods,
                'warehouse': warehouse,
                'stock_code_filter': stock_code,
                'item_accuracy': summary_stats,
                'model_performance': model_comparison,
                'overall_stats': self._calculate_overall_accuracy_stats(summary_stats),
                'recommendations': self._generate_forecast_recommendations(summary_stats, model_comparison)
            }
            
        except Exception as e:
            return {"error": str(e)}
            
    def _get_historical_demand(self, stock_code: str, warehouse: str, 
                             periods: int, period_type: str) -> List[Dict]:
        """Get historical demand data"""
        # Calculate date range
        if period_type == 'WEEK':
            start_date = datetime.now() - timedelta(weeks=periods)
        elif period_type == 'MONTH':
            start_date = datetime.now() - timedelta(days=periods * 30)
        elif period_type == 'QUARTER':
            start_date = datetime.now() - timedelta(days=periods * 90)
        else:
            start_date = datetime.now() - timedelta(days=periods * 30)
            
        start_date_int = int(start_date.strftime("%Y%m%d"))
        
        # Get sales/issues from movements
        movements = self.db.query(StockMovementRec).filter(
            and_(
                StockMovementRec.move_stock_code == stock_code,
                StockMovementRec.move_warehouse == warehouse,
                StockMovementRec.move_date >= start_date_int,
                StockMovementRec.move_qty_out > 0
            )
        ).order_by(StockMovementRec.move_date).all()
        
        # Group by period
        period_data = {}
        
        for movement in movements:
            move_date = datetime.strptime(str(movement.move_date), "%Y%m%d")
            
            if period_type == 'WEEK':
                period_key = move_date.strftime("%Y-W%W")
            elif period_type == 'MONTH':
                period_key = move_date.strftime("%Y-%m")
            elif period_type == 'QUARTER':
                quarter = (move_date.month - 1) // 3 + 1
                period_key = f"{move_date.year}-Q{quarter}"
                
            if period_key not in period_data:
                period_data[period_key] = 0
                
            period_data[period_key] += float(movement.move_qty_out)
            
        # Convert to list format
        historical_data = []
        for period, quantity in sorted(period_data.items()):
            historical_data.append({
                'period': period,
                'quantity': quantity
            })
            
        return historical_data
        
    def _select_best_model(self, historical_data: List[Dict]) -> str:
        """Select best forecasting model based on historical data"""
        if len(historical_data) < 6:
            return 'MOVING_AVG'
            
        quantities = [data['quantity'] for data in historical_data]
        
        # Test different models and calculate accuracy
        models_to_test = ['MOVING_AVG', 'EXP_SMOOTH', 'LINEAR_TREND']
        model_accuracies = {}
        
        for model in models_to_test:
            # Use first 75% of data to train, last 25% to test
            train_size = int(len(quantities) * 0.75)
            train_data = quantities[:train_size]
            test_data = quantities[train_size:]
            
            if len(test_data) == 0:
                continue
                
            # Generate forecasts for test period
            test_forecasts = self._generate_forecast_values(
                [{'quantity': q} for q in train_data], 
                len(test_data), 
                model
            )
            
            if test_forecasts:
                # Calculate accuracy
                errors = [abs(actual - forecast) for actual, forecast in zip(test_data, test_forecasts)]
                avg_error = sum(errors) / len(errors) if errors else float('inf')
                model_accuracies[model] = avg_error
                
        # Return model with lowest error
        if model_accuracies:
            return min(model_accuracies.keys(), key=lambda k: model_accuracies[k])
        else:
            return 'MOVING_AVG'
            
    def _generate_forecast_values(self, historical_data: List[Dict], 
                                periods: int, model: str) -> List[float]:
        """Generate forecast values using specified model"""
        quantities = [data['quantity'] for data in historical_data]
        
        if not quantities:
            return []
            
        if model == 'MOVING_AVG':
            return self._moving_average_forecast(quantities, periods)
        elif model == 'EXP_SMOOTH':
            return self._exponential_smoothing_forecast(quantities, periods)
        elif model == 'LINEAR_TREND':
            return self._linear_trend_forecast(quantities, periods)
        else:
            return self._moving_average_forecast(quantities, periods)
            
    def _moving_average_forecast(self, quantities: List[float], periods: int) -> List[float]:
        """Simple moving average forecast"""
        if len(quantities) < 3:
            avg = sum(quantities) / len(quantities)
            return [avg] * periods
            
        # Use last 6 periods for moving average
        window_size = min(6, len(quantities))
        recent_avg = sum(quantities[-window_size:]) / window_size
        
        return [recent_avg] * periods
        
    def _exponential_smoothing_forecast(self, quantities: List[float], periods: int) -> List[float]:
        """Exponential smoothing forecast"""
        if len(quantities) < 2:
            return [quantities[0] if quantities else 0] * periods
            
        alpha = 0.3  # Smoothing parameter
        
        # Initialize
        smoothed = [quantities[0]]
        
        # Calculate smoothed values
        for i in range(1, len(quantities)):
            smoothed_value = alpha * quantities[i] + (1 - alpha) * smoothed[i-1]
            smoothed.append(smoothed_value)
            
        # Forecast future periods
        last_smoothed = smoothed[-1]
        return [last_smoothed] * periods
        
    def _linear_trend_forecast(self, quantities: List[float], periods: int) -> List[float]:
        """Linear trend forecast"""
        if len(quantities) < 3:
            avg = sum(quantities) / len(quantities)
            return [avg] * periods
            
        # Calculate linear trend using least squares
        n = len(quantities)
        x_values = list(range(n))
        
        # Calculate slope and intercept
        sum_x = sum(x_values)
        sum_y = sum(quantities)
        sum_xy = sum(x * y for x, y in zip(x_values, quantities))
        sum_x2 = sum(x * x for x in x_values)
        
        slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
        intercept = (sum_y - slope * sum_x) / n
        
        # Generate forecasts
        forecasts = []
        for i in range(periods):
            forecast_value = intercept + slope * (n + i)
            forecasts.append(max(0, forecast_value))  # Ensure non-negative
            
        return forecasts
        
    def _detect_seasonality(self, historical_data: List[Dict], period_type: str) -> Optional[List[float]]:
        """Detect seasonal patterns in historical data"""
        if len(historical_data) < 12:  # Need at least a year of data
            return None
            
        quantities = [data['quantity'] for data in historical_data]
        
        if period_type == 'MONTH' and len(quantities) >= 24:
            # Monthly seasonality - calculate factors for each month
            monthly_factors = {}
            overall_avg = sum(quantities) / len(quantities)
            
            for i, quantity in enumerate(quantities):
                month = (i % 12) + 1
                if month not in monthly_factors:
                    monthly_factors[month] = []
                monthly_factors[month].append(quantity / overall_avg if overall_avg > 0 else 1)
                
            # Calculate average factor for each month
            seasonal_factors = []
            for month in range(1, 13):
                if month in monthly_factors:
                    avg_factor = sum(monthly_factors[month]) / len(monthly_factors[month])
                    seasonal_factors.append(avg_factor)
                else:
                    seasonal_factors.append(1.0)
                    
            return seasonal_factors
            
        return None
        
    def _apply_seasonal_adjustment(self, forecasts: List[float], 
                                 seasonal_factors: List[float], period_type: str) -> List[float]:
        """Apply seasonal adjustments to forecasts"""
        adjusted_forecasts = []
        
        for i, forecast in enumerate(forecasts):
            if period_type == 'MONTH':
                factor_index = i % len(seasonal_factors)
                factor = seasonal_factors[factor_index]
                adjusted_forecasts.append(forecast * factor)
            else:
                adjusted_forecasts.append(forecast)
                
        return adjusted_forecasts
        
    def _calculate_period_date(self, period: int, period_type: str) -> int:
        """Calculate date for forecast period"""
        base_date = datetime.now()
        
        if period_type == 'WEEK':
            future_date = base_date + timedelta(weeks=period)
        elif period_type == 'MONTH':
            future_date = base_date + timedelta(days=period * 30)
        elif period_type == 'QUARTER':
            future_date = base_date + timedelta(days=period * 90)
        else:
            future_date = base_date + timedelta(days=period * 30)
            
        return int(future_date.strftime("%Y%m%d"))
        
    def _calculate_confidence_level(self, historical_data: List[Dict], model: str) -> Decimal:
        """Calculate confidence level for forecast"""
        if len(historical_data) < 3:
            return Decimal('50.0')
            
        quantities = [data['quantity'] for data in historical_data]
        
        # Calculate coefficient of variation
        mean_qty = statistics.mean(quantities)
        std_qty = statistics.stdev(quantities) if len(quantities) > 1 else 0
        
        if mean_qty > 0:
            cv = std_qty / mean_qty
            # Higher variation = lower confidence
            confidence = max(30, min(95, 90 - (cv * 100)))
        else:
            confidence = 50
            
        # Adjust based on model reliability
        model_adjustments = {
            'MOVING_AVG': 0,
            'EXP_SMOOTH': 5,
            'LINEAR_TREND': -5
        }
        
        confidence += model_adjustments.get(model, 0)
        return Decimal(str(max(30, min(95, confidence))))
        
    def _update_model_performance(self, stock_code: str, warehouse: str, 
                                 model: str, historical_data: List[Dict]):
        """Update model performance tracking"""
        existing = self.db.query(ForecastModelRec).filter(
            and_(
                ForecastModelRec.model_stock_code == stock_code,
                ForecastModelRec.model_warehouse == warehouse,
                ForecastModelRec.model_type == model
            )
        ).first()
        
        if existing:
            existing.model_last_used = int(datetime.now().strftime("%Y%m%d"))
            existing.model_usage_count += 1
        else:
            model_rec = ForecastModelRec(
                model_stock_code=stock_code,
                model_warehouse=warehouse,
                model_type=model,
                model_accuracy=Decimal('75.0'),  # Default
                model_last_used=int(datetime.now().strftime("%Y%m%d")),
                model_usage_count=1,
                model_created_by=self.current_user.username if self.current_user else 'SYSTEM'
            )
            self.db.add(model_rec)
            
    def _get_current_safety_stock(self, stock_code: str, warehouse: str) -> float:
        """Get current safety stock level"""
        stock, _ = self.stock_handler.process(4, key_value=stock_code)
        return float(getattr(stock, 'stock_safety_stock', 0)) if stock else 0
        
    def _get_stock_cost(self, stock_code: str) -> Decimal:
        """Get stock unit cost"""
        stock, _ = self.stock_handler.process(4, key_value=stock_code)
        return stock.stock_average_cost if stock else Decimal('1')
        
    def _calculate_forecast_accuracy(self, stock_code: str, warehouse: str) -> float:
        """Calculate recent forecast accuracy"""
        # This would compare recent forecasts to actual demand
        return 75.0  # Default accuracy percentage
        
    def _get_actual_demand_for_period(self, stock_code: str, warehouse: str, 
                                    period_date: int, period_type: str) -> Optional[float]:
        """Get actual demand for a specific period"""
        period_start = datetime.strptime(str(period_date), "%Y%m%d")
        
        if period_type == 'WEEK':
            period_end = period_start + timedelta(weeks=1)
        elif period_type == 'MONTH':
            period_end = period_start + timedelta(days=30)
        elif period_type == 'QUARTER':
            period_end = period_start + timedelta(days=90)
        else:
            period_end = period_start + timedelta(days=30)
            
        start_int = int(period_start.strftime("%Y%m%d"))
        end_int = int(period_end.strftime("%Y%m%d"))
        
        # Get actual sales/issues
        actual = self.db.query(
            func.sum(StockMovementRec.move_qty_out)
        ).filter(
            and_(
                StockMovementRec.move_stock_code == stock_code,
                StockMovementRec.move_warehouse == warehouse,
                StockMovementRec.move_date >= start_int,
                StockMovementRec.move_date < end_int,
                StockMovementRec.move_qty_out > 0
            )
        ).scalar()
        
        return float(actual) if actual else 0.0
        
    def _calculate_accuracy_grade(self, abs_pct_error: float) -> str:
        """Calculate accuracy grade based on absolute percentage error"""
        if abs_pct_error <= 10:
            return 'EXCELLENT'
        elif abs_pct_error <= 20:
            return 'GOOD'
        elif abs_pct_error <= 30:
            return 'FAIR'
        else:
            return 'POOR'
            
    def _calculate_overall_accuracy_stats(self, item_stats: Dict) -> Dict:
        """Calculate overall accuracy statistics"""
        if not item_stats:
            return {}
            
        total_items = len(item_stats)
        total_mape = sum(stats['mean_abs_pct_error'] for stats in item_stats.values())
        avg_mape = total_mape / total_items
        
        grade_counts = {}
        for stats in item_stats.values():
            grade = stats['accuracy_grade']
            grade_counts[grade] = grade_counts.get(grade, 0) + 1
            
        return {
            'total_items_analyzed': total_items,
            'overall_mape': avg_mape,
            'overall_grade': self._calculate_accuracy_grade(avg_mape),
            'grade_distribution': grade_counts
        }
        
    def _generate_forecast_recommendations(self, item_stats: Dict, model_performance: Dict) -> List[Dict]:
        """Generate forecasting recommendations"""
        recommendations = []
        
        # Poor accuracy items
        poor_items = [item for item, stats in item_stats.items() 
                     if stats['accuracy_grade'] == 'POOR']
        
        if poor_items:
            recommendations.append({
                'type': 'ACCURACY_IMPROVEMENT',
                'priority': 'HIGH',
                'description': f"{len(poor_items)} items have poor forecast accuracy",
                'action': 'Review forecasting models and consider manual adjustments'
            })
            
        # Model performance
        best_model = min(model_performance.items(), key=lambda x: x[1]['avg_abs_pct_error'])[0] if model_performance else None
        
        if best_model:
            recommendations.append({
                'type': 'MODEL_OPTIMIZATION',
                'priority': 'MEDIUM',
                'description': f"{best_model} shows best performance",
                'action': f'Consider using {best_model} as default model for new items'
            })
            
        return recommendations
        
    def _get_next_forecast_run_id(self) -> str:
        """Generate next forecast run ID"""
        return f"FCST{datetime.now().strftime('%Y%m%d%H%M%S')}"