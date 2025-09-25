"""
Bin Management Service - ST100 migration
Handles bin location setup and maintenance for WMS
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from datetime import datetime
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func, desc

from app.services.file_handlers.stock_handler import StockFileHandler
from app.services.file_handlers.system_handler import SystemFileHandler
from app.models.stock import StockLocationRec, StockBinRec
from app.models.warehouse import (
    WarehouseRec, WarehouseZoneRec, BinTypeRec, 
    BinLocationRec, BinCapacityRec
)
from app.core.security import log_user_action
from app.models.auth import User


class BinManagementService:
    """
    Bin Management functionality
    Implements ST100 - bin location setup and optimization
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.stock_handler = StockFileHandler(db)
        self.system_handler = SystemFileHandler(db)
        
    def create_bin_location(self, bin_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Create new bin location
        Returns (success, error_message)
        """
        warehouse = bin_data.get('warehouse')
        zone = bin_data.get('zone')
        aisle = bin_data.get('aisle')
        bay = bin_data.get('bay')
        level = bin_data.get('level')
        position = bin_data.get('position', '')
        bin_type = bin_data.get('bin_type', 'STANDARD')
        
        # Generate bin location code
        bin_location = self._generate_bin_code(warehouse, zone, aisle, bay, level, position)
        
        # Check if bin already exists
        existing = self.db.query(BinLocationRec).filter(
            and_(
                BinLocationRec.bin_warehouse == warehouse,
                BinLocationRec.bin_location == bin_location
            )
        ).first()
        
        if existing:
            return False, f"Bin location {bin_location} already exists"
            
        try:
            # Create bin location record
            bin_rec = BinLocationRec(
                bin_warehouse=warehouse,
                bin_location=bin_location,
                bin_zone=zone,
                bin_aisle=aisle,
                bin_bay=bay,
                bin_level=level,
                bin_position=position,
                bin_type=bin_type,
                bin_status='ACTIVE',
                bin_pickable=bin_data.get('pickable', 'Y'),
                bin_replenishable=bin_data.get('replenishable', 'Y'),
                bin_capacity_weight=Decimal(str(bin_data.get('capacity_weight', 1000))),
                bin_capacity_volume=Decimal(str(bin_data.get('capacity_volume', 10))),
                bin_capacity_units=bin_data.get('capacity_units', 100),
                bin_current_weight=Decimal('0'),
                bin_current_volume=Decimal('0'),
                bin_current_units=0,
                bin_temperature_zone=bin_data.get('temperature_zone', 'AMBIENT'),
                bin_hazmat_class=bin_data.get('hazmat_class', ''),
                bin_pick_sequence=bin_data.get('pick_sequence', 0),
                bin_putaway_sequence=bin_data.get('putaway_sequence', 0),
                bin_created_date=int(datetime.now().strftime("%Y%m%d")),
                bin_created_by=self.current_user.username if self.current_user else 'SYSTEM'
            )
            
            self.db.add(bin_rec)
            self.db.flush()
            
            # Create capacity tracking
            self._create_bin_capacity_tracking(bin_rec)
            
            self.db.commit()
            
            # Log creation
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="CREATE_BIN_LOCATION",
                    table="bin_location_rec",
                    key=bin_location,
                    new_values=bin_data,
                    module="STOCK"
                )
                
            return True, f"Bin location {bin_location} created successfully"
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def update_bin_location(self, bin_location: str, warehouse: str, updates: Dict) -> Tuple[bool, Optional[str]]:
        """
        Update bin location details
        Returns (success, error_message)
        """
        bin_rec = self.db.query(BinLocationRec).filter(
            and_(
                BinLocationRec.bin_warehouse == warehouse,
                BinLocationRec.bin_location == bin_location
            )
        ).first()
        
        if not bin_rec:
            return False, "Bin location not found"
            
        try:
            # Store old values for audit
            old_values = {
                'status': bin_rec.bin_status,
                'pickable': bin_rec.bin_pickable,
                'capacity_weight': float(bin_rec.bin_capacity_weight)
            }
            
            # Apply updates
            for field, value in updates.items():
                if hasattr(bin_rec, f"bin_{field}"):
                    if field in ['capacity_weight', 'capacity_volume']:
                        value = Decimal(str(value))
                    setattr(bin_rec, f"bin_{field}", value)
                    
            bin_rec.bin_last_updated = int(datetime.now().strftime("%Y%m%d"))
            bin_rec.bin_updated_by = self.current_user.username if self.current_user else 'SYSTEM'
            
            self.db.commit()
            
            # Log update
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="UPDATE_BIN_LOCATION",
                    table="bin_location_rec",
                    key=bin_location,
                    old_values=old_values,
                    new_values=updates,
                    module="STOCK"
                )
                
            return True, f"Bin location {bin_location} updated successfully"
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def optimize_bin_allocation(self, optimization_request: Dict) -> Dict:
        """
        Optimize bin allocation for stock items
        Returns optimization results
        """
        warehouse = optimization_request.get('warehouse', 'MAIN')
        optimization_type = optimization_request.get('type', 'VELOCITY')  # VELOCITY, SIZE, ABC
        zone = optimization_request.get('zone')
        
        try:
            # Get stock items to optimize
            stock_items = self._get_stock_items_for_optimization(warehouse, zone)
            
            # Get available bins
            available_bins = self._get_available_bins(warehouse, zone)
            
            if optimization_type == 'VELOCITY':
                recommendations = self._optimize_by_velocity(stock_items, available_bins)
            elif optimization_type == 'SIZE':
                recommendations = self._optimize_by_size(stock_items, available_bins)
            elif optimization_type == 'ABC':
                recommendations = self._optimize_by_abc_class(stock_items, available_bins)
            else:
                return {"error": f"Unknown optimization type: {optimization_type}"}
                
            return {
                'optimization_type': optimization_type,
                'warehouse': warehouse,
                'zone': zone,
                'total_items': len(stock_items),
                'recommendations': recommendations,
                'potential_savings': self._calculate_potential_savings(recommendations)
            }
            
        except Exception as e:
            return {"error": str(e)}
            
    def suggest_putaway_locations(self, putaway_request: Dict) -> List[Dict]:
        """
        Suggest optimal putaway locations for received goods
        Returns list of suggested locations
        """
        stock_code = putaway_request.get('stock_code')
        quantity = Decimal(str(putaway_request.get('quantity', 0)))
        warehouse = putaway_request.get('warehouse', 'MAIN')
        unit_dimensions = putaway_request.get('dimensions', {})
        
        # Get stock item details
        stock, _ = self.stock_handler.process(4, key_value=stock_code)
        if not stock:
            return [{"error": "Stock item not found"}]
            
        try:
            # Get putaway rules for item
            putaway_rules = self._get_putaway_rules(stock)
            
            # Get candidate bins
            candidate_bins = self._get_putaway_candidate_bins(warehouse, putaway_rules, quantity, stock)
            
            # Score and rank bins
            scored_bins = []
            for bin_rec in candidate_bins:
                score = self._calculate_putaway_score(bin_rec, stock, quantity, putaway_rules)
                
                scored_bins.append({
                    'bin_location': bin_rec.bin_location,
                    'zone': bin_rec.bin_zone,
                    'aisle': bin_rec.bin_aisle,
                    'bay': bin_rec.bin_bay,
                    'level': bin_rec.bin_level,
                    'score': score,
                    'available_capacity': {
                        'weight': float(bin_rec.bin_capacity_weight - bin_rec.bin_current_weight),
                        'volume': float(bin_rec.bin_capacity_volume - bin_rec.bin_current_volume),
                        'units': bin_rec.bin_capacity_units - bin_rec.bin_current_units
                    },
                    'distance_from_receiving': self._calculate_distance_from_receiving(bin_rec),
                    'pick_frequency': self._get_bin_pick_frequency(bin_rec),
                    'recommendation_reason': self._get_putaway_reason(score, putaway_rules)
                })
                
            # Sort by score (highest first) and return top suggestions
            scored_bins.sort(key=lambda x: x['score'], reverse=True)
            
            return scored_bins[:5]  # Top 5 suggestions
            
        except Exception as e:
            return [{"error": str(e)}]
            
    def analyze_bin_utilization(self, warehouse: str, analysis_period: int = 30) -> Dict:
        """
        Analyze bin utilization and efficiency
        Returns utilization analysis
        """
        try:
            # Get all bins in warehouse
            bins = self.db.query(BinLocationRec).filter(
                BinLocationRec.bin_warehouse == warehouse
            ).all()
            
            utilization_data = []
            total_bins = len(bins)
            empty_bins = 0
            full_bins = 0
            overutilized_bins = 0
            
            for bin_rec in bins:
                # Calculate utilization percentages
                weight_util = (float(bin_rec.bin_current_weight) / float(bin_rec.bin_capacity_weight) * 100) if bin_rec.bin_capacity_weight > 0 else 0
                volume_util = (float(bin_rec.bin_current_volume) / float(bin_rec.bin_capacity_volume) * 100) if bin_rec.bin_capacity_volume > 0 else 0
                unit_util = (bin_rec.bin_current_units / bin_rec.bin_capacity_units * 100) if bin_rec.bin_capacity_units > 0 else 0
                
                # Overall utilization (max of the three)
                overall_util = max(weight_util, volume_util, unit_util)
                
                # Categorize bin
                if overall_util == 0:
                    empty_bins += 1
                elif overall_util >= 95:
                    full_bins += 1
                elif overall_util > 100:
                    overutilized_bins += 1
                    
                # Get activity metrics
                pick_activity = self._get_bin_pick_activity(bin_rec, analysis_period)
                putaway_activity = self._get_bin_putaway_activity(bin_rec, analysis_period)
                
                utilization_data.append({
                    'bin_location': bin_rec.bin_location,
                    'zone': bin_rec.bin_zone,
                    'type': bin_rec.bin_type,
                    'status': bin_rec.bin_status,
                    'utilization': {
                        'weight_pct': weight_util,
                        'volume_pct': volume_util,
                        'units_pct': unit_util,
                        'overall_pct': overall_util
                    },
                    'capacity': {
                        'weight': float(bin_rec.bin_capacity_weight),
                        'volume': float(bin_rec.bin_capacity_volume),
                        'units': bin_rec.bin_capacity_units
                    },
                    'current': {
                        'weight': float(bin_rec.bin_current_weight),
                        'volume': float(bin_rec.bin_current_volume),
                        'units': bin_rec.bin_current_units
                    },
                    'activity': {
                        'picks': pick_activity,
                        'putaways': putaway_activity,
                        'total_transactions': pick_activity + putaway_activity
                    }
                })
                
            # Calculate zone summaries
            zone_summary = {}
            for bin_data in utilization_data:
                zone = bin_data['zone']
                if zone not in zone_summary:
                    zone_summary[zone] = {
                        'total_bins': 0,
                        'avg_utilization': 0,
                        'total_activity': 0
                    }
                    
                zone_summary[zone]['total_bins'] += 1
                zone_summary[zone]['avg_utilization'] += bin_data['utilization']['overall_pct']
                zone_summary[zone]['total_activity'] += bin_data['activity']['total_transactions']
                
            # Calculate averages
            for zone_data in zone_summary.values():
                if zone_data['total_bins'] > 0:
                    zone_data['avg_utilization'] /= zone_data['total_bins']
                    
            return {
                'warehouse': warehouse,
                'analysis_period_days': analysis_period,
                'summary': {
                    'total_bins': total_bins,
                    'empty_bins': empty_bins,
                    'full_bins': full_bins,
                    'overutilized_bins': overutilized_bins,
                    'average_utilization': sum(b['utilization']['overall_pct'] for b in utilization_data) / total_bins if total_bins > 0 else 0
                },
                'zone_summary': zone_summary,
                'bin_details': utilization_data,
                'recommendations': self._generate_bin_recommendations(utilization_data)
            }
            
        except Exception as e:
            return {"error": str(e)}
            
    def get_bin_layout(self, warehouse: str, zone: Optional[str] = None) -> Dict:
        """
        Get bin layout visualization data
        Returns layout structure
        """
        try:
            query = self.db.query(BinLocationRec).filter(
                BinLocationRec.bin_warehouse == warehouse
            )
            
            if zone:
                query = query.filter(BinLocationRec.bin_zone == zone)
                
            bins = query.order_by(
                BinLocationRec.bin_zone,
                BinLocationRec.bin_aisle,
                BinLocationRec.bin_bay,
                BinLocationRec.bin_level
            ).all()
            
            # Organize by zone -> aisle -> bay -> level
            layout = {}
            
            for bin_rec in bins:
                zone_key = bin_rec.bin_zone
                if zone_key not in layout:
                    layout[zone_key] = {}
                    
                aisle_key = bin_rec.bin_aisle
                if aisle_key not in layout[zone_key]:
                    layout[zone_key][aisle_key] = {}
                    
                bay_key = bin_rec.bin_bay
                if bay_key not in layout[zone_key][aisle_key]:
                    layout[zone_key][aisle_key][bay_key] = {}
                    
                level_key = bin_rec.bin_level
                layout[zone_key][aisle_key][bay_key][level_key] = {
                    'bin_location': bin_rec.bin_location,
                    'status': bin_rec.bin_status,
                    'type': bin_rec.bin_type,
                    'pickable': bin_rec.bin_pickable == 'Y',
                    'utilization': self._calculate_bin_utilization(bin_rec),
                    'contents': self._get_bin_contents(bin_rec)
                }
                
            return {
                'warehouse': warehouse,
                'zone_filter': zone,
                'layout': layout,
                'statistics': {
                    'total_bins': len(bins),
                    'zones': len(layout),
                    'aisles': sum(len(zone_data) for zone_data in layout.values()),
                    'active_bins': len([b for b in bins if b.bin_status == 'ACTIVE'])
                }
            }
            
        except Exception as e:
            return {"error": str(e)}
            
    def _generate_bin_code(self, warehouse: str, zone: str, aisle: str, bay: str, level: str, position: str) -> str:
        """Generate standardized bin location code"""
        if position:
            return f"{zone}-{aisle:0>2}-{bay:0>2}-{level:0>2}-{position}"
        else:
            return f"{zone}-{aisle:0>2}-{bay:0>2}-{level:0>2}"
            
    def _create_bin_capacity_tracking(self, bin_rec: BinLocationRec):
        """Create capacity tracking record for bin"""
        capacity = BinCapacityRec(
            cap_bin_location=bin_rec.bin_location,
            cap_warehouse=bin_rec.bin_warehouse,
            cap_date=int(datetime.now().strftime("%Y%m%d")),
            cap_max_weight=bin_rec.bin_capacity_weight,
            cap_max_volume=bin_rec.bin_capacity_volume,
            cap_max_units=bin_rec.bin_capacity_units,
            cap_current_weight=Decimal('0'),
            cap_current_volume=Decimal('0'),
            cap_current_units=0
        )
        
        self.db.add(capacity)
        
    def _get_stock_items_for_optimization(self, warehouse: str, zone: Optional[str]) -> List[Dict]:
        """Get stock items that need bin optimization"""
        # This would analyze current stock locations and identify optimization opportunities
        return []
        
    def _get_available_bins(self, warehouse: str, zone: Optional[str]) -> List[BinLocationRec]:
        """Get available bins for optimization"""
        query = self.db.query(BinLocationRec).filter(
            and_(
                BinLocationRec.bin_warehouse == warehouse,
                BinLocationRec.bin_status == 'ACTIVE'
            )
        )
        
        if zone:
            query = query.filter(BinLocationRec.bin_zone == zone)
            
        return query.all()
        
    def _optimize_by_velocity(self, stock_items: List[Dict], bins: List[BinLocationRec]) -> List[Dict]:
        """Optimize bin allocation by pick velocity"""
        # High-velocity items should be in easily accessible locations
        return []
        
    def _optimize_by_size(self, stock_items: List[Dict], bins: List[BinLocationRec]) -> List[Dict]:
        """Optimize bin allocation by item size"""
        # Match item sizes to appropriate bin capacities
        return []
        
    def _optimize_by_abc_class(self, stock_items: List[Dict], bins: List[BinLocationRec]) -> List[Dict]:
        """Optimize bin allocation by ABC classification"""
        # A-class items in prime locations, C-class in remote locations
        return []
        
    def _calculate_potential_savings(self, recommendations: List[Dict]) -> Dict:
        """Calculate potential savings from optimization"""
        return {
            'pick_time_reduction': 0,  # minutes per day
            'travel_distance_reduction': 0,  # meters per day
            'space_utilization_improvement': 0  # percentage points
        }
        
    def _get_putaway_rules(self, stock) -> Dict:
        """Get putaway rules for stock item"""
        return {
            'preferred_zone': getattr(stock, 'stock_putaway_zone', 'GENERAL'),
            'temperature_requirements': getattr(stock, 'stock_temperature_zone', 'AMBIENT'),
            'hazmat_class': getattr(stock, 'stock_hazmat_class', ''),
            'size_category': self._determine_size_category(stock),
            'velocity_class': getattr(stock, 'stock_abc_class', 'C')
        }
        
    def _get_putaway_candidate_bins(self, warehouse: str, rules: Dict, quantity: Decimal, stock) -> List[BinLocationRec]:
        """Get candidate bins for putaway"""
        query = self.db.query(BinLocationRec).filter(
            and_(
                BinLocationRec.bin_warehouse == warehouse,
                BinLocationRec.bin_status == 'ACTIVE',
                BinLocationRec.bin_replenishable == 'Y'
            )
        )
        
        # Apply rules
        if rules.get('preferred_zone'):
            query = query.filter(BinLocationRec.bin_zone == rules['preferred_zone'])
        if rules.get('temperature_requirements'):
            query = query.filter(BinLocationRec.bin_temperature_zone == rules['temperature_requirements'])
            
        # Filter by available capacity
        item_weight = stock.stock_weight * quantity if stock else Decimal('1') * quantity
        item_volume = stock.stock_volume * quantity if stock else Decimal('0.1') * quantity
        
        candidates = []
        for bin_rec in query.all():
            if (bin_rec.bin_current_weight + item_weight <= bin_rec.bin_capacity_weight and
                bin_rec.bin_current_volume + item_volume <= bin_rec.bin_capacity_volume and
                bin_rec.bin_current_units + int(quantity) <= bin_rec.bin_capacity_units):
                candidates.append(bin_rec)
                
        return candidates
        
    def _calculate_putaway_score(self, bin_rec: BinLocationRec, stock, quantity: Decimal, rules: Dict) -> float:
        """Calculate putaway score for bin"""
        score = 0.0
        
        # Distance from receiving (closer is better)
        distance_score = max(0, 100 - self._calculate_distance_from_receiving(bin_rec))
        score += distance_score * 0.3
        
        # Capacity utilization (better utilization is better)
        utilization = self._calculate_bin_utilization(bin_rec)
        utilization_score = min(100, utilization + 20)  # Prefer bins with some existing stock
        score += utilization_score * 0.2
        
        # Zone preference match
        if rules.get('preferred_zone') == bin_rec.bin_zone:
            score += 30
            
        # Pick accessibility for high-velocity items
        if rules.get('velocity_class') == 'A' and bin_rec.bin_pickable == 'Y':
            score += 20
            
        return score
        
    def _calculate_distance_from_receiving(self, bin_rec: BinLocationRec) -> float:
        """Calculate distance from receiving area (simplified)"""
        # This would use actual warehouse layout coordinates
        # For now, use zone-based approximation
        zone_distances = {
            'RECEIVE': 0,
            'FAST': 10,
            'GENERAL': 30,
            'BULK': 50,
            'OVERFLOW': 80
        }
        return zone_distances.get(bin_rec.bin_zone, 50)
        
    def _get_bin_pick_frequency(self, bin_rec: BinLocationRec) -> int:
        """Get pick frequency for bin (last 30 days)"""
        # This would query actual pick transaction history
        return 0
        
    def _get_putaway_reason(self, score: float, rules: Dict) -> str:
        """Get human-readable reason for putaway recommendation"""
        if score >= 80:
            return "Optimal location - close to receiving, good capacity match"
        elif score >= 60:
            return "Good location - meets most criteria"
        elif score >= 40:
            return "Acceptable location - some compromises"
        else:
            return "Suboptimal location - consider alternatives"
            
    def _get_bin_pick_activity(self, bin_rec: BinLocationRec, days: int) -> int:
        """Get bin pick activity count"""
        # This would query pick transaction history
        return 0
        
    def _get_bin_putaway_activity(self, bin_rec: BinLocationRec, days: int) -> int:
        """Get bin putaway activity count"""
        # This would query putaway transaction history
        return 0
        
    def _calculate_bin_utilization(self, bin_rec: BinLocationRec) -> float:
        """Calculate overall bin utilization percentage"""
        weight_util = (float(bin_rec.bin_current_weight) / float(bin_rec.bin_capacity_weight) * 100) if bin_rec.bin_capacity_weight > 0 else 0
        volume_util = (float(bin_rec.bin_current_volume) / float(bin_rec.bin_capacity_volume) * 100) if bin_rec.bin_capacity_volume > 0 else 0
        unit_util = (bin_rec.bin_current_units / bin_rec.bin_capacity_units * 100) if bin_rec.bin_capacity_units > 0 else 0
        
        return max(weight_util, volume_util, unit_util)
        
    def _get_bin_contents(self, bin_rec: BinLocationRec) -> List[Dict]:
        """Get current contents of bin"""
        # This would query stock location records
        stock_locations = self.db.query(StockLocationRec).filter(
            and_(
                StockLocationRec.loc_warehouse == bin_rec.bin_warehouse,
                StockLocationRec.loc_location == bin_rec.bin_location,
                StockLocationRec.loc_qty_on_hand > 0
            )
        ).all()
        
        return [
            {
                'stock_code': loc.loc_stock_code,
                'quantity': float(loc.loc_qty_on_hand),
                'allocated': float(loc.loc_qty_allocated)
            }
            for loc in stock_locations
        ]
        
    def _generate_bin_recommendations(self, utilization_data: List[Dict]) -> List[Dict]:
        """Generate bin management recommendations"""
        recommendations = []
        
        # Find empty bins
        empty_bins = [b for b in utilization_data if b['utilization']['overall_pct'] == 0]
        if len(empty_bins) > 10:
            recommendations.append({
                'type': 'CONSOLIDATION',
                'priority': 'MEDIUM',
                'description': f"Consider consolidating stock from {len(empty_bins)} empty bins to improve utilization"
            })
            
        # Find overutilized bins
        overutilized = [b for b in utilization_data if b['utilization']['overall_pct'] > 100]
        if overutilized:
            recommendations.append({
                'type': 'CAPACITY_EXPANSION',
                'priority': 'HIGH',
                'description': f"{len(overutilized)} bins are over capacity - immediate attention required"
            })
            
        # Find low-activity bins in prime locations
        prime_zones = ['FAST', 'PICK']
        low_activity_prime = [b for b in utilization_data 
                             if b['zone'] in prime_zones and b['activity']['total_transactions'] < 5]
        if low_activity_prime:
            recommendations.append({
                'type': 'RELOCATION',
                'priority': 'MEDIUM',
                'description': f"Consider relocating low-activity items from {len(low_activity_prime)} prime location bins"
            })
            
        return recommendations
        
    def _determine_size_category(self, stock) -> str:
        """Determine size category for stock item"""
        if hasattr(stock, 'stock_volume') and stock.stock_volume:
            volume = float(stock.stock_volume)
            if volume < 0.001:
                return 'SMALL'
            elif volume < 0.01:
                return 'MEDIUM'
            else:
                return 'LARGE'
        return 'MEDIUM'