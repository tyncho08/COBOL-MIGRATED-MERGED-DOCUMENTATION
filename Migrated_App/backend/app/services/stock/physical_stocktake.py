"""
Physical Stocktake Service - ST130 migration
Handles full physical stocktake processing and reconciliation
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from datetime import datetime, timedelta
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func, desc, asc

from app.services.file_handlers.stock_handler import StockFileHandler
from app.services.file_handlers.system_handler import SystemFileHandler
from app.models.stock import (
    StockMasterRec, StockLocationRec, StockMovementRec,
    PhysicalStocktakeRec, StocktakeLineRec, StocktakeVarianceRec,
    StocktakeTagRec
)
from app.models.warehouse import BinLocationRec
from app.services.stock.stock_movements import StockMovementsService
from app.core.security import log_user_action
from app.models.auth import User


class PhysicalStocktakeService:
    """
    Physical Stocktake functionality
    Implements ST130 - full physical stocktake processing
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.stock_handler = StockFileHandler(db)
        self.system_handler = SystemFileHandler(db)
        
    def create_stocktake(self, stocktake_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Create new physical stocktake
        Returns (success, error_message or stocktake_data)
        """
        warehouse = stocktake_data.get('warehouse', 'MAIN')
        stocktake_type = stocktake_data.get('type', 'FULL')  # FULL, PARTIAL, ZONE
        scope = stocktake_data.get('scope', {})
        freeze_movements = stocktake_data.get('freeze_movements', True)
        count_date = stocktake_data.get('count_date', int(datetime.now().strftime("%Y%m%d")))
        
        try:
            # Create stocktake header
            stocktake_no = self._get_next_stocktake_number()
            
            stocktake = PhysicalStocktakeRec(
                stocktake_no=stocktake_no,
                stocktake_warehouse=warehouse,
                stocktake_type=stocktake_type,
                stocktake_status='CREATED',
                stocktake_count_date=count_date,
                stocktake_created_date=int(datetime.now().strftime("%Y%m%d")),
                stocktake_created_time=int(datetime.now().strftime("%H%M%S")),
                stocktake_created_by=self.current_user.username if self.current_user else 'SYSTEM',
                stocktake_freeze_movements='Y' if freeze_movements else 'N',
                stocktake_scope=str(scope),
                stocktake_total_lines=0,
                stocktake_counted_lines=0,
                stocktake_variance_lines=0
            )
            
            self.db.add(stocktake)
            self.db.flush()
            
            # Generate stocktake lines
            lines_created = self._generate_stocktake_lines(stocktake, scope)
            
            if lines_created == 0:
                self.db.rollback()
                return False, "No stock items found matching the specified criteria"
                
            stocktake.stocktake_total_lines = lines_created
            
            # Freeze movements if requested
            if freeze_movements:
                self._freeze_stock_movements(warehouse, stocktake_no)
                
            self.db.commit()
            
            # Log creation
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="CREATE_PHYSICAL_STOCKTAKE",
                    table="physical_stocktake_rec",
                    key=stocktake_no,
                    new_values={
                        'warehouse': warehouse,
                        'type': stocktake_type,
                        'lines': lines_created
                    },
                    module="STOCK"
                )
                
            return True, {
                'stocktake_no': stocktake_no,
                'warehouse': warehouse,
                'type': stocktake_type,
                'total_lines': lines_created,
                'count_date': count_date
            }
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def generate_count_tags(self, tag_request: Dict) -> Tuple[bool, Optional[str]]:
        """
        Generate count tags for stocktake
        Returns (success, error_message or tag_data)
        """
        stocktake_no = tag_request.get('stocktake_no')
        tag_type = tag_request.get('tag_type', 'BARCODE')  # BARCODE, RFID, PAPER
        sort_by = tag_request.get('sort_by', 'LOCATION')  # LOCATION, STOCK_CODE, ABC
        
        stocktake = self.db.query(PhysicalStocktakeRec).filter(
            PhysicalStocktakeRec.stocktake_no == stocktake_no
        ).first()
        
        if not stocktake:
            return False, "Stocktake not found"
            
        if stocktake.stocktake_status != 'CREATED':
            return False, f"Cannot generate tags for stocktake with status: {stocktake.stocktake_status}"
            
        try:
            # Get stocktake lines
            query = self.db.query(StocktakeLineRec).filter(
                StocktakeLineRec.line_stocktake_no == stocktake_no
            )
            
            # Sort according to request
            if sort_by == 'LOCATION':
                query = query.order_by(
                    StocktakeLineRec.line_location,
                    StocktakeLineRec.line_bin
                )
            elif sort_by == 'STOCK_CODE':
                query = query.order_by(StocktakeLineRec.line_stock_code)
            elif sort_by == 'ABC':
                query = query.order_by(
                    StocktakeLineRec.line_abc_class,
                    StocktakeLineRec.line_location
                )
                
            stocktake_lines = query.all()
            
            # Generate tags
            tags_generated = 0
            
            for line in stocktake_lines:
                tag_no = self._get_next_tag_number(stocktake_no)
                
                tag = StocktakeTagRec(
                    tag_no=tag_no,
                    tag_stocktake_no=stocktake_no,
                    tag_line_seq=line.line_seq,
                    tag_stock_code=line.line_stock_code,
                    tag_location=line.line_location,
                    tag_bin=line.line_bin,
                    tag_type=tag_type,
                    tag_status='GENERATED',
                    tag_system_qty=line.line_system_qty,
                    tag_generated_date=int(datetime.now().strftime("%Y%m%d")),
                    tag_generated_time=int(datetime.now().strftime("%H%M%S"))
                )
                
                self.db.add(tag)
                tags_generated += 1
                
            # Update stocktake status
            stocktake.stocktake_status = 'TAGS_GENERATED'
            stocktake.stocktake_tags_generated = tags_generated
            
            self.db.commit()
            
            return True, {
                'stocktake_no': stocktake_no,
                'tags_generated': tags_generated,
                'tag_type': tag_type,
                'sort_by': sort_by
            }
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def record_count(self, count_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Record physical count result
        Returns (success, error_message)
        """
        stocktake_no = count_data.get('stocktake_no')
        tag_no = count_data.get('tag_no', '')
        line_seq = count_data.get('line_seq')
        counted_qty = Decimal(str(count_data.get('counted_qty', 0)))
        counter = count_data.get('counter', '')
        count_method = count_data.get('count_method', 'PHYSICAL')
        notes = count_data.get('notes', '')
        
        # Get stocktake line
        if tag_no:
            # Find line by tag
            tag = self.db.query(StocktakeTagRec).filter(
                StocktakeTagRec.tag_no == tag_no
            ).first()
            
            if not tag:
                return False, "Count tag not found"
                
            line_seq = tag.tag_line_seq
            
        line = self.db.query(StocktakeLineRec).filter(
            and_(
                StocktakeLineRec.line_stocktake_no == stocktake_no,
                StocktakeLineRec.line_seq == line_seq
            )
        ).first()
        
        if not line:
            return False, "Stocktake line not found"
            
        if line.line_status in ['COUNTED', 'COMPLETE']:
            return False, f"Line already counted with status: {line.line_status}"
            
        try:
            # Calculate variance
            system_qty = line.line_system_qty
            variance_qty = counted_qty - system_qty
            variance_pct = (float(variance_qty) / float(system_qty) * 100) if system_qty != 0 else (100 if counted_qty > 0 else 0)
            
            # Update stocktake line
            line.line_counted_qty = counted_qty
            line.line_variance_qty = variance_qty
            line.line_variance_pct = Decimal(str(variance_pct))
            line.line_counter = counter
            line.line_count_method = count_method
            line.line_notes = notes
            line.line_counted_date = int(datetime.now().strftime("%Y%m%d"))
            line.line_counted_time = int(datetime.now().strftime("%H%M%S"))
            line.line_status = 'COUNTED'
            
            # Update tag if used
            if tag_no:
                tag = self.db.query(StocktakeTagRec).filter(
                    StocktakeTagRec.tag_no == tag_no
                ).first()
                if tag:
                    tag.tag_status = 'COUNTED'
                    tag.tag_counted_qty = counted_qty
                    tag.tag_counted_date = int(datetime.now().strftime("%Y%m%d"))
                    tag.tag_counted_time = int(datetime.now().strftime("%H%M%S"))
                    tag.tag_counter = counter
                    
            # Check if variance is significant
            system_rec, _ = self.system_handler.read_system_params()
            variance_tolerance = float(system_rec.stocktake_tolerance) if system_rec else 5.0
            
            significant_variance = abs(variance_pct) > variance_tolerance
            
            if significant_variance and variance_qty != 0:
                # Create variance record
                variance_record = StocktakeVarianceRec(
                    var_stocktake_no=stocktake_no,
                    var_line_seq=line_seq,
                    var_stock_code=line.line_stock_code,
                    var_location=line.line_location,
                    var_system_qty=system_qty,
                    var_counted_qty=counted_qty,
                    var_variance_qty=variance_qty,
                    var_variance_pct=Decimal(str(variance_pct)),
                    var_status='PENDING',
                    var_reason='',
                    var_created_date=int(datetime.now().strftime("%Y%m%d")),
                    var_counter=counter
                )
                
                self.db.add(variance_record)
                line.line_requires_approval = 'Y'
                
            # Update stocktake header counts
            stocktake = self.db.query(PhysicalStocktakeRec).filter(
                PhysicalStocktakeRec.stocktake_no == stocktake_no
            ).first()
            
            if stocktake:
                stocktake.stocktake_counted_lines += 1
                if significant_variance and variance_qty != 0:
                    stocktake.stocktake_variance_lines += 1
                    
            self.db.commit()
            
            # Log count
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="RECORD_STOCKTAKE_COUNT",
                    table="stocktake_line_rec",
                    key=f"{stocktake_no}-{line_seq}",
                    new_values={
                        'counted_qty': float(counted_qty),
                        'variance_qty': float(variance_qty),
                        'counter': counter
                    },
                    module="STOCK"
                )
                
            return True, {
                'variance_detected': variance_qty != 0,
                'variance_qty': float(variance_qty),
                'variance_pct': variance_pct,
                'requires_approval': significant_variance and variance_qty != 0,
                'tolerance': variance_tolerance
            }
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def approve_variances(self, approval_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Approve stocktake variances and create adjustments
        Returns (success, error_message)
        """
        stocktake_no = approval_data.get('stocktake_no')
        variance_approvals = approval_data.get('variances', [])
        
        stocktake = self.db.query(PhysicalStocktakeRec).filter(
            PhysicalStocktakeRec.stocktake_no == stocktake_no
        ).first()
        
        if not stocktake:
            return False, "Stocktake not found"
            
        try:
            approved_count = 0
            rejected_count = 0
            
            for approval in variance_approvals:
                line_seq = approval.get('line_seq')
                approved = approval.get('approved', True)
                reason = approval.get('reason', '')
                
                # Get variance record
                variance = self.db.query(StocktakeVarianceRec).filter(
                    and_(
                        StocktakeVarianceRec.var_stocktake_no == stocktake_no,
                        StocktakeVarianceRec.var_line_seq == line_seq,
                        StocktakeVarianceRec.var_status == 'PENDING'
                    )
                ).first()
                
                if not variance:
                    continue
                    
                if approved:
                    # Create stock adjustment
                    adjustment_data = {
                        'stock_code': variance.var_stock_code,
                        'warehouse': stocktake.stocktake_warehouse,
                        'location': variance.var_location,
                        'adjustment_qty': float(variance.var_variance_qty),
                        'reason': 'STOCKTAKE_ADJUSTMENT',
                        'reference': f"STKADJ-{stocktake_no}-{line_seq}"
                    }
                    
                    from app.services.stock.stock_movements import StockMovementsService
                    movements_service = StockMovementsService(self.db, self.current_user)
                    success, error = movements_service.process_adjustment(adjustment_data)
                    
                    if success:
                        variance.var_status = 'APPROVED'
                        variance.var_approved_by = self.current_user.username if self.current_user else 'SYSTEM'
                        variance.var_approved_date = int(datetime.now().strftime("%Y%m%d"))
                        variance.var_reason = reason
                        approved_count += 1
                        
                        # Update stocktake line
                        line = self.db.query(StocktakeLineRec).filter(
                            and_(
                                StocktakeLineRec.line_stocktake_no == stocktake_no,
                                StocktakeLineRec.line_seq == line_seq
                            )
                        ).first()
                        
                        if line:
                            line.line_status = 'COMPLETE'
                            line.line_requires_approval = 'N'
                            
                else:
                    # Reject variance
                    variance.var_status = 'REJECTED'
                    variance.var_rejected_by = self.current_user.username if self.current_user else 'SYSTEM'
                    variance.var_rejected_date = int(datetime.now().strftime("%Y%m%d"))
                    variance.var_reason = reason
                    rejected_count += 1
                    
                    # Update line for recount
                    line = self.db.query(StocktakeLineRec).filter(
                        and_(
                            StocktakeLineRec.line_stocktake_no == stocktake_no,
                            StocktakeLineRec.line_seq == line_seq
                        )
                    ).first()
                    
                    if line:
                        line.line_status = 'RECOUNT_REQUIRED'
                        
            self.db.commit()
            
            # Log approvals
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="APPROVE_STOCKTAKE_VARIANCES",
                    table="stocktake_variance_rec",
                    key=stocktake_no,
                    new_values={
                        'approved': approved_count,
                        'rejected': rejected_count
                    },
                    module="STOCK"
                )
                
            return True, f"Approved {approved_count} variances, rejected {rejected_count}"
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def finalize_stocktake(self, stocktake_no: str) -> Tuple[bool, Optional[str]]:
        """
        Finalize stocktake and update system balances
        Returns (success, error_message)
        """
        stocktake = self.db.query(PhysicalStocktakeRec).filter(
            PhysicalStocktakeRec.stocktake_no == stocktake_no
        ).first()
        
        if not stocktake:
            return False, "Stocktake not found"
            
        if stocktake.stocktake_status in ['FINALIZED', 'COMPLETE']:
            return False, f"Stocktake already finalized"
            
        # Check for pending items
        pending_lines = self.db.query(StocktakeLineRec).filter(
            and_(
                StocktakeLineRec.line_stocktake_no == stocktake_no,
                StocktakeLineRec.line_status.in_(['PENDING', 'RECOUNT_REQUIRED'])
            )
        ).count()
        
        pending_variances = self.db.query(StocktakeVarianceRec).filter(
            and_(
                StocktakeVarianceRec.var_stocktake_no == stocktake_no,
                StocktakeVarianceRec.var_status == 'PENDING'
            )
        ).count()
        
        if pending_lines > 0 or pending_variances > 0:
            return False, f"Cannot finalize: {pending_lines} lines pending count, {pending_variances} variances pending approval"
            
        try:
            # Update stocktake status
            stocktake.stocktake_status = 'FINALIZED'
            stocktake.stocktake_finalized_date = int(datetime.now().strftime("%Y%m%d"))
            stocktake.stocktake_finalized_time = int(datetime.now().strftime("%H%M%S"))
            stocktake.stocktake_finalized_by = self.current_user.username if self.current_user else 'SYSTEM'
            
            # Update stock master last counted dates
            self._update_last_counted_dates(stocktake_no)
            
            # Unfreeze movements
            if stocktake.stocktake_freeze_movements == 'Y':
                self._unfreeze_stock_movements(stocktake.stocktake_warehouse, stocktake_no)
                
            self.db.commit()
            
            # Generate completion report
            completion_report = self._generate_completion_report(stocktake)
            
            # Log finalization
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="FINALIZE_STOCKTAKE",
                    table="physical_stocktake_rec",
                    key=stocktake_no,
                    new_values=completion_report,
                    module="STOCK"
                )
                
            return True, completion_report
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def get_stocktake_progress(self, stocktake_no: str) -> Dict:
        """Get stocktake progress information"""
        try:
            # Get stocktake header
            stocktake = self.db.query(PhysicalStocktakeRec).filter(
                PhysicalStocktakeRec.stocktake_no == stocktake_no
            ).first()
            
            if not stocktake:
                return {"error": "Stocktake not found"}
                
            # Get line status summary
            line_status = self.db.query(
                StocktakeLineRec.line_status,
                func.count(StocktakeLineRec.line_seq).label('count')
            ).filter(
                StocktakeLineRec.line_stocktake_no == stocktake_no
            ).group_by(StocktakeLineRec.line_status).all()
            
            status_summary = {status: count for status, count in line_status}
            
            # Get variance summary
            variance_status = self.db.query(
                StocktakeVarianceRec.var_status,
                func.count(StocktakeVarianceRec.var_line_seq).label('count')
            ).filter(
                StocktakeVarianceRec.var_stocktake_no == stocktake_no
            ).group_by(StocktakeVarianceRec.var_status).all()
            
            variance_summary = {status: count for status, count in variance_status}
            
            # Calculate progress
            total_lines = stocktake.stocktake_total_lines
            counted_lines = status_summary.get('COUNTED', 0) + status_summary.get('COMPLETE', 0)
            progress_pct = (counted_lines / total_lines * 100) if total_lines > 0 else 0
            
            return {
                'stocktake_no': stocktake.stocktake_no,
                'warehouse': stocktake.stocktake_warehouse,
                'type': stocktake.stocktake_type,
                'status': stocktake.stocktake_status,
                'count_date': stocktake.stocktake_count_date,
                'progress': {
                    'total_lines': total_lines,
                    'counted_lines': counted_lines,
                    'pending_lines': status_summary.get('PENDING', 0),
                    'recount_required': status_summary.get('RECOUNT_REQUIRED', 0),
                    'progress_pct': progress_pct
                },
                'line_status': status_summary,
                'variance_status': variance_summary,
                'created_by': stocktake.stocktake_created_by,
                'created_date': stocktake.stocktake_created_date,
                'finalized_date': stocktake.stocktake_finalized_date
            }
            
        except Exception as e:
            return {"error": str(e)}
            
    def get_stocktake_variances(self, stocktake_no: str, filters: Optional[Dict] = None) -> List[Dict]:
        """Get stocktake variances with optional filters"""
        filters = filters or {}
        
        query = self.db.query(StocktakeVarianceRec).filter(
            StocktakeVarianceRec.var_stocktake_no == stocktake_no
        )
        
        # Apply filters
        if filters.get('status'):
            query = query.filter(StocktakeVarianceRec.var_status == filters['status'])
        if filters.get('min_variance_pct'):
            query = query.filter(abs(StocktakeVarianceRec.var_variance_pct) >= filters['min_variance_pct'])
        if filters.get('location'):
            query = query.filter(StocktakeVarianceRec.var_location == filters['location'])
            
        variances = query.order_by(desc(abs(StocktakeVarianceRec.var_variance_pct))).all()
        
        variance_list = []
        for variance in variances:
            # Get stock details
            stock, _ = self.stock_handler.process(4, key_value=variance.var_stock_code)
            
            variance_list.append({
                'line_seq': variance.var_line_seq,
                'stock_code': variance.var_stock_code,
                'description': stock.stock_desc if stock else '',
                'location': variance.var_location,
                'system_qty': float(variance.var_system_qty),
                'counted_qty': float(variance.var_counted_qty),
                'variance_qty': float(variance.var_variance_qty),
                'variance_pct': float(variance.var_variance_pct),
                'variance_value': float(variance.var_variance_qty * stock.stock_average_cost) if stock else 0,
                'status': variance.var_status,
                'counter': variance.var_counter,
                'reason': variance.var_reason,
                'created_date': variance.var_created_date
            })
            
        return variance_list
        
    def _generate_stocktake_lines(self, stocktake: PhysicalStocktakeRec, scope: Dict) -> int:
        """Generate stocktake lines based on scope criteria"""
        warehouse = stocktake.stocktake_warehouse
        
        # Build stock location query
        query = self.db.query(StockLocationRec).filter(
            and_(
                StockLocationRec.loc_warehouse == warehouse,
                StockLocationRec.loc_active == 'Y'
            )
        )
        
        # Apply scope filters
        if scope.get('zone'):
            query = query.filter(StockLocationRec.loc_zone == scope['zone'])
        if scope.get('location_pattern'):
            query = query.filter(StockLocationRec.loc_location.like(f"{scope['location_pattern']}%"))
        if scope.get('abc_class'):
            query = query.join(StockMasterRec).filter(
                StockMasterRec.stock_abc_class == scope['abc_class']
            )
        if scope.get('min_value'):
            query = query.join(StockMasterRec).filter(
                StockLocationRec.loc_qty_on_hand * StockMasterRec.stock_average_cost >= scope['min_value']
            )
        if scope.get('exclude_zero_qty', True):
            query = query.filter(StockLocationRec.loc_qty_on_hand > 0)
            
        stock_locations = query.all()
        
        lines_created = 0
        
        for location in stock_locations:
            # Get stock details
            stock, _ = self.stock_handler.process(4, key_value=location.loc_stock_code)
            if not stock:
                continue
                
            line = StocktakeLineRec(
                line_stocktake_no=stocktake.stocktake_no,
                line_seq=lines_created + 1,
                line_stock_code=location.loc_stock_code,
                line_description=stock.stock_desc,
                line_location=location.loc_location,
                line_bin=location.loc_bin,
                line_system_qty=location.loc_qty_on_hand,
                line_counted_qty=Decimal('0'),
                line_variance_qty=Decimal('0'),
                line_variance_pct=Decimal('0'),
                line_unit=stock.stock_unit,
                line_unit_cost=stock.stock_average_cost,
                line_abc_class=stock.stock_abc_class,
                line_status='PENDING',
                line_requires_approval='N',
                line_freeze_date=int(datetime.now().strftime("%Y%m%d"))
            )
            
            self.db.add(line)
            lines_created += 1
            
        return lines_created
        
    def _freeze_stock_movements(self, warehouse: str, stocktake_no: str):
        """Freeze stock movements for stocktake"""
        # This would implement movement freezing logic
        # For now, just log the action
        pass
        
    def _unfreeze_stock_movements(self, warehouse: str, stocktake_no: str):
        """Unfreeze stock movements after stocktake"""
        # This would implement movement unfreezing logic
        # For now, just log the action
        pass
        
    def _update_last_counted_dates(self, stocktake_no: str):
        """Update last counted dates on stock master"""
        count_date = int(datetime.now().strftime("%Y%m%d"))
        
        # Get all stock codes in stocktake
        stock_codes = self.db.query(StocktakeLineRec.line_stock_code.distinct()).filter(
            StocktakeLineRec.line_stocktake_no == stocktake_no
        ).all()
        
        # Update stock master records
        for (stock_code,) in stock_codes:
            stock = self.db.query(StockMasterRec).filter(
                StockMasterRec.stock_code == stock_code
            ).first()
            
            if stock:
                stock.stock_last_counted = count_date
                
    def _generate_completion_report(self, stocktake: PhysicalStocktakeRec) -> Dict:
        """Generate stocktake completion report"""
        # Get summary statistics
        lines = self.db.query(StocktakeLineRec).filter(
            StocktakeLineRec.line_stocktake_no == stocktake.stocktake_no
        ).all()
        
        variances = self.db.query(StocktakeVarianceRec).filter(
            StocktakeVarianceRec.var_stocktake_no == stocktake.stocktake_no
        ).all()
        
        total_value_before = sum(float(line.line_system_qty * line.line_unit_cost) for line in lines)
        total_value_after = sum(float(line.line_counted_qty * line.line_unit_cost) for line in lines)
        total_adjustment = total_value_after - total_value_before
        
        return {
            'total_lines': len(lines),
            'variance_lines': len(variances),
            'total_value_before': total_value_before,
            'total_value_after': total_value_after,
            'total_adjustment_value': total_adjustment,
            'accuracy_pct': ((len(lines) - len(variances)) / len(lines) * 100) if lines else 100
        }
        
    def _get_next_stocktake_number(self) -> str:
        """Generate next stocktake number"""
        return f"STKTKR{datetime.now().strftime('%Y%m%d%H%M%S')}"
        
    def _get_next_tag_number(self, stocktake_no: str) -> str:
        """Generate next tag number"""
        # Get existing tag count
        tag_count = self.db.query(StocktakeTagRec).filter(
            StocktakeTagRec.tag_stocktake_no == stocktake_no
        ).count()
        
        return f"{stocktake_no}-TAG{tag_count + 1:04d}"