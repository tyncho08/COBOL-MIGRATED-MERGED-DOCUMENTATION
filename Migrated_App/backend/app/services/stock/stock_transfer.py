"""
Stock Transfer Service - ST150 migration
Handles inter-location and inter-warehouse stock transfers
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from datetime import datetime
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func, desc

from app.services.file_handlers.stock_handler import StockFileHandler
from app.services.file_handlers.system_handler import SystemFileHandler
from app.models.stock import (
    StockMasterRec, StockLocationRec, StockTransferRec,
    StockTransferLineRec, TransferRequestRec, StockMovementRec
)
from app.services.stock.stock_movements import StockMovementsService
from app.core.security import log_user_action
from app.models.auth import User


class StockTransferService:
    """
    Stock Transfer functionality
    Implements ST150 - inter-location and inter-warehouse transfers
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.stock_handler = StockFileHandler(db)
        self.system_handler = SystemFileHandler(db)
        
    def create_transfer_request(self, request_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Create stock transfer request
        Returns (success, error_message or request_data)
        """
        from_warehouse = request_data.get('from_warehouse')
        to_warehouse = request_data.get('to_warehouse')
        from_location = request_data.get('from_location', '')
        to_location = request_data.get('to_location', '')
        transfer_type = request_data.get('type', 'LOCATION')  # LOCATION, WAREHOUSE
        priority = request_data.get('priority', 'NORMAL')
        reason = request_data.get('reason', '')
        transfer_lines = request_data.get('lines', [])
        
        if not transfer_lines:
            return False, "Transfer lines required"
            
        try:
            # Create transfer request
            request_no = self._get_next_transfer_request_number()
            
            transfer_request = TransferRequestRec(
                req_no=request_no,
                req_from_warehouse=from_warehouse,
                req_to_warehouse=to_warehouse,
                req_from_location=from_location,
                req_to_location=to_location,
                req_type=transfer_type,
                req_status='PENDING',
                req_priority=priority,
                req_reason=reason,
                req_created_date=int(datetime.now().strftime("%Y%m%d")),
                req_created_time=int(datetime.now().strftime("%H%M%S")),
                req_created_by=self.current_user.username if self.current_user else 'SYSTEM',
                req_total_lines=len(transfer_lines),
                req_approved_lines=0,
                req_rejected_lines=0
            )
            
            self.db.add(transfer_request)
            self.db.flush()
            
            # Validate and create transfer lines
            valid_lines = 0
            
            for line_data in transfer_lines:
                stock_code = line_data.get('stock_code')
                quantity = Decimal(str(line_data.get('quantity', 0)))
                
                # Validate stock availability
                if transfer_type == 'WAREHOUSE':
                    available_qty = self._get_warehouse_availability(stock_code, from_warehouse)
                else:
                    available_qty = self._get_location_availability(stock_code, from_warehouse, from_location)
                    
                if quantity > available_qty:
                    # Create line with note about insufficient stock
                    line_status = 'INSUFFICIENT'
                    line_note = f"Requested: {quantity}, Available: {available_qty}"
                else:
                    line_status = 'PENDING'
                    line_note = ''
                    valid_lines += 1
                    
                # Get stock details
                stock, _ = self.stock_handler.process(4, key_value=stock_code)
                
                request_line = StockTransferLineRec(
                    line_request_no=request_no,
                    line_seq=len(transfer_lines),
                    line_stock_code=stock_code,
                    line_description=stock.stock_desc if stock else '',
                    line_quantity_requested=quantity,
                    line_quantity_approved=Decimal('0'),
                    line_quantity_transferred=Decimal('0'),
                    line_unit=stock.stock_unit if stock else 'EA',
                    line_unit_cost=stock.stock_average_cost if stock else Decimal('0'),
                    line_status=line_status,
                    line_notes=line_note
                )
                
                self.db.add(request_line)
                
            # Update request summary
            transfer_request.req_valid_lines = valid_lines
            
            self.db.commit()
            
            # Log creation
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="CREATE_TRANSFER_REQUEST",
                    table="transfer_request_rec",
                    key=request_no,
                    new_values={
                        'from_warehouse': from_warehouse,
                        'to_warehouse': to_warehouse,
                        'type': transfer_type,
                        'lines': len(transfer_lines)
                    },
                    module="STOCK"
                )
                
            return True, {
                'request_no': request_no,
                'total_lines': len(transfer_lines),
                'valid_lines': valid_lines,
                'status': 'PENDING'
            }
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def approve_transfer_request(self, approval_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Approve transfer request
        Returns (success, error_message)
        """
        request_no = approval_data.get('request_no')
        line_approvals = approval_data.get('lines', [])
        approval_notes = approval_data.get('notes', '')
        
        request = self.db.query(TransferRequestRec).filter(
            TransferRequestRec.req_no == request_no
        ).first()
        
        if not request:
            return False, "Transfer request not found"
            
        if request.req_status != 'PENDING':
            return False, f"Cannot approve request with status: {request.req_status}"
            
        try:
            approved_lines = 0
            rejected_lines = 0
            
            for approval in line_approvals:
                line_seq = approval.get('line_seq')
                approved = approval.get('approved', True)
                approved_qty = Decimal(str(approval.get('approved_qty', 0)))
                rejection_reason = approval.get('reason', '')
                
                # Get transfer line
                transfer_line = self.db.query(StockTransferLineRec).filter(
                    and_(
                        StockTransferLineRec.line_request_no == request_no,
                        StockTransferLineRec.line_seq == line_seq
                    )
                ).first()
                
                if not transfer_line:
                    continue
                    
                if approved and approved_qty > 0:
                    transfer_line.line_status = 'APPROVED'
                    transfer_line.line_quantity_approved = approved_qty
                    approved_lines += 1
                else:
                    transfer_line.line_status = 'REJECTED'
                    transfer_line.line_notes = rejection_reason
                    rejected_lines += 1
                    
                transfer_line.line_approved_by = self.current_user.username if self.current_user else 'SYSTEM'
                transfer_line.line_approved_date = int(datetime.now().strftime("%Y%m%d"))
                
            # Update request status
            if approved_lines > 0:
                request.req_status = 'APPROVED' if rejected_lines == 0 else 'PARTIAL'
            else:
                request.req_status = 'REJECTED'
                
            request.req_approved_lines = approved_lines
            request.req_rejected_lines = rejected_lines
            request.req_approved_by = self.current_user.username if self.current_user else 'SYSTEM'
            request.req_approved_date = int(datetime.now().strftime("%Y%m%d"))
            request.req_approval_notes = approval_notes
            
            self.db.commit()
            
            # Log approval
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="APPROVE_TRANSFER_REQUEST",
                    table="transfer_request_rec",
                    key=request_no,
                    new_values={
                        'approved_lines': approved_lines,
                        'rejected_lines': rejected_lines,
                        'status': request.req_status
                    },
                    module="STOCK"
                )
                
            return True, f"Request {request_no}: {approved_lines} lines approved, {rejected_lines} rejected"
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def execute_transfer(self, transfer_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Execute approved stock transfer
        Returns (success, error_message or transfer_data)
        """
        request_no = transfer_data.get('request_no', '')
        direct_transfer = transfer_data.get('direct_transfer', False)
        transfer_lines = transfer_data.get('lines', [])
        
        if not direct_transfer and not request_no:
            return False, "Transfer request number or direct transfer data required"
            
        try:
            # Create transfer header
            transfer_no = self._get_next_transfer_number()
            
            if request_no:
                # Transfer from approved request
                request = self.db.query(TransferRequestRec).filter(
                    TransferRequestRec.req_no == request_no
                ).first()
                
                if not request:
                    return False, "Transfer request not found"
                    
                if request.req_status not in ['APPROVED', 'PARTIAL']:
                    return False, f"Cannot execute request with status: {request.req_status}"
                    
                # Get approved lines
                approved_lines = self.db.query(StockTransferLineRec).filter(
                    and_(
                        StockTransferLineRec.line_request_no == request_no,
                        StockTransferLineRec.line_status == 'APPROVED'
                    )
                ).all()
                
                transfer = StockTransferRec(
                    trans_no=transfer_no,
                    trans_request_no=request_no,
                    trans_from_warehouse=request.req_from_warehouse,
                    trans_to_warehouse=request.req_to_warehouse,
                    trans_from_location=request.req_from_location,
                    trans_to_location=request.req_to_location,
                    trans_type=request.req_type,
                    trans_status='IN_PROGRESS',
                    trans_reason=request.req_reason,
                    trans_created_date=int(datetime.now().strftime("%Y%m%d")),
                    trans_created_time=int(datetime.now().strftime("%H%M%S")),
                    trans_created_by=self.current_user.username if self.current_user else 'SYSTEM',
                    trans_total_lines=len(approved_lines),
                    trans_completed_lines=0
                )
                
                lines_to_transfer = [
                    {
                        'stock_code': line.line_stock_code,
                        'quantity': line.line_quantity_approved,
                        'unit_cost': line.line_unit_cost,
                        'line_seq': line.line_seq
                    }
                    for line in approved_lines
                ]
                
            else:
                # Direct transfer
                from_warehouse = transfer_data.get('from_warehouse')
                to_warehouse = transfer_data.get('to_warehouse')
                from_location = transfer_data.get('from_location', '')
                to_location = transfer_data.get('to_location', '')
                transfer_type = transfer_data.get('type', 'LOCATION')
                
                transfer = StockTransferRec(
                    trans_no=transfer_no,
                    trans_from_warehouse=from_warehouse,
                    trans_to_warehouse=to_warehouse,
                    trans_from_location=from_location,
                    trans_to_location=to_location,
                    trans_type=transfer_type,
                    trans_status='IN_PROGRESS',
                    trans_reason=transfer_data.get('reason', ''),
                    trans_created_date=int(datetime.now().strftime("%Y%m%d")),
                    trans_created_time=int(datetime.now().strftime("%H%M%S")),
                    trans_created_by=self.current_user.username if self.current_user else 'SYSTEM',
                    trans_total_lines=len(transfer_lines),
                    trans_completed_lines=0
                )
                
                lines_to_transfer = transfer_lines
                
            self.db.add(transfer)
            self.db.flush()
            
            # Execute transfer lines
            completed_lines = 0
            total_value = Decimal('0')
            
            for line_data in lines_to_transfer:
                stock_code = line_data.get('stock_code')
                quantity = Decimal(str(line_data.get('quantity', 0)))
                unit_cost = line_data.get('unit_cost', Decimal('0'))
                
                # Execute the actual transfer
                success, error = self._execute_stock_transfer(
                    stock_code,
                    quantity,
                    unit_cost,
                    transfer.trans_from_warehouse,
                    transfer.trans_to_warehouse,
                    transfer.trans_from_location,
                    transfer.trans_to_location,
                    transfer_no
                )
                
                if success:
                    completed_lines += 1
                    total_value += quantity * unit_cost
                    
                    # Update request line if applicable
                    if request_no:
                        request_line = self.db.query(StockTransferLineRec).filter(
                            and_(
                                StockTransferLineRec.line_request_no == request_no,
                                StockTransferLineRec.line_seq == line_data.get('line_seq')
                            )
                        ).first()
                        
                        if request_line:
                            request_line.line_quantity_transferred = quantity
                            request_line.line_status = 'TRANSFERRED'
                            
            # Update transfer status
            transfer.trans_completed_lines = completed_lines
            transfer.trans_total_value = total_value
            
            if completed_lines == transfer.trans_total_lines:
                transfer.trans_status = 'COMPLETE'
            elif completed_lines > 0:
                transfer.trans_status = 'PARTIAL'
            else:
                transfer.trans_status = 'FAILED'
                
            transfer.trans_completed_date = int(datetime.now().strftime("%Y%m%d"))
            transfer.trans_completed_time = int(datetime.now().strftime("%H%M%S"))
            
            # Update request status if applicable
            if request_no:
                request.req_status = 'TRANSFERRED' if transfer.trans_status == 'COMPLETE' else 'PARTIAL_TRANSFER'
                
            self.db.commit()
            
            # Log execution
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="EXECUTE_TRANSFER",
                    table="stock_transfer_rec",
                    key=transfer_no,
                    new_values={
                        'completed_lines': completed_lines,
                        'total_value': float(total_value),
                        'status': transfer.trans_status
                    },
                    module="STOCK"
                )
                
            return True, {
                'transfer_no': transfer_no,
                'completed_lines': completed_lines,
                'total_lines': transfer.trans_total_lines,
                'total_value': float(total_value),
                'status': transfer.trans_status
            }
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def get_transfer_requests(self, filters: Optional[Dict] = None) -> List[Dict]:
        """Get transfer requests with optional filters"""
        filters = filters or {}
        
        query = self.db.query(TransferRequestRec)
        
        # Apply filters
        if filters.get('status'):
            query = query.filter(TransferRequestRec.req_status == filters['status'])
        if filters.get('from_warehouse'):
            query = query.filter(TransferRequestRec.req_from_warehouse == filters['from_warehouse'])
        if filters.get('to_warehouse'):
            query = query.filter(TransferRequestRec.req_to_warehouse == filters['to_warehouse'])
        if filters.get('created_by'):
            query = query.filter(TransferRequestRec.req_created_by == filters['created_by'])
        if filters.get('date_from'):
            query = query.filter(TransferRequestRec.req_created_date >= filters['date_from'])
        if filters.get('date_to'):
            query = query.filter(TransferRequestRec.req_created_date <= filters['date_to'])
            
        # Order by creation date descending
        requests = query.order_by(desc(TransferRequestRec.req_created_date)).all()
        
        return [
            {
                'request_no': req.req_no,
                'type': req.req_type,
                'from_warehouse': req.req_from_warehouse,
                'to_warehouse': req.req_to_warehouse,
                'from_location': req.req_from_location,
                'to_location': req.req_to_location,
                'status': req.req_status,
                'priority': req.req_priority,
                'reason': req.req_reason,
                'total_lines': req.req_total_lines,
                'valid_lines': req.req_valid_lines,
                'approved_lines': req.req_approved_lines,
                'rejected_lines': req.req_rejected_lines,
                'created_by': req.req_created_by,
                'created_date': req.req_created_date,
                'approved_by': req.req_approved_by,
                'approved_date': req.req_approved_date
            }
            for req in requests
        ]
        
    def get_transfer_history(self, filters: Optional[Dict] = None) -> List[Dict]:
        """Get transfer history with optional filters"""
        filters = filters or {}
        
        query = self.db.query(StockTransferRec)
        
        # Apply filters
        if filters.get('status'):
            query = query.filter(StockTransferRec.trans_status == filters['status'])
        if filters.get('from_warehouse'):
            query = query.filter(StockTransferRec.trans_from_warehouse == filters['from_warehouse'])
        if filters.get('to_warehouse'):
            query = query.filter(StockTransferRec.trans_to_warehouse == filters['to_warehouse'])
        if filters.get('date_from'):
            query = query.filter(StockTransferRec.trans_created_date >= filters['date_from'])
        if filters.get('date_to'):
            query = query.filter(StockTransferRec.trans_created_date <= filters['date_to'])
            
        transfers = query.order_by(desc(StockTransferRec.trans_created_date)).all()
        
        return [
            {
                'transfer_no': trans.trans_no,
                'request_no': trans.trans_request_no,
                'type': trans.trans_type,
                'from_warehouse': trans.trans_from_warehouse,
                'to_warehouse': trans.trans_to_warehouse,
                'from_location': trans.trans_from_location,
                'to_location': trans.trans_to_location,
                'status': trans.trans_status,
                'reason': trans.trans_reason,
                'total_lines': trans.trans_total_lines,
                'completed_lines': trans.trans_completed_lines,
                'total_value': float(trans.trans_total_value or 0),
                'created_by': trans.trans_created_by,
                'created_date': trans.trans_created_date,
                'completed_date': trans.trans_completed_date
            }
            for trans in transfers
        ]
        
    def get_transfer_analytics(self, warehouse: str, period_days: int = 30) -> Dict:
        """Get transfer analytics for warehouse"""
        try:
            # Date range
            end_date = int(datetime.now().strftime("%Y%m%d"))
            start_date = int((datetime.now() - timedelta(days=period_days)).strftime("%Y%m%d"))
            
            # Get transfers for period
            outbound_transfers = self.db.query(StockTransferRec).filter(
                and_(
                    StockTransferRec.trans_from_warehouse == warehouse,
                    StockTransferRec.trans_created_date >= start_date,
                    StockTransferRec.trans_created_date <= end_date
                )
            ).all()
            
            inbound_transfers = self.db.query(StockTransferRec).filter(
                and_(
                    StockTransferRec.trans_to_warehouse == warehouse,
                    StockTransferRec.trans_created_date >= start_date,
                    StockTransferRec.trans_created_date <= end_date
                )
            ).all()
            
            # Calculate metrics
            outbound_value = sum(float(t.trans_total_value or 0) for t in outbound_transfers)
            inbound_value = sum(float(t.trans_total_value or 0) for t in inbound_transfers)
            net_value = inbound_value - outbound_value
            
            # Status breakdown
            status_counts = {}
            for transfer in outbound_transfers + inbound_transfers:
                status = transfer.trans_status
                status_counts[status] = status_counts.get(status, 0) + 1
                
            # Top destination/source analysis
            destinations = {}
            sources = {}
            
            for transfer in outbound_transfers:
                dest = transfer.trans_to_warehouse
                destinations[dest] = destinations.get(dest, 0) + 1
                
            for transfer in inbound_transfers:
                source = transfer.trans_from_warehouse
                sources[source] = sources.get(source, 0) + 1
                
            return {
                'warehouse': warehouse,
                'period_days': period_days,
                'summary': {
                    'total_outbound_transfers': len(outbound_transfers),
                    'total_inbound_transfers': len(inbound_transfers),
                    'outbound_value': outbound_value,
                    'inbound_value': inbound_value,
                    'net_transfer_value': net_value
                },
                'status_breakdown': status_counts,
                'top_destinations': sorted(destinations.items(), key=lambda x: x[1], reverse=True)[:5],
                'top_sources': sorted(sources.items(), key=lambda x: x[1], reverse=True)[:5],
                'trends': self._calculate_transfer_trends(outbound_transfers + inbound_transfers)
            }
            
        except Exception as e:
            return {"error": str(e)}
            
    def _get_warehouse_availability(self, stock_code: str, warehouse: str) -> Decimal:
        """Get total available quantity in warehouse"""
        total = self.db.query(
            func.sum(StockLocationRec.loc_qty_on_hand - StockLocationRec.loc_qty_allocated)
        ).filter(
            and_(
                StockLocationRec.loc_stock_code == stock_code,
                StockLocationRec.loc_warehouse == warehouse,
                StockLocationRec.loc_active == 'Y'
            )
        ).scalar() or Decimal('0')
        
        return max(total, Decimal('0'))
        
    def _get_location_availability(self, stock_code: str, warehouse: str, location: str) -> Decimal:
        """Get available quantity in specific location"""
        location_rec = self.db.query(StockLocationRec).filter(
            and_(
                StockLocationRec.loc_stock_code == stock_code,
                StockLocationRec.loc_warehouse == warehouse,
                StockLocationRec.loc_location == location
            )
        ).first()
        
        if not location_rec:
            return Decimal('0')
            
        return max(location_rec.loc_qty_on_hand - location_rec.loc_qty_allocated, Decimal('0'))
        
    def _execute_stock_transfer(self, stock_code: str, quantity: Decimal, unit_cost: Decimal,
                              from_warehouse: str, to_warehouse: str, from_location: str, 
                              to_location: str, transfer_no: str) -> Tuple[bool, Optional[str]]:
        """Execute the actual stock movement for transfer"""
        try:
            movements_service = StockMovementsService(self.db, self.current_user)
            
            if from_warehouse == to_warehouse:
                # Location transfer within same warehouse
                transfer_data = {
                    'stock_code': stock_code,
                    'from_warehouse': from_warehouse,
                    'to_warehouse': to_warehouse,
                    'from_location': from_location,
                    'to_location': to_location,
                    'quantity': float(quantity),
                    'reference': f"TRF-{transfer_no}"
                }
                
                return movements_service.process_transfer(transfer_data)
                
            else:
                # Inter-warehouse transfer - issue from source, receive at destination
                
                # Issue from source warehouse
                issue_data = {
                    'stock_code': stock_code,
                    'warehouse': from_warehouse,
                    'location': from_location,
                    'issue_qty': float(quantity),
                    'reason': 'WAREHOUSE_TRANSFER',
                    'reference': f"TRFOUT-{transfer_no}"
                }
                
                success, error = movements_service.process_issue(issue_data)
                if not success:
                    return False, f"Failed to issue from source: {error}"
                    
                # Receipt at destination warehouse
                receipt_data = {
                    'stock_code': stock_code,
                    'warehouse': to_warehouse,
                    'location': to_location,
                    'receipt_qty': float(quantity),
                    'unit_cost': float(unit_cost),
                    'reason': 'WAREHOUSE_TRANSFER',
                    'reference': f"TRFIN-{transfer_no}"
                }
                
                return movements_service.process_receipt(receipt_data)
                
        except Exception as e:
            return False, str(e)
            
    def _calculate_transfer_trends(self, transfers: List[StockTransferRec]) -> Dict:
        """Calculate transfer trends"""
        if not transfers:
            return {}
            
        # Group by week
        weekly_counts = {}
        weekly_values = {}
        
        for transfer in transfers:
            if transfer.trans_created_date:
                transfer_date = datetime.strptime(str(transfer.trans_created_date), "%Y%m%d")
                week_start = transfer_date - timedelta(days=transfer_date.weekday())
                week_key = week_start.strftime("%Y-%W")
                
                weekly_counts[week_key] = weekly_counts.get(week_key, 0) + 1
                weekly_values[week_key] = weekly_values.get(week_key, 0) + float(transfer.trans_total_value or 0)
                
        # Calculate trends
        weeks = sorted(weekly_counts.keys())
        if len(weeks) >= 2:
            recent_count = weekly_counts[weeks[-1]]
            previous_count = weekly_counts[weeks[-2]] if len(weeks) > 1 else recent_count
            count_trend = 'INCREASING' if recent_count > previous_count else 'DECREASING' if recent_count < previous_count else 'STABLE'
        else:
            count_trend = 'STABLE'
            
        return {
            'weekly_transfer_counts': weekly_counts,
            'weekly_transfer_values': weekly_values,
            'trend_direction': count_trend,
            'peak_week': max(weekly_counts.items(), key=lambda x: x[1])[0] if weekly_counts else None
        }
        
    def _get_next_transfer_request_number(self) -> str:
        """Generate next transfer request number"""
        return f"TREQ{datetime.now().strftime('%Y%m%d%H%M%S')}"
        
    def _get_next_transfer_number(self) -> str:
        """Generate next transfer number"""
        return f"TRF{datetime.now().strftime('%Y%m%d%H%M%S')}"