"""
Returns Processing Service - ST200 migration
Handles customer returns, RMA processing and disposition
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from datetime import datetime, timedelta
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func, desc

from app.services.file_handlers.stock_handler import StockFileHandler
from app.services.file_handlers.system_handler import SystemFileHandler
from app.models.stock import (
    ReturnRec, ReturnLineRec, ReturnAuthorizationRec, ReturnDispositionRec,
    StockMasterRec, StockLocationRec, SerialNumberRec, LotNumberRec
)
from app.models.sales import SalesOrderRec, SalesOrderLineRec
from app.services.stock.stock_movements import StockMovementsService
from app.services.stock.quality_control import QualityControlService
from app.core.security import log_user_action
from app.models.auth import User


class ReturnsProcessingService:
    """
    Returns Processing functionality
    Implements ST200 - customer returns and RMA processing
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.stock_handler = StockFileHandler(db)
        self.system_handler = SystemFileHandler(db)
        
    def create_return_authorization(self, rma_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Create Return Merchandise Authorization (RMA)
        Returns (success, error_message or rma_data)
        """
        customer_code = rma_data.get('customer_code')
        original_order = rma_data.get('original_order', '')
        return_reason = rma_data.get('reason', '')
        return_lines = rma_data.get('lines', [])
        authorized_by = rma_data.get('authorized_by', '')
        credit_required = rma_data.get('credit_required', 'Y')
        replacement_required = rma_data.get('replacement_required', 'N')
        
        if not customer_code or not return_lines:
            return False, "Customer code and return lines required"
            
        try:
            # Create RMA
            rma_no = self._get_next_rma_number()
            
            rma = ReturnAuthorizationRec(
                rma_no=rma_no,
                rma_customer_code=customer_code,
                rma_original_order=original_order,
                rma_reason=return_reason,
                rma_status='AUTHORIZED',
                rma_authorized_date=int(datetime.now().strftime("%Y%m%d")),
                rma_authorized_by=authorized_by,
                rma_created_by=self.current_user.username if self.current_user else 'SYSTEM',
                rma_expiry_date=int((datetime.now() + timedelta(days=30)).strftime("%Y%m%d")),
                rma_credit_required=credit_required,
                rma_replacement_required=replacement_required,
                rma_total_lines=len(return_lines),
                rma_returned_lines=0,
                rma_total_value=Decimal('0')
            )
            
            self.db.add(rma)
            self.db.flush()
            
            # Create RMA lines
            total_value = Decimal('0')
            
            for line_data in return_lines:
                stock_code = line_data.get('stock_code')
                quantity = Decimal(str(line_data.get('quantity', 0)))
                unit_price = Decimal(str(line_data.get('unit_price', 0)))
                serial_number = line_data.get('serial_number', '')
                lot_number = line_data.get('lot_number', '')
                line_reason = line_data.get('line_reason', return_reason)
                
                # Validate stock item
                stock, _ = self.stock_handler.process(4, key_value=stock_code)
                if not stock:
                    continue
                    
                line_value = quantity * unit_price
                total_value += line_value
                
                rma_line = ReturnLineRec(
                    line_rma_no=rma_no,
                    line_seq=len(return_lines),
                    line_stock_code=stock_code,
                    line_description=stock.stock_desc,
                    line_quantity_authorized=quantity,
                    line_quantity_returned=Decimal('0'),
                    line_unit_price=unit_price,
                    line_line_value=line_value,
                    line_serial_number=serial_number,
                    line_lot_number=lot_number,
                    line_reason=line_reason,
                    line_status='AUTHORIZED',
                    line_disposition='PENDING'
                )
                
                self.db.add(rma_line)
                
            # Update RMA totals
            rma.rma_total_value = total_value
            
            self.db.commit()
            
            # Log creation
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="CREATE_RMA",
                    table="return_authorization_rec",
                    key=rma_no,
                    new_values={
                        'customer': customer_code,
                        'lines': len(return_lines),
                        'value': float(total_value)
                    },
                    module="STOCK"
                )
                
            return True, {
                'rma_no': rma_no,
                'customer_code': customer_code,
                'total_lines': len(return_lines),
                'total_value': float(total_value),
                'expiry_date': rma.rma_expiry_date
            }
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def receive_return(self, return_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Receive customer return
        Returns (success, error_message or return_data)
        """
        rma_no = return_data.get('rma_no', '')
        warehouse = return_data.get('warehouse', 'MAIN')
        return_location = return_data.get('location', 'RETURNS')
        return_lines = return_data.get('lines', [])
        received_by = return_data.get('received_by', '')
        carrier = return_data.get('carrier', '')
        tracking_number = return_data.get('tracking_number', '')
        
        # Validate RMA if provided
        if rma_no:
            rma = self.db.query(ReturnAuthorizationRec).filter(
                ReturnAuthorizationRec.rma_no == rma_no
            ).first()
            
            if not rma:
                return False, f"RMA {rma_no} not found"
                
            if rma.rma_status != 'AUTHORIZED':
                return False, f"RMA {rma_no} is not authorized (status: {rma.rma_status})"
                
            # Check expiry
            if rma.rma_expiry_date < int(datetime.now().strftime("%Y%m%d")):
                return False, f"RMA {rma_no} has expired"
                
        try:
            # Create return receipt
            return_no = self._get_next_return_number()
            
            return_receipt = ReturnRec(
                return_no=return_no,
                return_rma_no=rma_no,
                return_warehouse=warehouse,
                return_location=return_location,
                return_received_date=int(datetime.now().strftime("%Y%m%d")),
                return_received_time=int(datetime.now().strftime("%H%M%S")),
                return_received_by=received_by,
                return_carrier=carrier,
                return_tracking_number=tracking_number,
                return_status='RECEIVED',
                return_created_by=self.current_user.username if self.current_user else 'SYSTEM',
                return_total_lines=len(return_lines),
                return_total_value=Decimal('0')
            )
            
            self.db.add(return_receipt)
            self.db.flush()
            
            # Process return lines
            movements_service = StockMovementsService(self.db, self.current_user)
            qc_service = QualityControlService(self.db, self.current_user)
            total_value = Decimal('0')
            processed_lines = 0
            
            for line_data in return_lines:
                stock_code = line_data.get('stock_code')
                quantity = Decimal(str(line_data.get('quantity', 0)))
                condition = line_data.get('condition', 'UNKNOWN')  # GOOD, DAMAGED, DEFECTIVE
                serial_number = line_data.get('serial_number', '')
                lot_number = line_data.get('lot_number', '')
                line_notes = line_data.get('notes', '')
                
                # Get stock details
                stock, _ = self.stock_handler.process(4, key_value=stock_code)
                if not stock:
                    continue
                    
                unit_cost = stock.stock_average_cost
                line_value = quantity * unit_cost
                total_value += line_value
                
                # Create return line
                return_line = ReturnLineRec(
                    line_return_no=return_no,
                    line_seq=processed_lines + 1,
                    line_stock_code=stock_code,
                    line_description=stock.stock_desc,
                    line_quantity_returned=quantity,
                    line_unit_cost=unit_cost,
                    line_line_value=line_value,
                    line_serial_number=serial_number,
                    line_lot_number=lot_number,
                    line_condition=condition,
                    line_status='RECEIVED',
                    line_disposition='PENDING',
                    line_notes=line_notes
                )
                
                self.db.add(return_line)
                
                # Process based on condition
                if condition == 'GOOD':
                    # Receive back to available stock
                    receipt_data = {
                        'stock_code': stock_code,
                        'warehouse': warehouse,
                        'location': return_location,
                        'receipt_qty': float(quantity),
                        'unit_cost': float(unit_cost),
                        'reason': 'CUSTOMER_RETURN',
                        'reference': f"RET-{return_no}",
                        'lot_number': lot_number,
                        'serial_number': serial_number
                    }
                    
                    success, error = movements_service.process_receipt(receipt_data)
                    if success:
                        return_line.line_disposition = 'RESTOCKED'
                        return_line.line_status = 'PROCESSED'
                    else:
                        return_line.line_notes = f"Receipt failed: {error}"
                        
                elif condition in ['DAMAGED', 'DEFECTIVE']:
                    # Receive to quarantine location
                    receipt_data = {
                        'stock_code': stock_code,
                        'warehouse': warehouse,
                        'location': 'QUARANTINE',
                        'receipt_qty': float(quantity),
                        'unit_cost': float(unit_cost),
                        'reason': 'RETURN_QUARANTINE',
                        'reference': f"RET-{return_no}",
                        'lot_number': lot_number,
                        'serial_number': serial_number
                    }
                    
                    success, error = movements_service.process_receipt(receipt_data)
                    if success:
                        return_line.line_disposition = 'QUARANTINE'
                        
                        # Create QC inspection for damaged/defective returns
                        if condition == 'DEFECTIVE':
                            inspection_data = {
                                'stock_code': stock_code,
                                'warehouse': warehouse,
                                'location': 'QUARANTINE',
                                'lot_number': lot_number,
                                'serial_number': serial_number,
                                'quantity': float(quantity),
                                'type': 'RETURN',
                                'supplier': '',
                                'receipt_reference': f"RET-{return_no}"
                            }
                            
                            qc_success, qc_result = qc_service.create_qc_inspection(inspection_data)
                            if qc_success:
                                return_line.line_notes = f"QC inspection created: {qc_result.get('inspection_no', '')}"
                                
                else:
                    # Unknown condition - hold for inspection
                    return_line.line_disposition = 'INSPECTION_REQUIRED'
                    
                # Update serial/lot status if applicable
                if serial_number:
                    self._update_serial_return_status(stock_code, serial_number, condition)
                    
                if lot_number:
                    self._update_lot_return_status(stock_code, lot_number, condition)
                    
                # Update RMA line if applicable
                if rma_no:
                    rma_line = self.db.query(ReturnLineRec).filter(
                        and_(
                            ReturnLineRec.line_rma_no == rma_no,
                            ReturnLineRec.line_stock_code == stock_code
                        )
                    ).first()
                    
                    if rma_line:
                        rma_line.line_quantity_returned += quantity
                        if rma_line.line_quantity_returned >= rma_line.line_quantity_authorized:
                            rma_line.line_status = 'COMPLETED'
                            
                processed_lines += 1
                
            # Update return totals
            return_receipt.return_total_lines = processed_lines
            return_receipt.return_total_value = total_value
            
            # Update RMA status if applicable
            if rma_no:
                rma.rma_returned_lines += processed_lines
                if rma.rma_returned_lines >= rma.rma_total_lines:
                    rma.rma_status = 'COMPLETED'
                elif rma.rma_returned_lines > 0:
                    rma.rma_status = 'PARTIAL'
                    
            self.db.commit()
            
            return True, {
                'return_no': return_no,
                'rma_no': rma_no,
                'processed_lines': processed_lines,
                'total_value': float(total_value),
                'status': 'RECEIVED'
            }
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def process_return_disposition(self, disposition_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Process return disposition (approve, scrap, rework, etc.)
        Returns (success, error_message)
        """
        return_no = disposition_data.get('return_no')
        line_seq = disposition_data.get('line_seq')
        disposition = disposition_data.get('disposition')  # APPROVE, SCRAP, REWORK, REPLACE, CREDIT
        disposition_notes = disposition_data.get('notes', '')
        credit_value = Decimal(str(disposition_data.get('credit_value', 0)))
        
        # Get return line
        return_line = self.db.query(ReturnLineRec).filter(
            and_(
                ReturnLineRec.line_return_no == return_no,
                ReturnLineRec.line_seq == line_seq
            )
        ).first()
        
        if not return_line:
            return False, f"Return line {return_no}-{line_seq} not found"
            
        if return_line.line_status != 'RECEIVED':
            return False, f"Cannot process disposition for line with status: {return_line.line_status}"
            
        try:
            movements_service = StockMovementsService(self.db, self.current_user)
            
            # Create disposition record
            disposition_record = ReturnDispositionRec(
                disp_return_no=return_no,
                disp_line_seq=line_seq,
                disp_disposition=disposition,
                disp_disposition_date=int(datetime.now().strftime("%Y%m%d")),
                disp_disposition_by=self.current_user.username if self.current_user else 'SYSTEM',
                disp_notes=disposition_notes,
                disp_credit_value=credit_value,
                disp_status='PROCESSED'
            )
            
            self.db.add(disposition_record)
            
            # Process based on disposition
            if disposition == 'APPROVE':
                # Move from returns/quarantine to available stock
                if return_line.line_condition in ['DAMAGED', 'DEFECTIVE']:
                    # Transfer from quarantine to main stock
                    transfer_data = {
                        'stock_code': return_line.line_stock_code,
                        'from_warehouse': 'MAIN',
                        'to_warehouse': 'MAIN',
                        'from_location': 'QUARANTINE',
                        'to_location': 'MAIN',
                        'quantity': float(return_line.line_quantity_returned),
                        'reference': f"DISP-{return_no}-{line_seq}"
                    }
                    
                    movements_service.process_transfer(transfer_data)
                    
                return_line.line_disposition = 'APPROVED'
                
            elif disposition == 'SCRAP':
                # Issue stock as scrap
                scrap_data = {
                    'stock_code': return_line.line_stock_code,
                    'warehouse': 'MAIN',
                    'location': return_line.line_condition == 'GOOD' and 'RETURNS' or 'QUARANTINE',
                    'issue_qty': float(return_line.line_quantity_returned),
                    'reason': 'RETURN_SCRAP',
                    'reference': f"SCRAP-{return_no}-{line_seq}"
                }
                
                movements_service.process_issue(scrap_data)
                return_line.line_disposition = 'SCRAPPED'
                
            elif disposition == 'REWORK':
                # Transfer to rework location
                transfer_data = {
                    'stock_code': return_line.line_stock_code,
                    'from_warehouse': 'MAIN',
                    'to_warehouse': 'MAIN', 
                    'from_location': return_line.line_condition == 'GOOD' and 'RETURNS' or 'QUARANTINE',
                    'to_location': 'REWORK',
                    'quantity': float(return_line.line_quantity_returned),
                    'reference': f"REWORK-{return_no}-{line_seq}"
                }
                
                movements_service.process_transfer(transfer_data)
                return_line.line_disposition = 'REWORK'
                
            elif disposition == 'REPLACE':
                # Keep original disposition, create replacement requirement
                return_line.line_disposition = 'REPLACEMENT_DUE'
                disposition_record.disp_replacement_required = 'Y'
                
            elif disposition == 'CREDIT':
                # Credit only, no stock movement
                return_line.line_disposition = 'CREDITED'
                disposition_record.disp_credit_issued = 'Y'
                
            # Update return line status
            return_line.line_status = 'PROCESSED'
            return_line.line_processed_date = int(datetime.now().strftime("%Y%m%d"))
            return_line.line_processed_by = self.current_user.username if self.current_user else 'SYSTEM'
            
            self.db.commit()
            
            # Log disposition
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="PROCESS_RETURN_DISPOSITION",
                    table="return_disposition_rec",
                    key=f"{return_no}-{line_seq}",
                    new_values={
                        'disposition': disposition,
                        'credit_value': float(credit_value)
                    },
                    module="STOCK"
                )
                
            return True, f"Return line disposed as: {disposition}"
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def get_returns_dashboard(self, dashboard_filters: Optional[Dict] = None) -> Dict:
        """Get returns processing dashboard"""
        try:
            filters = dashboard_filters or {}
            date_from = filters.get('date_from', int((datetime.now() - timedelta(days=30)).strftime("%Y%m%d")))
            date_to = filters.get('date_to', int(datetime.now().strftime("%Y%m%d")))
            customer = filters.get('customer')
            
            # Get return data
            return_query = self.db.query(ReturnRec).filter(
                and_(
                    ReturnRec.return_received_date >= date_from,
                    ReturnRec.return_received_date <= date_to
                )
            )
            
            returns = return_query.all()
            
            # Get RMA data
            rma_query = self.db.query(ReturnAuthorizationRec).filter(
                and_(
                    ReturnAuthorizationRec.rma_authorized_date >= date_from,
                    ReturnAuthorizationRec.rma_authorized_date <= date_to
                )
            )
            
            if customer:
                rma_query = rma_query.filter(ReturnAuthorizationRec.rma_customer_code == customer)
                
            rmas = rma_query.all()
            
            # Calculate metrics
            total_returns = len(returns)
            total_rmas = len(rmas)
            total_return_value = sum(float(r.return_total_value) for r in returns)
            
            # Status breakdown
            rma_status_counts = {}
            for rma in rmas:
                status = rma.rma_status
                rma_status_counts[status] = rma_status_counts.get(status, 0) + 1
                
            # Return reasons analysis
            return_lines = self.db.query(ReturnLineRec).join(ReturnRec).filter(
                and_(
                    ReturnRec.return_received_date >= date_from,
                    ReturnRec.return_received_date <= date_to
                )
            ).all()
            
            reason_analysis = {}
            condition_analysis = {}
            
            for line in return_lines:
                # Reason analysis
                reason = getattr(line, 'line_reason', 'UNKNOWN')
                reason_analysis[reason] = reason_analysis.get(reason, 0) + 1
                
                # Condition analysis  
                condition = line.line_condition
                condition_analysis[condition] = condition_analysis.get(condition, 0) + 1
                
            # Top returned items
            returned_items = {}
            for line in return_lines:
                stock_code = line.line_stock_code
                if stock_code not in returned_items:
                    returned_items[stock_code] = {
                        'quantity': 0,
                        'value': 0,
                        'returns': 0
                    }
                returned_items[stock_code]['quantity'] += float(line.line_quantity_returned)
                returned_items[stock_code]['value'] += float(line.line_line_value)
                returned_items[stock_code]['returns'] += 1
                
            top_returned = sorted(
                returned_items.items(),
                key=lambda x: x[1]['value'],
                reverse=True
            )[:10]
            
            # Processing efficiency
            processed_lines = len([line for line in return_lines if line.line_status == 'PROCESSED'])
            pending_lines = len([line for line in return_lines if line.line_status == 'RECEIVED'])
            
            processing_efficiency = (processed_lines / len(return_lines) * 100) if return_lines else 100
            
            return {
                'period': {
                    'date_from': date_from,
                    'date_to': date_to,
                    'customer_filter': customer
                },
                'summary': {
                    'total_returns': total_returns,
                    'total_rmas': total_rmas,
                    'total_return_value': total_return_value,
                    'avg_return_value': total_return_value / total_returns if total_returns > 0 else 0,
                    'processing_efficiency': processing_efficiency
                },
                'rma_status_breakdown': rma_status_counts,
                'return_analysis': {
                    'by_reason': reason_analysis,
                    'by_condition': condition_analysis
                },
                'top_returned_items': [
                    {
                        'stock_code': item[0],
                        'quantity': item[1]['quantity'],
                        'value': item[1]['value'],
                        'returns': item[1]['returns']
                    }
                    for item in top_returned
                ],
                'processing_status': {
                    'processed_lines': processed_lines,
                    'pending_lines': pending_lines,
                    'total_lines': len(return_lines)
                },
                'trends': self._calculate_return_trends(returns, rmas)
            }
            
        except Exception as e:
            return {"error": str(e)}
            
    def get_pending_dispositions(self, warehouse: str = None) -> List[Dict]:
        """Get return lines pending disposition"""
        query = self.db.query(ReturnLineRec).join(ReturnRec).filter(
            and_(
                ReturnLineRec.line_status == 'RECEIVED',
                ReturnLineRec.line_disposition == 'PENDING'
            )
        )
        
        if warehouse:
            query = query.filter(ReturnRec.return_warehouse == warehouse)
            
        pending_lines = query.order_by(ReturnRec.return_received_date).all()
        
        results = []
        for line in pending_lines:
            # Get stock details
            stock, _ = self.stock_handler.process(4, key_value=line.line_stock_code)
            
            # Get return details
            return_rec = self.db.query(ReturnRec).filter(
                ReturnRec.return_no == line.line_return_no
            ).first()
            
            results.append({
                'return_no': line.line_return_no,
                'line_seq': line.line_seq,
                'rma_no': return_rec.return_rma_no if return_rec else '',
                'stock_code': line.line_stock_code,
                'description': stock.stock_desc if stock else '',
                'quantity': float(line.line_quantity_returned),
                'condition': line.line_condition,
                'serial_number': line.line_serial_number,
                'lot_number': line.line_lot_number,
                'line_value': float(line.line_line_value),
                'received_date': return_rec.return_received_date if return_rec else 0,
                'age_days': (datetime.now() - datetime.strptime(str(return_rec.return_received_date), "%Y%m%d")).days if return_rec and return_rec.return_received_date else 0,
                'notes': line.line_notes
            })
            
        return results
        
    def _update_serial_return_status(self, stock_code: str, serial_number: str, condition: str):
        """Update serial number return status"""
        serial = self.db.query(SerialNumberRec).filter(
            and_(
                SerialNumberRec.serial_stock_code == stock_code,
                SerialNumberRec.serial_number == serial_number
            )
        ).first()
        
        if serial:
            if condition == 'GOOD':
                serial.serial_status = 'RETURNED'
            elif condition in ['DAMAGED', 'DEFECTIVE']:
                serial.serial_status = 'QUARANTINE'
            else:
                serial.serial_status = 'INSPECTION'
                
            serial.serial_returned_date = int(datetime.now().strftime("%Y%m%d"))
            
    def _update_lot_return_status(self, stock_code: str, lot_number: str, condition: str):
        """Update lot return status"""
        lot = self.db.query(LotNumberRec).filter(
            and_(
                LotNumberRec.lot_stock_code == stock_code,
                LotNumberRec.lot_number == lot_number
            )
        ).first()
        
        if lot:
            if condition == 'GOOD':
                lot.lot_status = 'AVAILABLE'
                lot.lot_quality_status = 'APPROVED'
            elif condition in ['DAMAGED', 'DEFECTIVE']:
                lot.lot_status = 'QUARANTINE'
                lot.lot_quality_status = 'PENDING'
                
    def _calculate_return_trends(self, returns: List[ReturnRec], rmas: List[ReturnAuthorizationRec]) -> Dict:
        """Calculate return trends over time"""
        if not returns and not rmas:
            return {}
            
        # Weekly return trends
        weekly_returns = {}
        weekly_values = {}
        
        for ret in returns:
            if ret.return_received_date:
                return_date = datetime.strptime(str(ret.return_received_date), "%Y%m%d")
                week_start = return_date - timedelta(days=return_date.weekday())
                week_key = week_start.strftime("%Y-%W")
                
                weekly_returns[week_key] = weekly_returns.get(week_key, 0) + 1
                weekly_values[week_key] = weekly_values.get(week_key, 0) + float(ret.return_total_value)
                
        return {
            'weekly_return_counts': weekly_returns,
            'weekly_return_values': weekly_values,
            'trend_direction': self._determine_trend_direction(weekly_returns)
        }
        
    def _determine_trend_direction(self, weekly_data: Dict) -> str:
        """Determine trend direction"""
        weeks = sorted(weekly_data.keys())
        if len(weeks) < 2:
            return 'STABLE'
            
        recent = weekly_data[weeks[-1]]
        previous = weekly_data[weeks[-2]] if len(weeks) > 1 else recent
        
        if recent > previous * 1.1:
            return 'INCREASING'
        elif recent < previous * 0.9:
            return 'DECREASING'
        else:
            return 'STABLE'
            
    def _get_next_rma_number(self) -> str:
        """Generate next RMA number"""
        return f"RMA{datetime.now().strftime('%Y%m%d%H%M%S')}"
        
    def _get_next_return_number(self) -> str:
        """Generate next return number"""
        return f"RET{datetime.now().strftime('%Y%m%d%H%M%S')}"