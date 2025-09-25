"""
Lot/Batch Tracking Service - ST180 migration
Handles lot and batch number tracking for stock items
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from datetime import datetime, timedelta
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func, desc

from app.services.file_handlers.stock_handler import StockFileHandler
from app.services.file_handlers.system_handler import SystemFileHandler
from app.models.stock import (
    StockMasterRec, LotNumberRec, LotMovementRec, LotAllocationRec,
    StockLocationRec, ExpiryDateRec, QualityTestRec
)
from app.core.security import log_user_action
from app.models.auth import User


class LotTrackingService:
    """
    Lot/Batch Tracking functionality
    Implements ST180 - lot and batch number tracking
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.stock_handler = StockFileHandler(db)
        self.system_handler = SystemFileHandler(db)
        
    def create_lot(self, lot_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Create new lot/batch
        Returns (success, error_message or lot_data)
        """
        stock_code = lot_data.get('stock_code')
        lot_number = lot_data.get('lot_number')
        warehouse = lot_data.get('warehouse', 'MAIN')
        location = lot_data.get('location', '')
        quantity = Decimal(str(lot_data.get('quantity', 0)))
        unit_cost = Decimal(str(lot_data.get('unit_cost', 0)))
        supplier = lot_data.get('supplier', '')
        manufacture_date = lot_data.get('manufacture_date', 0)
        expiry_date = lot_data.get('expiry_date', 0)
        supplier_lot = lot_data.get('supplier_lot', '')
        receipt_reference = lot_data.get('receipt_reference', '')
        
        # Validate stock item supports lot tracking
        stock, _ = self.stock_handler.process(4, key_value=stock_code)
        if not stock:
            return False, "Stock item not found"
            
        if stock.stock_lot_tracked != 'Y':
            return False, f"Stock item {stock_code} is not configured for lot tracking"
            
        # Check if lot already exists
        existing = self.db.query(LotNumberRec).filter(
            and_(
                LotNumberRec.lot_stock_code == stock_code,
                LotNumberRec.lot_number == lot_number,
                LotNumberRec.lot_warehouse == warehouse
            )
        ).first()
        
        if existing:
            return False, f"Lot {lot_number} already exists for {stock_code} in {warehouse}"
            
        try:
            # Create lot record
            lot_record = LotNumberRec(
                lot_stock_code=stock_code,
                lot_number=lot_number,
                lot_warehouse=warehouse,
                lot_location=location,
                lot_quantity_on_hand=quantity,
                lot_quantity_allocated=Decimal('0'),
                lot_quantity_available=quantity,
                lot_unit_cost=unit_cost,
                lot_total_value=quantity * unit_cost,
                lot_supplier=supplier,
                lot_supplier_lot=supplier_lot,
                lot_manufacture_date=manufacture_date,
                lot_expiry_date=expiry_date,
                lot_received_date=int(datetime.now().strftime("%Y%m%d")),
                lot_received_reference=receipt_reference,
                lot_status='AVAILABLE',
                lot_quality_status='APPROVED',
                lot_created_by=self.current_user.username if self.current_user else 'SYSTEM'
            )
            
            self.db.add(lot_record)
            self.db.flush()
            
            # Create initial movement record
            self._create_lot_movement(
                stock_code, lot_number, 'RECEIPT', warehouse, location,
                quantity, Decimal('0'), receipt_reference, 'Lot created'
            )
            
            # Create expiry tracking if applicable
            if expiry_date > 0:
                self._create_expiry_tracking(stock_code, lot_number, expiry_date, quantity)
                
            self.db.commit()
            
            # Log creation
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="CREATE_LOT",
                    table="lot_number_rec",
                    key=f"{stock_code}-{lot_number}",
                    new_values={
                        'stock_code': stock_code,
                        'lot_number': lot_number,
                        'quantity': float(quantity),
                        'warehouse': warehouse
                    },
                    module="STOCK"
                )
                
            return True, {
                'stock_code': stock_code,
                'lot_number': lot_number,
                'quantity': float(quantity),
                'total_value': float(quantity * unit_cost),
                'expiry_date': expiry_date
            }
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def allocate_lot(self, allocation_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Allocate lot quantity to sales order
        Returns (success, error_message)
        """
        stock_code = allocation_data.get('stock_code')
        lot_number = allocation_data.get('lot_number')
        quantity = Decimal(str(allocation_data.get('quantity', 0)))
        reference_type = allocation_data.get('reference_type', 'SO')
        reference_no = allocation_data.get('reference_no')
        reference_line = allocation_data.get('reference_line', 1)
        
        # Get lot record
        lot = self.db.query(LotNumberRec).filter(
            and_(
                LotNumberRec.lot_stock_code == stock_code,
                LotNumberRec.lot_number == lot_number
            )
        ).first()
        
        if not lot:
            return False, f"Lot {lot_number} not found"
            
        if lot.lot_status != 'AVAILABLE':
            return False, f"Lot {lot_number} is not available (status: {lot.lot_status})"
            
        if lot.lot_quantity_available < quantity:
            return False, f"Insufficient lot quantity. Available: {lot.lot_quantity_available}, Requested: {quantity}"
            
        try:
            # Update lot allocation
            lot.lot_quantity_allocated += quantity
            lot.lot_quantity_available -= quantity
            
            # Create allocation record
            allocation = LotAllocationRec(
                alloc_stock_code=stock_code,
                alloc_lot_number=lot_number,
                alloc_quantity=quantity,
                alloc_reference_type=reference_type,
                alloc_reference_no=reference_no,
                alloc_reference_line=reference_line,
                alloc_warehouse=lot.lot_warehouse,
                alloc_location=lot.lot_location,
                alloc_unit_cost=lot.lot_unit_cost,
                alloc_allocated_date=int(datetime.now().strftime("%Y%m%d")),
                alloc_allocated_by=self.current_user.username if self.current_user else 'SYSTEM',
                alloc_status='ALLOCATED'
            )
            
            self.db.add(allocation)
            
            # Create movement record
            self._create_lot_movement(
                stock_code, lot_number, 'ALLOCATION', lot.lot_warehouse, lot.lot_location,
                Decimal('0'), quantity, reference_no, f"Allocated to {reference_type} {reference_no}"
            )
            
            self.db.commit()
            
            return True, f"Allocated {quantity} of lot {lot_number} to {reference_type} {reference_no}"
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def issue_lot(self, issue_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Issue lot quantity (shipped or consumed)
        Returns (success, error_message)
        """
        reference_type = issue_data.get('reference_type', 'SO')
        reference_no = issue_data.get('reference_no')
        shipment_reference = issue_data.get('shipment_reference', '')
        
        # Get allocations for reference
        allocations = self.db.query(LotAllocationRec).filter(
            and_(
                LotAllocationRec.alloc_reference_type == reference_type,
                LotAllocationRec.alloc_reference_no == reference_no,
                LotAllocationRec.alloc_status == 'ALLOCATED'
            )
        ).all()
        
        if not allocations:
            return False, f"No lot allocations found for {reference_type} {reference_no}"
            
        try:
            issued_lots = []
            
            for allocation in allocations:
                # Get lot record
                lot = self.db.query(LotNumberRec).filter(
                    and_(
                        LotNumberRec.lot_stock_code == allocation.alloc_stock_code,
                        LotNumberRec.lot_number == allocation.alloc_lot_number
                    )
                ).first()
                
                if lot:
                    # Update lot quantities
                    lot.lot_quantity_allocated -= allocation.alloc_quantity
                    lot.lot_quantity_on_hand -= allocation.alloc_quantity
                    
                    # Update allocation status
                    allocation.alloc_status = 'ISSUED'
                    allocation.alloc_issued_date = int(datetime.now().strftime("%Y%m%d"))
                    allocation.alloc_shipment_reference = shipment_reference
                    
                    # Create movement record
                    self._create_lot_movement(
                        allocation.alloc_stock_code, allocation.alloc_lot_number,
                        'ISSUE', lot.lot_warehouse, lot.lot_location,
                        -allocation.alloc_quantity, -allocation.alloc_quantity,
                        shipment_reference, f"Issued for {reference_type} {reference_no}"
                    )
                    
                    issued_lots.append({
                        'stock_code': allocation.alloc_stock_code,
                        'lot_number': allocation.alloc_lot_number,
                        'quantity': float(allocation.alloc_quantity)
                    })
                    
            self.db.commit()
            
            return True, {
                'issued_lots': issued_lots,
                'total_lots': len(issued_lots),
                'reference': f"{reference_type} {reference_no}"
            }
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def transfer_lot(self, transfer_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Transfer lot between locations
        Returns (success, error_message)
        """
        stock_code = transfer_data.get('stock_code')
        lot_number = transfer_data.get('lot_number')
        quantity = Decimal(str(transfer_data.get('quantity', 0)))
        from_warehouse = transfer_data.get('from_warehouse')
        to_warehouse = transfer_data.get('to_warehouse')
        from_location = transfer_data.get('from_location', '')
        to_location = transfer_data.get('to_location', '')
        reference = transfer_data.get('reference', '')
        
        # Get source lot
        source_lot = self.db.query(LotNumberRec).filter(
            and_(
                LotNumberRec.lot_stock_code == stock_code,
                LotNumberRec.lot_number == lot_number,
                LotNumberRec.lot_warehouse == from_warehouse
            )
        ).first()
        
        if not source_lot:
            return False, f"Source lot {lot_number} not found in {from_warehouse}"
            
        if source_lot.lot_quantity_available < quantity:
            return False, f"Insufficient lot quantity. Available: {source_lot.lot_quantity_available}, Requested: {quantity}"
            
        try:
            if from_warehouse == to_warehouse:
                # Location transfer within same warehouse
                source_lot.lot_location = to_location
                
                # Create movement record
                self._create_lot_movement(
                    stock_code, lot_number, 'TRANSFER', to_warehouse, to_location,
                    Decimal('0'), Decimal('0'), reference, f"Transferred from {from_location} to {to_location}"
                )
                
            else:
                # Inter-warehouse transfer
                
                # Reduce source lot
                source_lot.lot_quantity_on_hand -= quantity
                source_lot.lot_quantity_available -= quantity
                
                # Check if destination lot exists
                dest_lot = self.db.query(LotNumberRec).filter(
                    and_(
                        LotNumberRec.lot_stock_code == stock_code,
                        LotNumberRec.lot_number == lot_number,
                        LotNumberRec.lot_warehouse == to_warehouse
                    )
                ).first()
                
                if dest_lot:
                    # Add to existing lot
                    dest_lot.lot_quantity_on_hand += quantity
                    dest_lot.lot_quantity_available += quantity
                    dest_lot.lot_location = to_location
                else:
                    # Create new lot in destination
                    dest_lot = LotNumberRec(
                        lot_stock_code=stock_code,
                        lot_number=lot_number,
                        lot_warehouse=to_warehouse,
                        lot_location=to_location,
                        lot_quantity_on_hand=quantity,
                        lot_quantity_allocated=Decimal('0'),
                        lot_quantity_available=quantity,
                        lot_unit_cost=source_lot.lot_unit_cost,
                        lot_total_value=quantity * source_lot.lot_unit_cost,
                        lot_supplier=source_lot.lot_supplier,
                        lot_supplier_lot=source_lot.lot_supplier_lot,
                        lot_manufacture_date=source_lot.lot_manufacture_date,
                        lot_expiry_date=source_lot.lot_expiry_date,
                        lot_received_date=int(datetime.now().strftime("%Y%m%d")),
                        lot_received_reference=reference,
                        lot_status='AVAILABLE',
                        lot_quality_status=source_lot.lot_quality_status,
                        lot_created_by=self.current_user.username if self.current_user else 'SYSTEM'
                    )
                    self.db.add(dest_lot)
                    
                # Create movement records
                self._create_lot_movement(
                    stock_code, lot_number, 'TRANSFER_OUT', from_warehouse, from_location,
                    -quantity, Decimal('0'), reference, f"Transferred to {to_warehouse}"
                )
                
                self._create_lot_movement(
                    stock_code, lot_number, 'TRANSFER_IN', to_warehouse, to_location,
                    quantity, Decimal('0'), reference, f"Received from {from_warehouse}"
                )
                
            self.db.commit()
            
            return True, f"Transferred {quantity} of lot {lot_number} from {from_warehouse} to {to_warehouse}"
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def get_expiring_lots(self, expiry_filters: Optional[Dict] = None) -> List[Dict]:
        """Get lots that are expiring or expired"""
        filters = expiry_filters or {}
        warehouse = filters.get('warehouse')
        days_ahead = filters.get('days_ahead', 30)
        
        # Calculate cutoff date
        cutoff_date = int((datetime.now() + timedelta(days=days_ahead)).strftime("%Y%m%d"))
        
        query = self.db.query(LotNumberRec).filter(
            and_(
                LotNumberRec.lot_expiry_date > 0,
                LotNumberRec.lot_expiry_date <= cutoff_date,
                LotNumberRec.lot_quantity_on_hand > 0
            )
        )
        
        if warehouse:
            query = query.filter(LotNumberRec.lot_warehouse == warehouse)
            
        expiring_lots = query.order_by(LotNumberRec.lot_expiry_date).all()
        
        results = []
        for lot in expiring_lots:
            expiry_date = datetime.strptime(str(lot.lot_expiry_date), "%Y%m%d")
            days_to_expiry = (expiry_date - datetime.now()).days
            
            # Get stock details
            stock, _ = self.stock_handler.process(4, key_value=lot.lot_stock_code)
            
            results.append({
                'stock_code': lot.lot_stock_code,
                'description': stock.stock_desc if stock else '',
                'lot_number': lot.lot_number,
                'warehouse': lot.lot_warehouse,
                'location': lot.lot_location,
                'quantity_on_hand': float(lot.lot_quantity_on_hand),
                'quantity_allocated': float(lot.lot_quantity_allocated),
                'quantity_available': float(lot.lot_quantity_available),
                'unit_cost': float(lot.lot_unit_cost),
                'total_value': float(lot.lot_total_value),
                'expiry_date': lot.lot_expiry_date,
                'days_to_expiry': days_to_expiry,
                'status': 'EXPIRED' if days_to_expiry < 0 else 'EXPIRING',
                'supplier': lot.lot_supplier,
                'manufacture_date': lot.lot_manufacture_date
            })
            
        return results
        
    def get_lot_traceability(self, trace_request: Dict) -> Dict:
        """Get complete traceability for lot"""
        try:
            stock_code = trace_request.get('stock_code')
            lot_number = trace_request.get('lot_number')
            trace_direction = trace_request.get('direction', 'FORWARD')  # FORWARD, BACKWARD, BOTH
            
            # Get lot details
            lot = self.db.query(LotNumberRec).filter(
                and_(
                    LotNumberRec.lot_stock_code == stock_code,
                    LotNumberRec.lot_number == lot_number
                )
            ).first()
            
            if not lot:
                return {"error": "Lot not found"}
                
            # Get all movements for this lot
            movements = self.db.query(LotMovementRec).filter(
                and_(
                    LotMovementRec.move_stock_code == stock_code,
                    LotMovementRec.move_lot_number == lot_number
                )
            ).order_by(LotMovementRec.move_date, LotMovementRec.move_time).all()
            
            # Get allocations/issues
            allocations = self.db.query(LotAllocationRec).filter(
                and_(
                    LotAllocationRec.alloc_stock_code == stock_code,
                    LotAllocationRec.alloc_lot_number == lot_number
                )
            ).all()
            
            # Build traceability chain
            traceability = {
                'lot_details': {
                    'stock_code': stock_code,
                    'lot_number': lot_number,
                    'supplier': lot.lot_supplier,
                    'supplier_lot': lot.lot_supplier_lot,
                    'manufacture_date': lot.lot_manufacture_date,
                    'expiry_date': lot.lot_expiry_date,
                    'received_date': lot.lot_received_date,
                    'current_status': lot.lot_status,
                    'quality_status': lot.lot_quality_status
                },
                'receipt_info': {
                    'received_date': lot.lot_received_date,
                    'received_reference': lot.lot_received_reference,
                    'original_quantity': self._get_original_quantity(movements),
                    'supplier': lot.lot_supplier
                },
                'current_status': {
                    'warehouse': lot.lot_warehouse,
                    'location': lot.lot_location,
                    'quantity_on_hand': float(lot.lot_quantity_on_hand),
                    'quantity_allocated': float(lot.lot_quantity_allocated),
                    'quantity_available': float(lot.lot_quantity_available)
                },
                'movement_history': [
                    {
                        'date': move.move_date,
                        'time': move.move_time,
                        'type': move.move_type,
                        'warehouse': move.move_warehouse,
                        'location': move.move_location,
                        'quantity_change': float(move.move_quantity_change),
                        'allocated_change': float(move.move_allocated_change),
                        'reference': move.move_reference,
                        'notes': move.move_notes,
                        'user': move.move_user
                    }
                    for move in movements
                ],
                'allocations_issues': [
                    {
                        'reference_type': alloc.alloc_reference_type,
                        'reference_no': alloc.alloc_reference_no,
                        'reference_line': alloc.alloc_reference_line,
                        'quantity': float(alloc.alloc_quantity),
                        'status': alloc.alloc_status,
                        'allocated_date': alloc.alloc_allocated_date,
                        'issued_date': alloc.alloc_issued_date,
                        'shipment_reference': alloc.alloc_shipment_reference
                    }
                    for alloc in allocations
                ]
            }
            
            # Add forward/backward trace if requested
            if trace_direction in ['FORWARD', 'BOTH']:
                traceability['forward_trace'] = self._trace_forward(stock_code, lot_number)
                
            if trace_direction in ['BACKWARD', 'BOTH']:
                traceability['backward_trace'] = self._trace_backward(stock_code, lot_number)
                
            return traceability
            
        except Exception as e:
            return {"error": str(e)}
            
    def _create_lot_movement(self, stock_code: str, lot_number: str, movement_type: str,
                           warehouse: str, location: str, quantity_change: Decimal,
                           allocated_change: Decimal, reference: str, notes: str):
        """Create lot movement history record"""
        movement = LotMovementRec(
            move_stock_code=stock_code,
            move_lot_number=lot_number,
            move_type=movement_type,
            move_warehouse=warehouse,
            move_location=location,
            move_quantity_change=quantity_change,
            move_allocated_change=allocated_change,
            move_reference=reference,
            move_notes=notes,
            move_date=int(datetime.now().strftime("%Y%m%d")),
            move_time=int(datetime.now().strftime("%H%M%S")),
            move_user=self.current_user.username if self.current_user else 'SYSTEM'
        )
        
        self.db.add(movement)
        
    def _create_expiry_tracking(self, stock_code: str, lot_number: str, expiry_date: int, quantity: Decimal):
        """Create expiry date tracking record"""
        expiry_record = ExpiryDateRec(
            expiry_stock_code=stock_code,
            expiry_lot_number=lot_number,
            expiry_date=expiry_date,
            expiry_quantity=quantity,
            expiry_status='ACTIVE',
            expiry_created_date=int(datetime.now().strftime("%Y%m%d"))
        )
        
        self.db.add(expiry_record)
        
    def _get_original_quantity(self, movements: List[LotMovementRec]) -> float:
        """Get original received quantity from movements"""
        for movement in movements:
            if movement.move_type == 'RECEIPT':
                return float(movement.move_quantity_change)
        return 0.0
        
    def _trace_forward(self, stock_code: str, lot_number: str) -> Dict:
        """Trace lot forward through supply chain"""
        # Get all issues/shipments for this lot
        allocations = self.db.query(LotAllocationRec).filter(
            and_(
                LotAllocationRec.alloc_stock_code == stock_code,
                LotAllocationRec.alloc_lot_number == lot_number,
                LotAllocationRec.alloc_status == 'ISSUED'
            )
        ).all()
        
        forward_trace = []
        for alloc in allocations:
            forward_trace.append({
                'destination_type': alloc.alloc_reference_type,
                'destination_ref': alloc.alloc_reference_no,
                'quantity': float(alloc.alloc_quantity),
                'issued_date': alloc.alloc_issued_date,
                'shipment_ref': alloc.alloc_shipment_reference
            })
            
        return {'forward_destinations': forward_trace}
        
    def _trace_backward(self, stock_code: str, lot_number: str) -> Dict:
        """Trace lot backward through supply chain"""
        # Get lot creation/receipt information
        lot = self.db.query(LotNumberRec).filter(
            and_(
                LotNumberRec.lot_stock_code == stock_code,
                LotNumberRec.lot_number == lot_number
            )
        ).first()
        
        backward_trace = {
            'supplier_info': {
                'supplier': lot.lot_supplier if lot else '',
                'supplier_lot': lot.lot_supplier_lot if lot else '',
                'manufacture_date': lot.lot_manufacture_date if lot else 0,
                'received_date': lot.lot_received_date if lot else 0,
                'received_reference': lot.lot_received_reference if lot else ''
            }
        }
        
        return backward_trace