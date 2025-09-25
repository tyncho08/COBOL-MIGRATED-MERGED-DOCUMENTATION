"""
Serialized Stock Management Service - ST170 migration
Handles serial number tracking and management
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from datetime import datetime
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func, desc

from app.services.file_handlers.stock_handler import StockFileHandler
from app.services.file_handlers.system_handler import SystemFileHandler
from app.models.stock import (
    StockMasterRec, SerialNumberRec, SerialMovementRec,
    SerialAllocationRec, SerialHistoryRec, StockLocationRec
)
from app.core.security import log_user_action
from app.models.auth import User


class SerializedStockService:
    """
    Serialized Stock Management functionality
    Implements ST170 - serial number tracking and management
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.stock_handler = StockFileHandler(db)
        self.system_handler = SystemFileHandler(db)
        
    def register_serial_numbers(self, serial_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Register serial numbers for received stock
        Returns (success, error_message or serial_data)
        """
        stock_code = serial_data.get('stock_code')
        warehouse = serial_data.get('warehouse', 'MAIN')
        location = serial_data.get('location', '')
        serial_numbers = serial_data.get('serial_numbers', [])
        receipt_reference = serial_data.get('receipt_reference', '')
        supplier = serial_data.get('supplier', '')
        unit_cost = Decimal(str(serial_data.get('unit_cost', 0)))
        
        if not serial_numbers:
            return False, "Serial numbers required"
            
        # Validate stock item is serialized
        stock, _ = self.stock_handler.process(4, key_value=stock_code)
        if not stock:
            return False, "Stock item not found"
            
        if stock.stock_serialized != 'Y':
            return False, f"Stock item {stock_code} is not configured for serialization"
            
        try:
            registered_count = 0
            duplicate_serials = []
            
            for serial_no in serial_numbers:
                # Check if serial already exists
                existing = self.db.query(SerialNumberRec).filter(
                    and_(
                        SerialNumberRec.serial_stock_code == stock_code,
                        SerialNumberRec.serial_number == serial_no
                    )
                ).first()
                
                if existing:
                    duplicate_serials.append(serial_no)
                    continue
                    
                # Create serial record
                serial_record = SerialNumberRec(
                    serial_stock_code=stock_code,
                    serial_number=serial_no,
                    serial_warehouse=warehouse,
                    serial_location=location,
                    serial_status='AVAILABLE',
                    serial_supplier=supplier,
                    serial_unit_cost=unit_cost,
                    serial_received_date=int(datetime.now().strftime("%Y%m%d")),
                    serial_received_reference=receipt_reference,
                    serial_last_movement_date=int(datetime.now().strftime("%Y%m%d")),
                    serial_created_by=self.current_user.username if self.current_user else 'SYSTEM'
                )
                
                self.db.add(serial_record)
                
                # Create movement history
                self._create_serial_movement(
                    stock_code, serial_no, 'RECEIPT', warehouse, location,
                    receipt_reference, 'Serial number registered'
                )
                
                registered_count += 1
                
            self.db.commit()
            
            # Log registration
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="REGISTER_SERIAL_NUMBERS",
                    table="serial_number_rec",
                    key=f"{stock_code}-BATCH",
                    new_values={
                        'stock_code': stock_code,
                        'registered': registered_count,
                        'duplicates': len(duplicate_serials)
                    },
                    module="STOCK"
                )
                
            result_msg = f"{registered_count} serial numbers registered"
            if duplicate_serials:
                result_msg += f", {len(duplicate_serials)} duplicates skipped: {duplicate_serials[:5]}"
                
            return True, {
                'registered_count': registered_count,
                'duplicate_count': len(duplicate_serials),
                'duplicates': duplicate_serials[:10],  # Return first 10 duplicates
                'message': result_msg
            }
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def allocate_serial_numbers(self, allocation_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Allocate specific serial numbers to sales orders
        Returns (success, error_message)
        """
        stock_code = allocation_data.get('stock_code')
        serial_numbers = allocation_data.get('serial_numbers', [])
        reference_type = allocation_data.get('reference_type', 'SO')
        reference_no = allocation_data.get('reference_no')
        reference_line = allocation_data.get('reference_line', 1)
        
        if not serial_numbers or not reference_no:
            return False, "Serial numbers and reference required"
            
        try:
            allocated_count = 0
            unavailable_serials = []
            
            for serial_no in serial_numbers:
                # Get serial record
                serial_rec = self.db.query(SerialNumberRec).filter(
                    and_(
                        SerialNumberRec.serial_stock_code == stock_code,
                        SerialNumberRec.serial_number == serial_no,
                        SerialNumberRec.serial_status == 'AVAILABLE'
                    )
                ).first()
                
                if not serial_rec:
                    unavailable_serials.append(serial_no)
                    continue
                    
                # Update serial status
                serial_rec.serial_status = 'ALLOCATED'
                serial_rec.serial_allocated_reference = reference_no
                serial_rec.serial_allocated_date = int(datetime.now().strftime("%Y%m%d"))
                
                # Create allocation record
                allocation = SerialAllocationRec(
                    alloc_stock_code=stock_code,
                    alloc_serial_number=serial_no,
                    alloc_reference_type=reference_type,
                    alloc_reference_no=reference_no,
                    alloc_reference_line=reference_line,
                    alloc_warehouse=serial_rec.serial_warehouse,
                    alloc_location=serial_rec.serial_location,
                    alloc_allocated_date=int(datetime.now().strftime("%Y%m%d")),
                    alloc_allocated_by=self.current_user.username if self.current_user else 'SYSTEM',
                    alloc_status='ALLOCATED'
                )
                
                self.db.add(allocation)
                
                # Create movement history
                self._create_serial_movement(
                    stock_code, serial_no, 'ALLOCATION', 
                    serial_rec.serial_warehouse, serial_rec.serial_location,
                    reference_no, f"Allocated to {reference_type} {reference_no}"
                )
                
                allocated_count += 1
                
            self.db.commit()
            
            result_msg = f"{allocated_count} serial numbers allocated"
            if unavailable_serials:
                result_msg += f", {len(unavailable_serials)} unavailable: {unavailable_serials[:5]}"
                
            return True, result_msg
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def ship_serial_numbers(self, shipment_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Ship allocated serial numbers
        Returns (success, error_message)
        """
        reference_type = shipment_data.get('reference_type', 'SO')
        reference_no = shipment_data.get('reference_no')
        shipment_reference = shipment_data.get('shipment_reference', '')
        customer = shipment_data.get('customer', '')
        
        # Get allocated serials for reference
        allocations = self.db.query(SerialAllocationRec).filter(
            and_(
                SerialAllocationRec.alloc_reference_type == reference_type,
                SerialAllocationRec.alloc_reference_no == reference_no,
                SerialAllocationRec.alloc_status == 'ALLOCATED'
            )
        ).all()
        
        if not allocations:
            return False, f"No allocated serial numbers found for {reference_type} {reference_no}"
            
        try:
            shipped_count = 0
            
            for allocation in allocations:
                # Update serial record
                serial_rec = self.db.query(SerialNumberRec).filter(
                    and_(
                        SerialNumberRec.serial_stock_code == allocation.alloc_stock_code,
                        SerialNumberRec.serial_number == allocation.alloc_serial_number
                    )
                ).first()
                
                if serial_rec:
                    serial_rec.serial_status = 'SHIPPED'
                    serial_rec.serial_customer = customer
                    serial_rec.serial_shipped_date = int(datetime.now().strftime("%Y%m%d"))
                    serial_rec.serial_shipped_reference = shipment_reference
                    serial_rec.serial_last_movement_date = int(datetime.now().strftime("%Y%m%d"))
                    
                    # Update allocation
                    allocation.alloc_status = 'SHIPPED'
                    allocation.alloc_shipped_date = int(datetime.now().strftime("%Y%m%d"))
                    
                    # Create movement history
                    self._create_serial_movement(
                        allocation.alloc_stock_code, allocation.alloc_serial_number,
                        'SHIPMENT', allocation.alloc_warehouse, allocation.alloc_location,
                        shipment_reference, f"Shipped to customer {customer}"
                    )
                    
                    shipped_count += 1
                    
            self.db.commit()
            
            return True, f"{shipped_count} serial numbers shipped"
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def return_serial_numbers(self, return_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Process returned serial numbers
        Returns (success, error_message)
        """
        stock_code = return_data.get('stock_code')
        serial_numbers = return_data.get('serial_numbers', [])
        warehouse = return_data.get('warehouse', 'MAIN')
        location = return_data.get('location', 'RETURNS')
        return_reason = return_data.get('reason', '')
        return_reference = return_data.get('return_reference', '')
        condition = return_data.get('condition', 'GOOD')  # GOOD, DAMAGED, DEFECTIVE
        
        try:
            returned_count = 0
            not_found_serials = []
            
            for serial_no in serial_numbers:
                # Get serial record
                serial_rec = self.db.query(SerialNumberRec).filter(
                    and_(
                        SerialNumberRec.serial_stock_code == stock_code,
                        SerialNumberRec.serial_number == serial_no
                    )
                ).first()
                
                if not serial_rec:
                    not_found_serials.append(serial_no)
                    continue
                    
                # Update serial record
                previous_status = serial_rec.serial_status
                serial_rec.serial_status = 'RETURNED' if condition == 'GOOD' else 'QUARANTINE'
                serial_rec.serial_warehouse = warehouse
                serial_rec.serial_location = location
                serial_rec.serial_condition = condition
                serial_rec.serial_return_reason = return_reason
                serial_rec.serial_returned_date = int(datetime.now().strftime("%Y%m%d"))
                serial_rec.serial_return_reference = return_reference
                serial_rec.serial_last_movement_date = int(datetime.now().strftime("%Y%m%d"))
                
                # Clear allocation if exists
                if serial_rec.serial_allocated_reference:
                    serial_rec.serial_allocated_reference = ''
                    serial_rec.serial_allocated_date = 0
                    
                # Create movement history
                self._create_serial_movement(
                    stock_code, serial_no, 'RETURN', warehouse, location,
                    return_reference, f"Returned: {return_reason} - Condition: {condition}"
                )
                
                returned_count += 1
                
            self.db.commit()
            
            result_msg = f"{returned_count} serial numbers returned"
            if not_found_serials:
                result_msg += f", {len(not_found_serials)} not found: {not_found_serials[:5]}"
                
            return True, result_msg
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def get_serial_history(self, stock_code: str, serial_number: str) -> Dict:
        """Get complete history for a serial number"""
        try:
            # Get serial record
            serial_rec = self.db.query(SerialNumberRec).filter(
                and_(
                    SerialNumberRec.serial_stock_code == stock_code,
                    SerialNumberRec.serial_number == serial_number
                )
            ).first()
            
            if not serial_rec:
                return {"error": "Serial number not found"}
                
            # Get movement history
            movements = self.db.query(SerialMovementRec).filter(
                and_(
                    SerialMovementRec.move_stock_code == stock_code,
                    SerialMovementRec.move_serial_number == serial_number
                )
            ).order_by(SerialMovementRec.move_date, SerialMovementRec.move_time).all()
            
            # Get current allocation if any
            current_allocation = self.db.query(SerialAllocationRec).filter(
                and_(
                    SerialAllocationRec.alloc_stock_code == stock_code,
                    SerialAllocationRec.alloc_serial_number == serial_number,
                    SerialAllocationRec.alloc_status.in_(['ALLOCATED', 'SHIPPED'])
                )
            ).first()
            
            return {
                'serial_number': serial_number,
                'stock_code': stock_code,
                'current_status': {
                    'status': serial_rec.serial_status,
                    'warehouse': serial_rec.serial_warehouse,
                    'location': serial_rec.serial_location,
                    'condition': getattr(serial_rec, 'serial_condition', 'GOOD'),
                    'supplier': serial_rec.serial_supplier,
                    'customer': serial_rec.serial_customer,
                    'unit_cost': float(serial_rec.serial_unit_cost)
                },
                'current_allocation': {
                    'reference_type': current_allocation.alloc_reference_type if current_allocation else '',
                    'reference_no': current_allocation.alloc_reference_no if current_allocation else '',
                    'allocated_date': current_allocation.alloc_allocated_date if current_allocation else 0
                } if current_allocation else None,
                'movement_history': [
                    {
                        'date': move.move_date,
                        'time': move.move_time,
                        'movement_type': move.move_type,
                        'warehouse': move.move_warehouse,
                        'location': move.move_location,
                        'reference': move.move_reference,
                        'notes': move.move_notes,
                        'user': move.move_user
                    }
                    for move in movements
                ],
                'key_dates': {
                    'received_date': serial_rec.serial_received_date,
                    'allocated_date': serial_rec.serial_allocated_date,
                    'shipped_date': serial_rec.serial_shipped_date,
                    'returned_date': getattr(serial_rec, 'serial_returned_date', 0)
                }
            }
            
        except Exception as e:
            return {"error": str(e)}
            
    def get_serial_availability(self, stock_code: str, warehouse: str = None) -> List[Dict]:
        """Get available serial numbers for stock item"""
        query = self.db.query(SerialNumberRec).filter(
            and_(
                SerialNumberRec.serial_stock_code == stock_code,
                SerialNumberRec.serial_status == 'AVAILABLE'
            )
        )
        
        if warehouse:
            query = query.filter(SerialNumberRec.serial_warehouse == warehouse)
            
        serials = query.order_by(SerialNumberRec.serial_received_date).all()
        
        return [
            {
                'serial_number': serial.serial_number,
                'warehouse': serial.serial_warehouse,
                'location': serial.serial_location,
                'supplier': serial.serial_supplier,
                'unit_cost': float(serial.serial_unit_cost),
                'received_date': serial.serial_received_date,
                'age_days': (datetime.now() - datetime.strptime(str(serial.serial_received_date), "%Y%m%d")).days
            }
            for serial in serials
        ]
        
    def validate_serial_transactions(self, validation_request: Dict) -> Dict:
        """Validate serial number transactions for audit"""
        try:
            stock_code = validation_request.get('stock_code')
            date_from = validation_request.get('date_from')
            date_to = validation_request.get('date_to')
            
            # Build query
            query = self.db.query(SerialMovementRec)
            
            if stock_code:
                query = query.filter(SerialMovementRec.move_stock_code == stock_code)
            if date_from:
                query = query.filter(SerialMovementRec.move_date >= date_from)
            if date_to:
                query = query.filter(SerialMovementRec.move_date <= date_to)
                
            movements = query.all()
            
            # Validation checks
            validation_results = {
                'total_movements': len(movements),
                'validation_errors': [],
                'summary': {
                    'receipts': 0,
                    'allocations': 0,
                    'shipments': 0,
                    'returns': 0
                }
            }
            
            serial_states = {}
            
            for movement in movements:
                serial_key = f"{movement.move_stock_code}-{movement.move_serial_number}"
                
                if serial_key not in serial_states:
                    serial_states[serial_key] = []
                    
                serial_states[serial_key].append(movement)
                
                # Count by type
                if movement.move_type == 'RECEIPT':
                    validation_results['summary']['receipts'] += 1
                elif movement.move_type == 'ALLOCATION':
                    validation_results['summary']['allocations'] += 1
                elif movement.move_type == 'SHIPMENT':
                    validation_results['summary']['shipments'] += 1
                elif movement.move_type == 'RETURN':
                    validation_results['summary']['returns'] += 1
                    
            # Validate serial state transitions
            for serial_key, serial_movements in serial_states.items():
                sorted_movements = sorted(serial_movements, key=lambda x: (x.move_date, x.move_time))
                
                # Check for invalid state transitions
                prev_type = None
                for movement in sorted_movements:
                    if prev_type:
                        if not self._is_valid_transition(prev_type, movement.move_type):
                            validation_results['validation_errors'].append({
                                'serial': movement.move_serial_number,
                                'error': f"Invalid transition from {prev_type} to {movement.move_type}",
                                'date': movement.move_date
                            })
                    prev_type = movement.move_type
                    
            validation_results['errors_found'] = len(validation_results['validation_errors'])
            return validation_results
            
        except Exception as e:
            return {"error": str(e)}
            
    def _create_serial_movement(self, stock_code: str, serial_number: str, 
                              movement_type: str, warehouse: str, location: str,
                              reference: str, notes: str):
        """Create serial movement history record"""
        movement = SerialMovementRec(
            move_stock_code=stock_code,
            move_serial_number=serial_number,
            move_type=movement_type,
            move_warehouse=warehouse,
            move_location=location,
            move_reference=reference,
            move_notes=notes,
            move_date=int(datetime.now().strftime("%Y%m%d")),
            move_time=int(datetime.now().strftime("%H%M%S")),
            move_user=self.current_user.username if self.current_user else 'SYSTEM'
        )
        
        self.db.add(movement)
        
    def _is_valid_transition(self, from_type: str, to_type: str) -> bool:
        """Check if state transition is valid"""
        valid_transitions = {
            'RECEIPT': ['ALLOCATION', 'RETURN'],
            'ALLOCATION': ['SHIPMENT', 'RETURN'],
            'SHIPMENT': ['RETURN'],
            'RETURN': ['ALLOCATION']
        }
        
        return to_type in valid_transitions.get(from_type, [])