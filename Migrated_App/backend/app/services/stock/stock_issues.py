"""
Stock Issues Service - ST060 migration
Handles automated stock issue processing for sales orders and production
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from datetime import datetime
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func, desc

from app.services.file_handlers.stock_handler import StockFileHandler
from app.services.file_handlers.customer_handler import CustomerFileHandler
from app.services.file_handlers.system_handler import SystemFileHandler
from app.models.stock import StockMasterRec, StockLocationRec, StockMovementRec
from app.models.sales import SalesOrderRec, SalesOrderLineRec, DeliveryNoteRec, DeliveryNoteLineRec
from app.models.customer import SalesLedgerRec
from app.services.stock.stock_movements import StockMovementsService
from app.core.security import log_user_action
from app.models.auth import User


class StockIssuesService:
    """
    Stock Issues Processing functionality
    Implements ST060 - automated issue processing
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.stock_handler = StockFileHandler(db)
        self.customer_handler = CustomerFileHandler(db)
        self.system_handler = SystemFileHandler(db)
        self.movements_service = StockMovementsService(db, current_user)
        
    def process_delivery_note(self, dn_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Process delivery note and issue stock
        Returns (success, error_message)
        """
        dn_no = dn_data.get('dn_no')
        if not dn_no:
            dn_no = self._get_next_dn_number()
            
        customer_no = dn_data.get('customer_no')
        warehouse = dn_data.get('warehouse', 'MAIN')
        delivery_date = dn_data.get('delivery_date', int(datetime.now().strftime("%Y%m%d")))
        delivery_address = dn_data.get('delivery_address', {})
        carrier = dn_data.get('carrier', '')
        
        # Validate customer
        customer, status = self.customer_handler.process(4, key_value=customer_no)
        if status.fs_reply != "00":
            return False, "Customer not found"
            
        # Validate lines
        lines = dn_data.get('lines', [])
        if not lines:
            return False, "Delivery note must have at least one line"
            
        try:
            # Create delivery note header
            dn = DeliveryNoteRec(
                dn_no=dn_no,
                dn_customer=customer_no,
                dn_date=delivery_date,
                dn_warehouse=warehouse,
                dn_delivery_name=delivery_address.get('name', customer.sales_name),
                dn_delivery_add1=delivery_address.get('address1', customer.sales_add1),
                dn_delivery_add2=delivery_address.get('address2', customer.sales_add2),
                dn_delivery_add3=delivery_address.get('address3', customer.sales_add3),
                dn_delivery_postcode=delivery_address.get('postcode', customer.sales_postcode),
                dn_carrier=carrier,
                dn_status='DRAFT',
                dn_issued='N',
                dn_created_by=self.current_user.username if self.current_user else 'SYSTEM',
                dn_created_date=int(datetime.now().strftime("%Y%m%d")),
                dn_total_lines=len(lines),
                dn_total_qty=Decimal('0'),
                dn_total_weight=Decimal('0')
            )
            
            self.db.add(dn)
            self.db.flush()
            
            # Process each line
            total_qty = Decimal('0')
            total_weight = Decimal('0')
            
            for line_data in lines:
                line, error = self._process_dn_line(dn, line_data)
                if error:
                    self.db.rollback()
                    return False, error
                    
                total_qty += line.dn_line_qty_issued
                total_weight += line.dn_line_weight
                
            # Update DN totals
            dn.dn_total_qty = total_qty
            dn.dn_total_weight = total_weight
            
            self.db.commit()
            
            # Log creation
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="CREATE_DELIVERY_NOTE",
                    table="delivery_note_rec",
                    key=dn_no,
                    new_values={
                        'customer': customer_no,
                        'total_qty': float(total_qty),
                        'warehouse': warehouse
                    },
                    module="STOCK"
                )
                
            return True, f"Delivery note {dn_no} created successfully"
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def _process_dn_line(self, dn: DeliveryNoteRec, line_data: Dict) -> Tuple[DeliveryNoteLineRec, Optional[str]]:
        """Process individual delivery note line"""
        stock_code = line_data.get('stock_code')
        so_no = line_data.get('so_no', '')
        so_line = line_data.get('so_line', 0)
        qty_ordered = Decimal(str(line_data.get('qty_ordered', 0)))
        qty_issued = Decimal(str(line_data.get('qty_issued', 0)))
        location = line_data.get('location', '')  # Empty means auto-allocate
        
        if qty_issued <= 0:
            return None, "Issued quantity must be greater than zero"
            
        # Validate stock item
        stock, status = self.stock_handler.process(4, key_value=stock_code)
        if status.fs_reply != "00":
            return None, f"Stock item {stock_code} not found"
            
        # Check availability
        if not location:
            # Auto-allocate from best locations
            available = self.movements_service._get_available_stock(stock_code, dn.dn_warehouse)
        else:
            # Specific location
            available = self.movements_service._get_available_stock(stock_code, dn.dn_warehouse, location)
            
        if qty_issued > available:
            return None, f"Insufficient stock for {stock_code}. Available: {available}, Required: {qty_issued}"
            
        # Get next line number
        line_no = self.db.query(func.count(DeliveryNoteLineRec.dn_line_id)).filter(
            DeliveryNoteLineRec.dn_line_dn_no == dn.dn_no
        ).scalar() + 1
        
        # Calculate weight
        line_weight = qty_issued * stock.stock_weight
        
        # Create DN line
        dn_line = DeliveryNoteLineRec(
            dn_line_dn_no=dn.dn_no,
            dn_line_no=line_no,
            dn_line_stock_code=stock_code,
            dn_line_description=stock.stock_desc,
            dn_line_so_no=so_no,
            dn_line_so_line=so_line,
            dn_line_qty_ordered=qty_ordered,
            dn_line_qty_issued=qty_issued,
            dn_line_location=location,
            dn_line_weight=line_weight,
            dn_line_unit=stock.stock_unit,
            dn_line_status='PREPARED'
        )
        
        self.db.add(dn_line)
        self.db.flush()
        
        return dn_line, None
        
    def issue_delivery_note(self, dn_no: str) -> Tuple[bool, Optional[str]]:
        """
        Issue delivery note and update stock quantities
        Returns (success, error_message)
        """
        # Get delivery note
        dn = self.db.query(DeliveryNoteRec).filter(
            DeliveryNoteRec.dn_no == dn_no
        ).first()
        
        if not dn:
            return False, "Delivery note not found"
            
        if dn.dn_issued == 'Y':
            return False, "Delivery note already issued"
            
        try:
            # Get DN lines
            dn_lines = self.db.query(DeliveryNoteLineRec).filter(
                DeliveryNoteLineRec.dn_line_dn_no == dn_no
            ).all()
            
            # Process each line
            for line in dn_lines:
                # Create stock issue movement
                issue_data = {
                    'stock_code': line.dn_line_stock_code,
                    'warehouse': dn.dn_warehouse,
                    'location': line.dn_line_location if line.dn_line_location else None,
                    'quantity': float(line.dn_line_qty_issued),
                    'reference': f"DN-{dn_no}",
                    'cost_method': 'AVERAGE'
                }
                
                success, error = self.movements_service.process_issue(issue_data)
                if not success:
                    self.db.rollback()
                    return False, f"Failed to issue line {line.dn_line_no}: {error}"
                    
                # Update SO line if linked
                if line.dn_line_so_no:
                    self._update_so_line_issue(line)
                    
                # Update line status
                line.dn_line_status = 'ISSUED'
                
            # Mark DN as issued
            dn.dn_issued = 'Y'
            dn.dn_issued_date = int(datetime.now().strftime("%Y%m%d"))
            dn.dn_issued_by = self.current_user.username if self.current_user else 'SYSTEM'
            dn.dn_status = 'ISSUED'
            
            self.db.commit()
            
            # Log issue
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="ISSUE_DELIVERY_NOTE",
                    table="delivery_note_rec",
                    key=dn_no,
                    new_values={'total_qty': float(dn.dn_total_qty)},
                    module="STOCK"
                )
                
            return True, f"Delivery note {dn_no} issued successfully"
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def process_sales_order_issue(self, so_issue_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Process issue directly from sales order
        Returns (success, error_message)
        """
        so_no = so_issue_data.get('so_no')
        warehouse = so_issue_data.get('warehouse', 'MAIN')
        partial_issue = so_issue_data.get('partial_issue', False)
        issue_lines = so_issue_data.get('lines', [])
        
        # Get sales order
        so = self.db.query(SalesOrderRec).filter(
            SalesOrderRec.so_no == so_no
        ).first()
        
        if not so:
            return False, "Sales order not found"
            
        if so.so_status not in ['OPEN', 'PARTIAL', 'ALLOCATED']:
            return False, f"Cannot issue against SO with status: {so.so_status}"
            
        try:
            # Create DN from SO
            dn_data = {
                'customer_no': so.so_customer,
                'warehouse': warehouse,
                'delivery_date': int(datetime.now().strftime("%Y%m%d")),
                'delivery_address': {
                    'name': so.so_del_name,
                    'address1': so.so_del_add1,
                    'address2': so.so_del_add2,
                    'address3': so.so_del_add3,
                    'postcode': so.so_del_postcode
                },
                'carrier': so_issue_data.get('carrier', ''),
                'lines': []
            }
            
            # Get SO lines
            so_lines = self.db.query(SalesOrderLineRec).filter(
                SalesOrderLineRec.sol_so_no == so_no
            ).all()
            
            # Process issue lines
            for issue_line in issue_lines:
                so_line_no = issue_line.get('so_line_no')
                qty_issued = Decimal(str(issue_line.get('qty_issued', 0)))
                
                # Find matching SO line
                so_line = next((sol for sol in so_lines if sol.sol_line_no == so_line_no), None)
                if not so_line:
                    return False, f"SO line {so_line_no} not found"
                    
                if qty_issued <= 0:
                    continue
                    
                # Check quantity not exceeding outstanding
                if qty_issued > so_line.sol_outstanding:
                    if not so_issue_data.get('allow_over_issue', False):
                        return False, f"Line {so_line_no}: Issued quantity exceeds outstanding"
                        
                # Add to DN lines
                dn_line = {
                    'stock_code': so_line.sol_stock_code,
                    'so_no': so_no,
                    'so_line': so_line_no,
                    'qty_ordered': float(so_line.sol_quantity),
                    'qty_issued': float(qty_issued),
                    'location': issue_line.get('location', '')
                }
                
                dn_data['lines'].append(dn_line)
                
            if not dn_data['lines']:
                return False, "No lines to issue"
                
            # Process DN
            success, error = self.process_delivery_note(dn_data)
            if not success:
                return False, error
                
            # Auto-issue if configured
            system_rec, _ = self.system_handler.read_system_params()
            auto_issue = system_rec and system_rec.dn_auto_issue == 'Y'
            
            if auto_issue:
                # Get the DN number from the created DN
                latest_dn = self.db.query(DeliveryNoteRec).filter(
                    DeliveryNoteRec.dn_customer == so.so_customer
                ).order_by(DeliveryNoteRec.dn_created_date.desc()).first()
                
                if latest_dn:
                    issue_success, issue_error = self.issue_delivery_note(latest_dn.dn_no)
                    if not issue_success:
                        return False, f"DN created but issue failed: {issue_error}"
                        
            return True, "Sales order issue processed successfully"
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def process_production_issue(self, production_issue_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Process material issue to production
        Returns (success, error_message)
        """
        work_order = production_issue_data.get('work_order')
        warehouse = production_issue_data.get('warehouse', 'MAIN')
        issue_date = production_issue_data.get('issue_date', int(datetime.now().strftime("%Y%m%d")))
        materials = production_issue_data.get('materials', [])
        
        if not materials:
            return False, "Production issue must have materials"
            
        try:
            issue_no = self._get_next_production_issue_number()
            
            # Process each material
            for material in materials:
                stock_code = material.get('stock_code')
                qty_required = Decimal(str(material.get('qty_required', 0)))
                qty_issued = Decimal(str(material.get('qty_issued', qty_required)))
                location = material.get('location', '')
                
                if qty_issued <= 0:
                    continue
                    
                # Check availability
                if location:
                    available = self.movements_service._get_available_stock(stock_code, warehouse, location)
                else:
                    available = self.movements_service._get_available_stock(stock_code, warehouse)
                    
                if qty_issued > available:
                    return False, f"Insufficient stock for {stock_code}. Available: {available}"
                    
                # Create production issue
                issue_data = {
                    'stock_code': stock_code,
                    'warehouse': warehouse,
                    'location': location if location else None,
                    'quantity': float(qty_issued),
                    'reference': f"PROD-{issue_no}-{work_order}",
                    'cost_method': 'AVERAGE'
                }
                
                success, error = self.movements_service.process_issue(issue_data)
                if not success:
                    self.db.rollback()
                    return False, f"Failed to issue {stock_code}: {error}"
                    
            self.db.commit()
            
            # Log production issue
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="PRODUCTION_ISSUE",
                    table="stock_movement_rec",
                    key=issue_no,
                    new_values={
                        'work_order': work_order,
                        'warehouse': warehouse,
                        'materials': len(materials)
                    },
                    module="STOCK"
                )
                
            return True, f"Production issue {issue_no} processed successfully"
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def process_miscellaneous_issue(self, misc_issue_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Process miscellaneous stock issue
        Returns (success, error_message)
        """
        warehouse = misc_issue_data.get('warehouse', 'MAIN')
        reason = misc_issue_data.get('reason', 'MISCELLANEOUS')
        reference = misc_issue_data.get('reference', '')
        cost_center = misc_issue_data.get('cost_center', '')
        items = misc_issue_data.get('items', [])
        
        if not items:
            return False, "Miscellaneous issue must have items"
            
        try:
            issue_no = self._get_next_misc_issue_number()
            
            # Process each item
            for item in items:
                stock_code = item.get('stock_code')
                quantity = Decimal(str(item.get('quantity', 0)))
                location = item.get('location', '')
                
                if quantity <= 0:
                    continue
                    
                # Check availability
                if location:
                    available = self.movements_service._get_available_stock(stock_code, warehouse, location)
                else:
                    available = self.movements_service._get_available_stock(stock_code, warehouse)
                    
                if quantity > available:
                    return False, f"Insufficient stock for {stock_code}. Available: {available}"
                    
                # Create miscellaneous issue
                issue_data = {
                    'stock_code': stock_code,
                    'warehouse': warehouse,
                    'location': location if location else None,
                    'quantity': float(quantity),
                    'reference': f"MISC-{issue_no}",
                    'cost_method': 'AVERAGE'
                }
                
                success, error = self.movements_service.process_issue(issue_data)
                if not success:
                    self.db.rollback()
                    return False, f"Failed to issue {stock_code}: {error}"
                    
            self.db.commit()
            
            # Log miscellaneous issue
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="MISCELLANEOUS_ISSUE",
                    table="stock_movement_rec",
                    key=issue_no,
                    new_values={
                        'reason': reason,
                        'reference': reference,
                        'cost_center': cost_center,
                        'warehouse': warehouse
                    },
                    module="STOCK"
                )
                
            return True, f"Miscellaneous issue {issue_no} processed successfully"
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def get_outstanding_sales_orders(self, customer_no: Optional[str] = None, warehouse: Optional[str] = None) -> List[Dict]:
        """Get outstanding sales orders ready for issue"""
        query = self.db.query(SalesOrderRec).filter(
            SalesOrderRec.so_status.in_(['OPEN', 'PARTIAL', 'ALLOCATED'])
        )
        
        if customer_no:
            query = query.filter(SalesOrderRec.so_customer == customer_no)
            
        sos = query.order_by(SalesOrderRec.so_date.desc()).all()
        
        so_list = []
        for so in sos:
            # Get outstanding lines
            so_lines = self.db.query(SalesOrderLineRec).filter(
                and_(
                    SalesOrderLineRec.sol_so_no == so.so_no,
                    SalesOrderLineRec.sol_outstanding > 0
                )
            ).all()
            
            if so_lines:
                so_data = {
                    'so_no': so.so_no,
                    'customer': so.so_customer,
                    'customer_name': so.so_customer_name,
                    'so_date': so.so_date,
                    'due_date': so.so_due_date,
                    'total_value': float(so.so_total_value),
                    'status': so.so_status,
                    'priority': so.so_priority,
                    'lines': [
                        {
                            'line_no': sol.sol_line_no,
                            'stock_code': sol.sol_stock_code,
                            'description': sol.sol_description,
                            'quantity_ordered': float(sol.sol_quantity),
                            'quantity_issued': float(sol.sol_quantity - sol.sol_outstanding),
                            'quantity_outstanding': float(sol.sol_outstanding),
                            'unit_price': float(sol.sol_unit_price),
                            'line_value': float(sol.sol_outstanding * sol.sol_unit_price),
                            'allocated': float(sol.sol_allocated) if hasattr(sol, 'sol_allocated') else 0
                        }
                        for sol in so_lines
                    ]
                }
                
                so_list.append(so_data)
                
        return so_list
        
    def get_delivery_note_list(self, filters: Optional[Dict] = None) -> List[Dict]:
        """Get list of delivery notes with filters"""
        filters = filters or {}
        
        query = self.db.query(DeliveryNoteRec)
        
        # Apply filters
        if filters.get('customer'):
            query = query.filter(DeliveryNoteRec.dn_customer == filters['customer'])
        if filters.get('warehouse'):
            query = query.filter(DeliveryNoteRec.dn_warehouse == filters['warehouse'])
        if filters.get('date_from'):
            query = query.filter(DeliveryNoteRec.dn_date >= filters['date_from'])
        if filters.get('date_to'):
            query = query.filter(DeliveryNoteRec.dn_date <= filters['date_to'])
        if filters.get('issued_only'):
            query = query.filter(DeliveryNoteRec.dn_issued == 'Y')
        if filters.get('unissued_only'):
            query = query.filter(DeliveryNoteRec.dn_issued == 'N')
            
        # Order and limit
        limit = filters.get('limit', 100)
        dns = query.order_by(DeliveryNoteRec.dn_date.desc()).limit(limit).all()
        
        return [
            {
                'dn_no': dn.dn_no,
                'customer': dn.dn_customer,
                'date': dn.dn_date,
                'warehouse': dn.dn_warehouse,
                'delivery_name': dn.dn_delivery_name,
                'carrier': dn.dn_carrier,
                'total_lines': dn.dn_total_lines,
                'total_qty': float(dn.dn_total_qty),
                'total_weight': float(dn.dn_total_weight),
                'status': dn.dn_status,
                'issued': dn.dn_issued == 'Y',
                'created_by': dn.dn_created_by,
                'created_date': dn.dn_created_date
            }
            for dn in dns
        ]
        
    def get_delivery_note_details(self, dn_no: str) -> Dict:
        """Get detailed delivery note information"""
        dn = self.db.query(DeliveryNoteRec).filter(
            DeliveryNoteRec.dn_no == dn_no
        ).first()
        
        if not dn:
            return {"error": "Delivery note not found"}
            
        # Get lines
        dn_lines = self.db.query(DeliveryNoteLineRec).filter(
            DeliveryNoteLineRec.dn_line_dn_no == dn_no
        ).order_by(DeliveryNoteLineRec.dn_line_no).all()
        
        # Get customer details
        customer, _ = self.customer_handler.process(4, key_value=dn.dn_customer)
        
        return {
            'header': {
                'dn_no': dn.dn_no,
                'customer': dn.dn_customer,
                'customer_name': customer.sales_name if customer else 'Unknown',
                'date': dn.dn_date,
                'warehouse': dn.dn_warehouse,
                'delivery_address': {
                    'name': dn.dn_delivery_name,
                    'address1': dn.dn_delivery_add1,
                    'address2': dn.dn_delivery_add2,
                    'address3': dn.dn_delivery_add3,
                    'postcode': dn.dn_delivery_postcode
                },
                'carrier': dn.dn_carrier,
                'status': dn.dn_status,
                'issued': dn.dn_issued == 'Y',
                'totals': {
                    'lines': dn.dn_total_lines,
                    'quantity': float(dn.dn_total_qty),
                    'weight': float(dn.dn_total_weight)
                }
            },
            'lines': [
                {
                    'line_no': line.dn_line_no,
                    'stock_code': line.dn_line_stock_code,
                    'description': line.dn_line_description,
                    'so_no': line.dn_line_so_no,
                    'so_line': line.dn_line_so_line,
                    'qty_ordered': float(line.dn_line_qty_ordered),
                    'qty_issued': float(line.dn_line_qty_issued),
                    'location': line.dn_line_location,
                    'weight': float(line.dn_line_weight),
                    'unit': line.dn_line_unit,
                    'status': line.dn_line_status
                }
                for line in dn_lines
            ]
        }
        
    def get_issue_analysis(self, analysis_type: str, options: Dict) -> Dict:
        """Get issue analysis and reports"""
        if analysis_type == 'CUSTOMER_PERFORMANCE':
            return self._analyze_customer_performance(options)
        elif analysis_type == 'ISSUE_VARIANCE':
            return self._analyze_issue_variance(options)
        elif analysis_type == 'DELIVERY_PERFORMANCE':
            return self._analyze_delivery_performance(options)
        elif analysis_type == 'WAREHOUSE_PERFORMANCE':
            return self._analyze_warehouse_performance(options)
        else:
            return {"error": f"Unknown analysis type: {analysis_type}"}
            
    def check_stock_availability(self, availability_check: Dict) -> Dict:
        """Check stock availability for multiple items"""
        warehouse = availability_check.get('warehouse', 'MAIN')
        items = availability_check.get('items', [])
        
        results = []
        all_available = True
        
        for item in items:
            stock_code = item.get('stock_code')
            qty_required = Decimal(str(item.get('quantity', 0)))
            location = item.get('location', '')
            
            # Check availability
            if location:
                available = self.movements_service._get_available_stock(stock_code, warehouse, location)
            else:
                available = self.movements_service._get_available_stock(stock_code, warehouse)
                
            # Get stock details
            stock, _ = self.stock_handler.process(4, key_value=stock_code)
            
            can_fulfill = available >= qty_required
            if not can_fulfill:
                all_available = False
                
            results.append({
                'stock_code': stock_code,
                'description': stock.stock_desc if stock else 'Unknown',
                'qty_required': float(qty_required),
                'qty_available': float(available),
                'can_fulfill': can_fulfill,
                'shortage': float(max(qty_required - available, 0)),
                'location': location,
                'reorder_level': float(stock.stock_reorder_level) if stock else 0,
                'on_order': float(stock.stock_on_order) if stock else 0
            })
            
        return {
            'warehouse': warehouse,
            'all_available': all_available,
            'total_items': len(items),
            'available_items': sum(1 for r in results if r['can_fulfill']),
            'items': results
        }
        
    def _update_so_line_issue(self, dn_line: DeliveryNoteLineRec):
        """Update sales order line with issue information"""
        if not dn_line.dn_line_so_no:
            return
            
        so_line = self.db.query(SalesOrderLineRec).filter(
            and_(
                SalesOrderLineRec.sol_so_no == dn_line.dn_line_so_no,
                SalesOrderLineRec.sol_line_no == dn_line.dn_line_so_line
            )
        ).first()
        
        if so_line:
            # Update issued quantities
            so_line.sol_issued += dn_line.dn_line_qty_issued
            so_line.sol_outstanding = so_line.sol_quantity - so_line.sol_issued
            so_line.sol_last_issue = int(datetime.now().strftime("%Y%m%d"))
            
            # Update SO status if fully issued
            if so_line.sol_outstanding <= 0:
                so_line.sol_status = 'COMPLETE'
                
                # Check if all lines complete to update SO header
                remaining_lines = self.db.query(func.count(SalesOrderLineRec.sol_id)).filter(
                    and_(
                        SalesOrderLineRec.sol_so_no == dn_line.dn_line_so_no,
                        SalesOrderLineRec.sol_outstanding > 0
                    )
                ).scalar()
                
                if remaining_lines == 0:
                    so = self.db.query(SalesOrderRec).filter(
                        SalesOrderRec.so_no == dn_line.dn_line_so_no
                    ).first()
                    if so:
                        so.so_status = 'COMPLETE'
                        so.so_completed_date = int(datetime.now().strftime("%Y%m%d"))
            else:
                so_line.sol_status = 'PARTIAL'
                
                # Update SO header to partial
                so = self.db.query(SalesOrderRec).filter(
                    SalesOrderRec.so_no == dn_line.dn_line_so_no
                ).first()
                if so and so.so_status == 'OPEN':
                    so.so_status = 'PARTIAL'
                    
    def _get_next_dn_number(self) -> str:
        """Generate next delivery note number"""
        system_rec, _ = self.system_handler.read_system_params()
        if system_rec:
            next_no = system_rec.dn_next_number + 1
            system_rec.dn_next_number = next_no
            return f"DN{next_no:06d}"
        return f"DN{datetime.now().strftime('%Y%m%d%H%M%S')}"
        
    def _get_next_production_issue_number(self) -> str:
        """Generate next production issue number"""
        return f"PI{datetime.now().strftime('%Y%m%d%H%M%S')}"
        
    def _get_next_misc_issue_number(self) -> str:
        """Generate next miscellaneous issue number"""
        return f"MI{datetime.now().strftime('%Y%m%d%H%M%S')}"
        
    def _analyze_customer_performance(self, options: Dict) -> Dict:
        """Analyze customer delivery performance"""
        return {"analysis": "customer_performance", "data": "Not implemented"}
        
    def _analyze_issue_variance(self, options: Dict) -> Dict:
        """Analyze issue quantity variances"""
        return {"analysis": "issue_variance", "data": "Not implemented"}
        
    def _analyze_delivery_performance(self, options: Dict) -> Dict:
        """Analyze delivery time performance"""
        return {"analysis": "delivery_performance", "data": "Not implemented"}
        
    def _analyze_warehouse_performance(self, options: Dict) -> Dict:
        """Analyze warehouse issue performance"""
        return {"analysis": "warehouse_performance", "data": "Not implemented"}