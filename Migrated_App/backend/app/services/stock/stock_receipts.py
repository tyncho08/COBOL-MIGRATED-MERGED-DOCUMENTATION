"""
Stock Receipts Service - ST050 migration
Handles automated stock receipt processing from purchase orders
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from datetime import datetime
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func

from app.services.file_handlers.stock_handler import StockFileHandler
from app.services.file_handlers.supplier_handler import SupplierFileHandler
from app.services.file_handlers.system_handler import SystemFileHandler
from app.models.stock import StockMasterRec, StockLocationRec, StockMovementRec
from app.models.purchase import PurchaseOrderRec, PurchaseOrderLineRec, GoodsReceivedRec, GoodsReceivedLineRec
from app.models.supplier import PurchaseLedgerRec
from app.services.stock.stock_movements import StockMovementsService
from app.core.security import log_user_action
from app.models.auth import User


class StockReceiptsService:
    """
    Stock Receipts Processing functionality
    Implements ST050 - automated receipt processing
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.stock_handler = StockFileHandler(db)
        self.supplier_handler = SupplierFileHandler(db)
        self.system_handler = SystemFileHandler(db)
        self.movements_service = StockMovementsService(db, current_user)
        
    def process_goods_received(self, grn_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Process goods received note
        Returns (success, error_message)
        """
        grn_no = grn_data.get('grn_no')
        if not grn_no:
            grn_no = self._get_next_grn_number()
            
        supplier_no = grn_data.get('supplier_no')
        warehouse = grn_data.get('warehouse', 'MAIN')
        received_date = grn_data.get('received_date', int(datetime.now().strftime("%Y%m%d")))
        delivery_note = grn_data.get('delivery_note', '')
        carrier = grn_data.get('carrier', '')
        
        # Validate supplier
        supplier, status = self.supplier_handler.process(4, key_value=supplier_no)
        if status.fs_reply != "00":
            return False, "Supplier not found"
            
        # Validate lines
        lines = grn_data.get('lines', [])
        if not lines:
            return False, "GRN must have at least one line"
            
        try:
            # Create GRN header
            grn = GoodsReceivedRec(
                grn_no=grn_no,
                grn_supplier=supplier_no,
                grn_date=received_date,
                grn_warehouse=warehouse,
                grn_delivery_note=delivery_note,
                grn_carrier=carrier,
                grn_status='DRAFT',
                grn_posted='N',
                grn_created_by=self.current_user.username if self.current_user else 'SYSTEM',
                grn_created_date=int(datetime.now().strftime("%Y%m%d")),
                grn_total_lines=len(lines),
                grn_total_qty=Decimal('0'),
                grn_total_value=Decimal('0')
            )
            
            self.db.add(grn)
            self.db.flush()
            
            # Process each line
            total_qty = Decimal('0')
            total_value = Decimal('0')
            
            for line_data in lines:
                line, error = self._process_grn_line(grn, line_data)
                if error:
                    self.db.rollback()
                    return False, error
                    
                total_qty += line.grn_line_qty_received
                total_value += line.grn_line_value
                
            # Update GRN totals
            grn.grn_total_qty = total_qty
            grn.grn_total_value = total_value
            
            self.db.commit()
            
            # Log creation
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="CREATE_GRN",
                    table="goods_received_rec",
                    key=grn_no,
                    new_values={
                        'supplier': supplier_no,
                        'total_qty': float(total_qty),
                        'total_value': float(total_value)
                    },
                    module="STOCK"
                )
                
            return True, f"GRN {grn_no} created successfully"
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def _process_grn_line(self, grn: GoodsReceivedRec, line_data: Dict) -> Tuple[GoodsReceivedLineRec, Optional[str]]:
        """Process individual GRN line"""
        stock_code = line_data.get('stock_code')
        po_no = line_data.get('po_no', '')
        po_line = line_data.get('po_line', 0)
        qty_ordered = Decimal(str(line_data.get('qty_ordered', 0)))
        qty_received = Decimal(str(line_data.get('qty_received', 0)))
        unit_cost = Decimal(str(line_data.get('unit_cost', 0)))
        location = line_data.get('location', 'UNASSIGNED')
        
        if qty_received <= 0:
            return None, "Received quantity must be greater than zero"
            
        # Validate stock item
        stock, status = self.stock_handler.process(4, key_value=stock_code)
        if status.fs_reply != "00":
            return None, f"Stock item {stock_code} not found"
            
        # Get next line number
        line_no = self.db.query(func.count(GoodsReceivedLineRec.grn_line_id)).filter(
            GoodsReceivedLineRec.grn_line_grn_no == grn.grn_no
        ).scalar() + 1
        
        # Create GRN line
        grn_line = GoodsReceivedLineRec(
            grn_line_grn_no=grn.grn_no,
            grn_line_no=line_no,
            grn_line_stock_code=stock_code,
            grn_line_description=stock.stock_desc,
            grn_line_po_no=po_no,
            grn_line_po_line=po_line,
            grn_line_qty_ordered=qty_ordered,
            grn_line_qty_received=qty_received,
            grn_line_unit_cost=unit_cost,
            grn_line_value=qty_received * unit_cost,
            grn_line_location=location,
            grn_line_status='RECEIVED',
            grn_line_unit=stock.stock_unit
        )
        
        self.db.add(grn_line)
        self.db.flush()
        
        return grn_line, None
        
    def post_grn(self, grn_no: str) -> Tuple[bool, Optional[str]]:
        """
        Post GRN to update stock quantities
        Returns (success, error_message)
        """
        # Get GRN
        grn = self.db.query(GoodsReceivedRec).filter(
            GoodsReceivedRec.grn_no == grn_no
        ).first()
        
        if not grn:
            return False, "GRN not found"
            
        if grn.grn_posted == 'Y':
            return False, "GRN already posted"
            
        try:
            # Get GRN lines
            grn_lines = self.db.query(GoodsReceivedLineRec).filter(
                GoodsReceivedLineRec.grn_line_grn_no == grn_no
            ).all()
            
            # Process each line
            for line in grn_lines:
                # Create stock receipt movement
                receipt_data = {
                    'stock_code': line.grn_line_stock_code,
                    'warehouse': grn.grn_warehouse,
                    'location': line.grn_line_location,
                    'quantity': float(line.grn_line_qty_received),
                    'unit_cost': float(line.grn_line_unit_cost),
                    'reference': f"GRN-{grn_no}"
                }
                
                success, error = self.movements_service.process_receipt(receipt_data)
                if not success:
                    self.db.rollback()
                    return False, f"Failed to post line {line.grn_line_no}: {error}"
                    
                # Update PO line if linked
                if line.grn_line_po_no:
                    self._update_po_line_receipt(line)
                    
            # Mark GRN as posted
            grn.grn_posted = 'Y'
            grn.grn_posted_date = int(datetime.now().strftime("%Y%m%d"))
            grn.grn_posted_by = self.current_user.username if self.current_user else 'SYSTEM'
            grn.grn_status = 'POSTED'
            
            self.db.commit()
            
            # Log posting
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="POST_GRN",
                    table="goods_received_rec",
                    key=grn_no,
                    new_values={'total_value': float(grn.grn_total_value)},
                    module="STOCK"
                )
                
            return True, f"GRN {grn_no} posted successfully"
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def process_purchase_order_receipt(self, po_receipt_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Process receipt directly from purchase order
        Returns (success, error_message)
        """
        po_no = po_receipt_data.get('po_no')
        warehouse = po_receipt_data.get('warehouse', 'MAIN')
        partial_receipt = po_receipt_data.get('partial_receipt', False)
        receipt_lines = po_receipt_data.get('lines', [])
        
        # Get purchase order
        po = self.db.query(PurchaseOrderRec).filter(
            PurchaseOrderRec.po_no == po_no
        ).first()
        
        if not po:
            return False, "Purchase order not found"
            
        if po.po_status not in ['OPEN', 'PARTIAL']:
            return False, f"Cannot receive against PO with status: {po.po_status}"
            
        try:
            # Create GRN from PO
            grn_data = {
                'supplier_no': po.po_supplier,
                'warehouse': warehouse,
                'received_date': int(datetime.now().strftime("%Y%m%d")),
                'delivery_note': po_receipt_data.get('delivery_note', ''),
                'carrier': po_receipt_data.get('carrier', ''),
                'lines': []
            }
            
            # Get PO lines
            po_lines = self.db.query(PurchaseOrderLineRec).filter(
                PurchaseOrderLineRec.pol_po_no == po_no
            ).all()
            
            # Process receipt lines
            for receipt_line in receipt_lines:
                po_line_no = receipt_line.get('po_line_no')
                qty_received = Decimal(str(receipt_line.get('qty_received', 0)))
                
                # Find matching PO line
                po_line = next((pol for pol in po_lines if pol.pol_line_no == po_line_no), None)
                if not po_line:
                    return False, f"PO line {po_line_no} not found"
                    
                if qty_received <= 0:
                    continue
                    
                # Check quantity not exceeding outstanding
                if qty_received > po_line.pol_outstanding:
                    if not po_receipt_data.get('allow_over_receipt', False):
                        return False, f"Line {po_line_no}: Received quantity exceeds outstanding"
                        
                # Add to GRN lines
                grn_line = {
                    'stock_code': po_line.pol_stock_code,
                    'po_no': po_no,
                    'po_line': po_line_no,
                    'qty_ordered': float(po_line.pol_quantity),
                    'qty_received': float(qty_received),
                    'unit_cost': float(po_line.pol_unit_cost),
                    'location': receipt_line.get('location', 'UNASSIGNED')
                }
                
                grn_data['lines'].append(grn_line)
                
            if not grn_data['lines']:
                return False, "No lines to receive"
                
            # Process GRN
            success, error = self.process_goods_received(grn_data)
            if not success:
                return False, error
                
            # Auto-post if configured
            system_rec, _ = self.system_handler.read_system_params()
            auto_post = system_rec and system_rec.grn_auto_post == 'Y'
            
            if auto_post:
                # Get the GRN number from the created GRN
                latest_grn = self.db.query(GoodsReceivedRec).filter(
                    GoodsReceivedRec.grn_supplier == po.po_supplier
                ).order_by(GoodsReceivedRec.grn_created_date.desc()).first()
                
                if latest_grn:
                    post_success, post_error = self.post_grn(latest_grn.grn_no)
                    if not post_success:
                        return False, f"GRN created but posting failed: {post_error}"
                        
            return True, "Purchase order receipt processed successfully"
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def process_return_to_supplier(self, return_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Process return to supplier (negative receipt)
        Returns (success, error_message)
        """
        supplier_no = return_data.get('supplier_no')
        warehouse = return_data.get('warehouse', 'MAIN')
        return_reason = return_data.get('reason', '')
        reference = return_data.get('reference', '')
        lines = return_data.get('lines', [])
        
        if not lines:
            return False, "Return must have at least one line"
            
        try:
            # Get next return number
            return_no = self._get_next_return_number()
            
            # Process each return line as negative receipt
            for line_data in lines:
                stock_code = line_data.get('stock_code')
                qty_returned = Decimal(str(line_data.get('quantity', 0)))
                location = line_data.get('location', 'UNASSIGNED')
                
                if qty_returned <= 0:
                    continue
                    
                # Check stock availability
                available = self.movements_service._get_available_stock(stock_code, warehouse, location)
                if qty_returned > available:
                    return False, f"Insufficient stock for return. Available: {available}"
                    
                # Get stock item for costing
                stock, _ = self.stock_handler.process(4, key_value=stock_code)
                unit_cost = stock.stock_average_cost if stock else Decimal('0')
                
                # Create negative receipt (issue)
                issue_data = {
                    'stock_code': stock_code,
                    'warehouse': warehouse,
                    'location': location,
                    'quantity': float(qty_returned),
                    'reference': f"RTS-{return_no}"
                }
                
                success, error = self.movements_service.process_issue(issue_data)
                if not success:
                    self.db.rollback()
                    return False, f"Failed to process return for {stock_code}: {error}"
                    
            self.db.commit()
            
            # Log return
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="RETURN_TO_SUPPLIER",
                    table="stock_movement_rec",
                    key=return_no,
                    new_values={
                        'supplier': supplier_no,
                        'reason': return_reason,
                        'reference': reference
                    },
                    module="STOCK"
                )
                
            return True, f"Return {return_no} processed successfully"
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def get_outstanding_pos(self, supplier_no: Optional[str] = None, warehouse: Optional[str] = None) -> List[Dict]:
        """Get outstanding purchase orders ready for receipt"""
        query = self.db.query(PurchaseOrderRec).filter(
            PurchaseOrderRec.po_status.in_(['OPEN', 'PARTIAL'])
        )
        
        if supplier_no:
            query = query.filter(PurchaseOrderRec.po_supplier == supplier_no)
            
        pos = query.order_by(PurchaseOrderRec.po_date.desc()).all()
        
        po_list = []
        for po in pos:
            # Get outstanding lines
            po_lines = self.db.query(PurchaseOrderLineRec).filter(
                and_(
                    PurchaseOrderLineRec.pol_po_no == po.po_no,
                    PurchaseOrderLineRec.pol_outstanding > 0
                )
            ).all()
            
            if po_lines:
                po_data = {
                    'po_no': po.po_no,
                    'supplier': po.po_supplier,
                    'supplier_name': po.po_supplier_name,
                    'po_date': po.po_date,
                    'due_date': po.po_due_date,
                    'total_value': float(po.po_total_value),
                    'status': po.po_status,
                    'lines': [
                        {
                            'line_no': pol.pol_line_no,
                            'stock_code': pol.pol_stock_code,
                            'description': pol.pol_description,
                            'quantity_ordered': float(pol.pol_quantity),
                            'quantity_received': float(pol.pol_quantity - pol.pol_outstanding),
                            'quantity_outstanding': float(pol.pol_outstanding),
                            'unit_cost': float(pol.pol_unit_cost),
                            'line_value': float(pol.pol_outstanding * pol.pol_unit_cost)
                        }
                        for pol in po_lines
                    ]
                }
                
                po_list.append(po_data)
                
        return po_list
        
    def get_grn_list(self, filters: Optional[Dict] = None) -> List[Dict]:
        """Get list of goods received notes with filters"""
        filters = filters or {}
        
        query = self.db.query(GoodsReceivedRec)
        
        # Apply filters
        if filters.get('supplier'):
            query = query.filter(GoodsReceivedRec.grn_supplier == filters['supplier'])
        if filters.get('warehouse'):
            query = query.filter(GoodsReceivedRec.grn_warehouse == filters['warehouse'])
        if filters.get('date_from'):
            query = query.filter(GoodsReceivedRec.grn_date >= filters['date_from'])
        if filters.get('date_to'):
            query = query.filter(GoodsReceivedRec.grn_date <= filters['date_to'])
        if filters.get('posted_only'):
            query = query.filter(GoodsReceivedRec.grn_posted == 'Y')
        if filters.get('unposted_only'):
            query = query.filter(GoodsReceivedRec.grn_posted == 'N')
            
        # Order and limit
        limit = filters.get('limit', 100)
        grns = query.order_by(GoodsReceivedRec.grn_date.desc()).limit(limit).all()
        
        return [
            {
                'grn_no': grn.grn_no,
                'supplier': grn.grn_supplier,
                'date': grn.grn_date,
                'warehouse': grn.grn_warehouse,
                'delivery_note': grn.grn_delivery_note,
                'carrier': grn.grn_carrier,
                'total_lines': grn.grn_total_lines,
                'total_qty': float(grn.grn_total_qty),
                'total_value': float(grn.grn_total_value),
                'status': grn.grn_status,
                'posted': grn.grn_posted == 'Y',
                'created_by': grn.grn_created_by,
                'created_date': grn.grn_created_date
            }
            for grn in grns
        ]
        
    def get_grn_details(self, grn_no: str) -> Dict:
        """Get detailed GRN information"""
        grn = self.db.query(GoodsReceivedRec).filter(
            GoodsReceivedRec.grn_no == grn_no
        ).first()
        
        if not grn:
            return {"error": "GRN not found"}
            
        # Get lines
        grn_lines = self.db.query(GoodsReceivedLineRec).filter(
            GoodsReceivedLineRec.grn_line_grn_no == grn_no
        ).order_by(GoodsReceivedLineRec.grn_line_no).all()
        
        # Get supplier details
        supplier, _ = self.supplier_handler.process(4, key_value=grn.grn_supplier)
        
        return {
            'header': {
                'grn_no': grn.grn_no,
                'supplier': grn.grn_supplier,
                'supplier_name': supplier.purch_name if supplier else 'Unknown',
                'date': grn.grn_date,
                'warehouse': grn.grn_warehouse,
                'delivery_note': grn.grn_delivery_note,
                'carrier': grn.grn_carrier,
                'status': grn.grn_status,
                'posted': grn.grn_posted == 'Y',
                'totals': {
                    'lines': grn.grn_total_lines,
                    'quantity': float(grn.grn_total_qty),
                    'value': float(grn.grn_total_value)
                }
            },
            'lines': [
                {
                    'line_no': line.grn_line_no,
                    'stock_code': line.grn_line_stock_code,
                    'description': line.grn_line_description,
                    'po_no': line.grn_line_po_no,
                    'po_line': line.grn_line_po_line,
                    'qty_ordered': float(line.grn_line_qty_ordered),
                    'qty_received': float(line.grn_line_qty_received),
                    'unit_cost': float(line.grn_line_unit_cost),
                    'line_value': float(line.grn_line_value),
                    'location': line.grn_line_location,
                    'unit': line.grn_line_unit,
                    'status': line.grn_line_status
                }
                for line in grn_lines
            ]
        }
        
    def get_receipt_analysis(self, analysis_type: str, options: Dict) -> Dict:
        """Get receipt analysis and reports"""
        if analysis_type == 'SUPPLIER_PERFORMANCE':
            return self._analyze_supplier_performance(options)
        elif analysis_type == 'RECEIPT_VARIANCE':
            return self._analyze_receipt_variance(options)
        elif analysis_type == 'DELIVERY_PERFORMANCE':
            return self._analyze_delivery_performance(options)
        elif analysis_type == 'COST_VARIANCE':
            return self._analyze_cost_variance(options)
        else:
            return {"error": f"Unknown analysis type: {analysis_type}"}
            
    def _update_po_line_receipt(self, grn_line: GoodsReceivedLineRec):
        """Update purchase order line with receipt information"""
        if not grn_line.grn_line_po_no:
            return
            
        po_line = self.db.query(PurchaseOrderLineRec).filter(
            and_(
                PurchaseOrderLineRec.pol_po_no == grn_line.grn_line_po_no,
                PurchaseOrderLineRec.pol_line_no == grn_line.grn_line_po_line
            )
        ).first()
        
        if po_line:
            # Update received quantities
            po_line.pol_received += grn_line.grn_line_qty_received
            po_line.pol_outstanding = po_line.pol_quantity - po_line.pol_received
            po_line.pol_last_receipt = int(datetime.now().strftime("%Y%m%d"))
            
            # Update PO status if fully received
            if po_line.pol_outstanding <= 0:
                po_line.pol_status = 'COMPLETE'
                
                # Check if all lines complete to update PO header
                remaining_lines = self.db.query(func.count(PurchaseOrderLineRec.pol_id)).filter(
                    and_(
                        PurchaseOrderLineRec.pol_po_no == grn_line.grn_line_po_no,
                        PurchaseOrderLineRec.pol_outstanding > 0
                    )
                ).scalar()
                
                if remaining_lines == 0:
                    po = self.db.query(PurchaseOrderRec).filter(
                        PurchaseOrderRec.po_no == grn_line.grn_line_po_no
                    ).first()
                    if po:
                        po.po_status = 'COMPLETE'
                        po.po_completed_date = int(datetime.now().strftime("%Y%m%d"))
            else:
                po_line.pol_status = 'PARTIAL'
                
                # Update PO header to partial
                po = self.db.query(PurchaseOrderRec).filter(
                    PurchaseOrderRec.po_no == grn_line.grn_line_po_no
                ).first()
                if po and po.po_status == 'OPEN':
                    po.po_status = 'PARTIAL'
                    
    def _get_next_grn_number(self) -> str:
        """Generate next GRN number"""
        system_rec, _ = self.system_handler.read_system_params()
        if system_rec:
            next_no = system_rec.grn_next_number + 1
            system_rec.grn_next_number = next_no
            return f"GRN{next_no:06d}"
        return f"GRN{datetime.now().strftime('%Y%m%d%H%M%S')}"
        
    def _get_next_return_number(self) -> str:
        """Generate next return number"""
        return f"RTS{datetime.now().strftime('%Y%m%d%H%M%S')}"
        
    def _analyze_supplier_performance(self, options: Dict) -> Dict:
        """Analyze supplier receipt performance"""
        # This would analyze supplier delivery performance
        return {"analysis": "supplier_performance", "data": "Not implemented"}
        
    def _analyze_receipt_variance(self, options: Dict) -> Dict:
        """Analyze receipt quantity variances"""
        # This would analyze ordered vs received variances
        return {"analysis": "receipt_variance", "data": "Not implemented"}
        
    def _analyze_delivery_performance(self, options: Dict) -> Dict:
        """Analyze delivery time performance"""  
        # This would analyze delivery time vs promised dates
        return {"analysis": "delivery_performance", "data": "Not implemented"}
        
    def _analyze_cost_variance(self, options: Dict) -> Dict:
        """Analyze cost variances between PO and receipt"""
        # This would analyze cost differences
        return {"analysis": "cost_variance", "data": "Not implemented"}