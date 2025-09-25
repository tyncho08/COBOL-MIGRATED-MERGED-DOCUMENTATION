"""
Kit Management Service - ST160 migration
Handles kit/BOM (Bill of Materials) creation, maintenance and processing
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from datetime import datetime
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func, desc

from app.services.file_handlers.stock_handler import StockFileHandler
from app.services.file_handlers.system_handler import SystemFileHandler
from app.models.stock import (
    StockMasterRec, KitMasterRec, KitComponentRec, KitAssemblyRec,
    KitDisassemblyRec, StockLocationRec, StockMovementRec
)
from app.services.stock.stock_movements import StockMovementsService
from app.core.security import log_user_action
from app.models.auth import User


class KitManagementService:
    """
    Kit Management functionality
    Implements ST160 - kit/BOM creation and processing
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.stock_handler = StockFileHandler(db)
        self.system_handler = SystemFileHandler(db)
        
    def create_kit(self, kit_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Create new kit/BOM
        Returns (success, error_message or kit_data)
        """
        kit_code = kit_data.get('kit_code')
        kit_name = kit_data.get('kit_name')
        kit_type = kit_data.get('type', 'STANDARD')  # STANDARD, PHANTOM, CONFIGURABLE
        components = kit_data.get('components', [])
        cost_method = kit_data.get('cost_method', 'COMPONENT_COST')  # COMPONENT_COST, FIXED_COST
        fixed_cost = Decimal(str(kit_data.get('fixed_cost', 0)))
        
        if not kit_code or not kit_name:
            return False, "Kit code and name are required"
            
        if not components:
            return False, "Kit must have at least one component"
            
        # Check if kit already exists
        existing_kit = self.db.query(KitMasterRec).filter(
            KitMasterRec.kit_code == kit_code
        ).first()
        
        if existing_kit:
            return False, f"Kit {kit_code} already exists"
            
        try:
            # Create kit master
            kit = KitMasterRec(
                kit_code=kit_code,
                kit_name=kit_name,
                kit_type=kit_type,
                kit_status='ACTIVE',
                kit_cost_method=cost_method,
                kit_fixed_cost=fixed_cost,
                kit_total_components=len(components),
                kit_created_date=int(datetime.now().strftime("%Y%m%d")),
                kit_created_by=self.current_user.username if self.current_user else 'SYSTEM',
                kit_version='1.0',
                kit_effective_date=int(datetime.now().strftime("%Y%m%d")),
                kit_obsolete_date=0
            )
            
            self.db.add(kit)
            self.db.flush()
            
            # Create components
            total_cost = Decimal('0')
            
            for comp_data in components:
                component_code = comp_data.get('component_code')
                quantity = Decimal(str(comp_data.get('quantity', 1)))
                unit = comp_data.get('unit', 'EA')
                scrap_factor = Decimal(str(comp_data.get('scrap_factor', 0)))
                operation_seq = comp_data.get('operation_seq', 10)
                
                # Validate component exists
                component_stock, _ = self.stock_handler.process(4, key_value=component_code)
                if not component_stock:
                    self.db.rollback()
                    return False, f"Component {component_code} not found"
                    
                # Calculate component cost
                component_cost = component_stock.stock_average_cost * quantity
                if scrap_factor > 0:
                    # Add scrap allowance
                    component_cost *= (1 + scrap_factor / 100)
                    
                total_cost += component_cost
                
                kit_component = KitComponentRec(
                    comp_kit_code=kit_code,
                    comp_seq=len(components),
                    comp_component_code=component_code,
                    comp_description=component_stock.stock_desc,
                    comp_quantity=quantity,
                    comp_unit=unit,
                    comp_unit_cost=component_stock.stock_average_cost,
                    comp_total_cost=component_cost,
                    comp_scrap_factor=scrap_factor,
                    comp_operation_seq=operation_seq,
                    comp_phantom='N',
                    comp_optional='N',
                    comp_substitutable='N',
                    comp_effective_date=int(datetime.now().strftime("%Y%m%d")),
                    comp_obsolete_date=0
                )
                
                self.db.add(kit_component)
                
            # Update kit cost
            if cost_method == 'COMPONENT_COST':
                kit.kit_calculated_cost = total_cost
            else:
                kit.kit_calculated_cost = fixed_cost
                
            self.db.commit()
            
            # Log creation
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="CREATE_KIT",
                    table="kit_master_rec",
                    key=kit_code,
                    new_values={
                        'name': kit_name,
                        'type': kit_type,
                        'components': len(components),
                        'cost': float(kit.kit_calculated_cost)
                    },
                    module="STOCK"
                )
                
            return True, {
                'kit_code': kit_code,
                'kit_name': kit_name,
                'type': kit_type,
                'components': len(components),
                'calculated_cost': float(kit.kit_calculated_cost)
            }
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def update_kit_components(self, kit_code: str, component_updates: List[Dict]) -> Tuple[bool, Optional[str]]:
        """
        Update kit components
        Returns (success, error_message)
        """
        kit = self.db.query(KitMasterRec).filter(
            KitMasterRec.kit_code == kit_code
        ).first()
        
        if not kit:
            return False, "Kit not found"
            
        if kit.kit_status != 'ACTIVE':
            return False, f"Cannot update kit with status: {kit.kit_status}"
            
        try:
            # Version the kit
            old_version = kit.kit_version
            new_version = self._increment_version(old_version)
            
            # Mark old components as obsolete
            old_components = self.db.query(KitComponentRec).filter(
                and_(
                    KitComponentRec.comp_kit_code == kit_code,
                    KitComponentRec.comp_obsolete_date == 0
                )
            ).all()
            
            obsolete_date = int(datetime.now().strftime("%Y%m%d"))
            for old_comp in old_components:
                old_comp.comp_obsolete_date = obsolete_date
                
            # Create new components
            total_cost = Decimal('0')
            
            for comp_update in component_updates:
                component_code = comp_update.get('component_code')
                quantity = Decimal(str(comp_update.get('quantity', 1)))
                
                # Get component details
                component_stock, _ = self.stock_handler.process(4, key_value=component_code)
                if not component_stock:
                    continue
                    
                component_cost = component_stock.stock_average_cost * quantity
                total_cost += component_cost
                
                new_component = KitComponentRec(
                    comp_kit_code=kit_code,
                    comp_seq=len(component_updates),
                    comp_component_code=component_code,
                    comp_description=component_stock.stock_desc,
                    comp_quantity=quantity,
                    comp_unit=comp_update.get('unit', 'EA'),
                    comp_unit_cost=component_stock.stock_average_cost,
                    comp_total_cost=component_cost,
                    comp_scrap_factor=Decimal(str(comp_update.get('scrap_factor', 0))),
                    comp_operation_seq=comp_update.get('operation_seq', 10),
                    comp_effective_date=int(datetime.now().strftime("%Y%m%d")),
                    comp_obsolete_date=0
                )
                
                self.db.add(new_component)
                
            # Update kit
            kit.kit_version = new_version
            kit.kit_total_components = len(component_updates)
            if kit.kit_cost_method == 'COMPONENT_COST':
                kit.kit_calculated_cost = total_cost
            kit.kit_last_updated = int(datetime.now().strftime("%Y%m%d"))
            kit.kit_updated_by = self.current_user.username if self.current_user else 'SYSTEM'
            
            self.db.commit()
            
            return True, f"Kit {kit_code} updated to version {new_version}"
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def assemble_kit(self, assembly_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Assemble kit from components
        Returns (success, error_message or assembly_data)
        """
        kit_code = assembly_data.get('kit_code')
        quantity = Decimal(str(assembly_data.get('quantity', 1)))
        warehouse = assembly_data.get('warehouse', 'MAIN')
        location = assembly_data.get('location', '')
        work_order = assembly_data.get('work_order', '')
        
        # Get kit details
        kit = self.db.query(KitMasterRec).filter(
            KitMasterRec.kit_code == kit_code
        ).first()
        
        if not kit:
            return False, "Kit not found"
            
        if kit.kit_status != 'ACTIVE':
            return False, f"Cannot assemble kit with status: {kit.kit_status}"
            
        # Get active components
        components = self.db.query(KitComponentRec).filter(
            and_(
                KitComponentRec.comp_kit_code == kit_code,
                KitComponentRec.comp_obsolete_date == 0
            )
        ).order_by(KitComponentRec.comp_operation_seq).all()
        
        if not components:
            return False, "No active components found for kit"
            
        try:
            # Check component availability
            availability_check = self._check_component_availability(components, quantity, warehouse)
            if not availability_check['available']:
                return False, f"Insufficient components: {availability_check['shortages']}"
                
            # Create assembly record
            assembly_no = self._get_next_assembly_number()
            
            assembly = KitAssemblyRec(
                assembly_no=assembly_no,
                assembly_kit_code=kit_code,
                assembly_quantity=quantity,
                assembly_warehouse=warehouse,
                assembly_location=location,
                assembly_work_order=work_order,
                assembly_status='IN_PROGRESS',
                assembly_created_date=int(datetime.now().strftime("%Y%m%d")),
                assembly_created_time=int(datetime.now().strftime("%H%M%S")),
                assembly_created_by=self.current_user.username if self.current_user else 'SYSTEM',
                assembly_total_cost=Decimal('0')
            )
            
            self.db.add(assembly)
            self.db.flush()
            
            # Issue components
            movements_service = StockMovementsService(self.db, self.current_user)
            total_cost = Decimal('0')
            
            for component in components:
                required_qty = component.comp_quantity * quantity
                
                # Include scrap allowance
                if component.comp_scrap_factor > 0:
                    required_qty *= (1 + component.comp_scrap_factor / 100)
                    
                # Issue component
                issue_data = {
                    'stock_code': component.comp_component_code,
                    'warehouse': warehouse,
                    'location': location,
                    'issue_qty': float(required_qty),
                    'reason': 'KIT_ASSEMBLY',
                    'reference': f"ASSM-{assembly_no}",
                    'work_order': work_order
                }
                
                success, error = movements_service.process_issue(issue_data)
                if not success:
                    self.db.rollback()
                    return False, f"Failed to issue component {component.comp_component_code}: {error}"
                    
                total_cost += component.comp_unit_cost * required_qty
                
            # Create/receive finished kit
            if self._stock_exists(kit_code):
                # Receive finished kit
                receipt_data = {
                    'stock_code': kit_code,
                    'warehouse': warehouse,
                    'location': location,
                    'receipt_qty': float(quantity),
                    'unit_cost': float(kit.kit_calculated_cost),
                    'reason': 'KIT_ASSEMBLY',
                    'reference': f"ASSM-{assembly_no}",
                    'work_order': work_order
                }
                
                success, error = movements_service.process_receipt(receipt_data)
                if not success:
                    self.db.rollback()
                    return False, f"Failed to receive finished kit: {error}"
                    
            # Update assembly record
            assembly.assembly_status = 'COMPLETE'
            assembly.assembly_completed_date = int(datetime.now().strftime("%Y%m%d"))
            assembly.assembly_completed_time = int(datetime.now().strftime("%H%M%S"))
            assembly.assembly_total_cost = total_cost
            
            self.db.commit()
            
            # Log assembly
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="ASSEMBLE_KIT",
                    table="kit_assembly_rec",
                    key=assembly_no,
                    new_values={
                        'kit_code': kit_code,
                        'quantity': float(quantity),
                        'total_cost': float(total_cost),
                        'warehouse': warehouse
                    },
                    module="STOCK"
                )
                
            return True, {
                'assembly_no': assembly_no,
                'kit_code': kit_code,
                'quantity': float(quantity),
                'total_cost': float(total_cost),
                'components_issued': len(components)
            }
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def disassemble_kit(self, disassembly_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Disassemble kit into components
        Returns (success, error_message or disassembly_data)
        """
        kit_code = disassembly_data.get('kit_code')
        quantity = Decimal(str(disassembly_data.get('quantity', 1)))
        warehouse = disassembly_data.get('warehouse', 'MAIN')
        location = disassembly_data.get('location', '')
        recovery_rate = Decimal(str(disassembly_data.get('recovery_rate', 100)))  # % of components recoverable
        
        # Check kit availability
        kit_availability = self._get_kit_availability(kit_code, warehouse, location)
        if quantity > kit_availability:
            return False, f"Insufficient kit stock. Available: {kit_availability}, Requested: {quantity}"
            
        # Get kit and components
        kit = self.db.query(KitMasterRec).filter(
            KitMasterRec.kit_code == kit_code
        ).first()
        
        if not kit:
            return False, "Kit not found"
            
        components = self.db.query(KitComponentRec).filter(
            and_(
                KitComponentRec.comp_kit_code == kit_code,
                KitComponentRec.comp_obsolete_date == 0
            )
        ).all()
        
        try:
            # Create disassembly record
            disassembly_no = self._get_next_disassembly_number()
            
            disassembly = KitDisassemblyRec(
                disassembly_no=disassembly_no,
                disassembly_kit_code=kit_code,
                disassembly_quantity=quantity,
                disassembly_warehouse=warehouse,
                disassembly_location=location,
                disassembly_recovery_rate=recovery_rate,
                disassembly_status='IN_PROGRESS',
                disassembly_created_date=int(datetime.now().strftime("%Y%m%d")),
                disassembly_created_time=int(datetime.now().strftime("%H%M%S")),
                disassembly_created_by=self.current_user.username if self.current_user else 'SYSTEM',
                disassembly_recovered_value=Decimal('0')
            )
            
            self.db.add(disassembly)
            self.db.flush()
            
            # Issue finished kit
            movements_service = StockMovementsService(self.db, self.current_user)
            
            issue_data = {
                'stock_code': kit_code,
                'warehouse': warehouse,
                'location': location,
                'issue_qty': float(quantity),
                'reason': 'KIT_DISASSEMBLY',
                'reference': f"DISASM-{disassembly_no}"
            }
            
            success, error = movements_service.process_issue(issue_data)
            if not success:
                self.db.rollback()
                return False, f"Failed to issue kit for disassembly: {error}"
                
            # Receive components
            recovered_value = Decimal('0')
            components_recovered = 0
            
            for component in components:
                recoverable_qty = component.comp_quantity * quantity * (recovery_rate / 100)
                
                if recoverable_qty > 0:
                    receipt_data = {
                        'stock_code': component.comp_component_code,
                        'warehouse': warehouse,
                        'location': location,
                        'receipt_qty': float(recoverable_qty),
                        'unit_cost': float(component.comp_unit_cost),
                        'reason': 'KIT_DISASSEMBLY',
                        'reference': f"DISASM-{disassembly_no}"
                    }
                    
                    success, error = movements_service.process_receipt(receipt_data)
                    if success:
                        recovered_value += component.comp_unit_cost * recoverable_qty
                        components_recovered += 1
                        
            # Update disassembly record
            disassembly.disassembly_status = 'COMPLETE'
            disassembly.disassembly_completed_date = int(datetime.now().strftime("%Y%m%d"))
            disassembly.disassembly_completed_time = int(datetime.now().strftime("%H%M%S"))
            disassembly.disassembly_recovered_value = recovered_value
            
            self.db.commit()
            
            return True, {
                'disassembly_no': disassembly_no,
                'kit_code': kit_code,
                'quantity': float(quantity),
                'components_recovered': components_recovered,
                'recovered_value': float(recovered_value),
                'recovery_rate': float(recovery_rate)
            }
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def get_kit_where_used(self, component_code: str) -> List[Dict]:
        """Get list of kits that use a specific component"""
        where_used = self.db.query(KitComponentRec).filter(
            and_(
                KitComponentRec.comp_component_code == component_code,
                KitComponentRec.comp_obsolete_date == 0
            )
        ).all()
        
        results = []
        for usage in where_used:
            # Get kit details
            kit = self.db.query(KitMasterRec).filter(
                KitMasterRec.kit_code == usage.comp_kit_code
            ).first()
            
            if kit and kit.kit_status == 'ACTIVE':
                results.append({
                    'kit_code': kit.kit_code,
                    'kit_name': kit.kit_name,
                    'kit_type': kit.kit_type,
                    'quantity_required': float(usage.comp_quantity),
                    'unit': usage.comp_unit,
                    'scrap_factor': float(usage.comp_scrap_factor),
                    'operation_seq': usage.comp_operation_seq,
                    'kit_cost': float(kit.kit_calculated_cost)
                })
                
        return sorted(results, key=lambda x: x['kit_code'])
        
    def get_kit_costing(self, kit_code: str) -> Dict:
        """Get detailed kit costing breakdown"""
        try:
            # Get kit details
            kit = self.db.query(KitMasterRec).filter(
                KitMasterRec.kit_code == kit_code
            ).first()
            
            if not kit:
                return {"error": "Kit not found"}
                
            # Get components
            components = self.db.query(KitComponentRec).filter(
                and_(
                    KitComponentRec.comp_kit_code == kit_code,
                    KitComponentRec.comp_obsolete_date == 0
                )
            ).order_by(KitComponentRec.comp_operation_seq).all()
            
            component_costs = []
            total_component_cost = Decimal('0')
            
            for component in components:
                # Get current stock cost
                stock, _ = self.stock_handler.process(4, key_value=component.comp_component_code)
                current_cost = stock.stock_average_cost if stock else component.comp_unit_cost
                
                line_cost = current_cost * component.comp_quantity
                if component.comp_scrap_factor > 0:
                    scrap_cost = line_cost * (component.comp_scrap_factor / 100)
                    line_cost += scrap_cost
                else:
                    scrap_cost = Decimal('0')
                    
                total_component_cost += line_cost
                
                component_costs.append({
                    'component_code': component.comp_component_code,
                    'description': component.comp_description,
                    'quantity': float(component.comp_quantity),
                    'unit': component.comp_unit,
                    'unit_cost': float(current_cost),
                    'extended_cost': float(current_cost * component.comp_quantity),
                    'scrap_factor': float(component.comp_scrap_factor),
                    'scrap_cost': float(scrap_cost),
                    'total_cost': float(line_cost),
                    'operation_seq': component.comp_operation_seq
                })
                
            # Calculate cost variances
            stored_cost = kit.kit_calculated_cost
            current_cost = total_component_cost if kit.kit_cost_method == 'COMPONENT_COST' else kit.kit_fixed_cost
            cost_variance = current_cost - stored_cost
            
            return {
                'kit_code': kit.kit_code,
                'kit_name': kit.kit_name,
                'kit_type': kit.kit_type,
                'cost_method': kit.kit_cost_method,
                'costing': {
                    'stored_cost': float(stored_cost),
                    'current_component_cost': float(total_component_cost),
                    'fixed_cost': float(kit.kit_fixed_cost),
                    'effective_cost': float(current_cost),
                    'cost_variance': float(cost_variance),
                    'variance_pct': float(cost_variance / stored_cost * 100) if stored_cost > 0 else 0
                },
                'components': component_costs,
                'totals': {
                    'total_components': len(components),
                    'total_cost': float(current_cost)
                }
            }
            
        except Exception as e:
            return {"error": str(e)}
            
    def _check_component_availability(self, components: List[KitComponentRec], 
                                    kit_quantity: Decimal, warehouse: str) -> Dict:
        """Check availability of all kit components"""
        shortages = []
        available = True
        
        for component in components:
            required_qty = component.comp_quantity * kit_quantity
            
            # Include scrap allowance
            if component.comp_scrap_factor > 0:
                required_qty *= (1 + component.comp_scrap_factor / 100)
                
            # Get available quantity
            available_qty = self._get_component_availability(
                component.comp_component_code, warehouse
            )
            
            if available_qty < required_qty:
                shortages.append({
                    'component_code': component.comp_component_code,
                    'required': float(required_qty),
                    'available': float(available_qty),
                    'shortage': float(required_qty - available_qty)
                })
                available = False
                
        return {
            'available': available,
            'shortages': shortages
        }
        
    def _get_component_availability(self, stock_code: str, warehouse: str) -> Decimal:
        """Get available quantity for component"""
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
        
    def _get_kit_availability(self, kit_code: str, warehouse: str, location: str = '') -> Decimal:
        """Get available quantity for kit"""
        query = self.db.query(
            func.sum(StockLocationRec.loc_qty_on_hand - StockLocationRec.loc_qty_allocated)
        ).filter(
            and_(
                StockLocationRec.loc_stock_code == kit_code,
                StockLocationRec.loc_warehouse == warehouse,
                StockLocationRec.loc_active == 'Y'
            )
        )
        
        if location:
            query = query.filter(StockLocationRec.loc_location == location)
            
        total = query.scalar() or Decimal('0')
        return max(total, Decimal('0'))
        
    def _stock_exists(self, stock_code: str) -> bool:
        """Check if stock item exists"""
        stock, _ = self.stock_handler.process(4, key_value=stock_code)
        return stock is not None
        
    def _increment_version(self, version: str) -> str:
        """Increment version number"""
        try:
            major, minor = version.split('.')
            minor_int = int(minor) + 1
            return f"{major}.{minor_int}"
        except:
            return "1.1"
            
    def _get_next_assembly_number(self) -> str:
        """Generate next assembly number"""
        return f"ASSM{datetime.now().strftime('%Y%m%d%H%M%S')}"
        
    def _get_next_disassembly_number(self) -> str:
        """Generate next disassembly number"""
        return f"DISASM{datetime.now().strftime('%Y%m%d%H%M%S')}"