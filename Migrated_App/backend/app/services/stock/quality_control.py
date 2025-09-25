"""
Quality Control Integration Service - ST190 migration
Handles quality control processes and stock quarantine
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from datetime import datetime, timedelta
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func, desc

from app.services.file_handlers.stock_handler import StockFileHandler
from app.services.file_handlers.system_handler import SystemFileHandler
from app.models.stock import (
    StockMasterRec, QualityControlRec, QualityTestRec, QualityResultRec,
    QuarantineRec, QualityInspectionRec, QualityDefectRec,
    StockLocationRec, LotNumberRec, SerialNumberRec
)
from app.services.stock.stock_movements import StockMovementsService
from app.core.security import log_user_action
from app.models.auth import User


class QualityControlService:
    """
    Quality Control functionality
    Implements ST190 - quality control processes and quarantine management
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.stock_handler = StockFileHandler(db)
        self.system_handler = SystemFileHandler(db)
        
    def create_qc_inspection(self, inspection_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Create quality control inspection
        Returns (success, error_message or inspection_data)
        """
        stock_code = inspection_data.get('stock_code')
        warehouse = inspection_data.get('warehouse', 'MAIN')
        location = inspection_data.get('location', 'QC')
        lot_number = inspection_data.get('lot_number', '')
        serial_number = inspection_data.get('serial_number', '')
        quantity = Decimal(str(inspection_data.get('quantity', 0)))
        inspection_type = inspection_data.get('type', 'RECEIPT')  # RECEIPT, PRODUCTION, RETURN, RANDOM
        priority = inspection_data.get('priority', 'NORMAL')
        supplier = inspection_data.get('supplier', '')
        receipt_reference = inspection_data.get('receipt_reference', '')
        
        # Validate stock item
        stock, _ = self.stock_handler.process(4, key_value=stock_code)
        if not stock:
            return False, "Stock item not found"
            
        try:
            # Create inspection record
            inspection_no = self._get_next_inspection_number()
            
            inspection = QualityInspectionRec(
                insp_no=inspection_no,
                insp_stock_code=stock_code,
                insp_warehouse=warehouse,
                insp_location=location,
                insp_lot_number=lot_number,
                insp_serial_number=serial_number,
                insp_quantity=quantity,
                insp_type=inspection_type,
                insp_status='PENDING',
                insp_priority=priority,
                insp_supplier=supplier,
                insp_receipt_reference=receipt_reference,
                insp_created_date=int(datetime.now().strftime("%Y%m%d")),
                insp_created_time=int(datetime.now().strftime("%H%M%S")),
                insp_created_by=self.current_user.username if self.current_user else 'SYSTEM',
                insp_required_tests=self._get_required_tests(stock_code, inspection_type),
                insp_completed_tests=0,
                insp_passed_tests=0,
                insp_failed_tests=0
            )
            
            self.db.add(inspection)
            self.db.flush()
            
            # Create quality control tests
            required_tests = self._create_qc_tests(inspection_no, stock_code, inspection_type)
            
            # Put stock on QC hold if required
            if inspection_type in ['RECEIPT', 'RETURN'] and quantity > 0:
                success, error = self._create_qc_hold(
                    stock_code, warehouse, location, lot_number, 
                    serial_number, quantity, inspection_no
                )
                if not success:
                    self.db.rollback()
                    return False, f"Failed to create QC hold: {error}"
                    
            self.db.commit()
            
            # Log creation
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="CREATE_QC_INSPECTION",
                    table="quality_inspection_rec",
                    key=inspection_no,
                    new_values={
                        'stock_code': stock_code,
                        'type': inspection_type,
                        'quantity': float(quantity),
                        'tests': len(required_tests)
                    },
                    module="STOCK"
                )
                
            return True, {
                'inspection_no': inspection_no,
                'stock_code': stock_code,
                'quantity': float(quantity),
                'required_tests': len(required_tests),
                'status': 'PENDING'
            }
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def record_test_result(self, test_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Record quality test result
        Returns (success, error_message)
        """
        inspection_no = test_data.get('inspection_no')
        test_code = test_data.get('test_code')
        result_value = test_data.get('result_value', '')
        result_status = test_data.get('result_status', 'PASS')  # PASS, FAIL, WARNING
        test_notes = test_data.get('notes', '')
        tested_by = test_data.get('tested_by', '')
        equipment_used = test_data.get('equipment_used', '')
        
        # Get inspection
        inspection = self.db.query(QualityInspectionRec).filter(
            QualityInspectionRec.insp_no == inspection_no
        ).first()
        
        if not inspection:
            return False, "Inspection not found"
            
        # Get test record
        test = self.db.query(QualityTestRec).filter(
            and_(
                QualityTestRec.test_inspection_no == inspection_no,
                QualityTestRec.test_code == test_code
            )
        ).first()
        
        if not test:
            return False, f"Test {test_code} not found for inspection {inspection_no}"
            
        if test.test_status != 'PENDING':
            return False, f"Test {test_code} already completed with status: {test.test_status}"
            
        try:
            # Update test record
            test.test_result_value = result_value
            test.test_result_status = result_status
            test.test_status = 'COMPLETED'
            test.test_notes = test_notes
            test.test_tested_by = tested_by
            test.test_equipment_used = equipment_used
            test.test_completed_date = int(datetime.now().strftime("%Y%m%d"))
            test.test_completed_time = int(datetime.now().strftime("%H%M%S"))
            
            # Validate result against specifications
            validation_result = self._validate_test_result(test, result_value, result_status)
            test.test_within_spec = validation_result['within_spec']
            test.test_variance = validation_result['variance']
            
            # Create detailed result record
            result_record = QualityResultRec(
                result_inspection_no=inspection_no,
                result_test_code=test_code,
                result_value=result_value,
                result_status=result_status,
                result_within_spec=validation_result['within_spec'],
                result_variance=validation_result['variance'],
                result_equipment=equipment_used,
                result_tested_by=tested_by,
                result_date=int(datetime.now().strftime("%Y%m%d")),
                result_time=int(datetime.now().strftime("%H%M%S")),
                result_notes=test_notes
            )
            
            self.db.add(result_record)
            
            # Update inspection counters
            inspection.insp_completed_tests += 1
            if result_status == 'PASS' and validation_result['within_spec']:
                inspection.insp_passed_tests += 1
            else:
                inspection.insp_failed_tests += 1
                
                # Create defect record if test failed
                if result_status == 'FAIL':
                    self._create_defect_record(inspection_no, test_code, result_value, test_notes)
                    
            # Check if all tests completed
            if inspection.insp_completed_tests >= inspection.insp_required_tests:
                overall_status = self._determine_inspection_result(inspection)
                inspection.insp_status = overall_status
                inspection.insp_completed_date = int(datetime.now().strftime("%Y%m%d"))
                inspection.insp_completed_time = int(datetime.now().strftime("%H%M%S"))
                inspection.insp_completed_by = self.current_user.username if self.current_user else 'SYSTEM'
                
                # Process inspection completion
                self._process_inspection_completion(inspection, overall_status)
                
            self.db.commit()
            
            return True, f"Test {test_code} recorded with result: {result_status}"
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def release_from_quarantine(self, release_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Release stock from quarantine
        Returns (success, error_message)
        """
        quarantine_id = release_data.get('quarantine_id')
        release_quantity = Decimal(str(release_data.get('release_quantity', 0)))
        release_to_location = release_data.get('release_to_location', 'MAIN')
        disposition = release_data.get('disposition', 'APPROVED')  # APPROVED, REWORK, SCRAP
        release_notes = release_data.get('notes', '')
        
        # Get quarantine record
        quarantine = self.db.query(QuarantineRec).filter(
            QuarantineRec.quar_id == quarantine_id
        ).first()
        
        if not quarantine:
            return False, "Quarantine record not found"
            
        if quarantine.quar_status != 'ACTIVE':
            return False, f"Cannot release from quarantine with status: {quarantine.quar_status}"
            
        if release_quantity > quarantine.quar_quantity:
            return False, f"Release quantity ({release_quantity}) exceeds quarantine quantity ({quarantine.quar_quantity})"
            
        try:
            movements_service = StockMovementsService(self.db, self.current_user)
            
            if disposition == 'APPROVED':
                # Transfer from quarantine to approved location
                transfer_data = {
                    'stock_code': quarantine.quar_stock_code,
                    'from_warehouse': quarantine.quar_warehouse,
                    'to_warehouse': quarantine.quar_warehouse,
                    'from_location': quarantine.quar_location,
                    'to_location': release_to_location,
                    'quantity': float(release_quantity),
                    'reference': f"QC-REL-{quarantine_id}"
                }
                
                success, error = movements_service.process_transfer(transfer_data)
                if not success:
                    return False, f"Failed to transfer stock: {error}"
                    
            elif disposition == 'SCRAP':
                # Issue stock as scrap
                scrap_data = {
                    'stock_code': quarantine.quar_stock_code,
                    'warehouse': quarantine.quar_warehouse,
                    'location': quarantine.quar_location,
                    'issue_qty': float(release_quantity),
                    'reason': 'QC_SCRAP',
                    'reference': f"QC-SCRAP-{quarantine_id}"
                }
                
                success, error = movements_service.process_issue(scrap_data)
                if not success:
                    return False, f"Failed to scrap stock: {error}"
                    
            elif disposition == 'REWORK':
                # Transfer to rework location
                transfer_data = {
                    'stock_code': quarantine.quar_stock_code,
                    'from_warehouse': quarantine.quar_warehouse,
                    'to_warehouse': quarantine.quar_warehouse,
                    'from_location': quarantine.quar_location,
                    'to_location': 'REWORK',
                    'quantity': float(release_quantity),
                    'reference': f"QC-REWORK-{quarantine_id}"
                }
                
                success, error = movements_service.process_transfer(transfer_data)
                if not success:
                    return False, f"Failed to transfer to rework: {error}"
                    
            # Update quarantine record
            if release_quantity >= quarantine.quar_quantity:
                # Full release
                quarantine.quar_status = 'RELEASED'
                quarantine.quar_released_quantity = quarantine.quar_quantity
            else:
                # Partial release
                quarantine.quar_released_quantity += release_quantity
                quarantine.quar_quantity -= release_quantity
                
            quarantine.quar_disposition = disposition
            quarantine.quar_released_date = int(datetime.now().strftime("%Y%m%d"))
            quarantine.quar_released_by = self.current_user.username if self.current_user else 'SYSTEM'
            quarantine.quar_release_notes = release_notes
            
            # Update lot/serial status if applicable
            if quarantine.quar_lot_number:
                self._update_lot_qc_status(quarantine.quar_stock_code, quarantine.quar_lot_number, disposition)
                
            if quarantine.quar_serial_number:
                self._update_serial_qc_status(quarantine.quar_stock_code, quarantine.quar_serial_number, disposition)
                
            self.db.commit()
            
            return True, f"Released {release_quantity} from quarantine with disposition: {disposition}"
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def get_qc_dashboard(self, dashboard_filters: Optional[Dict] = None) -> Dict:
        """Get quality control dashboard data"""
        try:
            filters = dashboard_filters or {}
            warehouse = filters.get('warehouse')
            date_from = filters.get('date_from', int((datetime.now() - timedelta(days=30)).strftime("%Y%m%d")))
            date_to = filters.get('date_to', int(datetime.now().strftime("%Y%m%d")))
            
            # Build base query
            insp_query = self.db.query(QualityInspectionRec).filter(
                and_(
                    QualityInspectionRec.insp_created_date >= date_from,
                    QualityInspectionRec.insp_created_date <= date_to
                )
            )
            
            if warehouse:
                insp_query = insp_query.filter(QualityInspectionRec.insp_warehouse == warehouse)
                
            inspections = insp_query.all()
            
            # Calculate metrics
            total_inspections = len(inspections)
            pending_inspections = len([i for i in inspections if i.insp_status == 'PENDING'])
            passed_inspections = len([i for i in inspections if i.insp_status == 'PASSED'])
            failed_inspections = len([i for i in inspections if i.insp_status == 'FAILED'])
            
            # Get quarantine summary
            quar_query = self.db.query(QuarantineRec).filter(
                QuarantineRec.quar_status == 'ACTIVE'
            )
            
            if warehouse:
                quar_query = quar_query.filter(QuarantineRec.quar_warehouse == warehouse)
                
            active_quarantines = quar_query.all()
            
            # Calculate quarantine value
            quarantine_value = sum(
                float(q.quar_quantity * (q.quar_unit_cost or Decimal('0')))
                for q in active_quarantines
            )
            
            # Test failure analysis
            test_failures = self.db.query(QualityTestRec).filter(
                and_(
                    QualityTestRec.test_result_status == 'FAIL',
                    QualityTestRec.test_completed_date >= date_from,
                    QualityTestRec.test_completed_date <= date_to
                )
            ).all()
            
            failure_by_test = {}
            for test in test_failures:
                test_code = test.test_code
                failure_by_test[test_code] = failure_by_test.get(test_code, 0) + 1
                
            # Supplier quality metrics
            supplier_metrics = {}
            for inspection in inspections:
                if inspection.insp_supplier and inspection.insp_status in ['PASSED', 'FAILED']:
                    supplier = inspection.insp_supplier
                    if supplier not in supplier_metrics:
                        supplier_metrics[supplier] = {'total': 0, 'passed': 0, 'failed': 0}
                        
                    supplier_metrics[supplier]['total'] += 1
                    if inspection.insp_status == 'PASSED':
                        supplier_metrics[supplier]['passed'] += 1
                    else:
                        supplier_metrics[supplier]['failed'] += 1
                        
            # Calculate supplier pass rates
            for supplier, metrics in supplier_metrics.items():
                metrics['pass_rate'] = (metrics['passed'] / metrics['total'] * 100) if metrics['total'] > 0 else 0
                
            return {
                'period': {
                    'date_from': date_from,
                    'date_to': date_to,
                    'warehouse': warehouse
                },
                'inspection_summary': {
                    'total_inspections': total_inspections,
                    'pending': pending_inspections,
                    'passed': passed_inspections,
                    'failed': failed_inspections,
                    'pass_rate': (passed_inspections / total_inspections * 100) if total_inspections > 0 else 0
                },
                'quarantine_summary': {
                    'active_quarantines': len(active_quarantines),
                    'quarantine_value': quarantine_value,
                    'avg_quarantine_age': self._calculate_avg_quarantine_age(active_quarantines)
                },
                'test_failure_analysis': dict(sorted(failure_by_test.items(), key=lambda x: x[1], reverse=True)[:10]),
                'supplier_quality': dict(sorted(supplier_metrics.items(), key=lambda x: x[1]['pass_rate'], reverse=True)[:10]),
                'trends': self._calculate_qc_trends(inspections, date_from, date_to)
            }
            
        except Exception as e:
            return {"error": str(e)}
            
    def _get_required_tests(self, stock_code: str, inspection_type: str) -> int:
        """Get number of required tests for stock item and inspection type"""
        # This would lookup QC test requirements from configuration
        # For now, return default based on inspection type
        test_counts = {
            'RECEIPT': 3,
            'PRODUCTION': 5,
            'RETURN': 2,
            'RANDOM': 1
        }
        return test_counts.get(inspection_type, 1)
        
    def _create_qc_tests(self, inspection_no: str, stock_code: str, inspection_type: str) -> List[str]:
        """Create required QC tests for inspection"""
        # This would lookup test configuration
        # For now, create standard tests based on type
        standard_tests = {
            'RECEIPT': ['VISUAL', 'DIMENSION', 'FUNCTION'],
            'PRODUCTION': ['VISUAL', 'DIMENSION', 'FUNCTION', 'PERFORMANCE', 'DURABILITY'],
            'RETURN': ['VISUAL', 'FUNCTION'],
            'RANDOM': ['FUNCTION']
        }
        
        test_codes = standard_tests.get(inspection_type, ['VISUAL'])
        created_tests = []
        
        for i, test_code in enumerate(test_codes):
            test = QualityTestRec(
                test_inspection_no=inspection_no,
                test_seq=i + 1,
                test_code=test_code,
                test_description=f"{test_code} Test",
                test_status='PENDING',
                test_required='Y',
                test_specification=self._get_test_specification(stock_code, test_code),
                test_created_date=int(datetime.now().strftime("%Y%m%d"))
            )
            
            self.db.add(test)
            created_tests.append(test_code)
            
        return created_tests
        
    def _create_qc_hold(self, stock_code: str, warehouse: str, location: str, 
                       lot_number: str, serial_number: str, quantity: Decimal, 
                       inspection_no: str) -> Tuple[bool, Optional[str]]:
        """Create QC hold/quarantine"""
        try:
            # Get unit cost
            stock, _ = self.stock_handler.process(4, key_value=stock_code)
            unit_cost = stock.stock_average_cost if stock else Decimal('0')
            
            # Create quarantine record
            quarantine = QuarantineRec(
                quar_stock_code=stock_code,
                quar_warehouse=warehouse,
                quar_location=location,
                quar_lot_number=lot_number,
                quar_serial_number=serial_number,
                quar_quantity=quantity,
                quar_unit_cost=unit_cost,
                quar_total_value=quantity * unit_cost,
                quar_reason='QC_INSPECTION',
                quar_status='ACTIVE',
                quar_inspection_no=inspection_no,
                quar_created_date=int(datetime.now().strftime("%Y%m%d")),
                quar_created_by=self.current_user.username if self.current_user else 'SYSTEM'
            )
            
            self.db.add(quarantine)
            return True, None
            
        except Exception as e:
            return False, str(e)
            
    def _validate_test_result(self, test: QualityTestRec, result_value: str, result_status: str) -> Dict:
        """Validate test result against specifications"""
        # Simplified validation - would use actual test specifications
        within_spec = result_status == 'PASS'
        variance = 0.0
        
        # If numeric result, calculate variance from specification
        try:
            if test.test_specification and result_value.replace('.', '').isdigit():
                spec_value = float(test.test_specification)
                actual_value = float(result_value)
                variance = abs(actual_value - spec_value) / spec_value * 100
        except:
            pass
            
        return {
            'within_spec': within_spec,
            'variance': variance
        }
        
    def _determine_inspection_result(self, inspection: QualityInspectionRec) -> str:
        """Determine overall inspection result"""
        if inspection.insp_failed_tests > 0:
            return 'FAILED'
        elif inspection.insp_passed_tests == inspection.insp_required_tests:
            return 'PASSED'
        else:
            return 'PENDING'
            
    def _process_inspection_completion(self, inspection: QualityInspectionRec, result: str):
        """Process completed inspection"""
        if result == 'PASSED':
            # Release from QC hold if applicable
            self._auto_release_qc_hold(inspection.insp_no)
        elif result == 'FAILED':
            # Keep in quarantine, may require disposition
            pass
            
    def _auto_release_qc_hold(self, inspection_no: str):
        """Automatically release QC hold for passed inspection"""
        quarantine = self.db.query(QuarantineRec).filter(
            and_(
                QuarantineRec.quar_inspection_no == inspection_no,
                QuarantineRec.quar_status == 'ACTIVE'
            )
        ).first()
        
        if quarantine:
            # Auto-release with approval disposition
            release_data = {
                'quarantine_id': quarantine.quar_id,
                'release_quantity': quarantine.quar_quantity,
                'disposition': 'APPROVED',
                'notes': 'Auto-released after QC pass'
            }
            self.release_from_quarantine(release_data)
            
    def _create_defect_record(self, inspection_no: str, test_code: str, result_value: str, notes: str):
        """Create defect record for failed test"""
        defect = QualityDefectRec(
            defect_inspection_no=inspection_no,
            defect_test_code=test_code,
            defect_type='TEST_FAILURE',
            defect_severity='MAJOR',
            defect_description=f"Test {test_code} failed with result: {result_value}",
            defect_notes=notes,
            defect_created_date=int(datetime.now().strftime("%Y%m%d")),
            defect_created_by=self.current_user.username if self.current_user else 'SYSTEM'
        )
        
        self.db.add(defect)
        
    def _get_test_specification(self, stock_code: str, test_code: str) -> str:
        """Get test specification for stock item and test"""
        # This would lookup from test specification master
        # For now, return default specifications
        default_specs = {
            'VISUAL': 'No visible defects',
            'DIMENSION': '±5%',
            'FUNCTION': 'Pass/Fail',
            'PERFORMANCE': '≥95%',
            'DURABILITY': '≥1000 cycles'
        }
        
        return default_specs.get(test_code, 'Pass/Fail')
        
    def _update_lot_qc_status(self, stock_code: str, lot_number: str, disposition: str):
        """Update lot QC status"""
        lot = self.db.query(LotNumberRec).filter(
            and_(
                LotNumberRec.lot_stock_code == stock_code,
                LotNumberRec.lot_number == lot_number
            )
        ).first()
        
        if lot:
            if disposition == 'APPROVED':
                lot.lot_quality_status = 'APPROVED'
                lot.lot_status = 'AVAILABLE'
            elif disposition == 'SCRAP':
                lot.lot_quality_status = 'REJECTED'
                lot.lot_status = 'SCRAPPED'
            elif disposition == 'REWORK':
                lot.lot_quality_status = 'REWORK'
                lot.lot_status = 'REWORK'
                
    def _update_serial_qc_status(self, stock_code: str, serial_number: str, disposition: str):
        """Update serial QC status"""
        serial = self.db.query(SerialNumberRec).filter(
            and_(
                SerialNumberRec.serial_stock_code == stock_code,
                SerialNumberRec.serial_number == serial_number
            )
        ).first()
        
        if serial:
            if disposition == 'APPROVED':
                serial.serial_status = 'AVAILABLE'
            elif disposition == 'SCRAP':
                serial.serial_status = 'SCRAPPED'
            elif disposition == 'REWORK':
                serial.serial_status = 'REWORK'
                
    def _calculate_avg_quarantine_age(self, quarantines: List[QuarantineRec]) -> float:
        """Calculate average age of quarantine items"""
        if not quarantines:
            return 0.0
            
        total_age = 0
        today = int(datetime.now().strftime("%Y%m%d"))
        
        for q in quarantines:
            age = (datetime.strptime(str(today), "%Y%m%d") - 
                   datetime.strptime(str(q.quar_created_date), "%Y%m%d")).days
            total_age += age
            
        return total_age / len(quarantines)
        
    def _calculate_qc_trends(self, inspections: List[QualityInspectionRec], 
                           start_date: int, end_date: int) -> Dict:
        """Calculate QC trends over period"""
        if not inspections:
            return {}
            
        # Group by week
        weekly_stats = {}
        
        for inspection in inspections:
            if inspection.insp_created_date:
                insp_date = datetime.strptime(str(inspection.insp_created_date), "%Y%m%d")
                week_start = insp_date - timedelta(days=insp_date.weekday())
                week_key = week_start.strftime("%Y-%W")
                
                if week_key not in weekly_stats:
                    weekly_stats[week_key] = {
                        'total': 0, 'passed': 0, 'failed': 0, 'pass_rate': 0
                    }
                    
                weekly_stats[week_key]['total'] += 1
                if inspection.insp_status == 'PASSED':
                    weekly_stats[week_key]['passed'] += 1
                elif inspection.insp_status == 'FAILED':
                    weekly_stats[week_key]['failed'] += 1
                    
        # Calculate pass rates
        for week_data in weekly_stats.values():
            if week_data['total'] > 0:
                week_data['pass_rate'] = week_data['passed'] / week_data['total'] * 100
                
        return {
            'weekly_stats': weekly_stats,
            'overall_trend': self._determine_trend_direction(weekly_stats)
        }
        
    def _determine_trend_direction(self, weekly_stats: Dict) -> str:
        """Determine if QC performance is improving or declining"""
        weeks = sorted(weekly_stats.keys())
        if len(weeks) < 2:
            return 'STABLE'
            
        recent_rate = weekly_stats[weeks[-1]]['pass_rate']
        previous_rate = weekly_stats[weeks[-2]]['pass_rate']
        
        if recent_rate > previous_rate + 5:
            return 'IMPROVING'
        elif recent_rate < previous_rate - 5:
            return 'DECLINING'
        else:
            return 'STABLE'
            
    def _get_next_inspection_number(self) -> str:
        """Generate next inspection number"""
        return f"QCI{datetime.now().strftime('%Y%m%d%H%M%S')}"