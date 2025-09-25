"""
System File Handler - COBOL acas000 migration
Handles system_rec with 4 record types:
1 = System parameters
2 = Default record
3 = Final record
4 = System totals
"""
from typing import Any, Tuple
from sqlalchemy.orm import Session

from app.services.file_handlers.base_handler import BaseFileHandler, FileAccess
from app.models.system import SystemRec


class SystemFileHandler(BaseFileHandler):
    """
    System File Handler (acas000)
    Handles the system file which contains 4 different record types
    """
    
    def __init__(self, db: Session):
        super().__init__(db, file_number=10, table_name="system_rec")
        
    def get_model_class(self):
        return SystemRec
        
    def get_key_field(self, key_type: int = 1):
        """
        System file uses record type as key:
        1 = System parameters (only one record)
        2 = Default record  
        3 = Final record
        4 = System totals record
        """
        return "system_rec_key"
        
    def _validate_key_type(self, key_type: int) -> bool:
        """Validate key type for system file"""
        return 1 <= key_type <= 4
        
    def process(self, function: int, record: Any = None, key_value: Any = None, 
                key_type: int = 1) -> Tuple[Any, FileAccess]:
        """
        Override to add key validation for system file
        """
        # Special validation for system file key types
        if function in [4, 5, 7]:  # READ_INDEXED, WRITE, REWRITE
            if not self._validate_key_type(key_type):
                self.file_access = FileAccess()
                self.file_access.we_error = 998  # File seeks key type out of range
                self.file_access.fs_reply = "99"
                return None, self.file_access
                
        # For system file, key_value is the record type (1-4)
        if key_value is None and key_type:
            key_value = key_type
            
        return super().process(function, record, key_value, key_type)
        
    def read_system_params(self) -> Tuple[SystemRec, FileAccess]:
        """Read main system parameters (record type 1)"""
        return self._read_indexed(1, 1)
        
    def read_defaults(self) -> Tuple[Any, FileAccess]:
        """Read default record (record type 2)"""
        # In the new system, defaults are in sysdeflt_rec table
        # For now, return empty success
        self.file_access = FileAccess()
        self.file_access.fs_reply = "00"
        return None, self.file_access
        
    def read_finals(self) -> Tuple[Any, FileAccess]:
        """Read final record (record type 3)"""
        # In the new system, finals are in sysfinal_rec table
        self.file_access = FileAccess()
        self.file_access.fs_reply = "00"
        return None, self.file_access
        
    def read_totals(self) -> Tuple[Any, FileAccess]:
        """Read system totals (record type 4)"""
        # In the new system, totals are in systot_rec table
        self.file_access = FileAccess()
        self.file_access.fs_reply = "00"
        return None, self.file_access
        
    def update_system_params(self, record: SystemRec) -> Tuple[SystemRec, FileAccess]:
        """Update system parameters"""
        # System record always has key = 1
        record.system_rec_key = 1
        return self._rewrite_record(record, 1, 1)
        
    def get_current_period(self) -> int:
        """Get current accounting period"""
        system_rec, status = self.read_system_params()
        if status.fs_reply == "00" and system_rec:
            return system_rec.period
        return 1
        
    def get_vat_rate(self, rate_number: int = 1) -> float:
        """Get VAT rate by number (1-5)"""
        system_rec, status = self.read_system_params()
        if status.fs_reply == "00" and system_rec:
            rate_map = {
                1: system_rec.vat_rate_1,
                2: system_rec.vat_rate_2,
                3: system_rec.vat_rate_3,
                4: system_rec.vat_rate_4,
                5: system_rec.vat_rate_5
            }
            return float(rate_map.get(rate_number, 0.0))
        return 0.0
        
    def get_next_invoice_number(self) -> int:
        """Get next invoice number and increment"""
        system_rec, status = self.read_system_params()
        if status.fs_reply == "00" and system_rec:
            next_num = system_rec.next_invoice
            system_rec.next_invoice += 1
            self.update_system_params(system_rec)
            return next_num
        return 1
        
    def is_module_active(self, module: str) -> bool:
        """Check if a module is active"""
        system_rec, status = self.read_system_params()
        if status.fs_reply == "00" and system_rec:
            module_map = {
                "GL": system_rec.level_1,
                "PL": system_rec.level_2,
                "SL": system_rec.level_3,
                "STOCK": system_rec.level_4,
                "IRS": system_rec.level_5,
                "OE": system_rec.level_6
            }
            return module_map.get(module.upper(), 0) == 1
        return False