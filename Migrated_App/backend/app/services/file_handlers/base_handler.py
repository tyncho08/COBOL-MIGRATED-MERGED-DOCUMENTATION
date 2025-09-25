"""
Base File Handler - COBOL acas000-032 migration
Implements exact COBOL file handling logic with all operations
"""
from abc import ABC, abstractmethod
from typing import Any, Dict, Optional, List, Tuple
from enum import IntEnum
from sqlalchemy.orm import Session
from sqlalchemy.exc import SQLAlchemyError
import logging

from app.core.database import get_db
from app.models.audit import AuditLog

logger = logging.getLogger(__name__)


class FileFunction(IntEnum):
    """COBOL File Functions from wsfnctn.cob"""
    OPEN = 1
    CLOSE = 2
    READ_NEXT = 3
    READ_INDEXED = 4
    WRITE = 5
    DELETE_ALL = 6
    REWRITE = 7
    DELETE = 8
    START = 9
    WRITE_RAW = 15
    READ_NEXT_RAW = 13
    READ_GREATER = 21
    READ_PREV = 28
    READ_PREV_STOP = 29
    READ_NEXT_STOP = 30
    READ_HEADER = 34
    NOT_GREATER_THAN = 35
    NOT_LESS_THAN = 37
    GREATER_THAN = 39
    READ_WITH_HOLD = 40


class FileStatus:
    """COBOL File Status Codes"""
    SUCCESS = "00"
    END_OF_FILE = "10"
    KEY_NOT_FOUND = "23"
    DUPLICATE_KEY = "22"
    LOCKED = "30"
    INVALID_KEY = "21"
    PERMANENT_ERROR = "37"
    BOUNDARY_VIOLATION = "34"
    NO_NEXT_RECORD = "46"
    PERMISSION_DENIED = "37"
    FILE_NOT_FOUND = "35"
    

class COBOLError:
    """COBOL Error Codes (WE-Error)"""
    SUCCESS = 0
    INVALID_FUNCTION = 901
    KEY_NOT_FOUND = 902
    DUPLICATE_KEY = 903
    LOCKED_RECORD = 904
    DATABASE_ERROR = 905
    INVALID_KEY_TYPE = 998
    GENERAL_ERROR = 999


class FileAccess:
    """COBOL File Access structure from wsfnctn.cob"""
    def __init__(self):
        self.we_error: int = 0
        self.rrn: int = 0  # Relative record number
        self.fs_reply: str = "00"
        self.s1: str = " "
        self.cursor: int = 0
        self.fs_action: str = " " * 22
        self.file_key_no: int = 1
        self.sql_err: str = "00000"
        self.sql_msg: str = ""
        self.sql_state: str = "00000"
        self.file_function: int = 0
        

class BaseFileHandler(ABC):
    """
    Base class for all COBOL file handlers
    Implements exact COBOL logic from acas000-032
    """
    
    def __init__(self, db: Session, file_number: int, table_name: str):
        self.db = db
        self.file_number = file_number
        self.table_name = table_name
        self.file_access = FileAccess()
        self.is_open = False
        self.current_position = None
        self.lock_mode = False
        
    @abstractmethod
    def get_model_class(self):
        """Return the SQLAlchemy model class for this handler"""
        pass
        
    @abstractmethod
    def get_key_field(self, key_type: int = 1):
        """Get the field name for the specified key type"""
        pass
        
    def process(self, function: int, record: Any = None, key_value: Any = None, 
                key_type: int = 1) -> Tuple[Any, FileAccess]:
        """
        Main entry point - processes file operations like COBOL
        Returns (record, file_access)
        """
        self.file_access = FileAccess()
        self.file_access.file_function = function
        self.file_access.file_key_no = key_type
        
        try:
            if function == FileFunction.OPEN:
                return self._open_file()
            elif function == FileFunction.CLOSE:
                return self._close_file()
            elif function == FileFunction.READ_NEXT:
                return self._read_next()
            elif function == FileFunction.READ_INDEXED:
                return self._read_indexed(key_value, key_type)
            elif function == FileFunction.WRITE:
                return self._write_record(record)
            elif function == FileFunction.REWRITE:
                return self._rewrite_record(record, key_value, key_type)
            elif function == FileFunction.DELETE:
                return self._delete_record(key_value, key_type)
            elif function == FileFunction.DELETE_ALL:
                return self._delete_all()
            elif function == FileFunction.START:
                return self._start(key_value, key_type)
            elif function == FileFunction.READ_GREATER:
                return self._read_greater(key_value, key_type)
            elif function == FileFunction.READ_PREV:
                return self._read_previous()
            elif function == FileFunction.NOT_LESS_THAN:
                return self._read_not_less_than(key_value, key_type)
            else:
                self.file_access.we_error = COBOLError.INVALID_FUNCTION
                self.file_access.fs_reply = "99"
                return None, self.file_access
                
        except SQLAlchemyError as e:
            logger.error(f"Database error in {self.table_name}: {str(e)}")
            self.file_access.we_error = COBOLError.DATABASE_ERROR
            self.file_access.fs_reply = "37"
            self.file_access.sql_err = str(e.orig.pgcode if hasattr(e, 'orig') and hasattr(e.orig, 'pgcode') else "99999")
            self.file_access.sql_msg = str(e)
            return None, self.file_access
            
    def _open_file(self) -> Tuple[None, FileAccess]:
        """Open file - sets state"""
        self.is_open = True
        self.current_position = None
        self.file_access.fs_reply = FileStatus.SUCCESS
        self.file_access.fs_action = "OPEN"
        return None, self.file_access
        
    def _close_file(self) -> Tuple[None, FileAccess]:
        """Close file - clears state"""
        self.is_open = False
        self.current_position = None
        self.file_access.fs_reply = FileStatus.SUCCESS
        self.file_access.fs_action = "CLOSE"
        return None, self.file_access
        
    def _read_indexed(self, key_value: Any, key_type: int = 1) -> Tuple[Any, FileAccess]:
        """Read by key - exact match"""
        if not self.is_open:
            self.file_access.we_error = COBOLError.GENERAL_ERROR
            self.file_access.fs_reply = "47"
            return None, self.file_access
            
        model = self.get_model_class()
        key_field = self.get_key_field(key_type)
        
        if not key_field:
            self.file_access.we_error = COBOLError.INVALID_KEY_TYPE
            self.file_access.fs_reply = "99"
            return None, self.file_access
            
        record = self.db.query(model).filter(
            getattr(model, key_field) == key_value
        ).first()
        
        if record:
            self.current_position = key_value
            self.file_access.fs_reply = FileStatus.SUCCESS
            self.file_access.fs_action = "READ-INDEXED"
            self._log_access("READ", record)
        else:
            self.file_access.we_error = COBOLError.KEY_NOT_FOUND
            self.file_access.fs_reply = FileStatus.KEY_NOT_FOUND
            
        return record, self.file_access
        
    def _read_next(self) -> Tuple[Any, FileAccess]:
        """Read next record in sequence"""
        if not self.is_open:
            self.file_access.we_error = COBOLError.GENERAL_ERROR
            self.file_access.fs_reply = "47"
            return None, self.file_access
            
        model = self.get_model_class()
        key_field = self.get_key_field()
        
        query = self.db.query(model)
        
        if self.current_position is not None:
            query = query.filter(getattr(model, key_field) > self.current_position)
            
        query = query.order_by(getattr(model, key_field))
        record = query.first()
        
        if record:
            self.current_position = getattr(record, key_field)
            self.file_access.fs_reply = FileStatus.SUCCESS
            self.file_access.fs_action = "READ-NEXT"
            self._log_access("READ", record)
        else:
            self.file_access.fs_reply = FileStatus.END_OF_FILE
            self.file_access.we_error = 0
            
        return record, self.file_access
        
    def _read_previous(self) -> Tuple[Any, FileAccess]:
        """Read previous record in sequence"""
        if not self.is_open:
            self.file_access.we_error = COBOLError.GENERAL_ERROR
            self.file_access.fs_reply = "47"
            return None, self.file_access
            
        model = self.get_model_class()
        key_field = self.get_key_field()
        
        query = self.db.query(model)
        
        if self.current_position is not None:
            query = query.filter(getattr(model, key_field) < self.current_position)
            
        query = query.order_by(getattr(model, key_field).desc())
        record = query.first()
        
        if record:
            self.current_position = getattr(record, key_field)
            self.file_access.fs_reply = FileStatus.SUCCESS
            self.file_access.fs_action = "READ-PREV"
            self._log_access("READ", record)
        else:
            self.file_access.fs_reply = FileStatus.END_OF_FILE
            
        return record, self.file_access
        
    def _write_record(self, record: Any) -> Tuple[Any, FileAccess]:
        """Write new record"""
        if not self.is_open:
            self.file_access.we_error = COBOLError.GENERAL_ERROR
            self.file_access.fs_reply = "47"
            return None, self.file_access
            
        try:
            self.db.add(record)
            self.db.flush()  # Flush to check constraints
            
            self.file_access.fs_reply = FileStatus.SUCCESS
            self.file_access.fs_action = "WRITE"
            self._log_access("WRITE", record)
            
            # Update current position
            key_field = self.get_key_field()
            self.current_position = getattr(record, key_field)
            
        except SQLAlchemyError as e:
            if "duplicate key" in str(e).lower():
                self.file_access.we_error = COBOLError.DUPLICATE_KEY
                self.file_access.fs_reply = FileStatus.DUPLICATE_KEY
            else:
                raise
                
        return record, self.file_access
        
    def _rewrite_record(self, record: Any, key_value: Any = None, key_type: int = 1) -> Tuple[Any, FileAccess]:
        """Update existing record"""
        if not self.is_open:
            self.file_access.we_error = COBOLError.GENERAL_ERROR
            self.file_access.fs_reply = "47"
            return None, self.file_access
            
        # If key provided, read first
        if key_value:
            existing, _ = self._read_indexed(key_value, key_type)
            if not existing:
                return None, self.file_access
                
        # Update the record
        self.db.merge(record)
        self.db.flush()
        
        self.file_access.fs_reply = FileStatus.SUCCESS
        self.file_access.fs_action = "REWRITE"
        self._log_access("UPDATE", record)
        
        return record, self.file_access
        
    def _delete_record(self, key_value: Any, key_type: int = 1) -> Tuple[None, FileAccess]:
        """Delete record by key"""
        if not self.is_open:
            self.file_access.we_error = COBOLError.GENERAL_ERROR
            self.file_access.fs_reply = "47"
            return None, self.file_access
            
        record, _ = self._read_indexed(key_value, key_type)
        if not record:
            return None, self.file_access
            
        self.db.delete(record)
        self.db.flush()
        
        self.file_access.fs_reply = FileStatus.SUCCESS
        self.file_access.fs_action = "DELETE"
        self._log_access("DELETE", record)
        
        return None, self.file_access
        
    def _delete_all(self) -> Tuple[None, FileAccess]:
        """Delete all records - DANGEROUS"""
        if not self.is_open:
            self.file_access.we_error = COBOLError.GENERAL_ERROR
            self.file_access.fs_reply = "47"
            return None, self.file_access
            
        model = self.get_model_class()
        count = self.db.query(model).count()
        
        self.db.query(model).delete()
        self.db.flush()
        
        self.file_access.fs_reply = FileStatus.SUCCESS
        self.file_access.fs_action = "DELETE-ALL"
        self.file_access.rrn = count  # Return count of deleted records
        
        # Log the mass deletion
        AuditLog.log_action(
            db_session=self.db,
            audit_user="SYSTEM",
            audit_action="DELETE_ALL",
            audit_table=self.table_name,
            audit_old_values={"count": count}
        )
        
        return None, self.file_access
        
    def _start(self, key_value: Any, key_type: int = 1) -> Tuple[None, FileAccess]:
        """Position at key for sequential read"""
        if not self.is_open:
            self.file_access.we_error = COBOLError.GENERAL_ERROR
            self.file_access.fs_reply = "47"
            return None, self.file_access
            
        self.current_position = key_value
        self.file_access.fs_reply = FileStatus.SUCCESS
        self.file_access.fs_action = "START"
        
        return None, self.file_access
        
    def _read_greater(self, key_value: Any, key_type: int = 1) -> Tuple[Any, FileAccess]:
        """Read first record with key > given value"""
        if not self.is_open:
            self.file_access.we_error = COBOLError.GENERAL_ERROR
            self.file_access.fs_reply = "47"
            return None, self.file_access
            
        model = self.get_model_class()
        key_field = self.get_key_field(key_type)
        
        record = self.db.query(model).filter(
            getattr(model, key_field) > key_value
        ).order_by(getattr(model, key_field)).first()
        
        if record:
            self.current_position = getattr(record, key_field)
            self.file_access.fs_reply = FileStatus.SUCCESS
            self.file_access.fs_action = "READ-GREATER"
            self._log_access("READ", record)
        else:
            self.file_access.fs_reply = FileStatus.END_OF_FILE
            
        return record, self.file_access
        
    def _read_not_less_than(self, key_value: Any, key_type: int = 1) -> Tuple[Any, FileAccess]:
        """Read first record with key >= given value"""
        if not self.is_open:
            self.file_access.we_error = COBOLError.GENERAL_ERROR
            self.file_access.fs_reply = "47"
            return None, self.file_access
            
        model = self.get_model_class()
        key_field = self.get_key_field(key_type)
        
        record = self.db.query(model).filter(
            getattr(model, key_field) >= key_value
        ).order_by(getattr(model, key_field)).first()
        
        if record:
            self.current_position = getattr(record, key_field)
            self.file_access.fs_reply = FileStatus.SUCCESS
            self.file_access.fs_action = "NOT-LESS-THAN"
            self._log_access("READ", record)
        else:
            self.file_access.fs_reply = FileStatus.END_OF_FILE
            
        return record, self.file_access
        
    def _log_access(self, action: str, record: Any):
        """Log file access for audit trail"""
        if hasattr(record, '__table__'):
            # Get primary key value
            pk_columns = record.__table__.primary_key.columns
            pk_values = {col.name: getattr(record, col.name) for col in pk_columns}
            key_str = str(pk_values)
        else:
            key_str = str(record)
            
        logger.debug(f"{action} {self.table_name}: {key_str}")
        
    def lock_record(self, key_value: Any, key_type: int = 1) -> bool:
        """Lock record for update - COBOL style"""
        # In PostgreSQL, we use SELECT FOR UPDATE
        model = self.get_model_class()
        key_field = self.get_key_field(key_type)
        
        record = self.db.query(model).filter(
            getattr(model, key_field) == key_value
        ).with_for_update().first()
        
        return record is not None