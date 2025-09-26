"""
COBOL Data Reader

Reads and parses COBOL data files including fixed-width records, 
EBCDIC encoding, packed decimals, and various COBOL data types.
Handles the actual legacy data formats from 49 years of ACAS operation.
"""

import os
import struct
import logging
from typing import Dict, List, Any, Optional, Union, BinaryIO
from datetime import datetime, date
from decimal import Decimal, getcontext, ROUND_HALF_UP
from dataclasses import dataclass
import codecs

# Set decimal precision for financial calculations
getcontext().prec = 28
getcontext().rounding = ROUND_HALF_UP

logger = logging.getLogger(__name__)


@dataclass
class FieldDefinition:
    """Definition of a COBOL field"""
    name: str
    start_pos: int
    length: int
    data_type: str  # CHAR, NUMERIC, PACKED, COMP, COMP-3, DATE
    decimal_places: int = 0
    signed: bool = False
    filler: bool = False
    description: str = ""


@dataclass
class RecordLayout:
    """Layout definition for a COBOL record"""
    record_name: str
    record_length: int
    fields: List[FieldDefinition]
    description: str = ""


class CobolDataReader:
    """Main class for reading COBOL data files"""
    
    def __init__(self, encoding: str = 'ebcdic-cp-us'):
        self.encoding = encoding
        self.record_layouts = {}
        self._initialize_standard_layouts()
    
    def _initialize_standard_layouts(self):
        """Initialize standard ACAS COBOL record layouts"""
        
        # Customer Master File Layout (CUSMAS.DAT)
        customer_fields = [
            FieldDefinition("customer_code", 1, 6, "CHAR", description="Customer Code"),
            FieldDefinition("customer_name", 7, 30, "CHAR", description="Customer Name"),
            FieldDefinition("address_line1", 37, 30, "CHAR", description="Address Line 1"),
            FieldDefinition("address_line2", 67, 30, "CHAR", description="Address Line 2"),
            FieldDefinition("city", 97, 20, "CHAR", description="City"),
            FieldDefinition("postal_code", 117, 10, "CHAR", description="Postal Code"),
            FieldDefinition("country_code", 127, 3, "CHAR", description="Country Code"),
            FieldDefinition("phone_number", 130, 15, "CHAR", description="Phone Number"),
            FieldDefinition("fax_number", 145, 15, "CHAR", description="Fax Number"),
            FieldDefinition("email_address", 160, 50, "CHAR", description="Email Address"),
            FieldDefinition("credit_limit", 210, 7, "PACKED", 2, description="Credit Limit"),
            FieldDefinition("current_balance", 217, 7, "PACKED", 2, signed=True, description="Current Balance"),
            FieldDefinition("territory_code", 224, 3, "CHAR", description="Territory Code"),
            FieldDefinition("salesman_code", 227, 4, "CHAR", description="Salesman Code"),
            FieldDefinition("payment_terms", 231, 2, "CHAR", description="Payment Terms"),
            FieldDefinition("discount_percent", 233, 3, "PACKED", 2, description="Discount %"),
            FieldDefinition("tax_code", 236, 2, "CHAR", description="Tax Code"),
            FieldDefinition("status_code", 238, 1, "CHAR", description="Status (A/I/D)"),
            FieldDefinition("date_created", 239, 8, "DATE", description="Date Created"),
            FieldDefinition("last_modified", 247, 8, "DATE", description="Last Modified"),
            FieldDefinition("last_invoice_date", 255, 8, "DATE", description="Last Invoice Date"),
            FieldDefinition("ytd_sales", 263, 9, "PACKED", 2, description="YTD Sales"),
            FieldDefinition("last_year_sales", 272, 9, "PACKED", 2, description="Last Year Sales"),
            FieldDefinition("filler", 281, 20, "CHAR", filler=True)
        ]
        
        self.record_layouts['CUSMAS'] = RecordLayout(
            "Customer Master", 300, customer_fields,
            "Customer master file from COBOL system"
        )
        
        # Supplier Master File Layout (SUPMAS.DAT)
        supplier_fields = [
            FieldDefinition("supplier_code", 1, 6, "CHAR", description="Supplier Code"),
            FieldDefinition("supplier_name", 7, 40, "CHAR", description="Supplier Name"),
            FieldDefinition("address_line1", 47, 30, "CHAR", description="Address Line 1"),
            FieldDefinition("address_line2", 77, 30, "CHAR", description="Address Line 2"),
            FieldDefinition("city", 107, 20, "CHAR", description="City"),
            FieldDefinition("postal_code", 127, 10, "CHAR", description="Postal Code"),
            FieldDefinition("country_code", 137, 3, "CHAR", description="Country Code"),
            FieldDefinition("phone_number", 140, 15, "CHAR", description="Phone Number"),
            FieldDefinition("fax_number", 155, 15, "CHAR", description="Fax Number"),
            FieldDefinition("email_address", 170, 50, "CHAR", description="Email Address"),
            FieldDefinition("contact_person", 220, 30, "CHAR", description="Contact Person"),
            FieldDefinition("payment_terms", 250, 2, "CHAR", description="Payment Terms"),
            FieldDefinition("tax_registration", 252, 15, "CHAR", description="Tax Registration"),
            FieldDefinition("currency_code", 267, 3, "CHAR", description="Currency Code"),
            FieldDefinition("status_code", 270, 1, "CHAR", description="Status (A/I/D)"),
            FieldDefinition("date_created", 271, 8, "DATE", description="Date Created"),
            FieldDefinition("last_modified", 279, 8, "DATE", description="Last Modified"),
            FieldDefinition("ytd_purchases", 287, 9, "PACKED", 2, description="YTD Purchases"),
            FieldDefinition("last_year_purchases", 296, 9, "PACKED", 2, description="Last Year Purchases"),
            FieldDefinition("filler", 305, 20, "CHAR", filler=True)
        ]
        
        self.record_layouts['SUPMAS'] = RecordLayout(
            "Supplier Master", 324, supplier_fields,
            "Supplier master file from COBOL system"
        )
        
        # Stock Item Master File Layout (STKMAS.DAT)
        stock_fields = [
            FieldDefinition("item_code", 1, 15, "CHAR", description="Stock Item Code"),
            FieldDefinition("item_name", 16, 40, "CHAR", description="Item Description"),
            FieldDefinition("item_category", 56, 8, "CHAR", description="Item Category"),
            FieldDefinition("unit_of_measure", 64, 4, "CHAR", description="Unit of Measure"),
            FieldDefinition("standard_cost", 68, 7, "PACKED", 4, description="Standard Cost"),
            FieldDefinition("average_cost", 75, 7, "PACKED", 4, description="Average Cost"),
            FieldDefinition("last_cost", 82, 7, "PACKED", 4, description="Last Cost"),
            FieldDefinition("selling_price", 89, 7, "PACKED", 4, description="Selling Price"),
            FieldDefinition("quantity_on_hand", 96, 7, "PACKED", 2, signed=True, description="Qty On Hand"),
            FieldDefinition("quantity_allocated", 103, 7, "PACKED", 2, description="Qty Allocated"),
            FieldDefinition("quantity_on_order", 110, 7, "PACKED", 2, description="Qty On Order"),
            FieldDefinition("reorder_level", 117, 7, "PACKED", 2, description="Reorder Level"),
            FieldDefinition("reorder_quantity", 124, 7, "PACKED", 2, description="Reorder Quantity"),
            FieldDefinition("lead_time_days", 131, 3, "NUMERIC", description="Lead Time Days"),
            FieldDefinition("costing_method", 134, 1, "CHAR", description="Costing Method"),
            FieldDefinition("abc_classification", 135, 1, "CHAR", description="ABC Class"),
            FieldDefinition("supplier_code", 136, 6, "CHAR", description="Primary Supplier"),
            FieldDefinition("location_code", 142, 4, "CHAR", description="Primary Location"),
            FieldDefinition("status_code", 146, 1, "CHAR", description="Status (A/I/D)"),
            FieldDefinition("date_created", 147, 8, "DATE", description="Date Created"),
            FieldDefinition("last_modified", 155, 8, "DATE", description="Last Modified"),
            FieldDefinition("last_movement_date", 163, 8, "DATE", description="Last Movement"),
            FieldDefinition("last_sale_date", 171, 8, "DATE", description="Last Sale Date"),
            FieldDefinition("last_purchase_date", 179, 8, "DATE", description="Last Purchase"),
            FieldDefinition("ytd_sales_qty", 187, 7, "PACKED", 2, description="YTD Sales Qty"),
            FieldDefinition("ytd_sales_value", 194, 9, "PACKED", 2, description="YTD Sales Value"),
            FieldDefinition("filler", 203, 22, "CHAR", filler=True)
        ]
        
        self.record_layouts['STKMAS'] = RecordLayout(
            "Stock Item Master", 224, stock_fields,
            "Stock item master file from COBOL system"
        )
        
        # GL Account Master File Layout (GLMAS.DAT)
        gl_fields = [
            FieldDefinition("account_code", 1, 8, "CHAR", description="GL Account Code"),
            FieldDefinition("account_name", 9, 40, "CHAR", description="Account Name"),
            FieldDefinition("account_type", 49, 2, "CHAR", description="Account Type"),
            FieldDefinition("account_category", 51, 10, "CHAR", description="Account Category"),
            FieldDefinition("normal_balance", 61, 1, "CHAR", description="Normal Balance (D/C)"),
            FieldDefinition("budget_code", 62, 4, "CHAR", description="Budget Code"),
            FieldDefinition("department_code", 66, 4, "CHAR", description="Department Code"),
            FieldDefinition("consolidation_code", 70, 8, "CHAR", description="Consolidation Code"),
            FieldDefinition("status_code", 78, 1, "CHAR", description="Status (A/I)"),
            FieldDefinition("opening_balance", 79, 9, "PACKED", 2, signed=True, description="Opening Balance"),
            FieldDefinition("ytd_debits", 88, 9, "PACKED", 2, description="YTD Debits"),
            FieldDefinition("ytd_credits", 97, 9, "PACKED", 2, description="YTD Credits"),
            FieldDefinition("last_year_debits", 106, 9, "PACKED", 2, description="Last Year Debits"),
            FieldDefinition("last_year_credits", 115, 9, "PACKED", 2, description="Last Year Credits"),
            FieldDefinition("budget_amount", 124, 9, "PACKED", 2, description="Budget Amount"),
            FieldDefinition("date_created", 133, 8, "DATE", description="Date Created"),
            FieldDefinition("last_modified", 141, 8, "DATE", description="Last Modified"),
            FieldDefinition("filler", 149, 26, "CHAR", filler=True)
        ]
        
        self.record_layouts['GLMAS'] = RecordLayout(
            "GL Account Master", 174, gl_fields,
            "General Ledger account master file from COBOL system"
        )
        
        # GL Transaction File Layout (GLTRAN.DAT)
        gltran_fields = [
            FieldDefinition("transaction_id", 1, 10, "NUMERIC", description="Transaction ID"),
            FieldDefinition("journal_number", 11, 8, "NUMERIC", description="Journal Number"),
            FieldDefinition("account_code", 19, 8, "CHAR", description="GL Account Code"),
            FieldDefinition("transaction_date", 27, 8, "DATE", description="Transaction Date"),
            FieldDefinition("transaction_type", 35, 1, "CHAR", description="Type (D/C)"),
            FieldDefinition("amount", 36, 9, "PACKED", 2, description="Transaction Amount"),
            FieldDefinition("reference", 45, 20, "CHAR", description="Reference"),
            FieldDefinition("description", 65, 40, "CHAR", description="Description"),
            FieldDefinition("source_code", 105, 2, "CHAR", description="Source Module"),
            FieldDefinition("batch_number", 107, 8, "NUMERIC", description="Batch Number"),
            FieldDefinition("period_id", 115, 6, "CHAR", description="Period ID"),
            FieldDefinition("user_id", 121, 8, "CHAR", description="User ID"),
            FieldDefinition("posted_flag", 129, 1, "CHAR", description="Posted Flag"),
            FieldDefinition("posted_date", 130, 8, "DATE", description="Posted Date"),
            FieldDefinition("filler", 138, 12, "CHAR", filler=True)
        ]
        
        self.record_layouts['GLTRAN'] = RecordLayout(
            "GL Transaction", 149, gltran_fields,
            "General Ledger transaction file from COBOL system"
        )
        
        # Sales Invoice Header (SLHDR.DAT)
        slhdr_fields = [
            FieldDefinition("invoice_id", 1, 10, "NUMERIC", description="Invoice ID"),
            FieldDefinition("invoice_number", 11, 10, "CHAR", description="Invoice Number"),
            FieldDefinition("customer_code", 21, 6, "CHAR", description="Customer Code"),
            FieldDefinition("invoice_date", 27, 8, "DATE", description="Invoice Date"),
            FieldDefinition("due_date", 35, 8, "DATE", description="Due Date"),
            FieldDefinition("order_number", 43, 15, "CHAR", description="Order Number"),
            FieldDefinition("salesman_code", 58, 4, "CHAR", description="Salesman Code"),
            FieldDefinition("territory_code", 62, 3, "CHAR", description="Territory Code"),
            FieldDefinition("currency_code", 65, 3, "CHAR", description="Currency Code"),
            FieldDefinition("exchange_rate", 68, 7, "PACKED", 6, description="Exchange Rate"),
            FieldDefinition("subtotal", 75, 9, "PACKED", 2, description="Subtotal"),
            FieldDefinition("discount_amount", 84, 7, "PACKED", 2, description="Discount Amount"),
            FieldDefinition("tax_amount", 91, 7, "PACKED", 2, description="Tax Amount"),
            FieldDefinition("freight_amount", 98, 7, "PACKED", 2, description="Freight Amount"),
            FieldDefinition("total_amount", 105, 9, "PACKED", 2, description="Total Amount"),
            FieldDefinition("paid_amount", 114, 9, "PACKED", 2, description="Paid Amount"),
            FieldDefinition("outstanding_amount", 123, 9, "PACKED", 2, description="Outstanding"),
            FieldDefinition("payment_terms", 132, 2, "CHAR", description="Payment Terms"),
            FieldDefinition("status_code", 134, 1, "CHAR", description="Status"),
            FieldDefinition("created_by", 135, 8, "CHAR", description="Created By"),
            FieldDefinition("date_created", 143, 8, "DATE", description="Date Created"),
            FieldDefinition("last_modified", 151, 8, "DATE", description="Last Modified"),
            FieldDefinition("filler", 159, 16, "CHAR", filler=True)
        ]
        
        self.record_layouts['SLHDR'] = RecordLayout(
            "Sales Invoice Header", 174, slhdr_fields,
            "Sales invoice header from COBOL system"
        )
    
    def read_file(self, file_path: str, layout_name: str, 
                  max_records: Optional[int] = None) -> List[Dict[str, Any]]:
        """
        Read a COBOL data file and return parsed records
        
        Args:
            file_path: Path to the COBOL data file
            layout_name: Name of the record layout to use
            max_records: Maximum number of records to read (for testing)
            
        Returns:
            List of dictionaries containing parsed record data
        """
        if layout_name not in self.record_layouts:
            raise ValueError(f"Unknown record layout: {layout_name}")
        
        layout = self.record_layouts[layout_name]
        records = []
        
        try:
            with open(file_path, 'rb') as file:
                record_count = 0
                
                while True:
                    if max_records and record_count >= max_records:
                        break
                    
                    # Read one record
                    raw_record = file.read(layout.record_length)
                    if not raw_record or len(raw_record) < layout.record_length:
                        break
                    
                    # Parse the record
                    try:
                        parsed_record = self._parse_record(raw_record, layout)
                        records.append(parsed_record)
                        record_count += 1
                        
                        if record_count % 1000 == 0:
                            logger.info(f"Processed {record_count} records from {file_path}")
                            
                    except Exception as e:
                        logger.error(f"Error parsing record {record_count + 1} in {file_path}: {str(e)}")
                        continue
                
                logger.info(f"Successfully read {len(records)} records from {file_path}")
                
        except FileNotFoundError:
            logger.error(f"File not found: {file_path}")
            raise
        except Exception as e:
            logger.error(f"Error reading file {file_path}: {str(e)}")
            raise
        
        return records
    
    def _parse_record(self, raw_record: bytes, layout: RecordLayout) -> Dict[str, Any]:
        """Parse a single COBOL record according to the layout"""
        parsed = {}
        
        for field in layout.fields:
            if field.filler:
                continue
                
            # Extract field data
            start = field.start_pos - 1  # Convert to 0-based indexing
            end = start + field.length
            field_data = raw_record[start:end]
            
            try:
                # Parse based on data type
                if field.data_type == "CHAR":
                    value = self._parse_char_field(field_data)
                elif field.data_type == "NUMERIC":
                    value = self._parse_numeric_field(field_data)
                elif field.data_type == "PACKED" or field.data_type == "COMP-3":
                    value = self._parse_packed_decimal(field_data, field.decimal_places, field.signed)
                elif field.data_type == "COMP":
                    value = self._parse_comp_field(field_data, field.signed)
                elif field.data_type == "DATE":
                    value = self._parse_date_field(field_data)
                else:
                    logger.warning(f"Unknown data type {field.data_type} for field {field.name}")
                    value = field_data.decode(self.encoding, errors='replace').strip()
                
                parsed[field.name] = value
                
            except Exception as e:
                logger.error(f"Error parsing field {field.name}: {str(e)}")
                parsed[field.name] = None
        
        return parsed
    
    def _parse_char_field(self, data: bytes) -> str:
        """Parse a character field"""
        try:
            # Try EBCDIC first, then ASCII
            try:
                return data.decode('ebcdic-cp-us').strip()
            except:
                return data.decode('ascii', errors='replace').strip()
        except Exception as e:
            logger.warning(f"Error decoding character field: {str(e)}")
            return data.decode('ascii', errors='replace').strip()
    
    def _parse_numeric_field(self, data: bytes) -> Optional[int]:
        """Parse a numeric field (zoned decimal)"""
        try:
            # Convert EBCDIC to ASCII and parse
            str_value = data.decode('ebcdic-cp-us', errors='replace').strip()
            if not str_value or str_value.isspace():
                return None
                
            # Handle signed numbers (last digit contains sign)
            if len(str_value) > 0:
                last_char = str_value[-1]
                if last_char in 'ABCDEFGHI':  # Positive overpunch
                    number_part = str_value[:-1] + str(ord(last_char) - ord('A'))
                    return int(number_part)
                elif last_char in 'JKLMNOPQR':  # Negative overpunch
                    number_part = str_value[:-1] + str(ord(last_char) - ord('J'))
                    return -int(number_part)
                else:
                    return int(str_value)
            
            return None
            
        except Exception as e:
            logger.warning(f"Error parsing numeric field: {str(e)}")
            return None
    
    def _parse_packed_decimal(self, data: bytes, decimal_places: int = 0, 
                             signed: bool = False) -> Optional[Decimal]:
        """Parse a packed decimal (COMP-3) field"""
        try:
            if not data:
                return None
            
            # Unpack the packed decimal
            value = 0
            sign = 1
            
            for i, byte in enumerate(data):
                if i == len(data) - 1:  # Last byte contains sign
                    high_digit = (byte >> 4) & 0x0F
                    sign_nibble = byte & 0x0F
                    
                    if high_digit <= 9:
                        value = value * 10 + high_digit
                    
                    # Check sign
                    if sign_nibble in [0x0B, 0x0D]:  # Negative
                        sign = -1
                    elif sign_nibble == 0x0C:  # Positive
                        sign = 1
                    elif sign_nibble == 0x0F:  # Unsigned positive
                        sign = 1
                else:
                    high_digit = (byte >> 4) & 0x0F
                    low_digit = byte & 0x0F
                    
                    if high_digit <= 9:
                        value = value * 10 + high_digit
                    if low_digit <= 9:
                        value = value * 10 + low_digit
            
            # Apply decimal places
            if decimal_places > 0:
                value = Decimal(value * sign) / (10 ** decimal_places)
            else:
                value = Decimal(value * sign)
            
            return value
            
        except Exception as e:
            logger.warning(f"Error parsing packed decimal: {str(e)}")
            return None
    
    def _parse_comp_field(self, data: bytes, signed: bool = False) -> Optional[int]:
        """Parse a COMP (binary) field"""
        try:
            if len(data) == 2:
                if signed:
                    return struct.unpack('>h', data)[0]  # Signed short
                else:
                    return struct.unpack('>H', data)[0]  # Unsigned short
            elif len(data) == 4:
                if signed:
                    return struct.unpack('>i', data)[0]  # Signed int
                else:
                    return struct.unpack('>I', data)[0]  # Unsigned int
            elif len(data) == 8:
                if signed:
                    return struct.unpack('>q', data)[0]  # Signed long long
                else:
                    return struct.unpack('>Q', data)[0]  # Unsigned long long
            else:
                logger.warning(f"Unsupported COMP field length: {len(data)}")
                return None
                
        except Exception as e:
            logger.warning(f"Error parsing COMP field: {str(e)}")
            return None
    
    def _parse_date_field(self, data: bytes) -> Optional[date]:
        """Parse a date field (YYYYMMDD format)"""
        try:
            date_str = data.decode('ebcdic-cp-us', errors='replace').strip()
            
            if not date_str or date_str == '00000000' or len(date_str) != 8:
                return None
            
            year = int(date_str[:4])
            month = int(date_str[4:6])
            day = int(date_str[6:8])
            
            # Validate date ranges
            if year < 1900 or year > 2100:
                return None
            if month < 1 or month > 12:
                return None
            if day < 1 or day > 31:
                return None
            
            return date(year, month, day)
            
        except Exception as e:
            logger.warning(f"Error parsing date field: {str(e)}")
            return None
    
    def get_layout_info(self, layout_name: str) -> Optional[RecordLayout]:
        """Get information about a record layout"""
        return self.record_layouts.get(layout_name)
    
    def list_layouts(self) -> List[str]:
        """List all available record layouts"""
        return list(self.record_layouts.keys())
    
    def validate_file_structure(self, file_path: str, layout_name: str) -> Dict[str, Any]:
        """
        Validate the structure of a COBOL data file
        
        Returns information about file size, record count estimates, etc.
        """
        if layout_name not in self.record_layouts:
            raise ValueError(f"Unknown record layout: {layout_name}")
        
        layout = self.record_layouts[layout_name]
        
        try:
            file_size = os.path.getsize(file_path)
            estimated_records = file_size // layout.record_length
            remaining_bytes = file_size % layout.record_length
            
            return {
                'file_path': file_path,
                'file_size_bytes': file_size,
                'record_length': layout.record_length,
                'estimated_records': estimated_records,
                'remaining_bytes': remaining_bytes,
                'structure_valid': remaining_bytes == 0,
                'layout_name': layout_name,
                'layout_description': layout.description
            }
            
        except Exception as e:
            logger.error(f"Error validating file structure: {str(e)}")
            raise