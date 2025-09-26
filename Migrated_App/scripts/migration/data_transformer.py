"""
Data Transformer

Transforms COBOL legacy data into PostgreSQL-compatible format.
Handles data type conversions, field mapping, business logic preservation,
and data cleansing for the modern database schema.
"""

import re
import logging
from typing import Dict, List, Any, Optional, Callable, Union
from datetime import datetime, date
from decimal import Decimal, getcontext, InvalidOperation
from dataclasses import dataclass
from enum import Enum
import uuid

# Set decimal precision for financial calculations
getcontext().prec = 28

logger = logging.getLogger(__name__)


class TransformationError(Exception):
    """Custom exception for data transformation errors"""
    pass


@dataclass
class FieldMapping:
    """Defines how a COBOL field maps to a PostgreSQL field"""
    cobol_field: str
    postgres_field: str
    transformer: Optional[Callable] = None
    required: bool = True
    default_value: Any = None
    validation_rules: List[Callable] = None


@dataclass
class TableMapping:
    """Defines how a COBOL record maps to a PostgreSQL table"""
    table_name: str
    field_mappings: List[FieldMapping]
    business_rules: List[Callable] = None
    post_processors: List[Callable] = None


class DataTransformer:
    """Main class for transforming COBOL data to PostgreSQL format"""
    
    def __init__(self):
        self.table_mappings = {}
        self.transformation_stats = {
            'records_processed': 0,
            'records_transformed': 0,
            'records_failed': 0,
            'validation_errors': 0,
            'business_rule_violations': 0
        }
        self._initialize_mappings()
    
    def _initialize_mappings(self):
        """Initialize COBOL to PostgreSQL field mappings"""
        
        # Customer Master Mapping
        customer_mappings = [
            FieldMapping("customer_code", "customer_code", self._clean_customer_code, required=True),
            FieldMapping("customer_name", "customer_name", self._clean_text_field, required=True),
            FieldMapping("address_line1", "address_line1", self._clean_text_field),
            FieldMapping("address_line2", "address_line2", self._clean_text_field),
            FieldMapping("city", "city", self._clean_text_field),
            FieldMapping("postal_code", "postal_code", self._clean_postal_code),
            FieldMapping("country_code", "country_code", self._clean_country_code),
            FieldMapping("phone_number", "phone_number", self._clean_phone_number),
            FieldMapping("fax_number", "fax_number", self._clean_phone_number),
            FieldMapping("email_address", "email_address", self._clean_email),
            FieldMapping("credit_limit", "credit_limit", self._clean_decimal, default_value=Decimal('0.00')),
            FieldMapping("current_balance", "current_balance", self._clean_decimal, default_value=Decimal('0.00')),
            FieldMapping("territory_code", "territory_code", self._clean_code_field),
            FieldMapping("salesman_code", "salesman_code", self._clean_code_field),
            FieldMapping("payment_terms", "payment_terms", self._clean_payment_terms),
            FieldMapping("discount_percent", "discount_percent", self._clean_percentage),
            FieldMapping("tax_code", "tax_code", self._clean_code_field),
            FieldMapping("status_code", "status", self._transform_customer_status, default_value='active'),
            FieldMapping("date_created", "created_at", self._clean_date),
            FieldMapping("last_modified", "updated_at", self._clean_date),
            FieldMapping("last_invoice_date", "last_invoice_date", self._clean_date),
            FieldMapping("ytd_sales", "ytd_sales", self._clean_decimal, default_value=Decimal('0.00')),
            FieldMapping("last_year_sales", "last_year_sales", self._clean_decimal, default_value=Decimal('0.00'))
        ]
        
        self.table_mappings['customers'] = TableMapping(
            'customers', 
            customer_mappings,
            business_rules=[self._validate_customer_business_rules],
            post_processors=[self._generate_customer_uuid]
        )
        
        # Supplier Master Mapping
        supplier_mappings = [
            FieldMapping("supplier_code", "supplier_code", self._clean_supplier_code, required=True),
            FieldMapping("supplier_name", "supplier_name", self._clean_text_field, required=True),
            FieldMapping("address_line1", "address_line1", self._clean_text_field),
            FieldMapping("address_line2", "address_line2", self._clean_text_field),
            FieldMapping("city", "city", self._clean_text_field),
            FieldMapping("postal_code", "postal_code", self._clean_postal_code),
            FieldMapping("country_code", "country_code", self._clean_country_code),
            FieldMapping("phone_number", "phone_number", self._clean_phone_number),
            FieldMapping("fax_number", "fax_number", self._clean_phone_number),
            FieldMapping("email_address", "email_address", self._clean_email),
            FieldMapping("contact_person", "contact_person", self._clean_text_field),
            FieldMapping("payment_terms", "payment_terms", self._clean_payment_terms),
            FieldMapping("tax_registration", "tax_registration", self._clean_text_field),
            FieldMapping("currency_code", "currency_code", self._clean_currency_code),
            FieldMapping("status_code", "status", self._transform_supplier_status, default_value='active'),
            FieldMapping("date_created", "created_at", self._clean_date),
            FieldMapping("last_modified", "updated_at", self._clean_date),
            FieldMapping("ytd_purchases", "ytd_purchases", self._clean_decimal, default_value=Decimal('0.00')),
            FieldMapping("last_year_purchases", "last_year_purchases", self._clean_decimal, default_value=Decimal('0.00'))
        ]
        
        self.table_mappings['suppliers'] = TableMapping(
            'suppliers',
            supplier_mappings,
            business_rules=[self._validate_supplier_business_rules],
            post_processors=[self._generate_supplier_uuid]
        )
        
        # Stock Item Master Mapping
        stock_mappings = [
            FieldMapping("item_code", "item_code", self._clean_item_code, required=True),
            FieldMapping("item_name", "item_name", self._clean_text_field, required=True),
            FieldMapping("item_category", "item_category", self._clean_category_code),
            FieldMapping("unit_of_measure", "unit_of_measure", self._clean_uom),
            FieldMapping("standard_cost", "standard_cost", self._clean_decimal, default_value=Decimal('0.00')),
            FieldMapping("average_cost", "average_cost", self._clean_decimal, default_value=Decimal('0.00')),
            FieldMapping("last_cost", "last_cost", self._clean_decimal, default_value=Decimal('0.00')),
            FieldMapping("selling_price", "selling_price", self._clean_decimal, default_value=Decimal('0.00')),
            FieldMapping("quantity_on_hand", "quantity_on_hand", self._clean_integer, default_value=0),
            FieldMapping("quantity_allocated", "quantity_allocated", self._clean_integer, default_value=0),
            FieldMapping("quantity_on_order", "quantity_on_order", self._clean_integer, default_value=0),
            FieldMapping("reorder_level", "reorder_level", self._clean_integer, default_value=0),
            FieldMapping("reorder_quantity", "reorder_quantity", self._clean_integer, default_value=0),
            FieldMapping("lead_time_days", "lead_time_days", self._clean_integer, default_value=0),
            FieldMapping("costing_method", "costing_method", self._transform_costing_method),
            FieldMapping("abc_classification", "abc_classification", self._clean_abc_class),
            FieldMapping("supplier_code", "primary_supplier_code", self._clean_supplier_code),
            FieldMapping("location_code", "primary_location_code", self._clean_location_code),
            FieldMapping("status_code", "status", self._transform_stock_status, default_value='active'),
            FieldMapping("date_created", "created_at", self._clean_date),
            FieldMapping("last_modified", "updated_at", self._clean_date),
            FieldMapping("last_movement_date", "last_movement_date", self._clean_date),
            FieldMapping("last_sale_date", "last_sale_date", self._clean_date),
            FieldMapping("last_purchase_date", "last_purchase_date", self._clean_date),
            FieldMapping("ytd_sales_qty", "ytd_sales_quantity", self._clean_integer, default_value=0),
            FieldMapping("ytd_sales_value", "ytd_sales_value", self._clean_decimal, default_value=Decimal('0.00'))
        ]
        
        self.table_mappings['stock_items'] = TableMapping(
            'stock_items',
            stock_mappings,
            business_rules=[self._validate_stock_business_rules],
            post_processors=[self._calculate_stock_metrics]
        )
        
        # GL Account Master Mapping
        gl_account_mappings = [
            FieldMapping("account_code", "account_code", self._clean_account_code, required=True),
            FieldMapping("account_name", "account_name", self._clean_text_field, required=True),
            FieldMapping("account_type", "account_type", self._transform_account_type, required=True),
            FieldMapping("account_category", "account_category", self._clean_account_category),
            FieldMapping("normal_balance", "normal_balance", self._transform_normal_balance),
            FieldMapping("budget_code", "budget_code", self._clean_code_field),
            FieldMapping("department_code", "department_code", self._clean_code_field),
            FieldMapping("consolidation_code", "consolidation_code", self._clean_code_field),
            FieldMapping("status_code", "status", self._transform_account_status, default_value='active'),
            FieldMapping("opening_balance", "opening_balance", self._clean_decimal, default_value=Decimal('0.00')),
            FieldMapping("ytd_debits", "ytd_debits", self._clean_decimal, default_value=Decimal('0.00')),
            FieldMapping("ytd_credits", "ytd_credits", self._clean_decimal, default_value=Decimal('0.00')),
            FieldMapping("budget_amount", "budget_amount", self._clean_decimal, default_value=Decimal('0.00')),
            FieldMapping("date_created", "created_at", self._clean_date),
            FieldMapping("last_modified", "updated_at", self._clean_date)
        ]
        
        self.table_mappings['gl_accounts'] = TableMapping(
            'gl_accounts',
            gl_account_mappings,
            business_rules=[self._validate_gl_account_business_rules],
            post_processors=[self._calculate_account_balance]
        )
        
        # Sales Invoice Header Mapping
        sales_invoice_mappings = [
            FieldMapping("invoice_id", "invoice_id", self._clean_integer, required=True),
            FieldMapping("invoice_number", "invoice_number", self._clean_invoice_number, required=True),
            FieldMapping("customer_code", "customer_code", self._clean_customer_code, required=True),
            FieldMapping("invoice_date", "invoice_date", self._clean_date, required=True),
            FieldMapping("due_date", "due_date", self._clean_date),
            FieldMapping("order_number", "order_number", self._clean_text_field),
            FieldMapping("salesman_code", "salesman_code", self._clean_code_field),
            FieldMapping("territory_code", "territory_code", self._clean_code_field),
            FieldMapping("currency_code", "currency_code", self._clean_currency_code),
            FieldMapping("exchange_rate", "exchange_rate", self._clean_exchange_rate),
            FieldMapping("subtotal", "subtotal", self._clean_decimal, default_value=Decimal('0.00')),
            FieldMapping("discount_amount", "discount_amount", self._clean_decimal, default_value=Decimal('0.00')),
            FieldMapping("tax_amount", "tax_amount", self._clean_decimal, default_value=Decimal('0.00')),
            FieldMapping("freight_amount", "freight_amount", self._clean_decimal, default_value=Decimal('0.00')),
            FieldMapping("total_amount", "invoice_total", self._clean_decimal, required=True),
            FieldMapping("paid_amount", "paid_amount", self._clean_decimal, default_value=Decimal('0.00')),
            FieldMapping("outstanding_amount", "outstanding_amount", self._clean_decimal, default_value=Decimal('0.00')),
            FieldMapping("payment_terms", "payment_terms", self._clean_payment_terms),
            FieldMapping("status_code", "status", self._transform_invoice_status, default_value='open'),
            FieldMapping("created_by", "created_by", self._clean_text_field),
            FieldMapping("date_created", "created_at", self._clean_date),
            FieldMapping("last_modified", "updated_at", self._clean_date)
        ]
        
        self.table_mappings['sales_invoices'] = TableMapping(
            'sales_invoices',
            sales_invoice_mappings,
            business_rules=[self._validate_sales_invoice_business_rules],
            post_processors=[self._calculate_invoice_totals]
        )
    
    def transform_record(self, cobol_record: Dict[str, Any], 
                        table_name: str) -> Optional[Dict[str, Any]]:
        """
        Transform a single COBOL record to PostgreSQL format
        
        Args:
            cobol_record: Raw COBOL record data
            table_name: Target PostgreSQL table name
            
        Returns:
            Transformed record or None if transformation fails
        """
        self.transformation_stats['records_processed'] += 1
        
        if table_name not in self.table_mappings:
            logger.error(f"Unknown table mapping: {table_name}")
            self.transformation_stats['records_failed'] += 1
            return None
        
        mapping = self.table_mappings[table_name]
        transformed = {}
        errors = []
        
        try:
            # Apply field mappings
            for field_mapping in mapping.field_mappings:
                try:
                    # Get source value
                    source_value = cobol_record.get(field_mapping.cobol_field)
                    
                    # Apply transformation
                    if field_mapping.transformer:
                        transformed_value = field_mapping.transformer(source_value)
                    else:
                        transformed_value = source_value
                    
                    # Handle None values
                    if transformed_value is None:
                        if field_mapping.required:
                            if field_mapping.default_value is not None:
                                transformed_value = field_mapping.default_value
                            else:
                                errors.append(f"Required field {field_mapping.postgres_field} is None")
                                continue
                        else:
                            transformed_value = field_mapping.default_value
                    
                    # Apply validation rules
                    if field_mapping.validation_rules:
                        for rule in field_mapping.validation_rules:
                            if not rule(transformed_value):
                                errors.append(f"Validation failed for {field_mapping.postgres_field}")
                                break
                    
                    transformed[field_mapping.postgres_field] = transformed_value
                    
                except Exception as e:
                    error_msg = f"Error transforming field {field_mapping.cobol_field} -> {field_mapping.postgres_field}: {str(e)}"
                    errors.append(error_msg)
                    logger.warning(error_msg)
            
            # Apply business rules
            if mapping.business_rules:
                for rule in mapping.business_rules:
                    try:
                        rule_errors = rule(transformed, cobol_record)
                        if rule_errors:
                            errors.extend(rule_errors)
                    except Exception as e:
                        errors.append(f"Business rule error: {str(e)}")
            
            # Apply post-processors
            if mapping.post_processors:
                for processor in mapping.post_processors:
                    try:
                        processor(transformed, cobol_record)
                    except Exception as e:
                        errors.append(f"Post-processor error: {str(e)}")
            
            if errors:
                logger.warning(f"Transformation errors for record: {errors}")
                self.transformation_stats['validation_errors'] += len(errors)
                # Return record anyway if not critical errors
                if not any('Required field' in error for error in errors):
                    self.transformation_stats['records_transformed'] += 1
                    return transformed
                else:
                    self.transformation_stats['records_failed'] += 1
                    return None
            
            self.transformation_stats['records_transformed'] += 1
            return transformed
            
        except Exception as e:
            logger.error(f"Critical error transforming record: {str(e)}")
            self.transformation_stats['records_failed'] += 1
            return None
    
    def transform_batch(self, cobol_records: List[Dict[str, Any]], 
                       table_name: str) -> List[Dict[str, Any]]:
        """Transform a batch of COBOL records"""
        transformed_records = []
        
        for record in cobol_records:
            transformed = self.transform_record(record, table_name)
            if transformed:
                transformed_records.append(transformed)
        
        return transformed_records
    
    # Field Transformation Methods
    def _clean_text_field(self, value: Any) -> Optional[str]:
        """Clean and validate text fields"""
        if value is None:
            return None
        
        if isinstance(value, str):
            cleaned = value.strip()
            # Remove control characters and normalize whitespace
            cleaned = re.sub(r'[\x00-\x1f\x7f-\x9f]', '', cleaned)
            cleaned = re.sub(r'\s+', ' ', cleaned)
            return cleaned if cleaned else None
        
        return str(value).strip() if str(value).strip() else None
    
    def _clean_decimal(self, value: Any) -> Optional[Decimal]:
        """Clean and validate decimal fields"""
        if value is None:
            return None
        
        try:
            if isinstance(value, Decimal):
                return value
            elif isinstance(value, (int, float)):
                return Decimal(str(value))
            elif isinstance(value, str):
                cleaned = value.strip().replace(',', '')
                if not cleaned:
                    return None
                return Decimal(cleaned)
            else:
                return Decimal(str(value))
        except (InvalidOperation, ValueError) as e:
            logger.warning(f"Invalid decimal value: {value} - {str(e)}")
            return None
    
    def _clean_integer(self, value: Any) -> Optional[int]:
        """Clean and validate integer fields"""
        if value is None:
            return None
        
        try:
            if isinstance(value, int):
                return value
            elif isinstance(value, (float, Decimal)):
                return int(value)
            elif isinstance(value, str):
                cleaned = value.strip()
                if not cleaned:
                    return None
                return int(float(cleaned))
            else:
                return int(value)
        except (ValueError, TypeError) as e:
            logger.warning(f"Invalid integer value: {value} - {str(e)}")
            return None
    
    def _clean_date(self, value: Any) -> Optional[datetime]:
        """Clean and validate date fields"""
        if value is None:
            return None
        
        try:
            if isinstance(value, datetime):
                return value
            elif isinstance(value, date):
                return datetime.combine(value, datetime.min.time())
            elif isinstance(value, str):
                cleaned = value.strip()
                if not cleaned or cleaned == '00000000':
                    return None
                # Try various date formats
                for fmt in ['%Y%m%d', '%Y-%m-%d', '%d/%m/%Y', '%m/%d/%Y']:
                    try:
                        return datetime.strptime(cleaned, fmt)
                    except ValueError:
                        continue
                logger.warning(f"Unable to parse date: {value}")
                return None
            else:
                return None
        except Exception as e:
            logger.warning(f"Error parsing date {value}: {str(e)}")
            return None
    
    def _clean_customer_code(self, value: Any) -> Optional[str]:
        """Clean and validate customer codes"""
        cleaned = self._clean_text_field(value)
        if not cleaned:
            return None
        
        # Ensure 6 character format, pad with zeros if needed
        cleaned = cleaned.upper().ljust(6, '0')[:6]
        
        # Validate format (alphanumeric)
        if not re.match(r'^[A-Z0-9]{6}$', cleaned):
            logger.warning(f"Invalid customer code format: {value}")
            return None
        
        return cleaned
    
    def _clean_supplier_code(self, value: Any) -> Optional[str]:
        """Clean and validate supplier codes"""
        cleaned = self._clean_text_field(value)
        if not cleaned:
            return None
        
        # Ensure 6 character format, pad with zeros if needed
        cleaned = cleaned.upper().ljust(6, '0')[:6]
        
        # Validate format (alphanumeric)
        if not re.match(r'^[A-Z0-9]{6}$', cleaned):
            logger.warning(f"Invalid supplier code format: {value}")
            return None
        
        return cleaned
    
    def _clean_item_code(self, value: Any) -> Optional[str]:
        """Clean and validate item codes"""
        cleaned = self._clean_text_field(value)
        if not cleaned:
            return None
        
        # Ensure 15 character format, pad with spaces if needed
        cleaned = cleaned.upper().ljust(15)[:15]
        
        # Validate format (alphanumeric plus dash)
        if not re.match(r'^[A-Z0-9\-\s]{15}$', cleaned):
            logger.warning(f"Invalid item code format: {value}")
            return None
        
        return cleaned.rstrip()  # Remove trailing spaces for storage
    
    def _clean_account_code(self, value: Any) -> Optional[str]:
        """Clean and validate GL account codes"""
        cleaned = self._clean_text_field(value)
        if not cleaned:
            return None
        
        # Ensure 8 character format
        cleaned = cleaned.upper().ljust(8, '0')[:8]
        
        # Validate format (numeric)
        if not re.match(r'^[0-9]{8}$', cleaned):
            logger.warning(f"Invalid account code format: {value}")
            return None
        
        return cleaned
    
    def _clean_email(self, value: Any) -> Optional[str]:
        """Clean and validate email addresses"""
        cleaned = self._clean_text_field(value)
        if not cleaned:
            return None
        
        # Basic email validation
        email_pattern = r'^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$'
        if re.match(email_pattern, cleaned.lower()):
            return cleaned.lower()
        else:
            logger.warning(f"Invalid email format: {value}")
            return None
    
    def _clean_phone_number(self, value: Any) -> Optional[str]:
        """Clean and validate phone numbers"""
        cleaned = self._clean_text_field(value)
        if not cleaned:
            return None
        
        # Remove all non-digit characters except + and spaces
        cleaned = re.sub(r'[^\d\+\s\-\(\)]', '', cleaned)
        cleaned = re.sub(r'\s+', ' ', cleaned).strip()
        
        return cleaned if cleaned else None
    
    def _clean_postal_code(self, value: Any) -> Optional[str]:
        """Clean and validate postal codes"""
        cleaned = self._clean_text_field(value)
        if not cleaned:
            return None
        
        # Remove spaces and convert to uppercase
        cleaned = cleaned.replace(' ', '').upper()
        
        return cleaned if cleaned else None
    
    def _clean_country_code(self, value: Any) -> Optional[str]:
        """Clean and validate country codes"""
        cleaned = self._clean_text_field(value)
        if not cleaned:
            return 'GBR'  # Default to UK
        
        cleaned = cleaned.upper()
        
        # Map common codes to ISO 3166-1 alpha-3
        country_mappings = {
            'UK': 'GBR', 'GB': 'GBR', 'US': 'USA', 'USA': 'USA',
            'CA': 'CAN', 'DE': 'DEU', 'FR': 'FRA', 'IT': 'ITA',
            'ES': 'ESP', 'NL': 'NLD', 'BE': 'BEL', 'AT': 'AUT',
            'CH': 'CHE', 'IE': 'IRL', 'DK': 'DNK', 'SE': 'SWE',
            'NO': 'NOR', 'FI': 'FIN', 'PL': 'POL', 'CZ': 'CZE'
        }
        
        return country_mappings.get(cleaned, cleaned[:3])
    
    def _clean_currency_code(self, value: Any) -> Optional[str]:
        """Clean and validate currency codes"""
        cleaned = self._clean_text_field(value)
        if not cleaned:
            return 'GBP'  # Default currency
        
        cleaned = cleaned.upper()
        
        # Validate common currency codes
        valid_currencies = [
            'GBP', 'USD', 'EUR', 'CAD', 'AUD', 'JPY', 'CHF', 'NOK', 'SEK', 'DKK'
        ]
        
        if cleaned in valid_currencies:
            return cleaned
        else:
            logger.warning(f"Unknown currency code: {value}, defaulting to GBP")
            return 'GBP'
    
    # Business Logic Transformation Methods
    def _transform_customer_status(self, value: Any) -> str:
        """Transform COBOL customer status to PostgreSQL enum"""
        if not value:
            return 'active'
        
        status_map = {
            'A': 'active',
            'I': 'inactive', 
            'D': 'deleted',
            'H': 'hold',
            'C': 'credit_hold'
        }
        
        cleaned = str(value).upper().strip()
        return status_map.get(cleaned, 'active')
    
    def _transform_supplier_status(self, value: Any) -> str:
        """Transform COBOL supplier status to PostgreSQL enum"""
        if not value:
            return 'active'
        
        status_map = {
            'A': 'active',
            'I': 'inactive',
            'D': 'deleted',
            'H': 'hold'
        }
        
        cleaned = str(value).upper().strip()
        return status_map.get(cleaned, 'active')
    
    def _transform_stock_status(self, value: Any) -> str:
        """Transform COBOL stock status to PostgreSQL enum"""
        if not value:
            return 'active'
        
        status_map = {
            'A': 'active',
            'I': 'inactive',
            'D': 'discontinued',
            'O': 'obsolete'
        }
        
        cleaned = str(value).upper().strip()
        return status_map.get(cleaned, 'active')
    
    def _transform_costing_method(self, value: Any) -> str:
        """Transform COBOL costing method to PostgreSQL enum"""
        if not value:
            return 'FIFO'
        
        method_map = {
            'F': 'FIFO',
            'L': 'LIFO', 
            'A': 'AVERAGE',
            'S': 'STANDARD'
        }
        
        cleaned = str(value).upper().strip()
        return method_map.get(cleaned, 'FIFO')
    
    def _transform_account_type(self, value: Any) -> str:
        """Transform COBOL account type to PostgreSQL enum"""
        if not value:
            return 'ASSET'
        
        type_map = {
            '01': 'ASSET',
            '02': 'LIABILITY',
            '03': 'EQUITY',
            '04': 'INCOME',
            '05': 'EXPENSE',
            '06': 'OTHER',
            'AS': 'ASSET',
            'LI': 'LIABILITY',
            'EQ': 'EQUITY',
            'IN': 'INCOME',
            'EX': 'EXPENSE'
        }
        
        cleaned = str(value).upper().strip()
        return type_map.get(cleaned, 'ASSET')
    
    def _transform_normal_balance(self, value: Any) -> str:
        """Transform COBOL normal balance to PostgreSQL enum"""
        if not value:
            return 'DEBIT'
        
        cleaned = str(value).upper().strip()
        return 'DEBIT' if cleaned == 'D' else 'CREDIT'
    
    # Business Rule Validators
    def _validate_customer_business_rules(self, transformed: Dict[str, Any], 
                                        original: Dict[str, Any]) -> List[str]:
        """Validate customer business rules"""
        errors = []
        
        # Credit limit should be positive
        if transformed.get('credit_limit', 0) < 0:
            errors.append("Credit limit cannot be negative")
        
        # Active customers must have valid contact information
        if transformed.get('status') == 'active':
            if not transformed.get('phone_number') and not transformed.get('email_address'):
                errors.append("Active customers must have phone or email contact")
        
        return errors
    
    def _validate_supplier_business_rules(self, transformed: Dict[str, Any],
                                        original: Dict[str, Any]) -> List[str]:
        """Validate supplier business rules"""
        errors = []
        
        # Active suppliers must have contact information
        if transformed.get('status') == 'active':
            if not transformed.get('contact_person') and not transformed.get('email_address'):
                errors.append("Active suppliers must have contact person or email")
        
        return errors
    
    def _validate_stock_business_rules(self, transformed: Dict[str, Any],
                                     original: Dict[str, Any]) -> List[str]:
        """Validate stock item business rules"""
        errors = []
        
        # Selling price should be greater than cost
        selling_price = transformed.get('selling_price', Decimal('0'))
        standard_cost = transformed.get('standard_cost', Decimal('0'))
        
        if selling_price > 0 and standard_cost > 0 and selling_price < standard_cost:
            logger.warning("Selling price is less than standard cost")
        
        # Reorder level should be positive for active items
        if transformed.get('status') == 'active':
            if transformed.get('reorder_level', 0) < 0:
                errors.append("Reorder level cannot be negative for active items")
        
        return errors
    
    # Post-processors
    def _generate_customer_uuid(self, transformed: Dict[str, Any], original: Dict[str, Any]):
        """Generate UUID for customer"""
        transformed['customer_id'] = str(uuid.uuid4())
    
    def _generate_supplier_uuid(self, transformed: Dict[str, Any], original: Dict[str, Any]):
        """Generate UUID for supplier"""
        transformed['supplier_id'] = str(uuid.uuid4())
    
    def _calculate_stock_metrics(self, transformed: Dict[str, Any], original: Dict[str, Any]):
        """Calculate derived stock metrics"""
        qty_on_hand = transformed.get('quantity_on_hand', 0)
        qty_allocated = transformed.get('quantity_allocated', 0)
        
        # Calculate available quantity
        transformed['quantity_available'] = max(0, qty_on_hand - qty_allocated)
        
        # Calculate inventory value
        standard_cost = transformed.get('standard_cost', Decimal('0'))
        transformed['inventory_value'] = Decimal(qty_on_hand) * standard_cost
    
    def get_transformation_stats(self) -> Dict[str, int]:
        """Get transformation statistics"""
        return self.transformation_stats.copy()
    
    def reset_stats(self):
        """Reset transformation statistics"""
        self.transformation_stats = {
            'records_processed': 0,
            'records_transformed': 0,
            'records_failed': 0,
            'validation_errors': 0,
            'business_rule_violations': 0
        }