"""
Data Validation Engine

Comprehensive data validation and quality checks for COBOL to PostgreSQL migration.
Ensures data integrity, referential consistency, and business rule compliance.
"""

import logging
from typing import Dict, List, Any, Optional, Tuple, Set
from datetime import datetime, date
from decimal import Decimal
from dataclasses import dataclass
import re
from collections import defaultdict, Counter
import asyncio
import asyncpg

logger = logging.getLogger(__name__)


@dataclass
class ValidationRule:
    """Represents a data validation rule"""
    rule_name: str
    description: str
    severity: str  # ERROR, WARNING, INFO
    rule_type: str  # REQUIRED, FORMAT, RANGE, REFERENCE, BUSINESS, CONSISTENCY
    validator_func: callable
    error_message: str
    table_name: Optional[str] = None
    field_name: Optional[str] = None


@dataclass
class ValidationResult:
    """Result of validation check"""
    rule_name: str
    severity: str
    passed: bool
    error_message: Optional[str]
    affected_records: int
    details: Optional[Dict[str, Any]] = None


@dataclass
class ValidationSummary:
    """Summary of validation results"""
    total_rules: int
    passed_rules: int
    failed_rules: int
    error_count: int
    warning_count: int
    info_count: int
    validation_time: float
    results: List[ValidationResult]


class ValidationEngine:
    """Main validation engine for data quality checks"""
    
    def __init__(self, db_connection_string: str):
        self.db_connection_string = db_connection_string
        self.validation_rules = []
        self.db_pool = None
        self._initialize_rules()
    
    async def initialize_database(self):
        """Initialize database connection pool"""
        self.db_pool = await asyncpg.create_pool(self.db_connection_string)
    
    async def close_database(self):
        """Close database connection pool"""
        if self.db_pool:
            await self.db_pool.close()
    
    def _initialize_rules(self):
        """Initialize all validation rules"""
        
        # Required Field Rules
        self.validation_rules.extend([
            ValidationRule(
                "CUST_CODE_REQUIRED", "Customer code is required",
                "ERROR", "REQUIRED", self._check_required_field,
                "Customer records must have a customer code",
                "customers", "customer_code"
            ),
            ValidationRule(
                "CUST_NAME_REQUIRED", "Customer name is required",
                "ERROR", "REQUIRED", self._check_required_field,
                "Customer records must have a customer name",
                "customers", "customer_name"
            ),
            ValidationRule(
                "SUPP_CODE_REQUIRED", "Supplier code is required",
                "ERROR", "REQUIRED", self._check_required_field,
                "Supplier records must have a supplier code",
                "suppliers", "supplier_code"
            ),
            ValidationRule(
                "SUPP_NAME_REQUIRED", "Supplier name is required",
                "ERROR", "REQUIRED", self._check_required_field,
                "Supplier records must have a supplier name",
                "suppliers", "supplier_name"
            ),
            ValidationRule(
                "ITEM_CODE_REQUIRED", "Item code is required",
                "ERROR", "REQUIRED", self._check_required_field,
                "Stock item records must have an item code",
                "stock_items", "item_code"
            ),
            ValidationRule(
                "ACCOUNT_CODE_REQUIRED", "Account code is required",
                "ERROR", "REQUIRED", self._check_required_field,
                "GL account records must have an account code",
                "gl_accounts", "account_code"
            )
        ])
        
        # Format Validation Rules
        self.validation_rules.extend([
            ValidationRule(
                "CUST_CODE_FORMAT", "Customer code format validation",
                "ERROR", "FORMAT", self._validate_customer_code_format,
                "Customer codes must be 6 alphanumeric characters",
                "customers", "customer_code"
            ),
            ValidationRule(
                "SUPP_CODE_FORMAT", "Supplier code format validation",
                "ERROR", "FORMAT", self._validate_supplier_code_format,
                "Supplier codes must be 6 alphanumeric characters",
                "suppliers", "supplier_code"
            ),
            ValidationRule(
                "ACCOUNT_CODE_FORMAT", "GL account code format validation",
                "ERROR", "FORMAT", self._validate_account_code_format,
                "GL account codes must be 8 numeric characters",
                "gl_accounts", "account_code"
            ),
            ValidationRule(
                "EMAIL_FORMAT", "Email address format validation",
                "WARNING", "FORMAT", self._validate_email_format,
                "Email addresses must be in valid format",
                None, None
            ),
            ValidationRule(
                "PHONE_FORMAT", "Phone number format validation",
                "WARNING", "FORMAT", self._validate_phone_format,
                "Phone numbers should contain only digits and formatting characters",
                None, None
            )
        ])
        
        # Range Validation Rules
        self.validation_rules.extend([
            ValidationRule(
                "CREDIT_LIMIT_RANGE", "Credit limit range validation",
                "WARNING", "RANGE", self._validate_credit_limit_range,
                "Credit limits should be between 0 and 1,000,000",
                "customers", "credit_limit"
            ),
            ValidationRule(
                "PRICE_RANGE", "Price range validation",
                "WARNING", "RANGE", self._validate_price_range,
                "Prices should be positive values",
                "stock_items", None
            ),
            ValidationRule(
                "QUANTITY_RANGE", "Quantity range validation",
                "WARNING", "RANGE", self._validate_quantity_range,
                "Quantities should be reasonable values",
                "stock_items", None
            ),
            ValidationRule(
                "DATE_RANGE", "Date range validation",
                "ERROR", "RANGE", self._validate_date_range,
                "Dates should be within reasonable range (1980-2030)",
                None, None
            )
        ])
        
        # Referential Integrity Rules
        self.validation_rules.extend([
            ValidationRule(
                "CUSTOMER_REFERENCE", "Customer reference integrity",
                "ERROR", "REFERENCE", self._validate_customer_references,
                "All customer references must exist in customer master",
                "sales_invoices", "customer_code"
            ),
            ValidationRule(
                "SUPPLIER_REFERENCE", "Supplier reference integrity",
                "ERROR", "REFERENCE", self._validate_supplier_references,
                "All supplier references must exist in supplier master",
                "stock_items", "primary_supplier_code"
            ),
            ValidationRule(
                "ACCOUNT_REFERENCE", "GL account reference integrity",
                "ERROR", "REFERENCE", self._validate_account_references,
                "All GL account references must exist in chart of accounts",
                "gl_transactions", "account_code"
            )
        ])
        
        # Business Rule Validation
        self.validation_rules.extend([
            ValidationRule(
                "CUSTOMER_BALANCE", "Customer balance consistency",
                "WARNING", "BUSINESS", self._validate_customer_balance_consistency,
                "Customer balances should match transaction history",
                "customers", None
            ),
            ValidationRule(
                "INVOICE_TOTALS", "Invoice total calculations",
                "ERROR", "BUSINESS", self._validate_invoice_totals,
                "Invoice totals must equal subtotal + tax + freight - discount",
                "sales_invoices", None
            ),
            ValidationRule(
                "STOCK_VALUATION", "Stock valuation consistency",
                "WARNING", "BUSINESS", self._validate_stock_valuation,
                "Stock values should be consistent with quantities and costs",
                "stock_items", None
            ),
            ValidationRule(
                "GL_BALANCE", "General Ledger balance check",
                "ERROR", "BUSINESS", self._validate_gl_balance,
                "General Ledger must balance (debits = credits)",
                "gl_transactions", None
            )
        ])
        
        # Data Consistency Rules
        self.validation_rules.extend([
            ValidationRule(
                "DUPLICATE_CUSTOMERS", "Duplicate customer detection",
                "ERROR", "CONSISTENCY", self._check_duplicate_customers,
                "No duplicate customer codes allowed",
                "customers", "customer_code"
            ),
            ValidationRule(
                "DUPLICATE_SUPPLIERS", "Duplicate supplier detection", 
                "ERROR", "CONSISTENCY", self._check_duplicate_suppliers,
                "No duplicate supplier codes allowed",
                "suppliers", "supplier_code"
            ),
            ValidationRule(
                "DUPLICATE_ITEMS", "Duplicate item detection",
                "ERROR", "CONSISTENCY", self._check_duplicate_items,
                "No duplicate item codes allowed",
                "stock_items", "item_code"
            ),
            ValidationRule(
                "DUPLICATE_ACCOUNTS", "Duplicate account detection",
                "ERROR", "CONSISTENCY", self._check_duplicate_accounts,
                "No duplicate GL account codes allowed",
                "gl_accounts", "account_code"
            )
        ])
    
    async def validate_all(self, tables_to_validate: Optional[List[str]] = None) -> ValidationSummary:
        """
        Run all validation rules
        
        Args:
            tables_to_validate: List of table names to validate, or None for all tables
            
        Returns:
            ValidationSummary with results
        """
        start_time = datetime.now()
        results = []
        
        logger.info("Starting comprehensive data validation")
        
        # Filter rules by tables if specified
        rules_to_run = self.validation_rules
        if tables_to_validate:
            rules_to_run = [rule for rule in self.validation_rules 
                           if rule.table_name is None or rule.table_name in tables_to_validate]
        
        # Run validation rules
        for rule in rules_to_run:
            try:
                logger.info(f"Running validation rule: {rule.rule_name}")
                result = await rule.validator_func(rule)
                results.append(result)
                
                if not result.passed:
                    logger.warning(f"Validation failed: {rule.rule_name} - {result.error_message}")
                
            except Exception as e:
                logger.error(f"Error running validation rule {rule.rule_name}: {str(e)}")
                results.append(ValidationResult(
                    rule.rule_name, rule.severity, False,
                    f"Validation error: {str(e)}", 0
                ))
        
        end_time = datetime.now()
        validation_time = (end_time - start_time).total_seconds()
        
        # Calculate summary statistics
        total_rules = len(results)
        passed_rules = sum(1 for r in results if r.passed)
        failed_rules = total_rules - passed_rules
        error_count = sum(1 for r in results if r.severity == "ERROR" and not r.passed)
        warning_count = sum(1 for r in results if r.severity == "WARNING" and not r.passed)
        info_count = sum(1 for r in results if r.severity == "INFO" and not r.passed)
        
        summary = ValidationSummary(
            total_rules, passed_rules, failed_rules,
            error_count, warning_count, info_count,
            validation_time, results
        )
        
        logger.info(f"Validation completed in {validation_time:.2f}s - "
                   f"Passed: {passed_rules}, Failed: {failed_rules}")
        
        return summary
    
    # Required Field Validators
    async def _check_required_field(self, rule: ValidationRule) -> ValidationResult:
        """Check that required fields are not null or empty"""
        async with self.db_pool.acquire() as conn:
            query = f"""
                SELECT COUNT(*) 
                FROM {rule.table_name} 
                WHERE {rule.field_name} IS NULL 
                   OR {rule.field_name} = ''
                   OR TRIM({rule.field_name}) = ''
            """
            
            null_count = await conn.fetchval(query)
            
            if null_count > 0:
                return ValidationResult(
                    rule.rule_name, rule.severity, False,
                    f"Found {null_count} records with missing {rule.field_name}",
                    null_count
                )
            else:
                return ValidationResult(
                    rule.rule_name, rule.severity, True, None, 0
                )
    
    # Format Validators
    async def _validate_customer_code_format(self, rule: ValidationRule) -> ValidationResult:
        """Validate customer code format (6 alphanumeric characters)"""
        async with self.db_pool.acquire() as conn:
            query = """
                SELECT COUNT(*) 
                FROM customers 
                WHERE customer_code !~ '^[A-Z0-9]{6}$'
                   OR LENGTH(customer_code) != 6
            """
            
            invalid_count = await conn.fetchval(query)
            
            if invalid_count > 0:
                return ValidationResult(
                    rule.rule_name, rule.severity, False,
                    f"Found {invalid_count} customers with invalid code format",
                    invalid_count
                )
            else:
                return ValidationResult(
                    rule.rule_name, rule.severity, True, None, 0
                )
    
    async def _validate_supplier_code_format(self, rule: ValidationRule) -> ValidationResult:
        """Validate supplier code format (6 alphanumeric characters)"""
        async with self.db_pool.acquire() as conn:
            query = """
                SELECT COUNT(*) 
                FROM suppliers 
                WHERE supplier_code !~ '^[A-Z0-9]{6}$'
                   OR LENGTH(supplier_code) != 6
            """
            
            invalid_count = await conn.fetchval(query)
            
            if invalid_count > 0:
                return ValidationResult(
                    rule.rule_name, rule.severity, False,
                    f"Found {invalid_count} suppliers with invalid code format",
                    invalid_count
                )
            else:
                return ValidationResult(
                    rule.rule_name, rule.severity, True, None, 0
                )
    
    async def _validate_account_code_format(self, rule: ValidationRule) -> ValidationResult:
        """Validate GL account code format (8 numeric characters)"""
        async with self.db_pool.acquire() as conn:
            query = """
                SELECT COUNT(*) 
                FROM gl_accounts 
                WHERE account_code !~ '^[0-9]{8}$'
                   OR LENGTH(account_code) != 8
            """
            
            invalid_count = await conn.fetchval(query)
            
            if invalid_count > 0:
                return ValidationResult(
                    rule.rule_name, rule.severity, False,
                    f"Found {invalid_count} GL accounts with invalid code format",
                    invalid_count
                )
            else:
                return ValidationResult(
                    rule.rule_name, rule.severity, True, None, 0
                )
    
    async def _validate_email_format(self, rule: ValidationRule) -> ValidationResult:
        """Validate email address formats across all tables"""
        async with self.db_pool.acquire() as conn:
            # Check customers
            query_customers = """
                SELECT COUNT(*) 
                FROM customers 
                WHERE email_address IS NOT NULL 
                  AND email_address != ''
                  AND email_address !~ '^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}$'
            """
            
            # Check suppliers
            query_suppliers = """
                SELECT COUNT(*) 
                FROM suppliers 
                WHERE email_address IS NOT NULL 
                  AND email_address != ''
                  AND email_address !~ '^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}$'
            """
            
            invalid_customers = await conn.fetchval(query_customers)
            invalid_suppliers = await conn.fetchval(query_suppliers)
            total_invalid = invalid_customers + invalid_suppliers
            
            if total_invalid > 0:
                return ValidationResult(
                    rule.rule_name, rule.severity, False,
                    f"Found {total_invalid} records with invalid email format "
                    f"({invalid_customers} customers, {invalid_suppliers} suppliers)",
                    total_invalid
                )
            else:
                return ValidationResult(
                    rule.rule_name, rule.severity, True, None, 0
                )
    
    async def _validate_phone_format(self, rule: ValidationRule) -> ValidationResult:
        """Validate phone number formats"""
        async with self.db_pool.acquire() as conn:
            query = """
                SELECT 
                    (SELECT COUNT(*) FROM customers 
                     WHERE phone_number IS NOT NULL AND phone_number != ''
                       AND phone_number !~ '^[0-9\s\-\+\(\)]+$') +
                    (SELECT COUNT(*) FROM suppliers 
                     WHERE phone_number IS NOT NULL AND phone_number != ''
                       AND phone_number !~ '^[0-9\s\-\+\(\)]+$')
            """
            
            invalid_count = await conn.fetchval(query)
            
            if invalid_count > 0:
                return ValidationResult(
                    rule.rule_name, rule.severity, False,
                    f"Found {invalid_count} records with invalid phone number format",
                    invalid_count
                )
            else:
                return ValidationResult(
                    rule.rule_name, rule.severity, True, None, 0
                )
    
    # Range Validators
    async def _validate_credit_limit_range(self, rule: ValidationRule) -> ValidationResult:
        """Validate credit limit ranges"""
        async with self.db_pool.acquire() as conn:
            query = """
                SELECT COUNT(*) 
                FROM customers 
                WHERE credit_limit < 0 OR credit_limit > 1000000
            """
            
            invalid_count = await conn.fetchval(query)
            
            if invalid_count > 0:
                return ValidationResult(
                    rule.rule_name, rule.severity, False,
                    f"Found {invalid_count} customers with credit limit outside valid range (0-1,000,000)",
                    invalid_count
                )
            else:
                return ValidationResult(
                    rule.rule_name, rule.severity, True, None, 0
                )
    
    async def _validate_price_range(self, rule: ValidationRule) -> ValidationResult:
        """Validate price ranges for stock items"""
        async with self.db_pool.acquire() as conn:
            query = """
                SELECT COUNT(*) 
                FROM stock_items 
                WHERE standard_cost < 0 
                   OR selling_price < 0 
                   OR average_cost < 0
                   OR (selling_price > 0 AND selling_price > 100000)
                   OR (standard_cost > 0 AND standard_cost > 50000)
            """
            
            invalid_count = await conn.fetchval(query)
            
            if invalid_count > 0:
                return ValidationResult(
                    rule.rule_name, rule.severity, False,
                    f"Found {invalid_count} stock items with prices outside valid ranges",
                    invalid_count
                )
            else:
                return ValidationResult(
                    rule.rule_name, rule.severity, True, None, 0
                )
    
    async def _validate_quantity_range(self, rule: ValidationRule) -> ValidationResult:
        """Validate quantity ranges for stock items"""
        async with self.db_pool.acquire() as conn:
            query = """
                SELECT COUNT(*) 
                FROM stock_items 
                WHERE quantity_on_hand < -10000 
                   OR quantity_on_hand > 1000000
                   OR quantity_allocated < 0
                   OR quantity_on_order < 0
                   OR reorder_level < 0
                   OR reorder_quantity < 0
            """
            
            invalid_count = await conn.fetchval(query)
            
            if invalid_count > 0:
                return ValidationResult(
                    rule.rule_name, rule.severity, False,
                    f"Found {invalid_count} stock items with quantities outside valid ranges",
                    invalid_count
                )
            else:
                return ValidationResult(
                    rule.rule_name, rule.severity, True, None, 0
                )
    
    async def _validate_date_range(self, rule: ValidationRule) -> ValidationResult:
        """Validate date ranges across all tables"""
        async with self.db_pool.acquire() as conn:
            # Check for dates outside reasonable range (1980-2030)
            tables_with_dates = [
                ('customers', ['created_at', 'updated_at', 'last_invoice_date']),
                ('suppliers', ['created_at', 'updated_at']),
                ('stock_items', ['created_at', 'updated_at', 'last_movement_date', 'last_sale_date']),
                ('gl_accounts', ['created_at', 'updated_at']),
                ('sales_invoices', ['invoice_date', 'due_date', 'created_at', 'updated_at'])
            ]
            
            total_invalid = 0
            
            for table, date_fields in tables_with_dates:
                for field in date_fields:
                    query = f"""
                        SELECT COUNT(*) 
                        FROM {table} 
                        WHERE {field} IS NOT NULL 
                          AND ({field} < '1980-01-01' OR {field} > '2030-12-31')
                    """
                    
                    invalid_count = await conn.fetchval(query)
                    total_invalid += invalid_count
            
            if total_invalid > 0:
                return ValidationResult(
                    rule.rule_name, rule.severity, False,
                    f"Found {total_invalid} records with dates outside valid range (1980-2030)",
                    total_invalid
                )
            else:
                return ValidationResult(
                    rule.rule_name, rule.severity, True, None, 0
                )
    
    # Referential Integrity Validators
    async def _validate_customer_references(self, rule: ValidationRule) -> ValidationResult:
        """Validate customer reference integrity"""
        async with self.db_pool.acquire() as conn:
            # Check sales invoices for invalid customer codes
            query = """
                SELECT COUNT(*) 
                FROM sales_invoices si
                LEFT JOIN customers c ON si.customer_code = c.customer_code
                WHERE c.customer_code IS NULL
            """
            
            orphaned_count = await conn.fetchval(query)
            
            if orphaned_count > 0:
                return ValidationResult(
                    rule.rule_name, rule.severity, False,
                    f"Found {orphaned_count} sales invoices with invalid customer references",
                    orphaned_count
                )
            else:
                return ValidationResult(
                    rule.rule_name, rule.severity, True, None, 0
                )
    
    async def _validate_supplier_references(self, rule: ValidationRule) -> ValidationResult:
        """Validate supplier reference integrity"""
        async with self.db_pool.acquire() as conn:
            # Check stock items for invalid supplier codes
            query = """
                SELECT COUNT(*) 
                FROM stock_items si
                LEFT JOIN suppliers s ON si.primary_supplier_code = s.supplier_code
                WHERE si.primary_supplier_code IS NOT NULL 
                  AND si.primary_supplier_code != ''
                  AND s.supplier_code IS NULL
            """
            
            orphaned_count = await conn.fetchval(query)
            
            if orphaned_count > 0:
                return ValidationResult(
                    rule.rule_name, rule.severity, False,
                    f"Found {orphaned_count} stock items with invalid supplier references",
                    orphaned_count
                )
            else:
                return ValidationResult(
                    rule.rule_name, rule.severity, True, None, 0
                )
    
    async def _validate_account_references(self, rule: ValidationRule) -> ValidationResult:
        """Validate GL account reference integrity"""
        async with self.db_pool.acquire() as conn:
            # Check GL transactions for invalid account codes
            query = """
                SELECT COUNT(*) 
                FROM gl_transactions gt
                LEFT JOIN gl_accounts ga ON gt.account_code = ga.account_code
                WHERE ga.account_code IS NULL
            """
            
            orphaned_count = await conn.fetchval(query)
            
            if orphaned_count > 0:
                return ValidationResult(
                    rule.rule_name, rule.severity, False,
                    f"Found {orphaned_count} GL transactions with invalid account references",
                    orphaned_count
                )
            else:
                return ValidationResult(
                    rule.rule_name, rule.severity, True, None, 0
                )
    
    # Business Rule Validators
    async def _validate_invoice_totals(self, rule: ValidationRule) -> ValidationResult:
        """Validate invoice total calculations"""
        async with self.db_pool.acquire() as conn:
            query = """
                SELECT COUNT(*) 
                FROM sales_invoices 
                WHERE ABS(invoice_total - (subtotal + tax_amount + freight_amount - discount_amount)) > 0.01
            """
            
            invalid_count = await conn.fetchval(query)
            
            if invalid_count > 0:
                return ValidationResult(
                    rule.rule_name, rule.severity, False,
                    f"Found {invalid_count} invoices with incorrect total calculations",
                    invalid_count
                )
            else:
                return ValidationResult(
                    rule.rule_name, rule.severity, True, None, 0
                )
    
    async def _validate_gl_balance(self, rule: ValidationRule) -> ValidationResult:
        """Validate General Ledger balance (debits = credits)"""
        async with self.db_pool.acquire() as conn:
            query = """
                SELECT 
                    SUM(CASE WHEN transaction_type = 'DEBIT' THEN amount ELSE 0 END) as total_debits,
                    SUM(CASE WHEN transaction_type = 'CREDIT' THEN amount ELSE 0 END) as total_credits
                FROM gl_transactions
                WHERE posted_flag = 'Y'
            """
            
            result = await conn.fetchrow(query)
            total_debits = result['total_debits'] or Decimal('0')
            total_credits = result['total_credits'] or Decimal('0')
            
            difference = abs(total_debits - total_credits)
            
            if difference > Decimal('0.01'):  # Allow for minor rounding differences
                return ValidationResult(
                    rule.rule_name, rule.severity, False,
                    f"GL out of balance: Debits={total_debits}, Credits={total_credits}, "
                    f"Difference={difference}",
                    1,
                    {'debits': float(total_debits), 'credits': float(total_credits), 'difference': float(difference)}
                )
            else:
                return ValidationResult(
                    rule.rule_name, rule.severity, True, None, 0
                )
    
    # Duplicate Checkers
    async def _check_duplicate_customers(self, rule: ValidationRule) -> ValidationResult:
        """Check for duplicate customer codes"""
        async with self.db_pool.acquire() as conn:
            query = """
                SELECT customer_code, COUNT(*) as count
                FROM customers
                GROUP BY customer_code
                HAVING COUNT(*) > 1
            """
            
            duplicates = await conn.fetch(query)
            
            if duplicates:
                total_duplicates = sum(row['count'] - 1 for row in duplicates)  # Exclude one valid record per group
                duplicate_codes = [row['customer_code'] for row in duplicates]
                
                return ValidationResult(
                    rule.rule_name, rule.severity, False,
                    f"Found {len(duplicates)} duplicate customer codes affecting {total_duplicates} records: {duplicate_codes[:5]}...",
                    total_duplicates,
                    {'duplicate_codes': duplicate_codes}
                )
            else:
                return ValidationResult(
                    rule.rule_name, rule.severity, True, None, 0
                )
    
    async def _check_duplicate_suppliers(self, rule: ValidationRule) -> ValidationResult:
        """Check for duplicate supplier codes"""
        async with self.db_pool.acquire() as conn:
            query = """
                SELECT supplier_code, COUNT(*) as count
                FROM suppliers
                GROUP BY supplier_code
                HAVING COUNT(*) > 1
            """
            
            duplicates = await conn.fetch(query)
            
            if duplicates:
                total_duplicates = sum(row['count'] - 1 for row in duplicates)
                duplicate_codes = [row['supplier_code'] for row in duplicates]
                
                return ValidationResult(
                    rule.rule_name, rule.severity, False,
                    f"Found {len(duplicates)} duplicate supplier codes affecting {total_duplicates} records",
                    total_duplicates,
                    {'duplicate_codes': duplicate_codes}
                )
            else:
                return ValidationResult(
                    rule.rule_name, rule.severity, True, None, 0
                )
    
    async def _check_duplicate_items(self, rule: ValidationRule) -> ValidationResult:
        """Check for duplicate item codes"""
        async with self.db_pool.acquire() as conn:
            query = """
                SELECT item_code, COUNT(*) as count
                FROM stock_items
                GROUP BY item_code
                HAVING COUNT(*) > 1
            """
            
            duplicates = await conn.fetch(query)
            
            if duplicates:
                total_duplicates = sum(row['count'] - 1 for row in duplicates)
                duplicate_codes = [row['item_code'] for row in duplicates]
                
                return ValidationResult(
                    rule.rule_name, rule.severity, False,
                    f"Found {len(duplicates)} duplicate item codes affecting {total_duplicates} records",
                    total_duplicates,
                    {'duplicate_codes': duplicate_codes}
                )
            else:
                return ValidationResult(
                    rule.rule_name, rule.severity, True, None, 0
                )
    
    async def _check_duplicate_accounts(self, rule: ValidationRule) -> ValidationResult:
        """Check for duplicate GL account codes"""
        async with self.db_pool.acquire() as conn:
            query = """
                SELECT account_code, COUNT(*) as count
                FROM gl_accounts
                GROUP BY account_code
                HAVING COUNT(*) > 1
            """
            
            duplicates = await conn.fetch(query)
            
            if duplicates:
                total_duplicates = sum(row['count'] - 1 for row in duplicates)
                duplicate_codes = [row['account_code'] for row in duplicates]
                
                return ValidationResult(
                    rule.rule_name, rule.severity, False,
                    f"Found {len(duplicates)} duplicate GL account codes affecting {total_duplicates} records",
                    total_duplicates,
                    {'duplicate_codes': duplicate_codes}
                )
            else:
                return ValidationResult(
                    rule.rule_name, rule.severity, True, None, 0
                )
    
    def generate_validation_report(self, summary: ValidationSummary) -> str:
        """Generate a formatted validation report"""
        report = []
        report.append("=" * 80)
        report.append("DATA VALIDATION REPORT")
        report.append("=" * 80)
        report.append(f"Validation Time: {summary.validation_time:.2f} seconds")
        report.append(f"Total Rules: {summary.total_rules}")
        report.append(f"Passed: {summary.passed_rules}")
        report.append(f"Failed: {summary.failed_rules}")
        report.append(f"Errors: {summary.error_count}")
        report.append(f"Warnings: {summary.warning_count}")
        report.append(f"Info: {summary.info_count}")
        report.append("")
        
        if summary.error_count > 0:
            report.append("CRITICAL ERRORS:")
            report.append("-" * 40)
            for result in summary.results:
                if result.severity == "ERROR" and not result.passed:
                    report.append(f"❌ {result.rule_name}: {result.error_message}")
                    report.append(f"   Affected records: {result.affected_records}")
            report.append("")
        
        if summary.warning_count > 0:
            report.append("WARNINGS:")
            report.append("-" * 40)
            for result in summary.results:
                if result.severity == "WARNING" and not result.passed:
                    report.append(f"⚠️  {result.rule_name}: {result.error_message}")
                    report.append(f"   Affected records: {result.affected_records}")
            report.append("")
        
        report.append("PASSED VALIDATIONS:")
        report.append("-" * 40)
        for result in summary.results:
            if result.passed:
                report.append(f"✅ {result.rule_name}")
        
        report.append("=" * 80)
        
        return "\n".join(report)