"""
Report Builder Service

Provides dynamic report building capabilities allowing users to create
custom reports by selecting fields, filters, and grouping options.
"""

from typing import Dict, List, Any, Optional, Union
from datetime import datetime, date
from dataclasses import dataclass
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func, text, case
from sqlalchemy.sql import select
import logging

from app.core.database import get_db
from app.models import *  # Import all models
from app.schemas.reports import CustomReportRequest, ReportField, ReportFilter

logger = logging.getLogger(__name__)


@dataclass
class FieldDefinition:
    """Definition of a report field"""
    name: str
    label: str
    table: str
    column: str
    data_type: str
    aggregation: Optional[str] = None


class ReportBuilder:
    """Dynamic report builder for creating custom reports"""
    
    def __init__(self):
        self.db = None
        self._field_definitions = self._initialize_field_definitions()
    
    def _get_db(self) -> Session:
        """Get database session"""
        if not self.db:
            self.db = next(get_db())
        return self.db
    
    def _initialize_field_definitions(self) -> Dict[str, FieldDefinition]:
        """Initialize available field definitions for report builder"""
        return {
            # GL Account fields
            'gl_account_code': FieldDefinition('gl_account_code', 'Account Code', 'gl_accounts', 'account_code', 'string'),
            'gl_account_name': FieldDefinition('gl_account_name', 'Account Name', 'gl_accounts', 'account_name', 'string'),
            'gl_account_type': FieldDefinition('gl_account_type', 'Account Type', 'gl_accounts', 'account_type', 'string'),
            'gl_account_category': FieldDefinition('gl_account_category', 'Account Category', 'gl_accounts', 'account_category', 'string'),
            
            # GL Transaction fields
            'gl_transaction_date': FieldDefinition('gl_transaction_date', 'Transaction Date', 'gl_transactions', 'transaction_date', 'date'),
            'gl_transaction_amount': FieldDefinition('gl_transaction_amount', 'Amount', 'gl_transactions', 'amount', 'decimal'),
            'gl_transaction_type': FieldDefinition('gl_transaction_type', 'Transaction Type', 'gl_transactions', 'transaction_type', 'string'),
            'gl_reference': FieldDefinition('gl_reference', 'Reference', 'gl_transactions', 'reference', 'string'),
            
            # Customer fields
            'customer_code': FieldDefinition('customer_code', 'Customer Code', 'customers', 'customer_code', 'string'),
            'customer_name': FieldDefinition('customer_name', 'Customer Name', 'customers', 'customer_name', 'string'),
            'customer_credit_limit': FieldDefinition('customer_credit_limit', 'Credit Limit', 'customers', 'credit_limit', 'decimal'),
            'customer_territory': FieldDefinition('customer_territory', 'Territory', 'customers', 'territory_code', 'string'),
            
            # Sales Invoice fields
            'sl_invoice_date': FieldDefinition('sl_invoice_date', 'Invoice Date', 'sales_invoices', 'invoice_date', 'date'),
            'sl_invoice_total': FieldDefinition('sl_invoice_total', 'Invoice Total', 'sales_invoices', 'invoice_total', 'decimal'),
            'sl_outstanding_amount': FieldDefinition('sl_outstanding_amount', 'Outstanding Amount', 'sales_invoices', 'outstanding_amount', 'decimal'),
            
            # Supplier fields
            'supplier_code': FieldDefinition('supplier_code', 'Supplier Code', 'suppliers', 'supplier_code', 'string'),
            'supplier_name': FieldDefinition('supplier_name', 'Supplier Name', 'suppliers', 'supplier_name', 'string'),
            
            # Purchase Invoice fields
            'pl_invoice_date': FieldDefinition('pl_invoice_date', 'Invoice Date', 'purchase_invoices', 'invoice_date', 'date'),
            'pl_invoice_total': FieldDefinition('pl_invoice_total', 'Invoice Total', 'purchase_invoices', 'invoice_total', 'decimal'),
            'pl_outstanding_amount': FieldDefinition('pl_outstanding_amount', 'Outstanding Amount', 'purchase_invoices', 'outstanding_amount', 'decimal'),
            
            # Stock Item fields
            'stock_item_code': FieldDefinition('stock_item_code', 'Item Code', 'stock_items', 'item_code', 'string'),
            'stock_item_name': FieldDefinition('stock_item_name', 'Item Name', 'stock_items', 'item_name', 'string'),
            'stock_item_category': FieldDefinition('stock_item_category', 'Category', 'stock_items', 'item_category', 'string'),
            'stock_standard_cost': FieldDefinition('stock_standard_cost', 'Standard Cost', 'stock_items', 'standard_cost', 'decimal'),
            
            # Stock Movement fields
            'stock_movement_date': FieldDefinition('stock_movement_date', 'Movement Date', 'stock_movements', 'movement_date', 'date'),
            'stock_movement_quantity': FieldDefinition('stock_movement_quantity', 'Quantity', 'stock_movements', 'quantity', 'integer'),
            'stock_movement_type': FieldDefinition('stock_movement_type', 'Movement Type', 'stock_movements', 'movement_type', 'string'),
        }
    
    async def get_available_fields(self, module: Optional[str] = None) -> List[Dict[str, str]]:
        """
        Get available fields for report building
        
        Args:
            module: Filter fields by module (gl, sl, pl, stock)
            
        Returns:
            List of available field definitions
        """
        try:
            fields = []
            
            for field_key, field_def in self._field_definitions.items():
                if module:
                    # Filter by module prefix
                    if not field_key.startswith(f"{module}_"):
                        continue
                
                fields.append({
                    'key': field_key,
                    'label': field_def.label,
                    'data_type': field_def.data_type,
                    'table': field_def.table
                })
            
            return sorted(fields, key=lambda x: x['label'])
            
        except Exception as e:
            logger.error(f"Error getting available fields: {str(e)}")
            raise
    
    async def build_custom_report(self, request: CustomReportRequest) -> Dict[str, Any]:
        """
        Build a custom report based on user specifications
        
        Args:
            request: Custom report request with fields, filters, and options
            
        Returns:
            Report data and metadata
        """
        try:
            db = self._get_db()
            
            # Build query based on selected fields
            query = self._build_query_from_fields(db, request.fields)
            
            # Apply filters
            if request.filters:
                query = self._apply_filters(query, request.filters)
            
            # Apply grouping
            if request.group_by:
                query = self._apply_grouping(query, request.group_by)
            
            # Apply sorting
            if request.sort_by:
                query = self._apply_sorting(query, request.sort_by)
            
            # Apply limit
            if request.limit:
                query = query.limit(request.limit)
            
            # Execute query
            results = query.all()
            
            # Format results
            report_data = self._format_query_results(results, request.fields)
            
            return {
                'title': request.report_name or 'Custom Report',
                'data': report_data,
                'total_records': len(report_data),
                'fields': [field.dict() for field in request.fields],
                'filters': [filter.dict() for filter in request.filters] if request.filters else [],
                'generated_at': datetime.now()
            }
            
        except Exception as e:
            logger.error(f"Error building custom report: {str(e)}")
            raise
    
    def _build_query_from_fields(self, db: Session, fields: List[ReportField]) -> Any:
        """Build SQLAlchemy query from selected fields"""
        try:
            # Determine which tables we need to join
            required_tables = set()
            select_fields = []
            
            for field in fields:
                if field.field_key in self._field_definitions:
                    field_def = self._field_definitions[field.field_key]
                    required_tables.add(field_def.table)
                    
                    # Build select expression
                    if field.aggregation:
                        if field.aggregation == 'sum':
                            select_expr = func.sum(text(f"{field_def.table}.{field_def.column}")).label(field.field_key)
                        elif field.aggregation == 'count':
                            select_expr = func.count(text(f"{field_def.table}.{field_def.column}")).label(field.field_key)
                        elif field.aggregation == 'avg':
                            select_expr = func.avg(text(f"{field_def.table}.{field_def.column}")).label(field.field_key)
                        elif field.aggregation == 'max':
                            select_expr = func.max(text(f"{field_def.table}.{field_def.column}")).label(field.field_key)
                        elif field.aggregation == 'min':
                            select_expr = func.min(text(f"{field_def.table}.{field_def.column}")).label(field.field_key)
                        else:
                            select_expr = text(f"{field_def.table}.{field_def.column}").label(field.field_key)
                    else:
                        select_expr = text(f"{field_def.table}.{field_def.column}").label(field.field_key)
                    
                    select_fields.append(select_expr)
            
            # Build base query
            if not select_fields:
                raise ValueError("No valid fields selected")
            
            # Create query with dynamic joins based on required tables
            query = db.query(*select_fields)
            
            # Add joins based on required tables
            if 'gl_transactions' in required_tables and 'gl_accounts' in required_tables:
                query = query.select_from(
                    text("gl_accounts").join(
                        text("gl_transactions"),
                        text("gl_accounts.account_code = gl_transactions.account_code")
                    )
                )
            elif 'sales_invoices' in required_tables and 'customers' in required_tables:
                query = query.select_from(
                    text("customers").join(
                        text("sales_invoices"),
                        text("customers.customer_code = sales_invoices.customer_code")
                    )
                )
            elif 'purchase_invoices' in required_tables and 'suppliers' in required_tables:
                query = query.select_from(
                    text("suppliers").join(
                        text("purchase_invoices"),
                        text("suppliers.supplier_code = purchase_invoices.supplier_code")
                    )
                )
            elif 'stock_movements' in required_tables and 'stock_items' in required_tables:
                query = query.select_from(
                    text("stock_items").join(
                        text("stock_movements"),
                        text("stock_items.item_code = stock_movements.item_code")
                    )
                )
            
            return query
            
        except Exception as e:
            logger.error(f"Error building query from fields: {str(e)}")
            raise
    
    def _apply_filters(self, query: Any, filters: List[ReportFilter]) -> Any:
        """Apply filters to query"""
        try:
            for filter in filters:
                if filter.field_key in self._field_definitions:
                    field_def = self._field_definitions[filter.field_key]
                    column_expr = text(f"{field_def.table}.{field_def.column}")
                    
                    if filter.operator == 'equals':
                        query = query.filter(column_expr == filter.value)
                    elif filter.operator == 'not_equals':
                        query = query.filter(column_expr != filter.value)
                    elif filter.operator == 'greater_than':
                        query = query.filter(column_expr > filter.value)
                    elif filter.operator == 'less_than':
                        query = query.filter(column_expr < filter.value)
                    elif filter.operator == 'greater_equal':
                        query = query.filter(column_expr >= filter.value)
                    elif filter.operator == 'less_equal':
                        query = query.filter(column_expr <= filter.value)
                    elif filter.operator == 'contains':
                        query = query.filter(column_expr.like(f"%{filter.value}%"))
                    elif filter.operator == 'starts_with':
                        query = query.filter(column_expr.like(f"{filter.value}%"))
                    elif filter.operator == 'ends_with':
                        query = query.filter(column_expr.like(f"%{filter.value}"))
                    elif filter.operator == 'in':
                        if isinstance(filter.value, list):
                            query = query.filter(column_expr.in_(filter.value))
                    elif filter.operator == 'between':
                        if isinstance(filter.value, list) and len(filter.value) == 2:
                            query = query.filter(column_expr.between(filter.value[0], filter.value[1]))
                    elif filter.operator == 'is_null':
                        query = query.filter(column_expr.is_(None))
                    elif filter.operator == 'is_not_null':
                        query = query.filter(column_expr.isnot(None))
            
            return query
            
        except Exception as e:
            logger.error(f"Error applying filters: {str(e)}")
            raise
    
    def _apply_grouping(self, query: Any, group_by: List[str]) -> Any:
        """Apply grouping to query"""
        try:
            group_expressions = []
            
            for field_key in group_by:
                if field_key in self._field_definitions:
                    field_def = self._field_definitions[field_key]
                    group_expressions.append(text(f"{field_def.table}.{field_def.column}"))
            
            if group_expressions:
                query = query.group_by(*group_expressions)
            
            return query
            
        except Exception as e:
            logger.error(f"Error applying grouping: {str(e)}")
            raise
    
    def _apply_sorting(self, query: Any, sort_by: List[Dict[str, str]]) -> Any:
        """Apply sorting to query"""
        try:
            for sort_spec in sort_by:
                field_key = sort_spec.get('field')
                direction = sort_spec.get('direction', 'asc')
                
                if field_key in self._field_definitions:
                    field_def = self._field_definitions[field_key]
                    column_expr = text(f"{field_def.table}.{field_def.column}")
                    
                    if direction.lower() == 'desc':
                        query = query.order_by(column_expr.desc())
                    else:
                        query = query.order_by(column_expr.asc())
            
            return query
            
        except Exception as e:
            logger.error(f"Error applying sorting: {str(e)}")
            raise
    
    def _format_query_results(self, results: List, fields: List[ReportField]) -> List[Dict[str, Any]]:
        """Format query results for output"""
        try:
            formatted_results = []
            
            for row in results:
                formatted_row = {}
                
                for i, field in enumerate(fields):
                    value = row[i] if i < len(row) else None
                    
                    # Format value based on data type
                    if field.field_key in self._field_definitions:
                        field_def = self._field_definitions[field.field_key]
                        
                        if field_def.data_type == 'decimal' and value is not None:
                            formatted_row[field.field_key] = float(value)
                        elif field_def.data_type == 'date' and value is not None:
                            if hasattr(value, 'isoformat'):
                                formatted_row[field.field_key] = value.isoformat()
                            else:
                                formatted_row[field.field_key] = str(value)
                        else:
                            formatted_row[field.field_key] = value
                    else:
                        formatted_row[field.field_key] = value
                
                formatted_results.append(formatted_row)
            
            return formatted_results
            
        except Exception as e:
            logger.error(f"Error formatting query results: {str(e)}")
            raise
    
    async def save_report_template(self, template_name: str, request: CustomReportRequest) -> str:
        """
        Save a custom report as a reusable template
        
        Args:
            template_name: Name for the template
            request: Custom report configuration
            
        Returns:
            Template ID
        """
        try:
            # In a real implementation, this would save to database
            # For now, we'll just return a mock template ID
            template_id = f"template_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
            
            logger.info(f"Report template '{template_name}' saved with ID: {template_id}")
            
            return template_id
            
        except Exception as e:
            logger.error(f"Error saving report template: {str(e)}")
            raise
    
    async def load_report_template(self, template_id: str) -> CustomReportRequest:
        """
        Load a saved report template
        
        Args:
            template_id: ID of the template to load
            
        Returns:
            Custom report request configuration
        """
        try:
            # In a real implementation, this would load from database
            # For now, return a mock template
            raise NotImplementedError("Template loading not yet implemented")
            
        except Exception as e:
            logger.error(f"Error loading report template {template_id}: {str(e)}")
            raise