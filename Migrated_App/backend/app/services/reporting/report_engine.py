"""
Core Report Engine

Provides the main reporting engine for generating all system reports.
Handles query execution, data processing, and report orchestration.
"""

from typing import Dict, List, Any, Optional, Union
from datetime import datetime, date
from decimal import Decimal
import logging
from sqlalchemy.orm import Session
from sqlalchemy import text, and_, or_, func, case
from sqlalchemy.exc import SQLAlchemyError

from app.core.database import get_db
from app.models.gl import GLAccount, GLTransaction, GLJournalEntry, GLPeriod
from app.models.sl import Customer, SalesInvoice, CashReceipt
from app.models.pl import Supplier, PurchaseInvoice, PurchasePayment
from app.models.stock import StockItem, StockMovement, StockLocation
from app.schemas.reports import (
    ReportRequest, ReportResponse, ReportParameter,
    TrialBalanceReport, ProfitLossReport, BalanceSheetReport
)

logger = logging.getLogger(__name__)


class ReportEngine:
    """Main reporting engine for ACAS system"""
    
    def __init__(self):
        self.db = None
    
    def _get_db(self) -> Session:
        """Get database session"""
        if not self.db:
            self.db = next(get_db_session())
        return self.db
    
    async def generate_report(self, request: ReportRequest) -> ReportResponse:
        """
        Generate a report based on the request parameters
        
        Args:
            request: Report request with type, parameters, and formatting options
            
        Returns:
            ReportResponse with data and metadata
        """
        try:
            db = self._get_db()
            
            # Route to specific report generator
            if request.report_type == "trial_balance":
                data = await self._generate_trial_balance(db, request.parameters)
            elif request.report_type == "profit_loss":
                data = await self._generate_profit_loss(db, request.parameters)
            elif request.report_type == "balance_sheet":
                data = await self._generate_balance_sheet(db, request.parameters)
            elif request.report_type == "customer_aging":
                data = await self._generate_customer_aging(db, request.parameters)
            elif request.report_type == "supplier_aging":
                data = await self._generate_supplier_aging(db, request.parameters)
            elif request.report_type == "stock_valuation":
                data = await self._generate_stock_valuation(db, request.parameters)
            elif request.report_type == "sales_analysis":
                data = await self._generate_sales_analysis(db, request.parameters)
            elif request.report_type == "purchase_analysis":
                data = await self._generate_purchase_analysis(db, request.parameters)
            else:
                raise ValueError(f"Unsupported report type: {request.report_type}")
            
            return ReportResponse(
                report_id=f"{request.report_type}_{datetime.now().strftime('%Y%m%d_%H%M%S')}",
                report_type=request.report_type,
                title=self._get_report_title(request.report_type),
                generated_at=datetime.now(),
                parameters=request.parameters,
                data=data,
                total_records=len(data) if isinstance(data, list) else 1,
                format=request.format
            )
            
        except Exception as e:
            logger.error(f"Error generating report {request.report_type}: {str(e)}")
            raise
    
    async def _generate_trial_balance(self, db: Session, params: Dict[str, Any]) -> List[Dict]:
        """Generate trial balance report"""
        try:
            period_id = params.get('period_id')
            as_at_date = params.get('as_at_date', date.today())
            level_of_detail = params.get('level_of_detail', 'summary')
            
            query = db.query(
                GLAccount.account_code,
                GLAccount.account_name,
                GLAccount.account_type,
                GLAccount.account_category,
                func.sum(
                    case(
                        (GLTransaction.transaction_type == 'DEBIT', GLTransaction.amount),
                        else_=-GLTransaction.amount
                    )
                ).label('balance')
            ).join(
                GLTransaction, GLAccount.account_code == GLTransaction.account_code
            )
            
            if period_id:
                query = query.filter(GLTransaction.period_id == period_id)
            else:
                query = query.filter(GLTransaction.transaction_date <= as_at_date)
            
            query = query.group_by(
                GLAccount.account_code,
                GLAccount.account_name,
                GLAccount.account_type,
                GLAccount.account_category
            ).order_by(GLAccount.account_code)
            
            results = query.all()
            
            trial_balance = []
            debit_total = Decimal('0.00')
            credit_total = Decimal('0.00')
            
            for row in results:
                balance = row.balance or Decimal('0.00')
                debit_amount = balance if balance > 0 else Decimal('0.00')
                credit_amount = abs(balance) if balance < 0 else Decimal('0.00')
                
                trial_balance.append({
                    'account_code': row.account_code,
                    'account_name': row.account_name,
                    'account_type': row.account_type,
                    'account_category': row.account_category,
                    'debit_amount': float(debit_amount),
                    'credit_amount': float(credit_amount),
                    'balance': float(balance)
                })
                
                debit_total += debit_amount
                credit_total += credit_amount
            
            # Add totals row
            trial_balance.append({
                'account_code': 'TOTAL',
                'account_name': 'TRIAL BALANCE TOTALS',
                'account_type': 'TOTAL',
                'account_category': 'TOTAL',
                'debit_amount': float(debit_total),
                'credit_amount': float(credit_total),
                'balance': float(debit_total - credit_total)
            })
            
            return trial_balance
            
        except Exception as e:
            logger.error(f"Error generating trial balance: {str(e)}")
            raise
    
    async def _generate_profit_loss(self, db: Session, params: Dict[str, Any]) -> Dict[str, Any]:
        """Generate profit & loss statement"""
        try:
            period_id = params.get('period_id')
            start_date = params.get('start_date')
            end_date = params.get('end_date', date.today())
            show_comparatives = params.get('show_comparatives', False)
            
            # Income accounts (4000-4999)
            income_query = db.query(
                GLAccount.account_code,
                GLAccount.account_name,
                func.sum(GLTransaction.amount).label('total_amount')
            ).join(
                GLTransaction, GLAccount.account_code == GLTransaction.account_code
            ).filter(
                GLAccount.account_code.like('4%'),
                GLTransaction.transaction_date.between(start_date, end_date)
            ).group_by(
                GLAccount.account_code,
                GLAccount.account_name
            ).order_by(GLAccount.account_code)
            
            income_results = income_query.all()
            
            # Expense accounts (5000-9999)
            expense_query = db.query(
                GLAccount.account_code,
                GLAccount.account_name,
                func.sum(GLTransaction.amount).label('total_amount')
            ).join(
                GLTransaction, GLAccount.account_code == GLTransaction.account_code
            ).filter(
                GLAccount.account_code.like('5%') | 
                GLAccount.account_code.like('6%') |
                GLAccount.account_code.like('7%') |
                GLAccount.account_code.like('8%') |
                GLAccount.account_code.like('9%'),
                GLTransaction.transaction_date.between(start_date, end_date)
            ).group_by(
                GLAccount.account_code,
                GLAccount.account_name
            ).order_by(GLAccount.account_code)
            
            expense_results = expense_query.all()
            
            # Build P&L structure
            income_total = sum(row.total_amount or 0 for row in income_results)
            expense_total = sum(row.total_amount or 0 for row in expense_results)
            net_profit = income_total - expense_total
            
            profit_loss = {
                'period_from': start_date.isoformat() if start_date else None,
                'period_to': end_date.isoformat(),
                'income': [
                    {
                        'account_code': row.account_code,
                        'account_name': row.account_name,
                        'amount': float(row.total_amount or 0)
                    }
                    for row in income_results
                ],
                'income_total': float(income_total),
                'expenses': [
                    {
                        'account_code': row.account_code,
                        'account_name': row.account_name,
                        'amount': float(row.total_amount or 0)
                    }
                    for row in expense_results
                ],
                'expense_total': float(expense_total),
                'net_profit': float(net_profit)
            }
            
            return profit_loss
            
        except Exception as e:
            logger.error(f"Error generating profit & loss: {str(e)}")
            raise
    
    async def _generate_balance_sheet(self, db: Session, params: Dict[str, Any]) -> Dict[str, Any]:
        """Generate balance sheet"""
        try:
            as_at_date = params.get('as_at_date', date.today())
            show_comparatives = params.get('show_comparatives', False)
            
            # Assets (1000-1999)
            assets_query = db.query(
                GLAccount.account_code,
                GLAccount.account_name,
                GLAccount.account_category,
                func.sum(
                    case(
                        (GLTransaction.transaction_type == 'DEBIT', GLTransaction.amount),
                        else_=-GLTransaction.amount
                    )
                ).label('balance')
            ).join(
                GLTransaction, GLAccount.account_code == GLTransaction.account_code
            ).filter(
                GLAccount.account_code.like('1%'),
                GLTransaction.transaction_date <= as_at_date
            ).group_by(
                GLAccount.account_code,
                GLAccount.account_name,
                GLAccount.account_category
            ).order_by(GLAccount.account_code)
            
            assets_results = assets_query.all()
            
            # Liabilities (2000-2999)
            liabilities_query = db.query(
                GLAccount.account_code,
                GLAccount.account_name,
                GLAccount.account_category,
                func.sum(
                    case(
                        (GLTransaction.transaction_type == 'CREDIT', GLTransaction.amount),
                        else_=-GLTransaction.amount
                    )
                ).label('balance')
            ).join(
                GLTransaction, GLAccount.account_code == GLTransaction.account_code
            ).filter(
                GLAccount.account_code.like('2%'),
                GLTransaction.transaction_date <= as_at_date
            ).group_by(
                GLAccount.account_code,
                GLAccount.account_name,
                GLAccount.account_category
            ).order_by(GLAccount.account_code)
            
            liabilities_results = liabilities_query.all()
            
            # Equity (3000-3999)
            equity_query = db.query(
                GLAccount.account_code,
                GLAccount.account_name,
                GLAccount.account_category,
                func.sum(
                    case(
                        (GLTransaction.transaction_type == 'CREDIT', GLTransaction.amount),
                        else_=-GLTransaction.amount
                    )
                ).label('balance')
            ).join(
                GLTransaction, GLAccount.account_code == GLTransaction.account_code
            ).filter(
                GLAccount.account_code.like('3%'),
                GLTransaction.transaction_date <= as_at_date
            ).group_by(
                GLAccount.account_code,
                GLAccount.account_name,
                GLAccount.account_category
            ).order_by(GLAccount.account_code)
            
            equity_results = equity_query.all()
            
            # Calculate totals
            total_assets = sum(row.balance or 0 for row in assets_results)
            total_liabilities = sum(row.balance or 0 for row in liabilities_results)
            total_equity = sum(row.balance or 0 for row in equity_results)
            
            balance_sheet = {
                'as_at_date': as_at_date.isoformat(),
                'assets': [
                    {
                        'account_code': row.account_code,
                        'account_name': row.account_name,
                        'account_category': row.account_category,
                        'amount': float(row.balance or 0)
                    }
                    for row in assets_results
                ],
                'total_assets': float(total_assets),
                'liabilities': [
                    {
                        'account_code': row.account_code,
                        'account_name': row.account_name,
                        'account_category': row.account_category,
                        'amount': float(row.balance or 0)
                    }
                    for row in liabilities_results
                ],
                'total_liabilities': float(total_liabilities),
                'equity': [
                    {
                        'account_code': row.account_code,
                        'account_name': row.account_name,
                        'account_category': row.account_category,
                        'amount': float(row.balance or 0)
                    }
                    for row in equity_results
                ],
                'total_equity': float(total_equity),
                'total_liabilities_equity': float(total_liabilities + total_equity)
            }
            
            return balance_sheet
            
        except Exception as e:
            logger.error(f"Error generating balance sheet: {str(e)}")
            raise
    
    async def _generate_customer_aging(self, db: Session, params: Dict[str, Any]) -> List[Dict]:
        """Generate customer aging report"""
        try:
            as_at_date = params.get('as_at_date', date.today())
            customer_range = params.get('customer_range', 'ALL')
            
            # Complex aging query
            aging_query = text("""
                SELECT 
                    c.customer_code,
                    c.customer_name,
                    c.credit_limit,
                    SUM(CASE WHEN CURRENT_DATE - si.invoice_date <= 30 THEN si.outstanding_amount ELSE 0 END) as current_amount,
                    SUM(CASE WHEN CURRENT_DATE - si.invoice_date BETWEEN 31 AND 60 THEN si.outstanding_amount ELSE 0 END) as days_31_60,
                    SUM(CASE WHEN CURRENT_DATE - si.invoice_date BETWEEN 61 AND 90 THEN si.outstanding_amount ELSE 0 END) as days_61_90,
                    SUM(CASE WHEN CURRENT_DATE - si.invoice_date > 90 THEN si.outstanding_amount ELSE 0 END) as days_over_90,
                    SUM(si.outstanding_amount) as total_outstanding
                FROM customers c
                LEFT JOIN sales_invoices si ON c.customer_code = si.customer_code 
                WHERE si.outstanding_amount > 0 
                    AND si.invoice_date <= :as_at_date
                GROUP BY c.customer_code, c.customer_name, c.credit_limit
                HAVING SUM(si.outstanding_amount) > 0
                ORDER BY c.customer_code
            """)
            
            results = db.execute(aging_query, {"as_at_date": as_at_date}).fetchall()
            
            aging_report = []
            for row in results:
                aging_report.append({
                    'customer_code': row[0],
                    'customer_name': row[1],
                    'credit_limit': float(row[2] or 0),
                    'current': float(row[3] or 0),
                    'days_31_60': float(row[4] or 0),
                    'days_61_90': float(row[5] or 0),
                    'days_over_90': float(row[6] or 0),
                    'total_outstanding': float(row[7] or 0)
                })
            
            return aging_report
            
        except Exception as e:
            logger.error(f"Error generating customer aging: {str(e)}")
            raise
    
    async def _generate_supplier_aging(self, db: Session, params: Dict[str, Any]) -> List[Dict]:
        """Generate supplier aging report"""
        try:
            as_at_date = params.get('as_at_date', date.today())
            
            aging_query = text("""
                SELECT 
                    s.supplier_code,
                    s.supplier_name,
                    SUM(CASE WHEN CURRENT_DATE - pi.invoice_date <= 30 THEN pi.outstanding_amount ELSE 0 END) as current_amount,
                    SUM(CASE WHEN CURRENT_DATE - pi.invoice_date BETWEEN 31 AND 60 THEN pi.outstanding_amount ELSE 0 END) as days_31_60,
                    SUM(CASE WHEN CURRENT_DATE - pi.invoice_date BETWEEN 61 AND 90 THEN pi.outstanding_amount ELSE 0 END) as days_61_90,
                    SUM(CASE WHEN CURRENT_DATE - pi.invoice_date > 90 THEN pi.outstanding_amount ELSE 0 END) as days_over_90,
                    SUM(pi.outstanding_amount) as total_outstanding
                FROM suppliers s
                LEFT JOIN purchase_invoices pi ON s.supplier_code = pi.supplier_code 
                WHERE pi.outstanding_amount > 0 
                    AND pi.invoice_date <= :as_at_date
                GROUP BY s.supplier_code, s.supplier_name
                HAVING SUM(pi.outstanding_amount) > 0
                ORDER BY s.supplier_code
            """)
            
            results = db.execute(aging_query, {"as_at_date": as_at_date}).fetchall()
            
            aging_report = []
            for row in results:
                aging_report.append({
                    'supplier_code': row[0],
                    'supplier_name': row[1],
                    'current': float(row[2] or 0),
                    'days_31_60': float(row[3] or 0),
                    'days_61_90': float(row[4] or 0),
                    'days_over_90': float(row[5] or 0),
                    'total_outstanding': float(row[6] or 0)
                })
            
            return aging_report
            
        except Exception as e:
            logger.error(f"Error generating supplier aging: {str(e)}")
            raise
    
    async def _generate_stock_valuation(self, db: Session, params: Dict[str, Any]) -> List[Dict]:
        """Generate stock valuation report"""
        try:
            as_at_date = params.get('as_at_date', date.today())
            location_code = params.get('location_code', 'ALL')
            costing_method = params.get('costing_method', 'FIFO')
            
            query = db.query(
                StockItem.item_code,
                StockItem.item_name,
                StockItem.item_category,
                StockItem.costing_method,
                StockItem.standard_cost,
                func.sum(StockMovement.quantity).label('quantity_on_hand'),
                case(
                    (StockItem.costing_method == 'FIFO', StockItem.fifo_cost),
                    (StockItem.costing_method == 'LIFO', StockItem.lifo_cost),
                    (StockItem.costing_method == 'AVERAGE', StockItem.average_cost),
                    else_=StockItem.standard_cost
                ).label('unit_cost')
            ).join(
                StockMovement, StockItem.item_code == StockMovement.item_code
            ).filter(
                StockMovement.movement_date <= as_at_date
            )
            
            if location_code != 'ALL':
                query = query.filter(StockMovement.location_code == location_code)
            
            query = query.group_by(
                StockItem.item_code,
                StockItem.item_name,
                StockItem.item_category,
                StockItem.costing_method,
                StockItem.standard_cost,
                StockItem.fifo_cost,
                StockItem.lifo_cost,
                StockItem.average_cost
            ).having(
                func.sum(StockMovement.quantity) > 0
            ).order_by(StockItem.item_code)
            
            results = query.all()
            
            valuation_report = []
            total_quantity = 0
            total_value = Decimal('0.00')
            
            for row in results:
                quantity = row.quantity_on_hand or 0
                unit_cost = row.unit_cost or Decimal('0.00')
                total_cost = quantity * unit_cost
                
                valuation_report.append({
                    'item_code': row.item_code,
                    'item_name': row.item_name,
                    'item_category': row.item_category,
                    'costing_method': row.costing_method,
                    'quantity_on_hand': quantity,
                    'unit_cost': float(unit_cost),
                    'total_cost': float(total_cost),
                    'standard_cost': float(row.standard_cost or 0)
                })
                
                total_quantity += quantity
                total_value += total_cost
            
            # Add summary
            valuation_report.append({
                'item_code': 'TOTAL',
                'item_name': 'STOCK VALUATION TOTAL',
                'item_category': 'TOTAL',
                'costing_method': costing_method,
                'quantity_on_hand': total_quantity,
                'unit_cost': 0,
                'total_cost': float(total_value),
                'standard_cost': 0
            })
            
            return valuation_report
            
        except Exception as e:
            logger.error(f"Error generating stock valuation: {str(e)}")
            raise
    
    async def _generate_sales_analysis(self, db: Session, params: Dict[str, Any]) -> List[Dict]:
        """Generate sales analysis report"""
        try:
            start_date = params.get('start_date')
            end_date = params.get('end_date', date.today())
            group_by = params.get('group_by', 'customer')
            
            if group_by == 'customer':
                query = db.query(
                    Customer.customer_code,
                    Customer.customer_name,
                    func.count(SalesInvoice.invoice_id).label('invoice_count'),
                    func.sum(SalesInvoice.invoice_total).label('sales_total'),
                    func.avg(SalesInvoice.invoice_total).label('average_sale')
                ).join(
                    SalesInvoice, Customer.customer_code == SalesInvoice.customer_code
                ).filter(
                    SalesInvoice.invoice_date.between(start_date, end_date)
                ).group_by(
                    Customer.customer_code,
                    Customer.customer_name
                ).order_by(func.sum(SalesInvoice.invoice_total).desc())
            
            results = query.all()
            
            analysis_report = []
            for row in results:
                analysis_report.append({
                    'customer_code': row.customer_code,
                    'customer_name': row.customer_name,
                    'invoice_count': row.invoice_count,
                    'sales_total': float(row.sales_total or 0),
                    'average_sale': float(row.average_sale or 0)
                })
            
            return analysis_report
            
        except Exception as e:
            logger.error(f"Error generating sales analysis: {str(e)}")
            raise
    
    async def _generate_purchase_analysis(self, db: Session, params: Dict[str, Any]) -> List[Dict]:
        """Generate purchase analysis report"""
        try:
            start_date = params.get('start_date')
            end_date = params.get('end_date', date.today())
            
            query = db.query(
                Supplier.supplier_code,
                Supplier.supplier_name,
                func.count(PurchaseInvoice.invoice_id).label('invoice_count'),
                func.sum(PurchaseInvoice.invoice_total).label('purchase_total'),
                func.avg(PurchaseInvoice.invoice_total).label('average_purchase')
            ).join(
                PurchaseInvoice, Supplier.supplier_code == PurchaseInvoice.supplier_code
            ).filter(
                PurchaseInvoice.invoice_date.between(start_date, end_date)
            ).group_by(
                Supplier.supplier_code,
                Supplier.supplier_name
            ).order_by(func.sum(PurchaseInvoice.invoice_total).desc())
            
            results = query.all()
            
            analysis_report = []
            for row in results:
                analysis_report.append({
                    'supplier_code': row.supplier_code,
                    'supplier_name': row.supplier_name,
                    'invoice_count': row.invoice_count,
                    'purchase_total': float(row.purchase_total or 0),
                    'average_purchase': float(row.average_purchase or 0)
                })
            
            return analysis_report
            
        except Exception as e:
            logger.error(f"Error generating purchase analysis: {str(e)}")
            raise
    
    def _get_report_title(self, report_type: str) -> str:
        """Get human-readable report title"""
        titles = {
            'trial_balance': 'Trial Balance',
            'profit_loss': 'Profit & Loss Statement',
            'balance_sheet': 'Balance Sheet',
            'customer_aging': 'Customer Aging Report',
            'supplier_aging': 'Supplier Aging Report',
            'stock_valuation': 'Stock Valuation Report',
            'sales_analysis': 'Sales Analysis Report',
            'purchase_analysis': 'Purchase Analysis Report'
        }
        return titles.get(report_type, report_type.title())