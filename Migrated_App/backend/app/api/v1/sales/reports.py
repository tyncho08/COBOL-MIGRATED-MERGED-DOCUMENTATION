"""Sales Reports API endpoints"""

from fastapi import APIRouter, Depends, HTTPException, status, Query
from sqlalchemy.orm import Session
from sqlalchemy import func, and_, or_, desc, extract
from typing import List, Optional, Dict, Any
from datetime import date, datetime, timedelta
from decimal import Decimal
import calendar

from app.core.database import get_db
from app.models.customer import SalesLedgerRec
from app.models.sales import SalesOpenItemRec, SalesHistoryRec

router = APIRouter()

@router.get("")
async def get_sales_reports_summary(
    db: Session = Depends(get_db)
) -> Dict[str, Any]:
    """
    Get summary of available sales reports and key metrics.
    """
    try:
        # Get key metrics for report summary
        total_customers = db.query(func.count(SalesLedgerRec.sales_account_code)).scalar() or 0
        active_customers = db.query(func.count(SalesLedgerRec.sales_account_code)).filter(
            SalesLedgerRec.credit_status != "INACTIVE"
        ).scalar() or 0
        
        total_outstanding = db.query(func.sum(SalesLedgerRec.sales_balance)).filter(
            SalesLedgerRec.sales_balance > 0
        ).scalar() or 0
        
        overdue_amount = db.query(func.sum(SalesOpenItemRec.outstanding_amount)).filter(
            and_(
                SalesOpenItemRec.outstanding_amount > 0,
                SalesOpenItemRec.due_date < date.today()
            )
        ).scalar() or 0
        
        # Current month sales (if we have sales history)
        current_month_start = date.today().replace(day=1)
        monthly_sales = db.query(func.sum(SalesHistoryRec.gross_amount)).filter(
            SalesHistoryRec.invoice_date >= current_month_start
        ).scalar() or 0
        
        return {
            "summary": {
                "total_customers": total_customers,
                "active_customers": active_customers,
                "total_outstanding": float(total_outstanding),
                "overdue_amount": float(overdue_amount),
                "monthly_sales": float(monthly_sales),
                "collection_efficiency": round(
                    (1 - (float(overdue_amount) / max(float(total_outstanding), 1))) * 100, 2
                )
            },
            "available_reports": [
                {
                    "id": "customer_summary",
                    "name": "Customer Summary Report",
                    "description": "Comprehensive customer listing with balances and credit info",
                    "endpoint": "/api/v1/sales/reports/customer-summary"
                },
                {
                    "id": "aging_analysis",
                    "name": "Aging Analysis Report",
                    "description": "Outstanding balances by aging periods",
                    "endpoint": "/api/v1/sales/reports/aging-analysis"
                },
                {
                    "id": "sales_analysis",
                    "name": "Sales Analysis Report",
                    "description": "Sales trends and performance metrics",
                    "endpoint": "/api/v1/sales/reports/sales-analysis"
                },
                {
                    "id": "credit_control",
                    "name": "Credit Control Report",
                    "description": "Customers over credit limit and risk analysis",
                    "endpoint": "/api/v1/sales/reports/credit-control"
                },
                {
                    "id": "payment_analysis",
                    "name": "Payment Analysis Report",
                    "description": "Payment patterns and collection metrics",
                    "endpoint": "/api/v1/sales/reports/payment-analysis"
                }
            ]
        }
        
    except Exception as e:
        print(f"Error fetching sales reports summary: {str(e)}")
        return {
            "summary": {
                "total_customers": 0,
                "active_customers": 0,
                "total_outstanding": 0,
                "overdue_amount": 0,
                "monthly_sales": 0,
                "collection_efficiency": 0
            },
            "available_reports": []
        }

@router.get("/customer-summary")
async def get_customer_summary_report(
    format: str = Query("json", description="Output format: json, csv, pdf"),
    include_inactive: bool = Query(False, description="Include inactive customers"),
    min_balance: Optional[float] = Query(None, description="Minimum balance filter"),
    sort_by: str = Query("name", description="Sort by: name, balance, code"),
    db: Session = Depends(get_db)
) -> Dict[str, Any]:
    """
    Generate customer summary report with balances and credit information.
    """
    try:
        query = db.query(SalesLedgerRec)
        
        if not include_inactive:
            query = query.filter(SalesLedgerRec.credit_status != "INACTIVE")
        
        if min_balance is not None:
            query = query.filter(SalesLedgerRec.sales_balance >= min_balance)
        
        # Apply sorting
        if sort_by == "balance":
            query = query.order_by(desc(SalesLedgerRec.sales_balance))
        elif sort_by == "code":
            query = query.order_by(SalesLedgerRec.sales_account_code)
        else:  # name
            query = query.order_by(SalesLedgerRec.sales_name)
        
        customers = query.all()
        
        report_data = []
        total_balance = 0
        total_credit_limit = 0
        over_limit_count = 0
        
        for customer in customers:
            balance = float(customer.sales_balance or 0)
            credit_limit = float(customer.credit_limit or 0)
            total_balance += balance
            total_credit_limit += credit_limit
            
            is_over_limit = balance > credit_limit if credit_limit > 0 else False
            if is_over_limit:
                over_limit_count += 1
            
            credit_utilization = (balance / credit_limit * 100) if credit_limit > 0 else 0
            
            report_data.append({
                "customer_code": customer.sales_account_code,
                "customer_name": customer.sales_name,
                "balance": balance,
                "credit_limit": credit_limit,
                "credit_utilization": round(credit_utilization, 2),
                "credit_status": customer.credit_status or "ACTIVE",
                "payment_terms": f"{customer.payment_terms_days or 30} days",
                "last_payment": customer.last_payment_date.isoformat() if customer.last_payment_date else None,
                "is_over_limit": is_over_limit,
                "address": f"{customer.address_line_1 or ''}, {customer.city or ''}".strip(", "),
                "contact": customer.contact_name or ""
            })
        
        summary = {
            "total_customers": len(customers),
            "total_balance": total_balance,
            "total_credit_limit": total_credit_limit,
            "average_balance": total_balance / max(len(customers), 1),
            "over_limit_count": over_limit_count,
            "over_limit_percentage": (over_limit_count / max(len(customers), 1)) * 100
        }
        
        return {
            "report_name": "Customer Summary Report",
            "generated_at": datetime.now().isoformat(),
            "parameters": {
                "format": format,
                "include_inactive": include_inactive,
                "min_balance": min_balance,
                "sort_by": sort_by
            },
            "summary": summary,
            "data": report_data
        }
        
    except Exception as e:
        print(f"Error generating customer summary report: {str(e)}")
        return {
            "report_name": "Customer Summary Report",
            "generated_at": datetime.now().isoformat(),
            "error": str(e),
            "data": []
        }

@router.get("/aging-analysis")
async def get_aging_analysis_report(
    as_of_date: Optional[date] = Query(None, description="Aging as of date"),
    customer_code: Optional[str] = Query(None, description="Specific customer"),
    format: str = Query("json", description="Output format: json, csv, pdf"),
    db: Session = Depends(get_db)
) -> Dict[str, Any]:
    """
    Generate detailed aging analysis report.
    """
    try:
        as_of = as_of_date or date.today()
        
        query = db.query(
            SalesOpenItemRec,
            SalesLedgerRec.sales_name
        ).join(
            SalesLedgerRec,
            SalesOpenItemRec.sales_key == SalesLedgerRec.sales_account_code
        ).filter(
            SalesOpenItemRec.outstanding_amount > 0
        )
        
        if customer_code:
            query = query.filter(SalesOpenItemRec.sales_key == customer_code)
        
        results = query.all()
        
        # Aging buckets
        aging_data = {
            "current": {"amount": 0, "count": 0, "invoices": []},
            "1_30": {"amount": 0, "count": 0, "invoices": []},
            "31_60": {"amount": 0, "count": 0, "invoices": []},
            "61_90": {"amount": 0, "count": 0, "invoices": []},
            "over_90": {"amount": 0, "count": 0, "invoices": []}
        }
        
        total_outstanding = 0
        
        for item, customer_name in results:
            amount = float(item.outstanding_amount or 0)
            total_outstanding += amount
            
            days_overdue = 0
            if item.due_date:
                days_overdue = (as_of - item.due_date).days
            
            invoice_data = {
                "invoice_number": item.invoice_number or f"INV-{item.invoice_id}",
                "customer_code": item.sales_key,
                "customer_name": customer_name,
                "invoice_date": item.invoice_date.isoformat() if item.invoice_date else None,
                "due_date": item.due_date.isoformat() if item.due_date else None,
                "amount": amount,
                "days_overdue": max(0, days_overdue)
            }
            
            # Classify into aging bucket
            if days_overdue <= 0:
                bucket = "current"
            elif days_overdue <= 30:
                bucket = "1_30"
            elif days_overdue <= 60:
                bucket = "31_60"
            elif days_overdue <= 90:
                bucket = "61_90"
            else:
                bucket = "over_90"
            
            aging_data[bucket]["amount"] += amount
            aging_data[bucket]["count"] += 1
            aging_data[bucket]["invoices"].append(invoice_data)
        
        # Calculate percentages
        aging_summary = []
        for bucket, data in aging_data.items():
            percentage = (data["amount"] / total_outstanding * 100) if total_outstanding > 0 else 0
            aging_summary.append({
                "period": bucket.replace("_", "-").replace("over-90", "90+ days").title(),
                "amount": data["amount"],
                "count": data["count"],
                "percentage": round(percentage, 2)
            })
        
        return {
            "report_name": "Aging Analysis Report",
            "generated_at": datetime.now().isoformat(),
            "as_of_date": as_of.isoformat(),
            "parameters": {
                "customer_code": customer_code,
                "format": format
            },
            "summary": {
                "total_outstanding": total_outstanding,
                "total_invoices": sum(bucket["count"] for bucket in aging_data.values()),
                "average_age": sum(
                    sum(inv["days_overdue"] for inv in bucket["invoices"]) 
                    for bucket in aging_data.values()
                ) / max(sum(bucket["count"] for bucket in aging_data.values()), 1)
            },
            "aging_summary": aging_summary,
            "detailed_aging": aging_data if not customer_code else None  # Include details only for all customers
        }
        
    except Exception as e:
        print(f"Error generating aging analysis report: {str(e)}")
        return {
            "report_name": "Aging Analysis Report",
            "generated_at": datetime.now().isoformat(),
            "error": str(e),
            "data": []
        }

@router.get("/sales-analysis")
async def get_sales_analysis_report(
    period: str = Query("monthly", description="Period: daily, weekly, monthly, quarterly, yearly"),
    start_date: Optional[date] = Query(None, description="Start date for analysis"),
    end_date: Optional[date] = Query(None, description="End date for analysis"),
    customer_code: Optional[str] = Query(None, description="Specific customer"),
    db: Session = Depends(get_db)
) -> Dict[str, Any]:
    """
    Generate sales analysis and trends report.
    """
    try:
        # Default date range (last 12 months)
        if not end_date:
            end_date = date.today()
        if not start_date:
            start_date = end_date - timedelta(days=365)
        
        query = db.query(SalesHistoryRec)
        
        if customer_code:
            query = query.filter(SalesHistoryRec.sales_key == customer_code)
        
        query = query.filter(
            and_(
                SalesHistoryRec.invoice_date >= start_date,
                SalesHistoryRec.invoice_date <= end_date
            )
        )
        
        sales_data = query.all()
        
        # Group data by period
        period_data = {}
        total_sales = 0
        total_invoices = 0
        
        for sale in sales_data:
            if not sale.invoice_date:
                continue
                
            amount = float(sale.gross_amount or 0)
            total_sales += amount
            total_invoices += 1
            
            # Determine period key
            if period == "daily":
                period_key = sale.invoice_date.isoformat()
            elif period == "weekly":
                year, week, _ = sale.invoice_date.isocalendar()
                period_key = f"{year}-W{week:02d}"
            elif period == "monthly":
                period_key = f"{sale.invoice_date.year}-{sale.invoice_date.month:02d}"
            elif period == "quarterly":
                quarter = (sale.invoice_date.month - 1) // 3 + 1
                period_key = f"{sale.invoice_date.year}-Q{quarter}"
            else:  # yearly
                period_key = str(sale.invoice_date.year)
            
            if period_key not in period_data:
                period_data[period_key] = {
                    "period": period_key,
                    "sales_amount": 0,
                    "invoice_count": 0,
                    "customers": set()
                }
            
            period_data[period_key]["sales_amount"] += amount
            period_data[period_key]["invoice_count"] += 1
            period_data[period_key]["customers"].add(sale.sales_key)
        
        # Convert to list and format
        trends = []
        for period_key in sorted(period_data.keys()):
            data = period_data[period_key]
            trends.append({
                "period": period_key,
                "sales_amount": data["sales_amount"],
                "invoice_count": data["invoice_count"],
                "unique_customers": len(data["customers"]),
                "average_invoice_value": data["sales_amount"] / max(data["invoice_count"], 1)
            })
        
        # Calculate growth rates
        for i in range(1, len(trends)):
            current = trends[i]["sales_amount"]
            previous = trends[i-1]["sales_amount"]
            growth_rate = ((current - previous) / max(previous, 1)) * 100
            trends[i]["growth_rate"] = round(growth_rate, 2)
        
        return {
            "report_name": "Sales Analysis Report",
            "generated_at": datetime.now().isoformat(),
            "parameters": {
                "period": period,
                "start_date": start_date.isoformat(),
                "end_date": end_date.isoformat(),
                "customer_code": customer_code
            },
            "summary": {
                "total_sales": total_sales,
                "total_invoices": total_invoices,
                "average_invoice_value": total_sales / max(total_invoices, 1),
                "date_range_days": (end_date - start_date).days
            },
            "trends": trends
        }
        
    except Exception as e:
        print(f"Error generating sales analysis report: {str(e)}")
        return {
            "report_name": "Sales Analysis Report",
            "generated_at": datetime.now().isoformat(),
            "error": str(e),
            "data": []
        }