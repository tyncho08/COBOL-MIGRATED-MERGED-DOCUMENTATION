"""Sales Analytics API endpoints"""

from fastapi import APIRouter, Depends, HTTPException, status, Query
from sqlalchemy.orm import Session
from sqlalchemy import func, and_, or_, desc, extract, case
from typing import List, Optional, Dict, Any
from datetime import date, datetime, timedelta
from decimal import Decimal
import calendar

from app.core.database import get_db
from app.models.customer import SalesLedgerRec
from app.models.sales import SalesOpenItemRec, SalesHistoryRec

router = APIRouter()

@router.get("")
async def get_sales_analytics_dashboard(
    period: str = Query("monthly", description="Analysis period: monthly, quarterly, yearly"),
    compare_periods: int = Query(12, description="Number of periods to compare"),
    db: Session = Depends(get_db)
) -> Dict[str, Any]:
    """
    Get comprehensive sales analytics dashboard with KPIs and trends.
    """
    try:
        today = date.today()
        
        # Calculate date ranges based on period
        if period == "monthly":
            current_period_start = today.replace(day=1)
            previous_period_start = (current_period_start - timedelta(days=1)).replace(day=1)
            period_name = "Month"
        elif period == "quarterly":
            quarter = (today.month - 1) // 3 + 1
            quarter_start_month = (quarter - 1) * 3 + 1
            current_period_start = today.replace(month=quarter_start_month, day=1)
            if quarter == 1:
                previous_period_start = today.replace(year=today.year-1, month=10, day=1)
            else:
                prev_quarter_start_month = (quarter - 2) * 3 + 1
                previous_period_start = today.replace(month=prev_quarter_start_month, day=1)
            period_name = "Quarter"
        else:  # yearly
            current_period_start = today.replace(month=1, day=1)
            previous_period_start = today.replace(year=today.year-1, month=1, day=1)
            period_name = "Year"
        
        # Key Performance Indicators
        kpis = await calculate_sales_kpis(db, current_period_start, previous_period_start, today)
        
        # Customer Analytics
        customer_analytics = await calculate_customer_analytics(db)
        
        # Revenue Trends
        revenue_trends = await calculate_revenue_trends(db, period, compare_periods, today)
        
        # Outstanding Analysis
        outstanding_analysis = await calculate_outstanding_analytics(db, today)
        
        # Top Performers
        top_performers = await calculate_top_performers(db, current_period_start, today)
        
        return {
            "dashboard_name": "Sales Analytics Dashboard",
            "generated_at": datetime.now().isoformat(),
            "period": period,
            "period_name": period_name,
            "current_period_start": current_period_start.isoformat(),
            "kpis": kpis,
            "customer_analytics": customer_analytics,
            "revenue_trends": revenue_trends,
            "outstanding_analysis": outstanding_analysis,
            "top_performers": top_performers
        }
        
    except Exception as e:
        print(f"Error generating sales analytics dashboard: {str(e)}")
        return {
            "dashboard_name": "Sales Analytics Dashboard",
            "generated_at": datetime.now().isoformat(),
            "error": str(e),
            "kpis": {},
            "customer_analytics": {},
            "revenue_trends": [],
            "outstanding_analysis": {},
            "top_performers": {}
        }

async def calculate_sales_kpis(db: Session, current_start: date, previous_start: date, today: date) -> Dict[str, Any]:
    """Calculate key sales performance indicators."""
    
    # Current period sales
    current_sales = db.query(func.sum(SalesHistoryRec.gross_amount)).filter(
        SalesHistoryRec.invoice_date >= current_start
    ).scalar() or 0
    
    # Previous period sales
    previous_sales = db.query(func.sum(SalesHistoryRec.gross_amount)).filter(
        and_(
            SalesHistoryRec.invoice_date >= previous_start,
            SalesHistoryRec.invoice_date < current_start
        )
    ).scalar() or 0
    
    # Growth calculation
    sales_growth = 0
    if previous_sales > 0:
        sales_growth = ((current_sales - previous_sales) / previous_sales) * 100
    
    # Current period invoice count
    current_invoices = db.query(func.count(SalesHistoryRec.invoice_id)).filter(
        SalesHistoryRec.invoice_date >= current_start
    ).scalar() or 0
    
    # Previous period invoice count
    previous_invoices = db.query(func.count(SalesHistoryRec.invoice_id)).filter(
        and_(
            SalesHistoryRec.invoice_date >= previous_start,
            SalesHistoryRec.invoice_date < current_start
        )
    ).scalar() or 0
    
    # Average invoice value
    avg_invoice_current = current_sales / max(current_invoices, 1)
    avg_invoice_previous = previous_sales / max(previous_invoices, 1)
    avg_invoice_growth = 0
    if avg_invoice_previous > 0:
        avg_invoice_growth = ((avg_invoice_current - avg_invoice_previous) / avg_invoice_previous) * 100
    
    # Outstanding metrics
    total_outstanding = db.query(func.sum(SalesLedgerRec.sales_balance)).filter(
        SalesLedgerRec.sales_balance > 0
    ).scalar() or 0
    
    overdue_amount = db.query(func.sum(SalesOpenItemRec.outstanding_amount)).filter(
        and_(
            SalesOpenItemRec.outstanding_amount > 0,
            SalesOpenItemRec.due_date < today
        )
    ).scalar() or 0
    
    # Collection efficiency
    collection_efficiency = 0
    if total_outstanding > 0:
        collection_efficiency = (1 - (overdue_amount / total_outstanding)) * 100
    
    return {
        "current_period_sales": float(current_sales),
        "previous_period_sales": float(previous_sales),
        "sales_growth_percentage": round(sales_growth, 2),
        "current_period_invoices": current_invoices,
        "previous_period_invoices": previous_invoices,
        "average_invoice_value": round(avg_invoice_current, 2),
        "avg_invoice_growth_percentage": round(avg_invoice_growth, 2),
        "total_outstanding": float(total_outstanding),
        "overdue_amount": float(overdue_amount),
        "collection_efficiency": round(collection_efficiency, 2)
    }

async def calculate_customer_analytics(db: Session) -> Dict[str, Any]:
    """Calculate customer-related analytics."""
    
    total_customers = db.query(func.count(SalesLedgerRec.sales_account_code)).scalar() or 0
    
    active_customers = db.query(func.count(SalesLedgerRec.sales_account_code)).filter(
        SalesLedgerRec.credit_status != "INACTIVE"
    ).scalar() or 0
    
    customers_with_balance = db.query(func.count(SalesLedgerRec.sales_account_code)).filter(
        SalesLedgerRec.sales_balance > 0
    ).scalar() or 0
    
    blocked_customers = db.query(func.count(SalesLedgerRec.sales_account_code)).filter(
        SalesLedgerRec.credit_status == "BLOCKED"
    ).scalar() or 0
    
    over_limit_customers = db.query(func.count(SalesLedgerRec.sales_account_code)).filter(
        SalesLedgerRec.sales_balance > SalesLedgerRec.credit_limit
    ).scalar() or 0
    
    # Credit utilization distribution
    credit_utilization_stats = db.query(
        func.avg(SalesLedgerRec.sales_balance / SalesLedgerRec.credit_limit).label('avg_utilization'),
        func.max(SalesLedgerRec.sales_balance / SalesLedgerRec.credit_limit).label('max_utilization')
    ).filter(
        and_(
            SalesLedgerRec.credit_limit > 0,
            SalesLedgerRec.sales_balance > 0
        )
    ).first()
    
    avg_utilization = credit_utilization_stats.avg_utilization or 0
    max_utilization = credit_utilization_stats.max_utilization or 0
    
    return {
        "total_customers": total_customers,
        "active_customers": active_customers,
        "customers_with_balance": customers_with_balance,
        "blocked_customers": blocked_customers,
        "over_limit_customers": over_limit_customers,
        "customer_activity_rate": round((customers_with_balance / max(total_customers, 1)) * 100, 2),
        "credit_risk_percentage": round((over_limit_customers / max(active_customers, 1)) * 100, 2),
        "average_credit_utilization": round(float(avg_utilization) * 100, 2),
        "max_credit_utilization": round(float(max_utilization) * 100, 2)
    }

async def calculate_revenue_trends(db: Session, period: str, periods: int, today: date) -> List[Dict[str, Any]]:
    """Calculate revenue trends over specified periods."""
    
    trends = []
    
    for i in range(periods):
        if period == "monthly":
            period_date = today.replace(day=1) - timedelta(days=i*30)
            period_start = period_date.replace(day=1)
            next_month = period_start.replace(day=28) + timedelta(days=4)
            period_end = next_month - timedelta(days=next_month.day)
            period_label = period_start.strftime("%Y-%m")
        elif period == "quarterly":
            # Simplified quarterly calculation
            quarter_offset = i
            year_offset = quarter_offset // 4
            quarter_in_year = quarter_offset % 4
            target_year = today.year - year_offset
            target_quarter = ((today.month - 1) // 3) - quarter_in_year + 1
            if target_quarter <= 0:
                target_quarter += 4
                target_year -= 1
            
            quarter_start_month = (target_quarter - 1) * 3 + 1
            period_start = date(target_year, quarter_start_month, 1)
            period_end = date(target_year, quarter_start_month + 2, 1)
            period_end = period_end.replace(day=calendar.monthrange(period_end.year, period_end.month)[1])
            period_label = f"{target_year}-Q{target_quarter}"
        else:  # yearly
            target_year = today.year - i
            period_start = date(target_year, 1, 1)
            period_end = date(target_year, 12, 31)
            period_label = str(target_year)
        
        # Calculate sales for this period
        period_sales = db.query(func.sum(SalesHistoryRec.gross_amount)).filter(
            and_(
                SalesHistoryRec.invoice_date >= period_start,
                SalesHistoryRec.invoice_date <= period_end
            )
        ).scalar() or 0
        
        period_invoices = db.query(func.count(SalesHistoryRec.invoice_id)).filter(
            and_(
                SalesHistoryRec.invoice_date >= period_start,
                SalesHistoryRec.invoice_date <= period_end
            )
        ).scalar() or 0
        
        trends.append({
            "period": period_label,
            "start_date": period_start.isoformat(),
            "end_date": period_end.isoformat(),
            "sales_amount": float(period_sales),
            "invoice_count": period_invoices,
            "average_invoice_value": float(period_sales) / max(period_invoices, 1)
        })
    
    # Sort by period (most recent first)
    trends.reverse()
    return trends

async def calculate_outstanding_analytics(db: Session, today: date) -> Dict[str, Any]:
    """Calculate detailed outstanding analysis."""
    
    # Aging buckets
    aging_query = db.query(
        case(
            [(SalesOpenItemRec.due_date >= today, 'current')],
            else_=case([
                (SalesOpenItemRec.due_date >= today - timedelta(days=30), '1_30'),
                (SalesOpenItemRec.due_date >= today - timedelta(days=60), '31_60'),
                (SalesOpenItemRec.due_date >= today - timedelta(days=90), '61_90')
            ], else_='over_90')
        ).label('bucket'),
        func.sum(SalesOpenItemRec.outstanding_amount).label('amount'),
        func.count(SalesOpenItemRec.invoice_id).label('count')
    ).filter(
        SalesOpenItemRec.outstanding_amount > 0
    ).group_by('bucket').all()
    
    aging_analysis = {}
    total_outstanding = 0
    
    for bucket, amount, count in aging_query:
        aging_analysis[bucket] = {
            "amount": float(amount or 0),
            "count": count or 0
        }
        total_outstanding += float(amount or 0)
    
    # Calculate percentages
    for bucket in aging_analysis:
        if total_outstanding > 0:
            aging_analysis[bucket]["percentage"] = round(
                (aging_analysis[bucket]["amount"] / total_outstanding) * 100, 2
            )
        else:
            aging_analysis[bucket]["percentage"] = 0
    
    return {
        "total_outstanding": total_outstanding,
        "aging_distribution": aging_analysis,
        "risk_metrics": {
            "overdue_amount": sum(
                aging_analysis.get(bucket, {}).get("amount", 0) 
                for bucket in ['1_30', '31_60', '61_90', 'over_90']
            ),
            "severely_overdue": aging_analysis.get('over_90', {}).get("amount", 0),
            "collection_priority_count": sum(
                aging_analysis.get(bucket, {}).get("count", 0) 
                for bucket in ['61_90', 'over_90']
            )
        }
    }

async def calculate_top_performers(db: Session, period_start: date, today: date) -> Dict[str, Any]:
    """Calculate top performing customers and metrics."""
    
    # Top customers by sales volume this period
    top_customers_query = db.query(
        SalesHistoryRec.sales_key,
        SalesLedgerRec.sales_name,
        func.sum(SalesHistoryRec.gross_amount).label('total_sales'),
        func.count(SalesHistoryRec.invoice_id).label('invoice_count')
    ).join(
        SalesLedgerRec,
        SalesHistoryRec.sales_key == SalesLedgerRec.sales_account_code
    ).filter(
        SalesHistoryRec.invoice_date >= period_start
    ).group_by(
        SalesHistoryRec.sales_key,
        SalesLedgerRec.sales_name
    ).order_by(
        desc('total_sales')
    ).limit(10).all()
    
    top_customers = []
    for customer in top_customers_query:
        top_customers.append({
            "customer_code": customer.sales_key,
            "customer_name": customer.sales_name,
            "total_sales": float(customer.total_sales or 0),
            "invoice_count": customer.invoice_count or 0,
            "average_invoice": float(customer.total_sales or 0) / max(customer.invoice_count or 1, 1)
        })
    
    # Highest outstanding balances
    top_outstanding_query = db.query(
        SalesLedgerRec.sales_account_code,
        SalesLedgerRec.sales_name,
        SalesLedgerRec.sales_balance
    ).filter(
        SalesLedgerRec.sales_balance > 0
    ).order_by(
        desc(SalesLedgerRec.sales_balance)
    ).limit(10).all()
    
    top_outstanding = []
    for customer in top_outstanding_query:
        top_outstanding.append({
            "customer_code": customer.sales_account_code,
            "customer_name": customer.sales_name,
            "outstanding_balance": float(customer.sales_balance or 0)
        })
    
    return {
        "top_customers_by_sales": top_customers,
        "top_outstanding_balances": top_outstanding
    }

@router.get("/kpis")
async def get_sales_kpis(
    period: str = Query("monthly", description="KPI period: monthly, quarterly, yearly"),
    db: Session = Depends(get_db)
) -> Dict[str, Any]:
    """
    Get focused sales KPIs for specific period.
    """
    try:
        today = date.today()
        
        if period == "monthly":
            current_start = today.replace(day=1)
            previous_start = (current_start - timedelta(days=1)).replace(day=1)
        elif period == "quarterly":
            quarter = (today.month - 1) // 3 + 1
            quarter_start_month = (quarter - 1) * 3 + 1
            current_start = today.replace(month=quarter_start_month, day=1)
            if quarter == 1:
                previous_start = today.replace(year=today.year-1, month=10, day=1)
            else:
                prev_quarter_start_month = (quarter - 2) * 3 + 1
                previous_start = today.replace(month=prev_quarter_start_month, day=1)
        else:  # yearly
            current_start = today.replace(month=1, day=1)
            previous_start = today.replace(year=today.year-1, month=1, day=1)
        
        kpis = await calculate_sales_kpis(db, current_start, previous_start, today)
        
        return {
            "period": period,
            "current_period_start": current_start.isoformat(),
            "generated_at": datetime.now().isoformat(),
            "kpis": kpis
        }
        
    except Exception as e:
        print(f"Error calculating sales KPIs: {str(e)}")
        return {
            "period": period,
            "generated_at": datetime.now().isoformat(),
            "error": str(e),
            "kpis": {}
        }