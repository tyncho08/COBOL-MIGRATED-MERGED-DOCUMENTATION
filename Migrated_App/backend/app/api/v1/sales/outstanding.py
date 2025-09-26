"""Sales Outstanding Invoices API endpoints"""

from fastapi import APIRouter, Depends, HTTPException, status, Query
from sqlalchemy.orm import Session
from sqlalchemy import func, and_, or_, desc, case
from typing import List, Optional, Dict, Any
from datetime import date, datetime, timedelta
from decimal import Decimal

from app.core.database import get_db
from app.models.customer import SalesLedgerRec
from app.models.sales import SalesOpenItemRec

router = APIRouter()

@router.get("")
async def get_outstanding_invoices(
    customer_code: Optional[str] = Query(None, description="Filter by customer"),
    overdue_only: bool = Query(False, description="Show only overdue invoices"),
    amount_min: Optional[float] = Query(None, description="Minimum outstanding amount"),
    amount_max: Optional[float] = Query(None, description="Maximum outstanding amount"),
    days_overdue_min: Optional[int] = Query(None, description="Minimum days overdue"),
    sort_by: str = Query("due_date", description="Sort by: due_date, amount, customer, days_overdue"),
    sort_order: str = Query("asc", description="Sort order: asc, desc"),
    skip: int = Query(0, description="Records to skip"),
    limit: int = Query(100, description="Max records to return"),
    db: Session = Depends(get_db)
) -> Dict[str, Any]:
    """
    Get outstanding invoices with comprehensive filtering and analytics.
    """
    try:
        # Base query for outstanding items
        query = db.query(
            SalesOpenItemRec,
            SalesLedgerRec.sales_name
        ).join(
            SalesLedgerRec,
            SalesOpenItemRec.sales_key == SalesLedgerRec.sales_account_code
        ).filter(
            SalesOpenItemRec.outstanding_amount > 0
        )
        
        # Apply filters
        if customer_code:
            query = query.filter(SalesOpenItemRec.sales_key == customer_code)
        
        if overdue_only:
            query = query.filter(SalesOpenItemRec.due_date < date.today())
        
        if amount_min:
            query = query.filter(SalesOpenItemRec.outstanding_amount >= amount_min)
        
        if amount_max:
            query = query.filter(SalesOpenItemRec.outstanding_amount <= amount_max)
        
        if days_overdue_min:
            cutoff_date = date.today() - timedelta(days=days_overdue_min)
            query = query.filter(SalesOpenItemRec.due_date <= cutoff_date)
        
        # Apply sorting
        if sort_by == "due_date":
            order_field = SalesOpenItemRec.due_date
        elif sort_by == "amount":
            order_field = SalesOpenItemRec.outstanding_amount
        elif sort_by == "customer":
            order_field = SalesLedgerRec.sales_name
        elif sort_by == "days_overdue":
            order_field = SalesOpenItemRec.due_date  # Will calculate days overdue
        else:
            order_field = SalesOpenItemRec.due_date
        
        if sort_order == "desc":
            query = query.order_by(desc(order_field))
        else:
            query = query.order_by(order_field)
        
        # Get total count before pagination
        total_invoices = query.count()
        
        # Apply pagination
        results = query.offset(skip).limit(limit).all()
        
        # Process results
        invoices = []
        total_outstanding = 0
        total_overdue = 0
        today = date.today()
        
        for item, customer_name in results:
            days_overdue = 0
            is_overdue = False
            
            if item.due_date and item.due_date < today:
                days_overdue = (today - item.due_date).days
                is_overdue = True
            
            outstanding_amount = float(item.outstanding_amount or 0)
            total_outstanding += outstanding_amount
            
            if is_overdue:
                total_overdue += outstanding_amount
            
            # Determine priority/risk level
            priority = "low"
            if days_overdue > 90:
                priority = "critical"
            elif days_overdue > 60:
                priority = "high"
            elif days_overdue > 30:
                priority = "medium"
            elif outstanding_amount > 10000:  # Large amounts
                priority = "medium"
            
            invoices.append({
                "invoice_id": item.invoice_id,
                "invoice_number": item.invoice_number or f"INV-{item.invoice_id}",
                "customer_code": item.sales_key,
                "customer_name": customer_name,
                "invoice_date": item.invoice_date.isoformat() if item.invoice_date else None,
                "due_date": item.due_date.isoformat() if item.due_date else None,
                "gross_amount": float(item.gross_amount or 0),
                "outstanding_amount": outstanding_amount,
                "days_overdue": days_overdue,
                "is_overdue": is_overdue,
                "priority": priority,
                "currency": item.currency_code or "USD",
                "payment_terms": f"{item.payment_terms_days or 30} days" if item.payment_terms_days else "30 days"
            })
        
        # Calculate aging summary
        aging_buckets = {
            "current": {"amount": 0, "count": 0},
            "1_30": {"amount": 0, "count": 0},
            "31_60": {"amount": 0, "count": 0},
            "61_90": {"amount": 0, "count": 0},
            "over_90": {"amount": 0, "count": 0}
        }
        
        for invoice in invoices:
            days_overdue = invoice["days_overdue"]
            amount = invoice["outstanding_amount"]
            
            if days_overdue <= 0:
                aging_buckets["current"]["amount"] += amount
                aging_buckets["current"]["count"] += 1
            elif days_overdue <= 30:
                aging_buckets["1_30"]["amount"] += amount
                aging_buckets["1_30"]["count"] += 1
            elif days_overdue <= 60:
                aging_buckets["31_60"]["amount"] += amount
                aging_buckets["31_60"]["count"] += 1
            elif days_overdue <= 90:
                aging_buckets["61_90"]["amount"] += amount
                aging_buckets["61_90"]["count"] += 1
            else:
                aging_buckets["over_90"]["amount"] += amount
                aging_buckets["over_90"]["count"] += 1
        
        return {
            "invoices": invoices,
            "pagination": {
                "total": total_invoices,
                "skip": skip,
                "limit": limit,
                "has_more": skip + limit < total_invoices
            },
            "summary": {
                "total_outstanding": total_outstanding,
                "total_overdue": total_overdue,
                "total_invoices": total_invoices,
                "overdue_count": sum(1 for inv in invoices if inv["is_overdue"]),
                "average_outstanding": total_outstanding / max(total_invoices, 1),
                "average_days_overdue": sum(inv["days_overdue"] for inv in invoices) / max(total_invoices, 1)
            },
            "aging": [
                {
                    "period": "Current",
                    "amount": aging_buckets["current"]["amount"],
                    "count": aging_buckets["current"]["count"]
                },
                {
                    "period": "1-30 days",
                    "amount": aging_buckets["1_30"]["amount"],
                    "count": aging_buckets["1_30"]["count"]
                },
                {
                    "period": "31-60 days",
                    "amount": aging_buckets["31_60"]["amount"],
                    "count": aging_buckets["31_60"]["count"]
                },
                {
                    "period": "61-90 days",
                    "amount": aging_buckets["61_90"]["amount"],
                    "count": aging_buckets["61_90"]["count"]
                },
                {
                    "period": "90+ days",
                    "amount": aging_buckets["over_90"]["amount"],
                    "count": aging_buckets["over_90"]["count"]
                }
            ]
        }
        
    except Exception as e:
        print(f"Error fetching outstanding invoices: {str(e)}")
        return {
            "invoices": [],
            "pagination": {"total": 0, "skip": skip, "limit": limit, "has_more": False},
            "summary": {
                "total_outstanding": 0,
                "total_overdue": 0,
                "total_invoices": 0,
                "overdue_count": 0,
                "average_outstanding": 0,
                "average_days_overdue": 0
            },
            "aging": []
        }

@router.get("/aging")
async def get_aging_analysis(
    customer_code: Optional[str] = Query(None, description="Filter by customer"),
    as_of_date: Optional[date] = Query(None, description="Aging as of date (default today)"),
    group_by: str = Query("period", description="Group by: period, customer, both"),
    db: Session = Depends(get_db)
) -> Dict[str, Any]:
    """
    Get detailed aging analysis for outstanding amounts.
    """
    try:
        as_of = as_of_date or date.today()
        
        # Base query
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
        
        if group_by == "customer":
            # Group by customer
            customer_aging = {}
            for item, customer_name in results:
                if item.sales_key not in customer_aging:
                    customer_aging[item.sales_key] = {
                        "customer_code": item.sales_key,
                        "customer_name": customer_name,
                        "total_outstanding": 0,
                        "current": 0,
                        "days_1_30": 0,
                        "days_31_60": 0,
                        "days_61_90": 0,
                        "days_over_90": 0
                    }
                
                amount = float(item.outstanding_amount or 0)
                customer_aging[item.sales_key]["total_outstanding"] += amount
                
                if item.due_date:
                    days_overdue = (as_of - item.due_date).days
                    if days_overdue <= 0:
                        customer_aging[item.sales_key]["current"] += amount
                    elif days_overdue <= 30:
                        customer_aging[item.sales_key]["days_1_30"] += amount
                    elif days_overdue <= 60:
                        customer_aging[item.sales_key]["days_31_60"] += amount
                    elif days_overdue <= 90:
                        customer_aging[item.sales_key]["days_61_90"] += amount
                    else:
                        customer_aging[item.sales_key]["days_over_90"] += amount
                else:
                    customer_aging[item.sales_key]["current"] += amount
            
            return {
                "as_of_date": as_of.isoformat(),
                "group_by": group_by,
                "aging_by_customer": list(customer_aging.values())
            }
        
        else:
            # Group by period (default)
            period_totals = {
                "current": {"amount": 0, "count": 0},
                "days_1_30": {"amount": 0, "count": 0},
                "days_31_60": {"amount": 0, "count": 0},
                "days_61_90": {"amount": 0, "count": 0},
                "days_over_90": {"amount": 0, "count": 0}
            }
            
            for item, customer_name in results:
                amount = float(item.outstanding_amount or 0)
                
                if item.due_date:
                    days_overdue = (as_of - item.due_date).days
                    if days_overdue <= 0:
                        period_totals["current"]["amount"] += amount
                        period_totals["current"]["count"] += 1
                    elif days_overdue <= 30:
                        period_totals["days_1_30"]["amount"] += amount
                        period_totals["days_1_30"]["count"] += 1
                    elif days_overdue <= 60:
                        period_totals["days_31_60"]["amount"] += amount
                        period_totals["days_31_60"]["count"] += 1
                    elif days_overdue <= 90:
                        period_totals["days_61_90"]["amount"] += amount
                        period_totals["days_61_90"]["count"] += 1
                    else:
                        period_totals["days_over_90"]["amount"] += amount
                        period_totals["days_over_90"]["count"] += 1
                else:
                    period_totals["current"]["amount"] += amount
                    period_totals["current"]["count"] += 1
            
            total_outstanding = sum(bucket["amount"] for bucket in period_totals.values())
            
            aging_summary = []
            for period, data in period_totals.items():
                percentage = (data["amount"] / total_outstanding * 100) if total_outstanding > 0 else 0
                aging_summary.append({
                    "period": period.replace("_", "-").title(),
                    "amount": data["amount"],
                    "count": data["count"],
                    "percentage": round(percentage, 2)
                })
            
            return {
                "as_of_date": as_of.isoformat(),
                "group_by": group_by,
                "total_outstanding": total_outstanding,
                "aging_summary": aging_summary
            }
        
    except Exception as e:
        print(f"Error generating aging analysis: {str(e)}")
        return {
            "as_of_date": (as_of_date or date.today()).isoformat(),
            "group_by": group_by,
            "total_outstanding": 0,
            "aging_summary": []
        }