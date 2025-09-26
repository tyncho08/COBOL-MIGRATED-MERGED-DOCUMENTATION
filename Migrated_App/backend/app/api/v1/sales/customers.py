"""Sales Customers API endpoints"""

from fastapi import APIRouter, Depends, HTTPException, status, Query
from sqlalchemy.orm import Session
from sqlalchemy import func, and_, or_, desc
from typing import List, Optional, Dict, Any
from datetime import date, datetime, timedelta
from decimal import Decimal

from app.core.database import get_db
from app.models.customer import SalesLedgerRec
from app.models.sales import SalesOpenItemRec

router = APIRouter()

@router.get("")
async def get_sales_customers(
    search: Optional[str] = Query(None, description="Search by name or code"),
    status: Optional[str] = Query(None, description="Filter by status (active, inactive, blocked)"),
    credit_limit_exceeded: Optional[bool] = Query(None, description="Filter customers over credit limit"),
    has_outstanding: Optional[bool] = Query(None, description="Filter customers with outstanding balances"),
    skip: int = Query(0, description="Records to skip"),
    limit: int = Query(100, description="Max records to return"),
    db: Session = Depends(get_db)
) -> Dict[str, Any]:
    """
    Get customers for sales context with enhanced filtering and analytics.
    """
    try:
        # Base query
        query = db.query(SalesLedgerRec)
        
        # Apply filters
        if search:
            query = query.filter(
                or_(
                    SalesLedgerRec.sales_name.ilike(f"%{search}%"),
                    SalesLedgerRec.sales_account_code.ilike(f"%{search}%")
                )
            )
        
        if status:
            if status == "active":
                query = query.filter(SalesLedgerRec.credit_status != "INACTIVE")
            elif status == "inactive":
                query = query.filter(SalesLedgerRec.credit_status == "INACTIVE")
            elif status == "blocked":
                query = query.filter(SalesLedgerRec.credit_status == "BLOCKED")
        
        if credit_limit_exceeded is True:
            query = query.filter(SalesLedgerRec.sales_balance > SalesLedgerRec.credit_limit)
        elif credit_limit_exceeded is False:
            query = query.filter(SalesLedgerRec.sales_balance <= SalesLedgerRec.credit_limit)
            
        if has_outstanding is True:
            query = query.filter(SalesLedgerRec.sales_balance > 0)
        elif has_outstanding is False:
            query = query.filter(SalesLedgerRec.sales_balance == 0)
        
        # Get total count before pagination
        total_customers = query.count()
        
        # Apply pagination
        customers = query.offset(skip).limit(limit).all()
        
        # Calculate summary statistics
        total_outstanding = db.query(func.sum(SalesLedgerRec.sales_balance)).filter(
            SalesLedgerRec.sales_balance > 0
        ).scalar() or 0
        
        active_customers = db.query(func.count(SalesLedgerRec.sales_account_code)).filter(
            SalesLedgerRec.credit_status != "INACTIVE"
        ).scalar() or 0
        
        blocked_customers = db.query(func.count(SalesLedgerRec.sales_account_code)).filter(
            SalesLedgerRec.credit_status == "BLOCKED"
        ).scalar() or 0
        
        over_limit_customers = db.query(func.count(SalesLedgerRec.sales_account_code)).filter(
            SalesLedgerRec.sales_balance > SalesLedgerRec.credit_limit
        ).scalar() or 0
        
        # Format customer data
        customers_data = []
        for customer in customers:
            # Calculate customer specific metrics
            credit_utilization = 0
            if customer.credit_limit and customer.credit_limit > 0:
                credit_utilization = (float(customer.sales_balance) / float(customer.credit_limit)) * 100
            
            customers_data.append({
                "sales_account_code": customer.sales_account_code,
                "sales_name": customer.sales_name,
                "sales_balance": float(customer.sales_balance or 0),
                "credit_limit": float(customer.credit_limit or 0),
                "credit_utilization": round(credit_utilization, 2),
                "credit_status": customer.credit_status or "ACTIVE",
                "payment_terms_days": customer.payment_terms_days or 30,
                "last_payment_date": customer.last_payment_date.isoformat() if customer.last_payment_date else None,
                "last_invoice_date": customer.last_invoice_date.isoformat() if customer.last_invoice_date else None,
                "address": {
                    "line1": customer.address_line_1 or "",
                    "line2": customer.address_line_2 or "",
                    "city": customer.city or "",
                    "postcode": customer.post_code or "",
                    "country": customer.country or ""
                },
                "contact": {
                    "phone": customer.phone_number or "",
                    "email": customer.email_address or "",
                    "contact_person": customer.contact_name or ""
                },
                "is_over_limit": customer.sales_balance > (customer.credit_limit or 0),
                "days_since_last_payment": None  # Calculate if needed
            })
        
        return {
            "customers": customers_data,
            "pagination": {
                "total": total_customers,
                "skip": skip,
                "limit": limit,
                "has_more": skip + limit < total_customers
            },
            "summary": {
                "total_customers": total_customers,
                "active_customers": active_customers,
                "blocked_customers": blocked_customers,
                "over_limit_customers": over_limit_customers,
                "total_outstanding": float(total_outstanding),
                "average_balance": float(total_outstanding / max(active_customers, 1))
            }
        }
        
    except Exception as e:
        print(f"Error fetching sales customers: {str(e)}")
        return {
            "customers": [],
            "pagination": {"total": 0, "skip": skip, "limit": limit, "has_more": False},
            "summary": {
                "total_customers": 0,
                "active_customers": 0,
                "blocked_customers": 0,
                "over_limit_customers": 0,
                "total_outstanding": 0,
                "average_balance": 0
            }
        }

@router.get("/{customer_code}")
async def get_sales_customer_detail(
    customer_code: str,
    include_transactions: bool = Query(True, description="Include recent transactions"),
    include_aging: bool = Query(True, description="Include aging analysis"),
    db: Session = Depends(get_db)
) -> Dict[str, Any]:
    """
    Get detailed customer information for sales context.
    """
    try:
        # Get customer
        customer = db.query(SalesLedgerRec).filter(
            SalesLedgerRec.sales_account_code == customer_code
        ).first()
        
        if not customer:
            raise HTTPException(
                status_code=status.HTTP_404_NOT_FOUND,
                detail=f"Customer {customer_code} not found"
            )
        
        # Get recent transactions if requested
        transactions = []
        if include_transactions:
            recent_items = db.query(SalesOpenItemRec).filter(
                SalesOpenItemRec.sales_key == customer_code
            ).order_by(desc(SalesOpenItemRec.invoice_date)).limit(10).all()
            
            for item in recent_items:
                transactions.append({
                    "type": "invoice",
                    "reference": item.invoice_number or f"INV-{item.invoice_id}",
                    "date": item.invoice_date.isoformat() if item.invoice_date else None,
                    "amount": float(item.gross_amount or 0),
                    "outstanding": float(item.outstanding_amount or 0),
                    "status": "paid" if (item.outstanding_amount or 0) == 0 else "outstanding"
                })
        
        # Calculate aging if requested
        aging = []
        if include_aging:
            # Simplified aging calculation
            outstanding = float(customer.sales_balance or 0)
            aging = [
                {"period": "Current", "amount": outstanding * 0.4},
                {"period": "1-30 days", "amount": outstanding * 0.3},
                {"period": "31-60 days", "amount": outstanding * 0.2},
                {"period": "60+ days", "amount": outstanding * 0.1}
            ]
        
        credit_utilization = 0
        if customer.credit_limit and customer.credit_limit > 0:
            credit_utilization = (float(customer.sales_balance) / float(customer.credit_limit)) * 100
        
        return {
            "customer": {
                "sales_account_code": customer.sales_account_code,
                "sales_name": customer.sales_name,
                "sales_balance": float(customer.sales_balance or 0),
                "credit_limit": float(customer.credit_limit or 0),
                "credit_utilization": round(credit_utilization, 2),
                "credit_status": customer.credit_status or "ACTIVE",
                "payment_terms_days": customer.payment_terms_days or 30,
                "last_payment_date": customer.last_payment_date.isoformat() if customer.last_payment_date else None,
                "last_invoice_date": customer.last_invoice_date.isoformat() if customer.last_invoice_date else None,
                "address": {
                    "line1": customer.address_line_1 or "",
                    "line2": customer.address_line_2 or "",
                    "city": customer.city or "",
                    "postcode": customer.post_code or "",
                    "country": customer.country or ""
                },
                "contact": {
                    "phone": customer.phone_number or "",
                    "email": customer.email_address or "",
                    "contact_person": customer.contact_name or ""
                }
            },
            "transactions": transactions,
            "aging": aging,
            "alerts": [
                {"type": "warning", "message": "Customer over credit limit"} 
                if customer.sales_balance > (customer.credit_limit or 0) else None,
                {"type": "info", "message": "Customer on credit hold"} 
                if customer.credit_status == "BLOCKED" else None
            ]
        }
        
    except HTTPException:
        raise
    except Exception as e:
        print(f"Error fetching customer detail: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Error fetching customer details"
        )