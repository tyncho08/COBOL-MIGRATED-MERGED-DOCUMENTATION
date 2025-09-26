"""Purchase Ledger Summary API endpoints - Simplified without auth"""

from fastapi import APIRouter, Depends
from sqlalchemy import func
from sqlalchemy.orm import Session
from typing import Dict, Any, List
from datetime import date

from app.core.database import get_db
from app.models.supplier import PurchaseLedgerRec

router = APIRouter()


@router.get("/summary")
async def get_purchase_summary(
    db: Session = Depends(get_db)
) -> Dict[str, Any]:
    """Get purchase ledger summary statistics"""
    
    # Get supplier statistics
    total_suppliers = db.query(PurchaseLedgerRec).count()
    active_suppliers = db.query(PurchaseLedgerRec).filter(
        PurchaseLedgerRec.purch_account_status == 'A'
    ).count()
    
    # Get financial statistics
    total_payable = db.query(func.sum(PurchaseLedgerRec.purch_balance)).scalar() or 0
    
    # Simplified statistics (in real app, these would come from other tables)
    overdue_payments = float(total_payable) * 0.15  # Estimate 15% overdue
    current_month_purchases = 45670.00  # Would query from purchase invoices
    pending_orders = 12  # Would query from purchase orders
    pending_invoices = 8  # Would query from purchase invoices
    pending_approvals = 5  # Would query from approval workflow
    average_payment_days = 28  # Would calculate from payment history
    
    return {
        "total_suppliers": total_suppliers,
        "active_suppliers": active_suppliers,
        "total_payable": float(total_payable),
        "overdue_payments": overdue_payments,
        "current_month_purchases": current_month_purchases,
        "pending_orders": pending_orders,
        "pending_invoices": pending_invoices,
        "pending_approvals": pending_approvals,
        "average_payment_days": average_payment_days
    }


@router.get("/recent")
async def get_recent_purchases(
    db: Session = Depends(get_db)
) -> Dict[str, Any]:
    """Get recent purchase activities"""
    
    # For now, return simplified mock data
    # In production, this would query actual purchase orders and invoices
    recent_purchases = [
        {
            "id": 1,
            "po_number": "PO-2024-0089",
            "supplier_code": "SUP001",
            "supplier_name": "Office Supplies Ltd",
            "amount": 1450.00,
            "order_date": "2024-01-15",
            "status": "ordered",
            "type": "order"
        },
        {
            "id": 2,
            "invoice_number": "INV-SUP002-456",
            "supplier_code": "SUP002",
            "supplier_name": "Tech Equipment Corp",
            "amount": 2850.75,
            "due_date": "2024-02-15",
            "status": "pending_approval",
            "days_outstanding": 0,
            "type": "invoice"
        }
    ]
    
    # Payment schedule would come from purchase_open_items
    payment_schedule = [
        {
            "supplier": "Office Supplies Ltd",
            "amount": 1450.00,
            "due_date": "2024-02-10",
            "days_until_due": 5,
            "priority": "high"
        },
        {
            "supplier": "Tech Equipment Corp", 
            "amount": 2850.75,
            "due_date": "2024-02-15",
            "days_until_due": 10,
            "priority": "medium"
        }
    ]
    
    return {
        "recent_purchases": recent_purchases,
        "payment_schedule": payment_schedule
    }


@router.get("/suppliers-list")
async def get_suppliers_simple(
    skip: int = 0,
    limit: int = 100,
    db: Session = Depends(get_db)
) -> List[Dict[str, Any]]:
    """Get simplified suppliers list without authentication"""
    
    suppliers = db.query(PurchaseLedgerRec).offset(skip).limit(limit).all()
    
    return [
        {
            "purch_key": supplier.purch_key,
            "purch_name": supplier.purch_name,
            "purch_balance": float(supplier.purch_balance),
            "purch_credit_limit": float(supplier.purch_credit_limit),
            "purch_account_status": supplier.purch_account_status,
            "is_active": supplier.purch_account_status == 'A'
        }
        for supplier in suppliers
    ]