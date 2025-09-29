"""
COBOL-Compatible Outstanding Invoices API
Uses actual COBOL database tables (sainvoice_rec)
"""
from fastapi import APIRouter, Depends, HTTPException
from sqlalchemy.orm import Session
from sqlalchemy import text
from typing import Dict, Any
from datetime import datetime, date, timedelta

from app.core.database import get_db

router = APIRouter()

@router.get("/outstanding")
async def get_cobol_outstanding_invoices(db: Session = Depends(get_db)) -> Dict[str, Any]:
    """
    Get outstanding invoices from COBOL database structure
    Uses sainvoice_rec table for real data
    """
    try:
        # Get outstanding invoices (where balance > 0)
        invoices_query = db.execute(text("""
            SELECT 
                i.invoice_key,
                i.invoice_customer,
                i.invoice_date,
                i.invoice_due_date,
                i.invoice_our_ref,
                i.invoice_total_amount,
                i.invoice_paid_amount,
                i.invoice_balance,
                i.invoice_status,
                i.invoice_period,
                c.sales_name
            FROM acas.sainvoice_rec i
            LEFT JOIN acas.saledger_rec c ON i.invoice_customer = c.sales_key
            WHERE i.invoice_balance > 0
            ORDER BY i.invoice_due_date
        """)).fetchall()
        
        # Process invoices
        invoices = []
        total_outstanding = 0.0
        current_amount = 0.0
        overdue_amount = 0.0
        critical_amount = 0.0
        overdue_count = 0
        oldest_days = 0
        
        today = date.today()
        today_int = int(today.strftime("%Y%m%d"))
        
        for inv in invoices_query:
            # Convert COBOL date (YYYYMMDD) to date object
            # Handle case where due_date is 0 or invalid
            if inv.invoice_due_date and inv.invoice_due_date > 0:
                due_date_str = str(inv.invoice_due_date)
                if len(due_date_str) == 8:
                    try:
                        due_date = date(int(due_date_str[:4]), int(due_date_str[4:6]), int(due_date_str[6:8]))
                    except ValueError:
                        # If due date is invalid, calculate from invoice date + 30 days
                        invoice_date_str = str(inv.invoice_date)
                        invoice_date = date(int(invoice_date_str[:4]), int(invoice_date_str[4:6]), int(invoice_date_str[6:8]))
                        due_date = invoice_date + timedelta(days=30)
                else:
                    # Invalid date format, use invoice date + 30 days
                    invoice_date_str = str(inv.invoice_date)
                    invoice_date = date(int(invoice_date_str[:4]), int(invoice_date_str[4:6]), int(invoice_date_str[6:8]))
                    due_date = invoice_date + timedelta(days=30)
            else:
                # No due date, calculate from invoice date + 30 days
                invoice_date_str = str(inv.invoice_date)
                invoice_date = date(int(invoice_date_str[:4]), int(invoice_date_str[4:6]), int(invoice_date_str[6:8]))
                due_date = invoice_date + timedelta(days=30)
            
            # Convert invoice date
            invoice_date_str = str(inv.invoice_date)
            invoice_date = date(int(invoice_date_str[:4]), int(invoice_date_str[4:6]), int(invoice_date_str[6:8]))
            
            # Calculate days overdue
            days_overdue = (today - due_date).days if due_date < today else 0
            oldest_days = max(oldest_days, days_overdue)
            
            # Determine status
            if days_overdue > 60:
                status = 'critical'
                critical_amount += float(inv.invoice_balance)
            elif days_overdue > 0:
                status = 'overdue'
                overdue_amount += float(inv.invoice_balance)
                overdue_count += 1
            else:
                status = 'current'
                current_amount += float(inv.invoice_balance)
            
            total_outstanding += float(inv.invoice_balance)
            
            # Generate invoice number if not present
            invoice_number = f"INV-2024-{str(inv.invoice_key).zfill(4)}"
            
            invoices.append({
                "id": inv.invoice_key,
                "invoice_number": invoice_number,
                "customer_code": inv.invoice_customer,
                "customer_name": inv.sales_name or inv.invoice_customer,
                "invoice_date": invoice_date.isoformat(),
                "due_date": due_date.isoformat(),
                "amount": float(inv.invoice_total_amount),
                "balance": float(inv.invoice_balance),
                "days_overdue": days_overdue,
                "status": status
            })
        
        # Calculate summary statistics
        summary = {
            "total_outstanding": total_outstanding,
            "current_amount": current_amount,
            "overdue_amount": overdue_amount,
            "critical_amount": critical_amount,
            "total_invoices": len(invoices),
            "overdue_invoices": overdue_count,
            "average_days_overdue": int(sum(inv["days_overdue"] for inv in invoices) / len(invoices)) if invoices else 0,
            "oldest_invoice_days": oldest_days
        }
        
        return {
            "summary": summary,
            "invoices": invoices,
            "data_source": "COBOL migrated database (sainvoice_rec)",
            "timestamp": datetime.now().isoformat()
        }
        
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Database error: {str(e)}")