"""
COBOL-Compatible Sales Invoices API
Uses actual COBOL database tables (sainvoice_rec)
"""
from fastapi import APIRouter, Depends, HTTPException, Query
from sqlalchemy.orm import Session
from sqlalchemy import text
from typing import Dict, Any, Optional
from datetime import datetime, date

from app.core.database import get_db

router = APIRouter()

@router.get("/invoices")
async def get_cobol_invoices(
    status: Optional[str] = Query(None, description="Filter by status: O=Open, P=Paid, D=Draft, C=Cancelled"),
    customer_code: Optional[str] = Query(None, description="Filter by customer code"),
    skip: int = Query(0, description="Records to skip"),
    limit: int = Query(100, description="Max records to return"),
    db: Session = Depends(get_db)
) -> Dict[str, Any]:
    """
    Get all sales invoices from COBOL database structure
    Uses sainvoice_rec table for real data
    """
    try:
        # Build query based on filters
        query = """
            SELECT 
                i.invoice_key,
                i.invoice_customer,
                i.invoice_date,
                i.invoice_total_amount,
                i.invoice_paid_amount,
                i.invoice_balance,
                i.invoice_status,
                i.invoice_goods_amount,
                i.invoice_vat_amount,
                c.sales_name
            FROM acas.sainvoice_rec i
            LEFT JOIN acas.saledger_rec c ON i.invoice_customer = c.sales_key
            WHERE 1=1
        """
        
        params = {}
        
        if status:
            query += " AND i.invoice_status = :status"
            params['status'] = status
            
        if customer_code:
            query += " AND i.invoice_customer = :customer_code"
            params['customer_code'] = customer_code
            
        query += " ORDER BY i.invoice_date DESC, i.invoice_key DESC"
        query += f" LIMIT {limit} OFFSET {skip}"
        
        # Execute query
        result = db.execute(text(query), params).fetchall()
        
        # Process invoices
        invoices = []
        total_amount = 0.0
        total_outstanding = 0.0
        
        for inv in result:
            # Convert COBOL dates
            invoice_date_str = str(inv.invoice_date)
            invoice_date = date(int(invoice_date_str[:4]), int(invoice_date_str[4:6]), int(invoice_date_str[6:8]))
            
            due_date_str = str(inv.invoice_due_date)
            due_date = date(int(due_date_str[:4]), int(due_date_str[4:6]), int(due_date_str[6:8]))
            
            # Generate invoice number
            invoice_number = f"INV-{invoice_date.year}-{str(inv.invoice_key).zfill(4)}"
            
            # Determine display status
            if inv.invoice_balance == 0 and inv.invoice_paid_amount > 0:
                display_status = 'P'  # Paid
            elif inv.invoice_balance > 0:
                display_status = 'O'  # Open
            else:
                display_status = inv.invoice_status or 'D'  # Draft or actual status
            
            total_amount += float(inv.invoice_total_amount or 0)
            total_outstanding += float(inv.invoice_balance or 0)
            
            invoices.append({
                "invoice_key": inv.invoice_key,
                "invoice_number": invoice_number,
                "invoice_customer": inv.invoice_customer,
                "customer_name": inv.sales_name or inv.invoice_customer,
                "invoice_date": invoice_date.isoformat(),
                "invoice_due_date": due_date.isoformat(),
                "invoice_reference": inv.invoice_our_ref or "",
                "invoice_total_amount": float(inv.invoice_total_amount or 0),
                "invoice_paid_amount": float(inv.invoice_paid_amount or 0),
                "invoice_balance": float(inv.invoice_balance or 0),
                "invoice_status": display_status,
                "invoice_lines": 0,  # Would need separate line items table
                "invoice_goods_amount": float(inv.invoice_goods_amount or 0),
                "invoice_vat_amount": float(inv.invoice_vat_amount or 0)
            })
        
        # Get total count
        count_query = """
            SELECT COUNT(*) FROM acas.sainvoice_rec i WHERE 1=1
        """
        if status:
            count_query += " AND i.invoice_status = :status"
        if customer_code:
            count_query += " AND i.invoice_customer = :customer_code"
            
        total_count = db.execute(text(count_query), params).scalar()
        
        return {
            "invoices": invoices,
            "pagination": {
                "total": total_count,
                "skip": skip,
                "limit": limit,
                "page": (skip // limit) + 1,
                "pages": (total_count + limit - 1) // limit
            },
            "summary": {
                "total_invoices": len(invoices),
                "total_amount": total_amount,
                "total_outstanding": total_outstanding
            },
            "data_source": "COBOL migrated database (sainvoice_rec)",
            "timestamp": datetime.now().isoformat()
        }
        
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Database error: {str(e)}")