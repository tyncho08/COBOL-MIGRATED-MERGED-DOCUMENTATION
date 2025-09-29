"""
Sales Summary API Endpoints
Provides real-time sales metrics from database
"""
from typing import Dict, Any, List
from fastapi import APIRouter, Depends, HTTPException
from sqlalchemy.orm import Session
from sqlalchemy import func, and_, or_, case, String
from datetime import datetime, date, timedelta
from decimal import Decimal

from app.core.database import get_db
from app.models.customer import SalesLedgerRec, SalesInvoiceRec, SalesInvoiceLineRec
from app.models.sales import SalesOpenItemRec

router = APIRouter()

@router.get("/summary")
async def get_sales_summary(
    db: Session = Depends(get_db)
) -> Dict[str, Any]:
    """Get comprehensive sales summary with real database metrics"""
    
    try:
        # Get current date info
        today = datetime.now()
        current_year = today.year
        current_month = today.month
        current_date_int = int(today.strftime("%Y%m%d"))
        
        # Start of current month
        month_start = int(f"{current_year}{current_month:02d}01")
        
        # Start of current year
        year_start = int(f"{current_year}0101")
        
        # 30 days ago for recent activity
        thirty_days_ago = int((today - timedelta(days=30)).strftime("%Y%m%d"))
        
        # Get active customer count
        active_customers = db.query(func.count(SalesLedgerRec.sales_key)).filter(
            SalesLedgerRec.date_deleted == 0
        ).scalar() or 0
        
        # Get total outstanding from open items
        outstanding_query = db.query(
            func.sum(SalesOpenItemRec.outstanding_amount)
        ).filter(
            SalesOpenItemRec.status.in_(['O', 'A'])
        ).scalar() or Decimal('0.00')
        
        # Get overdue amount (past due date)
        overdue_query = db.query(
            func.sum(SalesOpenItemRec.outstanding_amount)
        ).filter(
            and_(
                SalesOpenItemRec.status.in_(['O', 'A']),
                SalesOpenItemRec.due_date < current_date_int
            )
        ).scalar() or Decimal('0.00')
        
        # Get current month sales from invoices
        current_month_sales = db.query(
            func.sum(SalesInvoiceRec.invoice_amount)
        ).filter(
            SalesInvoiceRec.invoice_date >= month_start
        ).scalar() or Decimal('0.00')
        
        # Count pending invoices (created but not posted)
        pending_invoices = db.query(
            func.count(SalesInvoiceRec.invoice_key)
        ).filter(
            SalesInvoiceRec.invoice_status == 'O'
        ).scalar() or 0
        
        # Count credit notes (negative open items)
        credit_notes_pending = db.query(
            func.count(SalesOpenItemRec.item_id)
        ).filter(
            and_(
                SalesOpenItemRec.transaction_type == 'CN',
                SalesOpenItemRec.status == 'O'
            )
        ).scalar() or 0
        
        # Calculate average payment days (simplified for now)
        # TODO: Implement proper calculation based on allocation date when available
        average_payment_days = 30
        
        # Get recent invoices with customer details
        recent_invoices_query = db.query(
            SalesInvoiceRec.invoice_key,
            SalesInvoiceRec.invoice_date,
            SalesInvoiceRec.invoice_amount,
            SalesInvoiceRec.invoice_status,
            SalesLedgerRec.sales_name,
            SalesOpenItemRec.outstanding_amount,
            SalesOpenItemRec.due_date
        ).join(
            SalesLedgerRec, 
            SalesInvoiceRec.sales_key == SalesLedgerRec.sales_key
        ).outerjoin(
            SalesOpenItemRec,
            and_(
                SalesOpenItemRec.document_number == SalesInvoiceRec.invoice_key,
                SalesOpenItemRec.transaction_type == 'IN'
            )
        ).order_by(
            SalesInvoiceRec.invoice_date.desc()
        ).limit(20).all()
        
        recent_invoices = []
        for invoice in recent_invoices_query:
            status = 'paid'
            if invoice.outstanding_amount and invoice.outstanding_amount > 0:
                status = 'overdue' if invoice.due_date and invoice.due_date < current_date_int else 'outstanding'
            
            recent_invoices.append({
                "invoice_number": invoice.invoice_key,
                "customer_name": invoice.sales_name,
                "date": str(invoice.invoice_date),
                "amount": float(invoice.invoice_amount),
                "outstanding": float(invoice.outstanding_amount) if invoice.outstanding_amount else 0,
                "status": status
            })
        
        # Calculate aging buckets
        aging_buckets = db.query(
            func.sum(case(
                (SalesOpenItemRec.due_date >= current_date_int, SalesOpenItemRec.outstanding_amount),
                else_=0
            )).label('current'),
            func.sum(case(
                (and_(
                    SalesOpenItemRec.due_date < current_date_int,
                    SalesOpenItemRec.due_date >= current_date_int - 30
                ), SalesOpenItemRec.outstanding_amount),
                else_=0
            )).label('days_1_30'),
            func.sum(case(
                (and_(
                    SalesOpenItemRec.due_date < current_date_int - 30,
                    SalesOpenItemRec.due_date >= current_date_int - 60
                ), SalesOpenItemRec.outstanding_amount),
                else_=0
            )).label('days_31_60'),
            func.sum(case(
                (and_(
                    SalesOpenItemRec.due_date < current_date_int - 60,
                    SalesOpenItemRec.due_date >= current_date_int - 90
                ), SalesOpenItemRec.outstanding_amount),
                else_=0
            )).label('days_61_90'),
            func.sum(case(
                (SalesOpenItemRec.due_date < current_date_int - 90, SalesOpenItemRec.outstanding_amount),
                else_=0
            )).label('over_90')
        ).filter(
            and_(
                SalesOpenItemRec.status.in_(['O', 'A']),
                SalesOpenItemRec.transaction_type == 'IN'
            )
        ).first()
        
        aging_data = [
            {"period": "Current", "amount": float(aging_buckets.current or 0)},
            {"period": "1-30 days", "amount": float(aging_buckets.days_1_30 or 0)},
            {"period": "31-60 days", "amount": float(aging_buckets.days_31_60 or 0)},
            {"period": "61-90 days", "amount": float(aging_buckets.days_61_90 or 0)},
            {"period": "Over 90 days", "amount": float(aging_buckets.over_90 or 0)}
        ]
        
        # Calculate collection metrics
        # Get total invoiced in last 90 days
        ninety_days_ago = int((today - timedelta(days=90)).strftime("%Y%m%d"))
        total_invoiced_90days = db.query(
            func.sum(SalesInvoiceRec.invoice_amount)
        ).filter(
            SalesInvoiceRec.invoice_date >= ninety_days_ago
        ).scalar() or Decimal('1')  # Avoid division by zero
        
        # Get total collected (paid invoices) in last 90 days
        total_collected_90days = db.query(
            func.sum(SalesOpenItemRec.original_amount - SalesOpenItemRec.outstanding_amount)
        ).filter(
            and_(
                SalesOpenItemRec.transaction_type == 'IN',
                SalesOpenItemRec.transaction_date >= ninety_days_ago
            )
        ).scalar() or Decimal('0')
        
        collection_rate = float((total_collected_90days / total_invoiced_90days) * 100) if total_invoiced_90days > 0 else 0.0
        
        # Calculate bad debt provision (overdue > 90 days as percentage of total outstanding)
        bad_debt_amount = aging_buckets.over_90 or Decimal('0')
        bad_debt_rate = float((bad_debt_amount / outstanding_query) * 100) if outstanding_query > 0 else 0.0
        
        return {
            "stats": {
                "activeCustomers": active_customers,
                "totalOutstanding": float(outstanding_query),
                "overdueAmount": float(overdue_query),
                "currentMonthSales": float(current_month_sales),
                "invoicesPending": pending_invoices,
                "creditNotesPending": credit_notes_pending,
                "averagePaymentDays": average_payment_days
            },
            "recentInvoices": recent_invoices,
            "aging": aging_data,
            "metrics": {
                "collectionRate": round(collection_rate, 1),
                "badDebtProvision": round(bad_debt_rate, 1)
            }
        }
        
    except Exception as e:
        print(f"Error in sales summary: {str(e)}")
        raise HTTPException(status_code=500, detail=str(e))

@router.get("/invoices")
async def get_sales_invoices(
    limit: int = 50,
    offset: int = 0,
    status: str = None,
    db: Session = Depends(get_db)
) -> Dict[str, Any]:
    """Get list of sales invoices with filtering"""
    
    try:
        # Base query
        query = db.query(
            SalesInvoiceRec,
            SalesLedgerRec.sales_name,
            SalesOpenItemRec.outstanding_amount,
            SalesOpenItemRec.due_date
        ).join(
            SalesLedgerRec,
            SalesInvoiceRec.sales_key == SalesLedgerRec.sales_key
        ).outerjoin(
            SalesOpenItemRec,
            and_(
                SalesOpenItemRec.document_number == SalesInvoiceRec.invoice_key,
                SalesOpenItemRec.transaction_type == 'IN'
            )
        )
        
        # Apply status filter if provided
        if status:
            if status == 'outstanding':
                query = query.filter(SalesOpenItemRec.outstanding_amount > 0)
            elif status == 'paid':
                query = query.filter(
                    or_(
                        SalesOpenItemRec.outstanding_amount == 0,
                        SalesOpenItemRec.outstanding_amount == None
                    )
                )
        
        # Get total count
        total_count = query.count()
        
        # Apply pagination and get results
        invoices = query.order_by(
            SalesInvoiceRec.invoice_date.desc()
        ).limit(limit).offset(offset).all()
        
        # Format results
        invoice_list = []
        current_date_int = int(datetime.now().strftime("%Y%m%d"))
        
        for invoice, customer_name, outstanding, due_date in invoices:
            status = 'paid'
            if outstanding and outstanding > 0:
                status = 'overdue' if due_date and due_date < current_date_int else 'outstanding'
            
            invoice_list.append({
                "invoiceKey": invoice.invoice_key,
                "customerCode": invoice.sales_key,
                "customerName": customer_name,
                "invoiceDate": str(invoice.invoice_date),
                "invoiceAmount": float(invoice.invoice_amount),
                "outstandingAmount": float(outstanding) if outstanding else 0,
                "dueDate": str(due_date) if due_date else None,
                "status": status
            })
        
        return {
            "invoices": invoice_list,
            "pagination": {
                "total": total_count,
                "limit": limit,
                "offset": offset
            }
        }
        
    except Exception as e:
        print(f"Error fetching invoices: {str(e)}")
        raise HTTPException(status_code=500, detail=str(e))

@router.get("/aging")
async def get_aging_analysis(
    customer_code: str = None,
    db: Session = Depends(get_db)
) -> Dict[str, Any]:
    """Get detailed aging analysis by customer or overall"""
    
    try:
        current_date_int = int(datetime.now().strftime("%Y%m%d"))
        
        # Base query for aging
        base_query = db.query(
            SalesOpenItemRec.sales_key,
            SalesLedgerRec.sales_name,
            func.sum(case(
                (SalesOpenItemRec.due_date >= current_date_int, SalesOpenItemRec.outstanding_amount),
                else_=0
            )).label('current'),
            func.sum(case(
                (and_(
                    SalesOpenItemRec.due_date < current_date_int,
                    SalesOpenItemRec.due_date >= current_date_int - 30
                ), SalesOpenItemRec.outstanding_amount),
                else_=0
            )).label('days_1_30'),
            func.sum(case(
                (and_(
                    SalesOpenItemRec.due_date < current_date_int - 30,
                    SalesOpenItemRec.due_date >= current_date_int - 60
                ), SalesOpenItemRec.outstanding_amount),
                else_=0
            )).label('days_31_60'),
            func.sum(case(
                (and_(
                    SalesOpenItemRec.due_date < current_date_int - 60,
                    SalesOpenItemRec.due_date >= current_date_int - 90
                ), SalesOpenItemRec.outstanding_amount),
                else_=0
            )).label('days_61_90'),
            func.sum(case(
                (SalesOpenItemRec.due_date < current_date_int - 90, SalesOpenItemRec.outstanding_amount),
                else_=0
            )).label('over_90')
        ).join(
            SalesLedgerRec,
            SalesOpenItemRec.sales_key == SalesLedgerRec.sales_key
        ).filter(
            and_(
                SalesOpenItemRec.status.in_(['O', 'A']),
                SalesOpenItemRec.transaction_type == 'IN'
            )
        )
        
        # Apply customer filter if provided
        if customer_code:
            base_query = base_query.filter(SalesOpenItemRec.sales_key == customer_code)
        
        # Group by customer
        aging_results = base_query.group_by(
            SalesOpenItemRec.sales_key,
            SalesLedgerRec.sales_name
        ).all()
        
        # Format results
        aging_details = []
        totals = {
            'current': Decimal('0'),
            'days_1_30': Decimal('0'),
            'days_31_60': Decimal('0'),
            'days_61_90': Decimal('0'),
            'over_90': Decimal('0'),
            'total': Decimal('0')
        }
        
        for row in aging_results:
            customer_total = row.current + row.days_1_30 + row.days_31_60 + row.days_61_90 + row.over_90
            
            aging_details.append({
                "customerCode": row.sales_key,
                "customerName": row.sales_name,
                "current": float(row.current),
                "days1to30": float(row.days_1_30),
                "days31to60": float(row.days_31_60),
                "days61to90": float(row.days_61_90),
                "over90Days": float(row.over_90),
                "total": float(customer_total)
            })
            
            # Update totals
            totals['current'] += row.current
            totals['days_1_30'] += row.days_1_30
            totals['days_31_60'] += row.days_31_60
            totals['days_61_90'] += row.days_61_90
            totals['over_90'] += row.over_90
            totals['total'] += customer_total
        
        # Convert totals to float
        totals_float = {k: float(v) for k, v in totals.items()}
        
        return {
            "agingDetails": aging_details,
            "summary": {
                "current": totals_float['current'],
                "days1to30": totals_float['days_1_30'],
                "days31to60": totals_float['days_31_60'],
                "days61to90": totals_float['days_61_90'],
                "over90Days": totals_float['over_90'],
                "total": totals_float['total']
            },
            "generatedAt": datetime.now().isoformat()
        }
        
    except Exception as e:
        print(f"Error in aging analysis: {str(e)}")
        raise HTTPException(status_code=500, detail=str(e))