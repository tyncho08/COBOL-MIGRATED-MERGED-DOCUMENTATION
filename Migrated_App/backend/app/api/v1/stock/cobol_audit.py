from fastapi import APIRouter, Depends, HTTPException, Query
from sqlalchemy import text
from sqlalchemy.ext.asyncio import AsyncSession
from typing import List, Optional
from datetime import datetime, timedelta
import logging

from app.core.database import get_db

router = APIRouter()
logger = logging.getLogger(__name__)

@router.get("/cobol/audit")
async def get_stock_audit(
    db: AsyncSession = Depends(get_db),
    item_code: Optional[str] = Query(None, description="Filter by item code"),
    days: Optional[int] = Query(30, description="Number of days to look back")
):
    """
    Get stock audit trail from COBOL stockaudit_rec table.
    Returns movement history for stock items.
    """
    try:
        # Build query for stock audit records
        base_query = """
            SELECT 
                a.audit_id,
                a.stock_key,
                a.audit_date,
                a.audit_type,
                a.old_quantity,
                a.new_quantity,
                a.variance,
                a.reference,
                a.reason_code,
                a.notes,
                s.stock_desc as item_description
            FROM acas.stockaudit_rec a
            LEFT JOIN acas.stock_rec s ON a.stock_key = s.stock_key
            WHERE 1=1
        """
        
        params = {}
        
        if item_code:
            base_query += " AND a.stock_key = :item_code"
            params['item_code'] = item_code
            
        if days:
            # Calculate date range (mock since we don't have real dates)
            base_query += " AND a.audit_date >= :start_date"
            params['start_date'] = (datetime.now() - timedelta(days=days)).strftime('%Y%m%d')
        
        base_query += " ORDER BY a.audit_date DESC, a.audit_id DESC LIMIT 100"
        
        result = await db.execute(text(base_query), params)
        rows = result.fetchall()
        
        movements = []
        for row in rows:
            movements.append({
                "audit_id": row.audit_id,
                "item_code": row.stock_key,
                "item_description": row.item_description or 'Unknown Item',
                "date": row.audit_date,
                "transaction_type": row.audit_type or 'UNKNOWN',
                "old_quantity": float(row.old_quantity) if row.old_quantity else 0,
                "new_quantity": float(row.new_quantity) if row.new_quantity else 0,
                "variance": float(row.variance) if row.variance else 0,
                "reference": row.reference or '',
                "reason_code": row.reason_code or '',
                "notes": row.notes or ''
            })
        
        
        # Calculate summary statistics
        total_movements = len(movements)
        total_receipts = sum(1 for m in movements if m['transaction_type'] == 'RECEIPT')
        total_issues = sum(1 for m in movements if m['transaction_type'] == 'ISSUE')
        
        return {
            "movements": movements,
            "summary": {
                "total_movements": total_movements,
                "total_receipts": total_receipts,
                "total_issues": total_issues,
                "period_days": days
            }
        }
        
    except Exception as e:
        logger.error(f"Error fetching stock audit: {str(e)}")
        raise HTTPException(status_code=500, detail=str(e))