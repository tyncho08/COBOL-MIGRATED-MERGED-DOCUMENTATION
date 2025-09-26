from typing import Dict, Any, List, Optional
from fastapi import APIRouter, Depends, HTTPException, Body
from sqlalchemy.orm import Session
from sqlalchemy import func, and_, or_, desc
from datetime import datetime, date, timedelta
from app.db.database import get_db
from app.models.payment import Payment
from app.models.receipt import Receipt
from app.models.sales_ledger import SalesLedgerRec
from app.models.purchase_ledger import PurchaseLedgerRec

router = APIRouter()

@router.get("/summary")
async def get_payments_summary(
    db: Session = Depends(get_db)
) -> Dict[str, Any]:
    """Get payments and receipts summary"""
    
    try:
        # Calculate date ranges
        today = date.today()
        current_month_start = date(today.year, today.month, 1)
        last_month_start = (current_month_start - timedelta(days=1)).replace(day=1)
        
        # Current month receipts
        current_month_receipts = db.query(func.sum(Receipt.receipt_amount)).filter(
            Receipt.receipt_date >= current_month_start
        ).scalar() or 0
        
        # Last month receipts
        last_month_receipts = db.query(func.sum(Receipt.receipt_amount)).filter(
            and_(
                Receipt.receipt_date >= last_month_start,
                Receipt.receipt_date < current_month_start
            )
        ).scalar() or 0
        
        # Current month payments
        current_month_payments = db.query(func.sum(Payment.payment_amount)).filter(
            Payment.payment_date >= current_month_start
        ).scalar() or 0
        
        # Last month payments  
        last_month_payments = db.query(func.sum(Payment.payment_amount)).filter(
            and_(
                Payment.payment_date >= last_month_start,
                Payment.payment_date < current_month_start
            )
        ).scalar() or 0
        
        # Pending receipts (outstanding customer balances)
        pending_receipts = db.query(func.sum(SalesLedgerRec.sales_balance)).filter(
            SalesLedgerRec.sales_balance > 0
        ).scalar() or 0
        
        # Pending payments (outstanding supplier balances)
        pending_payments = db.query(func.sum(PurchaseLedgerRec.purch_balance)).filter(
            PurchaseLedgerRec.purch_balance > 0
        ).scalar() or 0
        
        # Calculate changes
        receipts_change = 0
        if last_month_receipts > 0:
            receipts_change = ((current_month_receipts - last_month_receipts) / last_month_receipts) * 100
            
        payments_change = 0
        if last_month_payments > 0:
            payments_change = ((current_month_payments - last_month_payments) / last_month_payments) * 100
        
        # Bank balances (simplified - in real app would come from bank integration)
        bank_balances = {
            "main": 125430.50,
            "savings": 250000.00,
            "payroll": 45670.25
        }
        total_bank_balance = sum(bank_balances.values())
        
        return {
            "summary": {
                "totalReceipts": float(current_month_receipts),
                "receiptsChange": float(receipts_change),
                "totalPayments": float(current_month_payments),
                "paymentsChange": float(payments_change),
                "pendingReceipts": float(pending_receipts),
                "pendingPayments": float(pending_payments),
                "bankBalance": float(total_bank_balance),
                "netCashFlow": float(current_month_receipts - current_month_payments)
            },
            "bankAccounts": [
                {
                    "id": "main",
                    "name": "Main Operating Account",
                    "accountNumber": "****1234",
                    "balance": bank_balances["main"],
                    "currency": "USD",
                    "bank": "First National Bank"
                },
                {
                    "id": "savings", 
                    "name": "Business Savings",
                    "accountNumber": "****5678",
                    "balance": bank_balances["savings"],
                    "currency": "USD",
                    "bank": "First National Bank"
                },
                {
                    "id": "payroll",
                    "name": "Payroll Account",
                    "accountNumber": "****9012",
                    "balance": bank_balances["payroll"],
                    "currency": "USD",
                    "bank": "Business Bank Corp"
                }
            ]
        }
        
    except Exception as e:
        print(f"Error fetching payments summary: {str(e)}")
        # Return default values if error occurs
        return {
            "summary": {
                "totalReceipts": 0,
                "receiptsChange": 0,
                "totalPayments": 0,
                "paymentsChange": 0,
                "pendingReceipts": 0,
                "pendingPayments": 0,
                "bankBalance": 0,
                "netCashFlow": 0
            },
            "bankAccounts": []
        }

@router.get("/transactions")
async def get_recent_transactions(
    db: Session = Depends(get_db),
    limit: int = 20
) -> Dict[str, Any]:
    """Get recent payment and receipt transactions"""
    
    try:
        # Get recent receipts
        recent_receipts = db.query(Receipt).order_by(
            desc(Receipt.receipt_date)
        ).limit(limit).all()
        
        # Get recent payments
        recent_payments = db.query(Payment).order_by(
            desc(Payment.payment_date)
        ).limit(limit).all()
        
        # Combine and format transactions
        transactions = []
        
        for receipt in recent_receipts:
            # Get customer name
            customer = db.query(SalesLedgerRec).filter(
                SalesLedgerRec.sales_account_code == receipt.customer_code
            ).first()
            
            transactions.append({
                "id": f"REC-{receipt.receipt_id}",
                "type": "receipt",
                "reference": receipt.receipt_number or f"REC{receipt.receipt_id}",
                "description": f"Payment from {customer.sales_name if customer else receipt.customer_code}",
                "amount": float(receipt.receipt_amount),
                "date": receipt.receipt_date.isoformat(),
                "account": "Main Operating Account",
                "status": "completed",
                "category": "customer_payment"
            })
        
        for payment in recent_payments:
            # Get supplier name
            supplier = db.query(PurchaseLedgerRec).filter(
                PurchaseLedgerRec.purch_account_code == payment.supplier_code
            ).first()
            
            transactions.append({
                "id": f"PAY-{payment.payment_id}",
                "type": "payment",
                "reference": payment.payment_number or f"PAY{payment.payment_id}",
                "description": f"Payment to {supplier.purch_name if supplier else payment.supplier_code}",
                "amount": float(payment.payment_amount),
                "date": payment.payment_date.isoformat(),
                "account": "Main Operating Account",
                "status": "completed",
                "category": "supplier_payment"
            })
        
        # Sort by date descending
        transactions.sort(key=lambda x: x["date"], reverse=True)
        
        return {
            "transactions": transactions[:limit]
        }
        
    except Exception as e:
        print(f"Error fetching recent transactions: {str(e)}")
        return {
            "transactions": []
        }

@router.post("/receipt")
async def record_receipt(
    customer_code: str = Body(...),
    amount: float = Body(...),
    reference: str = Body(None),
    payment_method: str = Body("bank_transfer"),
    notes: str = Body(None),
    db: Session = Depends(get_db)
) -> Dict[str, Any]:
    """Record a new customer receipt"""
    
    try:
        # Create new receipt
        receipt = Receipt(
            customer_code=customer_code,
            receipt_amount=amount,
            receipt_number=reference,
            receipt_date=date.today(),
            payment_method=payment_method,
            notes=notes
        )
        db.add(receipt)
        
        # Update customer balance
        customer = db.query(SalesLedgerRec).filter(
            SalesLedgerRec.sales_account_code == customer_code
        ).first()
        
        if customer:
            customer.sales_balance = max(0, customer.sales_balance - amount)
        
        db.commit()
        
        return {
            "success": True,
            "message": "Receipt recorded successfully",
            "receipt_id": receipt.receipt_id,
            "receipt_number": receipt.receipt_number
        }
        
    except Exception as e:
        db.rollback()
        return {
            "success": False,
            "message": f"Error recording receipt: {str(e)}"
        }

@router.post("/payment") 
async def record_payment(
    supplier_code: str = Body(...),
    amount: float = Body(...),
    reference: str = Body(None),
    payment_method: str = Body("bank_transfer"),
    notes: str = Body(None),
    db: Session = Depends(get_db)
) -> Dict[str, Any]:
    """Record a new supplier payment"""
    
    try:
        # Create new payment
        payment = Payment(
            supplier_code=supplier_code,
            payment_amount=amount,
            payment_number=reference,
            payment_date=date.today(),
            payment_method=payment_method,
            notes=notes
        )
        db.add(payment)
        
        # Update supplier balance
        supplier = db.query(PurchaseLedgerRec).filter(
            PurchaseLedgerRec.purch_account_code == supplier_code
        ).first()
        
        if supplier:
            supplier.purch_balance = max(0, supplier.purch_balance - amount)
        
        db.commit()
        
        return {
            "success": True,
            "message": "Payment recorded successfully",
            "payment_id": payment.payment_id,
            "payment_number": payment.payment_number
        }
        
    except Exception as e:
        db.rollback()
        return {
            "success": False,
            "message": f"Error recording payment: {str(e)}"
        }