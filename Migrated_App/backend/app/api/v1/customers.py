"""
ACAS Customer API Routes
Customer management endpoints
"""
from fastapi import APIRouter, Depends, HTTPException, status
from sqlalchemy.orm import Session
from typing import List

from app.core.database import get_db
from app.models.customer import SalesLedgerRec
from app.schemas.customer import CustomerCreate, CustomerUpdate, CustomerResponse, CustomerSummary
from app.services.business_logic import CreditControlService

router = APIRouter()

@router.get("", response_model=List[CustomerSummary])
async def list_customers(
    skip: int = 0,
    limit: int = 100,
    db: Session = Depends(get_db)
):
    """List all customers with pagination"""
    customers = db.query(SalesLedgerRec).offset(skip).limit(limit).all()
    return customers

@router.get("/{customer_code}", response_model=CustomerResponse)
async def get_customer(customer_code: str, db: Session = Depends(get_db)):
    """Get customer by code"""
    customer = db.query(SalesLedgerRec).filter(SalesLedgerRec.sales_key == customer_code).first()
    if not customer:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Customer {customer_code} not found"
        )
    return customer

@router.post("/", response_model=CustomerResponse, status_code=status.HTTP_201_CREATED)
async def create_customer(customer: CustomerCreate, db: Session = Depends(get_db)):
    """Create new customer"""
    # Check if customer already exists
    existing = db.query(SalesLedgerRec).filter(SalesLedgerRec.sales_key == customer.sales_key).first()
    if existing:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Customer {customer.sales_key} already exists"
        )
    
    # Create new customer
    db_customer = SalesLedgerRec(**customer.dict())
    db.add(db_customer)
    db.commit()
    db.refresh(db_customer)
    
    return db_customer

@router.put("/{customer_code}", response_model=CustomerResponse)
async def update_customer(
    customer_code: str,
    customer_update: CustomerUpdate,
    db: Session = Depends(get_db)
):
    """Update customer"""
    customer = db.query(SalesLedgerRec).filter(SalesLedgerRec.sales_key == customer_code).first()
    if not customer:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Customer {customer_code} not found"
        )
    
    # Update fields
    update_data = customer_update.dict(exclude_unset=True)
    for field, value in update_data.items():
        setattr(customer, field, value)
    
    db.commit()
    db.refresh(customer)
    
    return customer

@router.delete("/{customer_code}")
async def delete_customer(customer_code: str, db: Session = Depends(get_db)):
    """Delete customer"""
    customer = db.query(SalesLedgerRec).filter(SalesLedgerRec.sales_key == customer_code).first()
    if not customer:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Customer {customer_code} not found"
        )
    
    db.delete(customer)
    db.commit()
    
    return {"message": f"Customer {customer_code} deleted successfully"}

@router.post("/{customer_code}/credit-check")
async def check_customer_credit(
    customer_code: str,
    order_amount: float,
    db: Session = Depends(get_db)
):
    """Check customer credit limit for new order"""
    customer = db.query(SalesLedgerRec).filter(SalesLedgerRec.sales_key == customer_code).first()
    if not customer:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Customer {customer_code} not found"
        )
    
    # Perform credit check using business logic service
    credit_check = CreditControlService.check_credit_limit(
        customer_code=customer.sales_key,
        credit_limit=customer.sales_credit_limit,
        current_balance=customer.sales_balance,
        open_orders_total=0,  # Would calculate from open orders
        new_order_amount=order_amount,
        credit_rating=customer.sales_credit_rating,
        account_status=customer.sales_account_status
    )
    
    return credit_check