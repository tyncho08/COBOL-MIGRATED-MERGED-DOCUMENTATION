"""Sales Ledger Statement Generation API endpoints"""

from fastapi import APIRouter, Depends, HTTPException, status, Query, Body
from sqlalchemy.orm import Session
from typing import List, Optional, Dict, Any
from datetime import date, datetime
from decimal import Decimal
import io
import base64

from app.api import deps
from app.services.sl import customer_master as statement_service
from app.schemas.sl import (
    CustomerStatement, StatementLine, StatementFrequency,
    BatchStatementGeneration, CustomerAgedBalance,
    AgedAnalysisSummary
)

router = APIRouter()


@router.get("/{customer_code}", response_model=CustomerStatement)
async def generate_statement(
    customer_code: str,
    statement_date: Optional[date] = Query(None, description="Statement date (defaults to today)"),
    from_date: Optional[date] = Query(None, description="Start date for transactions"),
    include_paid_invoices: bool = Query(False, description="Include fully paid invoices"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Generate customer statement.
    
    Shows all transactions and running balance.
    """
    # Validate customer
    customer = statement_service.get_customer(db, customer_code=customer_code)
    if not customer:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Customer {customer_code} not found"
        )
    
    statement = statement_service.generate_customer_statement(
        db,
        customer_code=customer_code,
        statement_date=statement_date or date.today(),
        from_date=from_date,
        include_paid_invoices=include_paid_invoices
    )
    
    return statement


@router.post("/batch", response_model=Dict[str, Any])
async def generate_batch_statements(
    batch_request: BatchStatementGeneration,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Generate statements for multiple customers.
    
    Can filter by customer group or generate for all active customers.
    """
    results = []
    success_count = 0
    error_count = 0
    
    # Get list of customers
    if batch_request.customer_codes:
        customer_list = batch_request.customer_codes
    else:
        # Get all active customers with balances
        customers = statement_service.get_customers_with_balances(
            db,
            include_zero_balance=batch_request.include_zero_balance
        )
        customer_list = [c.customer_code for c in customers]
    
    for customer_code in customer_list:
        try:
            statement = statement_service.generate_customer_statement(
                db,
                customer_code=customer_code,
                statement_date=batch_request.statement_date,
                include_paid_invoices=False
            )
            
            # Check if statement should be included
            if not batch_request.include_zero_balance and statement.closing_balance == 0:
                continue
            
            success_count += 1
            results.append({
                "customer_code": customer_code,
                "success": True,
                "closing_balance": statement.closing_balance,
                "email_sent": False
            })
            
            # Email if requested
            if batch_request.email_statements:
                try:
                    statement_service.email_statement(
                        db,
                        customer_code=customer_code,
                        statement=statement
                    )
                    results[-1]["email_sent"] = True
                except Exception as e:
                    results[-1]["email_error"] = str(e)
                    
        except Exception as e:
            error_count += 1
            results.append({
                "customer_code": customer_code,
                "success": False,
                "error": str(e)
            })
    
    return {
        "total_processed": len(customer_list),
        "success_count": success_count,
        "error_count": error_count,
        "statements_generated": success_count,
        "emails_sent": sum(1 for r in results if r.get("email_sent")),
        "results": results
    }


@router.get("/{customer_code}/activity")
async def get_customer_activity(
    customer_code: str,
    start_date: Optional[date] = None,
    end_date: Optional[date] = None,
    transaction_type: Optional[str] = Query(None, description="Filter by type: invoice, payment, credit_note"),
    skip: int = 0,
    limit: int = 100,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Get detailed customer activity.
    
    Returns individual transactions with running balance.
    """
    activity = statement_service.get_customer_activity(
        db,
        customer_code=customer_code,
        start_date=start_date,
        end_date=end_date,
        transaction_type=transaction_type,
        skip=skip,
        limit=limit
    )
    
    if not activity:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Customer {customer_code} not found"
        )
    
    return activity


@router.post("/{customer_code}/email")
async def email_statement(
    customer_code: str,
    email_override: Optional[str] = Body(None, description="Override customer email address"),
    statement_date: Optional[date] = Body(None),
    include_attachments: bool = Body(True, description="Include PDF attachment"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Email statement to customer.
    
    Uses customer's email on file or override address.
    """
    # Generate statement first
    statement = statement_service.generate_customer_statement(
        db,
        customer_code=customer_code,
        statement_date=statement_date or date.today()
    )
    
    if not statement:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Customer {customer_code} not found"
        )
    
    # Send email
    result = statement_service.email_statement(
        db,
        customer_code=customer_code,
        statement=statement,
        email_override=email_override,
        include_attachments=include_attachments
    )
    
    return {
        "customer_code": customer_code,
        "email_sent_to": result["sent_to"],
        "statement_date": statement.statement_date,
        "closing_balance": statement.closing_balance,
        "attachments": result.get("attachments", [])
    }


@router.get("/{customer_code}/print")
async def print_statement(
    customer_code: str,
    statement_date: Optional[date] = Query(None),
    format: str = Query("PDF", regex="^(PDF|HTML|CSV)$"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Generate printable statement.
    
    Returns statement in PDF, HTML or CSV format.
    """
    statement = statement_service.generate_customer_statement(
        db,
        customer_code=customer_code,
        statement_date=statement_date or date.today()
    )
    
    if not statement:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Customer {customer_code} not found"
        )
    
    # Generate document
    document = statement_service.format_statement(
        statement=statement,
        format=format
    )
    
    if format == "PDF":
        return {
            "document_type": "PDF",
            "document_data": base64.b64encode(document).decode(),
            "filename": f"statement_{customer_code}_{statement.statement_date}.pdf"
        }
    else:
        return {
            "document_type": format,
            "document_data": document,
            "filename": f"statement_{customer_code}_{statement.statement_date}.{format.lower()}"
        }


@router.get("/aging-analysis", response_model=AgedAnalysisSummary)
async def get_aged_analysis(
    as_of_date: Optional[date] = Query(None, description="Analysis date (defaults to today)"),
    customer_code: Optional[str] = Query(None, description="Filter by customer"),
    salesperson_code: Optional[str] = Query(None, description="Filter by salesperson"),
    territory_code: Optional[str] = Query(None, description="Filter by territory"),
    credit_status: Optional[str] = Query(None, description="Filter by credit status"),
    min_balance: Decimal = Query(0, description="Minimum balance to include"),
    include_details: bool = Query(True, description="Include invoice details"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Get aged receivables analysis.
    
    Shows outstanding amounts by aging buckets.
    """
    analysis = statement_service.generate_aged_analysis(
        db,
        as_of_date=as_of_date or date.today(),
        customer_code=customer_code,
        salesperson_code=salesperson_code,
        territory_code=territory_code,
        credit_status=credit_status,
        min_balance=min_balance,
        include_details=include_details
    )
    
    return analysis


@router.get("/{customer_code}/aged-balance", response_model=CustomerAgedBalance)
async def get_customer_aged_balance(
    customer_code: str,
    as_of_date: Optional[date] = Query(None),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Get aged balance for specific customer.
    
    Shows outstanding invoices grouped by age.
    """
    aged_balance = statement_service.get_customer_aged_balance(
        db,
        customer_code=customer_code,
        as_of_date=as_of_date or date.today()
    )
    
    if not aged_balance:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Customer {customer_code} not found"
        )
    
    return aged_balance


@router.post("/reminder-letters")
async def generate_reminder_letters(
    min_days_overdue: int = Query(30, ge=1),
    min_amount_overdue: Decimal = Query(100, ge=0),
    letter_template: str = Query("standard", description="Template to use"),
    send_emails: bool = Query(False, description="Send via email"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Generate payment reminder letters.
    
    For customers with overdue invoices.
    """
    # Get overdue customers
    overdue_customers = statement_service.get_overdue_customers(
        db,
        min_days_overdue=min_days_overdue,
        min_amount_overdue=min_amount_overdue
    )
    
    results = []
    letters_generated = 0
    emails_sent = 0
    
    for customer in overdue_customers:
        try:
            # Generate reminder letter
            letter = statement_service.generate_reminder_letter(
                db,
                customer_code=customer.customer_code,
                template=letter_template
            )
            
            letters_generated += 1
            
            result = {
                "customer_code": customer.customer_code,
                "letter_generated": True,
                "overdue_amount": customer.overdue_amount,
                "days_overdue": customer.max_days_overdue
            }
            
            # Send email if requested
            if send_emails and customer.email:
                try:
                    statement_service.email_reminder_letter(
                        db,
                        customer_code=customer.customer_code,
                        letter=letter
                    )
                    emails_sent += 1
                    result["email_sent"] = True
                except Exception as e:
                    result["email_error"] = str(e)
            
            results.append(result)
            
        except Exception as e:
            results.append({
                "customer_code": customer.customer_code,
                "letter_generated": False,
                "error": str(e)
            })
    
    return {
        "customers_processed": len(overdue_customers),
        "letters_generated": letters_generated,
        "emails_sent": emails_sent,
        "results": results
    }


@router.post("/statement-settings/{customer_code}")
async def update_statement_settings(
    customer_code: str,
    frequency: StatementFrequency,
    email_enabled: bool = Body(True),
    email_address: Optional[str] = Body(None),
    include_paid_invoices: bool = Body(False),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Update customer statement preferences.
    
    Admin only - sets how statements are generated and delivered.
    """
    customer = statement_service.get_customer(db, customer_code=customer_code)
    if not customer:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Customer {customer_code} not found"
        )
    
    updated = statement_service.update_statement_settings(
        db,
        customer_code=customer_code,
        frequency=frequency,
        email_enabled=email_enabled,
        email_address=email_address,
        include_paid_invoices=include_paid_invoices
    )
    
    return {
        "customer_code": customer_code,
        "frequency": frequency,
        "email_enabled": email_enabled,
        "email_address": email_address or customer.email,
        "settings_updated": True
    }