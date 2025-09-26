"""Purchase Ledger Supplier Management API endpoints"""

from fastapi import APIRouter, Depends, HTTPException, status, Query, Body
from sqlalchemy.orm import Session
from typing import List, Optional, Dict, Any
from datetime import date, datetime
from decimal import Decimal

from app.api import deps
from app.services.pl import supplier_master as supplier_service
from app.schemas.pl import (
    Supplier, SupplierCreate, SupplierUpdate, SupplierListResponse,
    SupplierSummary, SupplierStatus, PaymentTerms,
    SupplierAgedBalance, AgedPayablesSummary
)

router = APIRouter()


@router.get("", response_model=SupplierListResponse)
async def list_suppliers(
    status: Optional[SupplierStatus] = Query(None, description="Filter by status"),
    search: Optional[str] = Query(None, description="Search in code/name"),
    payment_terms: Optional[PaymentTerms] = Query(None, description="Filter by payment terms"),
    is_1099_vendor: Optional[bool] = Query(None, description="Filter 1099 vendors"),
    has_balance: bool = Query(False, description="Only suppliers with balance"),
    skip: int = 0,
    limit: int = 100,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    List suppliers with optional filters.
    
    Returns supplier master data with current balances.
    """
    suppliers = supplier_service.get_suppliers(
        db,
        status=status,
        search=search,
        payment_terms=payment_terms,
        is_1099_vendor=is_1099_vendor,
        has_balance=has_balance,
        skip=skip,
        limit=limit
    )
    
    total = supplier_service.count_suppliers(
        db,
        status=status,
        search=search,
        payment_terms=payment_terms,
        is_1099_vendor=is_1099_vendor,
        has_balance=has_balance
    )
    
    return SupplierListResponse(
        suppliers=suppliers,
        total=total,
        skip=skip,
        limit=limit
    )


@router.get("/{supplier_code}", response_model=Supplier)
async def get_supplier(
    supplier_code: str,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Get specific supplier by code."""
    supplier = supplier_service.get_supplier(db, supplier_code=supplier_code)
    if not supplier:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Supplier {supplier_code} not found"
        )
    return supplier


@router.get("/{supplier_code}/summary", response_model=SupplierSummary)
async def get_supplier_summary(
    supplier_code: str,
    include_transactions: bool = Query(False, description="Include recent transactions"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Get comprehensive supplier summary.
    
    Includes balances, aging, and activity.
    """
    summary = supplier_service.get_supplier_summary(
        db,
        supplier_code=supplier_code,
        include_transactions=include_transactions
    )
    
    if not summary:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Supplier {supplier_code} not found"
        )
    
    return summary


@router.post("", response_model=Supplier, status_code=status.HTTP_201_CREATED)
async def create_supplier(
    supplier_in: SupplierCreate,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Create new supplier.
    
    Validates unique supplier code and tax ID.
    """
    # Check if supplier code already exists
    existing = supplier_service.get_supplier(db, supplier_code=supplier_in.supplier_code)
    if existing:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Supplier code {supplier_in.supplier_code} already exists"
        )
    
    # Validate tax ID if provided
    if supplier_in.tax_id:
        duplicate_tax_id = supplier_service.get_supplier_by_tax_id(db, tax_id=supplier_in.tax_id)
        if duplicate_tax_id:
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail=f"Tax ID {supplier_in.tax_id} already registered to supplier {duplicate_tax_id.supplier_code}"
            )
    
    supplier = supplier_service.create_supplier(
        db,
        supplier_in=supplier_in,
        created_by=current_user.id
    )
    
    return supplier


@router.put("/{supplier_code}", response_model=Supplier)
async def update_supplier(
    supplier_code: str,
    supplier_update: SupplierUpdate,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Update supplier information.
    
    Cannot change supplier code.
    """
    supplier = supplier_service.get_supplier(db, supplier_code=supplier_code)
    if not supplier:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Supplier {supplier_code} not found"
        )
    
    updated_supplier = supplier_service.update_supplier(
        db,
        supplier_code=supplier_code,
        supplier_update=supplier_update,
        updated_by=current_user.id
    )
    
    return updated_supplier


@router.post("/{supplier_code}/deactivate")
async def deactivate_supplier(
    supplier_code: str,
    reason: str = Body(..., min_length=10),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Deactivate supplier.
    
    Prevents new transactions but keeps history.
    """
    supplier = supplier_service.get_supplier(db, supplier_code=supplier_code)
    if not supplier:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Supplier {supplier_code} not found"
        )
    
    if supplier.status == SupplierStatus.INACTIVE:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Supplier is already inactive"
        )
    
    # Check for open transactions
    open_items = supplier_service.get_open_items_count(db, supplier_code=supplier_code)
    if open_items > 0:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Cannot deactivate supplier with {open_items} open transactions"
        )
    
    updated = supplier_service.update_supplier_status(
        db,
        supplier_code=supplier_code,
        status=SupplierStatus.INACTIVE,
        reason=reason,
        updated_by=current_user.id
    )
    
    return {
        "supplier_code": supplier_code,
        "status": updated.status,
        "deactivated_at": datetime.now(),
        "reason": reason
    }


@router.post("/{supplier_code}/reactivate")
async def reactivate_supplier(
    supplier_code: str,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """Reactivate inactive supplier."""
    supplier = supplier_service.get_supplier(db, supplier_code=supplier_code)
    if not supplier:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Supplier {supplier_code} not found"
        )
    
    if supplier.status == SupplierStatus.ACTIVE:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Supplier is already active"
        )
    
    updated = supplier_service.update_supplier_status(
        db,
        supplier_code=supplier_code,
        status=SupplierStatus.ACTIVE,
        reason="Reactivated by user",
        updated_by=current_user.id
    )
    
    return {
        "supplier_code": supplier_code,
        "status": updated.status,
        "reactivated_at": datetime.now()
    }


@router.put("/{supplier_code}/credit-limit")
async def update_credit_limit(
    supplier_code: str,
    new_limit: Decimal = Body(..., ge=0),
    reason: str = Body(..., min_length=10),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """Update supplier credit limit."""
    supplier = supplier_service.get_supplier(db, supplier_code=supplier_code)
    if not supplier:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Supplier {supplier_code} not found"
        )
    
    updated = supplier_service.update_credit_limit(
        db,
        supplier_code=supplier_code,
        new_limit=new_limit,
        reason=reason,
        updated_by=current_user.id
    )
    
    return {
        "supplier_code": supplier_code,
        "previous_limit": supplier.credit_limit,
        "new_limit": new_limit,
        "credit_available": new_limit - supplier.current_balance
    }


@router.get("/{supplier_code}/aged-balance", response_model=SupplierAgedBalance)
async def get_supplier_aged_balance(
    supplier_code: str,
    as_of_date: Optional[date] = Query(None),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Get aged balance for specific supplier.
    
    Shows outstanding invoices by age.
    """
    aged_balance = supplier_service.get_supplier_aged_balance(
        db,
        supplier_code=supplier_code,
        as_of_date=as_of_date or date.today()
    )
    
    if not aged_balance:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Supplier {supplier_code} not found"
        )
    
    return aged_balance


@router.get("/{supplier_code}/purchase-history")
async def get_purchase_history(
    supplier_code: str,
    start_date: Optional[date] = None,
    end_date: Optional[date] = None,
    include_details: bool = Query(False),
    skip: int = 0,
    limit: int = 100,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Get supplier purchase history."""
    history = supplier_service.get_purchase_history(
        db,
        supplier_code=supplier_code,
        start_date=start_date,
        end_date=end_date,
        include_details=include_details,
        skip=skip,
        limit=limit
    )
    
    if not history:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Supplier {supplier_code} not found"
        )
    
    return history


@router.get("/{supplier_code}/payment-history")
async def get_payment_history(
    supplier_code: str,
    start_date: Optional[date] = None,
    end_date: Optional[date] = None,
    skip: int = 0,
    limit: int = 100,
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Get supplier payment history."""
    history = supplier_service.get_payment_history(
        db,
        supplier_code=supplier_code,
        start_date=start_date,
        end_date=end_date,
        skip=skip,
        limit=limit
    )
    
    if not history:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Supplier {supplier_code} not found"
        )
    
    return history


@router.get("/{supplier_code}/documents")
async def get_supplier_documents(
    supplier_code: str,
    document_type: Optional[str] = Query(None, description="Filter by type: W9, CONTRACT, etc"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """Get supplier documents on file."""
    documents = supplier_service.get_supplier_documents(
        db,
        supplier_code=supplier_code,
        document_type=document_type
    )
    
    return {
        "supplier_code": supplier_code,
        "document_count": len(documents),
        "documents": documents
    }


@router.post("/{supplier_code}/documents")
async def upload_supplier_document(
    supplier_code: str,
    document_type: str = Body(...),
    document_name: str = Body(...),
    file_content: str = Body(..., description="Base64 encoded file"),
    expiry_date: Optional[date] = Body(None),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """Upload supplier document."""
    supplier = supplier_service.get_supplier(db, supplier_code=supplier_code)
    if not supplier:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Supplier {supplier_code} not found"
        )
    
    document = supplier_service.upload_document(
        db,
        supplier_code=supplier_code,
        document_type=document_type,
        document_name=document_name,
        file_content=file_content,
        expiry_date=expiry_date,
        uploaded_by=current_user.id
    )
    
    return {
        "document_id": document["id"],
        "document_type": document_type,
        "uploaded_at": datetime.now(),
        "expiry_date": expiry_date
    }


@router.get("/aged-payables", response_model=AgedPayablesSummary)
async def get_aged_payables_report(
    as_of_date: Optional[date] = Query(None, description="Analysis date"),
    supplier_code: Optional[str] = Query(None, description="Filter by supplier"),
    min_balance: Decimal = Query(0, description="Minimum balance"),
    include_details: bool = Query(True, description="Include invoice details"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_active_user),
):
    """
    Get aged payables analysis.
    
    Shows outstanding amounts by aging buckets.
    """
    analysis = supplier_service.generate_aged_payables(
        db,
        as_of_date=as_of_date or date.today(),
        supplier_code=supplier_code,
        min_balance=min_balance,
        include_details=include_details
    )
    
    return analysis


@router.get("/1099-report")
async def get_1099_report(
    tax_year: int = Query(..., ge=2000, le=2099),
    min_amount: Decimal = Query(600, description="Minimum reportable amount"),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Generate 1099 report for tax year.
    
    Lists vendors requiring 1099 forms.
    """
    report = supplier_service.generate_1099_report(
        db,
        tax_year=tax_year,
        min_amount=min_amount
    )
    
    return {
        "tax_year": tax_year,
        "report_date": date.today(),
        "minimum_amount": min_amount,
        "vendor_count": len(report["vendors"]),
        "total_payments": report["total_payments"],
        "vendors": report["vendors"]
    }


@router.post("/import")
async def import_suppliers(
    file_format: str = Body("csv", regex="^(csv|excel)$"),
    file_content: str = Body(..., description="Base64 encoded file"),
    update_existing: bool = Body(False),
    db: Session = Depends(deps.get_db),
    current_user=Depends(deps.get_current_admin_user),
):
    """
    Import suppliers from file.
    
    Validates and imports supplier master data.
    """
    result = supplier_service.import_suppliers(
        db,
        file_format=file_format,
        file_content=file_content,
        update_existing=update_existing,
        imported_by=current_user.id
    )
    
    return {
        "total_rows": result["total_rows"],
        "imported": result["imported"],
        "updated": result["updated"],
        "errors": result["errors"],
        "warnings": result["warnings"]
    }