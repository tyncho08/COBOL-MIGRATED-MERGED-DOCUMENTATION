"""
COBOL-Compatible Settings API
Designed to work with the actual migrated COBOL PostgreSQL database structure
"""
from fastapi import APIRouter, Depends, HTTPException
from sqlalchemy.orm import Session
from sqlalchemy import func, text
from decimal import Decimal
from datetime import datetime, date
from typing import Dict, List, Any

from app.core.database import get_db

router = APIRouter()

@router.get("/settings")
async def get_cobol_settings(db: Session = Depends(get_db)) -> Dict[str, Any]:
    """
    Get system settings using REAL COBOL database structure
    - Uses actual system_rec table data
    - Compatible with frontend expectations
    """
    try:
        # Get real system data from COBOL system_rec table
        system_data = db.execute(text("""
            SELECT 
                address_1, address_2, address_3, address_4, post_code, country,
                vat_reg_number, company_email, vat_rate_1, vat_rate_2, vat_rate_3,
                current_quarter, period, next_invoice, 
                sl_pay_ac, sl_debtors, sl_sales_ac, pl_pay_ac, pl_creditors, pl_purch_ac,
                system_record_version_prime, system_record_version_secondary,
                created_at, updated_at
            FROM acas.system_rec 
            WHERE system_rec_key = 1
        """)).fetchone()
        
        if not system_data:
            raise HTTPException(status_code=404, detail="System configuration not found")
        
        # === COMPANY SETTINGS ===
        
        # Use real data where available, sensible defaults where empty
        company_name = "Applewood Computer Accounting System"  # ACAS company name
        company_address = {
            "line1": system_data[0] or "123 Business Park Drive",
            "line2": system_data[1] or "Technology Center",
            "city": system_data[2] or "London",
            "postCode": system_data[4] or "SW1A 1AA"
        }
        
        # === FINANCIAL SETTINGS ===
        
        # Get account codes from real system
        sales_control_ac = system_data[14] or "1200"  # sl_debtors
        sales_ledger_ac = system_data[15] or "4000"   # sl_sales_ac
        purchase_control_ac = system_data[17] or "2000"  # pl_creditors
        purchase_ledger_ac = system_data[18] or "5000"   # pl_purch_ac
        
        financial_settings = {
            "yearStart": "January",
            "currentPeriod": system_data[12] or 1,  # period
            "currentQuarter": system_data[11] or 1,  # current_quarter
            "defaultCurrency": "GBP - British Pound",
            "paymentTerms": "30",
            "settlementDiscount": "2.5",
            "numberSequences": {
                "invoiceNumber": f"INV-{str(system_data[13] or 1).zfill(4)}",  # next_invoice
                "creditNoteNumber": "CN-0001",
                "purchaseOrderNumber": "PO-0001",
                "receiptNumber": "REC-0001"
            },
            "accountCodes": {
                "salesControl": sales_control_ac,
                "salesLedger": sales_ledger_ac,
                "purchaseControl": purchase_control_ac,
                "purchaseLedger": purchase_ledger_ac
            },
            "fiscalYearEnd": "31/03"  # UK standard
        }
        
        # === TAX SETTINGS ===
        
        vat_rates = []
        if system_data[8] and float(system_data[8]) > 0:  # vat_rate_1
            vat_rates.append({
                "code": "S",
                "description": "Standard Rate",
                "rate": float(system_data[8]),
                "default": True
            })
        if system_data[9] and float(system_data[9]) > 0:  # vat_rate_2
            vat_rates.append({
                "code": "R",
                "description": "Reduced Rate", 
                "rate": float(system_data[9]),
                "default": False
            })
        if system_data[10] and float(system_data[10]) > 0:  # vat_rate_3
            vat_rates.append({
                "code": "Z",
                "description": "Zero Rate",
                "rate": float(system_data[10]),
                "default": False
            })
        
        # Add standard zero rate if not present
        if not any(rate["rate"] == 0 for rate in vat_rates):
            vat_rates.append({
                "code": "Z",
                "description": "Zero Rate",
                "rate": 0.0,
                "default": False
            })
        
        tax_settings = {
            "vatRegistrationNumber": system_data[6] or "GB123456789",
            "defaultRate": f"{system_data[8] or 20}% - Standard Rate",
            "scheme": "Standard VAT",
            "taxCodes": vat_rates
        }
        
        # === SYSTEM SETTINGS ===
        
        version = f"{system_data[19] or 4}.{system_data[20] or 0}"  # version_prime.secondary
        
        # Get record counts from database
        try:
            customer_count = db.execute(text("SELECT COUNT(*) FROM acas.saledger_rec")).scalar() or 0
            supplier_count = db.execute(text("SELECT COUNT(*) FROM acas.puledger_rec")).scalar() or 0  
            stock_count = db.execute(text("SELECT COUNT(*) FROM acas.stock_rec")).scalar() or 0
            total_records = customer_count + supplier_count + stock_count
        except:
            total_records = 0
        
        system_settings = {
            "version": version,
            "database": "PostgreSQL (COBOL Migration)",
            "lastMigration": system_data[22].isoformat() if system_data[22] else None,  # updated_at
            "totalRecords": total_records,
            "modules": {
                "generalLedger": True,
                "salesLedger": True,
                "purchaseLedger": True,
                "stockControl": True,
                "irs": True
            },
            "backupSchedule": "daily",
            "retentionDays": 90,
            "auditTrail": True,
            "automaticBackups": True,
            "debugMode": False
        }
        
        # === NOTIFICATIONS SETTINGS ===
        
        notifications = {
            "emailNotifications": True,
            "systemAlerts": True,
            "backupNotifications": True,
            "email": system_data[7] or "admin@acas-system.com",
            "smsNotifications": False,
            "webhookUrl": ""
        }
        
        # === SECURITY SETTINGS ===
        
        security = {
            "passwordPolicy": {
                "minLength": 8,
                "requireNumbers": True,
                "requireSymbols": True,
                "requireUppercase": True
            },
            "sessionTimeout": 60,  # minutes
            "maxLoginAttempts": 5,
            "twoFactorAuth": False,
            "ipWhitelist": [],
            "auditLogging": True
        }
        
        # === BACKUP SETTINGS ===
        
        backup = {
            "automaticBackups": True,
            "schedule": "daily",
            "time": "02:00",
            "retentionDays": 90,
            "location": "/backup/acas",
            "compression": True,
            "encryptBackups": True,
            "lastBackup": system_data[22].isoformat() if system_data[22] else None
        }
        
        return {
            "company": {
                "companyName": company_name,
                "registrationNumber": "12345678",  # Default
                "vatNumber": tax_settings["vatRegistrationNumber"],
                "phoneNumber": "+44 20 7946 0958",  # Default UK number
                "email": notifications["email"],
                "address": company_address
            },
            "financial": financial_settings,
            "tax": tax_settings,
            "system": system_settings,
            "notifications": notifications,
            "security": security,
            "backup": backup,
            "lastUpdated": system_data[22].isoformat() if system_data[22] else datetime.now().isoformat(),
            "dataSource": "COBOL system_rec table",
            "timestamp": datetime.now().isoformat()
        }
        
    except Exception as e:
        print(f"Error fetching COBOL settings: {str(e)}")
        raise HTTPException(status_code=500, detail=f"Database error: {str(e)}")

@router.put("/settings")
async def update_cobol_settings(
    settings: Dict[str, Any],
    db: Session = Depends(get_db)
) -> Dict[str, Any]:
    """
    Update system settings in COBOL database
    """
    try:
        # Update key fields in system_rec table
        company = settings.get("company", {})
        financial = settings.get("financial", {})
        tax = settings.get("tax", {})
        
        # Prepare update data
        update_data = {}
        
        if company.get("address"):
            addr = company["address"]
            update_data.update({
                "address_1": addr.get("line1", ""),
                "address_2": addr.get("line2", ""),
                "address_3": addr.get("city", ""),
                "post_code": addr.get("postCode", "")
            })
        
        if company.get("email"):
            update_data["company_email"] = company["email"]
            
        if tax.get("vatRegistrationNumber"):
            update_data["vat_reg_number"] = tax["vatRegistrationNumber"]
        
        # Update VAT rates if provided
        tax_codes = tax.get("taxCodes", [])
        for i, tax_code in enumerate(tax_codes[:3]):  # Max 3 rates
            if tax_code.get("rate") is not None:
                update_data[f"vat_rate_{i+1}"] = tax_code["rate"]
        
        update_data["updated_at"] = datetime.now()
        update_data["updated_by"] = "web_interface"
        
        # Execute update
        if update_data:
            set_clause = ", ".join([f"{key} = :{key}" for key in update_data.keys()])
            query = f"UPDATE acas.system_rec SET {set_clause} WHERE system_rec_key = 1"
            db.execute(text(query), update_data)
            db.commit()
        
        return {
            "success": True,
            "message": "Settings updated successfully",
            "updated_fields": list(update_data.keys()),
            "timestamp": datetime.now().isoformat()
        }
        
    except Exception as e:
        db.rollback()
        print(f"Error updating COBOL settings: {str(e)}")
        return {
            "success": False,
            "message": f"Error updating settings: {str(e)}"
        }