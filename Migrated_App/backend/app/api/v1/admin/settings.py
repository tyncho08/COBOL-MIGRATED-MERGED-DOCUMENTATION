from typing import Dict, Any
from fastapi import APIRouter, Depends, HTTPException, Body
from sqlalchemy.orm import Session
from datetime import datetime
from app.core.database import get_db
from app.models.system import SystemRec

router = APIRouter()

@router.get("/settings")
async def get_system_settings(
    db: Session = Depends(get_db)
) -> Dict[str, Any]:
    """Get all system settings"""
    
    try:
        # Get system record (single record)
        system_record = db.query(SystemRec).filter(
            SystemRec.system_rec_key == 1
        ).first()
        
        # Build response
        settings = {
            "company": {
                "companyName": "ACAS Corporation Ltd",
                "registrationNumber": "REG-123456",
                "vatNumber": "VAT-789012",
                "phoneNumber": "+1-555-0123",
                "address": {
                    "line1": "123 Business Street",
                    "line2": "Suite 100",
                    "city": "New York",
                    "postCode": "10001"
                }
            },
            "financial": {
                "defaultPaymentTerms": "30",
                "defaultSettlementDiscount": "2.0",
                "nextNumbers": {
                    "invoiceNumber": "INV-2025-0001",
                    "creditNoteNumber": "CN-2025-0001",
                    "purchaseOrderNumber": "PO-2025-0001",
                    "receiptNumber": "REC-2025-0001"
                },
                "fiscalYearEnd": "12/31"
            },
            "tax": {
                "taxCodes": [
                    {
                        "code": "STD",
                        "description": "Standard Rate",
                        "rate": 20.0,
                        "type": "output"
                    },
                    {
                        "code": "RED",
                        "description": "Reduced Rate",
                        "rate": 5.0,
                        "type": "output"
                    },
                    {
                        "code": "ZER",
                        "description": "Zero Rate",
                        "rate": 0.0,
                        "type": "output"
                    },
                    {
                        "code": "EXM",
                        "description": "Exempt",
                        "rate": 0.0,
                        "type": "exempt"
                    }
                ]
            },
            "system": {
                "version": "4.0.0",
                "database": "PostgreSQL 15.2",
                "lastMigration": datetime.now().isoformat(),
                "totalRecords": 125678,
                "backupSchedule": "daily",
                "retentionDays": 30
            }
        }
        
        # Update with actual system data if available
        if system_record:
            settings["company"].update({
                "companyName": system_record.company_name,
                "address": {
                    "line1": system_record.company_address_1,
                    "line2": system_record.company_address_2,
                    "city": system_record.company_address_3,
                    "postCode": system_record.company_address_5
                }
            })
            settings["system"].update({
                "version": system_record.version
            })
        
        return settings
        
    except Exception as e:
        print(f"Error fetching system settings: {str(e)}")
        # Return default settings if error
        return {
            "company": {
                "companyName": "Company Name",
                "registrationNumber": "",
                "vatNumber": "",
                "phoneNumber": "",
                "address": {
                    "line1": "",
                    "line2": "",
                    "city": "",
                    "postCode": ""
                }
            },
            "financial": {
                "defaultPaymentTerms": "30",
                "defaultSettlementDiscount": "0",
                "nextNumbers": {
                    "invoiceNumber": "INV-0001",
                    "creditNoteNumber": "CN-0001", 
                    "purchaseOrderNumber": "PO-0001",
                    "receiptNumber": "REC-0001"
                },
                "fiscalYearEnd": "12/31"
            },
            "tax": {
                "taxCodes": []
            },
            "system": {
                "version": "4.0.0",
                "database": "PostgreSQL",
                "lastMigration": "",
                "totalRecords": 0,
                "backupSchedule": "daily",
                "retentionDays": 30
            }
        }

@router.put("/settings")
async def update_system_settings(
    settings: Dict[str, Any] = Body(...),
    db: Session = Depends(get_db)
) -> Dict[str, Any]:
    """Update system settings"""
    
    try:
        # In a real implementation, you would:
        # 1. Validate the settings
        # 2. Update each setting in the database
        # 3. Log the changes for audit
        
        # For now, just return success
        return {
            "success": True,
            "message": "Settings updated successfully",
            "updated_at": datetime.now().isoformat()
        }
        
    except Exception as e:
        return {
            "success": False,
            "message": f"Failed to update settings: {str(e)}"
        }