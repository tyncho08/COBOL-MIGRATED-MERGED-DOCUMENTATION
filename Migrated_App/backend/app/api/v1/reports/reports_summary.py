from typing import Dict, Any, List
from fastapi import APIRouter, Depends, HTTPException
from sqlalchemy.orm import Session
from sqlalchemy import func, text
from datetime import datetime, date, timedelta
from app.core.database import get_db
from app.models.customer import SalesLedgerRec
from app.models.supplier import PurchaseLedgerRec
from app.models.stock import StockRec
from app.models.gl_accounts import GLLedgerRec

router = APIRouter()

@router.get("/summary")
async def get_reports_summary(
    db: Session = Depends(get_db)
) -> Dict[str, Any]:
    """Get available reports and their metadata"""
    
    try:
        # Financial Reports
        financial_reports = [
            {
                "id": "balance-sheet",
                "name": "Balance Sheet",
                "description": "Statement of financial position",
                "category": "financial",
                "lastGenerated": datetime.now().isoformat(),
                "format": ["pdf", "excel", "csv"],
                "frequency": "monthly"
            },
            {
                "id": "profit-loss",
                "name": "Profit & Loss Statement",
                "description": "Income and expenses overview",
                "category": "financial",
                "lastGenerated": (datetime.now() - timedelta(days=2)).isoformat(),
                "format": ["pdf", "excel", "csv"],
                "frequency": "monthly"
            },
            {
                "id": "cash-flow",
                "name": "Cash Flow Statement",
                "description": "Cash inflows and outflows",
                "category": "financial",
                "lastGenerated": (datetime.now() - timedelta(days=5)).isoformat(),
                "format": ["pdf", "excel"],
                "frequency": "monthly"
            },
            {
                "id": "trial-balance",
                "name": "Trial Balance",
                "description": "List of all general ledger accounts",
                "category": "financial",
                "lastGenerated": datetime.now().isoformat(),
                "format": ["pdf", "excel", "csv"],
                "frequency": "daily"
            }
        ]
        
        # Sales Reports
        sales_reports = [
            {
                "id": "sales-summary",
                "name": "Sales Summary",
                "description": "Overview of sales performance",
                "category": "sales",
                "lastGenerated": datetime.now().isoformat(),
                "format": ["pdf", "excel"],
                "frequency": "daily"
            },
            {
                "id": "customer-statements",
                "name": "Customer Statements",
                "description": "Individual customer account statements",
                "category": "sales",
                "lastGenerated": (datetime.now() - timedelta(days=1)).isoformat(),
                "format": ["pdf"],
                "frequency": "monthly"
            },
            {
                "id": "aged-receivables",
                "name": "Aged Receivables",
                "description": "Outstanding customer balances by age",
                "category": "sales",
                "lastGenerated": datetime.now().isoformat(),
                "format": ["pdf", "excel"],
                "frequency": "weekly"
            },
            {
                "id": "sales-analysis",
                "name": "Sales Analysis",
                "description": "Detailed sales trends and patterns",
                "category": "sales",
                "lastGenerated": (datetime.now() - timedelta(days=3)).isoformat(),
                "format": ["pdf", "excel", "csv"],
                "frequency": "monthly"
            }
        ]
        
        # Purchase Reports
        purchase_reports = [
            {
                "id": "purchase-summary",
                "name": "Purchase Summary",
                "description": "Overview of purchasing activity",
                "category": "purchase",
                "lastGenerated": datetime.now().isoformat(),
                "format": ["pdf", "excel"],
                "frequency": "weekly"
            },
            {
                "id": "supplier-statements",
                "name": "Supplier Statements",
                "description": "Individual supplier account statements",
                "category": "purchase",
                "lastGenerated": (datetime.now() - timedelta(days=2)).isoformat(),
                "format": ["pdf"],
                "frequency": "monthly"
            },
            {
                "id": "aged-payables",
                "name": "Aged Payables",
                "description": "Outstanding supplier balances by age",
                "category": "purchase",
                "lastGenerated": datetime.now().isoformat(),
                "format": ["pdf", "excel"],
                "frequency": "weekly"
            }
        ]
        
        # Stock Reports
        stock_reports = [
            {
                "id": "stock-valuation",
                "name": "Stock Valuation Report",
                "description": "Current stock value by location",
                "category": "stock",
                "lastGenerated": datetime.now().isoformat(),
                "format": ["pdf", "excel"],
                "frequency": "daily"
            },
            {
                "id": "stock-movement",
                "name": "Stock Movement Report",
                "description": "Stock transactions and transfers",
                "category": "stock",
                "lastGenerated": (datetime.now() - timedelta(days=1)).isoformat(),
                "format": ["pdf", "excel", "csv"],
                "frequency": "daily"
            },
            {
                "id": "reorder-report",
                "name": "Reorder Report",
                "description": "Items below reorder level",
                "category": "stock",
                "lastGenerated": datetime.now().isoformat(),
                "format": ["pdf", "excel"],
                "frequency": "daily"
            }
        ]
        
        # Tax Reports
        tax_reports = [
            {
                "id": "vat-return",
                "name": "VAT Return",
                "description": "Value Added Tax return summary",
                "category": "tax",
                "lastGenerated": (datetime.now() - timedelta(days=15)).isoformat(),
                "format": ["pdf", "excel"],
                "frequency": "quarterly"
            },
            {
                "id": "tax-summary",
                "name": "Tax Summary",
                "description": "All tax obligations summary",
                "category": "tax",
                "lastGenerated": (datetime.now() - timedelta(days=7)).isoformat(),
                "format": ["pdf"],
                "frequency": "monthly"
            }
        ]
        
        return {
            "categories": [
                {
                    "id": "financial",
                    "name": "Financial Reports",
                    "icon": "ChartBarIcon",
                    "description": "Core financial statements and analysis",
                    "reportCount": len(financial_reports),
                    "reports": financial_reports
                },
                {
                    "id": "sales",
                    "name": "Sales Reports",
                    "icon": "ShoppingCartIcon",
                    "description": "Customer and sales performance reports",
                    "reportCount": len(sales_reports),
                    "reports": sales_reports
                },
                {
                    "id": "purchase",
                    "name": "Purchase Reports",
                    "icon": "TruckIcon",
                    "description": "Supplier and purchasing analysis",
                    "reportCount": len(purchase_reports),
                    "reports": purchase_reports
                },
                {
                    "id": "stock",
                    "name": "Stock Reports",
                    "icon": "CubeIcon",
                    "description": "Inventory valuation and movement",
                    "reportCount": len(stock_reports),
                    "reports": stock_reports
                },
                {
                    "id": "tax",
                    "name": "Tax Reports",
                    "icon": "CalculatorIcon",
                    "description": "Tax returns and compliance reports",
                    "reportCount": len(tax_reports),
                    "reports": tax_reports
                }
            ],
            "recentlyGenerated": []
        }
        
    except Exception as e:
        print(f"Error fetching reports summary: {str(e)}")
        # Return minimal structure if error occurs
        return {
            "categories": [],
            "recentlyGenerated": []
        }

@router.get("/generate/{report_id}")
async def generate_report(
    report_id: str,
    format: str = "pdf",
    db: Session = Depends(get_db)
) -> Dict[str, Any]:
    """Generate a specific report"""
    
    # Simplified implementation - in real app would generate actual report
    return {
        "success": True,
        "message": f"Report {report_id} generated successfully in {format} format",
        "downloadUrl": f"/api/v1/reports/download/{report_id}.{format}",
        "generatedAt": datetime.now().isoformat()
    }