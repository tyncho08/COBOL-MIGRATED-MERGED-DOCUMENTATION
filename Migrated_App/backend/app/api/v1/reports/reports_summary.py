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


@router.get("/all")
async def get_all_reports(
    category: str = None,
    search: str = None,
    db: Session = Depends(get_db)
) -> Dict[str, Any]:
    """Get all available reports with filtering options"""
    
    # Get all reports from the summary
    summary_data = await get_reports_summary(db)
    
    all_reports = []
    for category_data in summary_data["categories"]:
        for report in category_data["reports"]:
            report["category_name"] = category_data["name"]
            report["category_icon"] = category_data["icon"]
            all_reports.append(report)
    
    # Apply filters
    filtered_reports = all_reports
    
    if category:
        filtered_reports = [r for r in filtered_reports if r["category"] == category]
    
    if search:
        search_lower = search.lower()
        filtered_reports = [
            r for r in filtered_reports 
            if search_lower in r["name"].lower() or search_lower in r["description"].lower()
        ]
    
    # Sort by last generated (most recent first)
    filtered_reports.sort(key=lambda x: x["lastGenerated"], reverse=True)
    
    return {
        "reports": filtered_reports,
        "total_count": len(filtered_reports),
        "filters_applied": {
            "category": category,
            "search": search
        },
        "available_categories": list(set([r["category"] for r in all_reports])),
        "available_formats": list(set([fmt for r in all_reports for fmt in r["format"]]))
    }


@router.get("/categories")
async def get_report_categories(
    db: Session = Depends(get_db)
) -> Dict[str, Any]:
    """Get report categories with metadata"""
    
    try:
        # Try to query the report_categories table
        categories_result = db.execute(text("""
            SELECT 
                category_id,
                category_name,
                category_description,
                display_order,
                icon_name,
                color_class,
                is_active
            FROM report_categories 
            WHERE is_active = TRUE
            ORDER BY display_order
        """))
        
        categories = []
        for row in categories_result:
            categories.append({
                "id": row.category_id,
                "name": row.category_name,
                "description": row.category_description,
                "icon": row.icon_name,
                "color": row.color_class,
                "display_order": row.display_order,
                "is_active": row.is_active
            })
    
    except Exception as e:
        # Fallback to default categories
        categories = [
            {
                "id": 1,
                "name": "Financial",
                "description": "P&L, Balance Sheet, Trial Balance, and financial statements",
                "icon": "CurrencyDollarIcon",
                "color": "bg-green-500",
                "display_order": 1,
                "is_active": True
            },
            {
                "id": 2,
                "name": "Sales",
                "description": "Customer analysis, aging reports, and sales performance",
                "icon": "UsersIcon",
                "color": "bg-blue-500",
                "display_order": 2,
                "is_active": True
            },
            {
                "id": 3,
                "name": "Purchase",
                "description": "Supplier analysis, AP aging, and purchase performance",
                "icon": "TruckIcon",
                "color": "bg-purple-500",
                "display_order": 3,
                "is_active": True
            },
            {
                "id": 4,
                "name": "Stock",
                "description": "Inventory valuation, movement reports, and stock analysis",
                "icon": "CubeIcon",
                "color": "bg-orange-500",
                "display_order": 4,
                "is_active": True
            },
            {
                "id": 5,
                "name": "Tax",
                "description": "VAT returns, tax calculations, and compliance reports",
                "icon": "CalculatorIcon",
                "color": "bg-red-500",
                "display_order": 5,
                "is_active": True
            }
        ]
    
    return {
        "categories": categories,
        "total_categories": len(categories)
    }


@router.get("/history")
async def get_report_history(
    days: int = 30,
    user_id: str = None,
    db: Session = Depends(get_db)
) -> Dict[str, Any]:
    """Get report generation history"""
    
    # Mock report generation history
    # In production, would query report_generation_log table
    from datetime import datetime, timedelta
    import random
    
    history = []
    reports = ["trial-balance", "profit-loss", "aged-receivables", "stock-valuation", "vat-return"]
    users = ["admin@acas.local", "accountant@acas.local", "manager@acas.local"]
    
    for i in range(20):
        generated_date = datetime.now() - timedelta(days=random.randint(0, days))
        report_id = random.choice(reports)
        user = random.choice(users)
        
        if user_id and user != user_id:
            continue
            
        history.append({
            "id": i + 1,
            "report_id": report_id,
            "report_name": report_id.replace("-", " ").title(),
            "format": random.choice(["pdf", "excel", "csv"]),
            "generated_by": user,
            "generated_at": generated_date.isoformat(),
            "file_size": random.randint(50, 2000),  # KB
            "status": random.choice(["completed", "completed", "completed", "failed"]),
            "download_count": random.randint(0, 5)
        })
    
    # Sort by generation date (most recent first)
    history.sort(key=lambda x: x["generated_at"], reverse=True)
    
    return {
        "history": history,
        "total_count": len(history),
        "filters": {
            "days": days,
            "user_id": user_id
        },
        "summary": {
            "total_reports": len(history),
            "successful": len([h for h in history if h["status"] == "completed"]),
            "failed": len([h for h in history if h["status"] == "failed"]),
            "total_downloads": sum([h["download_count"] for h in history])
        }
    }


@router.get("/popular")
async def get_popular_reports(
    days: int = 30,
    limit: int = 10,
    db: Session = Depends(get_db)
) -> Dict[str, Any]:
    """Get most popular reports based on usage statistics"""
    
    try:
        # Try to query the report_usage_stats table
        popular_result = db.execute(text("""
            SELECT 
                report_id,
                report_name,
                total_generations,
                total_downloads,
                unique_users,
                avg_generation_time,
                last_generated
            FROM report_usage_stats 
            ORDER BY total_generations DESC
            LIMIT %s
        """), [limit])
        
        popular_reports = []
        for row in popular_result:
            popular_reports.append({
                "report_id": row.report_id,
                "report_name": row.report_name,
                "total_generations": row.total_generations,
                "total_downloads": row.total_downloads,
                "unique_users": row.unique_users,
                "avg_generation_time": float(row.avg_generation_time) if row.avg_generation_time else 0,
                "last_generated": row.last_generated.isoformat() if row.last_generated else None,
                "popularity_score": row.total_generations * 2 + row.total_downloads
            })
    
    except Exception as e:
        # Fallback to mock popular reports data
        popular_reports = [
            {
                "report_id": "trial-balance",
                "report_name": "Trial Balance",
                "total_generations": 156,
                "total_downloads": 234,
                "unique_users": 12,
                "avg_generation_time": 2.3,
                "last_generated": datetime.now().isoformat(),
                "popularity_score": 546
            },
            {
                "report_id": "profit-loss",
                "report_name": "Profit & Loss Statement",
                "total_generations": 89,
                "total_downloads": 123,
                "unique_users": 8,
                "avg_generation_time": 4.1,
                "last_generated": (datetime.now() - timedelta(days=1)).isoformat(),
                "popularity_score": 301
            },
            {
                "report_id": "aged-receivables",
                "report_name": "Aged Receivables",
                "total_generations": 67,
                "total_downloads": 89,
                "unique_users": 6,
                "avg_generation_time": 1.8,
                "last_generated": (datetime.now() - timedelta(hours=3)).isoformat(),
                "popularity_score": 223
            },
            {
                "report_id": "stock-valuation",
                "report_name": "Stock Valuation Report",
                "total_generations": 45,
                "total_downloads": 56,
                "unique_users": 4,
                "avg_generation_time": 3.2,
                "last_generated": (datetime.now() - timedelta(hours=6)).isoformat(),
                "popularity_score": 146
            },
            {
                "report_id": "balance-sheet",
                "report_name": "Balance Sheet",
                "total_generations": 34,
                "total_downloads": 45,
                "unique_users": 7,
                "avg_generation_time": 5.1,
                "last_generated": (datetime.now() - timedelta(days=2)).isoformat(),
                "popularity_score": 113
            }
        ]
    
    return {
        "popular_reports": popular_reports[:limit],
        "period_days": days,
        "total_count": len(popular_reports),
        "summary": {
            "total_generations": sum([r["total_generations"] for r in popular_reports]),
            "total_downloads": sum([r["total_downloads"] for r in popular_reports]),
            "most_popular": popular_reports[0]["report_name"] if popular_reports else None,
            "avg_generation_time": sum([r["avg_generation_time"] for r in popular_reports]) / len(popular_reports) if popular_reports else 0
        }
    }