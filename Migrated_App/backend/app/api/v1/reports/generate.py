"""
Reports Generation API

Handles report generation requests, standard reports, and custom reports.
Provides endpoints for generating, exporting, and managing reports.
"""

from typing import Dict, List, Any, Optional
from datetime import datetime, date
from fastapi import APIRouter, Depends, HTTPException, Query, BackgroundTasks, File, UploadFile
from fastapi.responses import FileResponse
from sqlalchemy.orm import Session
import logging

from app.core.database import get_db
from app.core.security import get_current_user
from app.models.auth import User
from app.schemas.reports import (
    ReportRequest, ReportResponse, ReportListResponse, 
    ExportFormat, CustomReportRequest
)
from app.services.reporting import ReportEngine, ExportService, ReportBuilder

logger = logging.getLogger(__name__)
router = APIRouter()

# Initialize services
report_engine = ReportEngine()
export_service = ExportService()
report_builder = ReportBuilder()


@router.post("/generate", response_model=ReportResponse)
async def generate_report(
    request: ReportRequest,
    background_tasks: BackgroundTasks,
    db: Session = Depends(get_db),
    current_user: User = Depends(get_current_user)
):
    """
    Generate a standard report
    
    Generate any of the standard system reports including:
    - Trial Balance
    - Profit & Loss Statement  
    - Balance Sheet
    - Customer Aging
    - Supplier Aging
    - Stock Valuation
    - Sales Analysis
    - Purchase Analysis
    """
    try:
        logger.info(f"Generating report: {request.report_type} for user: {current_user.username}")
        
        # Generate report
        report_response = await report_engine.generate_report(request)
        
        # Log report generation
        logger.info(f"Report generated: {report_response.report_id}")
        
        return report_response
        
    except ValueError as e:
        logger.error(f"Invalid report request: {str(e)}")
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        logger.error(f"Error generating report: {str(e)}")
        raise HTTPException(status_code=500, detail="Failed to generate report")


@router.post("/generate/{report_type}", response_model=ReportResponse)
async def generate_specific_report(
    report_type: str,
    parameters: Dict[str, Any] = None,
    db: Session = Depends(get_db),
    current_user: User = Depends(get_current_user)
):
    """
    Generate a specific report by type
    
    Quick generation of standard reports with parameters:
    - trial_balance: period_id, as_at_date, level_of_detail
    - profit_loss: start_date, end_date, show_comparatives
    - balance_sheet: as_at_date, show_comparatives
    - customer_aging: as_at_date, customer_range
    - supplier_aging: as_at_date, supplier_range
    - stock_valuation: as_at_date, location_code, costing_method
    - sales_analysis: start_date, end_date, group_by
    - purchase_analysis: start_date, end_date
    """
    try:
        # Create report request
        request = ReportRequest(
            report_type=report_type,
            parameters=parameters or {},
            format=ExportFormat.JSON
        )
        
        logger.info(f"Generating {report_type} report for user: {current_user.username}")
        
        # Generate report
        report_response = await report_engine.generate_report(request)
        
        return report_response
        
    except ValueError as e:
        logger.error(f"Invalid report type {report_type}: {str(e)}")
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        logger.error(f"Error generating {report_type} report: {str(e)}")
        raise HTTPException(status_code=500, detail="Failed to generate report")


@router.post("/export/{report_id}")
async def export_report(
    report_id: str,
    format: ExportFormat,
    db: Session = Depends(get_db),
    current_user: User = Depends(get_current_user)
):
    """
    Export a generated report to PDF, Excel, or CSV format
    
    Supported formats:
    - PDF: Professional formatted PDF with styling
    - EXCEL: Excel spreadsheet with formatting and formulas  
    - CSV: Comma-separated values for data import
    """
    try:
        # In a real implementation, we would retrieve the report from database
        # For now, we'll regenerate it (this should be cached/stored)
        
        logger.info(f"Exporting report {report_id} as {format} for user: {current_user.username}")
        
        # Get the report data - first try to regenerate since we don't have cache yet
        from app.services.reporting.report_engine import ReportEngine
        from app.services.reporting.export_service import ExportService
        from app.core.database import get_db
        
        # Create report request based on report_id
        report_request = {
            "report_type": "trial_balance",  # Default for now
            "parameters": {
                "as_of_date": datetime.now().date().isoformat(),
                "include_zero_balances": False
            },
            "format": "standard"
        }
        
        # Generate report
        db_session = next(get_db())
        try:
            report_engine = ReportEngine(db_session)
            report_data = await report_engine.generate_report(report_request)
            
            # Export report
            export_service = ExportService()
            export_result = await export_service.export_report(report_data, format)
            
            # Return file response
            from fastapi.responses import FileResponse
            return FileResponse(
                path=export_result["file_path"],
                filename=f"report_{report_id}_{format}.{format}",
                media_type=export_result["media_type"]
            )
            
        finally:
            db_session.close()
        
    except Exception as e:
        logger.error(f"Error exporting report {report_id}: {str(e)}")
        raise HTTPException(status_code=500, detail="Failed to export report")


@router.get("/download/{file_id}")
async def download_report(
    file_id: str,
    current_user: User = Depends(get_current_user)
):
    """
    Download an exported report file
    
    Returns the exported report file for download.
    """
    try:
        # Construct file path (in real implementation, this would be from database)
        filepath = f"/tmp/reports/{file_id}"
        
        logger.info(f"Downloading report file: {file_id} for user: {current_user.username}")
        
        return FileResponse(
            filepath,
            media_type="application/octet-stream",
            filename=file_id
        )
        
    except FileNotFoundError:
        raise HTTPException(status_code=404, detail="Report file not found")
    except Exception as e:
        logger.error(f"Error downloading report {file_id}: {str(e)}")
        raise HTTPException(status_code=500, detail="Failed to download report")


@router.post("/custom/generate", response_model=Dict[str, Any])
async def generate_custom_report(
    request: CustomReportRequest,
    db: Session = Depends(get_db),
    current_user: User = Depends(get_current_user)
):
    """
    Generate a custom report using the report builder
    
    Allows users to create custom reports by selecting:
    - Fields to include
    - Filters to apply
    - Grouping options
    - Sorting criteria
    """
    try:
        logger.info(f"Generating custom report for user: {current_user.username}")
        
        # Generate custom report
        report_data = await report_builder.build_custom_report(request)
        
        return report_data
        
    except Exception as e:
        logger.error(f"Error generating custom report: {str(e)}")
        raise HTTPException(status_code=500, detail="Failed to generate custom report")


@router.get("/custom/fields")
async def get_available_fields(
    module: Optional[str] = Query(None, description="Filter fields by module (gl, sl, pl, stock)"),
    current_user: User = Depends(get_current_user)
):
    """
    Get available fields for custom report building
    
    Returns all available fields that can be included in custom reports,
    optionally filtered by module.
    """
    try:
        fields = await report_builder.get_available_fields(module)
        
        return {
            "available_fields": fields,
            "total_fields": len(fields)
        }
        
    except Exception as e:
        logger.error(f"Error getting available fields: {str(e)}")
        raise HTTPException(status_code=500, detail="Failed to get available fields")


@router.get("/templates")
async def get_report_templates(
    current_user: User = Depends(get_current_user)
):
    """
    Get available report templates
    
    Returns list of standard report templates and user-saved custom templates.
    """
    try:
        # Standard report templates
        standard_templates = [
            {
                "id": "trial_balance",
                "name": "Trial Balance",
                "description": "Complete trial balance with all GL accounts",
                "category": "Financial",
                "parameters": ["period_id", "as_at_date", "level_of_detail"]
            },
            {
                "id": "profit_loss", 
                "name": "Profit & Loss Statement",
                "description": "Income statement showing revenue and expenses",
                "category": "Financial",
                "parameters": ["start_date", "end_date", "show_comparatives"]
            },
            {
                "id": "balance_sheet",
                "name": "Balance Sheet", 
                "description": "Statement of financial position",
                "category": "Financial",
                "parameters": ["as_at_date", "show_comparatives"]
            },
            {
                "id": "customer_aging",
                "name": "Customer Aging Report",
                "description": "Outstanding receivables by aging buckets", 
                "category": "Sales",
                "parameters": ["as_at_date", "customer_range"]
            },
            {
                "id": "supplier_aging",
                "name": "Supplier Aging Report",
                "description": "Outstanding payables by aging buckets",
                "category": "Purchase", 
                "parameters": ["as_at_date", "supplier_range"]
            },
            {
                "id": "stock_valuation",
                "name": "Stock Valuation Report",
                "description": "Inventory valuation by location and category",
                "category": "Stock",
                "parameters": ["as_at_date", "location_code", "costing_method"]
            },
            {
                "id": "sales_analysis",
                "name": "Sales Analysis",
                "description": "Sales performance by customer and product",
                "category": "Sales",
                "parameters": ["start_date", "end_date", "group_by"]
            },
            {
                "id": "purchase_analysis",
                "name": "Purchase Analysis", 
                "description": "Purchase performance and spend analysis",
                "category": "Purchase",
                "parameters": ["start_date", "end_date"]
            }
        ]
        
        return {
            "standard_templates": standard_templates,
            "custom_templates": [],  # Would load from database
            "total_templates": len(standard_templates)
        }
        
    except Exception as e:
        logger.error(f"Error getting report templates: {str(e)}")
        raise HTTPException(status_code=500, detail="Failed to get report templates")


@router.get("/history")
async def get_report_history(
    limit: int = Query(50, ge=1, le=1000),
    offset: int = Query(0, ge=0),
    report_type: Optional[str] = Query(None),
    user_id: Optional[str] = Query(None),
    current_user: User = Depends(get_current_user)
):
    """
    Get report generation history
    
    Returns history of generated reports with filtering and pagination.
    """
    try:
        # Mock report history - in real implementation this would come from database
        history = [
            {
                "report_id": "trial_balance_20240115_093000",
                "report_type": "trial_balance",
                "title": "Trial Balance",
                "generated_at": "2024-01-15T09:30:00Z",
                "generated_by": current_user.username,
                "parameters": {"period_id": "2024-01", "as_at_date": "2024-01-15"},
                "format": "PDF",
                "file_size": "1.2 MB",
                "status": "completed"
            },
            {
                "report_id": "customer_aging_20240115_101500", 
                "report_type": "customer_aging",
                "title": "Customer Aging Report",
                "generated_at": "2024-01-15T10:15:00Z",
                "generated_by": current_user.username,
                "parameters": {"as_at_date": "2024-01-15"},
                "format": "Excel",
                "file_size": "856 KB",
                "status": "completed"
            },
            {
                "report_id": "stock_valuation_20240115_080000",
                "report_type": "stock_valuation", 
                "title": "Stock Valuation Report",
                "generated_at": "2024-01-15T08:00:00Z",
                "generated_by": current_user.username,
                "parameters": {"as_at_date": "2024-01-15", "location_code": "MAIN"},
                "format": "PDF",
                "file_size": "2.1 MB", 
                "status": "completed"
            }
        ]
        
        return {
            "reports": history[offset:offset+limit],
            "total_count": len(history),
            "limit": limit,
            "offset": offset
        }
        
    except Exception as e:
        logger.error(f"Error getting report history: {str(e)}")
        raise HTTPException(status_code=500, detail="Failed to get report history")


@router.delete("/history/{report_id}")
async def delete_report_history(
    report_id: str,
    current_user: User = Depends(get_current_user)
):
    """
    Delete a report from history and remove associated files
    
    Removes the report from history and deletes any exported files.
    """
    try:
        logger.info(f"Deleting report {report_id} for user: {current_user.username}")
        
        # In real implementation, this would:
        # 1. Delete from database
        # 2. Remove files from storage
        # 3. Log the deletion
        
        return {"message": f"Report {report_id} deleted successfully"}
        
    except Exception as e:
        logger.error(f"Error deleting report {report_id}: {str(e)}")
        raise HTTPException(status_code=500, detail="Failed to delete report")


@router.get("/status/{report_id}")
async def get_report_status(
    report_id: str,
    current_user: User = Depends(get_current_user)
):
    """
    Get the generation status of a report
    
    Returns the current status of report generation for long-running reports.
    """
    try:
        # Mock status - in real implementation this would come from job queue
        status = {
            "report_id": report_id,
            "status": "completed",  # pending, processing, completed, failed
            "progress": 100,
            "message": "Report generation completed successfully",
            "started_at": "2024-01-15T09:30:00Z",
            "completed_at": "2024-01-15T09:30:15Z",
            "file_url": f"/api/v1/reports/download/{report_id}.pdf"
        }
        
        return status
        
    except Exception as e:
        logger.error(f"Error getting report status {report_id}: {str(e)}")
        raise HTTPException(status_code=500, detail="Failed to get report status")


@router.post("/validate")
async def validate_report_parameters(
    request: ReportRequest,
    current_user: User = Depends(get_current_user)
):
    """
    Validate report parameters before generation
    
    Validates that all required parameters are provided and have valid values.
    """
    try:
        # Basic validation
        if not request.report_type:
            raise ValueError("Report type is required")
        
        # Report-specific validation
        validation_rules = {
            "trial_balance": ["period_id", "as_at_date"],
            "profit_loss": ["start_date", "end_date"],
            "balance_sheet": ["as_at_date"],
            "customer_aging": ["as_at_date"],
            "supplier_aging": ["as_at_date"],
            "stock_valuation": ["as_at_date"],
            "sales_analysis": ["start_date", "end_date"],
            "purchase_analysis": ["start_date", "end_date"]
        }
        
        required_params = validation_rules.get(request.report_type, [])
        missing_params = [param for param in required_params if param not in (request.parameters or {})]
        
        if missing_params:
            return {
                "valid": False,
                "errors": [f"Missing required parameter: {param}" for param in missing_params],
                "warnings": []
            }
        
        return {
            "valid": True,
            "errors": [],
            "warnings": []
        }
        
    except Exception as e:
        logger.error(f"Error validating report parameters: {str(e)}")
        raise HTTPException(status_code=500, detail="Failed to validate parameters")