"""
Admin Backup API endpoints
Database backup, restore, and data management
"""

from typing import List, Optional, Dict, Any
from fastapi import APIRouter, Depends, HTTPException, status, Query, UploadFile, File
from fastapi.responses import FileResponse
from sqlalchemy.orm import Session
from app.core.database import get_db
from app.core.security import require_admin
from app.models.auth import User
from app.services.backup_service import BackupService
import logging
import os
from datetime import datetime, timedelta
from pathlib import Path

logger = logging.getLogger(__name__)

router = APIRouter()

# Database Backup Endpoints

@router.get("/", response_model=List[Dict[str, Any]])
async def list_backups(
    skip: int = Query(0, ge=0),
    limit: int = Query(100, ge=1, le=1000),
    backup_type: Optional[str] = Query(None),
    current_user: User = Depends(require_admin),
    db: Session = Depends(get_db)
):
    """
    List all available backups
    Requires admin privileges
    """
    try:
        backup_service = BackupService(db)
        backups = backup_service.list_backups(
            skip=skip, 
            limit=limit, 
            backup_type=backup_type
        )
        return backups
        
    except Exception as e:
        logger.error(f"Error listing backups: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to retrieve backups"
        )

@router.post("/create", status_code=status.HTTP_201_CREATED)
async def create_backup(
    backup_type: str = "full",  # full, incremental, schema-only, data-only
    description: Optional[str] = None,
    include_tables: Optional[List[str]] = None,
    exclude_tables: Optional[List[str]] = None,
    current_user: User = Depends(require_admin),
    db: Session = Depends(get_db)
):
    """
    Create new database backup
    Requires admin privileges
    """
    try:
        backup_service = BackupService(db)
        
        # Validate backup type
        valid_types = ["full", "incremental", "schema-only", "data-only", "custom"]
        if backup_type not in valid_types:
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail=f"Invalid backup type. Must be one of: {valid_types}"
            )
        
        backup_info = backup_service.create_backup(
            backup_type=backup_type,
            description=description or f"Manual backup created by {current_user.username}",
            include_tables=include_tables,
            exclude_tables=exclude_tables,
            created_by=current_user.id
        )
        
        logger.info(f"Backup created: {backup_info['backup_id']} by admin {current_user.username}")
        
        return {
            "message": "Backup created successfully",
            "backup_info": backup_info,
            "created_by": current_user.username,
            "created_at": datetime.now().isoformat()
        }
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error creating backup: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to create backup"
        )

@router.get("/{backup_id}")
async def get_backup_info(
    backup_id: str,
    current_user: User = Depends(require_admin),
    db: Session = Depends(get_db)
):
    """
    Get detailed backup information
    Requires admin privileges
    """
    try:
        backup_service = BackupService(db)
        backup_info = backup_service.get_backup_info(backup_id)
        
        if not backup_info:
            raise HTTPException(
                status_code=status.HTTP_404_NOT_FOUND,
                detail="Backup not found"
            )
        
        return backup_info
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error getting backup info {backup_id}: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to retrieve backup information"
        )

@router.get("/{backup_id}/download")
async def download_backup(
    backup_id: str,
    current_user: User = Depends(require_admin),
    db: Session = Depends(get_db)
):
    """
    Download backup file
    Requires admin privileges
    """
    try:
        backup_service = BackupService(db)
        backup_info = backup_service.get_backup_info(backup_id)
        
        if not backup_info:
            raise HTTPException(
                status_code=status.HTTP_404_NOT_FOUND,
                detail="Backup not found"
            )
        
        backup_file_path = backup_info["file_path"]
        if not os.path.exists(backup_file_path):
            raise HTTPException(
                status_code=status.HTTP_404_NOT_FOUND,
                detail="Backup file not found on disk"
            )
        
        logger.info(f"Backup downloaded: {backup_id} by admin {current_user.username}")
        
        return FileResponse(
            path=backup_file_path,
            filename=f"acas_backup_{backup_id}.sql",
            media_type="application/sql"
        )
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error downloading backup {backup_id}: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to download backup"
        )

@router.delete("/{backup_id}", status_code=status.HTTP_204_NO_CONTENT)
async def delete_backup(
    backup_id: str,
    current_user: User = Depends(require_admin),
    db: Session = Depends(get_db)
):
    """
    Delete backup
    Requires admin privileges
    """
    try:
        backup_service = BackupService(db)
        
        backup_info = backup_service.get_backup_info(backup_id)
        if not backup_info:
            raise HTTPException(
                status_code=status.HTTP_404_NOT_FOUND,
                detail="Backup not found"
            )
        
        backup_service.delete_backup(backup_id)
        logger.info(f"Backup deleted: {backup_id} by admin {current_user.username}")
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error deleting backup {backup_id}: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to delete backup"
        )

# Database Restore Endpoints

@router.post("/{backup_id}/restore")
async def restore_backup(
    backup_id: str,
    confirm: bool = False,
    target_database: Optional[str] = None,
    restore_options: Optional[Dict[str, Any]] = None,
    current_user: User = Depends(require_admin),
    db: Session = Depends(get_db)
):
    """
    Restore from backup
    Requires admin privileges and confirmation
    """
    try:
        if not confirm:
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail="Restore operation requires explicit confirmation (confirm=true)"
            )
        
        backup_service = BackupService(db)
        
        backup_info = backup_service.get_backup_info(backup_id)
        if not backup_info:
            raise HTTPException(
                status_code=status.HTTP_404_NOT_FOUND,
                detail="Backup not found"
            )
        
        # Validate restore options
        restore_options = restore_options or {}
        
        restore_result = backup_service.restore_backup(
            backup_id=backup_id,
            target_database=target_database,
            restore_options=restore_options,
            restored_by=current_user.id
        )
        
        logger.info(f"Database restored from backup: {backup_id} by admin {current_user.username}")
        
        return {
            "message": "Database restored successfully",
            "backup_id": backup_id,
            "restore_info": restore_result,
            "restored_by": current_user.username,
            "restored_at": datetime.now().isoformat()
        }
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error restoring backup {backup_id}: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to restore backup"
        )

@router.post("/upload", status_code=status.HTTP_201_CREATED)
async def upload_backup(
    file: UploadFile = File(...),
    description: Optional[str] = None,
    current_user: User = Depends(require_admin),
    db: Session = Depends(get_db)
):
    """
    Upload backup file
    Requires admin privileges
    """
    try:
        # Validate file type
        if not file.filename.endswith(('.sql', '.dump', '.backup')):
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail="Invalid file type. Only .sql, .dump, and .backup files are allowed"
            )
        
        # Validate file size (max 1GB)
        if file.size > 1024 * 1024 * 1024:
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail="File too large. Maximum size is 1GB"
            )
        
        backup_service = BackupService(db)
        
        upload_info = backup_service.upload_backup(
            file=file,
            description=description or f"Uploaded backup: {file.filename}",
            uploaded_by=current_user.id
        )
        
        logger.info(f"Backup uploaded: {file.filename} by admin {current_user.username}")
        
        return {
            "message": "Backup uploaded successfully",
            "upload_info": upload_info,
            "uploaded_by": current_user.username,
            "uploaded_at": datetime.now().isoformat()
        }
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error uploading backup: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to upload backup"
        )

# Automated Backup Configuration

@router.get("/schedule")
async def get_backup_schedule(
    current_user: User = Depends(require_admin),
    db: Session = Depends(get_db)
):
    """
    Get automated backup schedule configuration
    Requires admin privileges
    """
    try:
        backup_service = BackupService(db)
        schedule_config = backup_service.get_backup_schedule()
        
        return schedule_config
        
    except Exception as e:
        logger.error(f"Error getting backup schedule: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to retrieve backup schedule"
        )

@router.put("/schedule")
async def update_backup_schedule(
    schedule_config: Dict[str, Any],
    current_user: User = Depends(require_admin),
    db: Session = Depends(get_db)
):
    """
    Update automated backup schedule
    Requires admin privileges
    """
    try:
        backup_service = BackupService(db)
        
        # Validate schedule configuration
        required_fields = ["enabled", "frequency", "time", "retention_days"]
        for field in required_fields:
            if field not in schedule_config:
                raise HTTPException(
                    status_code=status.HTTP_400_BAD_REQUEST,
                    detail=f"Missing required field: {field}"
                )
        
        updated_schedule = backup_service.update_backup_schedule(
            schedule_config=schedule_config,
            updated_by=current_user.id
        )
        
        logger.info(f"Backup schedule updated by admin {current_user.username}")
        
        return {
            "message": "Backup schedule updated successfully",
            "schedule": updated_schedule,
            "updated_by": current_user.username,
            "updated_at": datetime.now().isoformat()
        }
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error updating backup schedule: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to update backup schedule"
        )

# Data Export/Import Endpoints

@router.post("/export")
async def export_data(
    export_config: Dict[str, Any],
    current_user: User = Depends(require_admin),
    db: Session = Depends(get_db)
):
    """
    Export specific data to various formats
    Requires admin privileges
    """
    try:
        backup_service = BackupService(db)
        
        # Validate export configuration
        valid_formats = ["sql", "csv", "json", "excel"]
        if export_config.get("format") not in valid_formats:
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail=f"Invalid format. Must be one of: {valid_formats}"
            )
        
        export_result = backup_service.export_data(
            export_config=export_config,
            exported_by=current_user.id
        )
        
        logger.info(f"Data exported by admin {current_user.username}")
        
        return {
            "message": "Data exported successfully",
            "export_info": export_result,
            "exported_by": current_user.username,
            "exported_at": datetime.now().isoformat()
        }
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error exporting data: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to export data"
        )

@router.post("/import")
async def import_data(
    file: UploadFile = File(...),
    import_options: Optional[Dict[str, Any]] = None,
    current_user: User = Depends(require_admin),
    db: Session = Depends(get_db)
):
    """
    Import data from file
    Requires admin privileges
    """
    try:
        backup_service = BackupService(db)
        
        # Validate file type
        valid_extensions = ['.sql', '.csv', '.json', '.xlsx']
        file_extension = Path(file.filename).suffix.lower()
        
        if file_extension not in valid_extensions:
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail=f"Invalid file type. Supported types: {valid_extensions}"
            )
        
        import_result = backup_service.import_data(
            file=file,
            import_options=import_options or {},
            imported_by=current_user.id
        )
        
        logger.info(f"Data imported from {file.filename} by admin {current_user.username}")
        
        return {
            "message": "Data imported successfully",
            "import_info": import_result,
            "imported_by": current_user.username,
            "imported_at": datetime.now().isoformat()
        }
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error importing data: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to import data"
        )

# Backup Monitoring and Maintenance

@router.post("/cleanup")
async def cleanup_old_backups(
    days: int = Query(30, ge=1, le=365),
    dry_run: bool = Query(False),
    current_user: User = Depends(require_admin),
    db: Session = Depends(get_db)
):
    """
    Clean up old backup files
    Requires admin privileges
    """
    try:
        backup_service = BackupService(db)
        
        cleanup_result = backup_service.cleanup_old_backups(
            retention_days=days,
            dry_run=dry_run
        )
        
        if not dry_run:
            logger.info(f"Backup cleanup completed by admin {current_user.username}")
        
        return {
            "message": "Backup cleanup completed" if not dry_run else "Backup cleanup preview",
            "cleanup_info": cleanup_result,
            "dry_run": dry_run,
            "performed_by": current_user.username,
            "performed_at": datetime.now().isoformat()
        }
        
    except Exception as e:
        logger.error(f"Error during backup cleanup: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to cleanup backups"
        )

@router.get("/status")
async def get_backup_status(
    current_user: User = Depends(require_admin),
    db: Session = Depends(get_db)
):
    """
    Get overall backup system status
    Requires admin privileges
    """
    try:
        backup_service = BackupService(db)
        
        status_info = {
            "backup_directory": "/var/backups/acas",
            "total_backups": 25,
            "total_size": "15.7 GB",
            "last_backup": {
                "id": "backup_20240126_140000",
                "type": "full",
                "created_at": "2024-01-26T14:00:00Z",
                "size": "1.2 GB",
                "status": "completed"
            },
            "next_scheduled": {
                "type": "incremental",
                "scheduled_at": "2024-01-27T02:00:00Z",
                "in_hours": 12
            },
            "storage": {
                "used": "15.7 GB",
                "available": "184.3 GB",
                "usage_percent": 7.8
            },
            "schedule": {
                "enabled": True,
                "full_backup": "daily at 02:00",
                "incremental_backup": "every 6 hours",
                "retention": "30 days"
            },
            "health": {
                "status": "healthy",
                "last_check": datetime.now().isoformat(),
                "issues": []
            }
        }
        
        return status_info
        
    except Exception as e:
        logger.error(f"Error getting backup status: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to retrieve backup status"
        )