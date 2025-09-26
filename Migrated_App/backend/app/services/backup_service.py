"""
Backup Service
Database backup, restore, and data management operations
"""

from typing import List, Optional, Dict, Any, IO
from sqlalchemy.orm import Session
from datetime import datetime, timedelta
import subprocess
import json
import os
import shutil
import tempfile
import zipfile
import logging
from pathlib import Path
import uuid
import asyncio
from fastapi import UploadFile
import pandas as pd

logger = logging.getLogger(__name__)


class BackupService:
    """Service for backup and restore operations"""
    
    def __init__(self, db: Session):
        self.db = db
        self.backup_directory = Path("/var/backups/acas")
        self.backup_directory.mkdir(parents=True, exist_ok=True)
        self.config = self._load_backup_config()
    
    # Backup Management Methods
    
    def list_backups(
        self, 
        skip: int = 0, 
        limit: int = 100,
        backup_type: Optional[str] = None
    ) -> List[Dict[str, Any]]:
        """List all available backups"""
        try:
            backups = []
            
            # Scan backup directory for backup files
            for backup_file in self.backup_directory.glob("*.backup"):
                backup_info = self._get_backup_metadata(backup_file)
                if backup_info:
                    if backup_type is None or backup_info.get("type") == backup_type:
                        backups.append(backup_info)
            
            # Sort by creation date (newest first)
            backups.sort(key=lambda x: x.get("created_at", ""), reverse=True)
            
            # Apply pagination
            return backups[skip:skip+limit]
            
        except Exception as e:
            logger.error(f"Error listing backups: {str(e)}")
            raise
    
    def create_backup(
        self,
        backup_type: str = "full",
        description: Optional[str] = None,
        include_tables: Optional[List[str]] = None,
        exclude_tables: Optional[List[str]] = None,
        created_by: Optional[int] = None
    ) -> Dict[str, Any]:
        """Create new database backup"""
        try:
            backup_id = f"backup_{datetime.now().strftime('%Y%m%d_%H%M%S')}_{uuid.uuid4().hex[:8]}"
            backup_filename = f"{backup_id}.backup"
            backup_path = self.backup_directory / backup_filename
            
            # Prepare backup command based on type
            if backup_type == "full":
                backup_info = self._create_full_backup(backup_path, description)
            elif backup_type == "schema-only":
                backup_info = self._create_schema_backup(backup_path, description)
            elif backup_type == "data-only":
                backup_info = self._create_data_backup(backup_path, description)
            elif backup_type == "custom":
                backup_info = self._create_custom_backup(
                    backup_path, description, include_tables, exclude_tables
                )
            else:
                raise ValueError(f"Unsupported backup type: {backup_type}")
            
            # Create metadata file
            metadata = {
                "backup_id": backup_id,
                "type": backup_type,
                "description": description,
                "created_at": datetime.now().isoformat(),
                "created_by": created_by,
                "file_path": str(backup_path),
                "file_size": backup_path.stat().st_size if backup_path.exists() else 0,
                "include_tables": include_tables,
                "exclude_tables": exclude_tables,
                "status": "completed",
                **backup_info
            }
            
            metadata_path = self.backup_directory / f"{backup_id}_metadata.json"
            with open(metadata_path, 'w') as f:
                json.dump(metadata, f, indent=2)
            
            logger.info(f"Backup created: {backup_id}")
            return metadata
            
        except Exception as e:
            logger.error(f"Error creating backup: {str(e)}")
            raise
    
    def get_backup_info(self, backup_id: str) -> Optional[Dict[str, Any]]:
        """Get detailed backup information"""
        try:
            metadata_path = self.backup_directory / f"{backup_id}_metadata.json"
            
            if not metadata_path.exists():
                return None
            
            with open(metadata_path, 'r') as f:
                metadata = json.load(f)
            
            # Update file info if backup file exists
            backup_path = Path(metadata["file_path"])
            if backup_path.exists():
                metadata["file_size"] = backup_path.stat().st_size
                metadata["file_exists"] = True
            else:
                metadata["file_exists"] = False
            
            return metadata
            
        except Exception as e:
            logger.error(f"Error getting backup info {backup_id}: {str(e)}")
            raise
    
    def delete_backup(self, backup_id: str) -> bool:
        """Delete backup and its metadata"""
        try:
            metadata = self.get_backup_info(backup_id)
            if not metadata:
                return False
            
            # Delete backup file
            backup_path = Path(metadata["file_path"])
            if backup_path.exists():
                backup_path.unlink()
            
            # Delete metadata file
            metadata_path = self.backup_directory / f"{backup_id}_metadata.json"
            if metadata_path.exists():
                metadata_path.unlink()
            
            logger.info(f"Backup deleted: {backup_id}")
            return True
            
        except Exception as e:
            logger.error(f"Error deleting backup {backup_id}: {str(e)}")
            raise
    
    # Restore Methods
    
    def restore_backup(
        self,
        backup_id: str,
        target_database: Optional[str] = None,
        restore_options: Optional[Dict[str, Any]] = None,
        restored_by: Optional[int] = None
    ) -> Dict[str, Any]:
        """Restore from backup"""
        try:
            backup_info = self.get_backup_info(backup_id)
            if not backup_info:
                raise ValueError(f"Backup {backup_id} not found")
            
            backup_path = Path(backup_info["file_path"])
            if not backup_path.exists():
                raise ValueError(f"Backup file not found: {backup_path}")
            
            restore_options = restore_options or {}
            
            # Prepare restore command
            restore_result = self._execute_restore(
                backup_path, target_database, restore_options
            )
            
            # Log restore operation
            restore_log = {
                "backup_id": backup_id,
                "target_database": target_database,
                "restored_at": datetime.now().isoformat(),
                "restored_by": restored_by,
                "options": restore_options,
                "result": restore_result
            }
            
            # Save restore log
            log_path = self.backup_directory / f"restore_log_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
            with open(log_path, 'w') as f:
                json.dump(restore_log, f, indent=2)
            
            logger.info(f"Database restored from backup: {backup_id}")
            return restore_result
            
        except Exception as e:
            logger.error(f"Error restoring backup {backup_id}: {str(e)}")
            raise
    
    def upload_backup(
        self,
        file: UploadFile,
        description: Optional[str] = None,
        uploaded_by: Optional[int] = None
    ) -> Dict[str, Any]:
        """Upload backup file"""
        try:
            backup_id = f"upload_{datetime.now().strftime('%Y%m%d_%H%M%S')}_{uuid.uuid4().hex[:8]}"
            file_extension = Path(file.filename).suffix
            backup_filename = f"{backup_id}{file_extension}"
            backup_path = self.backup_directory / backup_filename
            
            # Save uploaded file
            with open(backup_path, "wb") as buffer:
                shutil.copyfileobj(file.file, buffer)
            
            # Validate backup file
            validation_result = self._validate_backup_file(backup_path)
            
            # Create metadata
            metadata = {
                "backup_id": backup_id,
                "type": "uploaded",
                "description": description or f"Uploaded backup: {file.filename}",
                "original_filename": file.filename,
                "created_at": datetime.now().isoformat(),
                "uploaded_by": uploaded_by,
                "file_path": str(backup_path),
                "file_size": backup_path.stat().st_size,
                "status": "completed" if validation_result["valid"] else "invalid",
                "validation": validation_result
            }
            
            metadata_path = self.backup_directory / f"{backup_id}_metadata.json"
            with open(metadata_path, 'w') as f:
                json.dump(metadata, f, indent=2)
            
            logger.info(f"Backup uploaded: {backup_id}")
            return metadata
            
        except Exception as e:
            logger.error(f"Error uploading backup: {str(e)}")
            raise
    
    # Scheduled Backup Configuration
    
    def get_backup_schedule(self) -> Dict[str, Any]:
        """Get automated backup schedule configuration"""
        return self.config.get("schedule", {
            "enabled": False,
            "frequency": "daily",
            "time": "02:00",
            "retention_days": 30,
            "backup_types": ["full"],
            "email_notifications": True,
            "notification_recipients": []
        })
    
    def update_backup_schedule(
        self,
        schedule_config: Dict[str, Any],
        updated_by: Optional[int] = None
    ) -> Dict[str, Any]:
        """Update automated backup schedule"""
        try:
            # Validate schedule configuration
            self._validate_schedule_config(schedule_config)
            
            # Update configuration
            self.config["schedule"] = schedule_config
            self.config["schedule"]["updated_at"] = datetime.now().isoformat()
            self.config["schedule"]["updated_by"] = updated_by
            
            # Save configuration
            self._save_backup_config()
            
            # Update cron job if enabled
            if schedule_config.get("enabled", False):
                self._update_backup_cron_job(schedule_config)
            else:
                self._remove_backup_cron_job()
            
            logger.info("Backup schedule updated")
            return self.config["schedule"]
            
        except Exception as e:
            logger.error(f"Error updating backup schedule: {str(e)}")
            raise
    
    # Data Export/Import Methods
    
    def export_data(
        self,
        export_config: Dict[str, Any],
        exported_by: Optional[int] = None
    ) -> Dict[str, Any]:
        """Export specific data to various formats"""
        try:
            export_id = f"export_{datetime.now().strftime('%Y%m%d_%H%M%S')}_{uuid.uuid4().hex[:8]}"
            export_format = export_config.get("format", "csv")
            tables = export_config.get("tables", [])
            conditions = export_config.get("conditions", {})
            
            export_dir = self.backup_directory / "exports" / export_id
            export_dir.mkdir(parents=True, exist_ok=True)
            
            exported_files = []
            
            if export_format == "sql":
                result = self._export_to_sql(tables, conditions, export_dir)
            elif export_format == "csv":
                result = self._export_to_csv(tables, conditions, export_dir)
            elif export_format == "json":
                result = self._export_to_json(tables, conditions, export_dir)
            elif export_format == "excel":
                result = self._export_to_excel(tables, conditions, export_dir)
            else:
                raise ValueError(f"Unsupported export format: {export_format}")
            
            # Create zip archive
            archive_path = self.backup_directory / f"{export_id}.zip"
            with zipfile.ZipFile(archive_path, 'w', zipfile.ZIP_DEFLATED) as zipf:
                for file_path in export_dir.rglob("*"):
                    if file_path.is_file():
                        zipf.write(file_path, file_path.relative_to(export_dir))
            
            # Clean up temporary directory
            shutil.rmtree(export_dir)
            
            export_info = {
                "export_id": export_id,
                "format": export_format,
                "tables": tables,
                "exported_at": datetime.now().isoformat(),
                "exported_by": exported_by,
                "archive_path": str(archive_path),
                "archive_size": archive_path.stat().st_size,
                "record_counts": result.get("record_counts", {}),
                "status": "completed"
            }
            
            logger.info(f"Data exported: {export_id}")
            return export_info
            
        except Exception as e:
            logger.error(f"Error exporting data: {str(e)}")
            raise
    
    def import_data(
        self,
        file: UploadFile,
        import_options: Dict[str, Any],
        imported_by: Optional[int] = None
    ) -> Dict[str, Any]:
        """Import data from file"""
        try:
            import_id = f"import_{datetime.now().strftime('%Y%m%d_%H%M%S')}_{uuid.uuid4().hex[:8]}"
            file_extension = Path(file.filename).suffix.lower()
            
            # Create temporary file
            with tempfile.NamedTemporaryFile(delete=False, suffix=file_extension) as temp_file:
                shutil.copyfileobj(file.file, temp_file)
                temp_path = Path(temp_file.name)
            
            try:
                # Import based on file type
                if file_extension == '.sql':
                    result = self._import_from_sql(temp_path, import_options)
                elif file_extension == '.csv':
                    result = self._import_from_csv(temp_path, import_options)
                elif file_extension == '.json':
                    result = self._import_from_json(temp_path, import_options)
                elif file_extension in ['.xlsx', '.xls']:
                    result = self._import_from_excel(temp_path, import_options)
                else:
                    raise ValueError(f"Unsupported import file type: {file_extension}")
                
                import_info = {
                    "import_id": import_id,
                    "filename": file.filename,
                    "file_type": file_extension,
                    "imported_at": datetime.now().isoformat(),
                    "imported_by": imported_by,
                    "options": import_options,
                    "result": result,
                    "status": "completed"
                }
                
                logger.info(f"Data imported: {import_id}")
                return import_info
                
            finally:
                # Clean up temporary file
                temp_path.unlink()
                
        except Exception as e:
            logger.error(f"Error importing data: {str(e)}")
            raise
    
    # Cleanup and Maintenance Methods
    
    def cleanup_old_backups(self, retention_days: int = 30, dry_run: bool = False) -> Dict[str, Any]:
        """Clean up old backup files"""
        try:
            cutoff_date = datetime.now() - timedelta(days=retention_days)
            
            files_to_delete = []
            total_size = 0
            
            # Find old backup files
            for backup_file in self.backup_directory.glob("*.backup"):
                metadata_file = self.backup_directory / f"{backup_file.stem}_metadata.json"
                
                if metadata_file.exists():
                    with open(metadata_file, 'r') as f:
                        metadata = json.load(f)
                    
                    created_at = datetime.fromisoformat(metadata.get("created_at", ""))
                    if created_at < cutoff_date:
                        file_size = backup_file.stat().st_size
                        files_to_delete.append({
                            "backup_file": backup_file,
                            "metadata_file": metadata_file,
                            "backup_id": metadata.get("backup_id"),
                            "created_at": created_at.isoformat(),
                            "size": file_size
                        })
                        total_size += file_size
            
            if not dry_run:
                # Delete files
                for file_info in files_to_delete:
                    file_info["backup_file"].unlink()
                    file_info["metadata_file"].unlink()
            
            cleanup_info = {
                "cutoff_date": cutoff_date.isoformat(),
                "retention_days": retention_days,
                "files_found": len(files_to_delete),
                "total_size": total_size,
                "space_freed": self._format_bytes(total_size),
                "dry_run": dry_run,
                "deleted_backups": [f["backup_id"] for f in files_to_delete],
                "cleaned_at": datetime.now().isoformat()
            }
            
            if not dry_run:
                logger.info(f"Backup cleanup completed: {len(files_to_delete)} files deleted")
            
            return cleanup_info
            
        except Exception as e:
            logger.error(f"Error during backup cleanup: {str(e)}")
            raise
    
    # Private Helper Methods
    
    def _load_backup_config(self) -> Dict[str, Any]:
        """Load backup configuration"""
        config_file = self.backup_directory / "backup_config.json"
        
        if config_file.exists():
            try:
                with open(config_file, 'r') as f:
                    return json.load(f)
            except Exception as e:
                logger.error(f"Error loading backup config: {str(e)}")
        
        # Return default configuration
        return {
            "database": {
                "host": os.getenv("DATABASE_HOST", "localhost"),
                "port": int(os.getenv("DATABASE_PORT", "5432")),
                "name": os.getenv("DATABASE_NAME", "acas_db"),
                "username": os.getenv("DATABASE_USER", "postgres"),
                "password": os.getenv("DATABASE_PASSWORD", "")
            },
            "schedule": {
                "enabled": False,
                "frequency": "daily",
                "time": "02:00",
                "retention_days": 30
            }
        }
    
    def _save_backup_config(self):
        """Save backup configuration"""
        config_file = self.backup_directory / "backup_config.json"
        with open(config_file, 'w') as f:
            json.dump(self.config, f, indent=2)
    
    def _get_backup_metadata(self, backup_file: Path) -> Optional[Dict[str, Any]]:
        """Get backup metadata from file"""
        metadata_file = self.backup_directory / f"{backup_file.stem}_metadata.json"
        
        if metadata_file.exists():
            try:
                with open(metadata_file, 'r') as f:
                    return json.load(f)
            except Exception as e:
                logger.error(f"Error reading metadata for {backup_file}: {str(e)}")
        
        return None
    
    def _create_full_backup(self, backup_path: Path, description: Optional[str]) -> Dict[str, Any]:
        """Create full database backup"""
        db_config = self.config["database"]
        
        cmd = [
            "pg_dump",
            "-h", db_config["host"],
            "-p", str(db_config["port"]),
            "-U", db_config["username"],
            "-d", db_config["name"],
            "-f", str(backup_path),
            "--verbose",
            "--no-password"
        ]
        
        # Set password through environment
        env = os.environ.copy()
        env["PGPASSWORD"] = db_config["password"]
        
        result = subprocess.run(cmd, env=env, capture_output=True, text=True)
        
        if result.returncode != 0:
            raise Exception(f"Backup failed: {result.stderr}")
        
        return {
            "backup_method": "pg_dump",
            "tables_included": "all",
            "data_included": True,
            "schema_included": True
        }
    
    def _create_schema_backup(self, backup_path: Path, description: Optional[str]) -> Dict[str, Any]:
        """Create schema-only backup"""
        db_config = self.config["database"]
        
        cmd = [
            "pg_dump",
            "-h", db_config["host"],
            "-p", str(db_config["port"]),
            "-U", db_config["username"],
            "-d", db_config["name"],
            "-f", str(backup_path),
            "--schema-only",
            "--verbose",
            "--no-password"
        ]
        
        env = os.environ.copy()
        env["PGPASSWORD"] = db_config["password"]
        
        result = subprocess.run(cmd, env=env, capture_output=True, text=True)
        
        if result.returncode != 0:
            raise Exception(f"Schema backup failed: {result.stderr}")
        
        return {
            "backup_method": "pg_dump",
            "tables_included": "all",
            "data_included": False,
            "schema_included": True
        }
    
    def _create_data_backup(self, backup_path: Path, description: Optional[str]) -> Dict[str, Any]:
        """Create data-only backup"""
        db_config = self.config["database"]
        
        cmd = [
            "pg_dump",
            "-h", db_config["host"],
            "-p", str(db_config["port"]),
            "-U", db_config["username"],
            "-d", db_config["name"],
            "-f", str(backup_path),
            "--data-only",
            "--verbose",
            "--no-password"
        ]
        
        env = os.environ.copy()
        env["PGPASSWORD"] = db_config["password"]
        
        result = subprocess.run(cmd, env=env, capture_output=True, text=True)
        
        if result.returncode != 0:
            raise Exception(f"Data backup failed: {result.stderr}")
        
        return {
            "backup_method": "pg_dump",
            "tables_included": "all",
            "data_included": True,
            "schema_included": False
        }
    
    def _create_custom_backup(
        self, 
        backup_path: Path, 
        description: Optional[str],
        include_tables: Optional[List[str]],
        exclude_tables: Optional[List[str]]
    ) -> Dict[str, Any]:
        """Create custom backup with specific tables"""
        db_config = self.config["database"]
        
        cmd = [
            "pg_dump",
            "-h", db_config["host"],
            "-p", str(db_config["port"]),
            "-U", db_config["username"],
            "-d", db_config["name"],
            "-f", str(backup_path),
            "--verbose",
            "--no-password"
        ]
        
        # Add table filters
        if include_tables:
            for table in include_tables:
                cmd.extend(["-t", table])
        
        if exclude_tables:
            for table in exclude_tables:
                cmd.extend(["-T", table])
        
        env = os.environ.copy()
        env["PGPASSWORD"] = db_config["password"]
        
        result = subprocess.run(cmd, env=env, capture_output=True, text=True)
        
        if result.returncode != 0:
            raise Exception(f"Custom backup failed: {result.stderr}")
        
        return {
            "backup_method": "pg_dump",
            "tables_included": include_tables or "all",
            "tables_excluded": exclude_tables or [],
            "data_included": True,
            "schema_included": True
        }
    
    def _execute_restore(
        self, 
        backup_path: Path,
        target_database: Optional[str],
        restore_options: Dict[str, Any]
    ) -> Dict[str, Any]:
        """Execute database restore"""
        db_config = self.config["database"]
        target_db = target_database or db_config["name"]
        
        cmd = [
            "psql",
            "-h", db_config["host"],
            "-p", str(db_config["port"]),
            "-U", db_config["username"],
            "-d", target_db,
            "-f", str(backup_path),
            "--no-password"
        ]
        
        if restore_options.get("quiet", False):
            cmd.append("--quiet")
        
        env = os.environ.copy()
        env["PGPASSWORD"] = db_config["password"]
        
        result = subprocess.run(cmd, env=env, capture_output=True, text=True)
        
        return {
            "success": result.returncode == 0,
            "return_code": result.returncode,
            "stdout": result.stdout,
            "stderr": result.stderr,
            "target_database": target_db
        }
    
    def _validate_backup_file(self, backup_path: Path) -> Dict[str, Any]:
        """Validate backup file"""
        try:
            # Basic file validation
            if not backup_path.exists():
                return {"valid": False, "error": "File does not exist"}
            
            if backup_path.stat().st_size == 0:
                return {"valid": False, "error": "File is empty"}
            
            # TODO: Add more sophisticated validation
            # - Check if file is valid SQL
            # - Verify PostgreSQL dump format
            # - Check for corruption
            
            return {"valid": True, "size": backup_path.stat().st_size}
            
        except Exception as e:
            return {"valid": False, "error": str(e)}
    
    def _validate_schedule_config(self, config: Dict[str, Any]):
        """Validate backup schedule configuration"""
        required_fields = ["enabled", "frequency", "time", "retention_days"]
        for field in required_fields:
            if field not in config:
                raise ValueError(f"Missing required field: {field}")
        
        valid_frequencies = ["daily", "weekly", "monthly"]
        if config["frequency"] not in valid_frequencies:
            raise ValueError(f"Invalid frequency. Must be one of: {valid_frequencies}")
        
        if not isinstance(config["retention_days"], int) or config["retention_days"] < 1:
            raise ValueError("retention_days must be a positive integer")
    
    def _update_backup_cron_job(self, schedule_config: Dict[str, Any]):
        """Update cron job for automated backups"""
        # TODO: Implement cron job management
        logger.info("Backup cron job updated")
    
    def _remove_backup_cron_job(self):
        """Remove backup cron job"""
        # TODO: Implement cron job removal
        logger.info("Backup cron job removed")
    
    def _format_bytes(self, bytes_value: int) -> str:
        """Format bytes in human readable format"""
        for unit in ['B', 'KB', 'MB', 'GB', 'TB']:
            if bytes_value < 1024.0:
                return f"{bytes_value:.1f} {unit}"
            bytes_value /= 1024.0
        return f"{bytes_value:.1f} PB"
    
    # Export format methods (placeholder implementations)
    
    def _export_to_sql(self, tables: List[str], conditions: Dict[str, Any], export_dir: Path) -> Dict[str, Any]:
        """Export to SQL format"""
        # TODO: Implement SQL export
        return {"record_counts": {}}
    
    def _export_to_csv(self, tables: List[str], conditions: Dict[str, Any], export_dir: Path) -> Dict[str, Any]:
        """Export to CSV format"""
        # TODO: Implement CSV export using pandas
        return {"record_counts": {}}
    
    def _export_to_json(self, tables: List[str], conditions: Dict[str, Any], export_dir: Path) -> Dict[str, Any]:
        """Export to JSON format"""
        # TODO: Implement JSON export
        return {"record_counts": {}}
    
    def _export_to_excel(self, tables: List[str], conditions: Dict[str, Any], export_dir: Path) -> Dict[str, Any]:
        """Export to Excel format"""
        # TODO: Implement Excel export using pandas
        return {"record_counts": {}}
    
    # Import format methods (placeholder implementations)
    
    def _import_from_sql(self, file_path: Path, options: Dict[str, Any]) -> Dict[str, Any]:
        """Import from SQL file"""
        # TODO: Implement SQL import
        return {"records_imported": 0}
    
    def _import_from_csv(self, file_path: Path, options: Dict[str, Any]) -> Dict[str, Any]:
        """Import from CSV file"""
        # TODO: Implement CSV import using pandas
        return {"records_imported": 0}
    
    def _import_from_json(self, file_path: Path, options: Dict[str, Any]) -> Dict[str, Any]:
        """Import from JSON file"""
        # TODO: Implement JSON import
        return {"records_imported": 0}
    
    def _import_from_excel(self, file_path: Path, options: Dict[str, Any]) -> Dict[str, Any]:
        """Import from Excel file"""
        # TODO: Implement Excel import using pandas
        return {"records_imported": 0}