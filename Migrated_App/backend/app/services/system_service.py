"""
System Service
System monitoring, configuration, and maintenance operations
"""

from typing import Dict, List, Any, Optional, Tuple
from sqlalchemy.orm import Session
from datetime import datetime, timedelta
import psutil
import platform
import subprocess
import json
import os
import logging
from pathlib import Path

logger = logging.getLogger(__name__)


class SystemService:
    """Service for system administration and monitoring operations"""
    
    def __init__(self, db: Session):
        self.db = db
        self.config_file = "/etc/acas/system.conf"
        self.log_directory = "/var/log/acas"
    
    # System Information Methods
    
    def get_system_info(self) -> Dict[str, Any]:
        """Get comprehensive system information"""
        try:
            # Server information
            boot_time = datetime.fromtimestamp(psutil.boot_time())
            uptime = datetime.now() - boot_time
            
            system_info = {
                "server": {
                    "hostname": platform.node(),
                    "platform": platform.platform(),
                    "system": platform.system(),
                    "release": platform.release(),
                    "version": platform.version(),
                    "machine": platform.machine(),
                    "processor": platform.processor(),
                    "boot_time": boot_time.isoformat(),
                    "uptime_seconds": uptime.total_seconds(),
                    "uptime_human": self._format_uptime(uptime)
                },
                
                # Memory information
                "memory": self._get_memory_info(),
                
                # CPU information
                "cpu": self._get_cpu_info(),
                
                # Disk information
                "disk": self._get_disk_info(),
                
                # Network information
                "network": self._get_network_info(),
                
                # Application information
                "application": self._get_application_info()
            }
            
            return system_info
            
        except Exception as e:
            logger.error(f"Error getting system info: {str(e)}")
            raise
    
    def get_system_health(self) -> Dict[str, Any]:
        """Get system health status with checks"""
        try:
            memory = psutil.virtual_memory()
            cpu_percent = psutil.cpu_percent(interval=1)
            disk = psutil.disk_usage('/')
            
            health_status = {
                "status": "healthy",
                "timestamp": datetime.now().isoformat(),
                "checks": {
                    "database": self._check_database_health(),
                    "memory": {
                        "status": "healthy" if memory.percent < 85 else "warning" if memory.percent < 95 else "critical",
                        "usage_percent": memory.percent,
                        "threshold_warning": 85,
                        "threshold_critical": 95,
                        "available_gb": round(memory.available / (1024**3), 2)
                    },
                    "cpu": {
                        "status": "healthy" if cpu_percent < 80 else "warning" if cpu_percent < 95 else "critical",
                        "usage_percent": cpu_percent,
                        "threshold_warning": 80,
                        "threshold_critical": 95,
                        "load_average": os.getloadavg() if hasattr(os, 'getloadavg') else None
                    },
                    "disk": {
                        "status": "healthy" if disk.percent < 85 else "warning" if disk.percent < 95 else "critical",
                        "usage_percent": disk.percent,
                        "threshold_warning": 85,
                        "threshold_critical": 95,
                        "free_gb": round(disk.free / (1024**3), 2)
                    },
                    "processes": self._check_process_health()
                },
                "services": self._check_services_health()
            }
            
            # Determine overall status
            check_statuses = [check["status"] for check in health_status["checks"].values() if isinstance(check, dict)]
            service_statuses = [service["status"] for service in health_status["services"].values()]
            
            all_statuses = check_statuses + service_statuses
            
            if "critical" in all_statuses:
                health_status["status"] = "critical"
            elif "warning" in all_statuses:
                health_status["status"] = "warning"
            elif "degraded" in all_statuses:
                health_status["status"] = "degraded"
            
            return health_status
            
        except Exception as e:
            logger.error(f"Error getting system health: {str(e)}")
            return {
                "status": "error",
                "error": str(e),
                "timestamp": datetime.now().isoformat()
            }
    
    def get_performance_metrics(self, hours: int = 24) -> Dict[str, Any]:
        """Get system performance metrics"""
        try:
            # Current metrics
            memory = psutil.virtual_memory()
            cpu_percent = psutil.cpu_percent(interval=1)
            disk_io = psutil.disk_io_counters()
            network_io = psutil.net_io_counters()
            
            performance_data = {
                "time_range": {
                    "start": (datetime.now() - timedelta(hours=hours)).isoformat(),
                    "end": datetime.now().isoformat(),
                    "hours": hours
                },
                "current_metrics": {
                    "cpu_usage": cpu_percent,
                    "memory_usage": memory.percent,
                    "disk_usage": psutil.disk_usage('/').percent,
                    "network_bytes_sent": network_io.bytes_sent,
                    "network_bytes_recv": network_io.bytes_recv,
                    "disk_read_bytes": disk_io.read_bytes,
                    "disk_write_bytes": disk_io.write_bytes
                },
                "aggregated_metrics": self._get_historical_metrics(hours),
                "alerts": self._get_performance_alerts()
            }
            
            return performance_data
            
        except Exception as e:
            logger.error(f"Error getting performance metrics: {str(e)}")
            raise
    
    # Configuration Management Methods
    
    def get_system_configuration(self) -> Dict[str, Any]:
        """Get system configuration"""
        try:
            config = {
                "database": {
                    "host": os.getenv("DATABASE_HOST", "localhost"),
                    "port": int(os.getenv("DATABASE_PORT", "5432")),
                    "name": os.getenv("DATABASE_NAME", "acas_db"),
                    "max_connections": int(os.getenv("DATABASE_MAX_CONNECTIONS", "100")),
                    "timeout": int(os.getenv("DATABASE_TIMEOUT", "30"))
                },
                "security": {
                    "session_timeout": int(os.getenv("SESSION_TIMEOUT", "3600")),
                    "password_expiry_days": int(os.getenv("PASSWORD_EXPIRY_DAYS", "90")),
                    "max_login_attempts": int(os.getenv("MAX_LOGIN_ATTEMPTS", "5")),
                    "lockout_duration": int(os.getenv("LOCKOUT_DURATION", "300")),
                    "jwt_expiry_minutes": int(os.getenv("JWT_EXPIRY_MINUTES", "30"))
                },
                "application": {
                    "timezone": os.getenv("TIMEZONE", "UTC"),
                    "date_format": os.getenv("DATE_FORMAT", "YYYY-MM-DD"),
                    "decimal_places": int(os.getenv("DECIMAL_PLACES", "2")),
                    "max_file_upload_size": int(os.getenv("MAX_FILE_UPLOAD_SIZE", "10485760")),
                    "backup_retention_days": int(os.getenv("BACKUP_RETENTION_DAYS", "30")),
                    "debug_mode": os.getenv("DEBUG_MODE", "false").lower() == "true"
                },
                "email": {
                    "smtp_server": os.getenv("SMTP_SERVER", ""),
                    "smtp_port": int(os.getenv("SMTP_PORT", "587")),
                    "use_tls": os.getenv("SMTP_USE_TLS", "true").lower() == "true",
                    "username": os.getenv("SMTP_USERNAME", ""),
                    "from_address": os.getenv("SMTP_FROM_ADDRESS", "")
                },
                "logging": {
                    "level": os.getenv("LOG_LEVEL", "INFO"),
                    "retention_days": int(os.getenv("LOG_RETENTION_DAYS", "90")),
                    "max_file_size": os.getenv("LOG_MAX_FILE_SIZE", "100MB"),
                    "enable_audit_log": os.getenv("ENABLE_AUDIT_LOG", "true").lower() == "true"
                },
                "performance": {
                    "cache_size": int(os.getenv("CACHE_SIZE", "1000")),
                    "worker_processes": int(os.getenv("WORKER_PROCESSES", "4")),
                    "max_concurrent_requests": int(os.getenv("MAX_CONCURRENT_REQUESTS", "100")),
                    "request_timeout": int(os.getenv("REQUEST_TIMEOUT", "30"))
                }
            }
            
            # Load additional config from file if exists
            if os.path.exists(self.config_file):
                with open(self.config_file, 'r') as f:
                    file_config = json.load(f)
                    config.update(file_config)
            
            return config
            
        except Exception as e:
            logger.error(f"Error getting system configuration: {str(e)}")
            raise
    
    def update_system_configuration(self, config_update: Dict[str, Any]) -> Dict[str, Any]:
        """Update system configuration"""
        try:
            # Validate configuration
            validation_result = self._validate_configuration(config_update)
            if not validation_result["valid"]:
                raise ValueError(f"Invalid configuration: {validation_result['errors']}")
            
            # Load current config
            current_config = self.get_system_configuration()
            
            # Merge updates
            updated_config = self._merge_config(current_config, config_update)
            
            # Save to file
            config_dir = os.path.dirname(self.config_file)
            os.makedirs(config_dir, exist_ok=True)
            
            with open(self.config_file, 'w') as f:
                json.dump(updated_config, f, indent=2)
            
            # Apply runtime changes
            restart_required = self._apply_runtime_config_changes(config_update)
            
            logger.info("System configuration updated")
            
            return {
                "status": "success",
                "requires_restart": restart_required,
                "updated_at": datetime.now().isoformat(),
                "applied_changes": list(config_update.keys())
            }
            
        except Exception as e:
            logger.error(f"Error updating system configuration: {str(e)}")
            raise
    
    # Service Management Methods
    
    def get_system_services(self) -> Dict[str, Any]:
        """Get status of system services"""
        try:
            services = {
                "web_server": {
                    "name": "ACAS Web Server",
                    "status": self._get_service_status("acas-web"),
                    "pid": self._get_service_pid("acas-web"),
                    "uptime": self._get_service_uptime("acas-web"),
                    "memory_usage": self._get_service_memory("acas-web"),
                    "cpu_usage": self._get_service_cpu("acas-web"),
                    "port": 8000,
                    "last_restart": self._get_service_last_restart("acas-web")
                },
                "database": {
                    "name": "PostgreSQL Database",
                    "status": self._get_database_status(),
                    "version": self._get_database_version(),
                    "uptime": self._get_database_uptime(),
                    "connections": self._get_database_connections(),
                    "max_connections": self._get_database_max_connections(),
                    "size": self._get_database_size(),
                    "last_backup": self._get_last_backup_time()
                },
                "background_worker": {
                    "name": "Background Job Worker",
                    "status": self._get_service_status("acas-worker"),
                    "active_jobs": self._get_active_jobs_count(),
                    "completed_jobs": self._get_completed_jobs_count(),
                    "failed_jobs": self._get_failed_jobs_count(),
                    "last_job": self._get_last_job_info()
                },
                "email_service": {
                    "name": "Email Service",
                    "status": self._get_email_service_status(),
                    "queue_size": self._get_email_queue_size(),
                    "sent_today": self._get_emails_sent_today(),
                    "failed_today": self._get_emails_failed_today(),
                    "last_sent": self._get_last_email_sent()
                }
            }
            
            return services
            
        except Exception as e:
            logger.error(f"Error getting system services: {str(e)}")
            raise
    
    def restart_service(self, service_name: str) -> Dict[str, Any]:
        """Restart system service"""
        try:
            valid_services = {
                "background_worker": "acas-worker",
                "email_service": "acas-email",
                "cache_service": "acas-cache"
            }
            
            if service_name not in valid_services:
                raise ValueError(f"Service {service_name} cannot be restarted or does not exist")
            
            system_service = valid_services[service_name]
            
            # Stop service
            stop_result = subprocess.run(
                ["systemctl", "stop", system_service],
                capture_output=True,
                text=True
            )
            
            if stop_result.returncode != 0:
                raise Exception(f"Failed to stop service: {stop_result.stderr}")
            
            # Start service
            start_result = subprocess.run(
                ["systemctl", "start", system_service],
                capture_output=True,
                text=True
            )
            
            if start_result.returncode != 0:
                raise Exception(f"Failed to start service: {start_result.stderr}")
            
            # Verify service is running
            status_result = subprocess.run(
                ["systemctl", "is-active", system_service],
                capture_output=True,
                text=True
            )
            
            if status_result.stdout.strip() != "active":
                raise Exception(f"Service failed to start properly")
            
            logger.info(f"Service {service_name} restarted successfully")
            
            return {
                "status": "success",
                "service": service_name,
                "restarted_at": datetime.now().isoformat(),
                "new_status": "running"
            }
            
        except Exception as e:
            logger.error(f"Error restarting service {service_name}: {str(e)}")
            raise
    
    # Log Management Methods
    
    def get_system_logs(
        self,
        level: Optional[str] = None,
        service: Optional[str] = None,
        start_date: Optional[datetime] = None,
        end_date: Optional[datetime] = None,
        skip: int = 0,
        limit: int = 100
    ) -> Dict[str, Any]:
        """Get system logs with filtering"""
        try:
            logs = self._read_log_files(level, service, start_date, end_date)
            
            # Apply pagination
            total_logs = len(logs)
            paginated_logs = logs[skip:skip+limit]
            
            return {
                "logs": paginated_logs,
                "total": total_logs,
                "page_size": limit,
                "offset": skip,
                "filters": {
                    "level": level,
                    "service": service,
                    "start_date": start_date.isoformat() if start_date else None,
                    "end_date": end_date.isoformat() if end_date else None
                }
            }
            
        except Exception as e:
            logger.error(f"Error getting system logs: {str(e)}")
            raise
    
    def clear_old_logs(self, days: int = 30) -> Dict[str, Any]:
        """Clear logs older than specified days"""
        try:
            cutoff_date = datetime.now() - timedelta(days=days)
            
            log_directory = Path(self.log_directory)
            if not log_directory.exists():
                return {
                    "files_removed": 0,
                    "space_freed": "0 MB",
                    "message": "Log directory does not exist"
                }
            
            files_removed = 0
            total_size = 0
            
            for log_file in log_directory.rglob("*.log*"):
                if log_file.is_file():
                    file_modified = datetime.fromtimestamp(log_file.stat().st_mtime)
                    if file_modified < cutoff_date:
                        file_size = log_file.stat().st_size
                        total_size += file_size
                        log_file.unlink()
                        files_removed += 1
            
            space_freed = self._format_bytes(total_size)
            
            logger.info(f"Log cleanup completed: {files_removed} files, {space_freed} freed")
            
            return {
                "message": f"Logs older than {days} days cleared successfully",
                "cutoff_date": cutoff_date.isoformat(),
                "files_removed": files_removed,
                "space_freed": space_freed,
                "cleared_at": datetime.now().isoformat()
            }
            
        except Exception as e:
            logger.error(f"Error clearing logs: {str(e)}")
            raise
    
    # Maintenance Methods
    
    def start_maintenance_mode(self, message: str = "System maintenance in progress") -> Dict[str, Any]:
        """Enable maintenance mode"""
        try:
            maintenance_file = "/tmp/acas_maintenance.flag"
            
            maintenance_info = {
                "enabled": True,
                "message": message,
                "started_at": datetime.now().isoformat(),
                "started_by": "admin"  # TODO: Get from current user
            }
            
            with open(maintenance_file, 'w') as f:
                json.dump(maintenance_info, f)
            
            logger.info("Maintenance mode enabled")
            
            return maintenance_info
            
        except Exception as e:
            logger.error(f"Error starting maintenance mode: {str(e)}")
            raise
    
    def stop_maintenance_mode(self) -> Dict[str, Any]:
        """Disable maintenance mode"""
        try:
            maintenance_file = "/tmp/acas_maintenance.flag"
            
            if os.path.exists(maintenance_file):
                os.unlink(maintenance_file)
            
            logger.info("Maintenance mode disabled")
            
            return {
                "enabled": False,
                "stopped_at": datetime.now().isoformat(),
                "stopped_by": "admin"  # TODO: Get from current user
            }
            
        except Exception as e:
            logger.error(f"Error stopping maintenance mode: {str(e)}")
            raise
    
    def is_maintenance_mode(self) -> bool:
        """Check if maintenance mode is enabled"""
        maintenance_file = "/tmp/acas_maintenance.flag"
        return os.path.exists(maintenance_file)
    
    # Private Helper Methods
    
    def _get_memory_info(self) -> Dict[str, Any]:
        """Get memory information"""
        memory = psutil.virtual_memory()
        swap = psutil.swap_memory()
        
        return {
            "total": memory.total,
            "available": memory.available,
            "used": memory.used,
            "percentage": memory.percent,
            "free": memory.free,
            "buffers": getattr(memory, 'buffers', 0),
            "cached": getattr(memory, 'cached', 0),
            "swap_total": swap.total,
            "swap_used": swap.used,
            "swap_free": swap.free,
            "swap_percentage": swap.percent
        }
    
    def _get_cpu_info(self) -> Dict[str, Any]:
        """Get CPU information"""
        cpu_freq = psutil.cpu_freq()
        
        return {
            "count": psutil.cpu_count(),
            "count_logical": psutil.cpu_count(logical=True),
            "usage_percent": psutil.cpu_percent(interval=1),
            "frequency": {
                "current": cpu_freq.current if cpu_freq else None,
                "min": cpu_freq.min if cpu_freq else None,
                "max": cpu_freq.max if cpu_freq else None
            } if cpu_freq else None,
            "load_average": list(os.getloadavg()) if hasattr(os, 'getloadavg') else None
        }
    
    def _get_disk_info(self) -> Dict[str, Any]:
        """Get disk information"""
        disk = psutil.disk_usage('/')
        disk_io = psutil.disk_io_counters()
        
        return {
            "total": disk.total,
            "used": disk.used,
            "free": disk.free,
            "percentage": disk.percent,
            "read_bytes": disk_io.read_bytes if disk_io else 0,
            "write_bytes": disk_io.write_bytes if disk_io else 0,
            "read_count": disk_io.read_count if disk_io else 0,
            "write_count": disk_io.write_count if disk_io else 0
        }
    
    def _get_network_info(self) -> Dict[str, Any]:
        """Get network information"""
        network_io = psutil.net_io_counters()
        
        return {
            "bytes_sent": network_io.bytes_sent,
            "bytes_recv": network_io.bytes_recv,
            "packets_sent": network_io.packets_sent,
            "packets_recv": network_io.packets_recv,
            "errors_in": network_io.errin,
            "errors_out": network_io.errout,
            "drops_in": network_io.dropin,
            "drops_out": network_io.dropout
        }
    
    def _get_application_info(self) -> Dict[str, Any]:
        """Get application information"""
        return {
            "name": "ACAS System",
            "version": "1.0.0",
            "environment": os.getenv("ENVIRONMENT", "production"),
            "database_status": "connected",  # TODO: Check actual DB status
            "last_restart": datetime.now().isoformat(),  # TODO: Get actual restart time
            "active_sessions": 0,  # TODO: Get from session store
            "maintenance_mode": self.is_maintenance_mode()
        }
    
    def _format_uptime(self, uptime: timedelta) -> str:
        """Format uptime in human readable format"""
        days = uptime.days
        hours, remainder = divmod(uptime.seconds, 3600)
        minutes, _ = divmod(remainder, 60)
        
        parts = []
        if days:
            parts.append(f"{days} day{'s' if days != 1 else ''}")
        if hours:
            parts.append(f"{hours} hour{'s' if hours != 1 else ''}")
        if minutes:
            parts.append(f"{minutes} minute{'s' if minutes != 1 else ''}")
        
        return ", ".join(parts) if parts else "less than a minute"
    
    def _format_bytes(self, bytes_value: int) -> str:
        """Format bytes in human readable format"""
        for unit in ['B', 'KB', 'MB', 'GB', 'TB']:
            if bytes_value < 1024.0:
                return f"{bytes_value:.1f} {unit}"
            bytes_value /= 1024.0
        return f"{bytes_value:.1f} PB"
    
    # Placeholder methods for service status checks
    # TODO: Implement actual service monitoring
    
    def _check_database_health(self) -> Dict[str, Any]:
        """Check database health"""
        try:
            # TODO: Implement actual database health check
            return {
                "status": "healthy",
                "response_time_ms": 15,
                "connections": 12,
                "last_check": datetime.now().isoformat()
            }
        except:
            return {
                "status": "critical",
                "error": "Database connection failed",
                "last_check": datetime.now().isoformat()
            }
    
    def _check_process_health(self) -> Dict[str, Any]:
        """Check process health"""
        process_count = len(psutil.pids())
        return {
            "status": "healthy" if process_count < 1000 else "warning",
            "process_count": process_count,
            "threshold": 1000
        }
    
    def _check_services_health(self) -> Dict[str, Any]:
        """Check services health"""
        return {
            "web_server": {"status": "running", "uptime": "active"},
            "database": {"status": "running", "uptime": "active"},
            "background_jobs": {"status": "running", "uptime": "active"},
            "email_service": {"status": "running", "uptime": "active"}
        }
    
    def _get_historical_metrics(self, hours: int) -> Dict[str, Any]:
        """Get historical performance metrics"""
        # TODO: Implement actual historical data collection
        return {
            "cpu_average": 35.2,
            "memory_average": 62.1,
            "disk_average": 45.8,
            "network_average": 1250000
        }
    
    def _get_performance_alerts(self) -> List[Dict[str, Any]]:
        """Get performance alerts"""
        # TODO: Implement actual alerting system
        return []
    
    def _validate_configuration(self, config: Dict[str, Any]) -> Dict[str, Any]:
        """Validate configuration changes"""
        # TODO: Implement comprehensive validation
        return {"valid": True, "errors": []}
    
    def _merge_config(self, current: Dict[str, Any], updates: Dict[str, Any]) -> Dict[str, Any]:
        """Merge configuration updates"""
        # Deep merge configuration
        result = current.copy()
        for key, value in updates.items():
            if key in result and isinstance(result[key], dict) and isinstance(value, dict):
                result[key].update(value)
            else:
                result[key] = value
        return result
    
    def _apply_runtime_config_changes(self, config_update: Dict[str, Any]) -> bool:
        """Apply configuration changes that don't require restart"""
        # TODO: Implement runtime configuration updates
        return False  # Return True if restart is required
    
    def _read_log_files(self, level: Optional[str], service: Optional[str], 
                       start_date: Optional[datetime], end_date: Optional[datetime]) -> List[Dict[str, Any]]:
        """Read and parse log files"""
        # TODO: Implement actual log file reading and parsing
        return [
            {
                "timestamp": datetime.now().isoformat(),
                "level": "INFO",
                "service": "web_server",
                "message": "Sample log entry",
                "details": {}
            }
        ]
    
    # Placeholder methods for service monitoring
    # TODO: Implement actual service monitoring methods
    
    def _get_service_status(self, service: str) -> str:
        return "running"
    
    def _get_service_pid(self, service: str) -> Optional[int]:
        return 1234
    
    def _get_service_uptime(self, service: str) -> str:
        return "2 days, 14 hours"
    
    def _get_service_memory(self, service: str) -> str:
        return "256 MB"
    
    def _get_service_cpu(self, service: str) -> str:
        return "2.1%"
    
    def _get_service_last_restart(self, service: str) -> str:
        return "2024-01-24T10:30:00Z"
    
    def _get_database_status(self) -> str:
        return "running"
    
    def _get_database_version(self) -> str:
        return "14.5"
    
    def _get_database_uptime(self) -> str:
        return "7 days, 3 hours"
    
    def _get_database_connections(self) -> int:
        return 15
    
    def _get_database_max_connections(self) -> int:
        return 100
    
    def _get_database_size(self) -> str:
        return "2.4 GB"
    
    def _get_last_backup_time(self) -> str:
        return "2024-01-26T02:00:00Z"
    
    def _get_active_jobs_count(self) -> int:
        return 3
    
    def _get_completed_jobs_count(self) -> int:
        return 1247
    
    def _get_failed_jobs_count(self) -> int:
        return 2
    
    def _get_last_job_info(self) -> str:
        return "Report Generation - 2024-01-26T14:45:00Z"
    
    def _get_email_service_status(self) -> str:
        return "running"
    
    def _get_email_queue_size(self) -> int:
        return 5
    
    def _get_emails_sent_today(self) -> int:
        return 127
    
    def _get_emails_failed_today(self) -> int:
        return 1
    
    def _get_last_email_sent(self) -> str:
        return "2024-01-26T14:30:00Z"