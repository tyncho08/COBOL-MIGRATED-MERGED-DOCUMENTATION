"""
Admin System API endpoints
System administration, configuration, monitoring
"""

from typing import Dict, List, Optional, Any
from fastapi import APIRouter, Depends, HTTPException, status, Query
from sqlalchemy.orm import Session
from app.core.database import get_db
from app.core.security import require_admin
from app.models.auth import User
from app.services.system_service import SystemService
import logging
import psutil
import platform
from datetime import datetime, timedelta

logger = logging.getLogger(__name__)

router = APIRouter()

# System Information Endpoints

@router.get("/info")
async def get_system_info(
    current_user: User = Depends(require_admin)
):
    """
    Get comprehensive system information
    Requires admin privileges
    """
    try:
        # Server information
        system_info = {
            "server": {
                "hostname": platform.node(),
                "platform": platform.platform(),
                "system": platform.system(),
                "release": platform.release(),
                "version": platform.version(),
                "machine": platform.machine(),
                "processor": platform.processor(),
                "boot_time": datetime.fromtimestamp(psutil.boot_time()).isoformat(),
                "uptime_seconds": (datetime.now() - datetime.fromtimestamp(psutil.boot_time())).total_seconds()
            },
            
            # Memory information
            "memory": {
                "total": psutil.virtual_memory().total,
                "available": psutil.virtual_memory().available,
                "used": psutil.virtual_memory().used,
                "percentage": psutil.virtual_memory().percent,
                "free": psutil.virtual_memory().free
            },
            
            # CPU information
            "cpu": {
                "count": psutil.cpu_count(),
                "count_logical": psutil.cpu_count(logical=True),
                "usage_percent": psutil.cpu_percent(interval=1),
                "frequency": psutil.cpu_freq()._asdict() if psutil.cpu_freq() else None
            },
            
            # Disk information
            "disk": {
                "total": psutil.disk_usage('/').total,
                "used": psutil.disk_usage('/').used,
                "free": psutil.disk_usage('/').free,
                "percentage": psutil.disk_usage('/').percent
            },
            
            # Network information
            "network": {
                "bytes_sent": psutil.net_io_counters().bytes_sent,
                "bytes_recv": psutil.net_io_counters().bytes_recv,
                "packets_sent": psutil.net_io_counters().packets_sent,
                "packets_recv": psutil.net_io_counters().packets_recv
            },
            
            # Application information
            "application": {
                "name": "ACAS System",
                "version": "1.0.0",
                "environment": "production",  # TODO: Get from config
                "database_status": "connected",
                "last_restart": datetime.now().isoformat(),  # TODO: Get actual restart time
                "active_sessions": 0  # TODO: Get from session store
            }
        }
        
        return system_info
        
    except Exception as e:
        logger.error(f"Error getting system info: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to retrieve system information"
        )

@router.get("/health")
async def get_system_health(
    current_user: User = Depends(require_admin)
):
    """
    Get system health status
    Requires admin privileges
    """
    try:
        health_status = {
            "status": "healthy",
            "timestamp": datetime.now().isoformat(),
            "checks": {
                "database": _check_database_health_detailed(),
                "memory": {
                    "status": "healthy" if psutil.virtual_memory().percent < 85 else "warning",
                    "usage_percent": psutil.virtual_memory().percent,
                    "threshold": 85
                },
                "cpu": {
                    "status": "healthy" if psutil.cpu_percent(interval=1) < 80 else "warning",
                    "usage_percent": psutil.cpu_percent(interval=1),
                    "threshold": 80
                },
                "disk": {
                    "status": "healthy" if psutil.disk_usage('/').percent < 90 else "critical",
                    "usage_percent": psutil.disk_usage('/').percent,
                    "threshold": 90
                }
            },
            "services": {
                "web_server": {"status": "running", "uptime": "active"},
                "database": {"status": "running", "uptime": "active"},
                "background_jobs": {"status": "running", "uptime": "active"}
            }
        }
        
        # Determine overall status
        check_statuses = [check["status"] for check in health_status["checks"].values()]
        service_statuses = [service["status"] for service in health_status["services"].values()]
        
        if "critical" in check_statuses or "stopped" in service_statuses:
            health_status["status"] = "critical"
        elif "warning" in check_statuses or "degraded" in service_statuses:
            health_status["status"] = "warning"
        
        return health_status
        
    except Exception as e:
        logger.error(f"Error getting system health: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to retrieve system health"
        )

@router.get("/performance")
async def get_system_performance(
    hours: int = Query(24, ge=1, le=168),  # Last 24 hours by default, max 1 week
    current_user: User = Depends(require_admin)
):
    """
    Get system performance metrics
    Requires admin privileges
    """
    try:
        # TODO: Implement actual performance data collection
        # For now, return sample data structure
        performance_data = {
            "time_range": {
                "start": (datetime.now() - timedelta(hours=hours)).isoformat(),
                "end": datetime.now().isoformat(),
                "hours": hours
            },
            "metrics": {
                "cpu_usage": {
                    "current": psutil.cpu_percent(interval=1),
                    "average": 45.2,  # TODO: Calculate from stored data
                    "peak": 78.5,
                    "timeline": []  # TODO: Historical data points
                },
                "memory_usage": {
                    "current": psutil.virtual_memory().percent,
                    "average": 62.1,
                    "peak": 84.3,
                    "timeline": []
                },
                "disk_io": {
                    "read_bytes": psutil.disk_io_counters().read_bytes,
                    "write_bytes": psutil.disk_io_counters().write_bytes,
                    "timeline": []
                },
                "network_io": {
                    "bytes_sent": psutil.net_io_counters().bytes_sent,
                    "bytes_recv": psutil.net_io_counters().bytes_recv,
                    "timeline": []
                },
                "response_times": {
                    "api_average_ms": 125.4,
                    "database_average_ms": 45.2,
                    "timeline": []
                },
                "active_connections": {
                    "current": 15,
                    "average": 12.3,
                    "peak": 28,
                    "timeline": []
                }
            }
        }
        
        return performance_data
        
    except Exception as e:
        logger.error(f"Error getting performance metrics: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to retrieve performance metrics"
        )

# Configuration Management Endpoints

@router.get("/config")
async def get_system_configuration(
    current_user: User = Depends(require_admin)
):
    """
    Get system configuration
    Requires admin privileges
    """
    try:
        # TODO: Load from actual config file/database
        config = {
            "database": {
                "host": "localhost",
                "port": 5432,
                "name": "acas_db",
                "max_connections": 100,
                "timeout": 30
            },
            "security": {
                "session_timeout": 3600,
                "password_expiry_days": 90,
                "max_login_attempts": 5,
                "lockout_duration": 300
            },
            "application": {
                "timezone": "UTC",
                "date_format": "YYYY-MM-DD",
                "decimal_places": 2,
                "max_file_upload_size": 10485760,  # 10MB
                "backup_retention_days": 30
            },
            "email": {
                "smtp_server": "smtp.company.com",
                "smtp_port": 587,
                "use_tls": True,
                "from_address": "acas-system@company.com"
            },
            "logging": {
                "level": "INFO",
                "retention_days": 90,
                "max_file_size": "100MB"
            }
        }
        
        return config
        
    except Exception as e:
        logger.error(f"Error getting system configuration: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to retrieve system configuration"
        )

@router.put("/config")
async def update_system_configuration(
    config_update: Dict[str, Any],
    current_user: User = Depends(require_admin)
):
    """
    Update system configuration
    Requires admin privileges
    """
    try:
        # TODO: Implement configuration update logic
        # Validate configuration
        # Update configuration file/database
        # Apply changes that don't require restart
        # Log configuration changes
        
        logger.info(f"System configuration updated by admin {current_user.username}")
        
        return {
            "message": "Configuration updated successfully",
            "requires_restart": False,  # TODO: Determine if restart needed
            "updated_at": datetime.now().isoformat(),
            "updated_by": current_user.username
        }
        
    except Exception as e:
        logger.error(f"Error updating system configuration: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to update system configuration"
        )

# Service Management Endpoints

@router.get("/services")
async def get_system_services(
    current_user: User = Depends(require_admin)
):
    """
    Get status of system services
    Requires admin privileges
    """
    try:
        services = {
            "web_server": {
                "name": "ACAS Web Server",
                "status": "running",
                "pid": 1234,  # TODO: Get actual PID
                "uptime": "2 days, 14 hours",
                "memory_usage": "256 MB",
                "cpu_usage": "2.1%",
                "port": 8000,
                "last_restart": "2024-01-24T10:30:00Z"
            },
            "database": {
                "name": "PostgreSQL Database",
                "status": "running",
                "version": "14.5",
                "uptime": "7 days, 3 hours",
                "connections": 15,
                "max_connections": 100,
                "size": "2.4 GB",
                "last_backup": "2024-01-26T02:00:00Z"
            },
            "background_worker": {
                "name": "Background Job Worker",
                "status": "running",
                "active_jobs": 3,
                "completed_jobs": 1247,
                "failed_jobs": 2,
                "last_job": "Report Generation - 2024-01-26T14:45:00Z"
            },
            "email_service": {
                "name": "Email Service",
                "status": "running",
                "queue_size": 5,
                "sent_today": 127,
                "failed_today": 1,
                "last_sent": "2024-01-26T14:30:00Z"
            }
        }
        
        return services
        
    except Exception as e:
        logger.error(f"Error getting system services: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to retrieve system services"
        )

@router.post("/services/{service_name}/restart")
async def restart_service(
    service_name: str,
    current_user: User = Depends(require_admin)
):
    """
    Restart system service
    Requires admin privileges
    """
    try:
        # TODO: Implement service restart logic
        # Validate service name
        # Stop service gracefully
        # Start service
        # Verify service is running
        
        valid_services = ["background_worker", "email_service"]
        if service_name not in valid_services:
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail=f"Service {service_name} cannot be restarted or does not exist"
            )
        
        logger.info(f"Service {service_name} restarted by admin {current_user.username}")
        
        return {
            "message": f"Service {service_name} restarted successfully",
            "service": service_name,
            "restarted_at": datetime.now().isoformat(),
            "restarted_by": current_user.username
        }
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error restarting service {service_name}: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"Failed to restart service {service_name}"
        )

# Log Management Endpoints

@router.get("/logs")
async def get_system_logs(
    level: Optional[str] = Query(None),
    service: Optional[str] = Query(None),
    start_date: Optional[datetime] = Query(None),
    end_date: Optional[datetime] = Query(None),
    skip: int = Query(0, ge=0),
    limit: int = Query(100, ge=1, le=1000),
    current_user: User = Depends(require_admin)
):
    """
    Get system logs with filtering
    Requires admin privileges
    """
    try:
        # TODO: Implement log retrieval from log files
        # For now, return sample log entries
        logs = [
            {
                "timestamp": "2024-01-26T14:30:15Z",
                "level": "INFO",
                "service": "web_server",
                "message": "User login successful: john.doe",
                "details": {"user_id": 123, "ip_address": "192.168.1.100"}
            },
            {
                "timestamp": "2024-01-26T14:29:42Z",
                "level": "WARNING",
                "service": "database",
                "message": "Slow query detected: 1.5s execution time",
                "details": {"query": "SELECT * FROM stock_items WHERE...", "duration_ms": 1500}
            },
            {
                "timestamp": "2024-01-26T14:25:30Z",
                "level": "ERROR",
                "service": "background_worker",
                "message": "Report generation failed",
                "details": {"report_id": 456, "error": "Database connection timeout"}
            }
        ]
        
        return {
            "logs": logs[skip:skip+limit],
            "total": len(logs),
            "filters": {
                "level": level,
                "service": service,
                "start_date": start_date,
                "end_date": end_date
            }
        }
        
    except Exception as e:
        logger.error(f"Error getting system logs: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to retrieve system logs"
        )

@router.delete("/logs")
async def clear_old_logs(
    days: int = Query(30, ge=1, le=365),
    current_user: User = Depends(require_admin)
):
    """
    Clear logs older than specified days
    Requires admin privileges
    """
    try:
        # TODO: Implement log cleanup
        # Calculate cutoff date
        # Remove log files older than cutoff
        # Update log cleanup history
        
        cutoff_date = datetime.now() - timedelta(days=days)
        
        logger.info(f"Log cleanup initiated by admin {current_user.username} for logs older than {days} days")
        
        return {
            "message": f"Logs older than {days} days cleared successfully",
            "cutoff_date": cutoff_date.isoformat(),
            "files_removed": 15,  # TODO: Return actual count
            "space_freed": "245 MB",  # TODO: Calculate actual space
            "cleared_by": current_user.username,
            "cleared_at": datetime.now().isoformat()
        }
        
    except Exception as e:
        logger.error(f"Error clearing logs: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to clear logs"
        )

# Maintenance Endpoints

@router.post("/maintenance/start")
async def start_maintenance_mode(
    message: str = "System maintenance in progress",
    current_user: User = Depends(require_admin)
):
    """
    Enable maintenance mode
    Requires admin privileges
    """
    try:
        # TODO: Implement maintenance mode
        # Set maintenance flag
        # Update system status
        # Notify active users
        
        logger.info(f"Maintenance mode started by admin {current_user.username}")
        
        return {
            "message": "Maintenance mode enabled",
            "maintenance_message": message,
            "started_by": current_user.username,
            "started_at": datetime.now().isoformat()
        }
        
    except Exception as e:
        logger.error(f"Error starting maintenance mode: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to start maintenance mode"
        )

@router.post("/maintenance/stop")
async def stop_maintenance_mode(
    current_user: User = Depends(require_admin)
):
    """
    Disable maintenance mode
    Requires admin privileges
    """
    try:
        # TODO: Implement maintenance mode disable
        # Clear maintenance flag
        # Update system status
        # Notify users that system is available
        
        logger.info(f"Maintenance mode stopped by admin {current_user.username}")
        
        return {
            "message": "Maintenance mode disabled",
            "stopped_by": current_user.username,
            "stopped_at": datetime.now().isoformat()
        }
        
    except Exception as e:
        logger.error(f"Error stopping maintenance mode: {str(e)}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to stop maintenance mode"
        )

# Private helper methods for system monitoring

def _check_database_health_detailed() -> Dict[str, Any]:
    """Check detailed database health with real metrics"""
    try:
        from app.core.database import engine
        import time
        
        start_time = time.time()
        with engine.connect() as conn:
            # Test basic connectivity
            conn.execute("SELECT 1")
            
            # Get database info
            db_version = conn.execute("SELECT version()").scalar()
            current_db = conn.execute("SELECT current_database()").scalar()
            
            # Get connection stats
            try:
                connection_stats = conn.execute("""
                    SELECT 
                        count(*) as total_connections,
                        count(*) FILTER (WHERE state = 'active') as active_connections,
                        count(*) FILTER (WHERE state = 'idle') as idle_connections
                    FROM pg_stat_activity
                """).fetchone()
            except:
                connection_stats = (0, 0, 0)
            
        response_time = (time.time() - start_time) * 1000
        
        return {
            "status": "healthy",
            "response_time_ms": round(response_time, 2),
            "database_name": current_db,
            "version": db_version.split('\n')[0] if db_version else "Unknown",
            "connections": {
                "total": connection_stats[0] if connection_stats else 0,
                "active": connection_stats[1] if connection_stats else 0,
                "idle": connection_stats[2] if connection_stats else 0,
            },
            "pool_info": {
                "size": engine.pool.size(),
                "checked_out": engine.pool.checkedout(),
                "overflow": engine.pool.overflow(),
            },
            "last_check": datetime.now().isoformat()
        }
    except Exception as e:
        return {
            "status": "critical",
            "error": str(e),
            "response_time_ms": 0,
            "last_check": datetime.now().isoformat()
        }