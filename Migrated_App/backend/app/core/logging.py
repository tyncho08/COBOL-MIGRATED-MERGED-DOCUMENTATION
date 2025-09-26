"""
ACAS Logging Configuration
Centralized logging setup for the ACAS application
"""
import logging
import logging.handlers
import sys
from pathlib import Path
from typing import Optional
from .config import settings

def setup_logging(
    log_level: Optional[str] = None,
    log_to_file: bool = True,
    log_to_console: bool = True
) -> logging.Logger:
    """
    Configure logging for the ACAS application
    
    Args:
        log_level: Logging level (DEBUG, INFO, WARNING, ERROR, CRITICAL)
        log_to_file: Whether to log to files
        log_to_console: Whether to log to console
    
    Returns:
        Configured logger instance
    """
    # Determine log level
    level = getattr(logging, (log_level or settings.LOG_LEVEL).upper(), logging.INFO)
    
    # Create main logger
    logger = logging.getLogger("acas")
    logger.setLevel(level)
    
    # Clear existing handlers
    logger.handlers.clear()
    
    # Define log format
    formatter = logging.Formatter(
        fmt="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
        datefmt="%Y-%m-%d %H:%M:%S"
    )
    
    detailed_formatter = logging.Formatter(
        fmt="%(asctime)s - %(name)s - %(levelname)s - %(filename)s:%(lineno)d - %(funcName)s() - %(message)s",
        datefmt="%Y-%m-%d %H:%M:%S"
    )
    
    # Console handler
    if log_to_console:
        console_handler = logging.StreamHandler(sys.stdout)
        console_handler.setLevel(level)
        console_handler.setFormatter(formatter)
        logger.addHandler(console_handler)
    
    # File handlers
    if log_to_file:
        # Ensure log directory exists
        log_dir = settings.LOG_DIR
        log_dir.mkdir(exist_ok=True, parents=True)
        
        # Main application log (with rotation)
        app_log_file = log_dir / settings.LOG_FILE
        app_handler = logging.handlers.RotatingFileHandler(
            app_log_file,
            maxBytes=10 * 1024 * 1024,  # 10MB
            backupCount=5,
            encoding='utf-8'
        )
        app_handler.setLevel(level)
        app_handler.setFormatter(detailed_formatter)
        logger.addHandler(app_handler)
        
        # Error log (only errors and above)
        error_log_file = log_dir / settings.ERROR_LOG_FILE
        error_handler = logging.handlers.RotatingFileHandler(
            error_log_file,
            maxBytes=5 * 1024 * 1024,  # 5MB
            backupCount=3,
            encoding='utf-8'
        )
        error_handler.setLevel(logging.ERROR)
        error_handler.setFormatter(detailed_formatter)
        logger.addHandler(error_handler)
    
    # Set up specific loggers for different modules
    setup_module_loggers(level, formatter, detailed_formatter, log_dir if log_to_file else None)
    
    return logger

def setup_module_loggers(
    level: int,
    console_formatter: logging.Formatter,
    file_formatter: logging.Formatter,
    log_dir: Optional[Path] = None
):
    """Setup loggers for specific modules"""
    
    # Database logger
    db_logger = logging.getLogger("acas.database")
    db_logger.setLevel(level)
    if log_dir:
        db_handler = logging.handlers.RotatingFileHandler(
            log_dir / "database.log",
            maxBytes=5 * 1024 * 1024,
            backupCount=3
        )
        db_handler.setFormatter(file_formatter)
        db_logger.addHandler(db_handler)
    
    # API logger
    api_logger = logging.getLogger("acas.api")
    api_logger.setLevel(level)
    if log_dir:
        api_handler = logging.handlers.RotatingFileHandler(
            log_dir / "api.log",
            maxBytes=10 * 1024 * 1024,
            backupCount=5
        )
        api_handler.setFormatter(file_formatter)
        api_logger.addHandler(api_handler)
    
    # Business logic logger
    business_logger = logging.getLogger("acas.business")
    business_logger.setLevel(level)
    if log_dir:
        business_handler = logging.handlers.RotatingFileHandler(
            log_dir / "business.log",
            maxBytes=5 * 1024 * 1024,
            backupCount=3
        )
        business_handler.setFormatter(file_formatter)
        business_logger.addHandler(business_handler)
    
    # Security logger (always file-based for audit trail)
    security_logger = logging.getLogger("acas.security")
    security_logger.setLevel(logging.INFO)  # Always log security events
    if log_dir:
        security_handler = logging.handlers.RotatingFileHandler(
            log_dir / "security.log",
            maxBytes=10 * 1024 * 1024,
            backupCount=10  # Keep more security logs
        )
        security_handler.setFormatter(file_formatter)
        security_logger.addHandler(security_handler)

def get_logger(name: str) -> logging.Logger:
    """Get a logger with the given name"""
    return logging.getLogger(f"acas.{name}")

# FastAPI access logger setup
def setup_uvicorn_logging():
    """Configure uvicorn access logging"""
    access_logger = logging.getLogger("uvicorn.access")
    
    if settings.LOG_DIR:
        log_dir = settings.LOG_DIR
        log_dir.mkdir(exist_ok=True, parents=True)
        
        # Access log handler
        access_handler = logging.handlers.RotatingFileHandler(
            log_dir / settings.ACCESS_LOG_FILE,
            maxBytes=10 * 1024 * 1024,
            backupCount=5
        )
        access_formatter = logging.Formatter(
            '%(asctime)s - %(levelname)s - %(message)s'
        )
        access_handler.setFormatter(access_formatter)
        access_logger.addHandler(access_handler)

# Initialize logging
main_logger = setup_logging()

# Export commonly used loggers
__all__ = [
    'setup_logging',
    'get_logger',
    'setup_uvicorn_logging',
    'main_logger'
]