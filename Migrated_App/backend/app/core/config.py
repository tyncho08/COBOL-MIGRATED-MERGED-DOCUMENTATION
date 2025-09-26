"""
ACAS FastAPI Configuration
Core settings for the ACAS migration application
"""
from pydantic import validator
from pydantic_settings import BaseSettings
from typing import Optional
import os
from pathlib import Path

class Settings(BaseSettings):
    """Application settings"""
    
    # Application Info
    APP_NAME: str = "ACAS Migration API"
    APP_VERSION: str = "3.02"
    DEBUG: bool = False
    
    # Database Configuration
    DATABASE_URL: str = "postgresql://MartinGonella@localhost:5432/acas_db"
    DATABASE_POOL_SIZE: int = 10
    DATABASE_MAX_OVERFLOW: int = 20
    
    # Redis Configuration (for caching)
    REDIS_URL: str = "redis://localhost:6379/0"
    
    # Security
    SECRET_KEY: str = "your-secret-key-change-in-production"
    ALGORITHM: str = "HS256"
    ACCESS_TOKEN_EXPIRE_MINUTES: int = 30
    
    # CORS
    ALLOWED_HOSTS: list = ["*"]  # Change in production
    CORS_ORIGINS: list = [
        "http://localhost:3000",  # Next.js frontend
        "http://localhost:3001",  # Alternative frontend port
    ]
    
    # File Storage
    UPLOAD_DIR: Path = Path("uploads")
    MAX_UPLOAD_SIZE: int = 10 * 1024 * 1024  # 10MB
    
    # Logging
    LOG_LEVEL: str = "INFO"
    LOG_DIR: Path = Path("../logs")  # Logs directory in project root
    LOG_FILE: str = "app.log"
    ERROR_LOG_FILE: str = "error.log"
    ACCESS_LOG_FILE: str = "access.log"
    
    # Business Logic Settings
    DEFAULT_CURRENCY: str = "GBP"
    DEFAULT_VAT_RATE: float = 20.0
    DEFAULT_PAYMENT_TERMS: str = "30"
    
    # Financial Precision
    CURRENCY_DECIMAL_PLACES: int = 2
    QUANTITY_DECIMAL_PLACES: int = 3
    RATE_DECIMAL_PLACES: int = 4
    
    # System Limits
    MAX_INVOICE_LINES: int = 999
    MAX_CUSTOMER_CREDIT_LIMIT: float = 999999.99
    MAX_STOCK_QUANTITY: float = 999999999.999
    
    # Date Formats (matching COBOL YYYYMMDD format)
    DATE_FORMAT: str = "%Y%m%d"
    DISPLAY_DATE_FORMAT: str = "%d/%m/%Y"
    
    # Batch Processing
    BATCH_SIZE: int = 1000
    MAX_CONCURRENT_JOBS: int = 5
    
    # API Configuration
    API_V1_STR: str = "/api/v1"
    DOCS_URL: str = "/docs"
    REDOC_URL: str = "/redoc"
    OPENAPI_URL: str = "/openapi.json"
    
    # Email Settings (for notifications)
    SMTP_HOST: Optional[str] = None
    SMTP_PORT: int = 587
    SMTP_USER: Optional[str] = None
    SMTP_PASSWORD: Optional[str] = None
    
    @validator("DATABASE_URL", pre=True)
    def assemble_db_connection(cls, v: Optional[str], values: dict) -> str:
        """Validate and construct database URL"""
        if isinstance(v, str):
            return v
        # Fallback construction if needed
        return f"postgresql://acas_user:acas_password@localhost:5432/acas_db"
    
    @validator("UPLOAD_DIR", pre=True)
    def create_upload_dir(cls, v):
        """Ensure upload directory exists"""
        path = Path(v) if isinstance(v, str) else v
        path.mkdir(exist_ok=True)
        return path
    
    @validator("LOG_DIR", pre=True)
    def create_log_dir(cls, v):
        """Ensure logs directory exists"""
        path = Path(v) if isinstance(v, str) else v
        path.mkdir(exist_ok=True, parents=True)
        return path
    
    class Config:
        env_file = ".env"
        case_sensitive = True

# Global settings instance
settings = Settings()

# Database connection string for SQLAlchemy
DATABASE_URL = settings.DATABASE_URL

# Test database URL (for testing)
TEST_DATABASE_URL = DATABASE_URL.replace("/acas_db", "/test_acas_db")