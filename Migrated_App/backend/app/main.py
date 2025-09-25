"""
ACAS FastAPI Main Application
Entry point for the ACAS migration REST API
"""
from fastapi import FastAPI, HTTPException, Depends
from fastapi.middleware.cors import CORSMiddleware
from fastapi.middleware.trustedhost import TrustedHostMiddleware
from fastapi.responses import JSONResponse
import logging
import sys
from pathlib import Path

# Add app directory to Python path
sys.path.append(str(Path(__file__).parent))

from core.config import settings
from core.database import check_db_connection, init_db
from datetime import datetime

# Configure logging
logging.basicConfig(
    level=getattr(logging, settings.LOG_LEVEL),
    format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
    handlers=[
        logging.StreamHandler(),
        logging.FileHandler(settings.LOG_FILE) if settings.LOG_FILE else logging.NullHandler()
    ]
)

logger = logging.getLogger(__name__)

# Create FastAPI application
app = FastAPI(
    title=settings.APP_NAME,
    version=settings.APP_VERSION,
    description="""
    ## ACAS (Applewood Computers Accounting System) Migration API

    Complete ERP system migration from legacy COBOL to modern web application.
    
    ### Key Features:
    - **Sales Ledger**: Customer management, invoicing, payments
    - **Purchase Ledger**: Supplier management, purchase orders, payments  
    - **Stock Control**: Inventory management with multiple costing methods
    - **General Ledger**: Complete double-entry accounting
    - **Financial Reporting**: Trial balance, P&L, Balance Sheet
    - **Business Logic**: Exact replication of 49 years of COBOL logic
    
    ### Business Modules:
    - Customer Management with credit control
    - Supplier Management with payment terms
    - Stock Management with FIFO/LIFO/Average costing
    - Invoice Processing with tax calculations
    - Payment Processing with allocation
    - Financial Reporting with drill-down capability
    
    ### Architecture:
    - **Backend**: FastAPI + SQLAlchemy + PostgreSQL
    - **Frontend**: Next.js 14 + TypeScript + Tailwind CSS
    - **Database**: PostgreSQL 15+ with ACID compliance
    - **Business Logic**: Exact COBOL calculations preserved
    """,
    docs_url=settings.DOCS_URL,
    redoc_url=settings.REDOC_URL,
    openapi_url=settings.OPENAPI_URL,
)

# Add CORS middleware
app.add_middleware(
    CORSMiddleware,
    allow_origins=settings.CORS_ORIGINS,
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Add trusted host middleware for production
if not settings.DEBUG:
    app.add_middleware(
        TrustedHostMiddleware,
        allowed_hosts=settings.ALLOWED_HOSTS
    )

# Health check endpoint
@app.get("/health", tags=["System"])
async def health_check():
    """
    Health check endpoint for monitoring and load balancers
    
    Returns system status and database connectivity
    """
    try:
        db_status = check_db_connection()
        
        return {
            "status": "healthy" if db_status else "degraded",
            "version": settings.APP_VERSION,
            "database": "connected" if db_status else "disconnected",
            "debug": settings.DEBUG
        }
    except Exception as e:
        logger.error(f"Health check failed: {e}")
        raise HTTPException(status_code=503, detail="Service unavailable")

# System information endpoint
@app.get("/info", tags=["System"])
async def system_info():
    """
    System information endpoint
    
    Returns application configuration and build information
    """
    return {
        "application": settings.APP_NAME,
        "version": settings.APP_VERSION,
        "description": "ACAS Legacy COBOL to Modern Web Application Migration",
        "api_version": "v1",
        "docs_url": settings.DOCS_URL,
        "features": [
            "Sales Ledger Management",
            "Purchase Ledger Management", 
            "Stock Control & Inventory",
            "General Ledger & Accounting",
            "Financial Reporting",
            "Business Logic Preservation"
        ],
        "business_modules": {
            "sales_ledger": "Customer management, invoicing, credit control",
            "purchase_ledger": "Supplier management, purchase orders, payments",
            "stock_control": "Inventory with FIFO/LIFO/Average costing",
            "general_ledger": "Double-entry accounting, financial statements",
            "reporting": "Trial balance, P&L, Balance Sheet, aged analysis"
        }
    }

# Application startup event
@app.on_event("startup")
async def startup_event():
    """
    Application startup tasks
    
    Initialize database connections and verify system health
    """
    logger.info(f"Starting {settings.APP_NAME} v{settings.APP_VERSION}")
    
    try:
        # Check database connection
        if not check_db_connection():
            logger.error("Failed to connect to database on startup")
            raise Exception("Database connection failed")
        
        logger.info("Database connection established")
        
        # Initialize database tables
        init_db()
        
        logger.info("Application startup completed successfully")
        
    except Exception as e:
        logger.error(f"Startup failed: {e}")
        sys.exit(1)

# Application shutdown event
@app.on_event("shutdown")
async def shutdown_event():
    """
    Application shutdown tasks
    
    Clean up resources and close connections
    """
    logger.info("Shutting down application")

# Global exception handler
@app.exception_handler(Exception)
async def global_exception_handler(request, exc):
    """
    Global exception handler for unhandled errors
    
    Args:
        request: FastAPI request object
        exc: Exception that occurred
        
    Returns:
        JSON error response
    """
    logger.error(f"Unhandled exception: {exc}", exc_info=True)
    
    return JSONResponse(
        status_code=500,
        content={
            "error": "Internal server error",
            "detail": str(exc) if settings.DEBUG else "An unexpected error occurred",
            "type": "server_error"
        }
    )

# Include API routers
from api.v1 import auth

app.include_router(
    auth.router,
    prefix=f"{settings.API_V1_STR}",
    tags=["Authentication"]
)

# Additional routers to be implemented
"""
from api.v1 import customers, suppliers, stock, invoices, reports

app.include_router(
    customers.router,
    prefix=f"{settings.API_V1_STR}/customers",
    tags=["Customers"]
)

app.include_router(
    suppliers.router,
    prefix=f"{settings.API_V1_STR}/suppliers",
    tags=["Suppliers"]
)

app.include_router(
    stock.router,
    prefix=f"{settings.API_V1_STR}/stock",
    tags=["Stock Control"]
)

app.include_router(
    invoices.router,
    prefix=f"{settings.API_V1_STR}/invoices",
    tags=["Invoices"]
)

app.include_router(
    reports.router,
    prefix=f"{settings.API_V1_STR}/reports",
    tags=["Reports"]
)
"""

if __name__ == "__main__":
    import uvicorn
    
    uvicorn.run(
        "main:app",
        host="0.0.0.0",
        port=8000,
        reload=settings.DEBUG,
        log_level=settings.LOG_LEVEL.lower()
    )