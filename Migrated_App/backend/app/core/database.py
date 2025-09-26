"""
ACAS Database Configuration
SQLAlchemy setup for PostgreSQL connection
"""
from sqlalchemy import create_engine, MetaData, text
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import sessionmaker
from sqlalchemy.pool import QueuePool
from typing import Generator
import logging

from .config import settings

logger = logging.getLogger(__name__)

# Create SQLAlchemy engine with connection pooling
engine = create_engine(
    settings.DATABASE_URL,
    poolclass=QueuePool,
    pool_size=settings.DATABASE_POOL_SIZE,
    max_overflow=settings.DATABASE_MAX_OVERFLOW,
    pool_recycle=3600,  # Recycle connections after 1 hour
    pool_pre_ping=True,  # Validate connections before use
    echo=settings.DEBUG,  # Log SQL queries in debug mode
)

# Create SessionLocal class for database sessions
SessionLocal = sessionmaker(
    autocommit=False,
    autoflush=False,
    bind=engine
)

# Create base class for models
Base = declarative_base()

# Metadata with naming convention for constraints
Base.metadata = MetaData(naming_convention={
    "ix": "ix_%(column_0_label)s",
    "uq": "uq_%(table_name)s_%(column_0_name)s",
    "ck": "ck_%(table_name)s_%(constraint_name)s",
    "fk": "fk_%(table_name)s_%(column_0_name)s_%(referred_table_name)s",
    "pk": "pk_%(table_name)s"
})

def get_db() -> Generator:
    """
    Dependency function to get database session
    
    Yields:
        Database session
    """
    db = SessionLocal()
    try:
        yield db
    except Exception as e:
        logger.error(f"Database session error: {e}")
        db.rollback()
        raise
    finally:
        db.close()

def init_db():
    """
    Initialize database tables
    
    This function creates all tables defined in models
    """
    try:
        # Import all models to ensure they are registered with Base
        from app.models import (
            system, customer, supplier, stock, 
            gl_accounts, payments, sales, auth, audit, gl, irs, warehouse
        )
        
        # Create all tables
        Base.metadata.create_all(bind=engine)
        logger.info("Database tables created successfully")
        
    except Exception as e:
        logger.error(f"Error initializing database: {e}")
        raise

def check_db_connection() -> bool:
    """
    Check if database connection is working
    
    Returns:
        True if connection is successful, False otherwise
    """
    try:
        with engine.connect() as connection:
            connection.execute(text("SELECT 1"))
        return True
    except Exception as e:
        logger.error(f"Database connection failed: {e}")
        return False

class DatabaseManager:
    """Database operations manager"""
    
    @staticmethod
    def get_session():
        """Get a database session"""
        return SessionLocal()
    
    @staticmethod
    def close_session(db_session):
        """Close database session safely"""
        try:
            db_session.close()
        except Exception as e:
            logger.error(f"Error closing database session: {e}")
    
    @staticmethod
    def execute_raw_sql(sql: str, params: dict = None):
        """Execute raw SQL query"""
        try:
            with engine.connect() as connection:
                result = connection.execute(sql, params or {})
                return result
        except Exception as e:
            logger.error(f"Error executing raw SQL: {e}")
            raise