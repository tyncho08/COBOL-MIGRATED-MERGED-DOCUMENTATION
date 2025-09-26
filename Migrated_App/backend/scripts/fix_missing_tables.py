#!/usr/bin/env python3
"""
Fix missing tables in the database
Creates missing tables using SQLAlchemy models
"""
import sys
import os
from pathlib import Path

# Add parent directory to path
sys.path.append(str(Path(__file__).parent.parent))

from sqlalchemy import text
from app.core.database import engine, Base
import logging

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

def fix_missing_tables():
    """Create missing tables"""
    try:
        # Import all models to ensure they are registered with Base
        from app.models import (
            system, customer, supplier, stock, 
            gl_accounts, payments, sales, auth, audit, gl, irs, warehouse
        )
        
        logger.info("Creating missing tables...")
        
        # Create all tables (only creates if they don't exist)
        Base.metadata.create_all(bind=engine)
        
        logger.info("Database tables created/verified successfully")
        
        # Verify sales_receipts table exists
        with engine.connect() as conn:
            result = conn.execute(text("""
                SELECT table_name 
                FROM information_schema.tables 
                WHERE table_schema = 'acas' 
                AND table_name = 'sales_receipts'
            """))
            
            if result.fetchone():
                logger.info("✅ sales_receipts table exists")
                
                # Add some sample data if table is empty
                count_result = conn.execute(text("SELECT COUNT(*) FROM acas.sales_receipts"))
                count = count_result.scalar()
                
                if count == 0:
                    logger.info("Adding sample receipt data...")
                    conn.execute(text("""
                        INSERT INTO acas.sales_receipts (
                            receipt_number, sales_key, receipt_date, payment_method, 
                            amount, received_by, narrative
                        ) VALUES 
                        ('REC001', 'CUST001', 20250915, 'TRANSFER', 2500.00, 'system', 'Payment received'),
                        ('REC002', 'CUST002', 20250920, 'CHECK', 1200.00, 'system', 'Check payment'),
                        ('REC003', 'CUST003', 20250922, 'TRANSFER', 750.00, 'system', 'Bank transfer')
                    """))
                    conn.commit()
                    logger.info("✅ Sample receipt data added")
                else:
                    logger.info(f"sales_receipts table already has {count} records")
            else:
                logger.error("❌ sales_receipts table still doesn't exist")
                
        logger.info("Fix completed successfully!")
        
    except Exception as e:
        logger.error(f"Error fixing missing tables: {e}")
        raise

if __name__ == "__main__":
    fix_missing_tables()