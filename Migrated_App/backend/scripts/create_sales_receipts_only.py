#!/usr/bin/env python3
"""
Create only the sales_receipts table to fix payment issues
"""
import sys
import os
from pathlib import Path

# Add parent directory to path
sys.path.append(str(Path(__file__).parent.parent))

from sqlalchemy import text
from app.core.database import engine
import logging

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

def create_sales_receipts_table():
    """Create sales_receipts table directly"""
    try:
        logger.info("Creating sales_receipts table...")
        
        with engine.connect() as conn:
            # Check if table already exists
            result = conn.execute(text("""
                SELECT table_name 
                FROM information_schema.tables 
                WHERE table_schema = 'acas' 
                AND table_name = 'sales_receipts'
            """))
            
            if result.fetchone():
                logger.info("Table sales_receipts already exists")
            else:
                # Create the table
                conn.execute(text("""
                    CREATE TABLE acas.sales_receipts (
                        receipt_id SERIAL PRIMARY KEY,
                        receipt_number VARCHAR(20) UNIQUE NOT NULL,
                        sales_key VARCHAR(10) NOT NULL,
                        receipt_date INTEGER NOT NULL,
                        payment_method VARCHAR(10) NOT NULL,
                        amount DECIMAL(12,2) NOT NULL,
                        bank_account VARCHAR(20),
                        check_number VARCHAR(20),
                        check_date INTEGER,
                        status CHAR(1) DEFAULT 'U',
                        currency VARCHAR(3) DEFAULT 'USD',
                        exchange_rate DECIMAL(8,4) DEFAULT 1.0000,
                        customer_ref VARCHAR(30),
                        narrative VARCHAR(100),
                        received_by VARCHAR(30) NOT NULL,
                        created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
                        updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
                    );
                """))
                
                # Create indexes
                conn.execute(text("""
                    CREATE INDEX idx_sales_receipts_customer ON acas.sales_receipts(sales_key);
                """))
                conn.execute(text("""
                    CREATE INDEX idx_sales_receipts_date ON acas.sales_receipts(receipt_date);
                """))
                conn.execute(text("""
                    CREATE INDEX idx_sales_receipts_status ON acas.sales_receipts(status);
                """))
                
                logger.info("✅ sales_receipts table created successfully")
                
            # Add sample data if table is empty
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
                logger.info("✅ Sample receipt data added")
            else:
                logger.info(f"sales_receipts table already has {count} records")
                
            logger.info("✅ Sales receipts table setup completed!")
        
    except Exception as e:
        logger.error(f"Error creating sales_receipts table: {e}")
        raise

if __name__ == "__main__":
    create_sales_receipts_table()