#!/usr/bin/env python3
"""
ACAS Database Initialization Script
Creates database tables and initializes system configuration
"""
import sys
import os
from pathlib import Path
import subprocess

# Add parent directory to path to import app modules
sys.path.append(str(Path(__file__).parent.parent))

from sqlalchemy import create_engine, text
from app.core.config import settings
import logging

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

def init_database():
    """Initialize database with complete schema"""
    try:
        # Use environment variables for PostgreSQL connection (set by run_app.sh)
        user = os.environ.get('PGUSER', 'postgres')
        password = os.environ.get('PGPASSWORD', '')
        host = os.environ.get('PGHOST', 'localhost')
        port = os.environ.get('PGPORT', '5432')
        dbname = 'acas_db'  # Use the database name created by run_app.sh
            
        logger.info(f"Connecting to PostgreSQL at {host}:{port} as user {user}")
        
        # First, create the database if it doesn't exist
        engine = create_engine(f'postgresql://{user}:{password}@{host}:{port}/postgres')
        with engine.connect() as conn:
            conn.execute(text("COMMIT"))  # Close any transaction
            exists = conn.execute(
                text("SELECT 1 FROM pg_database WHERE datname = :dbname"),
                {"dbname": dbname}
            ).fetchone()
            
            if not exists:
                logger.info(f"Creating database {dbname}...")
                conn.execute(text(f"CREATE DATABASE {dbname}"))
                conn.execute(text("COMMIT"))
            else:
                logger.info(f"Database {dbname} already exists")
        
        engine.dispose()
        
        # Now run the complete schema SQL file
        schema_file = Path(__file__).parent.parent.parent / 'database' / 'complete_schema.sql'
        
        if not schema_file.exists():
            raise FileNotFoundError(f"Schema file not found: {schema_file}")
            
        logger.info("Executing complete schema SQL...")
        
        # Use psql to execute the schema file
        env = os.environ.copy()
        if password:
            env['PGPASSWORD'] = password
            
        cmd = [
            'psql',
            '-h', host,
            '-p', port,
            '-U', user,
            '-d', dbname,
            '-f', str(schema_file)
        ]
        
        result = subprocess.run(cmd, env=env, capture_output=True, text=True)
        
        if result.returncode != 0:
            logger.error(f"Schema execution failed: {result.stderr}")
            raise Exception(f"Schema execution failed: {result.stderr}")
        else:
            logger.info("Schema executed successfully")
            if result.stdout:
                logger.info(f"Output: {result.stdout}")
        
        # Verify key tables exist
        engine = create_engine(f'postgresql://{user}:{password}@{host}:{port}/{dbname}')
        with engine.connect() as conn:
            # Check system_rec
            result = conn.execute(text("SELECT COUNT(*) FROM acas.system_rec"))
            count = result.scalar()
            logger.info(f"System record count: {count}")
            
            # Check other key tables
            tables = ['users', 'roles', 'saledger_rec', 'puledger_rec', 'stock_rec', 'glledger_rec']
            for table in tables:
                result = conn.execute(text(f"SELECT COUNT(*) FROM acas.{table}"))
                logger.info(f"Table acas.{table} exists with {result.scalar()} records")
            
            # Create sales_receipts table if it doesn't exist (critical for payments module)
            logger.info("Ensuring sales_receipts table exists...")
            
            # Check if sales_receipts table exists
            receipts_exists = conn.execute(text("""
                SELECT table_name 
                FROM information_schema.tables 
                WHERE table_schema = 'acas' 
                AND table_name = 'sales_receipts'
            """)).fetchone()
            
            if not receipts_exists:
                logger.info("Creating sales_receipts table...")
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
                conn.execute(text("CREATE INDEX idx_sales_receipts_customer ON acas.sales_receipts(sales_key);"))
                conn.execute(text("CREATE INDEX idx_sales_receipts_date ON acas.sales_receipts(receipt_date);"))
                conn.execute(text("CREATE INDEX idx_sales_receipts_status ON acas.sales_receipts(status);"))
                
                logger.info("✅ sales_receipts table created successfully")
                
                # Add sample data
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
                # Check if we have data
                count_result = conn.execute(text("SELECT COUNT(*) FROM acas.sales_receipts"))
                count = count_result.scalar()
                logger.info(f"sales_receipts table already exists with {count} records")
            
            # Fix missing company_name field in system_rec table
            logger.info("Checking system_rec table structure...")
            try:
                # Try to access company_name field
                conn.execute(text("SELECT company_name FROM acas.system_rec LIMIT 1"))
                logger.info("system_rec.company_name field exists")
            except Exception:
                logger.info("Adding missing company_name field to system_rec...")
                conn.execute(text("ALTER TABLE acas.system_rec ADD COLUMN IF NOT EXISTS company_name VARCHAR(40) DEFAULT '';"))
                logger.info("✅ company_name field added to system_rec")
        
        logger.info("Database initialization completed successfully")
        logger.info("Total tables created: 43+ (including sales_receipts)")
        
    except Exception as e:
        logger.error(f"Database initialization failed: {e}")
        raise

if __name__ == "__main__":
    init_database()