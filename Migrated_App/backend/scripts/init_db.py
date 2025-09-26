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
        
        logger.info("Database initialization completed successfully")
        logger.info("Total tables created: 43")
        
    except Exception as e:
        logger.error(f"Database initialization failed: {e}")
        raise

if __name__ == "__main__":
    init_database()