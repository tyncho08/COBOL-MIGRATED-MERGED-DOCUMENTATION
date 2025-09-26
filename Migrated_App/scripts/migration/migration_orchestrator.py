"""
Migration Orchestrator

Main orchestrator for COBOL to PostgreSQL data migration.
Coordinates the entire migration process including data reading, transformation,
validation, loading, and rollback capabilities.
"""

import os
import json
import logging
import asyncio
from typing import Dict, List, Any, Optional, Tuple
from datetime import datetime, timedelta
from dataclasses import dataclass, asdict
from enum import Enum
import asyncpg
import shutil
from pathlib import Path

from cobol_data_reader import CobolDataReader
from data_transformer import DataTransformer
from validation.validation_engine import ValidationEngine, ValidationSummary

logger = logging.getLogger(__name__)


class MigrationStatus(Enum):
    """Migration status enumeration"""
    PENDING = "pending"
    IN_PROGRESS = "in_progress"
    COMPLETED = "completed"
    FAILED = "failed"
    ROLLED_BACK = "rolled_back"


@dataclass
class MigrationConfig:
    """Configuration for migration process"""
    source_data_directory: str
    backup_directory: str
    database_connection_string: str
    batch_size: int = 1000
    enable_validation: bool = True
    enable_rollback: bool = True
    max_errors_per_table: int = 100
    parallel_threads: int = 4
    log_level: str = "INFO"
    dry_run: bool = False


@dataclass
class TableMigrationResult:
    """Result of migrating a single table"""
    table_name: str
    source_file: str
    records_read: int
    records_transformed: int
    records_loaded: int
    records_failed: int
    start_time: datetime
    end_time: datetime
    status: MigrationStatus
    error_messages: List[str]
    validation_summary: Optional[ValidationSummary] = None


@dataclass
class MigrationResult:
    """Overall migration result"""
    migration_id: str
    start_time: datetime
    end_time: Optional[datetime]
    status: MigrationStatus
    total_tables: int
    completed_tables: int
    failed_tables: int
    total_records_processed: int
    total_records_loaded: int
    table_results: List[TableMigrationResult]
    validation_summary: Optional[ValidationSummary] = None
    error_messages: List[str]


class MigrationOrchestrator:
    """Main orchestrator for the migration process"""
    
    def __init__(self, config: MigrationConfig):
        self.config = config
        self.cobol_reader = CobolDataReader()
        self.data_transformer = DataTransformer()
        self.validation_engine = None
        self.db_pool = None
        
        # Migration tracking
        self.migration_id = f"migration_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
        self.migration_result = None
        
        # Table migration order (respecting dependencies)
        self.migration_order = [
            ('GLMAS', 'gl_accounts'),
            ('CUSMAS', 'customers'),
            ('SUPMAS', 'suppliers'),
            ('STKMAS', 'stock_items'),
            ('SLHDR', 'sales_invoices'),
            ('GLTRAN', 'gl_transactions')
        ]
        
        # Setup logging
        self._setup_logging()
    
    def _setup_logging(self):
        """Setup logging configuration"""
        logging.basicConfig(
            level=getattr(logging, self.config.log_level),
            format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
            handlers=[
                logging.FileHandler(f'migration_{self.migration_id}.log'),
                logging.StreamHandler()
            ]
        )
    
    async def initialize(self):
        """Initialize database connections and validation engine"""
        try:
            # Create database connection pool
            self.db_pool = await asyncpg.create_pool(
                self.config.database_connection_string,
                max_size=self.config.parallel_threads * 2
            )
            
            # Initialize validation engine
            if self.config.enable_validation:
                self.validation_engine = ValidationEngine(self.config.database_connection_string)
                await self.validation_engine.initialize_database()
            
            logger.info("Migration orchestrator initialized successfully")
            
        except Exception as e:
            logger.error(f"Failed to initialize migration orchestrator: {str(e)}")
            raise
    
    async def cleanup(self):
        """Cleanup resources"""
        try:
            if self.validation_engine:
                await self.validation_engine.close_database()
            
            if self.db_pool:
                await self.db_pool.close()
                
            logger.info("Migration orchestrator cleanup completed")
            
        except Exception as e:
            logger.error(f"Error during cleanup: {str(e)}")
    
    async def run_migration(self) -> MigrationResult:
        """
        Run the complete migration process
        
        Returns:
            MigrationResult with details of the migration
        """
        start_time = datetime.now()
        
        # Initialize migration result
        self.migration_result = MigrationResult(
            migration_id=self.migration_id,
            start_time=start_time,
            end_time=None,
            status=MigrationStatus.IN_PROGRESS,
            total_tables=len(self.migration_order),
            completed_tables=0,
            failed_tables=0,
            total_records_processed=0,
            total_records_loaded=0,
            table_results=[],
            error_messages=[]
        )
        
        logger.info(f"Starting migration {self.migration_id}")
        
        try:
            # Pre-migration validation
            await self._pre_migration_checks()
            
            # Create backup if enabled
            if self.config.enable_rollback:
                await self._create_backup()
            
            # Migrate each table in order
            for cobol_layout, table_name in self.migration_order:
                try:
                    table_result = await self._migrate_table(cobol_layout, table_name)
                    self.migration_result.table_results.append(table_result)
                    
                    if table_result.status == MigrationStatus.COMPLETED:
                        self.migration_result.completed_tables += 1
                    else:
                        self.migration_result.failed_tables += 1
                        
                        # Check if we should abort
                        if len(table_result.error_messages) > self.config.max_errors_per_table:
                            logger.error(f"Too many errors in {table_name}, aborting migration")
                            raise Exception(f"Migration aborted due to excessive errors in {table_name}")
                    
                    self.migration_result.total_records_processed += table_result.records_read
                    self.migration_result.total_records_loaded += table_result.records_loaded
                    
                except Exception as e:
                    error_msg = f"Failed to migrate table {table_name}: {str(e)}"
                    logger.error(error_msg)
                    self.migration_result.error_messages.append(error_msg)
                    self.migration_result.failed_tables += 1
            
            # Post-migration validation
            if self.config.enable_validation:
                logger.info("Running post-migration validation")
                validation_summary = await self.validation_engine.validate_all()
                self.migration_result.validation_summary = validation_summary
                
                # Check for critical validation errors
                if validation_summary.error_count > 0:
                    logger.warning(f"Migration completed with {validation_summary.error_count} validation errors")
            
            # Determine final status
            if self.migration_result.failed_tables == 0:
                self.migration_result.status = MigrationStatus.COMPLETED
                logger.info(f"Migration {self.migration_id} completed successfully")
            else:
                self.migration_result.status = MigrationStatus.FAILED
                logger.error(f"Migration {self.migration_id} completed with {self.migration_result.failed_tables} failed tables")
            
        except Exception as e:
            error_msg = f"Migration failed: {str(e)}"
            logger.error(error_msg)
            self.migration_result.error_messages.append(error_msg)
            self.migration_result.status = MigrationStatus.FAILED
            
            # Attempt rollback if enabled
            if self.config.enable_rollback and not self.config.dry_run:
                try:
                    await self._rollback()
                    self.migration_result.status = MigrationStatus.ROLLED_BACK
                except Exception as rollback_error:
                    logger.error(f"Rollback failed: {str(rollback_error)}")
                    self.migration_result.error_messages.append(f"Rollback failed: {str(rollback_error)}")
        
        finally:
            self.migration_result.end_time = datetime.now()
            await self._save_migration_log()
        
        return self.migration_result
    
    async def _pre_migration_checks(self):
        """Run pre-migration validation checks"""
        logger.info("Running pre-migration checks")
        
        # Check source data files exist
        for cobol_layout, table_name in self.migration_order:
            source_file = self._get_source_file_path(cobol_layout)
            if not os.path.exists(source_file):
                raise FileNotFoundError(f"Source data file not found: {source_file}")
            
            # Validate file structure
            validation_info = self.cobol_reader.validate_file_structure(source_file, cobol_layout)
            if not validation_info['structure_valid']:
                logger.warning(f"File structure warning for {source_file}: {validation_info['remaining_bytes']} remaining bytes")
        
        # Check database connectivity
        async with self.db_pool.acquire() as conn:
            version = await conn.fetchval('SELECT version()')
            logger.info(f"Connected to database: {version}")
        
        # Check target tables exist
        async with self.db_pool.acquire() as conn:
            for _, table_name in self.migration_order:
                exists = await conn.fetchval(
                    "SELECT EXISTS(SELECT 1 FROM information_schema.tables WHERE table_name = $1)",
                    table_name
                )
                if not exists:
                    raise Exception(f"Target table {table_name} does not exist")
        
        logger.info("Pre-migration checks completed successfully")
    
    async def _migrate_table(self, cobol_layout: str, table_name: str) -> TableMigrationResult:
        """
        Migrate a single table
        
        Args:
            cobol_layout: COBOL record layout name
            table_name: Target PostgreSQL table name
            
        Returns:
            TableMigrationResult with migration details
        """
        start_time = datetime.now()
        source_file = self._get_source_file_path(cobol_layout)
        
        logger.info(f"Starting migration of {table_name} from {source_file}")
        
        result = TableMigrationResult(
            table_name=table_name,
            source_file=source_file,
            records_read=0,
            records_transformed=0,
            records_loaded=0,
            records_failed=0,
            start_time=start_time,
            end_time=None,
            status=MigrationStatus.IN_PROGRESS,
            error_messages=[]
        )
        
        try:
            # Read COBOL data
            logger.info(f"Reading COBOL data from {source_file}")
            cobol_records = self.cobol_reader.read_file(source_file, cobol_layout)
            result.records_read = len(cobol_records)
            
            logger.info(f"Read {result.records_read} records from {source_file}")
            
            # Transform data in batches
            batch_size = self.config.batch_size
            transformed_records = []
            
            for i in range(0, len(cobol_records), batch_size):
                batch = cobol_records[i:i + batch_size]
                batch_transformed = self.data_transformer.transform_batch(batch, table_name)
                transformed_records.extend(batch_transformed)
                
                logger.info(f"Transformed batch {i//batch_size + 1}/{(len(cobol_records) + batch_size - 1)//batch_size}")
            
            result.records_transformed = len(transformed_records)
            result.records_failed = result.records_read - result.records_transformed
            
            # Load data to database
            if not self.config.dry_run:
                await self._load_data_to_database(transformed_records, table_name)
                result.records_loaded = result.records_transformed
            else:
                logger.info(f"DRY RUN: Would load {result.records_transformed} records to {table_name}")
                result.records_loaded = result.records_transformed
            
            # Get transformation statistics
            transform_stats = self.data_transformer.get_transformation_stats()
            if transform_stats['validation_errors'] > 0:
                result.error_messages.append(f"Transformation validation errors: {transform_stats['validation_errors']}")
            
            result.status = MigrationStatus.COMPLETED
            logger.info(f"Successfully migrated {result.records_loaded} records to {table_name}")
            
        except Exception as e:
            error_msg = f"Error migrating {table_name}: {str(e)}"
            logger.error(error_msg)
            result.error_messages.append(error_msg)
            result.status = MigrationStatus.FAILED
        
        finally:
            result.end_time = datetime.now()
            # Reset transformation stats for next table
            self.data_transformer.reset_stats()
        
        return result
    
    async def _load_data_to_database(self, records: List[Dict[str, Any]], table_name: str):
        """Load transformed data to PostgreSQL database"""
        if not records:
            logger.warning(f"No records to load for {table_name}")
            return
        
        async with self.db_pool.acquire() as conn:
            # Clear existing data if not appending
            if not self.config.dry_run:
                await conn.execute(f"TRUNCATE TABLE {table_name} CASCADE")
                logger.info(f"Truncated table {table_name}")
            
            # Prepare insert statement
            first_record = records[0]
            columns = list(first_record.keys())
            placeholders = ', '.join([f'${i+1}' for i in range(len(columns))])
            
            insert_query = f"""
                INSERT INTO {table_name} ({', '.join(columns)})
                VALUES ({placeholders})
            """
            
            # Insert records in batches
            batch_size = self.config.batch_size
            for i in range(0, len(records), batch_size):
                batch = records[i:i + batch_size]
                
                # Prepare values for batch
                batch_values = []
                for record in batch:
                    values = []
                    for column in columns:
                        value = record.get(column)
                        # Handle special cases for PostgreSQL
                        if isinstance(value, datetime):
                            values.append(value)
                        elif value is None:
                            values.append(None)
                        else:
                            values.append(value)
                    batch_values.append(values)
                
                # Execute batch insert
                try:
                    await conn.executemany(insert_query, batch_values)
                    logger.debug(f"Loaded batch {i//batch_size + 1} for {table_name}")
                    
                except Exception as e:
                    logger.error(f"Error loading batch to {table_name}: {str(e)}")
                    # Try individual inserts to identify problematic records
                    for j, values in enumerate(batch_values):
                        try:
                            await conn.execute(insert_query, *values)
                        except Exception as record_error:
                            logger.error(f"Error loading record {i+j+1} to {table_name}: {str(record_error)}")
                            logger.debug(f"Problematic record: {batch[j]}")
            
            # Update sequences
            await self._update_sequences(conn, table_name)
            
            logger.info(f"Successfully loaded {len(records)} records to {table_name}")
    
    async def _update_sequences(self, conn: asyncpg.Connection, table_name: str):
        """Update PostgreSQL sequences for tables with auto-increment columns"""
        # Find columns with sequences
        sequence_query = """
            SELECT c.column_name, c.column_default
            FROM information_schema.columns c
            WHERE c.table_name = $1
              AND c.column_default LIKE 'nextval%'
        """
        
        columns_with_sequences = await conn.fetch(sequence_query, table_name)
        
        for column_info in columns_with_sequences:
            column_name = column_info['column_name']
            
            # Get the maximum value in the column
            max_value_query = f"SELECT COALESCE(MAX({column_name}), 0) FROM {table_name}"
            max_value = await conn.fetchval(max_value_query)
            
            # Extract sequence name from default value
            default_value = column_info['column_default']
            sequence_name = default_value.split("'")[1]  # Extract sequence name from nextval('sequence_name'::regclass)
            
            # Set sequence to max_value + 1
            await conn.execute(f"SELECT setval('{sequence_name}', {max_value + 1})")
            
            logger.info(f"Updated sequence {sequence_name} to {max_value + 1}")
    
    def _get_source_file_path(self, cobol_layout: str) -> str:
        """Get the source file path for a COBOL layout"""
        file_mappings = {
            'CUSMAS': 'CUSMAS.DAT',
            'SUPMAS': 'SUPMAS.DAT', 
            'STKMAS': 'STKMAS.DAT',
            'GLMAS': 'GLMAS.DAT',
            'GLTRAN': 'GLTRAN.DAT',
            'SLHDR': 'SLHDR.DAT'
        }
        
        filename = file_mappings.get(cobol_layout, f"{cobol_layout}.DAT")
        return os.path.join(self.config.source_data_directory, filename)
    
    async def _create_backup(self):
        """Create database backup before migration"""
        logger.info("Creating database backup")
        
        backup_dir = Path(self.config.backup_directory) / self.migration_id
        backup_dir.mkdir(parents=True, exist_ok=True)
        
        # Export each table to backup
        tables_to_backup = [table_name for _, table_name in self.migration_order]
        
        async with self.db_pool.acquire() as conn:
            for table_name in tables_to_backup:
                backup_file = backup_dir / f"{table_name}.json"
                
                # Get all records from table
                records = await conn.fetch(f"SELECT * FROM {table_name}")
                
                # Convert to JSON serializable format
                json_records = []
                for record in records:
                    json_record = {}
                    for key, value in record.items():
                        if isinstance(value, datetime):
                            json_record[key] = value.isoformat()
                        elif hasattr(value, '__dict__'):  # Handle custom objects
                            json_record[key] = str(value)
                        else:
                            json_record[key] = value
                    json_records.append(json_record)
                
                # Save to file
                with open(backup_file, 'w') as f:
                    json.dump(json_records, f, indent=2, default=str)
                
                logger.info(f"Backed up {len(json_records)} records from {table_name}")
        
        logger.info(f"Database backup completed: {backup_dir}")
    
    async def _rollback(self):
        """Rollback migration using backup data"""
        logger.info(f"Starting rollback for migration {self.migration_id}")
        
        backup_dir = Path(self.config.backup_directory) / self.migration_id
        
        if not backup_dir.exists():
            raise Exception(f"Backup directory not found: {backup_dir}")
        
        # Restore tables in reverse order
        tables_to_restore = [table_name for _, table_name in reversed(self.migration_order)]
        
        async with self.db_pool.acquire() as conn:
            for table_name in tables_to_restore:
                backup_file = backup_dir / f"{table_name}.json"
                
                if backup_file.exists():
                    # Truncate table
                    await conn.execute(f"TRUNCATE TABLE {table_name} CASCADE")
                    
                    # Load backup data
                    with open(backup_file, 'r') as f:
                        backup_records = json.load(f)
                    
                    if backup_records:
                        # Prepare insert statement
                        columns = list(backup_records[0].keys())
                        placeholders = ', '.join([f'${i+1}' for i in range(len(columns))])
                        insert_query = f"INSERT INTO {table_name} ({', '.join(columns)}) VALUES ({placeholders})"
                        
                        # Insert backup data
                        for record in backup_records:
                            values = [record.get(col) for col in columns]
                            await conn.execute(insert_query, *values)
                    
                    logger.info(f"Restored {len(backup_records)} records to {table_name}")
                else:
                    logger.warning(f"Backup file not found for {table_name}")
        
        logger.info("Rollback completed successfully")
    
    async def _save_migration_log(self):
        """Save detailed migration log to file"""
        log_dir = Path(self.config.backup_directory) / "migration_logs"
        log_dir.mkdir(parents=True, exist_ok=True)
        
        log_file = log_dir / f"migration_log_{self.migration_id}.json"
        
        # Convert result to JSON serializable format
        log_data = asdict(self.migration_result)
        
        # Handle datetime serialization
        def serialize_datetime(obj):
            if isinstance(obj, datetime):
                return obj.isoformat()
            return obj
        
        with open(log_file, 'w') as f:
            json.dump(log_data, f, indent=2, default=serialize_datetime)
        
        logger.info(f"Migration log saved: {log_file}")
    
    def generate_migration_report(self) -> str:
        """Generate a formatted migration report"""
        if not self.migration_result:
            return "No migration result available"
        
        report = []
        report.append("=" * 80)
        report.append(f"MIGRATION REPORT - {self.migration_result.migration_id}")
        report.append("=" * 80)
        
        # Summary
        duration = (self.migration_result.end_time - self.migration_result.start_time) if self.migration_result.end_time else timedelta(0)
        report.append(f"Status: {self.migration_result.status.value.upper()}")
        report.append(f"Duration: {duration}")
        report.append(f"Total Tables: {self.migration_result.total_tables}")
        report.append(f"Completed Tables: {self.migration_result.completed_tables}")
        report.append(f"Failed Tables: {self.migration_result.failed_tables}")
        report.append(f"Total Records Processed: {self.migration_result.total_records_processed:,}")
        report.append(f"Total Records Loaded: {self.migration_result.total_records_loaded:,}")
        report.append("")
        
        # Table details
        report.append("TABLE MIGRATION DETAILS:")
        report.append("-" * 40)
        for table_result in self.migration_result.table_results:
            status_emoji = "✅" if table_result.status == MigrationStatus.COMPLETED else "❌"
            duration = table_result.end_time - table_result.start_time if table_result.end_time else timedelta(0)
            
            report.append(f"{status_emoji} {table_result.table_name}")
            report.append(f"   Records: {table_result.records_read:,} read, {table_result.records_loaded:,} loaded")
            report.append(f"   Duration: {duration}")
            
            if table_result.error_messages:
                for error in table_result.error_messages:
                    report.append(f"   Error: {error}")
        
        report.append("")
        
        # Validation summary
        if self.migration_result.validation_summary:
            validation = self.migration_result.validation_summary
            report.append("VALIDATION SUMMARY:")
            report.append("-" * 40)
            report.append(f"Total Rules: {validation.total_rules}")
            report.append(f"Passed: {validation.passed_rules}")
            report.append(f"Failed: {validation.failed_rules}")
            report.append(f"Errors: {validation.error_count}")
            report.append(f"Warnings: {validation.warning_count}")
            report.append("")
        
        # Overall errors
        if self.migration_result.error_messages:
            report.append("MIGRATION ERRORS:")
            report.append("-" * 40)
            for error in self.migration_result.error_messages:
                report.append(f"❌ {error}")
        
        report.append("=" * 80)
        
        return "\n".join(report)