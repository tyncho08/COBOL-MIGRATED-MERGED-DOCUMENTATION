"""
ACAS COBOL to PostgreSQL Migration Runner

Main script to execute the complete data migration from COBOL legacy system
to modern PostgreSQL database. Handles all modules and provides comprehensive
reporting and rollback capabilities.
"""

import asyncio
import argparse
import logging
import sys
import os
from pathlib import Path
from datetime import datetime
from typing import Dict, Any

# Add current directory to path
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from migration_orchestrator import MigrationOrchestrator, MigrationConfig, MigrationStatus
from validation.validation_engine import ValidationEngine


def setup_logging(log_level: str = "INFO", log_file: str = None):
    """Setup logging configuration"""
    log_format = '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    
    handlers = [logging.StreamHandler(sys.stdout)]
    if log_file:
        handlers.append(logging.FileHandler(log_file))
    
    logging.basicConfig(
        level=getattr(logging, log_level.upper()),
        format=log_format,
        handlers=handlers
    )


def parse_arguments():
    """Parse command line arguments"""
    parser = argparse.ArgumentParser(
        description='ACAS COBOL to PostgreSQL Migration Tool',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Full migration with validation
  python run_migration.py --source-dir /data/cobol --db-url postgresql://user:pass@localhost/acas

  # Dry run to test migration
  python run_migration.py --source-dir /data/cobol --db-url postgresql://user:pass@localhost/acas --dry-run

  # Migration with custom batch size and parallel processing
  python run_migration.py --source-dir /data/cobol --db-url postgresql://user:pass@localhost/acas --batch-size 2000 --threads 8

  # Migration without validation (faster)
  python run_migration.py --source-dir /data/cobol --db-url postgresql://user:pass@localhost/acas --no-validation
        """
    )
    
    # Required arguments
    parser.add_argument(
        '--source-dir', '-s',
        required=True,
        help='Directory containing COBOL data files (e.g., CUSMAS.DAT, SUPMAS.DAT)'
    )
    
    parser.add_argument(
        '--db-url', '-d',
        required=True,
        help='PostgreSQL database connection URL (postgresql://user:pass@host:port/database)'
    )
    
    # Optional arguments
    parser.add_argument(
        '--backup-dir', '-b',
        default='./backups',
        help='Directory for database backups (default: ./backups)'
    )
    
    parser.add_argument(
        '--batch-size',
        type=int,
        default=1000,
        help='Number of records to process in each batch (default: 1000)'
    )
    
    parser.add_argument(
        '--threads',
        type=int,
        default=4,
        help='Number of parallel processing threads (default: 4)'
    )
    
    parser.add_argument(
        '--max-errors',
        type=int,
        default=100,
        help='Maximum errors per table before aborting (default: 100)'
    )
    
    parser.add_argument(
        '--log-level',
        choices=['DEBUG', 'INFO', 'WARNING', 'ERROR'],
        default='INFO',
        help='Logging level (default: INFO)'
    )
    
    parser.add_argument(
        '--log-file',
        help='Log file path (logs to console if not specified)'
    )
    
    # Boolean flags
    parser.add_argument(
        '--dry-run',
        action='store_true',
        help='Run migration simulation without actually loading data'
    )
    
    parser.add_argument(
        '--no-validation',
        action='store_true',
        help='Skip data validation steps (faster but less safe)'
    )
    
    parser.add_argument(
        '--no-backup',
        action='store_true',
        help='Skip database backup creation (faster but no rollback capability)'
    )
    
    parser.add_argument(
        '--force',
        action='store_true',
        help='Force migration even if validation warnings exist'
    )
    
    parser.add_argument(
        '--tables',
        nargs='+',
        help='Migrate only specific tables (e.g., --tables customers suppliers)'
    )
    
    return parser.parse_args()


async def check_prerequisites(config: MigrationConfig) -> Dict[str, Any]:
    """Check migration prerequisites and requirements"""
    logger = logging.getLogger(__name__)
    logger.info("Checking migration prerequisites...")
    
    checks = {
        'source_directory_exists': False,
        'source_files_found': [],
        'missing_files': [],
        'database_accessible': False,
        'database_version': None,
        'tables_exist': False,
        'disk_space_sufficient': False,
        'backup_directory_writable': False
    }
    
    # Check source directory and files
    source_path = Path(config.source_data_directory)
    checks['source_directory_exists'] = source_path.exists() and source_path.is_dir()
    
    if checks['source_directory_exists']:
        expected_files = [
            'CUSMAS.DAT',   # Customer master
            'SUPMAS.DAT',   # Supplier master
            'STKMAS.DAT',   # Stock item master
            'GLMAS.DAT',    # GL account master
            'GLTRAN.DAT',   # GL transactions
            'SLHDR.DAT'     # Sales invoice headers
        ]
        
        for filename in expected_files:
            file_path = source_path / filename
            if file_path.exists():
                checks['source_files_found'].append(filename)
            else:
                checks['missing_files'].append(filename)
    
    # Check database connectivity
    try:
        import asyncpg
        conn = await asyncpg.connect(config.database_connection_string)
        checks['database_accessible'] = True
        checks['database_version'] = await conn.fetchval('SELECT version()')
        
        # Check if required tables exist
        required_tables = [
            'customers', 'suppliers', 'stock_items', 
            'gl_accounts', 'gl_transactions', 'sales_invoices'
        ]
        
        existing_tables = []
        for table in required_tables:
            exists = await conn.fetchval(
                "SELECT EXISTS(SELECT 1 FROM information_schema.tables WHERE table_name = $1)",
                table
            )
            if exists:
                existing_tables.append(table)
        
        checks['tables_exist'] = len(existing_tables) == len(required_tables)
        checks['existing_tables'] = existing_tables
        
        await conn.close()
        
    except Exception as e:
        logger.error(f"Database connectivity check failed: {str(e)}")
        checks['database_error'] = str(e)
    
    # Check backup directory
    backup_path = Path(config.backup_directory)
    try:
        backup_path.mkdir(parents=True, exist_ok=True)
        test_file = backup_path / 'test_write.tmp'
        test_file.write_text('test')
        test_file.unlink()
        checks['backup_directory_writable'] = True
    except Exception as e:
        logger.error(f"Backup directory check failed: {str(e)}")
        checks['backup_directory_error'] = str(e)
    
    # Basic disk space check (simplified)
    try:
        import shutil
        free_space = shutil.disk_usage(config.backup_directory).free
        checks['disk_space_sufficient'] = free_space > (1024 * 1024 * 1024)  # At least 1GB
        checks['free_space_gb'] = free_space / (1024 * 1024 * 1024)
    except Exception as e:
        logger.warning(f"Disk space check failed: {str(e)}")
    
    return checks


def print_prerequisites_report(checks: Dict[str, Any]):
    """Print prerequisites check report"""
    print("\n" + "="*80)
    print("MIGRATION PREREQUISITES CHECK")
    print("="*80)
    
    # Source files check
    print("\n📁 SOURCE FILES:")
    if checks['source_directory_exists']:
        print(f"✅ Source directory exists")
        print(f"✅ Found {len(checks['source_files_found'])} data files: {', '.join(checks['source_files_found'])}")
        if checks['missing_files']:
            print(f"⚠️  Missing files: {', '.join(checks['missing_files'])}")
    else:
        print("❌ Source directory does not exist")
    
    # Database check
    print(f"\n🗄️  DATABASE:")
    if checks['database_accessible']:
        print("✅ Database connection successful")
        print(f"✅ {checks['database_version']}")
        if checks['tables_exist']:
            print("✅ All required tables exist")
        else:
            print(f"⚠️  Some tables missing. Found: {checks.get('existing_tables', [])}")
    else:
        print("❌ Database connection failed")
        if 'database_error' in checks:
            print(f"   Error: {checks['database_error']}")
    
    # Backup and storage
    print(f"\n💾 BACKUP & STORAGE:")
    if checks['backup_directory_writable']:
        print("✅ Backup directory accessible")
    else:
        print("❌ Backup directory not writable")
        if 'backup_directory_error' in checks:
            print(f"   Error: {checks['backup_directory_error']}")
    
    if checks['disk_space_sufficient']:
        print(f"✅ Sufficient disk space ({checks.get('free_space_gb', 0):.1f} GB available)")
    else:
        print(f"⚠️  Low disk space ({checks.get('free_space_gb', 0):.1f} GB available)")
    
    print("="*80)


async def run_migration(config: MigrationConfig, args) -> bool:
    """Run the complete migration process"""
    logger = logging.getLogger(__name__)
    
    try:
        # Check prerequisites
        prereq_checks = await check_prerequisites(config)
        print_prerequisites_report(prereq_checks)
        
        # Validate prerequisites
        if not prereq_checks['source_directory_exists']:
            logger.error("Source directory not found - cannot proceed")
            return False
        
        if not prereq_checks['database_accessible']:
            logger.error("Database not accessible - cannot proceed")
            return False
        
        if not prereq_checks['tables_exist'] and not args.force:
            logger.error("Required database tables missing - use --force to override")
            return False
        
        if not prereq_checks['backup_directory_writable'] and not config.enable_rollback:
            logger.warning("Backup directory not accessible - disabling rollback capability")
            config.enable_rollback = False
        
        # Create and initialize orchestrator
        logger.info("Initializing migration orchestrator...")
        orchestrator = MigrationOrchestrator(config)
        await orchestrator.initialize()
        
        try:
            # Run migration
            logger.info("Starting ACAS COBOL to PostgreSQL migration...")
            print(f"\n🚀 Starting migration {orchestrator.migration_id}")
            print(f"   Source: {config.source_data_directory}")
            print(f"   Backup: {config.backup_directory}")
            print(f"   Batch size: {config.batch_size}")
            print(f"   Validation: {'Enabled' if config.enable_validation else 'Disabled'}")
            print(f"   Dry run: {'Yes' if config.dry_run else 'No'}")
            
            # Execute migration
            result = await orchestrator.run_migration()
            
            # Print results
            print(f"\n📊 MIGRATION RESULTS:")
            print(f"   Status: {result.status.value.upper()}")
            print(f"   Duration: {result.end_time - result.start_time if result.end_time else 'N/A'}")
            print(f"   Tables processed: {result.total_tables}")
            print(f"   Tables completed: {result.completed_tables}")
            print(f"   Tables failed: {result.failed_tables}")
            print(f"   Records processed: {result.total_records_processed:,}")
            print(f"   Records loaded: {result.total_records_loaded:,}")
            
            # Print detailed report
            report = orchestrator.generate_migration_report()
            print(f"\n{report}")
            
            # Save report to file
            report_file = Path(config.backup_directory) / f"migration_report_{orchestrator.migration_id}.txt"
            report_file.parent.mkdir(parents=True, exist_ok=True)
            report_file.write_text(report)
            logger.info(f"Migration report saved: {report_file}")
            
            # Return success status
            success = result.status in [MigrationStatus.COMPLETED, MigrationStatus.ROLLED_BACK]
            
            if success:
                print(f"\n✅ Migration completed successfully!")
                if result.validation_summary and result.validation_summary.warning_count > 0:
                    print(f"⚠️  Note: {result.validation_summary.warning_count} validation warnings found")
            else:
                print(f"\n❌ Migration failed!")
                if result.error_messages:
                    print("Errors:")
                    for error in result.error_messages[-5:]:  # Show last 5 errors
                        print(f"  - {error}")
            
            return success
            
        finally:
            await orchestrator.cleanup()
    
    except KeyboardInterrupt:
        logger.warning("Migration interrupted by user")
        print("\n⚠️  Migration interrupted by user")
        return False
    
    except Exception as e:
        logger.error(f"Migration failed with error: {str(e)}")
        print(f"\n❌ Migration failed: {str(e)}")
        return False


async def main():
    """Main function"""
    args = parse_arguments()
    
    # Setup logging
    log_file = args.log_file or f"migration_{datetime.now().strftime('%Y%m%d_%H%M%S')}.log"
    setup_logging(args.log_level, log_file)
    logger = logging.getLogger(__name__)
    
    print("🏗️  ACAS COBOL to PostgreSQL Migration Tool")
    print("="*60)
    
    # Create migration configuration
    config = MigrationConfig(
        source_data_directory=args.source_dir,
        backup_directory=args.backup_dir,
        database_connection_string=args.db_url,
        batch_size=args.batch_size,
        enable_validation=not args.no_validation,
        enable_rollback=not args.no_backup,
        max_errors_per_table=args.max_errors,
        parallel_threads=args.threads,
        log_level=args.log_level,
        dry_run=args.dry_run
    )
    
    logger.info(f"Migration configuration: {config}")
    
    # Run migration
    success = await run_migration(config, args)
    
    # Exit with appropriate code
    exit_code = 0 if success else 1
    print(f"\n🏁 Migration finished with exit code {exit_code}")
    sys.exit(exit_code)


if __name__ == "__main__":
    # Handle Windows event loop policy
    if sys.platform == 'win32':
        asyncio.set_event_loop_policy(asyncio.WindowsProactorEventLoopPolicy())
    
    asyncio.run(main())