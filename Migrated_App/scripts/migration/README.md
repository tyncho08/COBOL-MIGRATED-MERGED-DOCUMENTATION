# ACAS COBOL to PostgreSQL Migration System

Complete ETL system for migrating 49 years of COBOL legacy data to modern PostgreSQL database.

## Overview

This migration system handles the complete transformation of ACAS (Applewood Computers Accounting System) data from COBOL fixed-width files to PostgreSQL relational database. The system processes customer master files, supplier data, stock inventory, general ledger accounts, transactions, and sales data while preserving all business logic and data integrity.

## Features

- 🔄 **Complete COBOL Data Reading**: Supports EBCDIC encoding, packed decimals, zoned decimals, and complex COBOL data types
- 🔧 **Data Transformation**: Comprehensive field mapping with business rule validation
- ✅ **Data Validation**: 30+ validation rules ensuring data quality and integrity
- 🗄️ **PostgreSQL Integration**: Optimized loading with batch processing and transaction management
- 📊 **Comprehensive Reporting**: Detailed migration reports with statistics and error analysis
- 🔄 **Rollback Capability**: Complete backup and rollback system for safe migration
- 🚀 **Parallel Processing**: Multi-threaded processing for large datasets
- 🎯 **Dry Run Mode**: Test migrations without affecting database

## System Architecture

```
┌─────────────────────┐    ┌──────────────────────┐    ┌─────────────────────┐
│   COBOL Data Files  │ ─→ │  Migration System    │ ─→ │  PostgreSQL DB      │
│                     │    │                      │    │                     │
│ • CUSMAS.DAT        │    │ • COBOL Reader       │    │ • customers         │
│ • SUPMAS.DAT        │    │ • Data Transformer   │    │ • suppliers         │
│ • STKMAS.DAT        │    │ • Validation Engine  │    │ • stock_items       │
│ • GLMAS.DAT         │    │ • Migration Orch.    │    │ • gl_accounts       │
│ • GLTRAN.DAT        │    │ • Rollback System    │    │ • gl_transactions   │
│ • SLHDR.DAT         │    │                      │    │ • sales_invoices    │
└─────────────────────┘    └──────────────────────┘    └─────────────────────┘
```

## Prerequisites

### System Requirements
- Python 3.9 or higher
- PostgreSQL 12 or higher
- At least 4GB RAM
- Sufficient disk space (2x source data size recommended)

### Python Dependencies
```bash
pip install asyncpg asyncio logging pathlib decimal uuid
```

### Database Setup
Ensure your PostgreSQL database has the required tables created:
```sql
-- Run the database schema creation scripts first
psql -U your_user -d your_database -f ../../database/complete_schema.sql
```

## Quick Start

### 1. Basic Migration
```bash
python run_migration.py \
    --source-dir /path/to/cobol/data \
    --db-url postgresql://user:password@localhost:5432/acas_db
```

### 2. Dry Run (Recommended First)
```bash
python run_migration.py \
    --source-dir /path/to/cobol/data \
    --db-url postgresql://user:password@localhost:5432/acas_db \
    --dry-run
```

### 3. Production Migration
```bash
python run_migration.py \
    --source-dir /data/cobol \
    --db-url postgresql://acas_user:secure_pass@db-server:5432/acas_production \
    --backup-dir /backups/migration \
    --batch-size 2000 \
    --threads 8 \
    --log-level INFO \
    --log-file migration_$(date +%Y%m%d_%H%M%S).log
```

## Command Line Options

| Option | Description | Default |
|--------|-------------|---------|
| `--source-dir` | Directory containing COBOL data files | Required |
| `--db-url` | PostgreSQL connection URL | Required |
| `--backup-dir` | Backup directory for rollback | `./backups` |
| `--batch-size` | Records per batch | 1000 |
| `--threads` | Parallel processing threads | 4 |
| `--max-errors` | Max errors per table before abort | 100 |
| `--log-level` | Logging level (DEBUG/INFO/WARNING/ERROR) | INFO |
| `--dry-run` | Simulate without loading data | False |
| `--no-validation` | Skip validation (faster) | False |
| `--no-backup` | Skip backup creation | False |
| `--force` | Force migration with warnings | False |

## File Structure

```
migration/
├── run_migration.py              # Main migration runner
├── migration_orchestrator.py     # Migration coordinator
├── cobol_data_reader.py          # COBOL file parser
├── data_transformer.py           # Data transformation engine
├── modules/                      # Module-specific migrators
│   └── migrate_gl_data.py       # GL-specific migration
└── validation/                   # Data validation
    └── validation_engine.py     # Validation rules engine
```

## COBOL File Formats Supported

### Customer Master (CUSMAS.DAT)
- **Record Length**: 300 bytes
- **Fields**: Customer code, name, address, contact info, credit limits, balances
- **Encoding**: EBCDIC with packed decimal amounts

### Supplier Master (SUPMAS.DAT)
- **Record Length**: 324 bytes
- **Fields**: Supplier code, name, address, contact info, payment terms
- **Encoding**: EBCDIC with packed decimal amounts

### Stock Item Master (STKMAS.DAT)
- **Record Length**: 224 bytes
- **Fields**: Item code, description, costs, quantities, costing methods
- **Encoding**: EBCDIC with packed decimal amounts

### GL Account Master (GLMAS.DAT)
- **Record Length**: 174 bytes
- **Fields**: Account code, name, type, balances, budgets
- **Encoding**: EBCDIC with packed decimal amounts

### GL Transactions (GLTRAN.DAT)
- **Record Length**: 149 bytes
- **Fields**: Transaction details, amounts, references, posting info
- **Encoding**: EBCDIC with packed decimal amounts

### Sales Invoice Headers (SLHDR.DAT)
- **Record Length**: 174 bytes
- **Fields**: Invoice details, customer info, amounts, terms
- **Encoding**: EBCDIC with packed decimal amounts

## Data Transformation

### Field Mappings
The system includes comprehensive field mapping for:
- **Character Fields**: EBCDIC to UTF-8 conversion with cleaning
- **Numeric Fields**: Zoned decimal and packed decimal conversion
- **Dates**: YYYYMMDD to PostgreSQL timestamp
- **Amounts**: Packed decimal to PostgreSQL DECIMAL(15,2)
- **Codes**: Validation and standardization

### Business Rules
- Customer credit limit validation
- GL account balance verification
- Stock costing method consistency
- Referential integrity checks
- Data format validation

## Validation System

### 30+ Built-in Validation Rules

#### Required Field Validation
- Customer/Supplier/Item codes must be present
- GL account codes must be present
- Essential names and descriptions required

#### Format Validation
- Customer codes: 6 alphanumeric characters
- Supplier codes: 6 alphanumeric characters
- GL account codes: 8 numeric characters
- Email addresses: Valid format
- Phone numbers: Digits and formatting only

#### Range Validation
- Credit limits: 0 to 1,000,000
- Prices: Positive values
- Quantities: Reasonable ranges
- Dates: 1980-2030 range

#### Referential Integrity
- Customer references in invoices
- Supplier references in stock items
- GL account references in transactions

#### Business Rules
- Invoice total calculations
- GL debit/credit balance
- Stock valuation consistency
- Customer balance verification

#### Consistency Checks
- No duplicate customer codes
- No duplicate supplier codes
- No duplicate item codes
- No duplicate GL account codes

## Migration Process

### Phase 1: Prerequisites Check
- Source file availability
- Database connectivity
- Table structure validation
- Disk space verification
- Backup directory access

### Phase 2: Data Reading
- COBOL file structure validation
- EBCDIC encoding handling
- Packed decimal conversion
- Record layout verification

### Phase 3: Data Transformation
- Field mapping application
- Data type conversion
- Business rule application
- Data cleansing and validation

### Phase 4: Data Loading
- Database backup creation
- Batch processing with transactions
- Sequence management
- Foreign key handling

### Phase 5: Validation
- Post-load data validation
- Business rule verification
- Data integrity checks
- Report generation

### Phase 6: Reporting
- Migration statistics
- Validation results
- Error analysis
- Performance metrics

## Error Handling

### Error Categories
1. **File Errors**: Missing files, access issues, format problems
2. **Transformation Errors**: Data conversion issues, validation failures
3. **Database Errors**: Connection issues, constraint violations, loading failures
4. **Business Rule Violations**: Logic inconsistencies, reference problems

### Error Recovery
- **Batch-level**: Failed batches are logged but don't stop migration
- **Table-level**: Configurable error thresholds per table
- **Migration-level**: Complete rollback capability
- **Logging**: Detailed error logs with record identification

## Performance Optimization

### Processing Features
- **Batch Processing**: Configurable batch sizes (default 1000 records)
- **Parallel Processing**: Multi-threaded loading (default 4 threads)
- **Memory Management**: Streaming processing for large files
- **Connection Pooling**: Database connection optimization

### Performance Tips
- Use SSD storage for source files and database
- Increase batch size for larger datasets (2000-5000)
- Use more threads for high-performance servers (8-16)
- Run during low-activity periods
- Monitor system resources during migration

## Rollback System

### Backup Strategy
- **Pre-migration**: Complete table backups in JSON format
- **Incremental**: Transaction-level rollback points
- **Metadata**: Migration state and progress tracking

### Rollback Process
```bash
# Automatic rollback on failure
# Manual rollback if needed:
python rollback_migration.py --migration-id migration_20240126_143022
```

## Monitoring and Logging

### Log Levels
- **DEBUG**: Detailed processing information
- **INFO**: Progress updates and major milestones
- **WARNING**: Non-critical issues and validation warnings
- **ERROR**: Critical errors and failures

### Log Output
```
2024-01-26 14:30:15 - migration_orchestrator - INFO - Starting migration migration_20240126_143015
2024-01-26 14:30:16 - cobol_data_reader - INFO - Read 15,847 records from CUSMAS.DAT
2024-01-26 14:30:17 - data_transformer - INFO - Transformed 15,832 records successfully
2024-01-26 14:30:18 - migration_orchestrator - INFO - Loaded 15,832 records to customers table
```

### Progress Tracking
- Real-time progress updates
- Record counts and processing rates
- Estimated completion times
- Memory and disk usage

## Troubleshooting

### Common Issues

#### Source File Problems
```
Error: File not found: CUSMAS.DAT
Solution: Verify source directory path and file permissions
```

#### Database Connection Issues
```
Error: Connection refused to database
Solution: Check database URL, credentials, and network connectivity
```

#### Memory Issues
```
Error: Out of memory processing large file
Solution: Reduce batch size and increase system memory
```

#### Validation Failures
```
Error: 150 validation errors found
Solution: Review validation report and fix data issues or use --force
```

### Debug Mode
```bash
python run_migration.py --log-level DEBUG --dry-run ...
```

## Migration Report Example

```
================================================================================
MIGRATION REPORT - migration_20240126_143015
================================================================================
Status: COMPLETED
Duration: 0:45:23
Total Tables: 6
Completed Tables: 6
Failed Tables: 0
Total Records Processed: 125,847
Total Records Loaded: 125,832

TABLE MIGRATION DETAILS:
----------------------------------------
✅ gl_accounts
   Records: 125 read, 125 loaded
   Duration: 0:00:15

✅ customers
   Records: 15,847 read, 15,832 loaded
   Duration: 0:05:22

✅ suppliers
   Records: 3,456 read, 3,456 loaded
   Duration: 0:02:18

✅ stock_items
   Records: 45,234 read, 45,223 loaded
   Duration: 0:15:45

✅ sales_invoices
   Records: 35,678 read, 35,678 loaded
   Duration: 0:12:30

✅ gl_transactions
   Records: 25,507 read, 25,507 loaded
   Duration: 0:09:13

VALIDATION SUMMARY:
----------------------------------------
Total Rules: 32
Passed: 30
Failed: 2
Errors: 0
Warnings: 2

================================================================================
```

## Production Deployment

### Pre-Production Checklist
- [ ] Complete dry run successful
- [ ] Database schema deployed
- [ ] Backup strategy confirmed
- [ ] Rollback procedure tested
- [ ] Performance requirements validated
- [ ] Error handling verified
- [ ] Monitoring configured

### Production Migration Steps
1. **Maintenance Window**: Schedule appropriate downtime
2. **Final Backup**: Complete database backup
3. **Migration Execution**: Run with production parameters
4. **Validation**: Comprehensive post-migration validation
5. **Smoke Tests**: Basic functionality verification
6. **Go-Live**: Enable application access

### Post-Migration
- Monitor system performance
- Validate business processes
- Archive COBOL data files
- Update documentation
- Train users on new system

## Support and Maintenance

### Log Retention
- Migration logs: Keep for 1 year
- Backup files: Keep for 6 months
- Error reports: Keep permanently

### Regular Maintenance
- Monitor disk usage in backup directory
- Archive old migration files
- Update validation rules as needed
- Performance tuning based on usage patterns

## License and Support

This migration system is part of the ACAS modernization project. For support and questions, contact the development team.

---

*Last Updated: January 2024*
*Version: 1.0.0*