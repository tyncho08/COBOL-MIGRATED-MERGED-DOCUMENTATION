# ACAS System Architecture

## Overview

The ACAS system follows a modular monolithic architecture pattern, developed over 45+ years with consistent design principles. The system uses GnuCOBOL as its core technology with file-based data storage and partial database integration.

## Architecture Patterns

### Overall System Structure

```
┌─────────────────────────────────────────────────────────┐
│                    User Interface Layer                   │
│              (Character-based screens - maps04)           │
├─────────────────────────────────────────────────────────┤
│                  Business Logic Layer                     │
│   ┌──────────┬──────────┬──────────┬──────────┬──────┐ │
│   │  Sales   │ Purchase │  Stock   │ General  │ IRS  │ │
│   │  (SL)    │  (PL)    │  (ST)    │ Ledger   │      │ │
│   │          │          │          │  (GL)    │      │ │
│   └──────────┴──────────┴──────────┴──────────┴──────┘ │
├─────────────────────────────────────────────────────────┤
│               Data Access Layer (DAL)                     │
│         (*MT programs - File/Database handlers)           │
├─────────────────────────────────────────────────────────┤
│                    Data Storage Layer                     │
│   ┌────────────────────┐  ┌─────────────────────────┐  │
│   │   ISAM Files       │  │  MySQL/MariaDB          │  │
│   │  (Primary)         │  │  (Partial support)      │  │
│   └────────────────────┘  └─────────────────────────┘  │
└─────────────────────────────────────────────────────────┘
```

## Module Structure

### Core Modules

1. **System Control (ACAS)**
   - Main entry point
   - System initialization
   - Parameter management
   - User access control
   - Environment setup

2. **Sales Ledger (SL)**
   - Customer management
   - Order processing
   - Invoice generation
   - Cash receipts
   - Credit control
   - Sales analysis

3. **Purchase Ledger (PL)**
   - Supplier management
   - Purchase orders
   - Goods receipts
   - Invoice processing
   - Payment processing
   - Purchase analysis

4. **Stock Control (ST)**
   - Item master maintenance
   - Stock movements
   - Valuation
   - Reorder management
   - Physical inventory
   - Stock analysis

5. **General Ledger (GL)**
   - Chart of accounts
   - Journal entries
   - Financial reporting
   - Period/year-end processing
   - Budget management
   - Multi-company support

6. **IRS (Incomplete Records System)**
   - Simplified bookkeeping
   - Automatic double-entry
   - Bank reconciliation
   - VAT/tax reporting
   - Small business focused

### Support Modules

1. **Common Utilities**
   - Screen handling (maps04, maps09)
   - Date processing
   - Check digit validation
   - Error handling
   - File logging

2. **Data Access Layer**
   - File handlers (*MT programs)
   - Database connectors
   - Transaction control
   - Lock management

## Call Graphs and Dependencies

### System Initialization Flow

```
ACAS (Main Entry)
├── acas000 (System Init)
│   ├── systemMT (Read system parameters)
│   ├── dfltMT (Load defaults)
│   ├── finalMT (Setup final accounts)
│   └── fhlogger (Initialize logging)
└── maps04 (Screen handler)
    └── Screen I/O routines
```

### Sales Order Processing Flow

```
sales (Menu)
└── sl020 (Order Entry)
    ├── maps04 (Screen handling)
    ├── maps09 (Check digit validation)
    ├── salesMT (Customer access)
    ├── stockMT (Inventory check)
    ├── slpostingMT (Create transaction)
    └── sl810 (Auto invoice)
        ├── slinvoiceMT (Invoice generation)
        ├── slautogenMT (Number generation)
        └── nominalMT (GL posting)
```

### Data Access Layer Pattern

```
Business Program (e.g., sl010)
    │
    ├── Calls with operation code
    ↓
Data Access Layer (e.g., salesMT)
    │
    ├── Validates request
    ├── Handles locking
    ├── Performs I/O
    │   ├── File access (ISAM)
    │   └── Database access (SQL)
    └── Returns status
```

### Module Interaction Matrix

| From/To | Sales | Purchase | Stock | GL | System |
|---------|-------|----------|-------|----|---------
| Sales | Internal | - | stockMT | nominalMT | systemMT |
| Purchase | - | Internal | stockMT | nominalMT | systemMT |
| Stock | salesMT | purchMT | Internal | nominalMT | systemMT |
| GL | slpostingMT | plpostingMT | valueMT | Internal | systemMT |
| System | All | All | All | All | Internal |

## File Organization Strategy

### Directory Structure

```
Legacy_App/
├── common/        # System-wide utilities
├── copybooks/     # Shared data structures
├── sales/         # Sales ledger programs
├── purchase/      # Purchase ledger programs
├── stock/         # Stock control programs
├── general/       # General ledger programs
├── irs/           # IRS programs
└── mysql/         # Database scripts
```

### File Naming Conventions

- **Programs**: `xx###.cbl` where:
  - `xx` = Module prefix (sl, pl, st, gl)
  - `###` = Program number
  - Example: `sl010.cbl` = Sales customer maintenance

- **Copybooks**: Descriptive names
  - `FDxxxx` = File descriptions
  - `WSxxxx` = Working storage
  - `SCREEN*` = Screen definitions
  - `LINK*` = Linkage structures

- **Data Files**: Module-based
  - `SLMASTER` = Sales master file
  - `SLTRANS` = Sales transactions
  - Pattern: `XXmaster`, `XXtrans`

## Batch Job Scheduling

### Daily Processing Schedule

```
Time    Job              Description
------  ---------------  ----------------------------------
00:00   Backup          Full system backup
06:00   Import          Bank file imports
08:00   Credit Check    Update credit statuses
17:00   Invoice Gen     Generate daily invoices (sl810)
18:00   Reports         Daily operational reports
22:00   Maintenance     File reorganization
```

### Monthly Processing Schedule

```
Day     Job              Programs Used
------  ---------------  ----------------------------------
1st     Statements      sl110, pl100
5th     Month-End       sl900, pl900, st900, gl900
        - Close period
        - Age balances
        - Update stats
10th    Reports         sl910, pl910, st050, gl950
15th    Analysis        sl920, pl090, st100
Month-  Year-End        gl910 + all year-end programs
End     (if applicable)
```

### Batch Dependencies

```
Daily Invoicing Flow:
sl800 (Extract orders) → sl810 (Generate invoices) → sl820 (Print)
                                       ↓
                                 nominalMT (GL posting)

Month-End Flow:
Module closes (sl900, pl900, st900)
                ↓
         gl030 (GL period close)
                ↓
         gl050 (Trial balance)
                ↓
         gl950 (Financial statements)
```

## Error Handling Patterns

### Standard Error Handling Structure

```cobol
CALL 'program-name' USING parameters
                    GIVING return-code

EVALUATE return-code
    WHEN 0
        CONTINUE
    WHEN 1
        DISPLAY "Record not found"
        PERFORM error-recovery
    WHEN 9
        DISPLAY "File error"
        PERFORM abort-processing
    WHEN OTHER
        DISPLAY "Unexpected error: " return-code
        PERFORM abort-processing
END-EVALUATE
```

### Error Code Ranges

- **00-09**: Success and warnings
  - 00 = Success
  - 01 = Record not found (may be ok)
  - 02 = Duplicate key (warning)

- **10-89**: Application errors
  - 10-19 = Validation errors
  - 20-29 = Business rule violations
  - 30-39 = Authorization errors

- **90-99**: System errors
  - 90-94 = File errors
  - 95-98 = Database errors
  - 99 = Fatal system error

### Error Recovery Strategies

1. **Transaction Level**
   - Rollback current transaction
   - Log error details
   - Return to menu

2. **Batch Level**
   - Mark record as error
   - Continue with next record
   - Error report at end

3. **System Level**
   - Save state if possible
   - Close files properly
   - Generate dump for debugging

## Integration Patterns

### Embedded SQL vs File-Based

The system supports dual data access:

1. **File-Based (Primary)**
   ```cobol
   CALL 'salesMT' USING 'READ'
                        sales-record
                        return-code
   ```

2. **Database (Partial)**
   ```cobol
   EXEC SQL
       SELECT * FROM customers
       WHERE cust_no = :ws-cust-no
   END-EXEC
   ```

### Integration Decision Logic

```
IF use-database = 'Y' AND table-exists
    Use SQL access
ELSE
    Use file access
END-IF
```

### Transaction Boundaries

1. **Logical Transaction**
   - Begin with user action
   - Validate all data
   - Update all affected files/tables
   - Commit or rollback as unit

2. **File Locking Strategy**
   - Read with lock for updates
   - Hold lock minimum time
   - Release on commit/rollback
   - Deadlock detection timeout

## Technical Debt Analysis

### High-Complexity Programs

Programs exceeding acceptable complexity (>50):
- **sl910** (555): Massive report generation
- **xl150** (520): Period-end utilities
- **st030** (433): Stock valuation
- **gl030** (377): GL period processing

### Architectural Debt

1. **Tight Coupling**
   - Direct program calls
   - Shared file access
   - Global data dependencies

2. **Limited Abstraction**
   - Business logic in UI
   - Data access throughout
   - Minimal service layer

3. **Procedural Limitations**
   - Extensive GO TO usage
   - Long procedural sections
   - Limited modularization

### Modernization Opportunities

1. **Service-Oriented Architecture**
   ```
   Current: Program A → Program B (direct call)
   Target:  Program A → Service API → Program B
   ```

2. **Data Access Abstraction**
   ```
   Current: Business logic → File I/O
   Target:  Business logic → Repository → Data store
   ```

3. **Event-Driven Processing**
   ```
   Current: Batch processing with fixed schedule
   Target:  Event triggers and message queues
   ```

## GnuCOBOL Compatibility

### Compiler Requirements
- GnuCOBOL 3.0 or higher
- Support for ISAM files
- Screen section support
- Embedded SQL preprocessor

### Compilation Flags
```bash
cobc -x -O2 -Wall \
     -ffile-format=ibm \
     -fsource-location \
     program.cbl
```

### Runtime Environment
```bash
export COB_FILE_PATH=/path/to/datafiles
export COB_LIBRARY_PATH=/path/to/modules
export COB_SCREEN_ESC=Y
export COB_SCREEN_EXCEPTIONS=Y
```

## Migration Recommendations

### Phase 1: Foundation (3 months)
1. Create service layer for data access
2. Implement centralized error handling
3. Build automated testing framework
4. Create API definitions

### Phase 2: Module Migration (12 months)
1. Start with IRS (least complex)
2. Migrate Stock Control
3. Migrate Purchase Ledger
4. Migrate Sales Ledger
5. Migrate General Ledger (last)

### Phase 3: Modernization (6 months)
1. Replace character UI with web
2. Implement RESTful services
3. Add real-time integrations
4. Enable cloud deployment

### Risk Mitigation
1. Maintain parallel run capability
2. Implement comprehensive logging
3. Create rollback procedures
4. Extensive user training

## Performance Characteristics

### Current Performance Metrics
- Transaction response: <2 seconds
- Batch invoice run: 1000/minute
- Report generation: Varies by size
- Month-end processing: 2-4 hours

### Bottlenecks
1. Sequential file processing
2. Limited concurrent users
3. Batch window requirements
4. Single-threaded execution

### Optimization Opportunities
1. Parallel batch processing
2. Indexed file optimization
3. Query optimization for reports
4. Caching frequently accessed data