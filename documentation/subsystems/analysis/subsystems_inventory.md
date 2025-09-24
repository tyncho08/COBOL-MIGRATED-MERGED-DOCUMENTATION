# ACAS Subsystems Inventory

## Complete Subsystem Catalog

### 1. GL_CORE - General Ledger Core

**Purpose**: Central accounting engine managing financial transactions and reporting

**Business Functions**:
- Journal entry processing
- Account balance management
- Financial statement generation
- Period and year-end closing
- Multi-company consolidation
- Budget management

**Key Programs**:
- gl010: Chart of accounts maintenance
- gl020: Journal entry processing
- gl030: Period-end processing
- gl050: Trial balance generation
- gl900: Year-end procedures
- gl910: Financial statements
- gl950: Management reporting

**Criticality**: HIGH - Core financial system of record

---

### 2. AR_MGMT - Accounts Receivable Management

**Purpose**: Manage customer relationships and revenue collection

**Business Functions**:
- Customer master maintenance
- Sales order processing
- Invoice generation
- Cash receipt application
- Credit management
- Aging analysis
- Statement generation

**Key Programs**:
- sl010: Customer maintenance
- sl020: Order entry
- sl050: Invoice maintenance
- sl060: Cash receipts
- sl100: Credit control
- sl110: Statement printing
- sl810: Batch invoicing

**Criticality**: HIGH - Revenue generation and cash flow

---

### 3. AP_MGMT - Accounts Payable Management

**Purpose**: Manage supplier relationships and payment processing

**Business Functions**:
- Supplier master maintenance
- Purchase order management
- Invoice matching
- Payment processing
- Cash flow management
- Vendor analysis

**Key Programs**:
- pl010: Supplier maintenance
- pl020: Purchase orders
- pl040: Invoice entry
- pl055: Payment selection
- pl060: Payment processing
- pl080: Aging analysis
- pl090: Vendor analysis

**Criticality**: HIGH - Cash management and vendor relations

---

### 4. INV_CTRL - Inventory Control System

**Purpose**: Manage stock levels, movements, and valuation

**Business Functions**:
- Item master maintenance
- Stock movement tracking
- Multiple valuation methods (FIFO/LIFO/Average)
- Reorder point management
- Physical inventory
- Stock analysis

**Key Programs**:
- st010: Item maintenance
- st020: Stock movements
- st030: Valuation processing
- st040: Reorder analysis
- st050: Stock reports
- st800: Period-end processing

**Criticality**: HIGH - Asset management and cost control

---

### 5. IRS_PROC - Incomplete Records System

**Purpose**: Simplified accounting for small businesses

**Business Functions**:
- Bank reconciliation
- Automatic double-entry creation
- VAT/tax calculation
- Simple financial reporting
- Cash book management

**Key Programs**:
- irs010: Bank transaction entry
- irs020: Reconciliation
- irs030: VAT processing
- irs040: Report generation
- irs050: Period closing

**Criticality**: MEDIUM - Standalone module for specific user base

---

### 6. MDM - Master Data Management

**Purpose**: Central repository for all reference data

**Business Functions**:
- Customer master data
- Supplier master data
- Product/item catalog
- Chart of accounts
- System codes and parameters

**Key Programs**:
- Various *MT programs (data access layer)
- Code table maintenance
- Master file utilities

**Criticality**: HIGH - Foundation for all transactions

---

### 7. RPT_ENGINE - Reporting Engine

**Purpose**: Generate operational and management reports

**Business Functions**:
- Report generation
- Data extraction
- Format handling
- Report scheduling
- Export capabilities

**Key Programs**:
- All 9xx series programs
- Report utilities
- Export handlers

**Criticality**: MEDIUM - Important but not transactional

---

### 8. BATCH_FW - Batch Processing Framework

**Purpose**: Orchestrate and control batch operations

**Business Functions**:
- Job scheduling
- Dependency management
- Error recovery
- Process monitoring
- Log management

**Key Programs**:
- All 8xx series programs
- Batch control utilities
- Recovery procedures

**Criticality**: HIGH - Essential for daily operations

---

### 9. SEC_AUDIT - Security & Audit

**Purpose**: Control access and maintain compliance

**Business Functions**:
- User access control
- Password management
- Audit trail logging
- Compliance reporting
- Security monitoring

**Key Programs**:
- Security utilities
- Audit log processors
- Access control modules

**Criticality**: HIGH - Compliance and security

---

### 10. INTEGRATION - External Integration Services

**Purpose**: Interface with external systems

**Business Functions**:
- Bank file processing
- EDI transactions
- API services
- Data import/export
- Third-party interfaces

**Key Programs**:
- Import/export utilities
- Interface processors
- Format converters

**Criticality**: MEDIUM - External connectivity

---

### 11. FILE_SVC - File Management Services

**Purpose**: Handle all file operations and data persistence

**Business Functions**:
- File I/O operations
- Lock management
- Backup procedures
- Recovery services
- Archive management

**Key Programs**:
- File handlers
- Backup utilities
- Recovery tools

**Criticality**: HIGH - Data persistence layer

---

### 12. DATE_UTIL - Date & Calendar Management

**Purpose**: Centralized date processing and calendar logic

**Business Functions**:
- Date validation
- Date arithmetic
- Period calculations
- Holiday processing
- Working day calculation

**Key Programs**:
- maps04 (date utilities)
- Calendar processors
- Period calculators

**Criticality**: MEDIUM - Utility service

---

### 13. CURR_UTIL - Currency & Number Processing

**Purpose**: Handle multi-currency and numeric operations

**Business Functions**:
- Currency conversion
- Exchange rate management
- Rounding rules
- Decimal precision
- Format handling

**Key Programs**:
- Currency processors
- Rounding utilities
- Format converters

**Criticality**: MEDIUM - Financial calculations

---

### 14. ERROR_FW - Error Handling Framework

**Purpose**: Standardized error management across system

**Business Functions**:
- Error capture
- Error logging
- Recovery procedures
- Notification services
- Error reporting

**Key Programs**:
- Error handlers
- Recovery utilities
- Notification services

**Criticality**: MEDIUM - System stability

---

## Subsystem Program Allocation Summary

| Subsystem | Program Count | Primary Programs | Shared Utilities |
|-----------|--------------|-----------------|------------------|
| GL_CORE | 31 | gl0xx, gl9xx | maps04, systemMT |
| AR_MGMT | 42 | sl0xx, sl8xx, sl9xx | salesMT, maps04 |
| AP_MGMT | 38 | pl0xx, pl8xx, pl9xx | purchMT, maps04 |
| INV_CTRL | 35 | st0xx, st8xx, st9xx | stockMT, maps04 |
| IRS_PROC | 12 | irs0xx | irsMT, maps04 |
| MDM | 28 | Various MT programs | All *MT files |
| RPT_ENGINE | 25 | All 9xx programs | Report utilities |
| BATCH_FW | 18 | All 8xx programs | Batch utilities |
| SEC_AUDIT | 8 | Security utilities | Audit processors |
| INTEGRATION | 15 | Import/export | Interface handlers |
| FILE_SVC | 10 | File handlers | I/O utilities |
| DATE_UTIL | 3 | maps04, date utils | Date processors |
| CURR_UTIL | 4 | Currency utils | Format handlers |
| ERROR_FW | 5 | Error handlers | Recovery utils |

## Cross-Reference Matrix

| Function | Primary Subsystem | Secondary Subsystems |
|---------|------------------|---------------------|
| Customer Management | MDM | AR_MGMT |
| Order Processing | AR_MGMT | INV_CTRL, MDM |
| Invoicing | AR_MGMT | GL_CORE |
| Payment Processing | AP_MGMT, AR_MGMT | GL_CORE |
| Stock Management | INV_CTRL | MDM |
| Financial Reporting | GL_CORE | RPT_ENGINE |
| Period Closing | GL_CORE | All modules |
| Security | SEC_AUDIT | All modules |
| Data Access | FILE_SVC | All modules |
| Error Handling | ERROR_FW | All modules |