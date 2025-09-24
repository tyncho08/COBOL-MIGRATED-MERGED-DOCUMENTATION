# ACAS Data Ownership Map

## Overview

This document establishes clear data ownership boundaries, defining which subsystem owns each data entity and which subsystems have read/write access. This is critical for maintaining data integrity and guiding the migration to a service-oriented architecture.

## Data Ownership Principles

1. **Single Owner Rule**: Each data entity has exactly one owning subsystem
2. **Owner Responsibilities**: Create, update, delete, and maintain integrity
3. **Access Control**: Owners control who can access their data
4. **Audit Requirements**: All changes tracked by owner
5. **Migration Impact**: Owner migrates with their data

## Master Data Ownership

### Customer Data
**Owner**: MDM (Master Data Management)

| Entity | Description | Access Rights |
|--------|-------------|---------------|
| Customer Master | Core customer information | Read: AR_MGMT, RPT_ENGINE, SEC_AUDIT |
| Customer Addresses | Delivery and billing addresses | Read: AR_MGMT, RPT_ENGINE |
| Customer Contacts | Contact persons and details | Read: AR_MGMT |
| Customer Credit | Credit limits and ratings | Read: AR_MGMT; Update: AR_MGMT (delegated) |
| Customer Groups | Classification and segmentation | Read: AR_MGMT, RPT_ENGINE |

### Supplier Data
**Owner**: MDM (Master Data Management)

| Entity | Description | Access Rights |
|--------|-------------|---------------|
| Supplier Master | Core supplier information | Read: AP_MGMT, RPT_ENGINE, SEC_AUDIT |
| Supplier Addresses | Remit-to and ship-from addresses | Read: AP_MGMT |
| Supplier Contacts | Purchasing contacts | Read: AP_MGMT |
| Supplier Terms | Payment terms and conditions | Read: AP_MGMT |
| Supplier Classifications | Categories and ratings | Read: AP_MGMT, RPT_ENGINE |

### Product/Item Data
**Owner**: MDM (Master Data Management)

| Entity | Description | Access Rights |
|--------|-------------|---------------|
| Item Master | Product catalog | Read: AR_MGMT, AP_MGMT, INV_CTRL, RPT_ENGINE |
| Item Descriptions | Multi-language descriptions | Read: AR_MGMT, INV_CTRL |
| Item Categories | Product hierarchy | Read: All sales/purchase modules |
| Item Units | Units of measure | Read: All transaction modules |
| Item Pricing | Price lists and levels | Read: AR_MGMT; Update: AR_MGMT (delegated) |

### Chart of Accounts
**Owner**: MDM (Master Data Management)

| Entity | Description | Access Rights |
|--------|-------------|---------------|
| GL Accounts | Account definitions | Read: All modules; Update: GL_CORE (validation) |
| Cost Centers | Department/division codes | Read: All posting modules |
| Account Groups | Financial statement mapping | Read: GL_CORE, RPT_ENGINE |
| Budget Codes | Budget categories | Read: GL_CORE |

## Transactional Data Ownership

### Sales/Receivables Data
**Owner**: AR_MGMT (Accounts Receivable Management)

| Entity | Description | Access Rights |
|--------|-------------|---------------|
| Sales Orders | Customer orders | Read: INV_CTRL, RPT_ENGINE |
| Sales Invoices | Customer invoices | Read: GL_CORE, RPT_ENGINE |
| Credit Notes | Returns and adjustments | Read: GL_CORE, INV_CTRL |
| Cash Receipts | Customer payments | Read: GL_CORE, RPT_ENGINE |
| AR Open Items | Outstanding invoices | Read: RPT_ENGINE |
| Customer Statements | Account summaries | None (generated) |

### Purchase/Payables Data
**Owner**: AP_MGMT (Accounts Payable Management)

| Entity | Description | Access Rights |
|--------|-------------|---------------|
| Purchase Orders | Supplier orders | Read: INV_CTRL, RPT_ENGINE |
| Purchase Invoices | Supplier invoices | Read: GL_CORE, RPT_ENGINE |
| Debit Notes | Returns to suppliers | Read: GL_CORE, INV_CTRL |
| Payments | Supplier payments | Read: GL_CORE, RPT_ENGINE |
| AP Open Items | Outstanding bills | Read: RPT_ENGINE |

### Inventory Data
**Owner**: INV_CTRL (Inventory Control)

| Entity | Description | Access Rights |
|--------|-------------|---------------|
| Stock Balances | On-hand quantities | Read: AR_MGMT, AP_MGMT, RPT_ENGINE |
| Stock Movements | All inventory transactions | Read: GL_CORE, RPT_ENGINE |
| Stock Locations | Warehouse/bin data | Read: AR_MGMT, AP_MGMT |
| Stock Valuation | Cost layers (FIFO/LIFO) | Read: GL_CORE, RPT_ENGINE |
| Stock Counts | Physical inventory | Read: RPT_ENGINE |
| Reorder Levels | Min/max quantities | Read: AP_MGMT |

### General Ledger Data
**Owner**: GL_CORE (General Ledger)

| Entity | Description | Access Rights |
|--------|-------------|---------------|
| GL Transactions | All journal entries | Read: RPT_ENGINE, SEC_AUDIT |
| GL Balances | Account balances by period | Read: RPT_ENGINE |
| Budget Data | Budget vs actual | Read: RPT_ENGINE |
| Period Status | Open/closed periods | Read: All posting modules |
| Financial Statements | Generated reports | Read: RPT_ENGINE |

### IRS Data
**Owner**: IRS_PROC (Incomplete Records System)

| Entity | Description | Access Rights |
|--------|-------------|---------------|
| Bank Transactions | Bank statement lines | None (isolated) |
| IRS Postings | Generated journal entries | Write: GL_CORE |
| Reconciliations | Bank reconciliation data | None (isolated) |
| IRS Reports | Simplified accounts | None (isolated) |

## System Data Ownership

### Security Data
**Owner**: SEC_AUDIT (Security & Audit)

| Entity | Description | Access Rights |
|--------|-------------|---------------|
| User Profiles | User accounts and passwords | Read: All modules (authentication) |
| User Roles | Permission assignments | Read: All modules (authorization) |
| Access Rights | Module/function permissions | Read: All modules |
| Audit Logs | Transaction history | Write: All modules; Read: None |
| Login History | Access tracking | Write: All modules |

### System Parameters
**Owner**: SEC_AUDIT (Security & Audit)

| Entity | Description | Access Rights |
|--------|-------------|---------------|
| Company Data | Multi-company settings | Read: All modules |
| System Settings | Global parameters | Read: All modules |
| Module Parameters | Module-specific settings | Read: Respective modules |
| Default Values | System-wide defaults | Read: All modules |

### Batch Control Data
**Owner**: BATCH_FW (Batch Framework)

| Entity | Description | Access Rights |
|--------|-------------|---------------|
| Job Definitions | Batch job configurations | Read: All modules |
| Job Schedule | Execution timings | Update: Authorized users |
| Job History | Execution logs | Read: SEC_AUDIT, ERROR_FW |
| Dependencies | Job relationships | Read: All batch modules |

## Cross-Reference Tables

### Code Tables (Shared Ownership)

| Table | Primary Owner | Secondary Access |
|-------|---------------|------------------|
| Country Codes | MDM | All modules |
| Currency Codes | CURR_UTIL | All financial modules |
| Tax Codes | MDM | AR_MGMT, AP_MGMT, GL_CORE |
| Payment Terms | MDM | AR_MGMT, AP_MGMT |
| Delivery Terms | MDM | AR_MGMT, AP_MGMT |

## Data Access Patterns

### Read-Only Access Patterns

```
MDM (Master Data) → All Transaction Modules
GL_CORE (Balances) → RPT_ENGINE
All Modules → SEC_AUDIT (Audit Write)
```

### Update Delegation Patterns

```
AR_MGMT → MDM (Customer Credit Updates)
AR_MGMT → MDM (Pricing Updates)
All Modules → GL_CORE (Posting Only)
```

### Restricted Access Patterns

```
SEC_AUDIT (Audit Logs) → No Read Access
IRS_PROC → Isolated (GL_CORE posting only)
```

## Migration Impact Analysis

### Data Migration Priorities

1. **Phase 1**: Reference Data (MDM owned)
2. **Phase 2**: Security Data (SEC_AUDIT owned)
3. **Phase 3**: Master Files (MDM owned)
4. **Phase 4**: Open Transactions
5. **Phase 5**: Historical Data

### Ownership Changes During Migration

| Current State | Target State | Migration Impact |
|---------------|--------------|------------------|
| Shared file access | API-based access | Add access layer |
| Direct updates | Service calls | Implement services |
| No ownership | Clear ownership | Assign owners |
| Batch copies | Real-time sync | Event streaming |

## Data Governance Rules

### Ownership Responsibilities

1. **Data Quality**
   - Owner validates all updates
   - Owner maintains referential integrity
   - Owner handles deduplication

2. **Access Control**
   - Owner grants/revokes access
   - Owner monitors usage
   - Owner enforces policies

3. **Lifecycle Management**
   - Owner defines retention
   - Owner handles archival
   - Owner manages deletion

### Inter-Subsystem Rules

1. **No Direct Updates**
   - Only owner can update
   - Others request via API
   - Maintain audit trail

2. **Cached Data**
   - Clear expiry rules
   - Update mechanisms
   - Consistency checks

3. **Reference Integrity**
   - Owner validates references
   - Cascade rules defined
   - Error handling clear

## Compliance Considerations

### Data Privacy
- Customer data: MDM implements privacy controls
- Employee data: SEC_AUDIT implements access controls
- Financial data: GL_CORE implements retention policies

### Audit Requirements
- All changes logged by owner
- Read access tracked where required
- Retention per regulatory requirements

### Data Security
- Encryption requirements per owner
- Access logging by SEC_AUDIT
- Breach procedures defined