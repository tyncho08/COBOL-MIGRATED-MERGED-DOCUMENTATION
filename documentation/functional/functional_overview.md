# ACAS System Functional Overview

## Executive Summary

The Applewood Computers Accounting System (ACAS) is a comprehensive Enterprise Resource Planning (ERP) system that has been continuously operational for over 45 years. Originally developed by Applewood Computers, the system provides integrated accounting functionality including General Ledger, Sales/Accounts Receivable, Purchase/Accounts Payable, Stock Control, and a specialized IRS (Incomplete Records System) module.

### System Purpose and Scope

ACAS is designed to manage the complete financial and operational workflow for small to medium-sized businesses, providing:
- Full double-entry accounting with automatic posting to General Ledger
- Customer relationship management with credit control
- Supplier management with purchase order processing
- Inventory management with multiple valuation methods
- Financial reporting and compliance features
- Audit trail maintenance for all transactions

### Key Business Functions

#### 1. **General Ledger (GL) / Nominal Ledger**
- Chart of Accounts management with hierarchical structure
- Journal entry processing with automatic balancing
- Trial balance generation
- Financial statement preparation (P&L, Balance Sheet)
- Period-end processing and year-end closure
- Multi-period comparative reporting
- Default accounting to prevent out-of-balance conditions

#### 2. **Sales Ledger (SL) / Accounts Receivable (AR)**
- Customer master file maintenance
- Sales order entry and processing
- Invoice generation with automatic numbering
- Credit note processing
- Payment allocation and cash receipts
- Customer statement generation
- Aged debt analysis
- Credit limit monitoring
- Sales analysis by customer/product

#### 3. **Purchase Ledger (PL) / Accounts Payable (AP)**
- Supplier master file maintenance
- Purchase order generation
- Goods receipt processing
- Invoice matching and approval
- Payment scheduling
- Remittance advice generation
- Aged creditor analysis
- Purchase analysis reporting

#### 4. **Stock Control / Inventory Management**
- Item master maintenance with multiple units of measure
- Stock movement tracking (receipts, issues, adjustments)
- Multiple warehouse/location support
- Reorder point and quantity management
- Stock valuation (FIFO, LIFO, Average Cost)
- Physical inventory count support
- Stock aging analysis
- Integration with sales and purchase modules

#### 5. **IRS (Incomplete Records System)**
- Simplified accounting for smaller businesses
- Automatic double-entry generation
- Bank reconciliation features
- VAT/Tax calculation and reporting
- Designed for businesses without full accounting knowledge
- Default accounting prevents errors

### User Roles and Workflows

#### Primary User Roles:
1. **System Administrator**
   - System setup and configuration
   - User access management
   - Backup and recovery operations
   - Period-end processing

2. **Accounting Staff**
   - Transaction entry (sales, purchases, journals)
   - Payment processing
   - Bank reconciliation
   - Report generation

3. **Management**
   - Financial report review
   - Exception reporting
   - Performance analysis
   - Credit control decisions

4. **Warehouse Staff**
   - Stock receipts and issues
   - Stock count entry
   - Location transfers

#### Key Workflows:
1. **Order-to-Cash** (Sales Cycle)
   - Customer order → Stock allocation → Invoice generation → GL posting → Payment receipt → Cash allocation

2. **Procure-to-Pay** (Purchase Cycle)
   - Purchase requisition → PO generation → Goods receipt → Invoice matching → Payment scheduling → Check/payment run

3. **Period-End Processing**
   - Transaction cutoff → Accruals/prepayments → Depreciation → Stock valuation → Trial balance → Financial statements

### Technology Stack

#### Core Technology:
- **Language**: GnuCOBOL (OpenCOBOL compatible)
- **Architecture**: Modular monolithic design
- **Processing**: Batch and interactive modes

#### Data Storage:
- **Primary**: Indexed Sequential Access Method (ISAM) files
- **Secondary**: Sequential files for reports and interfaces
- **Database Support**: Partial MySQL/MariaDB integration
- **File Types**:
  - Master files (Customer, Supplier, Stock, GL)
  - Transaction files
  - Parameter/control files
  - Work files for processing

#### User Interface:
- Character-based screens using maps04/maps09 utilities
- Menu-driven navigation
- Function key shortcuts
- Report output to printer or file

### System Scale and Performance

#### Data Volumes:
- **Programs**: 278 COBOL programs
- **Copybooks**: 175 shared data structures
- **Lines of Code**: 133,973
- **Typical Transaction Volume**: Supports 1000s of transactions per day
- **User Base**: Designed for 1-50 concurrent users

#### Processing Characteristics:
- Real-time transaction validation
- Batch processing for reports and period-end
- File locking for multi-user access
- Transaction rollback capabilities

### Functional Coverage

#### Accounting Standards Compliance:
- Double-entry bookkeeping principles
- Accrual accounting support
- Multi-currency capable (with modifications)
- Tax calculation and reporting
- Audit trail requirements

#### Industry Features:
- Multi-location inventory
- Partial shipment handling
- Back-order management
- Consignment stock tracking
- Service item support (non-inventory)
- Project/job costing capabilities

#### Reporting Capabilities:
- Standard financial statements
- Management reports
- Operational reports
- Ad-hoc query capabilities
- Export to print/file
- Aging analysis
- Exception reporting

### System Integration Points

#### Internal Integration:
- Automatic GL posting from all modules
- Real-time stock updates from sales/purchases
- Integrated credit checking
- Consolidated reporting

#### External Interfaces:
- Bank statement import
- EDI capabilities (limited)
- Data export for spreadsheets
- Print spooling integration
- Backup script integration

### Compliance and Control Features

#### Security:
- User-level access control
- Menu-based restrictions
- Audit trail on all transactions
- Data validation at entry

#### Internal Controls:
- Segregation of duties
- Approval workflows
- Automatic sequence numbering
- Balance controls
- Exception reporting

#### Audit Features:
- Complete transaction history
- User activity logging
- Before/after data capture
- Report archive capabilities

### Business Rules Implementation

The system implements numerous business rules including:
- Automatic tax calculation based on customer/product codes
- Credit limit enforcement with override capabilities
- Inventory allocation rules (FIFO, specific identification)
- Discount calculation hierarchies
- Payment terms enforcement
- Period closure controls
- Inter-company transaction handling

### System Limitations and Constraints

1. **Technical Constraints**:
   - Character-based user interface
   - File-based architecture limits concurrent users
   - Batch processing windows required
   - Limited real-time integration capabilities

2. **Functional Constraints**:
   - Limited multi-currency support
   - Basic workflow capabilities
   - No built-in document management
   - Limited analytical reporting

3. **Scalability Constraints**:
   - File size limitations
   - Performance degradation with large data volumes
   - Backup window requirements

### Migration Considerations

The system's modular architecture and clear separation of concerns make it suitable for phased migration:
- Well-defined module boundaries
- Consistent data structures via copybooks
- Documented business rules
- Stable, proven functionality

However, technical debt including high complexity scores, extensive use of GO TO statements, and file-based architecture will require significant refactoring during modernization.