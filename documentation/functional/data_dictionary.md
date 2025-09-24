# ACAS Data Dictionary

## Overview
This data dictionary documents all files, tables, and data structures used in the ACAS system, providing field-level documentation, business meanings, and validation rules.

## File Organization

### File Types
- **Master Files**: Core business entities (customers, suppliers, items, accounts)
- **Transaction Files**: Business events (orders, invoices, payments, movements)
- **Control Files**: System parameters and configuration
- **Work Files**: Temporary processing files
- **Archive Files**: Historical data storage

### File Access Methods
- **Indexed (ISAM)**: Primary method for master files
- **Sequential**: Reports, interfaces, archives
- **Relative**: Some control files
- **Database Tables**: Partial MySQL/MariaDB support

## Master Files

### SLMASTER - Sales Ledger Master File
**Purpose**: Customer master records  
**Organization**: Indexed  
**Primary Key**: Customer Number (6 digits + check digit)

| Field Name | Type | Size | Description | Validation | Business Rules |
|-----------|------|------|-------------|------------|----------------|
| customer-no | X | 7 | Customer number with check digit | Check digit validation | Unique, system-generated |
| customer-name | X | 30 | Customer business name | Required, not blank | |
| customer-addr1 | X | 30 | Address line 1 | Required | |
| customer-addr2 | X | 30 | Address line 2 | Optional | |
| customer-addr3 | X | 30 | Address line 3 (City) | Required | |
| customer-addr4 | X | 30 | Address line 4 (State/Region) | Optional | |
| customer-postcode | X | 10 | Postal/ZIP code | Format validation | |
| customer-phone | X | 20 | Primary phone | Format validation | |
| customer-fax | X | 20 | Fax number | Optional | |
| customer-contact | X | 30 | Primary contact name | Optional | |
| customer-email | X | 50 | Email address | Format validation | |
| credit-limit | 9 | 7.2 | Credit limit amount | >= 0 | Enforced in order entry |
| payment-terms | 9 | 3 | Payment terms in days | Valid term codes | Standard: 30 days |
| discount-percent | 9 | 2.2 | Trade discount percentage | 0-99.99 | Applied to all sales |
| tax-code | X | 2 | Tax status code | Valid tax codes | E=Exempt, S=Standard |
| account-status | X | 1 | Account status | A=Active, H=Hold, C=Closed | |
| current-balance | S9 | 7.2 | Current outstanding | System maintained | Updated by transactions |
| period-sales | S9 | 7.2 | Current period sales | System maintained | Reset at period-end |
| ytd-sales | S9 | 7.2 | Year-to-date sales | System maintained | Reset at year-end |
| last-payment-date | 9 | 8 | Date of last payment | YYYYMMDD format | |
| last-payment-amt | 9 | 7.2 | Amount of last payment | >= 0 | |
| date-opened | 9 | 8 | Account open date | YYYYMMDD format | System generated |
| analysis-code | X | 4 | Sales analysis code | Valid analysis code | For reporting |
| territory-code | X | 2 | Sales territory | Valid territory | |
| salesperson-code | X | 3 | Assigned salesperson | Valid salesperson | |
| partial-ship-flag | X | 1 | Allow partial shipments | Y/N | Default from system |
| back-order-default | X | 1 | Create back orders | Y/N | Default = N |
| statement-flag | X | 1 | Print statements | Y/N | Default = Y |
| dunning-flag | X | 1 | Send dunning letters | Y/N | Default = Y |
| notes | X | 200 | Free-form notes | Optional | |

### PLMASTER - Purchase Ledger Master File
**Purpose**: Supplier master records  
**Organization**: Indexed  
**Primary Key**: Supplier Number (6 digits + check digit)

| Field Name | Type | Size | Description | Validation | Business Rules |
|-----------|------|------|-------------|------------|----------------|
| supplier-no | X | 7 | Supplier number with check digit | Check digit validation | Unique, system-generated |
| supplier-name | X | 30 | Supplier business name | Required, not blank | |
| supplier-addr1 | X | 30 | Address line 1 | Required | |
| supplier-addr2 | X | 30 | Address line 2 | Optional | |
| supplier-addr3 | X | 30 | Address line 3 (City) | Required | |
| supplier-addr4 | X | 30 | Address line 4 (State/Region) | Optional | |
| supplier-postcode | X | 10 | Postal/ZIP code | Format validation | |
| supplier-phone | X | 20 | Primary phone | Format validation | |
| supplier-fax | X | 20 | Fax number | Optional | |
| supplier-contact | X | 30 | Primary contact name | Optional | |
| supplier-email | X | 50 | Email address | Format validation | |
| payment-terms | 9 | 3 | Payment terms in days | Valid term codes | Used for aging |
| discount-percent | 9 | 2.2 | Settlement discount | 0-99.99 | Early payment discount |
| tax-code | X | 2 | Tax treatment | Valid tax codes | |
| account-status | X | 1 | Account status | A=Active, H=Hold, C=Closed | |
| current-balance | S9 | 7.2 | Current outstanding | System maintained | |
| ytd-purchases | S9 | 7.2 | Year-to-date purchases | System maintained | |
| last-payment-date | 9 | 8 | Date of last payment | YYYYMMDD format | |
| last-invoice-date | 9 | 8 | Date of last invoice | YYYYMMDD format | |
| analysis-code | X | 4 | Purchase analysis code | Valid analysis code | |
| currency-code | X | 3 | Currency for supplier | Valid currency | Default = base |
| bank-details | X | 60 | Bank account details | Optional | For payments |
| tax-number | X | 20 | Supplier tax ID | Optional | |

### STMASTER - Stock Master File
**Purpose**: Inventory item master  
**Organization**: Indexed  
**Primary Key**: Stock Number (15 characters)  
**Alternate Key**: Stock Abbreviation (10 characters)

| Field Name | Type | Size | Description | Validation | Business Rules |
|-----------|------|------|-------------|------------|----------------|
| stock-no | X | 15 | Stock item number | Unique, not blank | User-defined |
| stock-abbrev | X | 10 | Abbreviated code | Unique, not blank | Quick entry code |
| stock-desc | X | 30 | Item description | Required | |
| stock-desc2 | X | 30 | Extended description | Optional | |
| stock-location | X | 10 | Primary location | Valid location | |
| stock-unit | X | 5 | Unit of measure | Required | EA, BX, CS, etc. |
| stock-group | X | 4 | Product group | Valid group code | For reporting |
| supplier-no | X | 7 | Primary supplier | Valid supplier | With check digit |
| alt-supplier-1 | X | 7 | Alternate supplier 1 | Valid supplier | Optional |
| alt-supplier-2 | X | 7 | Alternate supplier 2 | Valid supplier | Optional |
| qty-on-hand | S9 | 7.3 | Current quantity | System maintained | |
| qty-allocated | S9 | 7.3 | Allocated quantity | System maintained | |
| qty-on-order | S9 | 7.3 | On purchase order | System maintained | |
| qty-back-order | S9 | 7.3 | On back order | System maintained | |
| reorder-point | 9 | 7.3 | Reorder trigger | >= 0 | |
| reorder-qty | 9 | 7.3 | Reorder quantity | > 0 | |
| unit-cost | 9 | 7.4 | Current unit cost | >= 0 | Average/FIFO/LIFO |
| sell-price-1 | 9 | 7.4 | Selling price level 1 | >= 0 | |
| sell-price-2 | 9 | 7.4 | Selling price level 2 | >= 0 | |
| sell-price-3 | 9 | 7.4 | Selling price level 3 | >= 0 | |
| last-cost | 9 | 7.4 | Last purchase cost | >= 0 | |
| ytd-sales-qty | S9 | 7.3 | YTD quantity sold | System maintained | |
| ytd-sales-val | S9 | 9.2 | YTD sales value | System maintained | |
| last-sale-date | 9 | 8 | Last sale date | YYYYMMDD | |
| last-receipt-date | 9 | 8 | Last receipt date | YYYYMMDD | |
| tax-code | X | 2 | Tax status | Valid tax code | |
| purchase-anal | X | 4 | Purchase analysis | Valid code | |
| sales-anal | X | 4 | Sales analysis | Valid code | |
| service-flag | X | 1 | Service item flag | Y/N | No inventory tracking |
| obsolete-flag | X | 1 | Obsolete indicator | Y/N | |
| serial-tracked | X | 1 | Serial number tracking | Y/N | |
| lot-tracked | X | 1 | Lot/batch tracking | Y/N | |

### GLMASTER - General Ledger Master File
**Purpose**: Chart of accounts  
**Organization**: Indexed  
**Primary Key**: Account Number (10 digits)

| Field Name | Type | Size | Description | Validation | Business Rules |
|-----------|------|------|-------------|------------|----------------|
| account-no | 9 | 10 | GL account number | Unique, valid structure | Hierarchical |
| account-desc | X | 30 | Account description | Required | |
| account-type | X | 1 | Account type code | A,L,C,I,E | Asset,Liability,Capital,Income,Expense |
| normal-balance | X | 1 | Normal balance | D/C | Debit or Credit |
| account-status | X | 1 | Status | A=Active, I=Inactive | |
| posting-flag | X | 1 | Allow posting | Y/N | N for headers |
| current-balance | S9 | 11.2 | Current period balance | System maintained | |
| ytd-balance | S9 | 11.2 | Year-to-date balance | System maintained | |
| budget-current | 9 | 11.2 | Current period budget | Optional | |
| budget-ytd | 9 | 11.2 | YTD budget | Optional | |
| last-year-bal | S9 | 11.2 | Prior year balance | System maintained | |
| department | X | 3 | Department code | Valid department | |
| cost-center | X | 4 | Cost center | Valid cost center | |
| report-level | 9 | 2 | Reporting level | 1-99 | For hierarchical reports |
| retained-earn | X | 1 | Retained earnings | Y/N | Year-end processing |

## Transaction Files

### SLTRANS - Sales Ledger Transactions
**Purpose**: Sales transactions (invoices, credits, payments)  
**Organization**: Indexed  
**Primary Key**: Transaction Number

| Field Name | Type | Size | Description | Validation | Business Rules |
|-----------|------|------|-------------|------------|----------------|
| trans-no | 9 | 8 | Transaction number | Unique, sequential | System-generated |
| trans-type | X | 2 | Transaction type | IN,CN,PM,AD | Invoice,Credit,Payment,Adjustment |
| customer-no | X | 7 | Customer number | Valid customer | |
| trans-date | 9 | 8 | Transaction date | Valid date | Period validation |
| reference | X | 15 | Document reference | Required | Invoice no, check no |
| description | X | 30 | Transaction description | Optional | |
| gross-amount | S9 | 9.2 | Gross amount | != 0 | Before tax/discount |
| tax-amount | S9 | 7.2 | Tax amount | Calculated | |
| net-amount | S9 | 9.2 | Net amount | Calculated | Gross + tax |
| paid-amount | S9 | 9.2 | Amount paid/allocated | System maintained | |
| period | 9 | 2 | Accounting period | Valid period | |
| year | 9 | 4 | Accounting year | Valid year | |
| gl-posted | X | 1 | Posted to GL flag | Y/N | |
| statement-flag | X | 1 | Include on statement | Y/N | Default = Y |

### PLTRANS - Purchase Ledger Transactions
**Purpose**: Purchase transactions (invoices, credits, payments)  
**Organization**: Indexed  
**Primary Key**: Transaction Number

| Field Name | Type | Size | Description | Validation | Business Rules |
|-----------|------|------|-------------|------------|----------------|
| trans-no | 9 | 8 | Transaction number | Unique, sequential | System-generated |
| trans-type | X | 2 | Transaction type | PI,PC,PM,AD | Purchase Invoice,Credit,Payment,Adjustment |
| supplier-no | X | 7 | Supplier number | Valid supplier | |
| trans-date | 9 | 8 | Transaction date | Valid date | |
| reference | X | 15 | Supplier reference | Required | |
| order-no | X | 10 | Purchase order ref | Optional | |
| gross-amount | S9 | 9.2 | Gross amount | != 0 | |
| tax-amount | S9 | 7.2 | Tax amount | Calculated | |
| net-amount | S9 | 9.2 | Net amount | Calculated | |
| paid-amount | S9 | 9.2 | Amount paid | System maintained | |
| due-date | 9 | 8 | Payment due date | Calculated | Based on terms |
| gl-posted | X | 1 | Posted to GL flag | Y/N | |

### STTRANS - Stock Transactions
**Purpose**: Inventory movements  
**Organization**: Sequential  
**Sort Key**: Date, Transaction Number

| Field Name | Type | Size | Description | Validation | Business Rules |
|-----------|------|------|-------------|------------|----------------|
| trans-no | 9 | 8 | Transaction number | Unique, sequential | |
| trans-type | X | 2 | Movement type | RC,IS,AJ,TR | Receipt,Issue,Adjust,Transfer |
| stock-no | X | 15 | Stock item | Valid item | |
| trans-date | 9 | 8 | Transaction date | Valid date | |
| quantity | S9 | 7.3 | Movement quantity | != 0 | +/- based on type |
| unit-cost | 9 | 7.4 | Unit cost | >= 0 | |
| total-value | S9 | 9.2 | Extended value | Calculated | |
| reference | X | 15 | Source reference | Required | PO, Invoice, etc. |
| location-from | X | 10 | Source location | Valid location | |
| location-to | X | 10 | Destination location | Valid location | |
| reason-code | X | 2 | Adjustment reason | Valid reason | For adjustments |
| gl-posted | X | 1 | Posted to GL | Y/N | |

### GLPOST - General Ledger Postings
**Purpose**: GL journal entries  
**Organization**: Indexed  
**Primary Key**: Batch Number, Entry Number

| Field Name | Type | Size | Description | Validation | Business Rules |
|-----------|------|------|-------------|------------|----------------|
| batch-no | 9 | 6 | Batch number | Sequential | System-generated |
| entry-no | 9 | 4 | Entry within batch | Sequential | |
| account-no | 9 | 10 | GL account | Valid account | Must allow posting |
| trans-date | 9 | 8 | Transaction date | Valid date, period | |
| reference | X | 15 | Source reference | Required | |
| description | X | 30 | Entry description | Required | |
| debit-amount | 9 | 11.2 | Debit amount | >= 0 | Either debit or credit |
| credit-amount | 9 | 11.2 | Credit amount | >= 0 | Either debit or credit |
| source-module | X | 2 | Source module | SL,PL,ST,GL,IR | |
| source-trans | 9 | 8 | Source transaction | Optional | |
| period | 9 | 2 | Accounting period | Valid period | |
| year | 9 | 4 | Accounting year | Valid year | |
| posted-flag | X | 1 | Posted status | Y/N | |
| post-date | 9 | 8 | Date posted | System date | When posted |

## Control Files

### SYSFILE - System Control File
**Purpose**: System-wide parameters and controls  
**Organization**: Indexed  
**Primary Key**: Record Type (1-4)

| Field Name | Type | Size | Description | Validation | Business Rules |
|-----------|------|------|-------------|------------|----------------|
| record-type | 9 | 1 | Record type identifier | 1-4 | 1=Company,2=Period,3=Numbers,4=Options |
| company-name | X | 40 | Company name | Required | Record 1 |
| company-addr1-5 | X | 30x5 | Company address | Required | Record 1 |
| tax-number | X | 20 | Company tax ID | Optional | Record 1 |
| current-period | 9 | 2 | Current period | 1-12/13 | Record 2 |
| current-year | 9 | 4 | Current year | Valid year | Record 2 |
| periods-per-year | 9 | 2 | Accounting periods | 12 or 13 | Record 2 |
| year-end-month | 9 | 2 | Year-end month | 1-12 | Record 2 |
| next-cust-no | 9 | 6 | Next customer number | Sequential | Record 3 |
| next-supp-no | 9 | 6 | Next supplier number | Sequential | Record 3 |
| next-inv-no | 9 | 8 | Next invoice number | Sequential | Record 3 |
| next-po-no | 9 | 8 | Next PO number | Sequential | Record 3 |
| base-currency | X | 3 | Base currency code | Valid currency | Record 4 |
| tax-rate | 9 | 2.2 | Default tax rate | 0-99.99 | Record 4 |
| date-format | X | 1 | Date display format | D,M,Y | DD/MM/YY,MM/DD/YY,YY/MM/DD |

### ANALFILE - Analysis Codes File
**Purpose**: Sales and purchase analysis codes  
**Organization**: Indexed  
**Primary Key**: Analysis Type, Analysis Code

| Field Name | Type | Size | Description | Validation | Business Rules |
|-----------|------|------|-------------|------------|----------------|
| anal-type | X | 1 | Analysis type | S/P | Sales or Purchase |
| anal-code | X | 4 | Analysis code | Unique within type | |
| anal-desc | X | 30 | Description | Required | |
| gl-account | 9 | 10 | Associated GL account | Valid account | For posting |
| report-seq | 9 | 3 | Report sequence | 1-999 | Sort order |
| active-flag | X | 1 | Active status | Y/N | |

## Archive Files

### SLARCH - Sales Ledger Archive
**Purpose**: Archived sales transactions  
**Organization**: Sequential  
**Structure**: Same as SLTRANS with additional archive-date field

### PLARCH - Purchase Ledger Archive
**Purpose**: Archived purchase transactions  
**Organization**: Sequential  
**Structure**: Same as PLTRANS with additional archive-date field

### GLARCH - General Ledger Archive
**Purpose**: Archived GL postings  
**Organization**: Sequential  
**Structure**: Same as GLPOST with additional archive-date and archive-period fields

## Work Files

### SLSORT - Sales Sort Work File
**Purpose**: Temporary file for report sorting  
**Organization**: Sequential  
**Usage**: Created and deleted during report generation

### PLSORT - Purchase Sort Work File
**Purpose**: Temporary file for report sorting  
**Organization**: Sequential  
**Usage**: Created and deleted during report generation

### GLWORK - GL Work File
**Purpose**: Period-end processing work file  
**Organization**: Sequential  
**Usage**: Temporary calculations during close

## Data Validation Rules

### Standard Validation Patterns

1. **Check Digit Validation**
   - Customer numbers: Modulus 11 check digit
   - Supplier numbers: Modulus 11 check digit
   - Algorithm: Sum(digit * weight) mod 11

2. **Date Validation**
   - Format: YYYYMMDD internal storage
   - Display format based on system parameter
   - Period/year validation against system date
   - No future dates unless specifically allowed

3. **Amount Fields**
   - Two decimal places standard
   - Sign indicators for credits/negatives
   - Range checking based on field size
   - Zero not allowed for transactions

4. **Code Validation**
   - Must exist in relevant master file
   - Status must be active
   - Cross-reference validation where applicable

5. **Batch Controls**
   - Hash totals
   - Record counts
   - Control totals must balance

## Business Code Meanings

### Transaction Type Codes
- **IN**: Sales Invoice
- **CN**: Sales Credit Note  
- **PM**: Payment Received
- **PI**: Purchase Invoice
- **PC**: Purchase Credit
- **PP**: Payment Made
- **JD**: Journal Debit
- **JC**: Journal Credit
- **AD**: Adjustment

### Status Codes
- **A**: Active
- **H**: Hold/Suspended
- **C**: Closed
- **I**: Inactive
- **D**: Deleted (logical)

### Tax Codes
- **S**: Standard Rate
- **Z**: Zero Rate
- **E**: Exempt
- **R**: Reduced Rate
- **N**: Not Applicable

### Account Types
- **A**: Asset
- **L**: Liability  
- **C**: Capital/Equity
- **I**: Income/Revenue
- **E**: Expense

## Data Retention

### Online Retention
- Current year plus one prior year
- Current period plus 12/13 prior periods
- Open transactions until fully allocated

### Archive Requirements
- 7 years for tax compliance
- Permanent for audit trail
- Annual archive after year-end close
- Compressed storage acceptable

## Integration Points

### Module Integration
- All modules post to GL through GLPOST file
- Stock movements update from sales/purchase
- Customer/supplier balances updated real-time
- Analysis codes shared across modules

### External Integration
- Bank statement import format defined
- EDI capability for orders/invoices
- Report export to CSV format
- Backup scripts for data protection