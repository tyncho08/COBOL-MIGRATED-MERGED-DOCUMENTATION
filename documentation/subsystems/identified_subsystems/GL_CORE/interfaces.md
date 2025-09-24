# GL_CORE Interface Definitions

## Overview

This document provides detailed specifications for all interfaces to and from the GL_CORE subsystem. Each interface includes data formats, validation rules, error handling, and sample data.

## Inbound Interfaces

### AR-GL-001: Sales and Receipts Posting

**Purpose**: Post daily sales revenue and cash receipts from Accounts Receivable

**Frequency**: Daily at 18:00  
**Protocol**: Batch file transfer  
**File Location**: `/interfaces/ar-to-gl/`  
**File Naming**: `AR_GL_YYYYMMDD_HHMMSS.txt`

**File Format**:
```
HEADER|ARGL|20240315|183000|001|000245|0000125432.50
DETAIL|JE|20240315|4000|1200||000015432.50|INV#12345|Daily Sales
DETAIL|JE|20240315|1100||1200|000015432.50||AR from Sales
DETAIL|CR|20240315|1100||1000|000010000.00|REC#54321|Customer Payment
DETAIL|CR|20240315|1000|1100||000010000.00||Cash Receipt
TRAILER|000004|000025432.50|000025432.50|0000000000
```

**Field Definitions**:

Header Record:
- Record Type: "HEADER"
- Interface ID: "ARGL"
- Process Date: YYYYMMDD
- Process Time: HHMMSS
- Batch Number: 3 digits
- Record Count: 6 digits
- Control Total: 13 digits (2 decimals)

Detail Record:
- Record Type: "DETAIL"
- Journal Type: JE=Journal Entry, CR=Cash Receipt
- Transaction Date: YYYYMMDD
- Account Number: 4 digits
- Debit Amount: 13 digits (2 decimals)
- Credit Amount: 13 digits (2 decimals)
- Reference: 20 characters
- Description: 50 characters

Trailer Record:
- Record Type: "TRAILER"
- Detail Count: 6 digits
- Total Debits: 13 digits (2 decimals)
- Total Credits: 13 digits (2 decimals)
- Hash Total: 10 digits

**Validation Rules**:
1. Header date must match current processing date
2. Batch number must be unique for the date
3. Detail count must match actual records
4. Debits must equal credits
5. All account numbers must exist in Chart of Accounts
6. Amounts must be positive
7. Reference must be unique within batch

**Error Handling**:
- File format errors: Reject entire file
- Validation errors: Create error report, process valid records
- Missing file: Alert after 30 minutes past expected time

---

### AP-GL-001: Purchases and Payments Posting

**Purpose**: Post daily purchase invoices and vendor payments

**Frequency**: Daily at 18:30  
**Protocol**: Batch file transfer  
**File Location**: `/interfaces/ap-to-gl/`  
**File Naming**: `AP_GL_YYYYMMDD_HHMMSS.txt`

**File Format**:
```
HEADER|APGL|20240315|183000|001|000187|0000087654.32
DETAIL|PI|20240315|5000|2000||000012345.67|PO#98765|Office Supplies
DETAIL|PI|20240315|2000||5000|000012345.67||AP from Purchase
DETAIL|PP|20240315|2000||1000|000008765.43|CHK#11111|Vendor Payment
DETAIL|PP|20240315|1000|2000||000008765.43||Cash Disbursement
TRAILER|000004|000021111.10|000021111.10|0000000000
```

**Journal Types**:
- PI = Purchase Invoice
- PP = Purchase Payment
- PD = Purchase Discount
- PR = Purchase Return

---

### INV-GL-001: Inventory Movement Posting

**Purpose**: Post inventory transactions affecting GL accounts

**Frequency**: Daily at 19:00  
**Protocol**: Batch file transfer  
**File Location**: `/interfaces/inv-to-gl/`

**File Format**:
```
HEADER|INVGL|20240315|190000|001|000089|0000045678.90
DETAIL|RC|20240315|1400|2000||000010000.00|GRN#55555|Goods Receipt
DETAIL|IS|20240315|5100||1400|000007500.00|ISS#66666|Cost of Goods Sold
DETAIL|AJ|20240315|1400||5200|000001000.00|ADJ#77777|Inventory Adjustment
TRAILER|000003|000018500.00|000018500.00|0000000000
```

**Journal Types**:
- RC = Receipt
- IS = Issue  
- AJ = Adjustment
- TR = Transfer

---

### IRS-GL-001: Simplified Bookkeeping Posting

**Purpose**: Post transactions from Incomplete Records System

**Frequency**: Weekly (Mondays at 06:00)  
**Protocol**: Batch file transfer  
**File Location**: `/interfaces/irs-to-gl/`

**Special Format**: Simplified with automatic account determination

---

### MAN-GL-001: Manual Journal Entry

**Purpose**: Direct entry of journal vouchers

**Protocol**: Real-time screen entry  
**Program**: gl020

**Screen Layout**:
```
GL020                JOURNAL ENTRY                    03/15/24

Journal No: [______]  Date: [__/__/__]  Type: [_]  Reference: [____________]

Line Account Description              Debit         Credit     Cost Center
---- ------- --------------------- ------------- ------------- -----------
01   [____] [___________________] [___________] [___________] [___]
02   [____] [___________________] [___________] [___________] [___]
03   [____] [___________________] [___________] [___________] [___]

                          Totals: [___________] [___________]

F1=Help F3=Exit F5=Post F7=List Accounts F10=Save Draft
```

## Outbound Interfaces

### GL-RPT-001: Balance Extract for Reporting

**Purpose**: Provide account balances for financial statement generation

**Frequency**: On-demand  
**Protocol**: File generation  
**Requestor**: RPT_ENGINE

**File Format**:
```
HEADER|GLBAL|20240315|YYYY|03|001
BALANCE|1000|Cash|A|01|000010000.00|000008000.00|000002000.00
BALANCE|1100|Accounts Receivable|A|01|000025000.00|000020000.00|000005000.00
BALANCE|4000|Sales Revenue|R|01|-000050000.00|-000040000.00|-000010000.00
TRAILER|000150|0000125000.00|0000125000.00
```

Fields:
- Account, Name, Type (A=Asset, L=Liability, R=Revenue, E=Expense)
- Department/Cost Center
- YTD Actual, YTD Budget, Variance

---

### GL-RPT-002: Transaction Detail Extract

**Purpose**: Provide journal entry details for transaction reporting

**Frequency**: On-demand  
**Parameters**: Date range, account range, journal type

**Format**: Similar to inbound with additional posted status fields

---

### GL-AUD-001: Audit Trail Feed

**Purpose**: Real-time feed of all GL changes for audit logging

**Protocol**: Message queue / Log file  
**Frequency**: Real-time

**Format**:
```
TIMESTAMP|USER|PROGRAM|ACTION|BEFORE|AFTER
20240315183001|JSMITH|gl020|INSERT|null|{journal entry data}
20240315183005|SYSTEM|gl030|UPDATE|{old balance}|{new balance}
```

## Interface Processing Logic

### Posting Sequence

1. **Validation Phase**
   - Verify file format
   - Check control totals
   - Validate account numbers
   - Verify period is open

2. **Processing Phase**
   - Create journal header
   - Process detail lines
   - Update account balances
   - Create audit records

3. **Confirmation Phase**
   - Generate confirmation file
   - Update interface log
   - Send notifications

### Error Recovery

**Partial Processing**:
- Process valid records
- Quarantine errors
- Generate exception report
- Allow manual correction and resubmission

**Full Rejection**:
- File format errors
- Control total mismatch
- Closed period posting
- Return entire file with error codes

## Interface Monitoring

### Key Metrics

| Metric | Target | Alert Threshold |
|--------|--------|----------------|
| File Arrival Time | On schedule | +30 minutes |
| Processing Time | <10 minutes | >20 minutes |
| Error Rate | <1% | >5% |
| Record Volume | Normal +/- 20% | >50% variance |

### Health Checks

1. **Pre-Processing**
   - File present
   - File readable
   - Header valid

2. **During Processing**
   - Record counter
   - Balance accumulator
   - Error counter

3. **Post-Processing**
   - Control reconciliation
   - Balance verification
   - Confirmation generation

## Testing Procedures

### Test Scenarios

1. **Happy Path**
   - Valid file with balanced entries
   - All accounts exist
   - Period is open

2. **Error Cases**
   - Unbalanced journal
   - Invalid account
   - Closed period
   - Duplicate reference
   - File format error

3. **Edge Cases**
   - Maximum file size
   - Single record file
   - Zero amount entries
   - Year-end processing

### Test Data Sets

Maintained in: `/test-data/gl-interfaces/`
- Small (10 records)
- Medium (1,000 records)  
- Large (10,000 records)
- Error scenarios