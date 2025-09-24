# IRS_PROC Interface Definitions

## Overview

The IRS_PROC subsystem has minimal integration points, making it an ideal candidate for early migration. Its primary interfaces are bank file imports and GL posting exports.

## Inbound Interfaces

### BANK-IRS-001: Bank Transaction Import

**Purpose**: Import bank statement transactions for categorization and posting

**Supported Formats**:
1. CSV (Comma Separated Values)
2. OFX (Open Financial Exchange)
3. Manual screen entry

**Frequency**: Daily/Weekly/Monthly (client-dependent)  
**Protocol**: File upload via UI or FTP  
**File Location**: `/imports/irs/bank/`

#### CSV Format Specification

**File Naming**: `BANK_[AccountNo]_YYYYMMDD.csv`

**Column Layout**:
```
Date,Description,Reference,Debit,Credit,Balance
```

**Sample Data**:
```csv
Date,Description,Reference,Debit,Credit,Balance
2024-03-15,DEPOSIT-SALES,DEP001,,5000.00,15000.00
2024-03-15,CHECK 1234,CHK1234,1250.00,,13750.00
2024-03-16,BANK FEE,FEE001,25.00,,13725.00
2024-03-16,TRANSFER FROM SAVINGS,TFR001,,10000.00,23725.00
```

**Field Specifications**:
- **Date**: YYYY-MM-DD format
- **Description**: Free text, max 100 characters
- **Reference**: Bank's reference number, max 20 characters
- **Debit**: Amount if money out (2 decimal places)
- **Credit**: Amount if money in (2 decimal places)
- **Balance**: Running balance after transaction

**Validation Rules**:
1. Date must be valid and within open periods
2. Either Debit or Credit must have value (not both)
3. Amounts must be positive
4. Balance must reconcile (previous + credit - debit)
5. No duplicate references within same account

#### OFX Format Specification

**Standard**: OFX 2.0 compliant

**Sample Structure**:
```xml
<?xml version="1.0" encoding="UTF-8"?>
<OFX>
  <BANKMSGSRSV1>
    <STMTTRNRS>
      <STMTRS>
        <BANKTRANLIST>
          <STMTTRN>
            <TRNTYPE>CREDIT</TRNTYPE>
            <DTPOSTED>20240315</DTPOSTED>
            <TRNAMT>5000.00</TRNAMT>
            <FITID>DEP001</FITID>
            <NAME>SALES DEPOSIT</NAME>
          </STMTTRN>
        </BANKTRANLIST>
      </STMTRS>
    </STMTTRNRS>
  </BANKMSGSRSV1>
</OFX>
```

### MAN-IRS-001: Manual Transaction Entry

**Purpose**: Allow manual entry of transactions not in bank feed

**Protocol**: Screen-based entry  
**Program**: irs010

**Screen Layout**:
```
IRS010          MANUAL TRANSACTION ENTRY          03/15/24

Bank Account: [__________]  Period: [MM/YYYY]

Date: [__/__/__]  Reference: [____________]
Description: [________________________________________]
Amount: [___________]  Type: ( ) Receipt  ( ) Payment
Category: [____] [________________________]
Tax Rate: [__]%   Tax Amount: [___________]

F1=Help F3=Exit F5=Save F6=Categories F10=Import Bank
```

## Outbound Interfaces

### IRS-GL-001: General Ledger Posting

**Purpose**: Post summarized journal entries to General Ledger

**Frequency**: Weekly (Mondays at 06:00)  
**Protocol**: Batch file transfer  
**File Location**: `/interfaces/irs-to-gl/`  
**File Naming**: `IRS_GL_YYYYMMDD_HHMMSS.txt`

**File Format**:
```
HEADER|IRSGL|20240315|060000|001|000025|0000045678.90
DETAIL|JE|20240315|4000|1999||000015000.00|IRS001|Sales Income
DETAIL|JE|20240315|1999||4000|000015000.00||Retained Earnings
DETAIL|JE|20240315|5100|1999||000008500.00|IRS002|Operating Expenses
DETAIL|JE|20240315|1999||5100|000008500.00||Retained Earnings
TRAILER|000004|000023500.00|000023500.00|0000000000
```

**Special Processing**:
- All entries use account 1999 (Retained Earnings) as the balancing account
- Categories are mapped to appropriate P&L accounts
- Weekly batch combines all transactions for the period

### IRS-RPT-001: Financial Reports

**Purpose**: Generate simplified financial reports

**Frequency**: On-demand  
**Format**: PDF or Excel  
**Delivery**: Screen display, email, or file download

**Available Reports**:

1. **Income Statement (Simplified)**
   ```
   INCOME STATEMENT - MARCH 2024
   
   INCOME
   Sales Revenue             15,000.00
   Other Income               1,000.00
   TOTAL INCOME              16,000.00
   
   EXPENSES  
   Rent                       2,500.00
   Salaries                   8,000.00
   Other Expenses            1,500.00
   TOTAL EXPENSES           12,000.00
   
   NET INCOME                4,000.00
   ```

2. **Cash Flow Summary**
   ```
   CASH FLOW - MARCH 2024
   
   Opening Balance          10,000.00
   
   RECEIPTS
   Customer Payments        15,000.00
   Other Receipts           1,000.00
   Total Receipts          16,000.00
   
   PAYMENTS
   Supplier Payments        8,000.00
   Operating Expenses       4,000.00
   Total Payments          12,000.00
   
   NET MOVEMENT             4,000.00
   Closing Balance         14,000.00
   ```

### IRS-TAX-001: Tax Returns

**Purpose**: Generate VAT/Sales tax returns

**Frequency**: Monthly/Quarterly  
**Format**: XML or CSV per tax authority requirements

**Sample VAT Return Format**:
```xml
<VATReturn>
  <Period>202403</Period>
  <BusinessID>12345678</BusinessID>
  <Sales>
    <Standard>
      <Net>50000.00</Net>
      <Tax>10000.00</Tax>
    </Standard>
    <Zero>
      <Net>5000.00</Net>
      <Tax>0.00</Tax>
    </Zero>
  </Sales>
  <Purchases>
    <Standard>
      <Net>30000.00</Net>
      <Tax>6000.00</Tax>
    </Standard>
  </Purchases>
  <NetTax>4000.00</NetTax>
</VATReturn>
```

## Processing Rules

### Transaction Categorization

**Auto-Categorization Rules**:

| Description Contains | Category | GL Account |
|---------------------|----------|-----------|
| SALES, DEPOSIT | Sales Income | 4000 |
| RENT | Rent Expense | 5200 |
| SALARY, WAGES | Payroll | 5300 |
| UTILITY, ELECTRIC | Utilities | 5400 |
| BANK FEE, CHARGE | Bank Charges | 5900 |

**Manual Override**: User can change auto-assigned categories

### Tax Extraction

**For Tax-Inclusive Amounts**:
```
Gross Amount = 120.00
Tax Rate = 20%
Tax Amount = 120.00 × (20 ÷ 120) = 20.00
Net Amount = 100.00
```

## Error Handling

### Import Errors

| Error Type | Handling | User Action |
|-----------|----------|-------------|
| Invalid date | Skip record | Correct in source |
| Duplicate reference | Skip record | Verify not duplicate |
| Out of balance | Reject file | Check file integrity |
| Unknown format | Reject file | Use supported format |

### Posting Errors

| Error Type | Handling | User Action |
|-----------|----------|-------------|
| Uncategorized trans | Block posting | Assign categories |
| Period not open | Delay posting | Wait for period open |
| Invalid GL mapping | Skip category | Fix mapping |

## Reconciliation Process

### Bank Reconciliation Interface

**Screen Layout**:
```
IRS020          BANK RECONCILIATION             03/15/24

Bank Account: First National    Period: 03/2024

Bank Statement Balance:         14,000.00
                                
Book Balance:                   14,500.00
Outstanding Checks:               -500.00
Deposits in Transit:                0.00
                                ---------
Adjusted Book Balance:          14,000.00

Difference:                         0.00

F1=Help F3=Exit F5=Complete F7=Outstanding Items
```

## Testing Procedures

### Test Data Sets

1. **Simple Import** (10 transactions)
   - Mix of income and expenses
   - All categorizable

2. **Complex Import** (100 transactions)  
   - Includes transfers
   - Some uncategorizable
   - Tax transactions

3. **Error Scenarios**
   - Duplicate references
   - Invalid dates
   - Unbalanced file

### Test Validation

- Import processes without errors
- Categories correctly assigned
- Tax calculations accurate
- GL posting balances
- Reports match source data