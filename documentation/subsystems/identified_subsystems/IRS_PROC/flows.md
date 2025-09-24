# IRS_PROC Process Flows

## Overview

The IRS_PROC subsystem follows a simplified flow designed for ease of use by non-accountants. All processes focus on converting single-entry records into proper double-entry bookkeeping.

## Major Process Flows

### 1. Bank Import and Categorization Flow

**Purpose**: Import bank transactions and automatically categorize them

```mermaid
flowchart TD
    A[Bank File Available] --> B{File Format?}
    B -->|CSV| C[Parse CSV]
    B -->|OFX| D[Parse OFX]
    B -->|Unknown| E[Reject File]
    
    C --> F[Validate Transactions]
    D --> F
    
    F --> G{Valid?}
    G -->|No| H[Error Report]
    G -->|Yes| I[Check Duplicates]
    
    I --> J{Duplicates?}
    J -->|Yes| K[Skip Duplicates]
    J -->|No| L[Apply Category Rules]
    K --> L
    
    L --> M[Auto-Categorize]
    M --> N{All Categorized?}
    N -->|No| O[Flag for Manual Review]
    N -->|Yes| P[Ready for Posting]
    O --> P
    
    P --> Q[Save Transactions]
    Q --> R[Update Running Balance]
```

**Key Rules**:
- Check against existing references to prevent duplicates
- Apply categorization rules based on description patterns
- Flag uncategorized items for user review
- Maintain running balance for reconciliation

---

### 2. Manual Transaction Entry Flow

**Purpose**: Allow direct entry of transactions not in bank feed

```mermaid
stateDiagram-v2
    [*] --> Entry: Start Entry
    Entry --> Validation: Save
    
    Validation --> Entry: Errors
    Validation --> Categorized: Valid
    
    Categorized --> TaxCalc: Has Tax
    Categorized --> Saved: No Tax
    
    TaxCalc --> Saved: Calculate Tax
    
    Saved --> [*]: Complete
```

**Validation Steps**:
1. Date within allowed periods
2. Amount is non-zero
3. Category is valid
4. Reference is unique

---

### 3. Automatic Double-Entry Generation

**Purpose**: Convert single-entry records to balanced journal entries

```mermaid
flowchart LR
    A[Categorized Transaction] --> B{Transaction Type}
    
    B -->|Income| C[Credit Income Account]
    B -->|Expense| D[Debit Expense Account]
    
    C --> E[Debit IRS Control]
    D --> F[Credit IRS Control]
    
    E --> G[Balanced Entry]
    F --> G
    
    G --> H[Ready for GL]
```

**Examples**:

Income Transaction:
```
Single Entry: Sales $1,000
Double Entry: 
  DR IRS Control Account    1,000
  CR Sales Income                   1,000
```

Expense Transaction:
```
Single Entry: Rent $500
Double Entry:
  DR Rent Expense          500
  CR IRS Control Account          500
```

---

### 4. Bank Reconciliation Flow

**Purpose**: Match book records to bank statement

```mermaid
flowchart TD
    A[Start Reconciliation] --> B[Get Bank Statement Balance]
    B --> C[Get Book Balance]
    C --> D[Identify Uncleared Items]
    
    D --> E[Outstanding Checks]
    D --> F[Deposits in Transit]
    D --> G[Bank Charges Not Recorded]
    
    E --> H[Calculate Adjusted Book Balance]
    F --> H
    G --> H
    
    H --> I{Balances Match?}
    I -->|Yes| J[Mark Reconciled]
    I -->|No| K[Investigation Required]
    
    K --> L[Find Discrepancies]
    L --> M[Make Adjustments]
    M --> I
    
    J --> N[Lock Period]
```

**Reconciliation Rules**:
- Book balance +/- timing differences = Bank balance
- All discrepancies must be explained
- Period cannot close until reconciled

---

### 5. Weekly GL Posting Flow

**Purpose**: Post accumulated transactions to General Ledger

```mermaid
sequenceDiagram
    participant IRS as IRS_PROC
    participant Val as Validation
    participant Gen as Entry Generator
    participant GL as GL_CORE
    
    Note over IRS: Monday 06:00
    
    IRS->>Val: Get Week's Transactions
    Val->>Val: Check All Categorized
    
    alt Uncategorized Exist
        Val->>IRS: Block Posting
        IRS->>IRS: Alert User
    else All Categorized
        Val->>Gen: Process Transactions
        Gen->>Gen: Group by Category
        Gen->>Gen: Create Journal Entries
        Gen->>GL: Send Batch File
        GL->>IRS: Confirmation
        IRS->>IRS: Mark Posted
    end
```

**Posting Rules**:
- Only reconciled periods can post
- All transactions must be categorized
- Batch by GL account for efficiency
- Maintain audit trail

---

### 6. VAT/Tax Processing Flow

**Purpose**: Extract tax from gross amounts and track liability

```mermaid
flowchart TD
    A[Transaction with Tax] --> B{Tax Inclusive?}
    B -->|Yes| C[Extract Tax Amount]
    B -->|No| D[Add Tax Amount]
    
    C --> E[Calculate Net]
    D --> F[Calculate Gross]
    
    E --> G[Track Tax Liability]
    F --> G
    
    G --> H{Period End?}
    H -->|No| I[Accumulate]
    H -->|Yes| J[Generate Tax Return]
    
    J --> K[Submit to Authority]
```

**Tax Calculations**:
```
Tax Inclusive:
  Gross = 120.00
  Rate = 20%
  Tax = 120 × (20 ÷ 120) = 20.00
  Net = 100.00

Tax Exclusive:
  Net = 100.00
  Rate = 20%  
  Tax = 100 × 0.20 = 20.00
  Gross = 120.00
```

---

### 7. Report Generation Flow

**Purpose**: Create simplified financial reports

```mermaid
flowchart LR
    A[Report Request] --> B{Report Type}
    
    B -->|P&L| C[Sum Income/Expense]
    B -->|Cash Flow| D[Track Money Movement]
    B -->|Tax Summary| E[Extract Tax Data]
    
    C --> F[Apply Report Format]
    D --> F
    E --> F
    
    F --> G{Output Format?}
    G -->|PDF| H[Generate PDF]
    G -->|Excel| I[Generate Excel]
    
    H --> J[Deliver Report]
    I --> J
```

**Report Features**:
- Simple terminology (no accounting jargon)
- Clear categorization
- Period comparisons
- Tax summaries

---

## Error Handling Flows

### Import Error Handling

```mermaid
flowchart TD
    A[Import Error] --> B{Error Type}
    
    B -->|Format| C[Reject File]
    B -->|Duplicate| D[Skip Record]
    B -->|Invalid Data| E[Flag Record]
    
    C --> F[User Notification]
    D --> G[Continue Processing]
    E --> G
    
    G --> H[Error Summary]
    F --> I[Require New File]
```

### Categorization Error Handling

```mermaid
flowchart LR
    A[Uncategorized Transaction] --> B[Add to Review Queue]
    B --> C[User Reviews]
    C --> D{Assign Category?}
    
    D -->|Yes| E[Update Rules]
    D -->|No| F[Use Miscellaneous]
    
    E --> G[Apply to Similar]
    F --> H[Process Transaction]
    G --> H
```

---

## Integration Points

### With GL_CORE

**Weekly Posting Cycle**:
```
Sunday 23:59 - Week ends
Monday 00:00 - New week begins  
Monday 05:00 - Reconciliation check
Monday 06:00 - Generate GL entries
Monday 06:30 - Post to GL
Monday 07:00 - Confirmation received
```

### With External Systems

**Bank Integration**:
- Daily: Auto-download (if configured)
- Manual: File upload
- Format: CSV or OFX

**Tax Authority**:
- Monthly/Quarterly: Generate returns
- Format: Authority-specific XML/CSV

---

## Performance Optimization

### Bulk Processing Strategy

```mermaid
flowchart TD
    A[100+ Transactions] --> B[Group by Type]
    B --> C[Income Batch]
    B --> D[Expense Batch]
    B --> E[Transfer Batch]
    
    C --> F[Process in Parallel]
    D --> F
    E --> F
    
    F --> G[Merge Results]
    G --> H[Single GL Batch]
```

**Benefits**:
- Faster categorization
- Efficient GL posting
- Better error isolation

---

## User Decision Points

### Key Decisions

| Decision | Options | Impact |
|----------|---------|--------|
| Category Unknown | Assign manually or use misc | Affects reports |
| Duplicate Found | Skip or force import | Data integrity |
| Out of Balance | Investigate or adjust | Reconciliation |
| Tax Treatment | Include or exempt | Tax liability |

### Automation Settings

| Setting | Default | Options |
|---------|---------|---------|
| Auto-categorize | Enabled | On/Off |
| Duplicate handling | Skip | Skip/Import |
| GL posting | Weekly | Weekly/Monthly |
| Tax extraction | Enabled | On/Off |