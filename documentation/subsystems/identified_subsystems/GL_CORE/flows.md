# GL_CORE Process Flows

## Overview

This document details the key business process flows within the GL_CORE subsystem, showing how data moves through the system and how different components interact to deliver general ledger functionality.

## Major Process Flows

### 1. Daily Journal Posting Flow

**Purpose**: Process journal entries from all sources into the general ledger

```mermaid
flowchart TD
    A[Start Daily Posting] --> B{Check Period Status}
    B -->|Closed| C[Reject Posting]
    B -->|Open| D[Validate Batch Files]
    
    D --> E{Files Valid?}
    E -->|No| F[Generate Error Report]
    E -->|Yes| G[Process AR Batch]
    
    G --> H[Process AP Batch]
    H --> I[Process INV Batch]
    I --> J[Process Manual Entries]
    
    J --> K[Update Account Balances]
    K --> L[Create Audit Trail]
    L --> M[Generate Confirmations]
    M --> N[Update Interface Log]
    N --> O[End]
    
    F --> P[Send to Error Queue]
    P --> Q[Manual Correction]
    Q --> D
```

**Key Steps**:
1. Verify current period is open for posting
2. Collect all interface files from subsystems
3. Validate file formats and control totals
4. Process each batch sequentially
5. Update running account balances
6. Create comprehensive audit trail
7. Send confirmations back to source systems

---

### 2. Period-End Closing Flow

**Purpose**: Close accounting period and prepare for next period

```mermaid
flowchart LR
    A[Initiate Period Close] --> B{All Days Posted?}
    B -->|No| C[Complete Daily Postings]
    B -->|Yes| D{Sub-ledgers Closed?}
    
    C --> D
    D -->|No| E[Wait for Sub-ledgers]
    D -->|Yes| F[Run Trial Balance]
    
    F --> G{Balanced?}
    G -->|No| H[Investigate Differences]
    G -->|Yes| I[Post Accruals]
    
    H --> I
    I --> J[Post Adjustments]
    J --> K[Final Trial Balance]
    K --> L[Lock Period]
    L --> M[Open Next Period]
    M --> N[Generate Reports]
    N --> O[Distribute Financials]
```

**Validation Points**:
- All daily batches processed
- All sub-ledgers reported closed
- Trial balance in balance
- No pending manual entries
- Adjusting entries posted

---

### 3. Manual Journal Entry Flow

**Purpose**: Allow authorized users to create manual journal entries

```mermaid
stateDiagram-v2
    [*] --> Draft: Create Entry
    Draft --> Validation: Submit
    
    Validation --> Draft: Errors Found
    Validation --> PendingApproval: Valid Entry
    
    PendingApproval --> Approved: Authorize
    PendingApproval --> Rejected: Deny
    
    Rejected --> Draft: Revise
    Rejected --> Cancelled: Abandon
    
    Approved --> Posted: Post to GL
    Posted --> [*]: Complete
    
    Cancelled --> [*]: End
```

**Business Rules**:
- Entries must balance (debits = credits)
- Require business justification
- Approval based on amount thresholds
- No posting to control accounts
- Supporting documentation required

---

### 4. Financial Statement Generation Flow

**Purpose**: Generate standard financial reports

```mermaid
flowchart TD
    A[Request Report] --> B{Select Report Type}
    B -->|Balance Sheet| C[Extract Asset/Liability Balances]
    B -->|Income Statement| D[Extract Revenue/Expense Balances]
    B -->|Trial Balance| E[Extract All Balances]
    
    C --> F[Apply Report Layout]
    D --> F
    E --> F
    
    F --> G[Calculate Subtotals]
    G --> H[Apply Formatting]
    H --> I{Multi-Company?}
    
    I -->|Yes| J[Consolidate Companies]
    I -->|No| K[Single Company Report]
    
    J --> L[Eliminate Intercompany]
    L --> K
    K --> M[Generate Output]
    M --> N[Deliver Report]
```

**Report Types**:
- Balance Sheet (Financial Position)
- Income Statement (P&L)
- Cash Flow Statement
- Trial Balance
- General Journal
- Account Analysis

---

### 5. Budget vs Actual Processing Flow

**Purpose**: Compare actual results against budget

```mermaid
flowchart LR
    A[Budget Entry] --> B[(Budget Database)]
    C[Actual Postings] --> D[(GL Balances)]
    
    B --> E[Budget vs Actual Process]
    D --> E
    
    E --> F{Calculate Variances}
    F --> G[Favorable Variances]
    F --> H[Unfavorable Variances]
    
    G --> I[Variance Report]
    H --> I
    
    I --> J{Threshold Exceeded?}
    J -->|Yes| K[Alert Management]
    J -->|No| L[Standard Distribution]
    
    K --> L
```

**Variance Calculations**:
- Revenue: Actual > Budget = Favorable
- Expense: Actual < Budget = Favorable
- Calculate both $ and % variances
- Year-to-date comparisons

---

### 6. Year-End Processing Flow

**Purpose**: Close fiscal year and prepare for new year

```mermaid
flowchart TD
    A[Start Year-End] --> B[Close Period 12]
    B --> C{Process Adjustments?}
    C -->|Yes| D[Open Period 13]
    C -->|No| E[Skip to Rollover]
    
    D --> F[Post Year-End Adjustments]
    F --> G[Close Period 13]
    G --> E
    
    E --> H[Calculate Net Income]
    H --> I[Close Revenue Accounts]
    I --> J[Close Expense Accounts]
    J --> K[Update Retained Earnings]
    K --> L[Create Opening Balances]
    L --> M[Open New Year]
    M --> N[Generate Annual Reports]
    N --> O[Archive Old Year]
    O --> P[End]
```

**Special Considerations**:
- Period 13 for adjustments
- Automatic retained earnings update
- Balance sheet accounts carry forward
- P&L accounts start at zero

---

### 7. Inter-Company Processing Flow

**Purpose**: Handle transactions between companies

```mermaid
sequenceDiagram
    participant Co1 as Company 1
    participant IC as Intercompany Logic
    participant Co2 as Company 2
    participant Elim as Elimination
    
    Co1->>IC: Post IC Sale
    IC->>Co1: Debit IC Receivable
    IC->>Co2: Credit IC Payable
    IC->>Co2: Debit IC Purchase
    
    Note over IC: Validate Matching
    
    IC->>Elim: Flag for Consolidation
    
    Note over Elim: During Consolidation
    Elim->>Elim: Remove IC Balances
    Elim->>Elim: Net to Zero
```

**Controls**:
- Matching IC account pairs
- Automatic balancing entries
- Consolidation elimination
- Mismatch reporting

---

## Error Handling Flows

### Journal Entry Errors

```mermaid
flowchart LR
    A[Error Detected] --> B{Error Type}
    B -->|Out of Balance| C[Return to User]
    B -->|Invalid Account| D[Show Valid List]
    B -->|Closed Period| E[Suggest Open Period]
    B -->|No Authority| F[Route for Approval]
    
    C --> G[Correct Entry]
    D --> G
    E --> G
    F --> H[Wait for Approval]
    
    G --> I[Resubmit]
    H --> I
```

### Batch Processing Errors

```mermaid
flowchart TD
    A[Batch Error] --> B{Severity}
    B -->|Critical| C[Reject Entire Batch]
    B -->|Warning| D[Process Valid Records]
    
    C --> E[Return to Source]
    D --> F[Quarantine Errors]
    
    E --> G[Source Corrects]
    F --> H[Manual Review]
    
    G --> I[Resubmit Batch]
    H --> J[Individual Correction]
    
    I --> K[Reprocess]
    J --> K
```

---

## Performance Optimization Flows

### Parallel Processing Strategy

```mermaid
flowchart TD
    A[Batch Files Received] --> B[File Validation]
    B --> C{Split by Type}
    
    C --> D[AR Stream]
    C --> E[AP Stream]
    C --> F[INV Stream]
    
    D --> G[Process AR]
    E --> H[Process AP]
    F --> I[Process INV]
    
    G --> J[Merge Results]
    H --> J
    I --> J
    
    J --> K[Update Balances]
    K --> L[Complete]
```

**Benefits**:
- Reduced processing time
- Better resource utilization
- Isolated error handling
- Improved throughput

---

## Integration Points

### With Other Subsystems

1. **AR_MGMT Integration**
   - Daily revenue postings
   - Cash receipt postings
   - Credit memo adjustments

2. **AP_MGMT Integration**
   - Purchase postings
   - Payment postings
   - Expense accruals

3. **INV_CTRL Integration**
   - Cost of goods sold
   - Inventory adjustments
   - Overhead allocations

4. **RPT_ENGINE Integration**
   - Balance extracts
   - Transaction details
   - Report parameters

### Timing Dependencies

```
Daily Schedule:
18:00 - AR batch arrives
18:30 - AP batch arrives  
19:00 - INV batch arrives
19:30 - Begin GL processing
20:30 - Complete posting
21:00 - Reports available
```

---

## Key Decision Points

### Posting Decisions

| Decision | Criteria | Action |
|----------|----------|--------|
| Accept Entry? | Balanced, valid accounts | Post or reject |
| Which Period? | Transaction date vs period | Assign to period |
| Approval Needed? | Amount threshold | Route or auto-post |
| Reversal Required? | Error found after posting | Create reversing entry |

### Reporting Decisions

| Decision | Criteria | Action |
|----------|----------|--------|
| Include Company? | Active status | Include in consolidation |
| Eliminate IC? | Consolidation report | Remove IC transactions |
| Show Details? | Report parameter | Summary or detail |
| Apply Security? | User role | Filter data |