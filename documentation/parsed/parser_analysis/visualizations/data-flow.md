# Data Flow Visualization

## Overview
This visualization shows how data flows through the ACAS system, including file access patterns, data transformations, and transaction boundaries.

## System-Wide Data Flow

```mermaid
graph TB
    subgraph "External Inputs"
        UserInput[User Input]
        BatchFiles[Batch Files]
        BankData[Bank Data]
    end
    
    subgraph "Entry Programs"
        SalesEntry[Sales Entry<br/>sl020]
        PurchaseEntry[Purchase Entry<br/>pl020]
        StockEntry[Stock Entry<br/>st020]
        GLEntry[GL Entry<br/>gl010]
    end
    
    subgraph "Master Files"
        CustomerMaster[(Customer Master<br/>SLMASTER)]
        SupplierMaster[(Supplier Master<br/>PLMASTER)]
        StockMaster[(Stock Master<br/>STMASTER)]
        NominalMaster[(Nominal Master<br/>GLMASTER)]
    end
    
    subgraph "Transaction Files"
        SalesTrans[(Sales Trans<br/>SLTRANS)]
        PurchTrans[(Purchase Trans<br/>PLTRANS)]
        StockTrans[(Stock Trans<br/>STTRANS)]
        GLTrans[(GL Trans<br/>GLTRANS)]
    end
    
    subgraph "Processing Programs"
        SalesProcess[Sales Processing<br/>sl810, sl910]
        PurchProcess[Purchase Processing<br/>pl810, pl910]
        StockProcess[Stock Processing<br/>st030]
        GLProcess[GL Processing<br/>gl030, gl050]
    end
    
    subgraph "Integration"
        PostingEngine[Posting Engine<br/>GLPOST]
        AuditTrail[Audit Trail<br/>AUDIT]
    end
    
    subgraph "Outputs"
        Reports[Reports]
        Invoices[Invoices]
        Statements[Statements]
        GLReports[GL Reports]
    end
    
    %% Input flows
    UserInput --> SalesEntry
    UserInput --> PurchaseEntry
    UserInput --> StockEntry
    UserInput --> GLEntry
    BatchFiles --> SalesEntry
    BatchFiles --> PurchaseEntry
    BankData --> GLEntry
    
    %% Entry to Master
    SalesEntry --> CustomerMaster
    SalesEntry --> SalesTrans
    PurchaseEntry --> SupplierMaster
    PurchaseEntry --> PurchTrans
    StockEntry --> StockMaster
    StockEntry --> StockTrans
    GLEntry --> NominalMaster
    GLEntry --> GLTrans
    
    %% Transaction Processing
    SalesTrans --> SalesProcess
    CustomerMaster --> SalesProcess
    PurchTrans --> PurchProcess
    SupplierMaster --> PurchProcess
    StockTrans --> StockProcess
    StockMaster --> StockProcess
    GLTrans --> GLProcess
    NominalMaster --> GLProcess
    
    %% Integration flows
    SalesProcess --> PostingEngine
    PurchProcess --> PostingEngine
    StockProcess --> PostingEngine
    PostingEngine --> GLTrans
    PostingEngine --> AuditTrail
    
    %% Output generation
    SalesProcess --> Invoices
    SalesProcess --> Reports
    PurchProcess --> Statements
    PurchProcess --> Reports
    StockProcess --> Reports
    GLProcess --> GLReports
```

## File Access Patterns (CRUD Matrix)

| Program | Customer | Supplier | Stock | Nominal | Trans | Audit |
|---------|----------|----------|-------|---------|-------|-------|
| sl010   | CRUD     | -        | R     | R       | -     | W     |
| sl020   | RU       | -        | RU    | R       | CRU   | W     |
| sl810   | R        | -        | R     | RU      | RUD   | W     |
| sl910   | R        | -        | -     | R       | R     | -     |
| pl010   | -        | CRUD     | R     | R       | -     | W     |
| pl020   | -        | RU       | RU    | R       | CRU   | W     |
| pl810   | -        | R        | R     | RU      | RUD   | W     |
| st010   | -        | -        | CRUD  | R       | -     | W     |
| st020   | -        | -        | RU    | R       | CRU   | W     |
| st030   | -        | -        | R     | R       | R     | -     |
| gl010   | -        | -        | -     | CRUD    | -     | W     |
| gl030   | R        | R        | R     | R       | R     | -     |
| gl050   | -        | -        | -     | RU      | CRU   | W     |

*C=Create, R=Read, U=Update, D=Delete, W=Write-only*

## Transaction Boundaries

```mermaid
sequenceDiagram
    participant User
    participant Entry as Entry Program
    participant Valid as Validation
    participant Master as Master File
    participant Trans as Transaction File
    participant Audit as Audit Trail
    participant GL as GL Posting
    
    User->>Entry: Input Transaction
    Entry->>Valid: Validate Data
    Valid-->>Entry: Valid/Invalid
    
    alt Valid Transaction
        Entry->>Master: Read Master Record
        Master-->>Entry: Master Data
        Entry->>Trans: Write Transaction
        Entry->>Audit: Write Audit Record
        Entry->>GL: Queue for Posting
        Entry-->>User: Success Message
    else Invalid Transaction
        Entry-->>User: Error Message
    end
    
    Note over GL: Batch GL Posting
    GL->>Trans: Read Transactions
    GL->>Master: Update Balances
    GL->>Audit: Write Audit Trail
```

## Batch Processing Sequences

### 1. End-of-Day Processing

```mermaid
graph LR
    subgraph "Sales EOD"
        SalesExtract[Extract Sales<br/>sl800] --> SalesPost[Post to GL<br/>sl810]
        SalesPost --> SalesReport[Daily Report<br/>sl820]
    end
    
    subgraph "Purchase EOD"
        PurchExtract[Extract Purch<br/>pl800] --> PurchPost[Post to GL<br/>pl810]
        PurchPost --> PurchReport[Daily Report<br/>pl820]
    end
    
    subgraph "Stock EOD"
        StockVal[Valuation<br/>st800] --> StockPost[Post to GL<br/>st810]
        StockPost --> StockReport[Stock Report<br/>st820]
    end
    
    subgraph "GL EOD"
        GLCollect[Collect Posts<br/>gl800] --> GLBalance[Update Balances<br/>gl810]
        GLBalance --> TrialBalance[Trial Balance<br/>gl820]
    end
    
    SalesPost --> GLCollect
    PurchPost --> GLCollect
    StockPost --> GLCollect
```

### 2. Month-End Processing

```mermaid
graph TD
    Start[Month-End Start] --> Backup[Backup All Files]
    Backup --> SalesMonth[Sales Month-End<br/>sl900-sl950]
    Backup --> PurchMonth[Purchase Month-End<br/>pl900-pl950]
    Backup --> StockMonth[Stock Month-End<br/>st900-st950]
    
    SalesMonth --> GLMonth[GL Month-End<br/>gl900]
    PurchMonth --> GLMonth
    StockMonth --> GLMonth
    
    GLMonth --> FinancialReports[Financial Reports<br/>gl950-gl999]
    FinancialReports --> Archive[Archive Transactions]
    Archive --> NextPeriod[Open Next Period]
```

## Data Transformation Points

### Key Transformation Programs

1. **sl810** - Sales to GL Transformation
   - Sales transactions → GL journal entries
   - Customer balances → Debtors control account
   - Tax calculations → Tax liability accounts

2. **pl810** - Purchase to GL Transformation
   - Purchase transactions → GL journal entries
   - Supplier balances → Creditors control account
   - Input tax → Tax reclaim accounts

3. **st030** - Stock Valuation
   - Physical quantities → Financial values
   - FIFO/LIFO/Average cost calculations
   - Stock movements → GL stock accounts

4. **gl050** - GL Posting Engine
   - Individual journals → Account balances
   - Foreign currency → Base currency
   - Department/cost center allocation

## Critical Data Dependencies

### High-Risk Dependencies
1. **Customer/Supplier Codes** - Used across all modules
2. **GL Account Codes** - Central to all posting
3. **Stock Codes** - Links sales/purchase/inventory
4. **Period/Year Controls** - Affects all processing

### Data Integrity Controls
- Transaction numbering sequences
- Batch control totals
- Cross-module reconciliation
- Audit trail maintenance