# SL810 - Sales Invoice Processing Flow

## Program Overview
SL810 is a critical sales invoice processing program with complexity score of 308. It handles invoice generation, stock updates, and GL posting preparation.

## Detailed Program Flow

```mermaid
flowchart TD
    Start([START SL810]) --> Init[INITIALIZATION-ROUTINE]
    
    Init --> OpenFiles[OPEN-FILES]
    OpenFiles --> CheckFiles{FILES-OK?}
    CheckFiles -->|No| AbortRun[ABORT-RUN]
    CheckFiles -->|Yes| GetParams[GET-RUN-PARAMETERS]
    
    GetParams --> ValidateParams{PARAMS-VALID?}
    ValidateParams -->|No| AbortRun
    ValidateParams -->|Yes| MainProcess[MAIN-PROCESS]
    
    MainProcess --> SelectInvoices[SELECT-INVOICES]
    SelectInvoices --> ReadInvoice{READ-NEXT-INVOICE}
    
    ReadInvoice -->|EOF| EndOfInvoices[END-OF-INVOICES]
    ReadInvoice -->|Found| ProcessInvoice[PROCESS-INVOICE]
    
    ProcessInvoice --> ValidateInvoice{VALIDATE-INVOICE}
    ValidateInvoice -->|Invalid| LogError[LOG-ERROR]
    ValidateInvoice -->|Valid| CheckCustomer[CHECK-CUSTOMER]
    
    LogError --> ReadInvoice
    
    CheckCustomer --> ReadCustomer[READ-CUSTOMER-MASTER]
    ReadCustomer --> CustomerOK{CUSTOMER-OK?}
    CustomerOK -->|No| LogError
    CustomerOK -->|Yes| ProcessLines[PROCESS-INVOICE-LINES]
    
    ProcessLines --> ReadLine{READ-NEXT-LINE}
    ReadLine -->|EOF| CalcTotals[CALCULATE-TOTALS]
    ReadLine -->|Found| ProcessLine[PROCESS-LINE]
    
    ProcessLine --> CheckStock[CHECK-STOCK]
    CheckStock --> ReadStock[READ-STOCK-MASTER]
    ReadStock --> StockOK{STOCK-OK?}
    StockOK -->|No| LogLineError[LOG-LINE-ERROR]
    StockOK -->|Yes| UpdateStock[UPDATE-STOCK]
    
    LogLineError --> ReadLine
    
    UpdateStock --> CalcLineValue[CALCULATE-LINE-VALUE]
    CalcLineValue --> ApplyDiscount{DISCOUNT?}
    ApplyDiscount -->|Yes| CalcDiscount[CALCULATE-DISCOUNT]
    ApplyDiscount -->|No| AddToTotal[ADD-TO-TOTAL]
    CalcDiscount --> AddToTotal
    AddToTotal --> ReadLine
    
    CalcTotals --> CalcTax[CALCULATE-TAX]
    CalcTax --> UpdateCustomer[UPDATE-CUSTOMER-BALANCE]
    UpdateCustomer --> PrepareGL[PREPARE-GL-POSTING]
    
    PrepareGL --> WriteGLTrans[WRITE-GL-TRANSACTIONS]
    WriteGLTrans --> PrintInvoice{PRINT-REQUIRED?}
    PrintInvoice -->|Yes| FormatInvoice[FORMAT-INVOICE]
    PrintInvoice -->|No| UpdateAudit[UPDATE-AUDIT-TRAIL]
    
    FormatInvoice --> PrintRoutine[PRINT-INVOICE-ROUTINE]
    PrintRoutine --> UpdateAudit
    
    UpdateAudit --> CommitTrans[COMMIT-TRANSACTION]
    CommitTrans --> ReadInvoice
    
    EndOfInvoices --> PrintSummary[PRINT-SUMMARY-REPORT]
    PrintSummary --> CloseFiles[CLOSE-ALL-FILES]
    CloseFiles --> End([END])
    
    AbortRun --> ErrorReport[GENERATE-ERROR-REPORT]
    ErrorReport --> CloseFiles
```

## Data Access Sequence

```mermaid
sequenceDiagram
    participant P as SL810
    participant IF as Invoice File
    participant CF as Customer File
    participant SF as Stock File
    participant GL as GL Trans File
    participant AF as Audit File
    
    P->>IF: Open Invoice File
    P->>CF: Open Customer File
    P->>SF: Open Stock File
    P->>GL: Open GL Trans File
    P->>AF: Open Audit File
    
    loop For Each Invoice
        P->>IF: Read Invoice Header
        P->>CF: Read Customer Record
        
        loop For Each Line
            P->>SF: Read Stock Record
            P->>SF: Update Stock Qty
            P->>SF: Rewrite Stock Record
        end
        
        P->>CF: Update Customer Balance
        P->>CF: Rewrite Customer Record
        
        P->>GL: Write Debit Entry
        P->>GL: Write Credit Entries
        P->>GL: Write Tax Entries
        
        P->>AF: Write Audit Record
        P->>IF: Update Invoice Status
    end
    
    P->>IF: Close All Files
```

## Error Handling Paths

```mermaid
flowchart LR
    subgraph "File Errors"
        FileError[File Error] --> CheckSeverity{Severity?}
        CheckSeverity -->|Fatal| AbortProgram[Abort Program]
        CheckSeverity -->|Warning| LogWarning[Log Warning]
        CheckSeverity -->|Retry| RetryOperation[Retry Operation]
    end
    
    subgraph "Data Errors"
        DataError[Data Error] --> ErrorType{Error Type?}
        ErrorType -->|Missing Customer| SkipInvoice[Skip Invoice]
        ErrorType -->|Invalid Stock| SkipLine[Skip Line]
        ErrorType -->|Calc Error| UseDefault[Use Default]
    end
    
    subgraph "System Errors"
        SystemError[System Error] --> SaveState[Save State]
        SaveState --> NotifyOperator[Notify Operator]
        NotifyOperator --> WaitAction{Operator Action}
        WaitAction -->|Retry| RestoreState[Restore & Retry]
        WaitAction -->|Abort| CleanAbort[Clean Abort]
    end
```

## Business Logic Highlights

### Tax Calculation Logic
```
IF customer-tax-code = 'E' (Exempt)
    tax-amount = 0
ELSE IF customer-tax-code = 'Z' (Zero-rated)
    tax-amount = 0
    Generate tax-audit-record
ELSE
    tax-amount = goods-total * current-tax-rate
    IF customer-location = 'EXPORT'
        tax-amount = 0
        Generate export-documentation
    END-IF
END-IF
```

### Discount Application Rules
```
1. Customer discount rate (from master file)
2. Special promotion discount (if applicable)
3. Volume discount (based on quantity breaks)
4. Payment terms discount (for early payment)
5. Maximum combined discount = 25%
```

### GL Posting Logic
```
For each invoice:
- DR: Customer Control Account
- CR: Sales Account (by product category)
- CR: Tax Liability Account
- CR: Discount Account (if applicable)
- All entries include: Department code, Cost center, Transaction reference
```

## Performance Considerations

1. **File Access Optimization**
   - Customer records cached after first read
   - Stock records read with key access
   - GL transactions buffered before write

2. **Calculation Efficiency**
   - Running totals maintained
   - Tax rates cached at start
   - Discount calculations optimized

3. **Error Recovery**
   - Checkpoint after every 100 invoices
   - Restart capability from checkpoint
   - Partial run recovery supported