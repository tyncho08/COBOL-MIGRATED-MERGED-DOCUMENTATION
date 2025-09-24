# Procedure Flow Diagrams

## Overview
This document shows the internal procedure flows within key ACAS programs, illustrating PERFORM chains, conditional logic, and control structures.

## SL010 - Sales Customer Maintenance Flow

```mermaid
flowchart TD
    Start([SL010 START]) --> Init[INITIALIZATION]
    Init --> MainLoop{MAIN-LOOP}
    
    MainLoop --> ScreenDisplay[DISPLAY-SCREEN]
    ScreenDisplay --> AcceptInput[ACCEPT-INPUT]
    AcceptInput --> ValidateInput{VALIDATE-INPUT}
    
    ValidateInput -->|Valid| ProcessAction{ACTION?}
    ValidateInput -->|Invalid| ErrorMsg[DISPLAY-ERROR]
    ErrorMsg --> ScreenDisplay
    
    ProcessAction -->|ADD| AddCustomer[ADD-CUSTOMER]
    ProcessAction -->|CHANGE| ChangeCustomer[CHANGE-CUSTOMER]
    ProcessAction -->|DELETE| DeleteCustomer[DELETE-CUSTOMER]
    ProcessAction -->|ENQUIRY| EnquiryCustomer[ENQUIRY-CUSTOMER]
    ProcessAction -->|EXIT| ExitProgram[EXIT-PROGRAM]
    
    AddCustomer --> ValidateNew{VALIDATE-NEW}
    ValidateNew -->|Valid| WriteRecord[WRITE-RECORD]
    ValidateNew -->|Invalid| ErrorMsg
    WriteRecord --> UpdateAudit[UPDATE-AUDIT]
    UpdateAudit --> ScreenDisplay
    
    ChangeCustomer --> ReadRecord[READ-RECORD]
    ReadRecord --> ValidateChange{VALIDATE-CHANGE}
    ValidateChange -->|Valid| RewriteRecord[REWRITE-RECORD]
    ValidateChange -->|Invalid| ErrorMsg
    RewriteRecord --> UpdateAudit
    
    DeleteCustomer --> ReadRecord
    ReadRecord --> ConfirmDelete{CONFIRM-DELETE?}
    ConfirmDelete -->|Yes| DeleteRecord[DELETE-RECORD]
    ConfirmDelete -->|No| ScreenDisplay
    DeleteRecord --> UpdateAudit
    
    EnquiryCustomer --> ReadRecord
    ReadRecord --> DisplayDetails[DISPLAY-DETAILS]
    DisplayDetails --> ScreenDisplay
    
    ExitProgram --> CloseFiles[CLOSE-FILES]
    CloseFiles --> End([END])
```

## SL910 - Sales Reports Complex Flow

```mermaid
flowchart TD
    Start([SL910 START]) --> Init[INITIALIZATION]
    Init --> OpenFiles[OPEN-FILES]
    OpenFiles --> CheckParams{CHECK-PARAMETERS}
    
    CheckParams -->|Valid| SelectReport{REPORT-TYPE?}
    CheckParams -->|Invalid| Abort[ABORT-RUN]
    
    SelectReport -->|DETAILED| DetailedReport[DETAILED-REPORT]
    SelectReport -->|SUMMARY| SummaryReport[SUMMARY-REPORT]
    SelectReport -->|AGED| AgedReport[AGED-REPORT]
    SelectReport -->|CUSTOM| CustomReport[CUSTOM-REPORT]
    
    DetailedReport --> ReadStart[START-READ]
    ReadStart --> ReadLoop{READ-NEXT}
    ReadLoop -->|EOF| PrintTotals[PRINT-TOTALS]
    ReadLoop -->|Record| ProcessDetail[PROCESS-DETAIL]
    
    ProcessDetail --> CheckBreak{CONTROL-BREAK?}
    CheckBreak -->|Yes| PrintSubtotal[PRINT-SUBTOTAL]
    CheckBreak -->|No| AccumulateTotals[ACCUMULATE-TOTALS]
    PrintSubtotal --> AccumulateTotals
    AccumulateTotals --> PrintLine[PRINT-LINE]
    PrintLine --> ReadLoop
    
    SummaryReport --> BuildSummary[BUILD-SUMMARY-TABLE]
    BuildSummary --> SortTable[SORT-TABLE]
    SortTable --> PrintSummary[PRINT-SUMMARY]
    
    AgedReport --> AgeAnalysis[AGE-ANALYSIS]
    AgeAnalysis --> Bucket1[0-30 DAYS]
    AgeAnalysis --> Bucket2[31-60 DAYS]
    AgeAnalysis --> Bucket3[61-90 DAYS]
    AgeAnalysis --> Bucket4[90+ DAYS]
    Bucket1 --> PrintAged[PRINT-AGED-REPORT]
    Bucket2 --> PrintAged
    Bucket3 --> PrintAged
    Bucket4 --> PrintAged
    
    CustomReport --> CustomLogic[CUSTOM-LOGIC]
    CustomLogic --> PrintCustom[PRINT-CUSTOM]
    
    PrintTotals --> CloseFiles[CLOSE-FILES]
    PrintSummary --> CloseFiles
    PrintAged --> CloseFiles
    PrintCustom --> CloseFiles
    CloseFiles --> End([END])
    Abort --> End
```

## Common Control Flow Patterns

### 1. Main Processing Loop Pattern

```mermaid
flowchart LR
    Init[INITIALIZATION] --> MainLoop{MAIN-LOOP}
    MainLoop --> Process[PROCESS-TRANSACTION]
    Process --> CheckEnd{END-CONDITION?}
    CheckEnd -->|No| MainLoop
    CheckEnd -->|Yes| Termination[TERMINATION]
```

### 2. File Processing Pattern

```mermaid
flowchart TD
    OpenFile[OPEN-FILE] --> ReadFirst[READ-FIRST]
    ReadFirst --> ProcessLoop{PROCESS-LOOP}
    ProcessLoop --> ReadNext[READ-NEXT]
    ReadNext --> CheckEOF{AT-END?}
    CheckEOF -->|No| ProcessRecord[PROCESS-RECORD]
    ProcessRecord --> ProcessLoop
    CheckEOF -->|Yes| CloseFile[CLOSE-FILE]
```

### 3. Error Handling Pattern

```mermaid
flowchart TD
    Operation[PERFORM-OPERATION] --> CheckStatus{CHECK-STATUS}
    CheckStatus -->|OK| Continue[CONTINUE]
    CheckStatus -->|ERROR| ErrorType{ERROR-TYPE?}
    ErrorType -->|FATAL| AbortRun[ABORT-RUN]
    ErrorType -->|WARNING| LogWarning[LOG-WARNING]
    ErrorType -->|RETRY| RetryOp[RETRY-OPERATION]
    LogWarning --> Continue
    RetryOp --> Operation
```

## Dead Code Detection

Programs with potentially unreachable code sections:
1. **sl080** - Paragraph UNUSED-ROUTINE never called
2. **pl020** - Section OLD-VALIDATION unreachable
3. **st010** - Paragraph LEGACY-CALC orphaned
4. **gl030** - Multiple GO TO statements create unreachable paths

## Complex Conditional Logic

### High Complexity Decision Trees

Programs with deeply nested conditions (>4 levels):
- **sl910**: Report selection logic with 7 nested levels
- **st030**: Inventory valuation with 6 nested conditions
- **gl030**: Account categorization with 5 nested evaluates

### GO TO Usage Patterns

Programs still using GO TO statements:
- 267 programs use GO TO (59% of codebase)
- Most common: GO TO EXIT-PARAGRAPH pattern
- Problematic: GO TO DEPENDING ON in 12 programs
- Spaghetti code risk: 8 programs with >20 GO TOs

## Performance Bottlenecks

### Inefficient PERFORM Chains
1. **sl920** - Recursive PERFORM detected in CALC-TOTALS
2. **pl810** - PERFORM VARYING with unnecessary iterations
3. **st020** - Nested PERFORM THRU spanning >500 lines

### Optimization Opportunities
- Convert sequential searches to indexed lookups
- Eliminate redundant file reads
- Consolidate multiple passes through same data