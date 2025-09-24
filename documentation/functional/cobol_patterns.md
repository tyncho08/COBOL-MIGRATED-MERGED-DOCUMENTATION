# ACAS COBOL Coding Patterns

## Overview

This document identifies and documents the common COBOL coding patterns used throughout the ACAS system, including naming conventions, program structure standards, and best practices followed or violated.

## Naming Conventions

### Program Naming

| Pattern | Description | Example |
|---------|-------------|---------|
| `xxNNN` | Module prefix + 3-digit number | sl010, pl020, st030 |
| `xx9NN` | Report programs (900 series) | sl910, pl920, gl950 |
| `xx8NN` | Batch processing (800 series) | sl810, st800 |
| `xx0NN` | Maintenance programs (000-099) | sl010, pl010 |
| `xxMT` | Data access layer (DAL) programs | salesMT, stockMT |
| `maps##` | Utility programs | maps04, maps09 |
| `sys###` | System utilities | sys002, sys003 |
| `acas###` | System initialization/control | acas000, acas010 |

### Data Naming

| Element | Pattern | Example |
|---------|---------|---------|
| File names | Module prefix + function | SLMASTER, SLTRANS |
| Record names | File name + `-RECORD` | SALES-RECORD, STOCK-RECORD |
| Field names | Hierarchical with hyphens | CUSTOMER-NAME, STOCK-QTY-ON-HAND |
| Working storage | `WS-` prefix | WS-CUSTOMER-NO, WS-TOTAL-AMOUNT |
| Linkage items | `LK-` prefix | LK-RETURN-CODE, LK-PARAMETERS |
| Constants | `C-` prefix or descriptive | C-MAX-CUSTOMERS, TAX-RATE |
| Flags/switches | `-FLAG` or `-SW` suffix | PRINT-FLAG, END-OF-FILE-SW |
| Counters | `-CTR` or `-COUNT` suffix | LINE-CTR, RECORD-COUNT |
| Totals | `-TOTAL` or `-AMT` suffix | INVOICE-TOTAL, YTD-AMT |

### Copybook Naming

| Type | Pattern | Description |
|------|---------|-------------|
| File descriptions | `FDxxxx` | FDSL (Sales file description) |
| Working storage | `WSxxxx` | WSSTOCK (Stock working storage) |
| Screen definitions | `SCREENxx` | SCREENIO, SCREENDEFS |
| Linkage sections | `LINKxxxx` | LINKDATA, LINKPASS |
| Constants | `CONSTANTS` | System-wide constants |
| Messages | `MESSAGES` | Error/info messages |

## Program Structure Standards

### Standard Program Layout

```cobol
      *> Header comments with program description
       >>source free
      *>************************************************
      *>                                               *
      *>            Program Title Here                 *
      *>                                               *
      *>************************************************
      *>
       IDENTIFICATION DIVISION.
      *>=======================
       PROGRAM-ID. xx###.
       AUTHOR. Developer Name.
       DATE-WRITTEN. DD/MM/YY.
       SECURITY. Copyright notice.
       
       ENVIRONMENT DIVISION.
      *>====================
       COPY "envdiv.cob".  *> Standard environment
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "selxxxx.cob". *> File selects
       
       DATA DIVISION.
      *>=============
       FILE SECTION.
       COPY "fdxxxx.cob".  *> File descriptions
       
       WORKING-STORAGE SECTION.
       77  prog-name    PIC X(15) VALUE "XX### (v.vv.rr)".
       COPY "wsxxxx.cob".  *> Working storage
       
       LINKAGE SECTION.
       COPY "linkxxxx.cob". *> Parameters
       
       PROCEDURE DIVISION USING parameters.
      *>===================================
       MAIN-CONTROL SECTION.
           PERFORM INITIALIZATION
           PERFORM MAIN-PROCESS
           PERFORM TERMINATION
           GOBACK.
```

### Section Organization Pattern

Programs typically organized into functional sections:

```cobol
      *> Control sections
       MAIN-CONTROL SECTION.
       INITIALIZATION SECTION.
       TERMINATION SECTION.
       
      *> Processing sections  
       MAIN-PROCESS SECTION.
       INPUT-PROCESS SECTION.
       OUTPUT-PROCESS SECTION.
       
      *> Utility sections
       ERROR-HANDLING SECTION.
       DISPLAY-ROUTINES SECTION.
       CALCULATION-ROUTINES SECTION.
```

## Common Coding Patterns

### 1. Menu Processing Pattern

```cobol
       MAIN-MENU.
           PERFORM DISPLAY-MENU
           PERFORM ACCEPT-CHOICE
           PERFORM PROCESS-CHOICE
               UNTIL menu-choice = "X" OR "x"
           GOBACK.
           
       PROCESS-CHOICE.
           EVALUATE menu-choice
               WHEN "1" PERFORM CUSTOMER-MAINTENANCE
               WHEN "2" PERFORM ORDER-ENTRY
               WHEN "3" PERFORM REPORTS-MENU
               WHEN "Z" PERFORM SYSTEM-SETUP
               WHEN OTHER PERFORM INVALID-CHOICE
           END-EVALUATE.
```

### 2. File Access Pattern (via DAL)

```cobol
      *> Read with lock
       MOVE "READ-LOCK" TO operation-code
       CALL 'salesMT' USING operation-code
                            sales-record
                            return-code
       IF return-code NOT = 0
           PERFORM HANDLE-FILE-ERROR
       END-IF
       
      *> Update
       MOVE "UPDATE" TO operation-code  
       CALL 'salesMT' USING operation-code
                            sales-record
                            return-code
```

### 3. Screen Handling Pattern

```cobol
      *> Display and accept pattern
       DISPLAY-CUSTOMER-SCREEN.
           DISPLAY screen-header
           DISPLAY customer-details
           ACCEPT customer-input
           PERFORM VALIDATE-INPUT
           IF input-valid
               PERFORM PROCESS-INPUT
           ELSE
               PERFORM DISPLAY-ERROR
               GO TO DISPLAY-CUSTOMER-SCREEN
           END-IF.
```

### 4. Validation Pattern

```cobol
       VALIDATE-CUSTOMER-INPUT.
           MOVE "Y" TO input-valid-flag
           
           IF customer-name = SPACES
               MOVE "N" TO input-valid-flag
               MOVE "Customer name required" TO error-message
               EXIT PARAGRAPH
           END-IF
           
           IF credit-limit NOT NUMERIC
               MOVE "N" TO input-valid-flag  
               MOVE "Credit limit must be numeric" TO error-message
               EXIT PARAGRAPH
           END-IF
           
           PERFORM CHECK-DUPLICATE-CUSTOMER
           IF duplicate-found
               MOVE "N" TO input-valid-flag
               MOVE "Customer already exists" TO error-message
           END-IF.
```

### 5. Batch Processing Pattern

```cobol
       BATCH-PROCESS.
           PERFORM OPEN-FILES
           PERFORM INITIALIZE-TOTALS
           
           PERFORM PROCESS-RECORD
               UNTIL end-of-file
               
           PERFORM PRINT-TOTALS
           PERFORM CLOSE-FILES.
           
       PROCESS-RECORD.
           READ input-file
               AT END 
                   MOVE "Y" TO end-of-file-flag
               NOT AT END
                   PERFORM VALIDATE-RECORD
                   IF record-valid
                       PERFORM UPDATE-FILES
                       PERFORM ACCUMULATE-TOTALS
                   ELSE
                       PERFORM WRITE-ERROR-RECORD
                   END-IF
           END-READ.
```

### 6. Report Generation Pattern

```cobol
       PRINT-REPORT.
           PERFORM PRINT-HEADER
           PERFORM INITIALIZE-CONTROL-BREAKS
           
           PERFORM READ-AND-PRINT
               UNTIL end-of-file
               
           PERFORM PRINT-FINAL-TOTALS
           
       READ-AND-PRINT.
           READ report-file
               AT END MOVE "Y" TO end-of-file-flag
               NOT AT END
                   PERFORM CHECK-CONTROL-BREAK
                   PERFORM PRINT-DETAIL-LINE
                   PERFORM ACCUMULATE-TOTALS
           END-READ.
           
       CHECK-CONTROL-BREAK.
           IF customer-no NOT = previous-customer-no
               PERFORM PRINT-CUSTOMER-TOTAL
               MOVE ZEROS TO customer-total
               MOVE customer-no TO previous-customer-no
           END-IF.
```

### 7. Error Handling Pattern

```cobol
       FILE-ERROR-HANDLER.
           EVALUATE file-status
               WHEN "00" CONTINUE
               WHEN "23" PERFORM RECORD-NOT-FOUND
               WHEN "35" PERFORM FILE-NOT-FOUND
               WHEN "39" PERFORM FILE-ATTRIBUTE-ERROR
               WHEN OTHER PERFORM GENERAL-FILE-ERROR
           END-EVALUATE.
           
       GENERAL-FILE-ERROR.
           DISPLAY "File error: " file-status
           DISPLAY "Program: " prog-name
           DISPLAY "File: " file-name
           PERFORM CLOSE-ALL-FILES
           MOVE 99 TO return-code
           GOBACK.
```

### 8. Date Handling Pattern

```cobol
       VALIDATE-DATE.
           CALL 'maps04' USING date-function
                               input-date
                               output-date
                               return-code
           IF return-code NOT = 0
               MOVE "Invalid date" TO error-message
               PERFORM ERROR-ROUTINE
           END-IF.
           
       FORMAT-DATE-FOR-DISPLAY.
           EVALUATE date-format
               WHEN "D" *> DD/MM/YY
                   STRING date-dd "/" date-mm "/" date-yy
                       INTO display-date
               WHEN "M" *> MM/DD/YY  
                   STRING date-mm "/" date-dd "/" date-yy
                       INTO display-date
               WHEN "Y" *> YY/MM/DD
                   STRING date-yy "/" date-mm "/" date-dd
                       INTO display-date
           END-EVALUATE.
```

### 9. Check Digit Pattern

```cobol
       VALIDATE-CHECK-DIGIT.
           CALL 'maps09' USING check-digit-function
                               customer-number
                               return-code
           IF return-code NOT = 0
               MOVE "Invalid customer number" TO error-message
               PERFORM ERROR-ROUTINE
           END-IF.
```

### 10. Transaction Control Pattern

```cobol
       PROCESS-TRANSACTION.
           PERFORM BEGIN-TRANSACTION
           
           PERFORM UPDATE-MASTER-FILE
           IF update-successful
               PERFORM CREATE-AUDIT-RECORD
               IF audit-successful
                   PERFORM UPDATE-BALANCES
                   IF balance-successful
                       PERFORM COMMIT-TRANSACTION
                   ELSE
                       PERFORM ROLLBACK-TRANSACTION
                   END-IF
               ELSE
                   PERFORM ROLLBACK-TRANSACTION
               END-IF
           ELSE
               PERFORM ROLLBACK-TRANSACTION
           END-IF.
```

## Best Practices Followed

### 1. Modular Design
- Separation of concerns (UI, business logic, data access)
- Reusable copybooks for common structures
- Standardized error handling routines
- Consistent parameter passing

### 2. Data Integrity
- Check digit validation on key fields
- Date validation on all date inputs
- Numeric validation before calculations
- Balance controls in financial processing

### 3. Audit Trail
- All transactions logged
- Before/after images captured
- User and timestamp recorded
- No deletion, only logical flags

### 4. Error Recovery
- Graceful error handling
- Transaction rollback capability
- Restart/recovery procedures
- Comprehensive error messages

## Best Practices Violated

### 1. Excessive Use of GO TO

**Current Practice**:
```cobol
      *> Problem: Spaghetti code
       IF condition-1
           GO TO PROCESS-A
       ELSE
           GO TO PROCESS-B
       END-IF
       
   PROCESS-A.
       ...
       GO TO EXIT-POINT.
       
   PROCESS-B.
       ...
       GO TO EXIT-POINT.
```

**Better Practice**:
```cobol
       IF condition-1
           PERFORM PROCESS-A
       ELSE
           PERFORM PROCESS-B
       END-IF.
```

### 2. Long Monolithic Paragraphs

**Current Practice**:
- Single paragraphs with 200+ lines
- Multiple responsibilities in one paragraph
- Difficult to test and maintain

**Better Practice**:
- Break into smaller, focused paragraphs
- Single responsibility principle
- Maximum 50 lines per paragraph

### 3. Global Data Dependencies

**Current Practice**:
```cobol
      *> Working storage visible to entire program
       01  WS-GLOBAL-CUSTOMER-TOTAL    PIC 9(9)V99.
       01  WS-GLOBAL-LINE-COUNT        PIC 999.
       01  WS-GLOBAL-ERROR-FLAG        PIC X.
```

**Better Practice**:
- Pass data as parameters
- Limit scope of variables
- Use local-storage where appropriate

### 4. Hardcoded Values

**Current Practice**:
```cobol
       IF tax-code = "S"
           COMPUTE tax-amount = gross-amount * 0.20
       END-IF
       
       IF customer-balance > 10000
           MOVE "GOLD" TO customer-category
       END-IF
```

**Better Practice**:
```cobol
       COPY "constants.cob".
       
       IF tax-code = C-STANDARD-TAX-CODE
           COMPUTE tax-amount = gross-amount * C-STANDARD-TAX-RATE
       END-IF
       
       IF customer-balance > C-GOLD-THRESHOLD
           MOVE C-GOLD-CATEGORY TO customer-category
       END-IF
```

### 5. Missing Error Handling

**Current Practice**:
```cobol
       READ customer-file
       MOVE customer-name TO output-name
       *> No check for AT END or file errors
```

**Better Practice**:
```cobol
       READ customer-file
           AT END
               PERFORM END-OF-FILE-ROUTINE
           NOT AT END
               MOVE customer-name TO output-name
       END-READ
       
       IF file-status NOT = "00"
           PERFORM FILE-ERROR-ROUTINE
       END-IF
```

### 6. Duplicate Code

**Current Practice**:
- Same validation logic repeated in multiple programs
- Identical calculations in different modules
- Copy-paste programming

**Better Practice**:
- Extract common routines to copybooks
- Create utility programs for shared logic
- Use CALL statements for reusability

## Code Complexity Patterns

### High Complexity Indicators

1. **Deeply Nested Conditions**
   - Found in: sl910, st030, gl030
   - Nesting levels up to 7 deep
   - Makes logic hard to follow

2. **Long EVALUATE Statements**
   - 20+ WHEN clauses
   - Complex compound conditions
   - Difficult to test all paths

3. **Excessive PERFORM Chains**
   - PERFORM A which PERFORMs B which PERFORMs C
   - Circular PERFORM references
   - Hard to trace execution flow

4. **Mixed Processing Levels**
   - Detail and summary logic intertwined
   - UI and business logic combined
   - Data access scattered throughout

## Recommended Refactoring Patterns

### 1. Extract Method Pattern
```cobol
      *> Before: Everything in one paragraph
      *> After: Broken into logical units
       PROCESS-ORDER.
           PERFORM VALIDATE-ORDER-HEADER
           PERFORM VALIDATE-ORDER-LINES
           PERFORM CHECK-CREDIT
           PERFORM ALLOCATE-STOCK
           PERFORM GENERATE-INVOICE
           PERFORM UPDATE-STATISTICS.
```

### 2. Guard Clause Pattern
```cobol
      *> Instead of deep nesting, exit early
       VALIDATE-INPUT.
           IF customer-no = SPACES
               MOVE "Customer required" TO error-msg
               EXIT PARAGRAPH
           END-IF
           
           IF amount NOT NUMERIC
               MOVE "Invalid amount" TO error-msg
               EXIT PARAGRAPH
           END-IF
           
           *> Continue with valid data...
```

### 3. Table-Driven Pattern
```cobol
      *> Replace long EVALUATE with table lookup
       01  TAX-TABLE.
           05  TAX-ENTRY OCCURS 10 TIMES.
               10  TAX-CODE    PIC XX.
               10  TAX-RATE    PIC V999.
               10  TAX-DESC    PIC X(20).
               
       FIND-TAX-RATE.
           PERFORM VARYING idx FROM 1 BY 1
                   UNTIL idx > 10
                   OR tax-found
               IF TAX-CODE(idx) = input-tax-code
                   MOVE TAX-RATE(idx) TO applicable-rate
                   MOVE "Y" TO tax-found
               END-IF
           END-PERFORM.
```

### 4. Strategy Pattern (COBOL style)
```cobol
      *> Different calculation methods
       EVALUATE calculation-type
           WHEN "FIFO"  PERFORM CALC-FIFO-COST
           WHEN "LIFO"  PERFORM CALC-LIFO-COST  
           WHEN "AVG"   PERFORM CALC-AVERAGE-COST
       END-EVALUATE.
```

## Migration Considerations

### Patterns to Preserve
1. Modular DAL architecture
2. Comprehensive audit trailing
3. Check digit validation
4. Batch control totals
5. Transaction integrity

### Patterns to Eliminate
1. GO TO statements
2. Global variables
3. Hardcoded values
4. Monolithic paragraphs
5. Duplicate code

### Patterns to Modernize
1. Error handling → Exception handling
2. File I/O → Database/API
3. Batch processing → Event-driven
4. Character UI → Web interface
5. Procedural → Object-oriented