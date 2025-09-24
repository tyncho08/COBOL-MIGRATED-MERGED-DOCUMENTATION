# AR_MGMT Process Flows

## Overview

This document details the key business process flows within the AR_MGMT subsystem. These flows represent the complete order-to-cash cycle and all supporting processes for managing customer relationships and revenue collection.

## Major Process Flows

### 1. Order-to-Cash Complete Flow

**Purpose**: End-to-end process from customer order through cash collection

```mermaid
flowchart TD
    START[Customer Order] --> VALIDATE{Validate Order}
    
    VALIDATE -->|Invalid| REJECT[Reject Order]
    VALIDATE -->|Valid| CREDIT{Credit Check}
    
    CREDIT -->|Failed| HOLD[Credit Hold]
    CREDIT -->|Passed| INVENTORY{Check Stock}
    
    HOLD --> APPROVE{Credit Override?}
    APPROVE -->|No| REJECT
    APPROVE -->|Yes| INVENTORY
    
    INVENTORY -->|Not Available| BACKORDER[Create Backorder]
    INVENTORY -->|Partial| PARTIAL[Partial Shipment]
    INVENTORY -->|Available| ALLOCATE[Allocate Stock]
    
    ALLOCATE --> PICK[Generate Pick List]
    PICK --> SHIP[Ship Goods]
    SHIP --> INVOICE[Generate Invoice]
    
    PARTIAL --> SHIP
    BACKORDER --> WAIT[Wait for Stock]
    WAIT --> ALLOCATE
    
    INVOICE --> POST[Post to GL]
    POST --> SEND[Send to Customer]
    
    SEND --> COLLECT[Collections Process]
    COLLECT --> PAYMENT{Payment Received?}
    
    PAYMENT -->|No| FOLLOWUP[Follow Up]
    PAYMENT -->|Yes| APPLY[Apply Payment]
    
    FOLLOWUP --> COLLECT
    APPLY --> CLOSE[Close Transaction]
    
    REJECT --> END[End]
    CLOSE --> END
```

---

### 2. Credit Management Flow

**Purpose**: Manage customer credit limits and authorization

```mermaid
flowchart LR
    subgraph "Credit Decision Process"
        REQUEST[Credit Request] --> HISTORY{Check History}
        
        HISTORY -->|Good| AUTO[Auto Approve]
        HISTORY -->|Poor| REVIEW[Manual Review]
        HISTORY -->|New| BUREAU[Credit Bureau]
        
        BUREAU --> SCORE{Score Check}
        SCORE -->|High| AUTO
        SCORE -->|Low| DECLINE[Decline]
        SCORE -->|Medium| REVIEW
        
        REVIEW --> DOCS[Request Financials]
        DOCS --> ANALYZE[Credit Analysis]
        ANALYZE --> DECISION{Decision}
        
        DECISION -->|Approve| SETLIMIT[Set Limit]
        DECISION -->|Decline| DECLINE
        
        AUTO --> SETLIMIT
        SETLIMIT --> MONITOR[Monitor Usage]
    end
```

**Credit Monitoring Process**:

```mermaid
stateDiagram-v2
    [*] --> Active: Credit Approved
    
    Active --> Warning: 80% Utilized
    Active --> AtLimit: 100% Utilized
    
    Warning --> Active: Payment Reduces
    Warning --> AtLimit: Further Orders
    
    AtLimit --> Hold: New Order Attempt
    AtLimit --> Warning: Payment Received
    
    Hold --> Active: Full Payment
    Hold --> Review: Increase Request
    
    Review --> Active: Limit Increased
    Review --> Suspended: Risk Identified
    
    Suspended --> Review: Resolution
    Suspended --> Closed: Write Off
    
    Closed --> [*]
```

---

### 3. Invoice Generation Flow

**Purpose**: Create and distribute customer invoices

```mermaid
sequenceDiagram
    participant SHIP as Shipping
    participant INV as Invoice Engine
    participant PRICE as Pricing
    participant TAX as Tax Engine
    participant GL as GL Posting
    participant CUST as Customer
    
    SHIP->>INV: Shipment Confirmed
    
    INV->>INV: Gather Shipment Details
    INV->>PRICE: Get Customer Pricing
    PRICE-->>INV: Prices & Discounts
    
    INV->>TAX: Calculate Tax
    TAX->>TAX: Determine Nexus
    TAX->>TAX: Apply Rates
    TAX-->>INV: Tax Amount
    
    INV->>INV: Generate Invoice
    INV->>GL: Post Journal Entry
    GL-->>INV: Posting Confirmation
    
    INV->>CUST: Send Invoice
    alt Electronic
        INV->>CUST: Email PDF
        INV->>CUST: EDI 810
    else Paper
        INV->>PRINT: Queue for Printing
        PRINT->>CUST: Mail Invoice
    end
    
    INV->>INV: Update AR Balance
```

---

### 4. Cash Application Flow

**Purpose**: Apply customer payments to open invoices

```mermaid
flowchart TD
    subgraph "Payment Sources"
        CHECK[Check Payment]
        WIRE[Wire Transfer]
        ACH[ACH Payment]
        CC[Credit Card]
        LOCKBOX[Lockbox]
    end
    
    subgraph "Processing"
        CHECK --> DEPOSIT[Bank Deposit]
        WIRE --> DEPOSIT
        ACH --> DEPOSIT
        CC --> DEPOSIT
        LOCKBOX --> IMPORT[Import File]
        
        DEPOSIT --> IDENTIFY{Identify Customer}
        IMPORT --> IDENTIFY
        
        IDENTIFY -->|Found| MATCH{Match Invoices}
        IDENTIFY -->|Not Found| SUSPEND[Suspend]
        
        MATCH -->|Exact| AUTO[Auto Apply]
        MATCH -->|Multiple| LOGIC[Apply Rules]
        MATCH -->|None| MANUAL[Manual Queue]
        
        LOGIC --> APPLY[Apply Payment]
        AUTO --> APPLY
        MANUAL --> REVIEW[Manual Review]
        REVIEW --> APPLY
        
        APPLY --> DISCOUNT{Discount?}
        DISCOUNT -->|Yes| CALC[Calculate Discount]
        DISCOUNT -->|No| POST
        
        CALC --> POST[Post to GL]
        POST --> UPDATE[Update Balance]
        
        SUSPEND --> RESEARCH[Research Customer]
        RESEARCH --> APPLY
    end
```

**Payment Application Rules**:

```mermaid
graph TD
    PAYMENT[Payment Received] --> CHECK1{Remittance Provided?}
    
    CHECK1 -->|Yes| SPECIFIC[Apply to Specified Invoices]
    CHECK1 -->|No| CHECK2{Single Open Invoice?}
    
    CHECK2 -->|Yes| SINGLE[Apply to Invoice]
    CHECK2 -->|No| RULES[Apply Rules]
    
    RULES --> OLDEST[Oldest First]
    RULES --> LARGEST[Largest First]
    RULES --> DUENOW[Due Now First]
    
    OLDEST --> PARTIAL{Full Payment?}
    LARGEST --> PARTIAL
    DUENOW --> PARTIAL
    
    PARTIAL -->|Yes| CLEAR[Clear Invoice]
    PARTIAL -->|No| PARTPAY[Partial Payment]
    
    CLEAR --> NEXT{More Payment?}
    PARTPAY --> BALANCE[Leave Balance]
    
    NEXT -->|Yes| RULES
    NEXT -->|No| COMPLETE[Complete]
```

---

### 5. Collections Process Flow

**Purpose**: Systematic follow-up on overdue accounts

```mermaid
flowchart TD
    START[Daily Aging Run] --> AGE{Check Age}
    
    AGE -->|Current| NOACTION[No Action]
    AGE -->|1-30 Days| REMINDER[Friendly Reminder]
    AGE -->|31-60 Days| LETTER1[First Letter]
    AGE -->|61-90 Days| LETTER2[Second Letter]
    AGE -->|Over 90| LETTER3[Final Notice]
    
    REMINDER --> LOG[Log Contact]
    LETTER1 --> CALL1[Phone Call]
    LETTER2 --> CALL2[Escalated Call]
    LETTER3 --> LEGAL{Legal Action?}
    
    CALL1 --> PROMISE{Payment Promise?}
    CALL2 --> PROMISE
    
    PROMISE -->|Yes| TRACK[Track Promise]
    PROMISE -->|No| ESCALATE[Escalate]
    
    TRACK --> CHECK{Promise Kept?}
    CHECK -->|Yes| PAYMENT[Process Payment]
    CHECK -->|No| ESCALATE
    
    LEGAL -->|Yes| ATTORNEY[Send to Legal]
    LEGAL -->|No| WRITEOFF[Write Off]
    
    ESCALATE --> HOLD[Credit Hold]
    HOLD --> LEGAL
    
    LOG --> END[End]
    PAYMENT --> END
    ATTORNEY --> END
    WRITEOFF --> END
```

---

### 6. Customer Statement Generation

**Purpose**: Produce monthly customer account statements

```mermaid
sequenceDiagram
    participant SCHED as Scheduler
    participant STMT as Statement Engine
    participant AR as AR Database
    participant FORMAT as Formatter
    participant DELIVER as Delivery
    
    SCHED->>STMT: Trigger Monthly Run
    
    loop For Each Customer
        STMT->>AR: Get Open Items
        AR-->>STMT: Invoice List
        
        STMT->>AR: Get Payments
        AR-->>STMT: Payment List
        
        STMT->>AR: Get Credits
        AR-->>STMT: Credit List
        
        STMT->>STMT: Calculate Aging
        STMT->>STMT: Calculate Balance
        
        alt Balance > Minimum
            STMT->>FORMAT: Format Statement
            
            alt Email Preference
                FORMAT->>DELIVER: Email PDF
            else Print Preference
                FORMAT->>DELIVER: Print Queue
            else No Statement
                FORMAT->>STMT: Skip
            end
        end
    end
    
    DELIVER->>DELIVER: Process Queue
    DELIVER->>STMT: Delivery Confirmation
```

---

### 7. Order Entry Process Flow

**Purpose**: Capture and validate customer orders

```mermaid
flowchart TD
    subgraph "Order Entry"
        START[New Order] --> SOURCE{Order Source}
        
        SOURCE -->|Phone| MANUAL[Manual Entry]
        SOURCE -->|EDI| IMPORT[EDI Import]
        SOURCE -->|Web| API[API Process]
        
        MANUAL --> VALIDATE
        IMPORT --> VALIDATE
        API --> VALIDATE
    end
    
    subgraph "Validation"
        VALIDATE[Validate Order] --> CUST{Valid Customer?}
        
        CUST -->|No| ERROR1[Customer Error]
        CUST -->|Yes| ITEMS{Valid Items?}
        
        ITEMS -->|No| ERROR2[Item Error]
        ITEMS -->|Yes| PRICE[Calculate Price]
        
        PRICE --> TAX[Calculate Tax]
        TAX --> TOTAL[Calculate Total]
    end
    
    subgraph "Fulfillment Check"
        TOTAL --> CREDIT{Credit OK?}
        
        CREDIT -->|No| HOLDORDER[Hold Order]
        CREDIT -->|Yes| STOCK{Stock Available?}
        
        STOCK -->|No| BACKORDER[Backorder]
        STOCK -->|Partial| SPLIT[Split Order]
        STOCK -->|Yes| CONFIRM[Confirm Order]
    end
    
    ERROR1 --> REJECT[Reject Order]
    ERROR2 --> REJECT
    HOLDORDER --> REVIEW[Credit Review]
    BACKORDER --> NOTIFY[Notify Customer]
    SPLIT --> CONFIRM
    CONFIRM --> PROCESS[To Fulfillment]
```

---

### 8. Month-End AR Close Process

**Purpose**: Close AR period and reconcile to GL

```mermaid
flowchart LR
    START[Month End] --> CUTOFF[Transaction Cutoff]
    
    CUTOFF --> INVOICE[Final Invoicing]
    INVOICE --> PAYMENT[Final Payments]
    
    PAYMENT --> AGING[Run Aging]
    AGING --> ACCRUE[Accrue Interest]
    
    ACCRUE --> RECON{Reconcile to GL}
    
    RECON -->|Matched| POST[Post Accruals]
    RECON -->|Difference| INVESTIGATE[Investigate]
    
    INVESTIGATE --> FIX[Corrections]
    FIX --> RECON
    
    POST --> REPORTS[Generate Reports]
    REPORTS --> CLOSE[Close Period]
    
    CLOSE --> OPEN[Open New Period]
```

---

## Integration Flow Points

### With Inventory (INV_CTRL)

```mermaid
sequenceDiagram
    participant AR as AR_MGMT
    participant INV as INV_CTRL
    
    Note over AR,INV: Order Processing
    AR->>INV: Check Availability
    INV-->>AR: Stock Levels
    
    AR->>INV: Allocate Stock
    INV-->>AR: Allocation Confirmed
    
    Note over AR,INV: Shipment
    AR->>INV: Ship Confirmation
    INV->>INV: Relieve Inventory
    INV-->>AR: Inventory Updated
```

### With General Ledger (GL_CORE)

```mermaid
sequenceDiagram
    participant AR as AR_MGMT
    participant GL as GL_CORE
    
    Note over AR,GL: Daily Posting
    AR->>AR: Summarize Transactions
    AR->>GL: Send Journal Batch
    
    GL->>GL: Validate Entries
    GL->>GL: Post to Accounts
    GL-->>AR: Posting Confirmation
    
    Note over AR,GL: Period End
    AR->>GL: AR Balance
    GL->>GL: Reconcile
    GL-->>AR: Reconciliation Status
```

---

## Error Handling Flows

### Payment Application Errors

```mermaid
flowchart TD
    ERROR[Application Error] --> TYPE{Error Type}
    
    TYPE -->|Overpayment| CREDIT[Create Credit]
    TYPE -->|Wrong Customer| TRANSFER[Transfer Payment]
    TYPE -->|No Match| SUSPEND[Suspend Payment]
    TYPE -->|System Error| RETRY[Retry Application]
    
    CREDIT --> NOTIFY[Notify Customer]
    TRANSFER --> REAPPLY[Reapply Correct]
    SUSPEND --> RESEARCH[Research]
    RETRY --> CHECK{Success?}
    
    CHECK -->|Yes| COMPLETE[Complete]
    CHECK -->|No| MANUAL[Manual Process]
    
    RESEARCH --> RESOLVE[Resolve Issue]
    RESOLVE --> REAPPLY
```

---

## Key Decision Points

### Order Processing Decisions

| Decision Point | Criteria | Action |
|---------------|----------|--------|
| Accept Order? | Credit OK, Stock available | Confirm or reject |
| Ship Partial? | Some items available | Ship available or wait |
| Apply Discount? | Within terms, good standing | Apply or skip |
| Charge Interest? | Past due date | Calculate or waive |

### Collection Decisions

| Decision Point | Criteria | Action |
|---------------|----------|--------|
| Collection Action? | Days overdue, amount | Letter, call, or legal |
| Write Off? | Age, amount, cost/benefit | Write off or pursue |
| Credit Hold? | Payment history | Hold or continue |
| Settlement Offer? | Collection likelihood | Offer or full amount |