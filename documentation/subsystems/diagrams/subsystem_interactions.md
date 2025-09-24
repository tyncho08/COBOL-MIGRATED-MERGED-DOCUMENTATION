# ACAS Subsystem Interaction Diagrams

## Overview

This document provides detailed interaction diagrams showing how the 14 ACAS subsystems communicate and collaborate to deliver business functionality.

## Master Subsystem Interaction Map

```mermaid
graph TB
    subgraph "Utility Layer"
        DATE[DATE_UTIL]
        CURR[CURR_UTIL]
        ERR[ERROR_FW]
    end
    
    subgraph "Infrastructure Layer"
        FILE[FILE_SVC]
        SEC[SEC_AUDIT]
        BATCH[BATCH_FW]
        INT[INTEGRATION]
    end
    
    subgraph "Data Layer"
        MDM[MDM]
    end
    
    subgraph "Business Logic Layer"
        AR[AR_MGMT]
        AP[AP_MGMT]
        INV[INV_CTRL]
        IRS[IRS_PROC]
    end
    
    subgraph "Core Financial Layer"
        GL[GL_CORE]
    end
    
    subgraph "Presentation Layer"
        RPT[RPT_ENGINE]
    end
    
    %% Utility Dependencies
    AR --> DATE
    AR --> CURR
    AP --> DATE
    AP --> CURR
    GL --> DATE
    GL --> CURR
    
    %% Error Framework
    ALL[All Subsystems] -.-> ERR
    
    %% File Services
    ALL --> FILE
    
    %% Master Data
    AR --> MDM
    AP --> MDM
    INV --> MDM
    SEC --> MDM
    
    %% GL Posting
    AR --> GL
    AP --> GL
    INV --> GL
    IRS --> GL
    
    %% Inventory
    AR --> INV
    AP --> INV
    
    %% Reporting
    GL --> RPT
    AR --> RPT
    AP --> RPT
    INV --> RPT
    
    %% Security
    ALL --> SEC
    
    %% Batch Processing
    BATCH --> ALL
```

## Interaction Patterns

### 1. Synchronous Request-Response Pattern

Used for real-time data access and validation

```mermaid
sequenceDiagram
    participant UI as User Interface
    participant AR as AR_MGMT
    participant MDM as MDM
    participant INV as INV_CTRL
    
    UI->>AR: Create Order
    AR->>MDM: Validate Customer
    MDM-->>AR: Customer Valid
    AR->>MDM: Get Credit Limit
    MDM-->>AR: Credit Info
    AR->>INV: Check Stock
    INV-->>AR: Available Qty
    AR->>INV: Reserve Stock
    INV-->>AR: Confirmed
    AR-->>UI: Order Created
```

### 2. Asynchronous Batch Pattern

Used for bulk processing and GL posting

```mermaid
sequenceDiagram
    participant BATCH as BATCH_FW
    participant AR as AR_MGMT
    participant FILE as FILE_SVC
    participant GL as GL_CORE
    participant RPT as RPT_ENGINE
    
    Note over BATCH: Daily 18:00
    
    BATCH->>AR: Trigger Daily Close
    AR->>FILE: Generate GL Batch
    FILE-->>AR: File Created
    AR->>BATCH: Close Complete
    
    Note over BATCH: Daily 18:30
    
    BATCH->>GL: Process AR Batch
    GL->>FILE: Read Batch File
    FILE-->>GL: Batch Data
    GL->>GL: Post Entries
    GL->>RPT: Update Reports
    GL->>BATCH: Posting Complete
```

### 3. Event-Driven Pattern

Used for audit and monitoring

```mermaid
graph LR
    subgraph "Event Sources"
        AR[AR_MGMT]
        AP[AP_MGMT]
        GL[GL_CORE]
    end
    
    subgraph "Event Bus"
        QUEUE[Event Queue]
    end
    
    subgraph "Event Consumers"
        SEC[SEC_AUDIT]
        ERR[ERROR_FW]
        RPT[RPT_ENGINE]
    end
    
    AR -->|Transaction Events| QUEUE
    AP -->|Payment Events| QUEUE
    GL -->|Posting Events| QUEUE
    
    QUEUE -->|Audit Events| SEC
    QUEUE -->|Error Events| ERR
    QUEUE -->|Report Events| RPT
```

## Critical Interaction Flows

### Order-to-Cash Interaction Flow

```mermaid
graph TB
    START[Customer Order] --> AR_VAL{AR Validation}
    
    AR_VAL -->|Check Customer| MDM_CUST[MDM: Customer Data]
    AR_VAL -->|Check Credit| AR_CREDIT[AR: Credit Check]
    
    AR_CREDIT -->|Check Stock| INV_CHECK[INV: Availability]
    INV_CHECK -->|Available| INV_ALLOC[INV: Allocate]
    INV_CHECK -->|Not Available| BACKORDER[AR: Backorder]
    
    INV_ALLOC --> AR_INV[AR: Create Invoice]
    AR_INV -->|Post Revenue| GL_POST[GL: Journal Entry]
    
    GL_POST --> CONFIRM[Order Confirmed]
    
    AR_INV -->|Update Stats| RPT_UPDATE[RPT: Statistics]
    
    style START fill:#90EE90
    style CONFIRM fill:#90EE90
```

### Month-End Close Interaction Flow

```mermaid
graph LR
    subgraph "Module Close"
        AR_CLS[AR Close]
        AP_CLS[AP Close]
        INV_CLS[INV Close]
    end
    
    subgraph "GL Processing"
        GL_VAL[GL Validation]
        GL_CLS[GL Close]
        GL_RPT[GL Reports]
    end
    
    subgraph "Reporting"
        RPT_FIN[Financial Reports]
        RPT_MGT[Management Reports]
    end
    
    AR_CLS -->|Status| GL_VAL
    AP_CLS -->|Status| GL_VAL
    INV_CLS -->|Status| GL_VAL
    
    GL_VAL -->|All Closed| GL_CLS
    GL_CLS --> GL_RPT
    
    GL_RPT --> RPT_FIN
    GL_RPT --> RPT_MGT
```

## Subsystem Communication Matrix

Detailed interaction frequency and data volumes:

| From | To | Type | Frequency | Volume | Critical? |
|------|----|----|-----------|---------|-----------|
| AR_MGMT | MDM | Sync | Per transaction | High | Yes |
| AR_MGMT | INV_CTRL | Sync | Per order line | High | Yes |
| AR_MGMT | GL_CORE | Batch | Daily | Medium | Yes |
| AP_MGMT | MDM | Sync | Per transaction | Medium | Yes |
| AP_MGMT | INV_CTRL | Sync | Per receipt | Medium | Yes |
| AP_MGMT | GL_CORE | Batch | Daily | Medium | Yes |
| INV_CTRL | GL_CORE | Batch | Daily | Low | Yes |
| IRS_PROC | GL_CORE | Batch | Weekly | Low | No |
| ALL | FILE_SVC | Sync | Continuous | Very High | Yes |
| ALL | ERROR_FW | Async | As needed | Low | No |
| ALL | SEC_AUDIT | Async | Continuous | Medium | Yes |
| BATCH_FW | ALL | Control | Scheduled | Low | Yes |

## Performance-Critical Interactions

### High-Performance Requirements

```mermaid
graph TD
    subgraph "Critical Path"
        USER[User Request]
        VAL[Validation]
        PROC[Processing]
        RESP[Response]
    end
    
    subgraph "Optimization"
        CACHE[MDM Cache]
        INDEX[Indexed Access]
        ASYNC[Async Updates]
    end
    
    USER -->|<100ms| VAL
    VAL -->|<200ms| PROC
    PROC -->|<500ms| RESP
    
    VAL -.-> CACHE
    PROC -.-> INDEX
    PROC -.-> ASYNC
    
    style USER fill:#ff9999
    style RESP fill:#99ff99
```

Target response times:
- Customer lookup: <100ms (cached)
- Stock check: <200ms
- Order creation: <2s total
- Report generation: <30s

## Error Propagation Paths

```mermaid
graph TD
    subgraph "Error Sources"
        VAL_ERR[Validation Error]
        SYS_ERR[System Error]
        BUS_ERR[Business Error]
    end
    
    subgraph "Error Handlers"
        LOCAL[Local Handler]
        CENTRAL[Central Handler]
        LOG[Error Logger]
    end
    
    subgraph "Recovery"
        RETRY[Retry Logic]
        FALLBACK[Fallback]
        ALERT[Alerts]
    end
    
    VAL_ERR --> LOCAL
    SYS_ERR --> CENTRAL
    BUS_ERR --> LOCAL
    
    LOCAL --> LOG
    CENTRAL --> LOG
    
    LOCAL --> RETRY
    CENTRAL --> FALLBACK
    CENTRAL --> ALERT
```

## Security Interaction Patterns

```mermaid
sequenceDiagram
    participant User
    participant App as Application
    participant SEC as SEC_AUDIT
    participant Module as Business Module
    participant AUDIT as Audit Log
    
    User->>App: Login Request
    App->>SEC: Authenticate
    SEC->>SEC: Validate Credentials
    SEC-->>App: Auth Token
    
    User->>App: Business Request
    App->>SEC: Authorize Action
    SEC->>SEC: Check Permissions
    SEC-->>App: Authorized
    
    App->>Module: Execute Request
    Module-->>App: Response
    
    App->>AUDIT: Log Activity
    App-->>User: Result
```

## Batch Processing Interactions

```mermaid
graph TB
    subgraph "Batch Controller"
        SCHED[Scheduler]
        CTRL[Controller]
        MON[Monitor]
    end
    
    subgraph "Batch Jobs"
        J1[Extract Jobs]
        J2[Process Jobs]
        J3[Load Jobs]
    end
    
    subgraph "Dependencies"
        DEP1[Job Dependencies]
        DEP2[Data Dependencies]
        DEP3[Time Dependencies]
    end
    
    SCHED --> CTRL
    CTRL --> J1
    CTRL --> J2
    CTRL --> J3
    
    J1 --> DEP1
    J2 --> DEP2
    J3 --> DEP3
    
    MON --> J1
    MON --> J2
    MON --> J3
```

## Integration Service Interactions

```mermaid
graph LR
    subgraph "Internal Systems"
        AR[AR_MGMT]
        AP[AP_MGMT]
        GL[GL_CORE]
    end
    
    subgraph "Integration Layer"
        INT[INTEGRATION]
        TRANS[Transform]
        ROUTE[Route]
    end
    
    subgraph "External Systems"
        BANK[Banks]
        TAX[Tax Systems]
        EDI[EDI Partners]
    end
    
    AR --> INT
    AP --> INT
    GL --> INT
    
    INT --> TRANS
    TRANS --> ROUTE
    
    ROUTE <--> BANK
    ROUTE <--> TAX
    ROUTE <--> EDI
```

## Real-Time vs Batch Decision Matrix

| Interaction | Current | Target | Rationale |
|------------|---------|--------|-----------|
| Customer validation | Sync | Sync | Immediate feedback |
| Stock check | Sync | Sync | Real-time accuracy |
| GL posting | Batch | Near real-time | Better visibility |
| Report generation | Batch | Mixed | On-demand + scheduled |
| Audit logging | Async | Async | Performance |
| Error handling | Mixed | Async | Decoupling |