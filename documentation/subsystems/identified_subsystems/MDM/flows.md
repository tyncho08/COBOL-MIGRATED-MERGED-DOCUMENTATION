# MDM Process Flows

## Overview

The MDM subsystem manages the lifecycle of all master data entities. These flows ensure data quality, consistency, and controlled access across the enterprise.

## Major Process Flows

### 1. Customer Master Lifecycle Flow

**Purpose**: Manage customer data from initial creation through archival

```mermaid
stateDiagram-v2
    [*] --> Prospect: Initial Contact
    
    Prospect --> Submitted: Credit Application
    Submitted --> UnderReview: Credit Check
    
    UnderReview --> Approved: Credit OK
    UnderReview --> Declined: Credit Failed
    
    Declined --> [*]: End
    
    Approved --> Active: Setup Complete
    
    Active --> OnHold: Credit Issues
    Active --> Inactive: No Activity
    Active --> Blocked: Compliance Issue
    
    OnHold --> Active: Resolved
    OnHold --> Inactive: Unresolved
    
    Blocked --> UnderReview: Investigation
    
    Inactive --> Active: Reactivated
    Inactive --> Archived: Retention Met
    
    Archived --> [*]: End
```

**Process Details**:

```mermaid
flowchart TD
    START[New Customer Request] --> SEARCH{Duplicate Check}
    
    SEARCH -->|Found| EXISTING[Use Existing]
    SEARCH -->|Not Found| CREATE[Create New]
    
    CREATE --> VALIDATE{Validate Data}
    VALIDATE -->|Invalid| FIX[Fix Issues]
    VALIDATE -->|Valid| TAXID{Verify Tax ID}
    
    FIX --> VALIDATE
    
    TAXID -->|Invalid| REJECT[Reject]
    TAXID -->|Valid| ADDRESS{Validate Address}
    
    ADDRESS -->|Invalid| MANUAL[Manual Review]
    ADDRESS -->|Valid| CREDIT[Credit Check]
    
    MANUAL --> CREDIT
    
    CREDIT -->|Approved| SETUP[Complete Setup]
    CREDIT -->|Declined| REJECT
    CREDIT -->|Review| DOCS[Request Docs]
    
    DOCS --> REVIEW[Review Docs]
    REVIEW --> CREDIT
    
    SETUP --> NOTIFY[Notify Systems]
    NOTIFY --> ACTIVE[Activate Customer]
    
    REJECT --> END[End]
    EXISTING --> END
    ACTIVE --> END
```

---

### 2. Vendor Master Onboarding Flow

**Purpose**: Ensure vendors meet compliance requirements before activation

```mermaid
flowchart LR
    subgraph "Vendor Submission"
        REQUEST[Vendor Request] --> W9{W9 Provided?}
        W9 -->|No| REQUESTW9[Request W9]
        W9 -->|Yes| VALIDATE[Validate TIN]
        REQUESTW9 --> W9
    end
    
    subgraph "Compliance Check"
        VALIDATE --> OFAC{OFAC Check}
        OFAC -->|Clear| DUNS{D&B Check}
        OFAC -->|Hit| REVIEW[Compliance Review]
        DUNS -->|Valid| FINANCE[Finance Review]
        DUNS -->|Invalid| VERIFY[Verify Info]
    end
    
    subgraph "Approval"
        FINANCE --> TERMS[Set Terms]
        TERMS --> BANK[Bank Details]
        BANK --> APPROVE{Final Approval}
        APPROVE -->|Yes| ACTIVATE[Activate]
        APPROVE -->|No| REJECT[Reject]
        REVIEW --> APPROVE
        VERIFY --> FINANCE
    end
```

**Compliance Validation Details**:

```mermaid
sequenceDiagram
    participant VEN as Vendor
    participant MDM as MDM System
    participant OFAC as OFAC Service
    participant TIN as TIN Match
    participant DUN as D&B Service
    
    VEN->>MDM: Submit Application
    MDM->>MDM: Validate W9 Data
    
    MDM->>TIN: Verify TIN/Name
    TIN-->>MDM: Match Result
    
    alt TIN Mismatch
        MDM->>VEN: Request Clarification
        VEN->>MDM: Updated Info
    end
    
    MDM->>OFAC: Screen Entity
    OFAC-->>MDM: Screening Result
    
    alt OFAC Hit
        MDM->>MDM: Flag for Review
        MDM->>MDM: Manual Investigation
    end
    
    MDM->>DUN: Business Verification
    DUN-->>MDM: Business Profile
    
    MDM->>MDM: Calculate Risk Score
    MDM->>MDM: Set Approval Level
```

---

### 3. Item Master Management Flow

**Purpose**: Control product information lifecycle

```mermaid
flowchart TD
    subgraph "Item Creation"
        NEW[New Item Request] --> TYPE{Item Type}
        TYPE -->|Purchased| VENDOR[Select Vendor]
        TYPE -->|Manufactured| BOM[Create BOM]
        TYPE -->|Service| SERVICE[Service Details]
        
        VENDOR --> DETAILS[Item Details]
        BOM --> DETAILS
        SERVICE --> DETAILS
    end
    
    subgraph "Validation"
        DETAILS --> CHECK{Validation}
        CHECK -->|Duplicate SKU| ERROR1[Reject]
        CHECK -->|Invalid UOM| ERROR2[Fix UOM]
        CHECK -->|Valid| ENRICH[Enrich Data]
        ERROR2 --> DETAILS
    end
    
    subgraph "Approval & Activation"
        ENRICH --> CATEGORY[Assign Category]
        CATEGORY --> PRICING[Set Pricing]
        PRICING --> APPROVE{Approval}
        APPROVE -->|Approved| ACTIVATE[Activate]
        APPROVE -->|Rejected| REVISE[Revise]
        REVISE --> DETAILS
    end
    
    subgraph "Maintenance"
        ACTIVATE --> MONITOR[Monitor Usage]
        MONITOR --> UPDATE{Updates?}
        UPDATE -->|Yes| CHANGE[Change Process]
        UPDATE -->|No| REVIEW{Periodic Review}
        CHANGE --> ACTIVATE
        REVIEW -->|Active| MONITOR
        REVIEW -->|Obsolete| PHASE[Phase Out]
    end
```

---

### 4. Chart of Accounts Maintenance Flow

**Purpose**: Maintain financial account structure with controls

```mermaid
flowchart TD
    REQUEST[GL Account Request] --> PURPOSE{Purpose}
    
    PURPOSE -->|New Dept| DEPT[Department Setup]
    PURPOSE -->|New Account| ACCT[Account Setup]
    PURPOSE -->|Restructure| REORG[Reorganization]
    
    DEPT --> VALIDATE{Validate Structure}
    ACCT --> VALIDATE
    REORG --> IMPACT[Impact Analysis]
    
    IMPACT --> VALIDATE
    
    VALIDATE -->|Invalid| CORRECT[Corrections]
    VALIDATE -->|Valid| APPROVE{Controller Approval}
    
    CORRECT --> VALIDATE
    
    APPROVE -->|Denied| REJECT[Return]
    APPROVE -->|Approved| IMPLEMENT[Implement]
    
    IMPLEMENT --> TEST{Test Posting}
    TEST -->|Success| ACTIVATE[Activate]
    TEST -->|Failed| ROLLBACK[Rollback]
    
    ROLLBACK --> CORRECT
    ACTIVATE --> NOTIFY[Notify Users]
```

---

### 5. Data Quality Management Flow

**Purpose**: Continuous monitoring and improvement of data quality

```mermaid
flowchart LR
    subgraph "Detection"
        SCHEDULE[Daily Schedule] --> SCAN[Quality Scan]
        SCAN --> ISSUES{Issues Found?}
        ISSUES -->|Yes| CATEGORIZE[Categorize]
        ISSUES -->|No| REPORT1[Clean Report]
    end
    
    subgraph "Resolution"
        CATEGORIZE --> PRIORITY{Priority}
        PRIORITY -->|Critical| IMMEDIATE[Fix Now]
        PRIORITY -->|High| QUEUE[Work Queue]
        PRIORITY -->|Low| BATCH[Batch Fix]
        
        IMMEDIATE --> FIX[Apply Fix]
        QUEUE --> ASSIGN[Assign Owner]
        ASSIGN --> FIX
        BATCH --> SCHEDULED[Scheduled Fix]
    end
    
    subgraph "Verification"
        FIX --> VERIFY{Verify Fix}
        SCHEDULED --> VERIFY
        VERIFY -->|Failed| ESCALATE[Escalate]
        VERIFY -->|Success| LOG[Log Resolution]
        ESCALATE --> FIX
        LOG --> REPORT2[Update Report]
    end
```

**Quality Check Examples**:

```mermaid
graph TD
    subgraph "Customer Quality Checks"
        C1[Missing Address] --> FIX1[Request Address]
        C2[Invalid Email] --> FIX2[Validate Format]
        C3[Duplicate TaxID] --> FIX3[Merge Records]
        C4[Incomplete Phone] --> FIX4[Standardize Format]
    end
    
    subgraph "Vendor Quality Checks"  
        V1[Expired W9] --> FIX5[Request Update]
        V2[Missing Bank Info] --> FIX6[For Active Vendors]
        V3[Invalid Address] --> FIX7[USPS Validation]
    end
    
    subgraph "Item Quality Checks"
        I1[No Category] --> FIX8[Assign Category]
        I2[Missing Weight] --> FIX9[For Shipped Items]
        I3[Duplicate Desc] --> FIX10[Make Unique]
    end
```

---

### 6. Code Table Management Flow

**Purpose**: Maintain system-wide reference codes

```mermaid
stateDiagram-v2
    [*] --> Active: Create Code
    
    Active --> Modified: Update Request
    Modified --> UnderReview: Needs Approval
    
    UnderReview --> Active: Approved
    UnderReview --> Modified: Rejected
    
    Active --> Scheduled: Future Inactive
    Scheduled --> Inactive: Effective Date
    
    Active --> Inactive: Immediate Inactive
    
    Inactive --> Active: Reactivate
    Inactive --> Archived: Retention Period
    
    Archived --> [*]: Purged
```

---

### 7. Master Data Search Flow

**Purpose**: Efficient search across master data

```mermaid
flowchart TD
    SEARCH[Search Request] --> PARSE{Parse Criteria}
    
    PARSE -->|Exact ID| DIRECT[Direct Lookup]
    PARSE -->|Name| FUZZY[Fuzzy Search]
    PARSE -->|Multiple| ADVANCED[Advanced Search]
    
    DIRECT --> CACHE{In Cache?}
    CACHE -->|Yes| RETURN1[Return Result]
    CACHE -->|No| DATABASE1[Query Database]
    DATABASE1 --> CACHE1[Update Cache]
    CACHE1 --> RETURN1
    
    FUZZY --> SOUNDEX[Soundex Match]
    FUZZY --> LIKE[Like Match]
    FUZZY --> DISTANCE[Edit Distance]
    
    SOUNDEX --> SCORE[Score Results]
    LIKE --> SCORE
    DISTANCE --> SCORE
    
    ADVANCED --> FILTER[Apply Filters]
    FILTER --> SORT[Sort Results]
    SORT --> PAGE[Paginate]
    
    SCORE --> RANK[Rank by Score]
    RANK --> RETURN2[Return List]
    PAGE --> RETURN2
```

---

### 8. Cross-Reference Management Flow

**Purpose**: Maintain relationships between entities

```mermaid
flowchart TD
    subgraph "Relationship Types"
        REL[Relationship] --> TYPE{Type?}
        TYPE -->|Parent/Child| HIERARCHY[Hierarchy]
        TYPE -->|Peer| ASSOCIATION[Association]
        TYPE -->|Alternate| ALIAS[Alias]
    end
    
    subgraph "Validation"
        HIERARCHY --> VAL1[No Circular]
        ASSOCIATION --> VAL2[Valid Entities]
        ALIAS --> VAL3[Not Duplicate]
        
        VAL1 --> CREATE[Create Link]
        VAL2 --> CREATE
        VAL3 --> CREATE
    end
    
    subgraph "Maintenance"
        CREATE --> MAINTAIN[Maintain Links]
        MAINTAIN --> UPDATE{Changes?}
        UPDATE -->|Entity Deleted| CASCADE[Handle Cascade]
        UPDATE -->|Entity Merged| RELINK[Update Links]
        CASCADE --> NOTIFY[Notify Affected]
        RELINK --> NOTIFY
    end
```

---

### 9. Data Access Control Flow

**Purpose**: Ensure authorized access to master data

```mermaid
sequenceDiagram
    participant USER as User/System
    participant AUTH as Authorization
    participant MDM as MDM Service
    participant AUDIT as Audit Log
    
    USER->>AUTH: Request Access
    AUTH->>AUTH: Check Permissions
    
    alt Authorized
        AUTH->>MDM: Forward Request
        MDM->>MDM: Process Request
        
        alt Read Operation
            MDM->>USER: Return Data
        else Write Operation
            MDM->>MDM: Validate Changes
            MDM->>MDM: Apply Changes
            MDM->>USER: Confirm Update
        end
        
        MDM->>AUDIT: Log Access
    else Unauthorized
        AUTH->>USER: Access Denied
        AUTH->>AUDIT: Log Denial
    end
```

---

## Integration Points

### Real-time Data Access Pattern

```mermaid
graph LR
    subgraph "Requesting Systems"
        AR[AR_MGMT]
        AP[AP_MGMT]
        INV[INV_CTRL]
    end
    
    subgraph "MDM Services"
        API[API Layer]
        CACHE[Cache Layer]
        DB[(Master DB)]
    end
    
    AR --> API
    AP --> API
    INV --> API
    
    API --> CACHE
    CACHE -->|Hit| API
    CACHE -->|Miss| DB
    DB --> CACHE
```

### Batch Synchronization Pattern

```mermaid
sequenceDiagram
    participant MDM
    participant EXTRACT as Extract Process
    participant FILE as File System
    participant TARGET as Target Systems
    
    Note over MDM: Daily 04:00
    
    MDM->>EXTRACT: Trigger Extract
    EXTRACT->>MDM: Query Changes
    MDM-->>EXTRACT: Changed Records
    
    EXTRACT->>FILE: Write Extract File
    FILE->>TARGET: File Available
    
    TARGET->>FILE: Read File
    TARGET->>TARGET: Process Updates
    TARGET->>MDM: Confirm Receipt
```

---

## Error Handling Flows

### Data Quality Error Resolution

```mermaid
flowchart TD
    ERROR[Quality Error] --> CLASSIFY{Error Type}
    
    CLASSIFY -->|Format| AUTO[Auto-Correct]
    CLASSIFY -->|Missing| REQUEST[Request Data]
    CLASSIFY -->|Invalid| MANUAL[Manual Review]
    CLASSIFY -->|Duplicate| MERGE[Merge Process]
    
    AUTO --> VERIFY{Success?}
    REQUEST --> WAIT[Wait Response]
    MANUAL --> ASSIGN[Assign to User]
    MERGE --> COMPARE[Compare Records]
    
    VERIFY -->|Yes| RESOLVED
    VERIFY -->|No| MANUAL
    
    WAIT --> RECEIVED{Received?}
    RECEIVED -->|Yes| VALIDATE[Validate]
    RECEIVED -->|No| ESCALATE[Escalate]
    
    ASSIGN --> FIX[Manual Fix]
    COMPARE --> SELECT[Select Master]
    
    VALIDATE --> RESOLVED
    FIX --> RESOLVED
    SELECT --> RESOLVED
    ESCALATE --> RESOLVED
    
    RESOLVED[Mark Resolved]
```

---

## Key Decision Points

### Data Governance Decisions

| Decision | Criteria | Action |
|----------|----------|--------|
| Approve new customer? | Credit check, compliance | Activate or reject |
| Allow duplicate? | Business justification | Create or merge |
| Inactivate master? | No dependencies | Inactivate or retain |
| Override quality rule? | Business need vs risk | Override or enforce |

### Performance Optimization Decisions

| Decision | Criteria | Action |
|----------|----------|--------|
| Cache data? | Access frequency > 10/hour | Add to cache |
| Index field? | Search frequency > 100/day | Create index |
| Archive data? | Last used > 2 years | Move to archive |
| Denormalize? | Join cost > 100ms | Denormalize |