# SEC_AUDIT Subsystem - Business Flow Documentation

## Overview

This document details the security and audit flows within the SEC_AUDIT subsystem, showing how user authentication, authorization, and audit logging are managed throughout the ACAS system.

## Process Flow Diagrams

### 1. User Authentication Flow

```mermaid
flowchart TD
    Start([Login Request]) --> A[Validate User ID]
    A --> B{User Exists?}
    B -->|No| C[Log Failed Attempt]
    B -->|Yes| D[Check Account Status]
    
    D --> E{Account Active?}
    E -->|No| F[Log Blocked Attempt]
    E -->|Yes| G[Validate Password]
    
    G --> H{Password Correct?}
    H -->|No| I[Increment Failed Count]
    H -->|Yes| J[Check Password Expiry]
    
    I --> K{Max Attempts Reached?}
    K -->|Yes| L[Lock Account]
    K -->|No| M[Return Auth Failed]
    
    J --> N{Password Expired?}
    N -->|Yes| O[Force Password Change]
    N -->|No| P[Generate Session Token]
    
    P --> Q[Log Successful Login]
    Q --> R[Return Auth Success]
    
    C --> End([Authentication Complete])
    F --> End
    L --> End
    M --> End
    O --> End
    R --> End
```

### 2. Authorization Check Flow

```mermaid
sequenceDiagram
    participant User as User Request
    participant Auth as Authorization Engine
    participant RBAC as Role-Based Access
    participant Audit as Audit Logger
    
    User->>Auth: Request Resource Access
    Auth->>Auth: Validate Session Token
    Auth->>RBAC: Check User Permissions
    RBAC->>RBAC: Evaluate Role Permissions
    RBAC->>Auth: Return Permission Result
    
    alt Access Granted
        Auth->>Audit: Log Authorized Access
        Auth->>User: Grant Access
    else Access Denied
        Auth->>Audit: Log Access Denial
        Auth->>User: Deny Access
    end
    
    Audit->>Audit: Store Audit Record
```

### 3. Security Monitoring Flow

```mermaid
flowchart LR
    subgraph "Event Detection"
        A[Security Event] --> B[Event Classification]
        B --> C[Risk Assessment]
    end
    
    subgraph "Response Determination"
        D[Apply Security Rules] --> E[Determine Response]
        E --> F[Generate Alerts]
    end
    
    subgraph "Action Execution"
        G[Automated Response] --> H[Manual Investigation]
        H --> I[Incident Resolution]
    end
    
    C --> D
    F --> G
    I --> J[Update Security Policies]
```

## Business Rules in Flows

### Authentication Rules
- **RULE_AUTH_001**: Maximum 3 failed login attempts before account lockout
- **RULE_AUTH_002**: Passwords must be changed every 90 days
- **RULE_AUTH_003**: Session timeout after 30 minutes of inactivity
- **RULE_AUTH_004**: Concurrent sessions limited to 2 per user

### Authorization Rules
- **RULE_AUTHZ_001**: Users can only access resources assigned to their role
- **RULE_AUTHZ_002**: Financial transactions require dual authorization above thresholds
- **RULE_AUTHZ_003**: Administrative functions require elevated privileges
- **RULE_AUTHZ_004**: Time-based access restrictions enforced for sensitive operations

### Audit Rules
- **RULE_AUDIT_001**: All data modifications must be logged with before/after values
- **RULE_AUDIT_002**: Failed access attempts logged with full context
- **RULE_AUDIT_003**: Audit records are tamper-proof and digitally signed
- **RULE_AUDIT_004**: Audit retention follows regulatory requirements (7 years minimum)

### Security Monitoring Rules
- **RULE_MON_001**: Unusual access patterns trigger security alerts
- **RULE_MON_002**: Multiple failed logins from same IP trigger blocking
- **RULE_MON_003**: After-hours access to sensitive data requires justification
- **RULE_MON_004**: Privileged account usage monitored continuously