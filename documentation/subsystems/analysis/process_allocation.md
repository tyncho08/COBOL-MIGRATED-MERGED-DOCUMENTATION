# ACAS Process Allocation Map

## Overview

This document maps all business processes to their owning subsystems, clarifying which subsystem is responsible for each end-to-end process and how processes span multiple subsystems.

## Process Ownership Principles

1. **Process Owner**: The subsystem that initiates and controls the process
2. **Process Participants**: Subsystems that contribute to the process
3. **Process Boundaries**: Clear handoff points between subsystems
4. **Process Integrity**: Owner ensures end-to-end completion
5. **Process Recovery**: Owner handles rollback/compensation

## Core Business Processes

### 1. Order-to-Cash (O2C) Process

**Process Owner**: AR_MGMT  
**Process Span**: Customer Order → Cash Receipt

| Sub-Process | Owner | Participants | Key Activities |
|------------|-------|--------------|----------------|
| Order Entry | AR_MGMT | MDM, INV_CTRL | Validate customer, check credit, verify stock |
| Order Fulfillment | AR_MGMT | INV_CTRL | Allocate stock, create picking list |
| Shipping | AR_MGMT | INV_CTRL | Update stock, create delivery note |
| Invoicing | AR_MGMT | GL_CORE | Generate invoice, post revenue |
| Collection | AR_MGMT | - | Send statements, follow up |
| Cash Application | AR_MGMT | GL_CORE | Apply payment, update GL |

**Process Flow**:
```
Customer Order → Credit Check → Stock Check → Allocation
    ↓
Invoice Generation → GL Posting → Statement → Payment
    ↓
Cash Application → GL Update → Close
```

### 2. Procure-to-Pay (P2P) Process

**Process Owner**: AP_MGMT  
**Process Span**: Purchase Requisition → Supplier Payment

| Sub-Process | Owner | Participants | Key Activities |
|------------|-------|--------------|----------------|
| Requisition | AP_MGMT | MDM | Create requirement, approval |
| Purchase Order | AP_MGMT | MDM, INV_CTRL | Create PO, send to supplier |
| Goods Receipt | AP_MGMT | INV_CTRL | Receive goods, update stock |
| Invoice Matching | AP_MGMT | - | 3-way match, approve |
| Payment Processing | AP_MGMT | GL_CORE | Schedule payment, update GL |
| Supplier Management | AP_MGMT | MDM | Performance tracking |

**Process Flow**:
```
Requisition → PO Creation → Approval → Send PO
    ↓
Goods Receipt → Invoice Receipt → 3-Way Match
    ↓
Payment Approval → Payment Run → GL Update
```

### 3. Period-End Close Process

**Process Owner**: GL_CORE  
**Process Span**: Sub-ledger Close → Financial Statements

| Sub-Process | Owner | Participants | Key Activities |
|------------|-------|--------------|----------------|
| Daily Close | GL_CORE | All modules | Verify daily postings |
| Sub-ledger Close | GL_CORE | AR, AP, INV | Close AR/AP/Inventory |
| Accruals | GL_CORE | - | Post accrual entries |
| Reconciliations | GL_CORE | All modules | Reconcile accounts |
| Financial Close | GL_CORE | - | Generate statements |
| Reporting | GL_CORE | RPT_ENGINE | Distribute reports |

**Process Flow**:
```
Daily Verification → Sub-ledger Cutoff → Post Accruals
    ↓
Run Reconciliations → Close Period → Generate Reports
    ↓
Review & Approve → Distribute → Archive
```

### 4. Inventory Management Process

**Process Owner**: INV_CTRL  
**Process Span**: Receipt → Issue → Valuation

| Sub-Process | Owner | Participants | Key Activities |
|------------|-------|--------------|----------------|
| Goods Receipt | INV_CTRL | AP_MGMT | Update stock, calculate cost |
| Stock Transfer | INV_CTRL | - | Move between locations |
| Goods Issue | INV_CTRL | AR_MGMT | Reduce stock, cost of sales |
| Cycle Count | INV_CTRL | - | Physical verification |
| Revaluation | INV_CTRL | GL_CORE | Adjust values, post variance |
| Reorder | INV_CTRL | AP_MGMT | Generate purchase requests |

### 5. Customer Management Process

**Process Owner**: MDM  
**Process Span**: Prospect → Active Customer → Inactive

| Sub-Process | Owner | Participants | Key Activities |
|------------|-------|--------------|----------------|
| Customer Onboarding | MDM | AR_MGMT | Create master, credit check |
| Credit Management | MDM | AR_MGMT | Set limits, monitor usage |
| Data Maintenance | MDM | - | Update information |
| Classification | MDM | AR_MGMT | Assign groups, terms |
| Deactivation | MDM | AR_MGMT | Block transactions |

## Supporting Processes

### 6. Report Generation Process

**Process Owner**: RPT_ENGINE  
**Participants**: All data-owning subsystems

| Report Type | Data Sources | Schedule |
|------------|--------------|----------|
| Operational | AR, AP, INV | Daily |
| Financial | GL_CORE | Monthly |
| Management | All modules | On-demand |
| Regulatory | GL, AR, AP | Periodic |
| Analytics | All modules | Ad-hoc |

### 7. Batch Processing

**Process Owner**: BATCH_FW  
**Participants**: All subsystems requiring scheduled processing

| Job Category | Subsystems | Timing |
|-------------|------------|---------|
| Daily Extract | AR, AP, INV | 06:00 |
| Transaction Processing | All | Throughout day |
| Interface Jobs | INTEGRATION | Hourly |
| End-of-Day | All | 22:00 |
| Month-End | All | 1st of month |

### 8. Security Administration

**Process Owner**: SEC_AUDIT  
**Participants**: All subsystems

| Process | Activities | Frequency |
|---------|------------|-----------|
| User Management | Create, modify, disable | On-demand |
| Access Control | Grant, revoke permissions | On-demand |
| Audit Monitoring | Review logs, alerts | Continuous |
| Compliance Reporting | Generate audit reports | Monthly |

## Utility Processes

### 9. Date Management

**Process Owner**: DATE_UTIL  
**Participants**: All date-dependent subsystems

| Function | Used By | Purpose |
|----------|---------|---------|
| Period Calculation | GL_CORE | Financial periods |
| Due Date Calc | AR, AP | Payment terms |
| Aging Calculation | AR, AP | Overdue analysis |
| Working Days | All | Business calendars |

### 10. Error Handling

**Process Owner**: ERROR_FW  
**Participants**: All subsystems

| Error Type | Handling | Recovery |
|-----------|----------|----------|
| Business Error | Log, notify user | User action |
| System Error | Log, alert admin | Auto retry |
| Data Error | Log, queue | Manual fix |
| Interface Error | Log, retry | Escalate |

## Cross-Functional Processes

### Multi-Currency Processing

**Participants**: CURR_UTIL, AR_MGMT, AP_MGMT, GL_CORE

```
Transaction Entry → Currency Validation → Rate Lookup
    ↓
Calculate Base Amount → Round Appropriately
    ↓
Store Both Amounts → GL Posting in Base
```

### Tax Processing

**Participants**: AR_MGMT, AP_MGMT, GL_CORE

```
Transaction → Tax Determination → Tax Calculation
    ↓
Tax Posting → Tax Reporting → Tax Payment
```

## Process Performance Metrics

| Process | SLA | Current | Volume |
|---------|-----|---------|--------|
| Order Entry | < 3 min | 2.5 min | 500/day |
| Invoice Generation | < 30 sec | 25 sec | 1000/day |
| Payment Processing | < 2 min | 1.5 min | 300/day |
| Month-End Close | < 4 hours | 3.5 hours | Monthly |
| Report Generation | < 5 min | Varies | 50/day |

## Process Dependencies

### Critical Path Processes

1. **Daily Revenue Cycle**
   ```
   Orders (08:00-17:00) → Invoicing (17:00) → GL Post (18:00)
   ```

2. **Daily Payment Cycle**
   ```
   Payment Selection (14:00) → Approval (15:00) → Bank File (16:00)
   ```

3. **Month-End Sequence**
   ```
   Day 1: Close Transactions
   Day 2: Run Reconciliations  
   Day 3: Post Adjustments
   Day 4: Generate Reports
   Day 5: Distribute Statements
   ```

## Process Modernization Opportunities

### Real-Time Processing

| Current Batch | Proposed Real-Time | Benefit |
|--------------|-------------------|---------|
| Invoice Generation | On-demand invoicing | Customer service |
| Stock Updates | Real-time availability | Accuracy |
| GL Posting | Continuous posting | Timely reports |
| Credit Checks | Real-time validation | Risk reduction |

### Process Automation

| Manual Process | Automation Opportunity | Technology |
|---------------|----------------------|-----------|
| Data Entry | OCR/API integration | AI/ML |
| Matching | Auto 3-way match | Rules engine |
| Approval | Workflow automation | BPM |
| Reporting | Self-service BI | Analytics platform |

## Process Governance

### Process Ownership Matrix

| Level | Role | Responsibility |
|-------|------|----------------|
| Strategic | Process Owner | End-to-end accountability |
| Tactical | Subsystem Owner | Component delivery |
| Operational | User | Execute activities |

### Change Control

1. **Process Changes**: Require owner approval
2. **Interface Changes**: Require participant agreement
3. **Timing Changes**: Require impact analysis
4. **New Processes**: Require architecture review