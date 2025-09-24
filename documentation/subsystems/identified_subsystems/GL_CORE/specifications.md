# Subsystem: GL_CORE - General Ledger Core

## Executive Summary

**Purpose**: The GL_CORE subsystem is the central accounting engine of ACAS, maintaining the financial books of record, processing journal entries from all modules, and generating financial statements for management and regulatory reporting.

**Business Value**: Provides accurate, timely, and compliant financial reporting that enables business decision-making and satisfies regulatory requirements. Ensures financial integrity through balanced double-entry bookkeeping.

**Key Users**: 
- Finance team (daily operations)
- Management (financial reports)
- Auditors (compliance verification)
- Controllers (period-end processing)

**Criticality**: HIGH - System of record for all financial data

## Functional Capabilities

### Core Functions

1. **Chart of Accounts Management**
   - Description: Maintain hierarchical account structure
   - Business Rules: Account groups map to financial statements, validation of account types
   - Triggers: Manual maintenance, new business requirements
   - Outcomes: Valid account structure for posting

2. **Journal Entry Processing**
   - Description: Record financial transactions with full audit trail
   - Business Rules: Must balance (debits = credits), proper authorization, valid periods
   - Triggers: Manual entries, automated postings from sub-ledgers
   - Outcomes: Updated account balances, audit trail created

3. **Period Management**
   - Description: Control accounting periods for transaction posting
   - Business Rules: Only one period open at a time, sequential closing, no posting to closed periods
   - Triggers: Calendar-based, management decision
   - Outcomes: Period status updated, prior period locked

4. **Financial Reporting**
   - Description: Generate standard financial statements
   - Business Rules: Follow accounting standards, proper grouping and subtotals
   - Triggers: Period-end, on-demand
   - Outcomes: Balance sheet, income statement, cash flow

5. **Multi-Company Processing**
   - Description: Maintain separate books for multiple entities
   - Business Rules: Inter-company balancing, consolidated reporting
   - Triggers: Transaction posting, reporting requests
   - Outcomes: Company-specific and consolidated financials

6. **Budget Management**
   - Description: Track budget vs actual performance
   - Business Rules: Budget by period, variance calculations, approval workflow
   - Triggers: Budget entry, actual posting
   - Outcomes: Variance reports, alerts for overruns

### Business Processes Supported

- **Month-End Close**: Sequential closing of periods with validation
- **Year-End Processing**: Special period 13 handling, retained earnings rollover
- **Financial Consolidation**: Multi-company combined reporting
- **Regulatory Reporting**: Tax and compliance report generation
- **Management Reporting**: Departmental P&L, cost center analysis

## Data Domain

### Owned Entities

**GL Master File**
- Key Attributes: Account number, description, type, group
- Business Identifiers: Account number (natural key)
- Lifecycle: Created at setup, rarely deleted, archived after years

**GL Transactions**
- Key Attributes: Journal number, date, account, amount, reference
- Business Identifiers: Journal number + line number
- Lifecycle: Created by posting, permanent retention

**GL Balances**
- Key Attributes: Account, period, company, balance amounts
- Business Identifiers: Account + period + company
- Lifecycle: Created on first posting, rolled forward each period

**Period Control**
- Key Attributes: Period number, year, status, dates
- Business Identifiers: Year + period
- Lifecycle: Created annually, status changes through year

**Budget Data**
- Key Attributes: Account, period, budget amount, version
- Business Identifiers: Account + period + version
- Lifecycle: Annual creation, periodic updates

### Referenced Entities

**Company Master** (from MDM)
- Why needed: Multi-company processing
- Access: Read-only

**Cost Centers** (from MDM)
- Why needed: Departmental reporting
- Access: Read-only

**Source Documents** (from AR/AP/INV)
- Why needed: Drill-down capability
- Access: Read-only reference

## Interface Contracts

### Inbound Interfaces

| Interface ID | Source | Data Type | Frequency | Business Purpose |
|-------------|--------|-----------|-----------|------------------|
| AR-GL-001 | AR_MGMT | Journal batch file | Daily | Post sales/receipts |
| AP-GL-001 | AP_MGMT | Journal batch file | Daily | Post purchases/payments |
| INV-GL-001 | INV_CTRL | Journal batch file | Daily | Post inventory movements |
| IRS-GL-001 | IRS_PROC | Journal batch file | Weekly | Post simplified entries |
| MAN-GL-001 | User entry | Screen input | Real-time | Manual journal entries |

### Outbound Interfaces

| Interface ID | Target | Data Type | Frequency | Business Purpose |
|-------------|--------|-----------|-----------|------------------|
| GL-RPT-001 | RPT_ENGINE | Balance file | On-demand | Financial statements |
| GL-RPT-002 | RPT_ENGINE | Transaction file | On-demand | Transaction reports |
| GL-AUD-001 | SEC_AUDIT | Change log | Real-time | Audit trail |

### Internal APIs/Services

**Post Journal Entry**: [Entry Data] → [Posting Status]
- Purpose: Accept journal entries for posting
- Validation: Balanced entry, valid accounts, open period
- Error Handling: Return specific error codes for validation failures

**Get Account Balance**: [Account, Period] → [Balance]
- Purpose: Retrieve account balance for period
- Validation: Valid account, valid period
- Error Handling: Zero if no activity

**Close Period**: [Period] → [Status]
- Purpose: Close accounting period
- Validation: All sub-ledgers closed, trial balance balances
- Error Handling: List of blocking issues

## Business Rules Engine

### Validation Rules

- **VR001**: Journal entries must balance (debits = credits)
- **VR002**: Posting only allowed to open periods
- **VR003**: Account must be active and posting-enabled
- **VR004**: Inter-company entries must balance by company
- **VR005**: No posting to control accounts from manual entry

### Calculation Rules

- **CR001**: Running balance = Previous balance + Period activity
- **CR002**: YTD balance = Sum of periods 1 through current
- **CR003**: Variance = Actual - Budget (unfavorable if negative for revenue)
- **CR004**: Retained earnings = Prior year + Net income - Dividends

### Workflow Rules

- **WF001**: Period close requires all sub-ledgers closed first
- **WF002**: Year-end close creates opening balances for new year
- **WF003**: Budget amendments require approval above threshold

## Operational Characteristics

### Processing Patterns

**Batch Processing**:
- Daily posting runs at 18:00 (process sub-ledger files)
- Month-end close sequence (1st-5th of month)
- Year-end special processing (after final period)

**Real-time Processing**:
- Manual journal entry and approval
- Balance inquiries
- Report generation

**Peak Periods**:
- Month-end (days 1-5): High volume of adjustments
- Year-end: Extended processing window
- Audit periods: Heavy report generation

### Data Volumes

- Transaction Volume: 50,000 journal lines/month
- Data Growth Rate: 20% annually
- Retention Requirements: 7 years active, permanent archive

## Dependencies

### Upstream Dependencies
- AR_MGMT: Revenue and receipt postings
- AP_MGMT: Expense and payment postings
- INV_CTRL: Inventory and cost postings
- IRS_PROC: Simplified business postings

### Downstream Dependencies
- RPT_ENGINE: Financial statement generation
- SEC_AUDIT: Compliance reporting

### External Dependencies
- Tax authorities: Regulatory report formats
- Auditors: Access requirements

## Quality Attributes

### Performance Requirements
- Response Time: <2 seconds for inquiry
- Throughput: 1000 journal lines/minute
- Batch Windows: 4-hour month-end window

### Reliability Requirements
- Availability: 99.9% during business hours
- Recovery Time: <1 hour
- Recovery Point: Last successful backup

### Compliance Requirements
- SOX: Segregation of duties, audit trail
- GAAP: Accounting standards compliance
- Tax: Jurisdiction-specific requirements

## Evolution Potential

### Enhancement Opportunities
- Real-time posting from sub-ledgers
- Automated month-end close
- Predictive analytics on variances
- Multi-GAAP support

### Modernization Candidates
- Move to in-memory database for performance
- RESTful APIs for all functions
- Cloud-based with multi-region support
- AI-powered anomaly detection

### Known Limitations
- Single currency reporting at consolidated level
- Limited drill-down capability
- Manual reconciliation processes
- Batch-oriented architecture