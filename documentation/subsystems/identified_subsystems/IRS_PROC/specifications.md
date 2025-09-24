# Subsystem: IRS_PROC - Incomplete Records System

## Executive Summary

**Purpose**: The IRS_PROC subsystem provides simplified bookkeeping functionality for small businesses that don't require full double-entry accounting. It automatically generates balanced journal entries from single-sided inputs like bank transactions and provides basic financial reporting.

**Business Value**: Enables small businesses to maintain compliant books with minimal accounting knowledge. Reduces complexity and training requirements while ensuring accurate financial records for tax and regulatory purposes.

**Key Users**:
- Small business owners
- Bookkeepers without formal accounting training
- Tax preparers needing organized records

**Criticality**: MEDIUM - Serves specific user segment, not integrated with main operations

## Functional Capabilities

### Core Functions

1. **Bank Transaction Import**
   - Description: Import and categorize bank statement transactions
   - Business Rules: Match to predefined categories, identify transfers
   - Triggers: Bank file upload or manual entry
   - Outcomes: Categorized transactions ready for posting

2. **Automatic Double-Entry Creation**
   - Description: Generate balanced journal entries from single-sided inputs
   - Business Rules: Every transaction gets balancing entry to retained earnings
   - Triggers: Transaction categorization completion
   - Outcomes: Balanced entries ready for GL posting

3. **Bank Reconciliation**
   - Description: Match book transactions to bank statement
   - Business Rules: Identify timing differences, uncleared items
   - Triggers: Month-end or statement receipt
   - Outcomes: Reconciliation report, adjusted book balance

4. **VAT/Tax Calculation**
   - Description: Extract tax amounts from gross transactions
   - Business Rules: Apply tax rates by category, handle exemptions
   - Triggers: Transaction entry with tax flag
   - Outcomes: Tax liability tracking, VAT returns

5. **Simplified Reporting**
   - Description: Generate basic financial reports for small business
   - Business Rules: Cash basis reporting, simplified categories
   - Triggers: Period-end or on-demand
   - Outcomes: P&L, balance sheet, tax summary

### Business Processes Supported

- **Daily Cash Management**: Track money in/out
- **Monthly Bookkeeping**: Categorize and reconcile transactions  
- **Tax Preparation**: Organize records for tax filing
- **Basic Financial Analysis**: Understand profitability and cash position

## Data Domain

### Owned Entities

**IRS Transactions**
- Key Attributes: Transaction ID, date, amount, category, bank reference
- Business Identifiers: Bank reference + date
- Lifecycle: Imported/entered → Categorized → Posted → Archived

**IRS Categories** 
- Key Attributes: Category code, description, tax treatment, GL mapping
- Business Identifiers: Category code
- Lifecycle: Setup at implementation, occasionally updated

**Bank Reconciliation**
- Key Attributes: Period, bank balance, book balance, reconciling items
- Business Identifiers: Bank account + period
- Lifecycle: Created monthly, retained for audit

**IRS Reports**
- Key Attributes: Report type, period, generated data
- Business Identifiers: Report type + period
- Lifecycle: Generated on-demand, archived annually

### Referenced Entities

**GL Accounts** (from GL_CORE)
- Why needed: Map categories to proper GL accounts
- Access: Read-only for validation

**Company Parameters** (from MDM)
- Why needed: Tax rates, reporting periods
- Access: Read-only

## Interface Contracts

### Inbound Interfaces

| Interface ID | Source | Data Type | Frequency | Business Purpose |
|-------------|--------|-----------|-----------|------------------|
| BANK-IRS-001 | Bank files | CSV/OFX | Daily/Weekly | Import transactions |
| MAN-IRS-001 | User entry | Screen input | Real-time | Manual transactions |

### Outbound Interfaces

| Interface ID | Target | Data Type | Frequency | Business Purpose |
|-------------|--------|-----------|-----------|------------------|
| IRS-GL-001 | GL_CORE | Journal file | Weekly | Post to General Ledger |
| IRS-RPT-001 | User | PDF/Excel | On-demand | Financial reports |
| IRS-TAX-001 | Tax authority | XML/CSV | Quarterly | VAT returns |

### Internal APIs/Services

**Import Bank File**: [File Data] → [Transaction List]
- Purpose: Parse and import bank transactions
- Validation: File format, duplicate check
- Error Handling: Skip invalid records, report issues

**Categorize Transaction**: [Transaction, Rules] → [Category]
- Purpose: Auto-assign categories based on rules
- Validation: Category exists and is active
- Error Handling: Default to "uncategorized"

**Generate Entries**: [Transactions] → [Journal Entries]
- Purpose: Create double-entry bookkeeping records
- Validation: Ensure balance, valid accounts
- Error Handling: Flag for manual review

## Business Rules Engine

### Validation Rules

- **VR001**: Bank account must be registered
- **VR002**: Transaction date within allowed periods
- **VR003**: Category must map to valid GL account
- **VR004**: No duplicate bank references
- **VR005**: Amounts must be non-zero

### Calculation Rules

- **CR001**: VAT amount = Gross amount × Tax rate ÷ (100 + Tax rate)
- **CR002**: Net amount = Gross amount - VAT amount
- **CR003**: Running balance = Previous balance + Debits - Credits

### Workflow Rules

- **WF001**: Uncategorized transactions block period close
- **WF002**: Reconciliation required before GL posting
- **WF003**: Tax returns require period close first

## Operational Characteristics

### Processing Patterns

**Batch Processing**:
- Weekly GL posting (Monday 06:00)
- Monthly reconciliation (1st of month)
- Quarterly tax returns (quarter-end + 10 days)

**Real-time Processing**:
- Transaction import
- Category assignment
- Report generation

**Peak Periods**:
- Month-end: Reconciliation activities
- Quarter-end: Tax return preparation
- Year-end: Annual report generation

### Data Volumes

- Transaction Volume: 100-500 transactions/month per client
- Data Growth Rate: 10% annually
- Retention Requirements: 6 years for tax compliance

## Dependencies

### Upstream Dependencies
- Bank systems: Transaction feeds
- User input: Manual transactions

### Downstream Dependencies  
- GL_CORE: Receives journal postings
- Tax systems: Receives tax returns

### External Dependencies
- Bank file formats: Industry standards
- Tax requirements: Regulatory compliance

## Quality Attributes

### Performance Requirements
- Response Time: <1 second for queries
- Throughput: Process 1000 transactions in 5 minutes
- Batch Windows: 1-hour weekly posting window

### Reliability Requirements
- Availability: 99% during business hours
- Recovery Time: <2 hours
- Recovery Point: Daily backup

### Compliance Requirements
- Tax regulations: Local tax authority rules
- Accounting standards: Simplified GAAP
- Data retention: 6-year minimum

## Evolution Potential

### Enhancement Opportunities
- Machine learning for categorization
- Mobile app for receipt capture
- Real-time bank feeds
- Automated tax filing

### Modernization Candidates
- Cloud-based architecture
- API-first design
- Multi-tenant SaaS model
- Integrated payment processing

### Known Limitations
- Single currency only
- Cash basis accounting only
- Limited reporting flexibility
- No multi-entity support