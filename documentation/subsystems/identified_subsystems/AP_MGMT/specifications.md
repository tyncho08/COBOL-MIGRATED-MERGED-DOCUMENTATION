# Subsystem: AP_MGMT - Accounts Payable Management

## Executive Summary

**Purpose**: The AP_MGMT subsystem manages all aspects of supplier relationships and expense management, from supplier setup through purchase order processing, goods receipt, invoice management, and payment processing. It ensures accurate tracking of organizational liabilities and timely payment to vendors.

**Business Value**: Enables efficient procure-to-pay cycles, maintains supplier relationships, ensures accurate expense recognition, manages cash disbursements, and provides critical spend analysis for cost control and negotiation leverage.

**Key Users**:
- Purchasing department (PO creation, supplier management)
- Warehouse team (goods receipt)
- Accounts payable clerks (invoice processing)
- Treasury team (payment runs)
- Management (spend analysis, approval)

**Criticality**: HIGH - Direct impact on supplier relationships and cash management

## Functional Capabilities

### Core Functions

1. **Supplier Master Management**
   - Description: Maintain comprehensive supplier information including demographics, payment terms, and tax details
   - Business Rules: Unique supplier ID with check digit validation, bank account verification, tax ID validation
   - Triggers: New supplier onboarding, changes to existing suppliers
   - Outcomes: Validated supplier records ready for transactions

2. **Purchase Order Processing**
   - Description: Create and manage purchase orders with approval workflow
   - Business Rules: Authorization limits, budget checking, preferred supplier validation
   - Triggers: Purchase requisition, inventory reorder points
   - Outcomes: Approved purchase orders sent to suppliers

3. **Goods Receipt Processing**
   - Description: Record receipt of goods and services with three-way matching
   - Business Rules: Quantity tolerance checking, quality inspection flags, partial receipt handling
   - Triggers: Physical delivery, service completion
   - Outcomes: Updated inventory, accrued liability

4. **Invoice Processing**
   - Description: Process supplier invoices with automated matching and exception handling
   - Business Rules: Three-way match (PO-Receipt-Invoice), price tolerance, tax validation
   - Triggers: Invoice receipt via mail, EDI, or manual entry
   - Outcomes: Posted payables in AP and GL

5. **Payment Selection and Processing**
   - Description: Select invoices for payment based on terms and cash availability
   - Business Rules: Payment terms calculation, early payment discounts, payment priority
   - Triggers: Payment run schedule, cash position
   - Outcomes: Payment batch ready for execution

6. **Check/EFT Generation**
   - Description: Generate physical checks or electronic payment files
   - Business Rules: Bank account validation, signature rules, positive pay file generation
   - Triggers: Approved payment batch
   - Outcomes: Payments issued, bank files transmitted

7. **Expense Distribution**
   - Description: Allocate expenses to appropriate GL accounts and cost centers
   - Business Rules: Account validation, project/department allocation, tax handling
   - Triggers: Invoice approval, receipt posting
   - Outcomes: Accurate expense recording in GL

8. **Supplier Statement Reconciliation**
   - Description: Reconcile supplier statements with AP records
   - Business Rules: Matching logic, dispute handling, aging analysis
   - Triggers: Statement receipt, month-end
   - Outcomes: Reconciled balances, dispute resolution

### Business Processes Supported

- **Procure-to-Pay**: Complete expense cycle from requisition to payment
- **Supplier Onboarding**: New vendor setup and approval
- **Expense Management**: Budget control and authorization
- **Cash Disbursement**: Planned payment execution
- **Month-End Accruals**: Goods received not invoiced
- **Vendor Performance**: On-time delivery and quality metrics

## Data Domain

### Owned Entities

**Supplier Master (PLMASTER)**
- Key Attributes: Supplier number, name, addresses, payment terms, bank details, tax ID
- Business Identifiers: Supplier number (6 digits + check digit)
- Lifecycle: Prospect → Active → Hold → Inactive → Archived

**Purchase Orders (PLORDER)**
- Key Attributes: PO number, supplier, order date, line items, status, approver
- Business Identifiers: PO number (sequential)
- Lifecycle: Created → Approved → Sent → Partial → Complete → Closed

**Goods Receipts (PLRECEIPT)**
- Key Attributes: Receipt number, PO reference, receipt date, quantities, location
- Business Identifiers: Receipt number (sequential)
- Lifecycle: Created → Posted → Matched → Archived

**AP Open Items (PLOPEN)**
- Key Attributes: Invoice number, supplier, amount, due date, balance, PO reference
- Business Identifiers: Invoice number (supplier + internal sequence)
- Lifecycle: Created → Outstanding → Scheduled → Paid → Closed

**Payment History (PLPAYMENT)**
- Key Attributes: Payment number, payment date, amount, method, bank reference
- Business Identifiers: Payment number, check number
- Lifecycle: Scheduled → Generated → Cleared → Reconciled

### Referenced Entities

**General Ledger Accounts** (from GL_CORE)
- Why needed: Expense posting and distribution
- Access: Read for validation, write for posting

**Stock Master** (from INV_CTRL)
- Why needed: Item validation, cost comparison
- Access: Read for validation, update for receipts

**Bank Master** (from MDM)
- Why needed: Payment processing, bank selection
- Access: Read-only

**Tax Codes** (from MDM)
- Why needed: Tax calculation and reporting
- Access: Read-only

## Interface Contracts

### Inbound Interfaces

| Interface ID | Source | Data Type | Frequency | Business Purpose |
|-------------|--------|-----------|-----------|------------------|
| AP_PO_CREATE | Inventory System | Batch File | Daily | Auto-generate POs for reorders |
| AP_INVOICE_EDI | EDI Gateway | EDI 810 | Real-time | Electronic invoice receipt |
| AP_STMT_IMPORT | Bank Portal | CSV File | Monthly | Supplier statement import |
| AP_EXP_IMPORT | Expense System | Batch File | Daily | Employee expense reimbursements |

### Outbound Interfaces

| Interface ID | Target | Data Type | Frequency | Business Purpose |
|-------------|--------|-----------|-----------|------------------|
| AP_GL_POST | GL_CORE | Journal Batch | Real-time | Post payable transactions |
| AP_INV_UPDATE | INV_CTRL | Receipt File | Real-time | Update inventory receipts |
| AP_BANK_PAY | Bank System | Payment File | Daily | Execute payments |
| AP_1099_REPORT | Tax Authority | Tax File | Annual | 1099 vendor reporting |

### Internal APIs/Services

- **SupplierValidation**: [SupplierID] → [Status, Terms, Details]
  - Purpose: Validate supplier and retrieve payment terms
  - Validation: Active status, valid payment method
  - Error Handling: Invalid supplier, on hold status

- **InvoiceMatching**: [Invoice, PO, Receipt] → [MatchStatus, Variances]
  - Purpose: Perform three-way match validation
  - Validation: Quantity/price tolerances, PO authorization
  - Error Handling: Outside tolerance, missing documents

- **PaymentSelection**: [SelectionCriteria] → [PaymentBatch]
  - Purpose: Select invoices for payment
  - Validation: Due date, available discount, cash position
  - Error Handling: Insufficient funds, blocked suppliers

## Business Rules Engine

### Validation Rules
- **RULE_AP_001**: Invoice amount must match PO amount within 5% tolerance
- **RULE_AP_002**: Payment cannot exceed invoice balance
- **RULE_AP_003**: Supplier must have valid tax ID for amounts over $600
- **RULE_AP_004**: PO approval required based on amount tiers

### Calculation Rules
- **CALC_AP_001**: Early payment discount = Invoice amount × discount % if paid within discount days
- **CALC_AP_002**: Late payment fee = Invoice amount × penalty rate × days late / 365
- **CALC_AP_003**: Tax amount = (Taxable amount × tax rate) with rounding to 2 decimals

### Workflow Rules
- **FLOW_AP_001**: PO Approval: <$1000 = Supervisor, <$10000 = Manager, >=$10000 = Director
- **FLOW_AP_002**: Invoice Hold: Price variance >5% requires manual review
- **FLOW_AP_003**: Payment Release: Batch >$50000 requires treasury approval

## Operational Characteristics

### Processing Patterns
- **Batch Processing**: 
  - Nightly payment selection (6:00 PM)
  - Weekly check runs (Wednesday 10:00 AM)
  - Monthly statement reconciliation
- **Real-time Processing**: 
  - PO entry and approval
  - Invoice receipt and matching
  - Inquiry functions
- **Peak Periods**: 
  - Month-end (25th-5th)
  - Quarter-end processing
  - Year-end 1099 processing

### Data Volumes
- Transaction Volume: 
  - 500-1000 invoices/month
  - 200-400 POs/month
  - 100-200 payments/week
- Data Growth Rate: 15% annual increase
- Retention Requirements: 
  - Active transactions: 2 years online
  - History: 7 years archived

## Dependencies

### Upstream Dependencies
- **INV_CTRL**: Item master data, reorder requirements
- **MDM**: Supplier standing data, tax codes
- **SEC_AUDIT**: User authorization for approvals

### Downstream Dependencies
- **GL_CORE**: Receives all AP postings
- **INV_CTRL**: Receives goods receipt updates
- **RPT_ENGINE**: Provides data for spend analysis

### External Dependencies
- **Banking System**: Payment file transmission
- **EDI Gateway**: Electronic invoice receipt
- **Tax Authority**: 1099 reporting

## Quality Attributes

### Performance Requirements
- Response Time: 
  - Invoice entry: <3 seconds
  - Payment selection: <5 minutes for 1000 invoices
  - Report generation: <10 minutes
- Throughput: 100 invoices/hour manual entry
- Batch Windows: 6 hours for month-end

### Reliability Requirements
- Availability: 99% during business hours
- Recovery Time: 2 hours RTO
- Recovery Point: 1 hour RPO

### Compliance Requirements
- **SOX Compliance**: Segregation of duties, approval trails
- **Tax Compliance**: 1099 reporting, sales tax handling
- **Payment Card Industry**: Secure payment data handling

## Evolution Potential

### Enhancement Opportunities
- **Electronic Invoicing**: Full EDI/XML invoice automation
- **Dynamic Discounting**: Optimize payment timing for discounts
- **Mobile Approvals**: PO/invoice approval via mobile devices
- **AI Matching**: Machine learning for invoice matching exceptions

### Modernization Candidates
- **Database Migration**: Move from ISAM to relational database
- **API Development**: REST APIs for supplier portal
- **Workflow Engine**: Replace hard-coded approval logic
- **Analytics Platform**: Real-time spend analytics

### Known Limitations
- **Manual Matching**: Limited automation for complex invoices
- **Payment Methods**: Check and basic EFT only
- **Multi-Currency**: Limited foreign currency support
- **Approval Routing**: Hard-coded approval hierarchies