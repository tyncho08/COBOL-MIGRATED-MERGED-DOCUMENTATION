# Subsystem: AR_MGMT - Accounts Receivable Management

## Executive Summary

**Purpose**: The AR_MGMT subsystem manages all aspects of customer relationships and revenue collection, from initial customer setup through order processing, invoicing, cash application, and credit management. It is the primary revenue-generating engine of the ACAS system.

**Business Value**: Enables efficient order-to-cash cycles, maintains customer relationships, ensures timely revenue recognition, manages credit risk, and provides critical cash flow management for the organization.

**Key Users**:
- Sales team (order entry, customer inquiries)
- Credit department (credit management)
- Collections team (payment follow-up)
- Customer service (order status, issues)
- Finance team (revenue reporting)

**Criticality**: HIGH - Direct impact on revenue and cash flow

## Functional Capabilities

### Core Functions

1. **Customer Master Management**
   - Description: Maintain comprehensive customer information including demographics, credit, and preferences
   - Business Rules: Unique customer ID with check digit, credit limit validation, status management
   - Triggers: New customer setup, changes to existing customers
   - Outcomes: Validated customer records ready for transactions

2. **Order Processing**
   - Description: Capture and validate customer orders with real-time inventory checking
   - Business Rules: Credit checking, stock availability, pricing validation, minimum quantities
   - Triggers: Customer order via phone, EDI, or manual entry
   - Outcomes: Confirmed orders ready for fulfillment

3. **Shipping and Delivery**
   - Description: Manage order fulfillment from pick list generation to delivery confirmation
   - Business Rules: Shipping rules, carrier selection, address validation
   - Triggers: Order confirmation, stock availability
   - Outcomes: Shipped orders with tracking information

4. **Invoice Generation**
   - Description: Create customer invoices with appropriate pricing, discounts, and taxes
   - Business Rules: Pricing hierarchies, discount calculations, tax determination
   - Triggers: Shipment confirmation or service completion
   - Outcomes: Posted invoices in AR and GL

5. **Cash Application**
   - Description: Apply customer payments to open invoices using automated and manual matching
   - Business Rules: Payment matching logic, discount terms, partial payment handling
   - Triggers: Bank deposits, check receipts, electronic payments
   - Outcomes: Updated customer balances, cleared invoices

6. **Credit Management**
   - Description: Monitor and control customer credit exposure
   - Business Rules: Credit limit checking, payment history analysis, hold/release logic
   - Triggers: Order entry, payment delays, credit reviews
   - Outcomes: Credit decisions, customer holds/releases

7. **Collections Processing**
   - Description: Manage overdue accounts through systematic follow-up
   - Business Rules: Aging buckets, collection letters, interest charges
   - Triggers: Invoice due dates, aging thresholds
   - Outcomes: Collection activities, payment promises

8. **Statement Generation**
   - Description: Produce periodic customer account statements
   - Business Rules: Statement cycles, minimum balances, format preferences
   - Triggers: Month-end, on-demand
   - Outcomes: Customer statements via print/email

### Business Processes Supported

- **Order-to-Cash**: Complete revenue cycle from order to payment
- **Credit-to-Collection**: Credit granting through collection activities
- **Customer Onboarding**: New customer setup and approval
- **Revenue Recognition**: Proper timing of revenue posting
- **Cash Management**: Daily cash application and forecasting
- **Customer Service**: Order status, account inquiries, issue resolution

## Data Domain

### Owned Entities

**Customer Master (SLMASTER)**
- Key Attributes: Customer number, name, addresses, credit limit, terms, status
- Business Identifiers: Customer number (6 digits + check digit)
- Lifecycle: Prospect → Active → Hold → Inactive → Archived

**Sales Orders (SLORDER)**
- Key Attributes: Order number, customer, order date, items, status
- Business Identifiers: Order number (sequential)
- Lifecycle: Entered → Confirmed → Picked → Shipped → Invoiced

**AR Open Items (SLOPEN)**
- Key Attributes: Invoice number, customer, amount, due date, balance
- Business Identifiers: Invoice number (sequential)
- Lifecycle: Created → Outstanding → Partial → Paid → Closed

**Cash Receipts (SLCASH)**
- Key Attributes: Receipt number, customer, amount, payment method
- Business Identifiers: Receipt number, bank reference
- Lifecycle: Received → Applied → Posted → Reconciled

**Customer Transactions (SLTRANS)**
- Key Attributes: Transaction type, date, amount, reference
- Business Identifiers: Transaction ID
- Lifecycle: Created → Posted → Archived

**Credit History (SLCREDIT)**
- Key Attributes: Customer, date, credit decision, amount
- Business Identifiers: Customer + date
- Lifecycle: Recorded → Active → Historical

### Referenced Entities

**Product Master** (from MDM)
- Why needed: Product information for orders
- Access: Read-only

**Inventory Levels** (from INV_CTRL)
- Why needed: Stock availability checking
- Access: Read and allocate

**GL Accounts** (from MDM)
- Why needed: Revenue posting accounts
- Access: Read-only

**Tax Codes** (from MDM)
- Why needed: Tax calculations
- Access: Read-only

## Interface Contracts

### Inbound Interfaces

| Interface ID | Source | Data Type | Frequency | Business Purpose |
|-------------|--------|-----------|-----------|------------------|
| EDI-AR-001 | Customers | EDI 850 | Real-time | Electronic orders |
| BANK-AR-001 | Banks | BAI2/MT940 | Daily | Payment files |
| WEB-AR-001 | Web portal | API/JSON | Real-time | Online orders |
| MAN-AR-001 | Users | Screen entry | Real-time | Manual orders |

### Outbound Interfaces

| Interface ID | Target | Data Type | Frequency | Business Purpose |
|-------------|--------|-----------|-----------|------------------|
| AR-GL-001 | GL_CORE | Journal batch | Daily | Revenue/cash posting |
| AR-INV-001 | INV_CTRL | Allocation request | Real-time | Stock reservation |
| AR-INV-002 | INV_CTRL | Shipment confirm | Real-time | Stock relief |
| AR-RPT-001 | RPT_ENGINE | Data extract | On-demand | AR reports |
| AR-CUST-001 | Customers | Invoice/Statement | Daily/Monthly | Customer documents |

### Internal APIs/Services

**Check Credit**: [Customer, Amount] → [Approved/Denied, Available]
- Purpose: Validate customer credit for orders
- Validation: Active customer, within limit
- Error Handling: Return specific denial reasons

**Calculate Price**: [Customer, Item, Quantity] → [Unit Price, Extended]
- Purpose: Determine customer-specific pricing
- Validation: Valid item, quantity breaks
- Error Handling: Default to list price

**Apply Payment**: [Payment Details] → [Application Results]
- Purpose: Apply payment to open invoices
- Validation: Valid payment, open items exist
- Error Handling: Suspend for manual review

## Business Rules Engine

### Validation Rules

- **VR001**: Customer must be active to create orders
- **VR002**: Order value cannot exceed available credit
- **VR003**: Payment cannot exceed invoice balance
- **VR004**: Shipping address must be validated
- **VR005**: Tax exemption requires certificate on file

### Calculation Rules

- **CR001**: Available Credit = Credit Limit - AR Balance - Open Orders
- **CR002**: Due Date = Invoice Date + Payment Terms Days
- **CR003**: Discount Amount = Invoice × Discount % if paid by discount date
- **CR004**: Late Fee = Overdue Balance × Monthly Rate × Months Late
- **CR005**: Commission = Net Sales × Commission Rate by territory

### Workflow Rules

- **WF001**: Orders over credit limit require approval
- **WF002**: Credits/returns require authorization
- **WF003**: Write-offs follow approval matrix
- **WF004**: Collection activities escalate by aging

## Operational Characteristics

### Processing Patterns

**Batch Processing**:
- Nightly invoice generation (17:00)
- Daily statement run (06:00)
- Weekly aging recalculation (Sunday)
- Monthly commission calculation (1st)

**Real-time Processing**:
- Order entry and validation
- Credit checking
- Payment application
- Customer inquiries

**Peak Periods**:
- Order entry: 10:00-14:00 weekdays
- Month-end: Last 5 days
- Collections: 8:00-10:00 daily

### Data Volumes

- Transaction Volume: 500-1000 orders/day, 2000-3000 invoices/month
- Data Growth Rate: 15% annually
- Retention Requirements: 7 years active, permanent archive

## Dependencies

### Upstream Dependencies
- MDM: Customer and product master data
- INV_CTRL: Stock availability
- INTEGRATION: EDI orders, bank files

### Downstream Dependencies
- GL_CORE: Revenue and cash posting
- RPT_ENGINE: AR reporting
- BATCH_FW: Nightly processing

### External Dependencies
- Banks: Payment files, lockbox
- Credit agencies: Credit reports
- Customers: Orders and payments

## Quality Attributes

### Performance Requirements
- Response Time: <2 seconds for order entry
- Throughput: 100 orders/hour
- Batch Windows: 4-hour nightly window

### Reliability Requirements
- Availability: 99.5% during business hours
- Recovery Time: <2 hours
- Recovery Point: Last completed transaction

### Compliance Requirements
- Revenue recognition: ASC 606 compliance
- Credit reporting: Fair Credit Reporting Act
- Data privacy: Customer data protection

## Evolution Potential

### Enhancement Opportunities
- Online customer self-service portal
- Mobile apps for sales team
- AI-powered credit decisions
- Automated payment matching
- Real-time revenue analytics

### Modernization Candidates
- Move from batch to real-time invoicing
- API-first architecture for omnichannel
- Cloud-based for scalability
- Machine learning for collections

### Known Limitations
- Batch-oriented invoice generation
- Limited payment methods
- Manual credit reviews
- No customer portal
- Single currency only