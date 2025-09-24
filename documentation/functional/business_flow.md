# ACAS Business Process Flows

## Overview
This document provides end-to-end process documentation for all major business workflows in the ACAS system, including decision trees, accounting period processing, and year-end procedures.

## Core Business Processes

### 1. Order-to-Cash Process (Sales Cycle)

#### 1.1 Customer Order Entry

**Entry Point**: Sales Menu → Order Entry (SL020)

**Process Flow**:
1. **Customer Selection**
   - Enter customer number or search by name
   - System validates customer exists and is active
   - Check credit status (Hold/Stop flags)
   - Display customer details and credit information

2. **Credit Check**
   ```
   IF customer-status = 'H' (Hold)
      Display warning
      Require supervisor override
   ELSE IF customer-balance + order-value > credit-limit
      IF strict-credit-control = 'Y'
         Reject order
      ELSE
         Warning only - allow override
   ```

3. **Order Header Entry**
   - Order date (defaults to system date)
   - Required date
   - Delivery address selection
   - Special instructions
   - Terms override (if authorized)

4. **Order Line Entry**
   - Stock item selection (by code or description)
   - Availability check:
     ```
     Available = qty-on-hand - qty-allocated + qty-on-order
     IF requested-qty > available
        IF partial-ship-allowed = 'Y'
           Allocate available qty
           Create back-order for balance
        ELSE
           Reject line or reduce quantity
     ```
   - Pricing:
     ```
     Base price = customer-price-level
     Apply customer discount
     Apply promotional discount
     Apply quantity breaks
     Calculate tax based on customer tax status
     ```

5. **Order Confirmation**
   - Review total order value
   - Print order confirmation
   - Update stock allocation
   - Create picking list

#### 1.2 Order Fulfillment

**Entry Point**: Sales Menu → Picking/Shipping (SL030)

**Process Flow**:
1. **Picking List Generation**
   - Select orders by date range, customer, or priority
   - Print by warehouse location sequence
   - Mark items as picked

2. **Shipping Confirmation**
   - Confirm quantities shipped
   - Enter shipping details (carrier, tracking)
   - Handle partial shipments
   - Update stock quantities

#### 1.3 Invoice Generation

**Entry Point**: Sales Menu → Invoice Generation (SL050/SL810)

**Process Flow**:
1. **Invoice Selection**
   - Select shipped orders for invoicing
   - Group by customer if required
   - Apply cutoff date/time

2. **Invoice Creation**
   - Generate unique invoice number
   - Calculate invoice totals
   - Apply payment terms
   - Create GL postings:
     ```
     DR: Customer Control Account    (Invoice Total)
     CR: Sales Account by Analysis   (Goods Value)
     CR: Tax Control Account         (Tax Amount)
     CR: Discount Given Account      (Discount Amount)
     ```

3. **Invoice Printing**
   - Format invoice document
   - Include customer PO reference
   - Show payment terms
   - Update customer balance

#### 1.4 Cash Receipt Processing

**Entry Point**: Sales Menu → Cash Receipts (SL060)

**Process Flow**:
1. **Payment Entry**
   - Enter customer number
   - Payment amount and date
   - Payment method (check, wire, cash)
   - Bank account for deposit

2. **Payment Allocation**
   - Display open invoices oldest first
   - Auto-allocate option:
     ```
     WHILE payment-amount > 0 AND more-invoices
        IF payment-amount >= invoice-balance
           Allocate full invoice
           payment-amount = payment-amount - invoice-balance
        ELSE
           Allocate partial payment
           payment-amount = 0
     ```
   - Manual allocation for specific invoices
   - Handle discounts for early payment

3. **GL Posting**
   ```
   DR: Bank Account                (Payment Amount)
   DR: Discount Allowed Account    (Discount Taken)
   CR: Customer Control Account    (Total Allocated)
   ```

4. **Update Records**
   - Mark invoices as paid/part-paid
   - Update customer balance
   - Record payment history

### 2. Procure-to-Pay Process (Purchase Cycle)

#### 2.1 Purchase Requisition

**Entry Point**: Purchase Menu → Requisitions (PL015)

**Process Flow**:
1. **Requisition Entry**
   - Department/requester
   - Item requirements
   - Required date
   - Justification/notes

2. **Approval Routing**
   ```
   IF requisition-value <= dept-limit
      Auto-approve
   ELSE IF requisition-value <= manager-limit
      Route to department manager
   ELSE
      Route to senior management
   ```

#### 2.2 Purchase Order Generation

**Entry Point**: Purchase Menu → Purchase Orders (PL020)

**Process Flow**:
1. **Supplier Selection**
   - Primary supplier from item master
   - Or select alternative supplier
   - Check supplier status

2. **PO Header**
   - Generate PO number
   - Delivery address
   - Payment terms
   - Special terms

3. **PO Lines**
   - Item selection
   - Quantity and unit price
   - Delivery date by line
   - GL account distribution

4. **PO Approval**
   ```
   IF po-value <= buyer-limit
      Release immediately
   ELSE
      Route for approval
      Send to supplier only after approval
   ```

#### 2.3 Goods Receipt

**Entry Point**: Purchase Menu → Goods Receipt (PL030)

**Process Flow**:
1. **Receipt Entry**
   - Select PO or enter manually
   - Receipt date and reference
   - Quantity received by line

2. **Quality Check**
   ```
   IF inspection-required = 'Y'
      Move to inspection location
      Set status = 'Awaiting QC'
   ELSE
      Move to stock location
      Available for use
   ```

3. **Stock Update**
   - Increase qty-on-hand
   - Reduce qty-on-order
   - Update last receipt date
   - Calculate new average cost:
     ```
     New avg cost = ((old-qty * old-cost) + (receipt-qty * receipt-cost)) 
                    / (old-qty + receipt-qty)
     ```

#### 2.4 Invoice Processing

**Entry Point**: Purchase Menu → Invoice Entry (PL040)

**Process Flow**:
1. **Invoice Matching**
   - Three-way match:
     ```
     PO quantity/price
     Receipt quantity
     Invoice quantity/price
     
     IF all match within tolerance
        Auto-approve for payment
     ELSE
        Route to buyer for resolution
     ```

2. **Invoice Entry**
   - Supplier invoice number
   - Invoice date
   - Line items with GL distribution
   - Tax calculation/verification

3. **GL Posting**
   ```
   DR: Expense/Asset Accounts     (Net Amount)
   DR: Tax Reclaimable Account    (Tax Amount)
   CR: Supplier Control Account   (Invoice Total)
   ```

#### 2.5 Payment Processing

**Entry Point**: Purchase Menu → Payment Run (PL055)

**Process Flow**:
1. **Payment Selection**
   - Due date selection
   - Take discount if beneficial:
     ```
     IF discount-amount > (payment-amount * daily-interest-rate * days-early)
        Take discount and pay early
     ```
   - Respect payment holds

2. **Payment Generation**
   - Group by supplier
   - Generate check/EFT
   - Print remittance advice

3. **GL Posting**
   ```
   DR: Supplier Control Account   (Payment Amount)
   CR: Bank Account               (Payment Amount)
   CR: Discount Received Account  (Discount Taken)
   ```

### 3. Inventory Management Process

#### 3.1 Stock Receipt

**Sources**: 
- Purchase order receipts
- Stock transfers in
- Manufacturing completion
- Customer returns

**Process Flow**:
1. **Receipt Validation**
   - Valid item code
   - Valid location
   - Quantity > 0

2. **Costing**
   ```
   IF costing-method = 'FIFO'
      Create new cost layer
   ELSE IF costing-method = 'Average'
      Calculate new weighted average
   ELSE IF costing-method = 'Standard'
      Use standard cost, variance to GL
   ```

3. **Update Balances**
   - Increase location quantity
   - Update master quantity
   - Post to GL if required

#### 3.2 Stock Issue

**Sources**:
- Sales order shipment
- Manufacturing consumption
- Stock transfers out
- Adjustments

**Process Flow**:
1. **Availability Check**
   ```
   IF requested-qty > available-qty
      IF allow-negative = 'N'
         Reject transaction
      ELSE
         Warning only
   ```

2. **Cost Selection**
   ```
   IF costing-method = 'FIFO'
      Use oldest cost layers first
   ELSE IF costing-method = 'Average'
      Use current average cost
   ```

3. **GL Posting**
   ```
   DR: Cost of Sales Account      (Qty * Unit Cost)
   CR: Stock Asset Account        (Qty * Unit Cost)
   ```

#### 3.3 Stock Valuation

**Entry Point**: Stock Menu → Valuation Report (ST030)

**Process Flow**:
1. **Valuation Method**
   - By item: FIFO, Average, Standard, Latest
   - By location or consolidated
   - Include/exclude obsolete items

2. **Calculations**
   ```
   FOR each item
      IF qty-on-hand > 0
         IF method = 'FIFO'
            Value = sum of cost layers
         ELSE IF method = 'Average'
            Value = qty * average-cost
         ELSE IF method = 'Latest'
            Value = qty * last-cost
      
      Apply lower of cost/market if required
      Apply obsolescence reserve if flagged
   ```

3. **Report Output**
   - Detail by item
   - Summary by category
   - Variance analysis
   - GL reconciliation

### 4. Month-End Processing

#### 4.1 Sales Ledger Month-End

**Entry Point**: Sales Menu → Month-End (SL900)

**Prerequisites**:
- All invoices posted
- All cash receipts entered
- Customer statements printed

**Process Flow**:
1. **Validation Checks**
   ```
   Check no unposted batches
   Check GL interface complete
   Verify control account balance
   ```

2. **Aging Update**
   - Age all open invoices
   - Update customer statistics
   - Flag overdue accounts

3. **Interest Charges** (if applicable)
   ```
   FOR each customer with overdue balance
      IF charge-interest = 'Y'
         Calculate interest on overdue
         Create interest invoice
   ```

4. **Period Roll**
   - Update period sales figures
   - Reset period counters
   - Archive closed transactions

#### 4.2 General Ledger Month-End

**Entry Point**: GL Menu → Period Close (GL030)

**Process Flow**:
1. **Pre-Close Validation**
   ```
   Verify all modules posted
   Check batch totals balance
   Review suspense accounts
   Ensure no future-dated entries
   ```

2. **Closing Entries**
   - Post accruals/prepayments
   - Post depreciation
   - Allocate costs/overhead
   - Post recurring journals

3. **Trial Balance**
   - Generate preliminary TB
   - Review for errors
   - Make adjustments
   - Generate final TB

4. **Financial Statements**
   - Profit & Loss Statement
   - Balance Sheet
   - Department analysis
   - Budget variance reports

5. **Period Roll**
   ```
   Current balances → Prior period
   Reset current period activity
   Increment period number
   Open new period for posting
   ```

### 5. Year-End Processing

#### 5.1 Year-End Preparation

**Process Flow**:
1. **Cutoff Procedures**
   - Stop all transaction entry
   - Complete all shipments
   - Ensure all receipts entered
   - Finalize payroll

2. **Reconciliations**
   - Bank reconciliations
   - Supplier statement reconciliation
   - Stock count and adjustment
   - Inter-company balances

3. **Year-End Adjustments**
   - Bad debt provision
   - Stock obsolescence
   - Depreciation
   - Accruals/prepayments
   - Tax provisions

#### 5.2 Year-End Close

**Entry Point**: GL Menu → Year-End Close (GL910)

**Process Flow**:
1. **Final Validation**
   ```
   Ensure all 12/13 periods closed
   Verify P&L accounts net to zero
   Review retained earnings calculation
   ```

2. **Close P&L Accounts**
   ```
   FOR each P&L account
      IF account-type IN ('I', 'E')
         Transfer balance to retained earnings
         Zero account balance
   ```

3. **Roll Forward Balances**
   ```
   FOR each Balance Sheet account
      Prior year balance = Current balance
      Current balance = Current balance (carried forward)
      YTD activity = 0
   ```

4. **Archive Data**
   - Copy transactions to archive
   - Create audit file
   - Backup before deletion
   - Purge closed transactions

5. **New Year Setup**
   - Reset document numbers (optional)
   - Update year in system file
   - Create new period structure
   - Initialize budget figures

### 6. Special Processes

#### 6.1 Bank Reconciliation

**Entry Point**: Cash Menu → Bank Reconciliation

**Process Flow**:
1. **Import Statement** (if available)
   - Electronic format
   - Or manual entry

2. **Auto-Matching**
   ```
   FOR each bank transaction
      Find GL entry by:
      - Amount and date (exact)
      - Amount (within date range)
      - Reference number
   ```

3. **Manual Matching**
   - Display unmatched items
   - Manual selection
   - Adjust for timing differences

4. **Reconciliation Report**
   - Book balance
   - Add: Deposits in transit
   - Less: Outstanding checks
   - Should equal: Bank balance

#### 6.2 Stock Take Process

**Entry Point**: Stock Menu → Physical Count (ST070)

**Process Flow**:
1. **Freeze Inventory**
   - Stop all movements
   - Or use cycle counting

2. **Count Entry**
   - By location/item
   - Enter counted quantity
   - Multiple counts for verification

3. **Variance Analysis**
   ```
   Variance = Counted qty - System qty
   Variance % = (Variance / System qty) * 100
   
   IF abs(Variance %) > tolerance
      Require recount or investigation
   ```

4. **Adjustment Posting**
   ```
   IF Variance > 0 (Gain)
      DR: Stock Account
      CR: Stock Adjustment Gain
   ELSE (Loss)
      DR: Stock Adjustment Loss  
      CR: Stock Account
   ```

### 7. Decision Trees

#### 7.1 Credit Control Decision Tree

```
Customer Order Received
│
├─ New Customer?
│  ├─ Yes → Credit Application
│  │         ├─ Approved → Set Credit Limit
│  │         └─ Declined → Cash Only Terms
│  └─ No → Check Account Status
│          ├─ Active → Check Credit
│          ├─ Hold → Supervisor Override Required
│          └─ Closed → Reject Order
│
└─ Credit Check
   ├─ Within Limit → Process Order
   ├─ Over Limit
   │  ├─ Good Payment History → Warning Only
   │  └─ Poor Payment History → 
   │     ├─ Partial Ship to Limit
   │     └─ Require Payment/Deposit
   └─ Cash Customer → Require Prepayment
```

#### 7.2 Inventory Allocation Decision Tree

```
Order Line Entered
│
├─ Check Stock Availability
│  ├─ Sufficient Stock → Allocate Full Quantity
│  ├─ Partial Stock Available
│  │  ├─ Accept Partial Ship? 
│  │  │  ├─ Yes → Allocate Available
│  │  │  │       └─ Create Back Order
│  │  │  └─ No → Check Alternatives
│  │  │          ├─ Substitute Available → Offer Substitute
│  │  │          └─ No Substitute → Reject Line
│  └─ No Stock
│     ├─ On Order?
│     │  ├─ Yes → Promise Date = Expected Receipt + Lead Time
│     │  └─ No → Check Reorder
│     │          ├─ Below Reorder Point → Generate PO
│     │          └─ Discontinued → Advise Customer
│     └─ Drop Ship Available? → Create Direct Ship PO
```

### 8. Compliance and Control Processes

#### 8.1 Audit Trail Maintenance

**Continuous Process**:
- Every transaction logged with:
  - User ID
  - Date/Time stamp
  - Before/after values
  - Source reference
  - Cannot be deleted or modified

#### 8.2 Segregation of Duties

**Built-in Controls**:
1. **Order Entry**: Cannot approve own credit override
2. **Purchasing**: Cannot approve own POs over limit
3. **Payments**: Separate payment selection and check signing
4. **GL Entries**: Journal approval required over threshold
5. **Master File**: Changes logged and reviewed

#### 8.3 Period Controls

**Enforcement Rules**:
```
IF transaction-date < closed-period
   Reject with error
ELSE IF transaction-date > current-period + 1
   Warning - require override
ELSE
   Accept transaction
```

### 9. Integration Points

#### 9.1 Module Integration Flow

```
Sales Order → Stock Allocation → Invoice → GL Posting
                     ↓                          ↑
              Stock Movement ───────────────────┘

Purchase Order → Receipt → Stock Update → GL Posting
                              ↓               ↑
                        Cost Update ──────────┘
```

#### 9.2 Batch Processing Schedule

**Daily**:
- 6:00 AM - Import bank files
- 8:00 AM - Credit check updates
- 5:00 PM - Invoice generation
- 6:00 PM - Pick list generation
- 10:00 PM - Backup

**Weekly**:
- Monday - Aged debt report
- Wednesday - Stock reorder report
- Friday - Sales analysis

**Monthly**:
- 1st - Customer statements
- 5th - Month-end close
- 10th - Financial reports