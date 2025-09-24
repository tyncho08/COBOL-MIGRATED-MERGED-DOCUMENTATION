# ACAS Calculation Engine Documentation

## Overview

This document provides comprehensive documentation of all financial calculations, formulas, and business algorithms implemented in the ACAS system. Each calculation is documented with its business purpose, formula details, and implementation rules.

## Financial Calculations

### 1. Tax Calculations

#### Standard Tax Calculation
**Purpose**: Calculate tax amount on sales and purchases  
**Programs**: sl050, sl810, pl040

```
Base Formula:
    Tax Amount = Taxable Amount × Tax Rate

Where:
    Taxable Amount = Gross Amount - Exempt Items
    Tax Rate = System default or customer/item specific

Implementation:
    IF customer-tax-code = "E" (Exempt)
        tax-amount = 0
    ELSE IF customer-tax-code = "Z" (Zero-rated)
        tax-amount = 0
        CREATE tax-audit-record
    ELSE IF item-tax-code = "E"
        tax-amount = 0
    ELSE
        tax-rate = COALESCE(customer-tax-rate, 
                           item-tax-rate, 
                           system-default-tax-rate)
        tax-amount = ROUNDED(taxable-amount * tax-rate / 100, 2)
    END-IF

Special Cases:
    - Export sales: Always zero-rated with audit trail
    - Mixed baskets: Calculate line by line
    - Rounding: Always round to 2 decimal places
```

#### Reverse Tax Calculation
**Purpose**: Extract tax from tax-inclusive amounts  
**Used in**: Purchase invoice entry when amounts include tax

```
Formula:
    Tax Amount = Tax-Inclusive Amount × (Tax Rate ÷ (100 + Tax Rate))
    Net Amount = Tax-Inclusive Amount - Tax Amount

Example:
    Tax-inclusive = 120.00
    Tax rate = 20%
    Tax amount = 120 × (20 ÷ 120) = 20.00
    Net amount = 100.00
```

### 2. Discount Calculations

#### Trade Discount
**Purpose**: Apply customer-specific discount rates  
**Programs**: sl020, sl050, sl810

```
Discount Hierarchy (applied in order):
1. Line-specific discount (if any)
2. Customer discount rate
3. Product category discount
4. Promotional discount
5. Volume discount

Calculation:
    line-discount = gross-amount * line-discount-percent / 100
    
    IF line-discount-specified
        discount-amount = line-discount
    ELSE
        base-discount = gross-amount * customer-discount-rate / 100
        
        IF promotional-discount-active
            promo-discount = gross-amount * promo-rate / 100
            discount-amount = MAX(base-discount, promo-discount)
        ELSE
            discount-amount = base-discount
        END-IF
    END-IF
    
    net-amount = gross-amount - discount-amount

Maximum Discount Rule:
    IF total-discount-percent > max-allowed-discount
        discount-amount = gross-amount * max-allowed-discount / 100
        REQUIRE supervisor-override
    END-IF
```

#### Settlement Discount
**Purpose**: Early payment discount for purchases  
**Programs**: pl055, pl060

```
Formula:
    Discount = Invoice Amount × Settlement Discount %
    
Eligibility:
    Payment Date ≤ Invoice Date + Discount Terms Days

Example:
    Invoice amount: 1,000.00
    Terms: 2% 10 days net 30
    If paid within 10 days: Discount = 20.00
    Amount to pay: 980.00
```

#### Volume Discount
**Purpose**: Quantity-based pricing tiers  
**Programs**: sl020, sl050

```
Tier Structure:
    Quantity        Discount %
    1-99           0%
    100-499        5%
    500-999        10%
    1000+          15%

Calculation:
    FOR each tier FROM highest TO lowest
        IF order-quantity >= tier-minimum
            discount-percent = tier-discount
            EXIT loop
        END-IF
    END-FOR
    
    discount-amount = (unit-price * quantity) * discount-percent / 100
```

### 3. Pricing Calculations

#### Standard Pricing
**Purpose**: Calculate selling price from cost  
**Programs**: st010, sl170

```
Price Calculation Methods:

1. Markup on Cost:
    Selling Price = Cost × (1 + Markup %)
    
2. Gross Margin:
    Selling Price = Cost ÷ (1 - Margin %)
    
3. Fixed Price:
    Selling Price = Defined Price

Price Level Selection:
    IF customer-price-level = 1
        use sell-price-1
    ELSE IF customer-price-level = 2
        use sell-price-2
    ELSE IF customer-price-level = 3
        use sell-price-3
    ELSE
        use sell-price-1 (default)
    END-IF
```

#### Price Variance
**Purpose**: Compare actual vs standard purchase prices  
**Programs**: pl040, pl095

```
Formula:
    Price Variance = (Actual Price - Standard Price) × Quantity
    Variance % = ((Actual - Standard) ÷ Standard) × 100

Reporting:
    IF ABS(variance-percent) > tolerance-percent
        flag-for-review
        IF variance-amount > approval-limit
            require-approval
        END-IF
    END-IF
```

### 4. Interest Calculations

#### Late Payment Interest
**Purpose**: Calculate interest on overdue accounts  
**Programs**: sl100, sl900

```
Formula:
    Interest = Principal × Rate × Days ÷ Days in Year

Where:
    Principal = Overdue amount
    Rate = Annual interest rate
    Days = Days overdue
    Days in Year = 365 (or 360 for commercial)

Implementation:
    overdue-days = current-date - due-date
    IF overdue-days > grace-period-days
        interest-days = overdue-days - grace-period-days
        interest-amount = ROUNDED(overdue-amount * 
                                 interest-rate / 100 * 
                                 interest-days / 365, 2)
        
        IF interest-amount < minimum-interest-charge
            interest-amount = 0  // Don't charge if below minimum
        END-IF
    END-IF
```

#### Finance Charge
**Purpose**: Monthly finance charges on outstanding balances  
**Programs**: sl900

```
Formula:
    Finance Charge = Average Daily Balance × Monthly Rate

Calculation:
    total-balance-days = 0
    FOR each day in period
        total-balance-days = total-balance-days + daily-balance
    END-FOR
    
    average-daily-balance = total-balance-days / days-in-period
    monthly-rate = annual-rate / 12 / 100
    finance-charge = ROUNDED(average-daily-balance * monthly-rate, 2)
```

### 5. Currency Handling

#### Exchange Rate Application
**Purpose**: Convert foreign currency amounts  
**Programs**: Multiple (if multi-currency enabled)

```
Formulas:
    Base Amount = Foreign Amount × Exchange Rate (multiply rate)
    Base Amount = Foreign Amount ÷ Exchange Rate (divide rate)

Rounding Rules:
    - Calculate in maximum precision
    - Round final base amount to 2 decimals
    - Store both foreign and base amounts

Rate Selection:
    IF transaction-rate-specified
        use transaction-rate
    ELSE IF daily-rate-exists
        use daily-rate-for-date
    ELSE
        use standard-rate
        flag-for-rate-update
    END-IF
```

### 6. Commission Calculations

#### Sales Commission
**Purpose**: Calculate sales representative commissions  
**Programs**: sl140, sl900

```
Commission Structures:

1. Flat Rate:
    Commission = Net Sales × Commission Rate %

2. Tiered Rate:
    Monthly Sales       Rate
    0-10,000           5%
    10,001-25,000      7.5%
    25,001+            10%
    
    Calculate on marginal basis:
    First 10,000 at 5% = 500
    Next 15,000 at 7.5% = 1,125
    Remainder at 10%

3. Product-Based:
    Commission = Σ(Product Sales × Product Commission Rate)

Special Rules:
    - No commission on tax amounts
    - Reduced commission on discounted sales
    - Clawback on returns/credits
```

### 7. Inventory Valuation

#### Average Cost Method
**Purpose**: Calculate weighted average cost  
**Programs**: st020, st030

```
Formula:
    New Average Cost = (Current Value + Receipt Value) ÷ 
                      (Current Qty + Receipt Qty)

Implementation:
    current-value = qty-on-hand * current-avg-cost
    receipt-value = receipt-qty * receipt-cost
    new-total-qty = qty-on-hand + receipt-qty
    
    IF new-total-qty > 0
        new-avg-cost = ROUNDED((current-value + receipt-value) / 
                               new-total-qty, 4)
    ELSE
        new-avg-cost = receipt-cost
    END-IF

Issue Cost:
    issue-cost = current-avg-cost
    cost-of-goods = issue-qty * issue-cost
```

#### FIFO (First In, First Out)
**Purpose**: Use oldest cost first  
**Programs**: st030

```
Structure:
    Cost Layers:
    Layer   Date        Qty     Cost    Value
    1       01/01/24    100     10.00   1,000.00
    2       15/01/24    50      11.00   550.00
    3       01/02/24    75      10.50   787.50

Issue Logic:
    remaining-to-issue = issue-quantity
    total-cost = 0
    
    FOR each layer FROM oldest TO newest
        IF remaining-to-issue > 0
            IF layer-qty >= remaining-to-issue
                issue-from-layer = remaining-to-issue
                layer-qty = layer-qty - remaining-to-issue
                remaining-to-issue = 0
            ELSE
                issue-from-layer = layer-qty
                remaining-to-issue = remaining-to-issue - layer-qty
                layer-qty = 0
            END-IF
            
            total-cost = total-cost + (issue-from-layer * layer-cost)
        END-IF
    END-FOR
    
    unit-cost = total-cost / issue-quantity
```

#### LIFO (Last In, First Out)
**Purpose**: Use newest cost first  
**Programs**: st030

```
Issue Logic:
    Same as FIFO but process layers FROM newest TO oldest
```

#### Standard Cost
**Purpose**: Use predetermined standard costs  
**Programs**: st030

```
Valuation:
    Inventory Value = Quantity on Hand × Standard Cost
    
Variance Calculation:
    Purchase Price Variance = (Actual - Standard) × Quantity
    Usage Variance = (Actual Qty - Standard Qty) × Standard Cost
    
GL Posting:
    DR: Inventory (at standard)
    DR: Purchase Price Variance (if unfavorable)
    CR: Purchase Price Variance (if favorable)
    CR: Accounts Payable (at actual)
```

### 8. Depreciation Methods

#### Straight-Line Depreciation
**Purpose**: Equal depreciation over asset life  
**Programs**: gl030 (if fixed assets module exists)

```
Formula:
    Annual Depreciation = (Cost - Salvage Value) ÷ Useful Life
    Monthly Depreciation = Annual Depreciation ÷ 12

Example:
    Asset cost: 10,000
    Salvage value: 1,000
    Useful life: 5 years
    Annual depreciation: (10,000 - 1,000) ÷ 5 = 1,800
    Monthly depreciation: 150
```

#### Declining Balance
**Purpose**: Accelerated depreciation  
**Programs**: gl030 (if implemented)

```
Formula:
    Depreciation = Book Value × Depreciation Rate
    Rate = (1 ÷ Useful Life) × Acceleration Factor

    Never depreciate below salvage value
```

### 9. Allocation Calculations

#### Overhead Allocation
**Purpose**: Distribute overhead costs  
**Programs**: gl030, cost accounting modules

```
Methods:

1. Direct Labor Hours:
    Allocation = Total Overhead × (Dept Labor Hours ÷ Total Hours)

2. Machine Hours:
    Allocation = Total Overhead × (Dept Machine Hours ÷ Total Hours)

3. Cost-Based:
    Allocation = Total Overhead × (Dept Direct Cost ÷ Total Cost)

Multi-Level Allocation:
    Level 1: Allocate service departments to production
    Level 2: Allocate production overhead to products
```

### 10. Financial Ratios

#### Aging Analysis
**Purpose**: Categorize outstanding balances by age  
**Programs**: sl120, pl080

```
Aging Buckets:
    Current: Not yet due
    1-30 days: 1-30 days overdue
    31-60 days: 31-60 days overdue
    61-90 days: 61-90 days overdue
    Over 90: More than 90 days overdue

Calculation:
    days-outstanding = current-date - invoice-date
    days-overdue = current-date - due-date
    
    IF days-overdue <= 0
        bucket = "Current"
    ELSE IF days-overdue <= 30
        bucket = "1-30"
    ELSE IF days-overdue <= 60
        bucket = "31-60"
    ELSE IF days-overdue <= 90
        bucket = "61-90"
    ELSE
        bucket = "Over 90"
    END-IF
```

#### Turnover Calculations
**Purpose**: Measure inventory efficiency  
**Programs**: st100, management reports

```
Inventory Turnover:
    Turnover = Cost of Goods Sold ÷ Average Inventory
    Days Sales = 365 ÷ Turnover

Receivables Turnover:
    Turnover = Credit Sales ÷ Average Receivables
    Collection Days = 365 ÷ Turnover

Payables Turnover:
    Turnover = Purchases ÷ Average Payables
    Payment Days = 365 ÷ Turnover
```

## Rounding Rules

### General Rounding Principles

1. **Monetary Amounts**: Always round to 2 decimal places
2. **Quantities**: Round based on unit of measure precision
3. **Percentages**: Store with 2 decimal precision, calculate with 4
4. **Unit Costs**: Store with 4 decimal precision
5. **Tax Calculations**: Round final tax amount, not rate calculations

### Rounding Methods

```
Standard Rounding (5/4 Rule):
    IF decimal-part >= 0.5
        round up
    ELSE
        round down
    END-IF

Always Round Up (Ceiling):
    Used for: Minimum order quantities
    
Always Round Down (Floor):
    Used for: Maximum discount calculations

Banker's Rounding (Round to Even):
    IF decimal-part > 0.5
        round up
    ELSE IF decimal-part < 0.5
        round down
    ELSE (exactly 0.5)
        round to nearest even number
    END-IF
```

### Rounding in Calculations

```
Order of Operations:
1. Perform calculation in full precision
2. Apply rounding to final result only
3. Store both rounded and precise values if needed

Example - Invoice Total:
    WRONG:
    line1 = ROUND(qty1 * price1 * (1 - discount1))
    line2 = ROUND(qty2 * price2 * (1 - discount2))
    total = line1 + line2
    
    RIGHT:
    line1 = qty1 * price1 * (1 - discount1)
    line2 = qty2 * price2 * (1 - discount2)
    total = ROUND(line1 + line2)
```

## Business Rule Validations

### Credit Limit Checking

```
Available Credit = Credit Limit - Current Balance - Unshipped Orders

IF new-order-value > available-credit
    IF customer-credit-rating = "A"
        warning-only
    ELSE IF override-authority >= required-level
        allow-with-override
    ELSE
        reject-order
    END-IF
END-IF
```

### Minimum Order Quantities

```
IF order-quantity < item-minimum-order-qty
    IF order-quantity < inner-pack-qty
        suggest-quantity = inner-pack-qty
    ELSE
        suggest-quantity = CEILING(order-quantity / inner-pack-qty) 
                          * inner-pack-qty
    END-IF
    
    require-confirmation
END-IF
```

### Price Protection

```
IF quoted-price < current-price
    IF quote-date + quote-valid-days >= current-date
        honor-quoted-price
    ELSE
        use-current-price
        notify-customer
    END-IF
END-IF
```

## Special Calculations

### Back Order Allocation

```
When Stock Becomes Available:
    
Priority Rules:
1. First Come First Served (FIFO)
2. Customer Priority
3. Order Value
4. Promise Date

Allocation Logic:
    available-qty = receipt-quantity
    
    FOR each back-order BY priority
        IF available-qty > 0
            IF back-order-qty <= available-qty
                allocate-qty = back-order-qty
                available-qty = available-qty - allocate-qty
            ELSE
                allocate-qty = available-qty
                available-qty = 0
            END-IF
            
            create-shipment(back-order, allocate-qty)
        END-IF
    END-FOR
```

### ABC Analysis

```
Classification by Value:
    A Items: Top 20% of value (typically 80% of revenue)
    B Items: Next 30% of value (typically 15% of revenue)
    C Items: Bottom 50% of value (typically 5% of revenue)

Calculation:
    1. Calculate annual value = annual-usage * unit-cost
    2. Sort items by annual value descending
    3. Calculate cumulative value percentage
    4. Assign classification based on cumulative %
```

## Compliance Calculations

### Tax Reporting

```
VAT/GST Return:
    Output Tax = Σ(Sales Invoices Tax) - Σ(Sales Credits Tax)
    Input Tax = Σ(Purchase Invoices Tax) - Σ(Purchase Credits Tax)
    Net Tax = Output Tax - Input Tax
    
    IF net-tax > 0
        payment-due
    ELSE
        refund-due
    END-IF
```

### Audit Trail

```
For Every Financial Transaction:
    audit-record.transaction-id = unique-sequence
    audit-record.date-time = system-timestamp
    audit-record.user-id = current-user
    audit-record.program-id = current-program
    audit-record.action = INSERT/UPDATE/DELETE
    audit-record.before-image = original-values
    audit-record.after-image = new-values
    
    WRITE audit-record
```

## Error Handling in Calculations

### Division by Zero

```
IF denominator = 0
    IF calculation-type = "average"
        result = 0
    ELSE IF calculation-type = "percentage"
        result = 0
        warning-flag = "Y"
    ELSE
        error-flag = "Y"
        error-message = "Division by zero"
    END-IF
ELSE
    result = numerator / denominator
END-IF
```

### Overflow Protection

```
IF calculated-value > field-maximum
    overflow-flag = "Y"
    
    IF critical-calculation
        abort-transaction
        error-message = "Calculation overflow"
    ELSE
        calculated-value = field-maximum
        warning-message = "Value truncated"
    END-IF
END-IF
```

## Testing Considerations

### Test Scenarios for Each Calculation

1. **Boundary Testing**
   - Zero values
   - Maximum values
   - Negative values
   - Single penny amounts

2. **Precision Testing**
   - Rounding edge cases
   - Cumulative rounding errors
   - Currency conversion precision

3. **Business Rule Testing**
   - Each discount tier
   - Tax exemptions
   - Credit limit boundaries
   - Minimum quantities

4. **Integration Testing**
   - Multi-currency transactions
   - Mixed tax rates
   - Complex discount combinations
   - Period-end calculations