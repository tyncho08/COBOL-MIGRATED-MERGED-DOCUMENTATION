# INV_CTRL Subsystem - Interface Documentation

## Overview

The INV_CTRL subsystem acts as the central repository for all inventory-related data and provides real-time interfaces to sales, purchasing, and financial systems. It ensures accurate stock tracking, optimal inventory levels, and proper valuation for financial reporting.

## Interface Architecture

```
                    ┌─────────────────┐
                    │   Warehouse     │
                    │   Management    │
                    └────────┬────────┘
                             │
            ┌────────────────┴────────────────┐
            │                                 │
            ▼                                 ▼
    ┌─────────────┐                   ┌─────────────┐
    │  Physical   │                   │   Cycle     │
    │ Inventory   │                   │  Counting   │
    └──────┬──────┘                   └──────┬──────┘
           │                                 │
           └─────────────┬───────────────────┘
                         │
                         ▼
    ┌────────────────────────────────────────────────┐
    │                  INV_CTRL                       │
    │  ┌──────────┐  ┌──────────┐  ┌──────────┐     │
    │  │  Stock   │  │Movement  │  │Valuation │     │
    │  │ Master   │  │Processing│  │ Engine   │     │
    │  └──────────┘  └──────────┘  └──────────┘     │
    └────────────────────┬───────────────────────────┘
                         │
     ┌──────────┬────────┴────────┬────────┬────────┐
     ▼          ▼                 ▼        ▼        ▼
┌─────────┐ ┌─────────┐    ┌─────────┐ ┌──────┐ ┌────────┐
│ AR_MGMT │ │ AP_MGMT │    │ GL_CORE │ │ MDM  │ │  RPT   │
│Sales    │ │Purchase │    │Financial│ │Master│ │Reports │
└─────────┘ └─────────┘    └─────────┘ └──────┘ └────────┘
```

## Detailed Interface Specifications

### 1. AR_MGMT ↔ INV_CTRL Interface

**Interface ID**: INV_AR_INTERFACE  
**Type**: Real-time bidirectional  
**Frequency**: Continuous during business hours

#### Stock Availability Request (AR → INV)

```
STOCK-AVAILABILITY-REQUEST
├── REQUEST-HEADER
│   ├── REQUEST-ID          (PIC X(12))
│   ├── REQUEST-TIMESTAMP   (PIC 9(14))
│   ├── REQUESTING-USER     (PIC X(8))
│   └── URGENCY-FLAG        (PIC X(1)) "H"=High, "N"=Normal
├── REQUEST-DETAILS
│   ├── ITEM-CODE           (PIC X(15))
│   ├── QUANTITY-REQUIRED   (PIC S9(7)V999)
│   ├── REQUIRED-DATE       (PIC 9(8))
│   ├── LOCATION-CODE       (PIC X(4))
│   └── CUSTOMER-CODE       (PIC X(7))

STOCK-AVAILABILITY-RESPONSE
├── RESPONSE-HEADER
│   ├── REQUEST-ID          (PIC X(12))
│   ├── RESPONSE-TIMESTAMP  (PIC 9(14))
│   ├── RESPONSE-CODE       (PIC X(2))
│   └── RESPONSE-MESSAGE    (PIC X(80))
├── AVAILABILITY-DETAILS
│   ├── ITEM-CODE           (PIC X(15))
│   ├── ITEM-DESCRIPTION    (PIC X(50))
│   ├── QUANTITY-ON-HAND    (PIC S9(7)V999)
│   ├── QUANTITY-ALLOCATED  (PIC S9(7)V999)
│   ├── QUANTITY-AVAILABLE  (PIC S9(7)V999)
│   ├── QUANTITY-ON-ORDER   (PIC S9(7)V999)
│   ├── NEXT-RECEIPT-DATE   (PIC 9(8))
│   ├── NEXT-RECEIPT-QTY    (PIC S9(7)V999)
│   ├── SUBSTITUTE-ITEMS    (PIC X(100))
│   └── LEAD-TIME-DAYS      (PIC 9(3))
```

#### Stock Allocation Request (AR → INV)

```
STOCK-ALLOCATION-REQUEST
├── ALLOCATION-HEADER
│   ├── ORDER-NUMBER        (PIC 9(8))
│   ├── CUSTOMER-CODE       (PIC X(7))
│   ├── ORDER-DATE          (PIC 9(8))
│   ├── REQUIRED-DATE       (PIC 9(8))
│   └── PRIORITY-CODE       (PIC X(1))
└── ALLOCATION-LINES (OCCURS 1 TO 999)
    ├── LINE-NUMBER         (PIC 9(3))
    ├── ITEM-CODE           (PIC X(15))
    ├── QUANTITY-ORDERED    (PIC S9(7)V999)
    ├── LOCATION-CODE       (PIC X(4))
    └── PROMISED-DATE       (PIC 9(8))

STOCK-ALLOCATION-RESPONSE
├── ALLOCATION-ID           (PIC 9(10))
├── ORDER-NUMBER            (PIC 9(8))
├── ALLOCATION-STATUS       (PIC X(1))
│   "C" = Complete
│   "P" = Partial
│   "B" = Backordered
│   "R" = Rejected
└── ALLOCATION-LINES (OCCURS 1 TO 999)
    ├── LINE-NUMBER         (PIC 9(3))
    ├── ITEM-CODE           (PIC X(15))
    ├── QUANTITY-ALLOCATED  (PIC S9(7)V999)
    ├── QUANTITY-BACKORDERED (PIC S9(7)V999)
    ├── PROMISED-DATE       (PIC 9(8))
    └── ALLOCATION-NOTES    (PIC X(50))
```

### 2. AP_MGMT ↔ INV_CTRL Interface

**Interface ID**: INV_AP_INTERFACE  
**Type**: Real-time bidirectional  
**Frequency**: Continuous

#### Purchase Receipt Processing (AP → INV)

```
PURCHASE-RECEIPT-HEADER
├── RECEIPT-NUMBER          (PIC 9(8))
├── RECEIPT-DATE            (PIC 9(8))
├── RECEIPT-TIME            (PIC 9(6))
├── PO-NUMBER               (PIC 9(8))
├── SUPPLIER-CODE           (PIC X(7))
├── RECEIVING-LOCATION      (PIC X(4))
├── RECEIVER-ID             (PIC X(8))
└── TOTAL-LINES             (PIC 9(3))

PURCHASE-RECEIPT-DETAIL
├── LINE-NUMBER             (PIC 9(3))
├── ITEM-CODE               (PIC X(15))
├── QUANTITY-RECEIVED       (PIC S9(7)V999)
├── UNIT-COST               (PIC S9(7)V9999)
├── LOT-NUMBER              (PIC X(20))
├── SERIAL-NUMBERS          (PIC X(200))
├── EXPIRY-DATE             (PIC 9(8))
├── QUALITY-STATUS          (PIC X(1))
│   "A" = Accepted
│   "Q" = Quarantine
│   "R" = Rejected
├── INSPECTION-NOTES        (PIC X(100))
└── SPECIAL-HANDLING        (PIC X(50))

RECEIPT-RESPONSE
├── RECEIPT-NUMBER          (PIC 9(8))
├── PROCESSING-STATUS       (PIC X(1))
├── TOTAL-VALUE             (PIC S9(11)V99)
└── RECEIPT-LINES (OCCURS 1 TO 999)
    ├── LINE-NUMBER         (PIC 9(3))
    ├── ITEM-CODE           (PIC X(15))
    ├── PROCESSED-STATUS    (PIC X(1))
    ├── NEW-ON-HAND-QTY     (PIC S9(7)V999)
    ├── NEW-AVERAGE-COST    (PIC S9(7)V9999)
    └── ERROR-MESSAGE       (PIC X(80))
```

#### Reorder Recommendations (INV → AP)

```
REORDER-RECOMMENDATION-HEADER
├── RECOMMENDATION-DATE     (PIC 9(8))
├── RECOMMENDATION-TIME     (PIC 9(6))
├── ANALYSIS-PERIOD-FROM    (PIC 9(8))
├── ANALYSIS-PERIOD-TO      (PIC 9(8))
├── TOTAL-RECOMMENDATIONS   (PIC 9(6))
└── PRIORITY-FILTER         (PIC X(1))

REORDER-RECOMMENDATION-DETAIL
├── ITEM-CODE               (PIC X(15))
├── ITEM-DESCRIPTION        (PIC X(50))
├── PREFERRED-SUPPLIER      (PIC X(7))
├── CURRENT-STOCK           (PIC S9(7)V999)
├── REORDER-POINT           (PIC S9(7)V999)
├── MAXIMUM-LEVEL           (PIC S9(7)V999)
├── RECOMMENDED-QTY         (PIC S9(7)V999)
├── LAST-COST               (PIC S9(7)V9999)
├── ESTIMATED-VALUE         (PIC S9(9)V99)
├── LEAD-TIME-DAYS          (PIC 9(3))
├── PRIORITY-CODE           (PIC X(1))
├── USAGE-LAST-30-DAYS      (PIC S9(7)V999)
├── USAGE-LAST-90-DAYS      (PIC S9(7)V999)
├── STOCKOUT-RISK           (PIC X(1))
└── NOTES                   (PIC X(100))
```

### 3. INV_CTRL → GL_CORE Interface

**Interface ID**: INV_GL_INTERFACE  
**Type**: Real-time journal posting  
**Frequency**: Continuous

#### Inventory Value Postings

```
INVENTORY-JOURNAL-HEADER
├── JOURNAL-BATCH-ID        (PIC X(10))
├── JOURNAL-DATE            (PIC 9(8))
├── JOURNAL-TIME            (PIC 9(6))
├── POSTING-TYPE            (PIC X(3))
│   "REC" = Receipt
│   "ISS" = Issue
│   "ADJ" = Adjustment
│   "TRF" = Transfer
│   "VAL" = Revaluation
├── TOTAL-ENTRIES           (PIC 9(4))
├── CONTROL-TOTAL-DR        (PIC S9(13)V99)
├── CONTROL-TOTAL-CR        (PIC S9(13)V99)
└── SOURCE-REFERENCE        (PIC X(20))

INVENTORY-JOURNAL-ENTRY
├── ENTRY-NUMBER            (PIC 9(4))
├── ACCOUNT-NUMBER          (PIC X(10))
├── COST-CENTER             (PIC X(6))
├── DEBIT-AMOUNT            (PIC S9(11)V99)
├── CREDIT-AMOUNT           (PIC S9(11)V99)
├── DESCRIPTION             (PIC X(50))
├── REFERENCE-NUMBER        (PIC X(20))
├── ITEM-CODE               (PIC X(15))
├── LOCATION-CODE           (PIC X(4))
├── QUANTITY                (PIC S9(7)V999)
├── UNIT-COST               (PIC S9(7)V9999)
└── ANALYSIS-CODE           (PIC X(6))
```

### 4. Warehouse Management Interface

**Interface ID**: INV_WMS_INTERFACE  
**Type**: Bidirectional file and real-time  
**Frequency**: Continuous

#### Physical Movement Confirmation

```
PHYSICAL-MOVEMENT-HEADER
├── MOVEMENT-BATCH-ID       (PIC X(10))
├── MOVEMENT-DATE           (PIC 9(8))
├── MOVEMENT-TIME           (PIC 9(6))
├── LOCATION-CODE           (PIC X(4))
├── OPERATOR-ID             (PIC X(8))
├── DEVICE-ID               (PIC X(10))
├── TOTAL-MOVEMENTS         (PIC 9(6))
└── BATCH-STATUS            (PIC X(1))

PHYSICAL-MOVEMENT-DETAIL
├── MOVEMENT-SEQ            (PIC 9(6))
├── MOVEMENT-TYPE           (PIC X(3))
│   "REC" = Receipt
│   "ISS" = Issue
│   "TRF" = Transfer
│   "ADJ" = Adjustment
│   "CNT" = Count
├── ITEM-CODE               (PIC X(15))
├── FROM-LOCATION           (PIC X(10))
├── TO-LOCATION             (PIC X(10))
├── QUANTITY                (PIC S9(7)V999)
├── LOT-NUMBER              (PIC X(20))
├── SERIAL-NUMBER           (PIC X(30))
├── REFERENCE-DOC           (PIC X(20))
├── TIMESTAMP               (PIC 9(14))
└── OPERATOR-VERIFIED       (PIC X(1))
```

### 5. Cycle Count Interface

**Interface ID**: INV_COUNT_INTERFACE  
**Type**: Bidirectional  
**Frequency**: Daily

#### Count Schedule Generation (INV → Count System)

```
COUNT-SCHEDULE-HEADER
├── SCHEDULE-ID             (PIC X(10))
├── SCHEDULE-DATE           (PIC 9(8))
├── COUNT-TYPE              (PIC X(1))
│   "C" = Cycle
│   "P" = Physical
│   "S" = Spot
├── LOCATION-CODE           (PIC X(4))
├── PRIORITY                (PIC X(1))
├── ASSIGNED-COUNTER        (PIC X(8))
├── TOTAL-ITEMS             (PIC 9(6))
└── COMPLETION-DATE         (PIC 9(8))

COUNT-SCHEDULE-DETAIL
├── ITEM-CODE               (PIC X(15))
├── ITEM-DESCRIPTION        (PIC X(50))
├── LOCATION-DETAIL         (PIC X(10))
├── EXPECTED-QUANTITY       (PIC S9(7)V999)
├── UNIT-OF-MEASURE         (PIC X(3))
├── LOT-TRACKED             (PIC X(1))
├── SERIAL-TRACKED          (PIC X(1))
├── LAST-COUNT-DATE         (PIC 9(8))
├── COUNT-FREQUENCY         (PIC 9(3))
├── ABC-CLASS               (PIC X(1))
└── SPECIAL-INSTRUCTIONS    (PIC X(100))
```

#### Count Results Processing (Count System → INV)

```
COUNT-RESULTS-HEADER
├── SCHEDULE-ID             (PIC X(10))
├── COUNT-DATE              (PIC 9(8))
├── COUNT-TIME              (PIC 9(6))
├── COUNTER-ID              (PIC X(8))
├── SUPERVISOR-ID           (PIC X(8))
├── RESULTS-STATUS          (PIC X(1))
├── TOTAL-ITEMS-COUNTED     (PIC 9(6))
├── TOTAL-VARIANCES         (PIC 9(6))
└── VARIANCE-VALUE          (PIC S9(11)V99)

COUNT-RESULTS-DETAIL
├── ITEM-CODE               (PIC X(15))
├── LOCATION-DETAIL         (PIC X(10))
├── EXPECTED-QUANTITY       (PIC S9(7)V999)
├── COUNTED-QUANTITY        (PIC S9(7)V999)
├── VARIANCE-QUANTITY       (PIC S9(7)V999)
├── VARIANCE-VALUE          (PIC S9(9)V99)
├── VARIANCE-PERCENT        (PIC S999V99)
├── LOT-NUMBERS-FOUND       (PIC X(200))
├── SERIAL-NUMBERS-FOUND    (PIC X(500))
├── COUNT-STATUS            (PIC X(1))
├── NOTES                   (PIC X(100))
└── REQUIRES-RECOUNT        (PIC X(1))
```

## Interface Control and Monitoring

### Performance Monitoring

```
INTERFACE-PERFORMANCE-LOG
├── LOG-TIMESTAMP           (PIC 9(14))
├── INTERFACE-ID            (PIC X(12))
├── OPERATION-TYPE          (PIC X(10))
├── RESPONSE-TIME-MS        (PIC 9(6))
├── RECORD-COUNT            (PIC 9(8))
├── SUCCESS-COUNT           (PIC 9(8))
├── ERROR-COUNT             (PIC 9(6))
├── THROUGHPUT-RATE         (PIC 9(8))
└── PEAK-HOUR-FLAG          (PIC X(1))
```

### Error Recovery Procedures

```
INTERFACE-ERROR-RECOVERY
├── ERROR-ID                (PIC 9(10))
├── INTERFACE-ID            (PIC X(12))
├── ERROR-TIMESTAMP         (PIC 9(14))
├── ERROR-TYPE              (PIC X(3))
├── ORIGINAL-MESSAGE        (PIC X(1000))
├── ERROR-DESCRIPTION       (PIC X(200))
├── RETRY-COUNT             (PIC 9(2))
├── RECOVERY-ACTION         (PIC X(50))
├── RESOLUTION-STATUS       (PIC X(1))
└── MANUAL-INTERVENTION     (PIC X(1))
```

## Business Rules in Interfaces

### Stock Availability Rules

1. **Allocation Priority**
   - Existing customer orders take priority
   - Credit-approved customers get preference
   - Rush orders override normal allocation
   - Key customers have reserved stock

2. **Multi-Location Logic**
   - Check primary location first
   - Cascade to alternate locations
   - Consider transfer time in availability
   - Respect location restrictions

3. **Substitution Rules**
   - Offer approved substitutes for stockouts
   - Maintain substitution hierarchy
   - Consider customer preferences
   - Price and margin implications

### Movement Validation Rules

1. **Receipt Validation**
   - Match to authorized purchase orders
   - Verify quantities within tolerance
   - Check quality inspection requirements
   - Validate lot/serial number formats

2. **Issue Validation**
   - Confirm allocation exists
   - Verify sufficient available stock
   - Check lot/serial tracking requirements
   - Ensure proper authorization

3. **Transfer Validation**
   - Verify source location has stock
   - Confirm target location can receive
   - Check transfer authorization
   - Validate item-location compatibility

### Cost Interface Rules

1. **Cost Method Consistency**
   - Use same method throughout period
   - Handle method changes at period end
   - Maintain cost layer integrity
   - Support multiple cost books

2. **Valuation Rules**
   - Lower of cost or market
   - Standard cost variance tracking
   - Obsolescence reserves
   - Write-down procedures

## Integration Patterns

### Real-time Availability Pattern

```
AVAILABILITY-CHECK-PATTERN
  CALL "STOCK-AVAILABILITY" USING
    item-code
    quantity-required
    required-date
    location-preference
  RETURNING
    available-quantity
    allocation-possible
    alternative-dates
    substitute-items
```

### Batch Reorder Pattern

```
REORDER-ANALYSIS-PATTERN
  CALL "ANALYZE-STOCK-LEVELS" USING
    analysis-date
    location-filter
    item-category-filter
  RETURNING
    reorder-list
    urgent-items
    excess-stock-items
```

### Movement Processing Pattern

```
MOVEMENT-PROCESSING-PATTERN
  CALL "VALIDATE-MOVEMENT" USING
    movement-details
  IF validation-successful
    CALL "UPDATE-BALANCES" USING
      movement-details
    CALL "POST-TO-GL" USING
      gl-entries
    CALL "UPDATE-COSTS" USING
      cost-details
  ELSE
    CALL "LOG-ERROR" USING
      error-details
  END-IF
```

## Security and Audit

### Access Control

```
INVENTORY-SECURITY-MATRIX
├── USER-ID                 (PIC X(8))
├── FUNCTION-CODE           (PIC X(6))
├── LOCATION-ACCESS         (PIC X(4) OCCURS 20)
├── ITEM-CATEGORY-ACCESS    (PIC X(3) OCCURS 10)
├── VALUE-LIMIT             (PIC S9(9)V99)
├── AUTHORIZATION-LEVEL     (PIC X(1))
└── APPROVAL-REQUIRED       (PIC X(1))
```

### Audit Trail

```
INVENTORY-AUDIT-TRAIL
├── AUDIT-ID                (PIC 9(12))
├── TRANSACTION-ID          (PIC 9(10))
├── USER-ID                 (PIC X(8))
├── FUNCTION-PERFORMED      (PIC X(20))
├── TIMESTAMP               (PIC 9(14))
├── BEFORE-VALUES           (PIC X(500))
├── AFTER-VALUES            (PIC X(500))
├── AUTHORIZATION-CODE      (PIC X(10))
└── BUSINESS-JUSTIFICATION  (PIC X(100))
```