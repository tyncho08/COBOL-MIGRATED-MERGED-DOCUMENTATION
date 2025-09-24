# AP_MGMT Subsystem - Interface Documentation

## Overview

The AP_MGMT subsystem serves as the central hub for all supplier-related financial transactions. It interfaces with multiple subsystems to ensure accurate expense recording, inventory updates, and payment processing.

## Interface Architecture

```
┌─────────────────┐     ┌─────────────────┐     ┌─────────────────┐
│   INV_CTRL      │────►│     AP_MGMT     │────►│    GL_CORE      │
│                 │◄────│                 │     │                 │
└─────────────────┘     └─────────────────┘     └─────────────────┘
                               │ ▲
                               │ │
                        ┌──────▼─┴──────┐
                        │     MDM       │
                        │               │
                        └───────────────┘
```

## Detailed Interface Specifications

### 1. AP → GL_CORE Interface

**Interface ID**: AP_GL_POST  
**Type**: Asynchronous Batch  
**Direction**: Outbound from AP_MGMT  
**Frequency**: Real-time (triggered by transaction)

#### Data Contract

```
JOURNAL-BATCH-RECORD
├── BATCH-HEADER
│   ├── BATCH-ID            (PIC 9(8))
│   ├── BATCH-DATE          (PIC 9(8))
│   ├── BATCH-TYPE          (PIC X(2)) "AP"
│   ├── BATCH-COUNT         (PIC 9(4))
│   └── BATCH-TOTAL         (PIC S9(11)V99)
└── JOURNAL-ENTRIES (OCCURS 1 TO 999)
    ├── JOURNAL-NUMBER      (PIC 9(8))
    ├── JOURNAL-DATE        (PIC 9(8))
    ├── ACCOUNT-NUMBER      (PIC X(10))
    ├── DEBIT-AMOUNT        (PIC S9(11)V99)
    ├── CREDIT-AMOUNT       (PIC S9(11)V99)
    ├── REFERENCE           (PIC X(20))
    ├── DESCRIPTION         (PIC X(50))
    └── SOURCE-DOC          (PIC X(15))
```

#### Processing Rules
- All journal entries must balance (total debits = total credits)
- Account numbers validated against GL chart of accounts
- Posting period must be open
- Batch control totals must match detail

#### Error Handling
- Invalid account: Reject entire batch, return error code
- Out of balance: Reject batch, log to error file
- Period closed: Queue for next period opening

### 2. AP → INV_CTRL Interface

**Interface ID**: AP_INV_UPDATE  
**Type**: Real-time Call  
**Direction**: Outbound from AP_MGMT  
**Frequency**: On goods receipt posting

#### Data Contract

```
INVENTORY-RECEIPT-RECORD
├── RECEIPT-HEADER
│   ├── RECEIPT-NUMBER      (PIC 9(8))
│   ├── RECEIPT-DATE        (PIC 9(8))
│   ├── PO-NUMBER           (PIC 9(8))
│   ├── SUPPLIER-ID         (PIC X(7))
│   └── LOCATION-CODE       (PIC X(4))
└── RECEIPT-LINES (OCCURS 1 TO 999)
    ├── LINE-NUMBER         (PIC 9(3))
    ├── ITEM-CODE           (PIC X(15))
    ├── QUANTITY-RECEIVED   (PIC S9(7)V999)
    ├── UNIT-COST           (PIC S9(7)V9999)
    ├── LOT-NUMBER          (PIC X(20))
    └── EXPIRY-DATE         (PIC 9(8))
```

#### Processing Rules
- Item code must exist in inventory master
- Quantity cannot exceed PO outstanding quantity
- Cost variance warnings if >5% from standard
- Update inventory on-hand and average cost

#### Error Handling
- Invalid item: Log error, continue with next line
- Over-receipt: Warning message, allow with override
- Location full: Suggest alternative locations

### 3. INV_CTRL → AP Interface

**Interface ID**: AP_PO_CREATE  
**Type**: Batch File  
**Direction**: Inbound to AP_MGMT  
**Frequency**: Daily at 2:00 AM

#### Data Contract

```
REORDER-REQUEST-RECORD
├── REQUEST-HEADER
│   ├── REQUEST-DATE        (PIC 9(8))
│   ├── REQUEST-TIME        (PIC 9(6))
│   ├── PRIORITY-CODE       (PIC X(1))
│   └── REQUESTOR-ID        (PIC X(8))
└── REORDER-ITEMS (OCCURS 1 TO 999)
    ├── ITEM-CODE           (PIC X(15))
    ├── ITEM-DESCRIPTION    (PIC X(50))
    ├── SUGGESTED-SUPPLIER  (PIC X(7))
    ├── QUANTITY-NEEDED     (PIC S9(7)V999)
    ├── REQUIRED-DATE       (PIC 9(8))
    └── LAST-PRICE          (PIC S9(7)V9999)
```

#### Processing Rules
- Consolidate multiple items for same supplier
- Apply quantity breaks for pricing
- Check preferred supplier agreements
- Generate PO with "PENDING" status

#### Error Handling
- Invalid supplier: Use default supplier for item
- No supplier defined: Create exception report
- Credit limit exceeded: Flag for manual review

### 4. AP → MDM Interface

**Interface ID**: AP_MDM_LOOKUP  
**Type**: Synchronous Call  
**Direction**: Bidirectional  
**Frequency**: As needed

#### Data Contract - Supplier Lookup

```
SUPPLIER-LOOKUP-REQUEST
├── LOOKUP-TYPE             (PIC X(1)) "S"=Supplier
├── SUPPLIER-ID             (PIC X(7))
└── INCLUDE-DETAILS         (PIC X(1)) "Y"/"N"

SUPPLIER-LOOKUP-RESPONSE
├── RESPONSE-CODE           (PIC X(2))
├── SUPPLIER-RECORD
│   ├── SUPPLIER-ID         (PIC X(7))
│   ├── SUPPLIER-NAME       (PIC X(50))
│   ├── PAYMENT-TERMS       (PIC X(3))
│   ├── CREDIT-LIMIT        (PIC S9(9)V99)
│   ├── TAX-ID              (PIC X(15))
│   └── STATUS-CODE         (PIC X(1))
└── ERROR-MESSAGE           (PIC X(80))
```

#### Processing Rules
- Cache frequently used suppliers in memory
- Validate check digit on supplier ID
- Return full details only if requested
- Apply security based on user role

### 5. AP → RPT_ENGINE Interface

**Interface ID**: AP_RPT_DATA  
**Type**: Data Extract  
**Direction**: Outbound from AP_MGMT  
**Frequency**: On-demand / Scheduled

#### Data Contract

```
AP-REPORT-EXTRACT
├── EXTRACT-HEADER
│   ├── EXTRACT-DATE        (PIC 9(8))
│   ├── EXTRACT-TYPE        (PIC X(10))
│   ├── PERIOD-START        (PIC 9(8))
│   └── PERIOD-END          (PIC 9(8))
└── DETAIL-RECORDS
    ├── RECORD-TYPE         (PIC X(2))
    └── RECORD-DATA         (PIC X(998))
```

Record Types:
- "IN" - Invoice detail
- "PO" - Purchase order detail
- "PM" - Payment detail
- "SP" - Supplier summary

### 6. External Bank Interface

**Interface ID**: AP_BANK_PAY  
**Type**: File Transfer  
**Direction**: Outbound from AP_MGMT  
**Frequency**: Daily at 3:00 PM

#### Data Contract

```
PAYMENT-FILE-RECORD
├── FILE-HEADER
│   ├── FILE-DATE           (PIC 9(8))
│   ├── COMPANY-ID          (PIC X(10))
│   ├── BANK-ID             (PIC X(9))
│   └── TOTAL-AMOUNT        (PIC S9(11)V99)
└── PAYMENT-DETAILS (OCCURS 1 TO 9999)
    ├── PAYMENT-TYPE        (PIC X(3)) "CHK"/"ACH"/"WIR"
    ├── PAYEE-NAME          (PIC X(50))
    ├── PAYEE-ACCOUNT       (PIC X(34))
    ├── PAYMENT-AMOUNT      (PIC S9(9)V99)
    ├── PAYMENT-DATE        (PIC 9(8))
    └── REFERENCE           (PIC X(35))
```

#### Processing Rules
- Generate check numbers sequentially
- Create positive pay file for fraud prevention
- Apply bank-specific formatting rules
- Generate remittance advice separately

## Interface Control and Monitoring

### Control Tables

**Interface Control (APINTCTL)**
```
01 INTERFACE-CONTROL.
   05 INTERFACE-ID          PIC X(12).
   05 LAST-RUN-DATE         PIC 9(8).
   05 LAST-RUN-TIME         PIC 9(6).
   05 LAST-STATUS           PIC X(1).
   05 RECORD-COUNT          PIC 9(9).
   05 CONTROL-TOTAL         PIC S9(13)V99.
   05 ERROR-COUNT           PIC 9(6).
```

### Error Logging

**Interface Error Log (APINTERR)**
```
01 INTERFACE-ERROR.
   05 ERROR-TIMESTAMP       PIC 9(14).
   05 INTERFACE-ID          PIC X(12).
   05 RECORD-KEY            PIC X(50).
   05 ERROR-CODE            PIC X(5).
   05 ERROR-MESSAGE         PIC X(100).
   05 ERROR-DATA            PIC X(200).
```

## Integration Patterns

### Pattern 1: Guaranteed Delivery
- Write to interface staging table
- Process with retry logic
- Archive after successful transmission

### Pattern 2: Idempotent Processing
- Include unique transaction ID
- Check for duplicates before processing
- Log all attempts for audit

### Pattern 3: Bulk vs Real-time
- Real-time for critical updates (GL postings)
- Batch for high-volume transactions (payment runs)
- Queue for asynchronous processing

## Security Considerations

1. **Data Encryption**
   - Bank files encrypted with PGP
   - Sensitive data masked in logs

2. **Access Control**
   - Interface execution restricted by user role
   - Audit trail for all interface runs

3. **Data Validation**
   - Input sanitization for external data
   - Check totals and record counts

## Monitoring and Alerts

### Key Metrics
- Interface success/failure rates
- Processing time trends
- Record count variances
- Error frequency by type

### Alert Conditions
- Interface failure (immediate)
- Processing time > threshold
- Error rate > 5%
- Missing expected file

## Disaster Recovery

### Backup Procedures
- Interface control tables backed up hourly
- Staging data retained for 7 days
- Archive files kept for 1 year

### Recovery Steps
1. Restore control tables to last known good state
2. Reprocess failed interfaces from staging
3. Reconcile with target systems
4. Update control totals

## Version History

| Version | Date | Changes | Author |
|---------|------|---------|--------|
| 1.0 | 2024-01-15 | Initial documentation | System |
| 1.1 | 2024-02-01 | Added bank interface | System |
| 1.2 | 2024-03-15 | Enhanced error handling | System |