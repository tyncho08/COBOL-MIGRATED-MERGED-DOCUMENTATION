# AR_MGMT Interface Definitions

## Overview

This document provides detailed specifications for all interfaces to and from the AR_MGMT subsystem. AR_MGMT has complex integration needs as it coordinates with inventory, processes payments, and feeds financial data to the general ledger.

## Inbound Interfaces

### EDI-AR-001: Electronic Order Interface

**Purpose**: Receive customer orders via EDI

**Protocol**: EDI X12 850 (Purchase Order)
**Frequency**: Real-time/Batch
**Transport**: VAN (Value Added Network) or AS2

**EDI 850 Mapping**:
```
ISA*00*          *00*          *ZZ*SENDER        *ZZ*RECEIVER
GS*PO*SENDER*RECEIVER*20240315*1430*1*X*004010
ST*850*0001
BEG*00*SA*PO123456**20240315
REF*DP*DEPT01
REF*IA*PROMO2024
PER*BD*John Smith*TE*555-1234
N1*BT*ABC Company
N3*123 Main Street
N4*New York*NY*10001*US
N1*ST*ABC Warehouse
N3*456 Ship Lane
N4*Newark*NJ*07101*US
PO1*001*100*EA*25.00**BP*ITEM12345*VN*VENDOR-SKU
PID*F****Premium Widget Blue
PO1*002*50*EA*15.00**BP*ITEM23456*VN*VENDOR-SKU2
CTT*2
SE*16*0001
GE*1*1
IEA*1*000000001
```

**Field Mapping**:
| EDI Element | ACAS Field | Validation |
|-------------|------------|------------|
| BEG03 | Order Number | Unique |
| N1*BT | Bill-to Customer | Must exist |
| N1*ST | Ship-to Address | Valid address |
| PO1*02 | Order Quantity | > 0 |
| PO1*04 | Unit Price | Optional |
| PO1*07 | Item Code | Must exist |

**Processing**:
1. Receive EDI file
2. Parse and validate structure
3. Map to internal order format
4. Validate customer and items
5. Check credit and inventory
6. Create order or reject with 855

---

### BANK-AR-001: Bank Payment File Interface

**Purpose**: Import electronic payments for automatic cash application

**Format**: BAI2 (Bank Administration Institute)
**Frequency**: Daily at 07:00
**File Location**: `/interfaces/bank/`
**File Naming**: `BANK_AR_YYYYMMDD.txt`

**BAI2 Format**:
```
01,SENDER,RECEIVER,240315,0700,1,,,2/
02,12345678,BIGBANK,1,240314,240315,USD/
03,12345678901,USD,010,25000.00,,,,/
16,275,10000.00,S,,,TRN001/INVOICE 12345/
16,475,15000.00,S,,,TRN002/INVOICES 23456,23457,23458/
49,50000.00,2/
98,50000.00,1,2/
99,50000.00,1,4/
```

**Record Types**:
- 01: File header
- 02: Group header (bank/account)
- 03: Account identifier
- 16: Transaction detail
- 49: Account trailer
- 98: Group trailer
- 99: File trailer

**Remittance Processing**:
1. Extract payment reference from 16 records
2. Parse remittance data after TRN
3. Match to open invoices
4. Auto-apply if unique match
5. Queue for manual review if ambiguous

---

### WEB-AR-001: Web Order API

**Purpose**: Accept orders from web portal/mobile app

**Protocol**: REST API over HTTPS
**Format**: JSON
**Authentication**: OAuth 2.0
**Endpoint**: `POST /api/v1/orders`

**Request Schema**:
```json
{
  "order": {
    "customer_id": "123456",
    "po_number": "PO-2024-001",
    "order_date": "2024-03-15",
    "requested_ship": "2024-03-20",
    "ship_to": {
      "name": "ABC Company Warehouse",
      "address1": "123 Shipping Lane",
      "address2": "Suite 100",
      "city": "Newark",
      "state": "NJ",
      "zip": "07101"
    },
    "items": [
      {
        "item_code": "ITEM12345",
        "quantity": 100,
        "unit_price": 25.00,
        "notes": "Rush delivery requested"
      }
    ],
    "special_instructions": "Call before delivery"
  }
}
```

**Response**:
```json
{
  "status": "success",
  "order_number": "ORD-2024-10234",
  "total": 2500.00,
  "tax": 200.00,
  "grand_total": 2700.00,
  "estimated_ship": "2024-03-21",
  "messages": [
    "Item ITEM12345 will ship from NJ warehouse"
  ]
}
```

---

### MAN-AR-001: Manual Order Entry Screen

**Purpose**: Direct order entry by customer service

**Protocol**: Character-based screen
**Program**: sl020

**Screen Flow**:
```
SL020            ORDER ENTRY                    03/15/24

Customer: [123456] ABC Company            Credit: OK
PO Number: [______________]  Ship Via: [___]  Terms: Net 30

Ship To: [x] Same as Bill To
         [ ] Alternate Address: [__________________________]
                                [__________________________]
                                [__________] [__] [_____]

Line Item     Description      Qty    Price    Extended
---- -------- -------------- ------- -------- ----------
01   [______] [____________] [_____] [______] [________]
02   [______] [____________] [_____] [______] [________]

                    Subtotal: [________]
                    Tax:      [________]
                    Total:    [________]

F1=Help F3=Exit F5=Save F6=Credit Check F7=Stock Check
```

## Outbound Interfaces

### AR-GL-001: General Ledger Posting Interface

**Purpose**: Post AR transactions to General Ledger

**Frequency**: Daily at 18:00
**File Format**: Fixed-width text
**File Location**: `/interfaces/ar-to-gl/`
**File Naming**: `AR_GL_YYYYMMDD_HHMMSS.txt`

**Record Layouts**:

Header Record:
```
Pos  Len  Field
1    2    Record Type ('HD')
3    8    Interface ID ('ARGL0001')
9    8    Process Date (YYYYMMDD)
17   6    Process Time (HHMMSS)
23   3    Company Code
26   6    Batch Number
32   8    Record Count
40   15   Control Total (999999999999.99)
```

Detail Records:
```
Pos  Len  Field
1    2    Record Type ('DT')
3    2    Journal Type (IN=Invoice, CR=Credit, PM=Payment)
5    8    Transaction Date (YYYYMMDD)
13   8    GL Account
21   15   Debit Amount (999999999999.99)
36   15   Credit Amount (999999999999.99)
51   10   Reference Number
61   30   Description
91   6    Customer Number
97   3    Department
```

Trailer Record:
```
Pos  Len  Field
1    2    Record Type ('TR')
3    8    Detail Count
11   15   Total Debits
26   15   Total Credits
41   10   Hash Total
```

**GL Account Mapping**:
| Transaction | Debit | Credit |
|------------|-------|--------|
| Sales Invoice | 1200 (AR) | 4000 (Revenue) |
| Tax | 1200 (AR) | 2300 (Tax Payable) |
| Payment | 1000 (Cash) | 1200 (AR) |
| Discount | 4100 (Discount) | 1200 (AR) |

---

### AR-INV-001: Inventory Allocation Request

**Purpose**: Reserve inventory for customer orders

**Protocol**: Program CALL
**Frequency**: Real-time
**Interface**: Through stockMT program

**Request Structure**:
```cobol
01 INVENTORY-ALLOCATION-REQUEST.
   05 OPERATION-CODE      PIC X(10) VALUE "ALLOCATE".
   05 ORDER-NUMBER        PIC 9(8).
   05 LINE-NUMBER         PIC 9(3).
   05 ITEM-CODE          PIC X(20).
   05 WAREHOUSE-CODE     PIC X(3).
   05 QUANTITY-REQUIRED  PIC 9(7)V99.
   05 REQUIRED-DATE      PIC 9(8).
   05 PRIORITY-CODE      PIC X.

01 INVENTORY-ALLOCATION-RESPONSE.
   05 RETURN-CODE        PIC 99.
   05 ALLOCATED-QTY      PIC 9(7)V99.
   05 WAREHOUSE-ALLOC    PIC X(3).
   05 PROMISE-DATE       PIC 9(8).
   05 BACKORDER-QTY      PIC 9(7)V99.
   05 ERROR-MESSAGE      PIC X(50).
```

**Return Codes**:
- 00: Full allocation successful
- 01: Partial allocation only
- 02: No stock available
- 10: Item not found
- 20: Invalid warehouse

---

### AR-INV-002: Shipment Confirmation

**Purpose**: Relieve inventory after shipment

**Protocol**: Batch file
**Frequency**: Real-time after pick confirm
**Format**: CSV

**File Format**:
```csv
HDR,SHIPCONF,20240315,143000
DTL,ORD10234,001,ITEM12345,WH01,100,LOT2024A
DTL,ORD10234,002,ITEM23456,WH01,50,LOT2024B
DTL,ORD10235,001,ITEM34567,WH02,25,LOT2024C
TRL,3,175
```

**Processing**:
1. Validate all items shipped
2. Update order status to shipped
3. Trigger invoice generation
4. Update inventory levels
5. Create shipping documents

---

### AR-RPT-001: AR Data Extract

**Purpose**: Provide AR data for reporting

**Protocol**: Database view/File extract
**Frequency**: On-demand
**Format**: Pipe-delimited

**Extract Types**:

1. **Open Items Extract**
```
CUSTOMER|INVOICE|INV_DATE|DUE_DATE|AMOUNT|BALANCE|DAYS_OLD
123456|INV10001|20240201|20240301|1000.00|1000.00|43
123456|INV10002|20240215|20240315|1500.00|750.00|29
```

2. **Customer Summary**
```
CUSTOMER|NAME|BALANCE|CURRENT|30_DAYS|60_DAYS|90_DAYS|OVER_90
123456|ABC Company|1750.00|750.00|1000.00|0.00|0.00|0.00
```

3. **Sales Analysis**
```
PERIOD|CUSTOMER|PRODUCT|QUANTITY|REVENUE|COST|MARGIN
202403|123456|ITEM12345|100|2500.00|1500.00|1000.00
```

---

### AR-CUST-001: Customer Documents

**Purpose**: Generate and deliver invoices and statements

**Formats**: PDF, Email, EDI
**Frequency**: 
- Invoices: Upon shipment
- Statements: Monthly

**Invoice Layout** (PDF):
```
                    INVOICE
                                        Invoice #: INV10001
Company Name                            Date: 03/15/2024
123 Main Street                         Terms: Net 30
City, ST 12345                         Due: 04/14/2024

Bill To:                Ship To:
ABC Company             ABC Company Warehouse
456 Customer Ln         789 Shipping Way
Somewhere, ST 54321     Somewhere, ST 54321

Item      Description         Qty    Price    Extended
------------------------------------------------------
ITEM12345 Premium Widget     100    25.00    2,500.00
ITEM23456 Standard Widget     50    15.00      750.00

                              Subtotal:      3,250.00
                              Tax (8%):        260.00
                              Total Due:     3,510.00

Remit To: [Bank details]
```

**Statement Format** (Email):
```
Subject: Statement - March 2024

Dear Customer,

Attached is your statement for March 2024.

Current Balance: $1,750.00
- Current: $750.00
- 30 Days: $1,000.00
- Over 30: $0.00

Please remit payment to...
```

## Interface Error Handling

### Error Codes and Recovery

| Interface | Error | Recovery Action |
|-----------|-------|----------------|
| EDI-AR-001 | Invalid customer | Return 855 rejection |
| BANK-AR-001 | Unmatched payment | Queue for manual |
| WEB-AR-001 | Insufficient credit | Return error response |
| AR-GL-001 | Out of balance | Reject batch, alert |
| AR-INV-001 | No stock | Create backorder |

### Monitoring and Alerts

**Interface Health Checks**:
```sql
-- Check for missing bank files
SELECT COUNT(*) AS missing_files
FROM expected_files
WHERE file_date = CURRENT_DATE
  AND received_flag = 'N';

-- Monitor unmatched payments
SELECT COUNT(*) AS unmatched
FROM payment_queue
WHERE status = 'UNMATCHED'
  AND days_old > 3;

-- Track interface errors
SELECT interface_id,
       error_count,
       last_error_time
FROM interface_monitor
WHERE error_count > threshold;
```

## Testing Requirements

### Interface Test Scenarios

1. **EDI Order Processing**
   - Valid order with all fields
   - Missing required fields
   - Invalid customer/item
   - Credit limit exceeded

2. **Payment Processing**
   - Exact match single invoice
   - Multiple invoice payment
   - Partial payment
   - Overpayment
   - No matching invoices

3. **GL Posting**
   - Balanced batch
   - Out of balance detection
   - Large volume (1000+ lines)
   - Period cutoff

### Test Data Management

- Maintain test customer accounts
- Test item codes with various attributes
- Known payment scenarios
- Error injection capabilities