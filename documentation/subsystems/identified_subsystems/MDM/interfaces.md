# MDM Interface Definitions

## Overview

The MDM subsystem serves as the master data provider for all other subsystems. Its interfaces are primarily outbound (providing data) with limited inbound interfaces for data maintenance and loading.

## Inbound Interfaces

### EXT-MDM-001: External Data Load Interface

**Purpose**: Bulk load master data from external sources

**Protocol**: File-based batch import
**Formats Supported**: CSV, Excel, XML
**Frequency**: On-demand
**File Location**: `/imports/mdm/`

#### Customer Import Format (CSV)

**File Naming**: `CUST_IMPORT_YYYYMMDD_HHMMSS.csv`

**Column Layout**:
```csv
Action,CustomerName,DBA,TaxID,CustomerType,Address1,Address2,City,State,Zip,Country,Phone,Email,CreditLimit,PaymentTerms,TaxExempt,Status
NEW,ABC Company Inc,ABC Company,12-3456789,Commercial,123 Main St,Suite 100,New York,NY,10001,US,212-555-1234,ar@abccompany.com,50000,NET30,N,A
UPDATE,XYZ Corp,,98-7654321,Commercial,456 Park Ave,,Boston,MA,02101,US,617-555-5678,billing@xyzcorp.com,100000,NET45,Y,A
```

**Field Definitions**:
| Field | Required | Format | Validation |
|-------|----------|--------|------------|
| Action | Y | NEW/UPDATE/DELETE | Valid action |
| CustomerName | Y | Varchar(100) | Not empty |
| DBA | N | Varchar(100) | - |
| TaxID | Y | XX-XXXXXXX | Valid format |
| CustomerType | Y | Code | Valid type code |
| Address1 | Y | Varchar(60) | Not empty |
| Address2 | N | Varchar(60) | - |
| City | Y | Varchar(40) | Not empty |
| State | Y | Char(2) | Valid state |
| Zip | Y | Varchar(10) | Valid format |
| Country | Y | Char(2) | Valid country |
| Phone | N | Varchar(20) | Valid format |
| Email | N | Varchar(100) | Valid email |
| CreditLimit | N | Decimal(11,2) | >= 0 |
| PaymentTerms | Y | Code | Valid terms |
| TaxExempt | Y | Y/N | Y or N |
| Status | Y | A/I/H | Valid status |

#### Vendor Import Format (CSV)

**Similar structure with vendor-specific fields**:
- 1099 Type
- W9 on File
- Bank Account Details (encrypted)
- Remit-to Address

#### Item Import Format (CSV)

**Column Layout**:
```csv
Action,ItemCode,Description,LongDesc,ItemType,Category,SubCategory,UOM,CaseQty,Weight,Status,TaxableItem,MinOrderQty,LeadTime
NEW,WIDGET001,Blue Widget,Premium Blue Widget 5in,FG,WIDGETS,BLUE,EA,12,0.5,A,Y,1,7
```

### USER-MDM-001: Maintenance Screen Interface

**Purpose**: Online maintenance of master data

**Protocol**: Character-based screens
**Programs**: Various maintenance programs

#### Customer Maintenance (custmaint)

```
CUSTMAINT            CUSTOMER MAINTENANCE             03/15/24

Action: [A]dd [C]hange [D]elete [I]nquire: _

Customer: [______] ________________________________
Tax ID: [__-_______]  Type: [___] _____________

Address: [________________________________________]
         [________________________________________]
City/State/Zip: [____________________] [__] [________]

Phone: [___-___-____]  Email: [_____________________]
Credit Limit: [_________]  Terms: [___]  Tax Exempt: [_]

Status: [_] Active  Created: __/__/__  Modified: __/__/__

F1=Help F3=Exit F5=Save F6=Search F10=Delete
```

### API-MDM-001: REST API Interface

**Purpose**: Real-time master data updates from external systems

**Protocol**: REST over HTTPS
**Authentication**: API Key + OAuth 2.0
**Base URL**: `/api/v1/mdm`

#### Customer API

**Create Customer**:
```http
POST /api/v1/mdm/customers
Content-Type: application/json
Authorization: Bearer {token}

{
  "customerName": "New Customer Inc",
  "taxId": "12-3456789",
  "customerType": "COMM",
  "addresses": [{
    "type": "BILLTO",
    "address1": "123 New Street",
    "city": "New York",
    "state": "NY",
    "postalCode": "10001",
    "country": "US"
  }],
  "creditLimit": 50000,
  "paymentTerms": "NET30"
}

Response:
{
  "customerId": "123456",
  "status": "success",
  "message": "Customer created successfully"
}
```

**Update Customer**:
```http
PUT /api/v1/mdm/customers/{customerId}
```

**Get Customer**:
```http
GET /api/v1/mdm/customers/{customerId}
```

## Outbound Interfaces

### MDM-ALL-001: Master Data Access Services

**Purpose**: Provide read access to master data for all subsystems

**Protocol**: Multiple access methods
1. Direct database views (legacy)
2. Program CALL interface
3. REST API (modernized)
4. Cached data access

#### Program CALL Interface

**Customer Lookup**:
```cobol
01 MDM-CUSTOMER-REQUEST.
   05 MDM-FUNCTION       PIC X(10).
      88 GET-CUSTOMER    VALUE "GETCUST".
      88 SEARCH-CUSTOMER VALUE "SEARCHCUST".
      88 VALIDATE-CUST   VALUE "VALIDCUST".
   05 MDM-CUSTOMER-ID    PIC 9(6).
   05 MDM-SEARCH-CRITERIA.
      10 SEARCH-NAME     PIC X(50).
      10 SEARCH-TAXID    PIC X(10).
      10 SEARCH-PHONE    PIC X(20).

01 MDM-CUSTOMER-RESPONSE.
   05 MDM-RETURN-CODE    PIC 99.
      88 SUCCESS         VALUE 00.
      88 NOT-FOUND       VALUE 01.
      88 MULTIPLE-FOUND  VALUE 02.
      88 INVALID-REQUEST VALUE 10.
   05 MDM-CUSTOMER-DATA.
      10 CUST-ID         PIC 9(6).
      10 CUST-NAME       PIC X(100).
      10 CUST-STATUS     PIC X.
      10 CUST-CREDIT-LIMIT PIC 9(9)V99.
      10 CUST-BALANCE    PIC S9(9)V99.
      10 CUST-TERMS      PIC X(6).
   05 MDM-ERROR-MESSAGE  PIC X(50).
```

**Code Table Lookup**:
```cobol
01 MDM-CODE-REQUEST.
   05 MDM-FUNCTION       PIC X(10) VALUE "GETCODE".
   05 MDM-TABLE-NAME     PIC X(20).
   05 MDM-CODE-VALUE     PIC X(10).

01 MDM-CODE-RESPONSE.
   05 MDM-RETURN-CODE    PIC 99.
   05 MDM-CODE-DESC      PIC X(50).
   05 MDM-CODE-STATUS    PIC X.
   05 MDM-EFFECTIVE-DATE PIC 9(8).
   05 MDM-EXPIRY-DATE    PIC 9(8).
```

### MDM-CACHE-001: Cache Synchronization

**Purpose**: Keep distributed caches synchronized

**Protocol**: Event-based cache invalidation
**Technology**: Redis pub/sub or similar

**Cache Events**:
```json
{
  "eventType": "CACHE_INVALIDATE",
  "entityType": "CUSTOMER",
  "entityId": "123456",
  "timestamp": "2024-03-15T14:30:00Z",
  "changeType": "UPDATE",
  "fields": ["creditLimit", "status"]
}
```

**Cache Strategy**:
- Customer data: 1 hour TTL
- Vendor data: 4 hour TTL
- Item data: 24 hour TTL
- Code tables: 24 hour TTL
- GL accounts: No expiry (event-based)

### MDM-AUDIT-001: Change Audit Trail

**Purpose**: Track all changes to master data

**Format**: Append-only log file
**Location**: `/audit/mdm/`
**Retention**: 7 years

**Audit Record Format**:
```
timestamp|user|program|entity|id|action|field|old_value|new_value
20240315143000|jsmith|custmaint|CUSTOMER|123456|UPDATE|credit_limit|50000|75000
20240315143001|jsmith|custmaint|CUSTOMER|123456|UPDATE|status|H|A
```

### MDM-REPORT-001: Master Data Extracts

**Purpose**: Provide master data for reporting and analytics

**Frequency**: Daily at 04:00
**Format**: Pipe-delimited text files
**Location**: `/extracts/mdm/`

**Customer Extract**:
```
HDR|CUSTOMER|20240315|040000|FULL
DTL|123456|ABC Company|A|50000|NET30|12-3456789|...
DTL|123457|XYZ Corp|A|100000|NET45|98-7654321|...
TRL|2534|20240315040100
```

## Data Quality Interfaces

### Duplicate Detection Service

**Internal Process**: Weekly duplicate check

```sql
-- Find potential customer duplicates
SELECT c1.customer_id, c2.customer_id, 
       c1.customer_name, c2.customer_name,
       similarity_score(c1.customer_name, c2.customer_name) as score,
       c1.tax_id = c2.tax_id as same_taxid
FROM customers c1
JOIN customers c2 ON c1.customer_id < c2.customer_id
WHERE similarity_score(c1.customer_name, c2.customer_name) > 0.85
   OR c1.tax_id = c2.tax_id
   OR soundex(c1.customer_name) = soundex(c2.customer_name);
```

### Address Validation Service

**External API**: USPS/Other provider

```http
POST /api/address/validate
{
  "address1": "123 Main St",
  "city": "New York",
  "state": "NY",
  "zip": "10001"
}

Response:
{
  "valid": true,
  "standardized": {
    "address1": "123 MAIN ST",
    "city": "NEW YORK",
    "state": "NY",
    "zip": "10001-1234",
    "county": "NEW YORK",
    "dpv": "Y"
  }
}
```

## Error Handling

### Error Codes

| Code | Description | Action |
|------|-------------|--------|
| 00 | Success | Continue |
| 01 | Not found | Return empty |
| 02 | Duplicate found | Return list |
| 10 | Invalid request | Check parameters |
| 20 | Unauthorized | Check permissions |
| 30 | Data quality error | Review and fix |
| 40 | Dependency exists | Cannot delete |
| 99 | System error | Retry/escalate |

### Error Recovery

**Batch Load Errors**:
```
Process:
1. Validate all records
2. Load valid records
3. Write errors to: {filename}.errors
4. Generate summary report
5. Email notification
```

**API Errors**:
- Return appropriate HTTP status
- Include error details in response
- Log all errors with context
- Alert on repeated failures

## Performance Optimization

### Batch Processing
- Process in chunks of 1000
- Commit every 100 records
- Parallel processing for different entities
- Index maintenance during off-hours

### Query Optimization
```cobol
*> Use specific field list instead of SELECT *
*> Include INDEX hints
*> Limit result sets
EXEC SQL
  SELECT customer_id, customer_name, status, credit_limit
  INTO :ws-customer-data
  FROM customers WITH (INDEX = idx_cust_active)
  WHERE customer_id = :search-id
    AND status = 'A'
END-EXEC
```

### Caching Strategy
- Pre-load frequently used codes
- Cache negative results
- Implement read-through cache
- Use write-through for updates