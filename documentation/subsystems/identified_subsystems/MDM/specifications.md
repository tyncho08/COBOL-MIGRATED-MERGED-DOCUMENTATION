# Subsystem: MDM - Master Data Management

## Executive Summary

**Purpose**: The MDM subsystem serves as the central repository and single source of truth for all reference and master data across the ACAS system. It manages customers, suppliers, products, chart of accounts, and all code tables, ensuring data consistency, quality, and controlled access across all business subsystems.

**Business Value**: Provides data integrity and consistency across the enterprise, reduces data duplication, ensures compliance with data governance policies, and enables efficient master data maintenance. Critical for accurate transaction processing and reporting.

**Key Users**:
- Master data administrators (data maintenance)
- All business users (indirect via transactions)
- System administrators (code table maintenance)
- Compliance team (data governance)

**Criticality**: HIGH - Foundation for all business transactions

## Functional Capabilities

### Core Functions

1. **Customer Master Management**
   - Description: Maintain complete customer information including legal entity data, addresses, contacts, and classifications
   - Business Rules: Unique ID generation, duplicate prevention, mandatory fields validation, status lifecycle
   - Triggers: New customer requests, updates from sales/finance
   - Outcomes: Validated customer records available system-wide

2. **Supplier Master Management**
   - Description: Maintain vendor information, payment terms, tax details, and compliance documentation
   - Business Rules: W-9/tax validation, bank account verification, approval workflow
   - Triggers: New vendor requests, procurement needs
   - Outcomes: Approved vendors for purchasing

3. **Product/Item Master Management**
   - Description: Maintain product catalog with descriptions, units of measure, categories, and attributes
   - Business Rules: SKU generation, category validation, unit conversions, status management
   - Triggers: New product introduction, engineering changes
   - Outcomes: Consistent product data across all modules

4. **Chart of Accounts Management**
   - Description: Maintain general ledger account structure with hierarchies and attributes
   - Business Rules: Account number format, posting controls, department validation
   - Triggers: Financial structure changes, new departments
   - Outcomes: Valid GL accounts for posting

5. **Code Table Management**
   - Description: Maintain system-wide reference codes including countries, currencies, terms, etc.
   - Business Rules: Code uniqueness, description standards, effective dating
   - Triggers: Regulatory changes, business expansion
   - Outcomes: Standardized codes for data entry

6. **Data Quality Management**
   - Description: Monitor and improve master data quality through validations and cleansing
   - Business Rules: Completeness checks, format validation, duplicate detection
   - Triggers: Scheduled quality checks, exception reports
   - Outcomes: High-quality master data

### Business Processes Supported

- **Customer Onboarding**: New customer setup with credit approval
- **Vendor Onboarding**: New supplier approval and setup
- **Product Launch**: New item introduction workflow
- **Data Governance**: Master data change control
- **Compliance Management**: Tax ID validation, regulatory compliance
- **Data Quality**: Ongoing data cleansing and enrichment

## Data Domain

### Owned Entities

**Customer Master (CUSTMAST)**
- Key Attributes: Customer ID, legal name, DBA, tax ID, status
- Business Identifiers: Customer ID (system generated)
- Lifecycle: Prospect → Approved → Active → Inactive → Archived

**Supplier Master (VENDMAST)**
- Key Attributes: Vendor ID, legal name, tax ID, payment terms
- Business Identifiers: Vendor ID (system generated)
- Lifecycle: Submitted → Approved → Active → Inactive → Archived

**Item Master (ITEMMAST)**
- Key Attributes: Item code, description, UOM, category, status
- Business Identifiers: Item code (user defined + check digit)
- Lifecycle: Draft → Approved → Active → Obsolete → Archived

**GL Account Master (GLMAST)**
- Key Attributes: Account number, description, type, normal balance
- Business Identifiers: Account number (structured)
- Lifecycle: Created → Active → Inactive → Archived

**Code Tables (Multiple)**
- Key Attributes: Table name, code value, description, status
- Business Identifiers: Table + code combination
- Lifecycle: Active → Inactive

**Address Master (ADDRMAST)**
- Key Attributes: Address ID, type, lines, city, state, postal
- Business Identifiers: Address ID + entity type + entity ID
- Lifecycle: Created → Verified → Active → Inactive

### Referenced Entities

None - MDM is the source system for master data

## Interface Contracts

### Inbound Interfaces

| Interface ID | Source | Data Type | Frequency | Business Purpose |
|-------------|--------|-----------|-----------|------------------|
| EXT-MDM-001 | External sources | CSV/Excel | On-demand | Bulk data loads |
| USER-MDM-001 | User screens | Online entry | Real-time | Manual maintenance |
| API-MDM-001 | External systems | REST/JSON | Real-time | Third-party updates |

### Outbound Interfaces

| Interface ID | Target | Data Type | Frequency | Business Purpose |
|-------------|--------|-----------|-----------|------------------|
| MDM-ALL-001 | All subsystems | Database/API | Real-time | Master data access |
| MDM-CACHE-001 | Cache layer | Memory | Real-time | Performance optimization |
| MDM-AUDIT-001 | Audit system | Change log | Real-time | Change tracking |
| MDM-REPORT-001 | Reporting | Extract files | Daily | Master data reports |

### Internal APIs/Services

**Get Customer**: [Customer ID] → [Customer Record]
- Purpose: Retrieve customer master data
- Validation: Valid ID format, exists
- Error Handling: Return empty with error code

**Validate Code**: [Table, Code] → [Valid/Invalid, Description]
- Purpose: Validate and retrieve code table entries
- Validation: Table exists, code active
- Error Handling: Return invalid flag

**Search Master**: [Entity Type, Criteria] → [Result Set]
- Purpose: Search master data by various criteria
- Validation: Valid entity type, search parameters
- Error Handling: Empty result set

**Update Master**: [Entity, Changes] → [Success/Failure]
- Purpose: Update master data with change tracking
- Validation: Authorization, data quality rules
- Error Handling: Rollback on failure

## Business Rules Engine

### Validation Rules

- **VR001**: Tax ID must be valid format and not duplicate
- **VR002**: Customer/vendor names must be unique within type
- **VR003**: Addresses must be validated against postal database
- **VR004**: GL accounts must follow chart structure
- **VR005**: Item codes must pass check digit validation
- **VR006**: Effective dates cannot be retroactive
- **VR007**: Inactive records cannot be referenced

### Data Quality Rules

- **DQ001**: Customer must have at least one active address
- **DQ002**: Vendor must have valid tax documentation
- **DQ003**: Items must have complete descriptions
- **DQ004**: Phone numbers must be formatted consistently
- **DQ005**: Email addresses must be valid format

### Workflow Rules

- **WF001**: New customers require credit approval
- **WF002**: Vendor changes require purchasing approval
- **WF003**: GL account changes require controller approval
- **WF004**: Inactivation requires dependency check

## Operational Characteristics

### Processing Patterns

**Real-time Processing**:
- Master data queries (continuous)
- Updates via maintenance screens
- API-based integrations
- Cache refresh triggers

**Batch Processing**:
- Daily quality reports (06:00)
- Weekly duplicate checks (Sunday)
- Monthly data governance reports
- Quarterly data archival

**Peak Periods**:
- Business hours: Heavy read access
- Month-end: GL account queries
- Year-end: Tax ID validations

### Data Volumes

- Customers: 50,000+ active records
- Suppliers: 10,000+ active records
- Items: 25,000+ active SKUs
- GL Accounts: 2,000+ accounts
- Addresses: 200,000+ records
- Code entries: 5,000+ across 50 tables

## Dependencies

### Upstream Dependencies
None - Master data is source

### Downstream Dependencies
- All business subsystems depend on MDM
- Reporting systems for extracts
- Caching layer for performance

### External Dependencies
- Address validation service
- Tax ID verification service
- D&B for business verification

## Quality Attributes

### Performance Requirements
- Response Time: <100ms for single record
- Throughput: 1000 queries/second
- Cache hit rate: >95%

### Reliability Requirements
- Availability: 99.9% (critical service)
- Recovery Time: <30 minutes
- Recovery Point: Real-time replication

### Compliance Requirements
- Data privacy: PII protection
- Audit trail: All changes logged
- Data retention: 7 years + current

## Evolution Potential

### Enhancement Opportunities
- Machine learning for duplicate detection
- Automated data enrichment
- Blockchain for data provenance
- Graph database for relationships
- Natural language search

### Modernization Candidates
- Move to microservices architecture
- Implement master data hub pattern
- Add data quality workflows
- Enable self-service maintenance
- Real-time synchronization

### Known Limitations
- Limited hierarchy support
- No versioning capability
- Basic workflow features
- Manual quality processes
- Single-language support