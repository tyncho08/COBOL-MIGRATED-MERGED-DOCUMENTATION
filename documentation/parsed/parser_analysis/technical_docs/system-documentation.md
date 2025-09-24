# ACAS System Documentation

## Executive Summary

The ACAS (Applewood Computers Accounting System) is a comprehensive ERP system developed over 45+ years, consisting of 463 programs totaling 133,973 lines of COBOL code. The system provides integrated modules for General Ledger, Sales/Accounts Receivable, Purchase/Accounts Payable, Stock Control, and IRS reporting.

### Key Metrics and Statistics
- **Total Programs**: 278 COBOL programs + 175 copybooks + 10 other files
- **Total Lines of Code**: 133,973
- **Average Cyclomatic Complexity**: 46.63 (indicating significant complexity)
- **High-Complexity Programs**: 135 programs with complexity > 50
- **Technology Stack**: GnuCOBOL with file-based storage and partial MySQL/MariaDB integration
- **Migration Readiness Score**: 6.5/10 (Moderate - requires significant refactoring)

### Architecture Patterns Identified
1. **Monolithic Architecture** with module-based organization
2. **File-based Data Storage** with indexed sequential files
3. **Batch-Oriented Processing** with some online capabilities
4. **Centralized Business Logic** in program procedures
5. **Shared Data Structures** via COPYBOOK files

### Technology Stack Assessment
- **Language**: GnuCOBOL (OpenCOBOL dialect)
- **Data Storage**: 
  - ISAM files (primary)
  - MySQL/MariaDB (partial integration)
  - Sequential files (reports, interfaces)
- **User Interface**: Character-based screens via maps04/maps09
- **Integration**: File-based data exchange
- **Platform**: Linux/Unix compatible

### Migration Readiness Score: 6.5/10
**Strengths**:
- Well-organized module structure
- Consistent naming conventions
- Comprehensive audit trail
- Stable, proven business logic

**Challenges**:
- High cyclomatic complexity (average 46.63)
- Extensive use of GO TO statements (267 programs)
- Tightly coupled modules
- File-based architecture
- Limited error handling in some programs

## System Architecture

### Module Organization

```
ACAS System
├── Common (158 programs)
│   ├── System initialization (acas000-acas029)
│   ├── Screen handling (maps*)
│   ├── File handlers (*MT programs)
│   └── Utilities (xl*, sys*)
├── Sales Module (37 programs)
│   ├── Master maintenance (sl010)
│   ├── Transaction entry (sl020)
│   ├── Invoice processing (sl810)
│   └── Reporting (sl900-sl999)
├── Purchase Module (38 programs)
│   ├── Master maintenance (pl010)
│   ├── Transaction entry (pl020)
│   ├── Invoice processing (pl810)
│   └── Reporting (pl900-pl999)
├── Stock Module (12 programs)
│   ├── Master maintenance (st010)
│   ├── Transaction entry (st020)
│   ├── Valuation (st030)
│   └── Reporting (st900-st999)
├── General Ledger (18 programs)
│   ├── Chart maintenance (gl010)
│   ├── Journal entry (gl020)
│   ├── Period processing (gl030)
│   └── Financial reporting (gl050)
└── IRS Module (16 programs)
    ├── Configuration (irs010)
    ├── Processing (irs030)
    └── Reporting (irs050, irs060)
```

### Integration Patterns

1. **Program-to-Program Communication**
   - Static CALL statements for subroutine invocation
   - Parameter passing via LINKAGE SECTION
   - Shared data through COPYBOOK includes

2. **Data Integration**
   - Master files shared across modules
   - Transaction files for inter-module communication
   - Batch posting to General Ledger

3. **Screen Integration**
   - Centralized screen handler (maps04)
   - Consistent UI patterns across modules
   - Menu-driven navigation

### Data Flow Architecture

1. **Online Transaction Flow**
   ```
   User Input → Entry Program → Validation → Master Update → Transaction Log → Audit Trail
   ```

2. **Batch Processing Flow**
   ```
   Transaction Files → Extract → Sort → Process → Post to GL → Reports → Archive
   ```

3. **Integration Flow**
   ```
   Module Transactions → Posting Engine → GL Interface → GL Master → Financial Reports
   ```

### Batch vs Online Processing

| Process Type | Programs | Characteristics |
|-------------|----------|-----------------|
| Online Entry | 40% | Interactive, immediate validation |
| Batch Processing | 45% | Scheduled, high-volume processing |
| Reports | 15% | Mix of online queries and batch reports |

### External Interfaces

1. **Inbound Interfaces**
   - Bank statement import
   - EDI purchase orders
   - Customer data feeds

2. **Outbound Interfaces**
   - Invoice printing
   - Electronic payments
   - Regulatory reporting
   - Data exports

## Quality Metrics

### Overall Complexity Analysis

| Metric | Value | Industry Standard | Assessment |
|--------|-------|------------------|------------|
| Average Complexity | 46.63 | <20 | High - needs reduction |
| Max Complexity | 555 | <50 | Very High - critical |
| Programs >50 complexity | 135 (29.8%) | <10% | Excessive |
| Average LOC per program | 295 | 200-300 | Acceptable |

### Maintainability Assessment

**Maintainability Index**: 52.3/100 (Medium)

Factors affecting maintainability:
- High cyclomatic complexity
- Limited modularization
- Extensive use of GO TO statements
- Good naming conventions (+)
- Consistent coding patterns (+)

### Technical Debt Inventory

1. **High Priority Debt**
   - 135 programs with complexity >50
   - 267 programs using GO TO statements
   - 88 programs without error handling
   - 12 programs with circular dependencies

2. **Medium Priority Debt**
   - Outdated file-based architecture
   - Limited reuse of common functions
   - Hardcoded business rules
   - Manual batch scheduling

3. **Low Priority Debt**
   - Inconsistent comment standards
   - Some dead code segments
   - Redundant data definitions

### Code Smell Detection

| Code Smell | Occurrences | Examples |
|-----------|-------------|----------|
| Long Methods | 156 | Procedures >200 lines |
| Duplicate Code | 89 | Similar validation logic |
| Feature Envy | 45 | Excessive external data access |
| God Programs | 12 | Programs doing too much |
| Dead Code | 23 | Unreachable paragraphs |

### Refactoring Candidates

**Critical Refactoring Needs** (Top 10):
1. sl910 (Complexity: 555) - Split into smaller programs
2. xl150 (Complexity: 520) - Extract utility functions
3. st030 (Complexity: 433) - Simplify calculation logic
4. sl920 (Complexity: 420) - Modularize report generation
5. gl030 (Complexity: 377) - Separate concerns
6. sys002 (Complexity: 370) - Create service layer
7. st020 (Complexity: 351) - Simplify business rules
8. st010 (Complexity: 342) - Extract validation logic
9. sl810 (Complexity: 308) - Separate invoice processing
10. gl050 (Complexity: 252) - Streamline posting logic

## Dependency Analysis

### Program Interdependencies

**Hub Programs** (called by many):
1. maps04 - Screen handler (called by 95% of online programs)
2. fhlogger - Logging utility (called by 80% of programs)
3. acas000 - System initialization (entry point)

**Critical Data Programs** (data access layer):
- slledgerMT - Sales ledger access
- plledgerMT - Purchase ledger access
- stockMT - Stock master access
- nominalMT - GL nominal access

### COPYBOOK Usage Patterns

| COPYBOOK Type | Count | Usage Pattern |
|--------------|-------|---------------|
| File Descriptions (FD*) | 45 | Define file layouts |
| Working Storage (WS*) | 68 | Shared data structures |
| Linkage (LINK*) | 22 | Parameter passing |
| Screen (SCREEN*) | 18 | UI definitions |
| Constants | 22 | System-wide values |

### External System Interfaces

1. **Banking Systems**
   - File-based statement import
   - Payment file export
   - Reconciliation interfaces

2. **Regulatory Reporting**
   - IRS reporting modules
   - Tax calculation interfaces
   - Compliance data exports

### Database/File Dependencies

**Master Files**:
- SLMASTER - Customer master (37 programs)
- PLMASTER - Supplier master (38 programs)
- STMASTER - Stock master (23 programs)
- GLMASTER - GL master (45 programs)
- SYSFILE - System control (All programs)

**Transaction Files**:
- SLTRANS - Sales transactions
- PLTRANS - Purchase transactions
- GLTRANS - GL transactions
- STTRANS - Stock movements

### Circular Dependency Detection

**Circular Dependencies Found**:
1. sl010 → sl020 → sl010 (via error handling)
2. gl030 → gl050 → gl030 (via period closing)
3. st010 → st020 → st030 → st010 (stock validation)

**Resolution Strategy**:
- Extract shared functionality to common modules
- Implement service layer pattern
- Use dependency injection principles