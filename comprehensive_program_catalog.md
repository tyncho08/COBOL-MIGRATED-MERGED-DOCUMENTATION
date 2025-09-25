# Comprehensive ACAS Program Catalog

## Executive Summary

The ACAS (Applewood Computers Accounting System) consists of 453 programs organized across 6 major modules, with an average complexity of 46.63. This catalog organizes all programs by module with their business purpose, technical complexity, and migration priority.

### Key Statistics
- **Total Programs**: 453 (101 main, 177 subprograms, 175 copybooks)
- **Total Lines of Code**: 133,973
- **High Complexity Programs**: 135 (complexity > 50)
- **SQL-Enabled Programs**: 45
- **Programs with GO TO**: 267

## Module Overview

| Module | Programs | Lines | Avg Complexity | Priority |
|--------|----------|-------|----------------|----------|
| Common | 157 | 67,839 | 59.30 | Critical |
| Sales | 37 | 21,412 | 113.35 | High |
| Purchase | 38 | 16,513 | 76.26 | High |
| General | 18 | 8,115 | 83.94 | Critical |
| IRS | 16 | 7,764 | 84.25 | Medium |
| Stock | 12 | 7,234 | 124.50 | High |

## System Core Programs (ACAS/Common)

### Critical System Programs

| Program | Purpose | Complexity | Lines | Priority | Refactoring Needs |
|---------|---------|------------|-------|----------|-------------------|
| ACAS | Main system entry point and menu dispatcher | 62 | 400 | Critical | Moderate - Remove GO TOs |
| acas000 | System file handler - central config/status management | 71 | 850 | Critical | High - Complex error handling |
| sys002 | System setup/maintenance | 370 | 2132 | Critical | **VERY HIGH** - Extreme complexity |
| maps04 | Screen handler/date utility (called 89 times) | 45 | 600 | Critical | Moderate - Most used program |
| fhlogger | File access logger (called 55 times) | 35 | 450 | Critical | Low - Well structured |

### Data Access Layer Programs

| Program | Purpose | Complexity | Lines | Priority | Refactoring Needs |
|---------|---------|------------|-------|----------|-------------------|
| systemMT | System data access | 84 | 1200 | Critical | High - SQL conversion needed |
| dfltMT | Default data handler | 90 | 1100 | High | High - Complex logic |
| finalMT | Final processing handler | 91 | 1150 | High | High - Complex logic |
| auditMT | Audit trail data access | 120 | 1500 | Critical | **HIGH** - Complex audit logic |
| nominalMT | GL nominal data access | 107 | 1400 | Critical | High - Core GL functionality |

### Utility Programs

| Program | Purpose | Complexity | Lines | Priority | Refactoring Needs |
|---------|---------|------------|-------|----------|-------------------|
| maps09 | Check digit utility | 25 | 300 | High | Low - Simple logic |
| maps01 | Menu handler utility | 40 | 500 | Medium | Moderate |
| xl150 | Period-end utilities | 520 | 1461 | High | **EXTREME** - Second most complex |
| acas-get-params | Parameter retrieval | 45 | 550 | High | Moderate |
| ACAS-Sysout | System output handler | 30 | 400 | Medium | Low |

## Sales Ledger Module

### High Priority Sales Programs

| Program | Purpose | Complexity | Lines | Priority | Refactoring Needs |
|---------|---------|------------|-------|----------|-------------------|
| sl910 | Sales reports generation | 555 | 2312 | Critical | **EXTREME** - Most complex program |
| sl920 | Sales analysis report | 420 | 1665 | High | **VERY HIGH** - Complex reporting |
| sl810 | Auto invoice generation | 308 | 1314 | Critical | **VERY HIGH** - Complex automation |
| sl010 | Customer maintenance | 288 | 1502 | High | **VERY HIGH** - Core master file |
| sl020 | Order entry | 133 | 1100 | Critical | High - Core transaction |
| sl050 | Invoice generation | 66 | 850 | Critical | Moderate |
| sl060 | Cash receipts | 136 | 1200 | Critical | High - Payment processing |

### Sales Data Access Programs

| Program | Purpose | Complexity | Lines | Priority | Refactoring Needs |
|---------|---------|------------|-------|----------|-------------------|
| salesMT | Sales data access | 135 | 1600 | Critical | High - Master data |
| slpostingMT | SL posting data access | 116 | 1400 | Critical | High - Transaction data |
| slinvoiceMT | SL invoice data access | 191 | 2305 | Critical | **VERY HIGH** - Complex invoice handling |
| slautogenMT | SL autogen data access | 189 | 2359 | High | **VERY HIGH** - Complex automation |

### Sales Support Programs

| Program | Purpose | Complexity | Lines | Priority | Refactoring Needs |
|---------|---------|------------|-------|----------|-------------------|
| sl100 | Credit control | 73 | 900 | High | Moderate |
| sl110 | Customer statements | 112 | 1200 | High | High - Statement generation |
| sl120 | Aged debtors | 136 | 1300 | High | High - Aging logic |
| sl900 | SL month-end | 64 | 800 | Critical | Moderate - Period processing |

## Purchase Ledger Module

### Core Purchase Programs

| Program | Purpose | Complexity | Lines | Priority | Refactoring Needs |
|---------|---------|------------|-------|----------|-------------------|
| pl010 | Supplier maintenance | 205 | 1077 | High | **VERY HIGH** - Master file maintenance |
| pl020 | Purchase order entry | 162 | 1200 | Critical | High - Order processing |
| pl040 | Invoice entry | 95 | 1000 | Critical | High - Invoice matching |
| pl050 | Payment selection | 52 | 700 | High | Moderate |
| pl055 | Check/payment run | 75 | 900 | Critical | High - Payment processing |
| pl080 | Aged creditors | 104 | 1100 | High | High - Aging logic |

### Purchase Data Access Programs

| Program | Purpose | Complexity | Lines | Priority | Refactoring Needs |
|---------|---------|------------|-------|----------|-------------------|
| purchMT | Purchase data access | 136 | 1500 | Critical | High - Master data |
| plpostingMT | PL posting data access | 115 | 1300 | Critical | High - Transaction data |
| plinvoiceMT | PL invoice data access | 191 | 2275 | High | **VERY HIGH** - Complex invoice logic |
| plautogenMT | PL autogen data access | 189 | 2348 | Medium | **VERY HIGH** - Complex automation |

## General Ledger Module

### Critical GL Programs

| Program | Purpose | Complexity | Lines | Priority | Refactoring Needs |
|---------|---------|------------|-------|----------|-------------------|
| gl030 | Period-end processing | 377 | 1835 | Critical | **VERY HIGH** - Complex period logic |
| gl050 | Trial balance | 252 | 1065 | High | **VERY HIGH** - Complex reporting |
| gl020 | Journal entry | 96 | 1000 | Critical | High - Core transaction |
| gl060 | Balance sheet | 52 | 700 | High | Moderate |
| gl070 | P&L statement | 69 | 850 | High | Moderate |
| general | GL menu system | 103 | 1100 | High | High - Menu complexity |

### GL Data Access Programs

| Program | Purpose | Complexity | Lines | Priority | Refactoring Needs |
|---------|---------|------------|-------|----------|-------------------|
| glpostingMT | GL posting data access | 116 | 1400 | Critical | High - Transaction data |
| glbatchMT | GL batch data access | 114 | 1300 | High | High - Batch processing |

## Stock Control Module

### High Complexity Stock Programs

| Program | Purpose | Complexity | Lines | Priority | Refactoring Needs |
|---------|---------|------------|-------|----------|-------------------|
| st030 | Stock valuation | 433 | 1765 | Critical | **EXTREME** - Third most complex |
| st020 | Stock movements | 351 | 1622 | Critical | **VERY HIGH** - Movement tracking |
| st010 | Stock item maintenance | 342 | 1639 | High | **VERY HIGH** - Master maintenance |
| stock | Stock menu system | 99 | 1000 | High | High - Menu complexity |
| stockMT | Stock data access | 120 | 1400 | Critical | High - Core data access |

### Stock Support Programs

| Program | Purpose | Complexity | Lines | Priority | Refactoring Needs |
|---------|---------|------------|-------|----------|-------------------|
| st050 | Stock reports | 51 | 700 | High | Moderate |
| st060 | Reorder report | 85 | 950 | High | High - Reorder logic |
| st040 | Stock inquiry | 64 | 800 | Medium | Moderate |

## IRS (Incomplete Records System) Module

### Core IRS Programs

| Program | Purpose | Complexity | Lines | Priority | Refactoring Needs |
|---------|---------|------------|-------|----------|-------------------|
| irs030 | Posting to nominal | 225 | 1115 | Critical | **VERY HIGH** - Complex posting |
| irs060 | Analysis reports | 224 | 1146 | Medium | **VERY HIGH** - Complex analysis |
| irs050 | Reports menu | 195 | 1010 | High | **VERY HIGH** - Report generation |
| irs020 | Transaction entry | 87 | 900 | Critical | High - Core transaction |
| irs010 | IRS setup | 167 | 1200 | High | High - System setup |
| irs | IRS menu system | 116 | 1100 | High | High - Menu complexity |

### IRS Data Access Programs

| Program | Purpose | Complexity | Lines | Priority | Refactoring Needs |
|---------|---------|------------|-------|----------|-------------------|
| irsnominalMT | IRS nominal data access | 153 | 1600 | Critical | **VERY HIGH** - Complex nominal |
| irspostingMT | IRS posting data access | 111 | 1300 | Critical | High - Posting logic |
| irsdfltMT | IRS default data access | 93 | 1100 | High | High - Default handling |

## Most Complex Programs Requiring Immediate Refactoring

### Top 20 Programs by Complexity

| Rank | Program | Module | Complexity | Lines | Business Impact | Refactoring Strategy |
|------|---------|--------|------------|-------|-----------------|---------------------|
| 1 | sl910 | Sales | 555 | 2312 | Sales reporting | Split into smaller report modules |
| 2 | xl150 | Common | 520 | 1461 | Period utilities | Decompose into specific utilities |
| 3 | st030 | Stock | 433 | 1765 | Stock valuation | Separate valuation methods |
| 4 | sl920 | Sales | 420 | 1665 | Sales analysis | Modularize analysis types |
| 5 | gl030 | GL | 377 | 1835 | GL period-end | Split period processing steps |
| 6 | sys002 | System | 370 | 2132 | System setup | Break into setup components |
| 7 | st020 | Stock | 351 | 1622 | Stock movements | Separate movement types |
| 8 | st010 | Stock | 342 | 1639 | Stock maintenance | Split CRUD operations |
| 9 | sl810 | Sales | 308 | 1314 | Auto invoicing | Modularize invoice generation |
| 10 | pl810 | Purchase | 308 | 1314 | Auto invoicing | Modularize invoice generation |
| 11 | sl010 | Sales | 288 | 1502 | Customer maint | Separate validation/processing |
| 12 | gl050 | GL | 252 | 1065 | Trial balance | Simplify balance calculations |
| 13 | irs030 | IRS | 225 | 1115 | Posting to nominal | Break down posting logic |
| 14 | irs060 | IRS | 224 | 1146 | Analysis reports | Separate report types |
| 15 | pl010 | Purchase | 205 | 1077 | Supplier maint | Separate validation/processing |
| 16 | irs050 | IRS | 195 | 1010 | Reports menu | Simplify menu structure |
| 17 | slinvoiceMT | Common | 191 | 2305 | Invoice data access | Optimize SQL operations |
| 18 | plinvoiceMT | Common | 191 | 2275 | Invoice data access | Optimize SQL operations |
| 19 | slautogenMT | Common | 189 | 2359 | Auto-gen data | Streamline generation logic |
| 20 | plautogenMT | Common | 189 | 2348 | Auto-gen data | Streamline generation logic |

## Migration Strategy by Priority

### Phase 1: Critical Foundation (Months 1-3)
1. **System Core**: ACAS, acas000, systemMT, dfltMT, finalMT
2. **Screen/Utility**: maps04, maps09, fhlogger
3. **Audit Trail**: auditMT

### Phase 2: IRS Module - Pilot (Months 4-6)
- Most isolated module with moderate complexity
- Good test case for migration approach
- Programs: irs, irs010, irs020, irs030, irsnominalMT

### Phase 3: Stock Module (Months 7-9)
- Clear boundaries, high business value
- Focus on refactoring st030, st020, st010 first
- Programs: stock, st010-st060, stockMT, valueMT

### Phase 4: Purchase Module (Months 10-12)
- Moderate complexity, clear workflows
- Refactor pl010, pl020 first
- Programs: purchase, pl010-pl190, purchMT, plpostingMT

### Phase 5: Sales Module (Months 13-16)
- Highest complexity, most dependencies
- Requires extensive refactoring of sl910, sl920, sl810
- Programs: sales, sl010-sl970, salesMT, slpostingMT

### Phase 6: General Ledger (Months 17-20)
- Most integrated, highest risk
- Careful coordination with all modules
- Programs: general, gl020-gl120, nominalMT, glpostingMT

## Key Refactoring Priorities

### Immediate Actions Required
1. **Extreme Complexity Programs** (>400): sl910, xl150, st030, sl920
2. **Critical Path Programs**: gl030, sys002, st020, st010
3. **High-Use Utilities**: maps04 (89 calls), fhlogger (55 calls)

### Technical Debt Reduction
1. **GO TO Elimination**: 267 programs need structured programming
2. **Error Handling**: Add to 218 programs lacking proper error handling
3. **SQL Standardization**: Consolidate 45 SQL programs to use consistent patterns
4. **Dead Code Removal**: Clean up 287 programs with possible dead code

### Data Access Modernization
1. Standardize all MT programs to use consistent DAL patterns
2. Convert file-based access to SQL where appropriate
3. Implement proper transaction management
4. Add comprehensive logging and error handling

## Risk Assessment

### High Risk Areas
1. **sl910**: Most complex program, critical for sales reporting
2. **xl150**: Period-end utilities affect all modules
3. **gl030**: GL period-end is business critical
4. **System setup**: sys002 complexity poses migration risk

### Mitigation Strategies
1. Create comprehensive test suites before refactoring
2. Implement parallel run capabilities
3. Phase migrations during low-activity periods
4. Maintain rollback procedures for each phase
5. Document all business rules during refactoring

---
*This catalog provides a comprehensive view of the ACAS system for migration planning. Regular updates should be made as refactoring progresses.*