# ACAS Program Catalog

## Overview
This catalog provides an alphabetical listing of all COBOL programs in the ACAS system, organized by module with business function mapping and key dependencies.

## Program Organization

### Naming Conventions
- **ACAS**: System-wide utilities and initialization
- **SL**: Sales Ledger programs
- **PL**: Purchase Ledger programs  
- **ST**: Stock Control programs
- **GL**: General Ledger programs
- **IRS**: Incomplete Records System programs
- **XL**: Cross-ledger utilities
- **MAPS**: Screen and utility handlers
- **SYS**: System maintenance utilities

### Program Numbering Patterns
- **000-009**: System initialization and menus
- **010-019**: Master file maintenance
- **020-049**: Transaction entry
- **050-099**: Processing and updates
- **100-199**: Inquiries and displays
- **800-899**: Batch processing
- **900-999**: Reports and period-end

## Alphabetical Program Listing

### A - ACAS System Programs

| Program | Description | Module | Type | Dependencies | Priority |
|---------|-------------|--------|------|--------------|----------|
| ACAS | Main system entry point | System | Main | acas000, maps04 | Critical |
| acas000 | System initialization | System | Sub | systemMT, dfltMT, finalMT | Critical |
| acas001 | System parameter maintenance | System | Sub | systemMT | High |
| acas002 | Company details setup | System | Sub | systemMT | High |
| acas003 | User access control | System | Sub | systemMT | Medium |
| acas004 | Sales autogen control | System | Sub | slautogenMT | Medium |
| acas005 | GL nominal control | System | Sub | nominalMT | High |
| acas006 | GL posting control | System | Sub | glpostingMT | High |
| acas007 | GL batch control | System | Sub | glbatchMT | High |
| acas008 | SL posting control | System | Sub | slpostingMT | High |
| acas010 | Audit trail handler | System | Sub | auditMT | Critical |
| acas011 | Stock file handler | System | Sub | stockMT | High |
| acas012 | Sales file handler | System | Sub | salesMT | High |
| acas013 | Stock value handler | System | Sub | valueMT | High |
| acas014 | Delivery file handler | System | Sub | deliveryMT | Medium |
| acas015 | Analysis file handler | System | Sub | analMT | Medium |
| acas016 | Price list handler | System | Sub | priceMT | Medium |
| acas017 | Order file handler | System | Sub | orderMT | Medium |
| acas018 | Back order handler | System | Sub | backorderMT | Medium |
| acas019 | Tax file handler | System | Sub | taxMT | High |
| acas020 | Currency handler | System | Sub | currencyMT | Low |
| acas021 | Period control handler | System | Sub | periodMT | High |
| acas022 | Purchase file handler | System | Sub | purchMT | High |
| acas023 | Report definition handler | System | Sub | reportMT | Low |
| acas024 | Security handler | System | Sub | securityMT | High |
| acas025 | Message file handler | System | Sub | messageMT | Low |
| acas026 | Parameter handler | System | Sub | paramMT | High |
| acas027 | Backup control handler | System | Sub | backupMT | High |
| acas028 | Archive handler | System | Sub | archiveMT | Medium |
| acas029 | Interface handler | System | Sub | interfaceMT | Medium |
| analMT | Analysis data access | System | DAL | Database/File | High |
| auditMT | Audit trail data access | System | DAL | Database/File | Critical |

### D - Data Access Layer Programs

| Program | Description | Module | Type | Dependencies | Priority |
|---------|-------------|--------|------|--------------|----------|
| deliveryMT | Delivery address data access | Common | DAL | Database/File | Medium |
| dfltMT | Default data handler | Common | DAL | Database/File | High |

### F - File Handler Programs

| Program | Description | Module | Type | Dependencies | Priority |
|---------|-------------|--------|------|--------------|----------|
| fhlogger | File access logger | Common | Sub | Log file | Medium |
| finalMT | Final processing handler | Common | DAL | Database/File | High |

### G - General Ledger Programs

| Program | Description | Module | Type | Dependencies | Priority |
|---------|-------------|--------|------|--------------|----------|
| general | GL menu system | GL | Main | maps04, sys002, acas000 | High |
| gl010 | GL master maintenance | GL | Sub | nominalMT, maps04 | High |
| gl020 | Journal entry | GL | Sub | glpostingMT, maps04 | Critical |
| gl030 | Period-end processing | GL | Sub | nominalMT, glpostingMT | Critical |
| gl040 | Final accounts (not active) | GL | Sub | - | Low |
| gl050 | Trial balance | GL | Sub | nominalMT, glpostingMT | High |
| gl060 | Balance sheet | GL | Sub | nominalMT | High |
| gl070 | P&L statement | GL | Sub | nominalMT | High |
| gl080 | GL inquiry | GL | Sub | nominalMT | Medium |
| gl090 | Account analysis | GL | Sub | nominalMT, glpostingMT | Medium |
| gl100 | Budget maintenance | GL | Sub | budgetMT | Medium |
| gl110 | Budget reports | GL | Sub | budgetMT, nominalMT | Medium |
| gl120 | Comparative reports | GL | Sub | nominalMT, archiveMT | Medium |
| gl130 | Consolidation (not active) | GL | Sub | - | Low |
| gl900 | GL month-end | GL | Sub | Various | Critical |
| gl910 | GL year-end | GL | Sub | Various | Critical |
| gl950 | Financial reports | GL | Sub | nominalMT | High |
| glbatchMT | GL batch data access | GL | DAL | Database/File | High |
| glpostingMT | GL posting data access | GL | DAL | Database/File | Critical |

### I - IRS Programs

| Program | Description | Module | Type | Dependencies | Priority |
|---------|-------------|--------|------|--------------|----------|
| irs | IRS menu system | IRS | Main | maps04, sys002, acas000 | High |
| irs010 | IRS setup | IRS | Sub | irsdfltMT, maps04 | High |
| irs020 | Transaction entry | IRS | Sub | postingMT, maps04 | Critical |
| irs030 | Posting to nominal | IRS | Sub | irsnominalMT, postingMT | Critical |
| irs040 | Bank reconciliation | IRS | Sub | postingMT, maps04 | High |
| irs050 | Reports menu | IRS | Sub | Various | High |
| irs060 | Analysis reports | IRS | Sub | irsnominalMT, postingMT | Medium |
| irs065 | Sort utility | IRS | Sub | Sort file | Medium |
| irs070 | Trial balance | IRS | Sub | irsnominalMT | High |
| irs080 | Final accounts | IRS | Sub | irsnominalMT, irsfinalMT | High |
| irs085 | Sort utility | IRS | Sub | Sort file | Medium |
| irs090 | Account inquiry | IRS | Sub | irsnominalMT | Medium |
| irsubp | Posting processor | IRS | Sub | postingMT | High |
| irsdfltMT | IRS default data access | IRS | DAL | Database/File | High |
| irsfinalMT | IRS final accounts data | IRS | DAL | Database/File | High |
| irsnominalMT | IRS nominal data access | IRS | DAL | Database/File | Critical |

### M - Maps and Screen Handler Programs

| Program | Description | Module | Type | Dependencies | Priority |
|---------|-------------|--------|------|--------------|----------|
| maps04 | Screen handler/date utility | Common | Sub | Screen I/O | Critical |
| maps09 | Check digit utility | Common | Sub | Calculation | High |

### N - Nominal/GL Data Access

| Program | Description | Module | Type | Dependencies | Priority |
|---------|-------------|--------|------|--------------|----------|
| nominalMT | GL nominal data access | GL | DAL | Database/File | Critical |

### P - Purchase Ledger Programs

| Program | Description | Module | Type | Dependencies | Priority |
|---------|-------------|--------|------|--------------|----------|
| purchase | PL menu system | Purchase | Main | maps04, sys002, acas000 | High |
| pl010 | Supplier maintenance | Purchase | Sub | plledgerMT, maps04, maps09 | High |
| pl020 | Purchase order entry | Purchase | Sub | plpostingMT, stockMT | Critical |
| pl030 | Goods receipt | Purchase | Sub | plpostingMT, stockMT | High |
| pl040 | Invoice entry | Purchase | Sub | plpostingMT, nominalMT | Critical |
| pl050 | Payment selection | Purchase | Sub | plledgerMT, plpostingMT | High |
| pl055 | Check/payment run | Purchase | Sub | plledgerMT, bankMT | Critical |
| pl060 | Supplier inquiry | Purchase | Sub | plledgerMT | Medium |
| pl070 | Order inquiry | Purchase | Sub | plpostingMT | Medium |
| pl080 | Aged creditors | Purchase | Sub | plledgerMT, plpostingMT | High |
| pl090 | Purchase analysis | Purchase | Sub | plpostingMT | Medium |
| pl095 | Price variance report | Purchase | Sub | plpostingMT, stockMT | Medium |
| pl100 | Supplier statements | Purchase | Sub | plledgerMT, plpostingMT | Medium |
| pl810 | Auto invoice matching | Purchase | Sub | Various | High |
| pl900 | PL month-end | Purchase | Sub | Various | High |
| pl910 | Supplier reports | Purchase | Sub | plledgerMT | Medium |
| plautogenMT | PL autogen data access | Purchase | DAL | Database/File | Medium |
| plinvoiceMT | PL invoice data access | Purchase | DAL | Database/File | High |
| plledgerMT | PL ledger data access | Purchase | DAL | Database/File | Critical |
| plpostingMT | PL posting data access | Purchase | DAL | Database/File | Critical |

### S - Sales and Stock Programs

| Program | Description | Module | Type | Dependencies | Priority |
|---------|-------------|--------|------|--------------|----------|
| sales | SL menu system | Sales | Main | maps04, sys002, acas000 | High |
| salesMT | Sales data access | Sales | DAL | Database/File | Critical |
| sl000 | Sales initialization | Sales | Sub | Various | High |
| sl010 | Customer maintenance | Sales | Sub | salesMT, deliveryMT, maps04 | High |
| sl020 | Order entry | Sales | Sub | slpostingMT, stockMT | Critical |
| sl050 | Invoice generation | Sales | Sub | slpostingMT, salesMT | Critical |
| sl055 | Credit note entry | Sales | Sub | slpostingMT, salesMT | High |
| sl060 | Cash receipts | Sales | Sub | salesMT, slpostingMT | Critical |
| sl070 | Analysis code setup | Sales | Sub | analMT | Medium |
| sl080 | Customer inquiry | Sales | Sub | salesMT | Medium |
| sl085 | Order inquiry | Sales | Sub | slpostingMT | Medium |
| sl090 | Invoice inquiry | Sales | Sub | slpostingMT | Medium |
| sl095 | Product inquiry | Sales | Sub | stockMT | Medium |
| sl100 | Credit control | Sales | Sub | salesMT, slpostingMT | High |
| sl110 | Customer statements | Sales | Sub | salesMT, slpostingMT | High |
| sl115 | Statement sort | Sales | Sub | Sort utility | Medium |
| sl120 | Aged debtors | Sales | Sub | salesMT, slpostingMT | High |
| sl130 | Sales analysis | Sales | Sub | slpostingMT, analMT | Medium |
| sl140 | Commission calculation | Sales | Sub | slpostingMT, salesMT | Medium |
| sl160 | Mailing labels | Sales | Sub | salesMT, deliveryMT | Low |
| sl165 | Label sort | Sales | Sub | Sort utility | Low |
| sl170 | Price list maintenance | Sales | Sub | priceMT, stockMT | Medium |
| sl180 | Dunning letters | Sales | Sub | salesMT, slpostingMT | Medium |
| sl190 | Sales reports menu | Sales | Sub | Various | Medium |
| sl200 | Custom reports | Sales | Sub | Various | Low |
| sl800 | Daily processing | Sales | Sub | Various | High |
| sl810 | Auto invoice generation | Sales | Sub | Various | Critical |
| sl900 | SL month-end | Sales | Sub | Various | Critical |
| sl910 | Detailed sales report | Sales | Sub | slpostingMT | High |
| sl920 | Sales analysis report | Sales | Sub | slpostingMT, analMT | High |
| sl930 | Export processing | Sales | Sub | slpostingMT | Medium |
| sl940 | Territory analysis | Sales | Sub | slpostingMT | Low |
| sl950 | Product analysis | Sales | Sub | slpostingMT, stockMT | Medium |
| slautogenMT | SL autogen data access | Sales | DAL | Database/File | High |
| slinvoiceMT | SL invoice data access | Sales | DAL | Database/File | Critical |
| slledgerMT | SL ledger data access | Sales | DAL | Database/File | Critical |
| slpostingMT | SL posting data access | Sales | DAL | Database/File | Critical |
| st010 | Stock item maintenance | Stock | Sub | stockMT, analMT, maps04 | High |
| st020 | Stock movements | Stock | Sub | stockMT, valueMT | Critical |
| st030 | Stock valuation | Stock | Sub | stockMT, valueMT | Critical |
| st040 | Stock inquiry | Stock | Sub | stockMT | Medium |
| st050 | Stock reports | Stock | Sub | stockMT, valueMT | High |
| st060 | Reorder report | Stock | Sub | stockMT | High |
| st070 | Stock take entry | Stock | Sub | stockMT | High |
| st080 | Variance report | Stock | Sub | stockMT, valueMT | Medium |
| st090 | Movement history | Stock | Sub | stockMT | Medium |
| st100 | ABC analysis | Stock | Sub | stockMT, valueMT | Low |
| st800 | Stock update batch | Stock | Sub | stockMT | High |
| st900 | Stock month-end | Stock | Sub | Various | High |
| stockMT | Stock data access | Stock | DAL | Database/File | Critical |

### System Programs

| Program | Description | Module | Type | Dependencies | Priority |
|---------|-------------|--------|------|--------------|----------|
| sys002 | System setup/maintenance | System | Sub | systemMT, maps04 | Critical |
| sys003 | User maintenance | System | Sub | userMT | High |
| sys004 | Security setup | System | Sub | securityMT | High |
| sys4MT | System control data | System | DAL | Database/File | Critical |
| systemMT | System data access | System | DAL | Database/File | Critical |

### V - Value and Other Programs

| Program | Description | Module | Type | Dependencies | Priority |
|---------|-------------|--------|------|--------------|----------|
| valueMT | Stock value data access | Stock | DAL | Database/File | High |

### X - Cross-Ledger Programs

| Program | Description | Module | Type | Dependencies | Priority |
|---------|-------------|--------|------|--------------|----------|
| xl150 | Period-end utilities | Common | Sub | Various | High |

## Dependencies Summary

### Critical Dependencies (Called by many programs)
1. **maps04** - Screen handler used by 95% of interactive programs
2. **fhlogger** - File access logging used by 80% of programs  
3. **systemMT** - System parameters accessed by all modules
4. **acas000** - System initialization required at startup

### Data Access Layer (DAL) Programs
All programs ending in 'MT' serve as the data access layer, providing:
- Abstraction between business logic and data storage
- Support for both file-based and database storage
- Centralized data validation
- Consistent error handling

### Module Entry Points
Each major module has a menu program that serves as the entry point:
- **ACAS** - Main system menu
- **sales** - Sales Ledger menu
- **purchase** - Purchase Ledger menu  
- **general** - General Ledger menu
- **irs** - IRS menu

### Maintenance Priority Levels
- **Critical**: Core functionality, system will not operate without these
- **High**: Important business functions, required for normal operations
- **Medium**: Supporting functions, enhance usability but not essential
- **Low**: Optional features, nice-to-have functionality

## Migration Considerations

### Recommended Migration Sequence
1. **Phase 1**: System utilities and DAL layer (foundation)
2. **Phase 2**: IRS module (most isolated, least complex)
3. **Phase 3**: Stock module (clear boundaries)
4. **Phase 4**: Purchase module (moderate complexity)
5. **Phase 5**: Sales module (high complexity, many dependencies)
6. **Phase 6**: General Ledger (most integrated, highest risk)

### High-Risk Programs
Programs with complexity scores >300 require significant refactoring:
- sl910 (555) - Sales reports
- xl150 (520) - Period utilities  
- st030 (433) - Stock valuation
- sl920 (420) - Sales analysis
- gl030 (377) - GL period-end