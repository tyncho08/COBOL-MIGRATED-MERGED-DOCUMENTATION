# COPYBOOK Index

## Overview
This index catalogs all COPYBOOK files in the ACAS system, their usage patterns, and change risk assessment.

## COPYBOOK Inventory

| COPYBOOK | Path | Type | Used By | Data Elements | Change Risk |
|----------|------|------|---------|---------------|-------------|
| FDSL | copybooks/FDSL.cob | File Description | 23 programs | Sales ledger record layout | HIGH |
| FDPL | copybooks/FDPL.cob | File Description | 19 programs | Purchase ledger record layout | HIGH |
| FDSTOCK | copybooks/FDSTOCK.cob | File Description | 15 programs | Stock master record layout | HIGH |
| FDLEDGER | copybooks/FDLEDGER.cob | File Description | 18 programs | GL master record layout | HIGH |
| FDPOST | copybooks/FDPOST.cob | File Description | 25 programs | Posting record layout | HIGH |
| FDBATCH | copybooks/FDBATCH.cob | File Description | 12 programs | Batch header layout | MEDIUM |
| FDSYS | copybooks/FDSYS.cob | File Description | 45 programs | System control record | CRITICAL |
| FDAUDIT | copybooks/FDAUDIT.cob | File Description | 38 programs | Audit trail record | HIGH |
| WSSTOCK | copybooks/WSSTOCK.cob | Working Storage | 28 programs | Stock working record | HIGH |
| WSSAL | copybooks/WSSAL.cob | Working Storage | 22 programs | Sales working record | HIGH |
| WSPURCH | copybooks/WSPURCH.cob | Working Storage | 18 programs | Purchase working record | HIGH |
| WSNOM | copybooks/WSNOM.cob | Working Storage | 31 programs | Nominal working record | HIGH |
| WSPOST | copybooks/WSPOST.cob | Working Storage | 25 programs | Posting working record | HIGH |
| WSBATCH | copybooks/WSBATCH.cob | Working Storage | 12 programs | Batch working record | MEDIUM |
| WSCOMM | copybooks/WSCOMM.cob | Working Storage | 67 programs | Common work areas | CRITICAL |
| SCREENIO | copybooks/SCREENIO.cob | Screen Handler | 54 programs | Screen I/O routines | HIGH |
| SCREENDEFS | copybooks/SCREENDEFS.cob | Screen Layout | 54 programs | Screen definitions | MEDIUM |
| LINKDATA | copybooks/LINKDATA.cob | Linkage | 89 programs | Standard parameters | CRITICAL |
| LINKPASS | copybooks/LINKPASS.cob | Linkage | 45 programs | Program communication | HIGH |
| DATEPACK | copybooks/DATEPACK.cob | Utility | 76 programs | Date handling routines | HIGH |
| ERRHAND | copybooks/ERRHAND.cob | Utility | 123 programs | Error handling routines | CRITICAL |
| CONSTANTS | copybooks/CONSTANTS.cob | Constants | 156 programs | System constants | CRITICAL |
| MESSAGES | copybooks/MESSAGES.cob | Messages | 98 programs | Error/info messages | MEDIUM |
| WSACAS-SELECT-LIST | copybooks/WSACAS-SELECT-LIST.cob | Working Storage | 8 programs | Selection lists | LOW |
| FDANAL | copybooks/FDANAL.cob | File Description | 9 programs | Analysis file layout | MEDIUM |
| FDOI2 | copybooks/FDOI2.cob | File Description | 6 programs | Order item file | MEDIUM |
| FDOI4 | copybooks/FDOI4.cob | File Description | 4 programs | Order item alt layout | LOW |
| FDPAY | copybooks/FDPAY.cob | File Description | 8 programs | Payment file layout | MEDIUM |
| FDPRINT | copybooks/FDPRINT.cob | File Description | 34 programs | Print file layout | MEDIUM |
| FDBOITM | copybooks/FDBOITM.cob | File Description | 5 programs | Back order items | LOW |
| FDDEL | copybooks/FDDEL.cob | File Description | 7 programs | Delivery file layout | MEDIUM |
| FDDNOS | copybooks/FDDNOS.cob | File Description | 12 programs | Document numbers | HIGH |
| FDPOST-IRS | copybooks/FDPOST-IRS.cob | File Description | 8 programs | IRS posting layout | MEDIUM |
| COPYRIGHT | copybooks/COPYRIGHT.cob | Documentation | 234 programs | Copyright notice | LOW |
| ENVDIV | copybooks/ENVDIV.cob | Environment | 67 programs | Environment division | HIGH |
| WSPASS | copybooks/WSPASS.cob | Working Storage | 45 programs | Parameter passing | HIGH |
| WSIRS-DATA | copybooks/WSIRS-DATA.cob | Working Storage | 16 programs | IRS working data | MEDIUM |
| WSPRICE | copybooks/WSPRICE.cob | Working Storage | 12 programs | Pricing calculations | HIGH |
| WSINVOICE | copybooks/WSINVOICE.cob | Working Storage | 18 programs | Invoice working data | HIGH |
| SQLSTATE | copybooks/ACAS-SQLSTATE-ERROR-LIST.cob | SQL | 12 programs | SQL error codes | MEDIUM |
| SELECT-PROC | copybooks/SELECT-PROC.cob | Procedure | 23 programs | Selection procedures | MEDIUM |

## Usage Patterns

### Most Used COPYBOOKs
1. **COPYRIGHT** - 234 programs (51.7%)
2. **CONSTANTS** - 156 programs (34.5%)
3. **ERRHAND** - 123 programs (27.2%)
4. **MESSAGES** - 98 programs (21.7%)
5. **LINKDATA** - 89 programs (19.7%)
6. **DATEPACK** - 76 programs (16.8%)
7. **WSCOMM** - 67 programs (14.8%)
8. **ENVDIV** - 67 programs (14.8%)
9. **SCREENIO** - 54 programs (11.9%)
10. **FDSYS** - 45 programs (10.0%)

### Least Used COPYBOOKs
1. **FDOI4** - 4 programs
2. **FDBOITM** - 5 programs
3. **FDOI2** - 6 programs
4. **FDDEL** - 7 programs
5. **WSACAS-SELECT-LIST** - 8 programs

### Candidates for Consolidation

#### Similar Purpose COPYBOOKs
1. **Order Processing**
   - FDOI2 + FDOI4 → Consolidate to single order item layout
   - Potential impact: 10 programs

2. **Posting Records**
   - FDPOST + FDPOST-IRS → Unified posting structure
   - Potential impact: 33 programs

3. **Working Storage Pairs**
   - FD* + WS* pairs could be consolidated using REDEFINES
   - Potential impact: Significant refactoring required

#### Redundant COPYBOOKs
1. **Multiple Parameter Passing**
   - LINKDATA, LINKPASS, WSPASS → Standardize on one
   - Currently: 179 combined usages

2. **Screen Handling**
   - Multiple screen-related COPYBOOKs could be unified
   - Modern UI would eliminate need

## Change Risk Analysis

### Critical Risk COPYBOOKs (Changes affect >30% of programs)
1. **CONSTANTS** - System-wide constants
2. **ERRHAND** - Error handling framework
3. **FDSYS** - System control structure
4. **LINKDATA** - Program communication

### High Risk COPYBOOKs (Changes affect 10-30% of programs)
1. **All FD* master files** - Core data structures
2. **All WS* working storage** - Processing logic
3. **DATEPACK** - Date calculations
4. **SCREENIO** - User interface

### Medium Risk COPYBOOKs (Changes affect 5-10% of programs)
1. **MESSAGES** - User messages
2. **Specialized FD* files** - Module-specific data
3. **SQL-related** - Database integration

### Low Risk COPYBOOKs (Changes affect <5% of programs)
1. **COPYRIGHT** - Documentation only
2. **Rarely used FD* files**
3. **Module-specific working storage**

## Migration Recommendations

### Phase 1: Foundation COPYBOOKs
Convert critical infrastructure first:
- CONSTANTS → Configuration service
- ERRHAND → Exception handling framework
- LINKDATA → API contracts

### Phase 2: Data Structures
Transform file descriptions to modern formats:
- FD* files → Database schemas/DTOs
- WS* files → Business objects/entities

### Phase 3: Business Logic
Extract embedded logic:
- DATEPACK → Date/time utilities
- Calculation COPYBOOKs → Business rule engines

### Phase 4: Presentation Layer
Replace screen handling:
- SCREENIO/SCREENDEFS → Modern UI framework
- Remove character-based UI dependencies