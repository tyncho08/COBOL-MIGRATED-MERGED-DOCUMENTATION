# ACAS COBOL System Analysis Report

Generated: 2025-09-19T16:23:31.130Z

## Executive Summary

The ACAS (Applewood Computers Accounting System) codebase consists of **453 programs** totaling **133,973 lines of code**. The system shows an average cyclomatic complexity of **46.63**, indicating significant complexity.

### Key Findings
- **101** main programs (entry points)
- **177** subprograms (called routines)
- **175** copybooks (shared data structures)
- **45** programs use embedded SQL
- **135** programs have high complexity (>50)
- **267** programs use GO TO statements

## System Overview

### Program Distribution by Type
| Type | Count | Percentage |
|------|-------|------------|
| Main Programs | 101 | 22.3% |
| Subprograms | 177 | 39.1% |
| Copybooks | 175 | 38.6% |

### Code Distribution by Module
| Module | Programs | Lines of Code | Avg Complexity |
|--------|----------|---------------|----------------|
| common | 157 | 67,839 | 59.30 |
| sales | 37 | 21,412 | 113.35 |
| purchase | 38 | 16,513 | 76.26 |
| general | 18 | 8,115 | 83.94 |
| irs | 16 | 7,764 | 84.25 |
| stock | 12 | 7,234 | 124.50 |
| copybooks | 174 | 5,059 | 2.09 |
| create-system-dat.cbl | 1 | 37 | 5.00 |

## Complexity Analysis

### Complexity Distribution
| Range | Count | Percentage |
|-------|-------|------------|
| 0-9 | 186 | 41.1% |
| 10-19 | 71 | 15.7% |
| 20-29 | 16 | 3.5% |
| 30-39 | 39 | 8.6% |
| 40-49 | 6 | 1.3% |
| 50-59 | 9 | 2.0% |
| 60-69 | 10 | 2.2% |
| 70-79 | 12 | 2.6% |
| 80-89 | 9 | 2.0% |
| 90-99 | 9 | 2.0% |
| 100-109 | 20 | 4.4% |
| 110-119 | 16 | 3.5% |
| 120-129 | 13 | 2.9% |
| 130-139 | 6 | 1.3% |
| 140-149 | 2 | 0.4% |
| 150-159 | 3 | 0.7% |
| 160-169 | 6 | 1.3% |
| 180-189 | 2 | 0.4% |
| 190-199 | 3 | 0.7% |
| 200-209 | 1 | 0.2% |
| 220-229 | 2 | 0.4% |
| 250-259 | 1 | 0.2% |
| 280-289 | 1 | 0.2% |
| 300-309 | 2 | 0.4% |
| 340-349 | 1 | 0.2% |
| 350-359 | 1 | 0.2% |
| 370-379 | 2 | 0.4% |
| 420-429 | 1 | 0.2% |
| 430-439 | 1 | 0.2% |
| 520-529 | 1 | 0.2% |
| 550-559 | 1 | 0.2% |

### Top 20 Most Complex Programs
| Program | File | Complexity | Lines |
|---------|------|------------|-------|
| sl910 | sales/sl910.cbl | 555 | 2312 |
| xl150 | common/xl150.cbl | 520 | 1461 |
| st030 | stock/st030.cbl | 433 | 1765 |
| sl920 | sales/sl920.cbl | 420 | 1665 |
| gl030 | general/gl030.cbl | 377 | 1835 |
| sys002 | common/sys002.cbl | 370 | 2132 |
| st020 | stock/st020.cbl | 351 | 1622 |
| st010 | stock/st010.cbl | 342 | 1639 |
| sl810 | sales/sl810.cbl | 308 | 1314 |
| sl810 | purchase/sl810.cbl | 308 | 1314 |
| sl010 | sales/sl010.cbl | 288 | 1502 |
| gl050 | general/gl050.cbl | 252 | 1065 |
| irs030 | irs/irs030.cbl | 225 | 1115 |
| irs060 | irs/irs060.cbl | 224 | 1146 |
| pl010 | purchase/pl010.cbl | 205 | 1077 |
| irs050 | irs/irs050.cbl | 195 | 1010 |
| slinvoiceMT | common/slinvoiceMT.cbl | 191 | 2305 |
| plinvoiceMT | common/plinvoiceMT.cbl | 191 | 2275 |
| slautogenMT | common/slautogenMT.cbl | 189 | 2359 |
| plautogenMT | common/plautogenMT.cbl | 189 | 2348 |

## Dependency Analysis

### Most Called Programs
| Program | Times Called |
|---------|--------------|
| maps04 | 89 |
| ACAS-Sysout | 79 |
| SYSTEM | 74 |
| fhlogger | 55 |
| acas-get-params | 28 |
| sl070 | 12 |
| maps01 | 11 |
| irsubn | 11 |
| fputs | 10 |
| acas000 | 8 |
| maps09 | 7 |
| irsubp | 4 |
| acas011 | 3 |
| sl960 | 3 |
| acas008 | 3 |
| acas010 | 3 |
| acas012 | 3 |
| acas022 | 3 |
| pl010 | 2 |
| irs055 | 2 |

### Most Used Copybooks
| Copybook | Times Used |
|----------|------------|
| of | 336 |
| the | 8 |
| code | 7 |
| in | 6 |
| to | 5 |
| Book | 5 |
| block | 4 |
| book | 4 |
| for | 4 |
| books | 3 |
| proc | 2 |
| verb | 2 |
| existing | 2 |
| WS | 2 |
| out | 1 |
| email | 1 |
| OF | 1 |
| Into | 1 |
| statement | 1 |
| if | 1 |

## Quality Analysis

### Programs with GO TO Statements (267)
stockconvert3, stockconvert2, stock, st060, st050, st040, st030, st020, st010, st000, acasconvert1, sl970, sl960, sl950, sl940, sl930, sl920, sl910, sl900, sl830...

### Programs without Error Handling (218)
stock, st040, st000, dummmy, sl900, sl820, sl800, sl000, sales, dummmy, sl820, purchase, pl900, pl800, pl180, pl000, dummmy, irs085, irs065, irs050...

### High Complexity Programs (>50) (135)
- stock (complexity: 99)
- st060 (complexity: 85)
- st050 (complexity: 51)
- st040 (complexity: 64)
- st030 (complexity: 433)
- st020 (complexity: 351)
- st010 (complexity: 342)
- sl970 (complexity: 123)
- sl950 (complexity: 125)
- sl940 (complexity: 83)
- sl930 (complexity: 162)
- sl920 (complexity: 420)
- sl910 (complexity: 555)
- sl900 (complexity: 64)
- sl830 (complexity: 129)
- sl820 (complexity: 58)
- sl810 (complexity: 308)
- sl190 (complexity: 101)
- sl180 (complexity: 77)
- sl170 (complexity: 108)
- sl160 (complexity: 114)
- sl120 (complexity: 136)
- sl110 (complexity: 112)
- sl100 (complexity: 73)
- sl095 (complexity: 66)
- sl085 (complexity: 73)
- sl080 (complexity: 108)
- sl070 (complexity: 121)
- sl060 (complexity: 136)
- sl055 (complexity: 92)
- sl050 (complexity: 66)
- sl020 (complexity: 133)
- sl010 (complexity: 288)
- sales (complexity: 127)
- sl830 (complexity: 129)
- sl820 (complexity: 58)
- sl810 (complexity: 308)
- purchase (complexity: 123)
- pl950 (complexity: 71)
- pl940 (complexity: 60)
- pl910 (complexity: 69)
- pl190 (complexity: 90)
- pl170 (complexity: 77)
- pl160 (complexity: 104)
- pl120 (complexity: 144)
- pl100 (complexity: 71)
- pl095 (complexity: 66)
- pl085 (complexity: 70)
- pl080 (complexity: 104)
- pl070 (complexity: 123)
- pl060 (complexity: 123)
- pl055 (complexity: 75)
- pl050 (complexity: 52)
- pl030 (complexity: 129)
- pl020 (complexity: 162)
- pl015 (complexity: 131)
- pl010 (complexity: 205)
- irs070 (complexity: 102)
- irs060 (complexity: 224)
- irs050 (complexity: 195)
- irs040 (complexity: 88)
- irs030 (complexity: 225)
- irs020 (complexity: 87)
- irs010 (complexity: 167)
- irs (complexity: 116)
- gl120 (complexity: 78)
- gl105 (complexity: 52)
- gl090a (complexity: 66)
- gl080 (complexity: 83)
- gl070 (complexity: 69)
- gl060 (complexity: 52)
- gl051 (complexity: 168)
- gl050 (complexity: 252)
- gl030 (complexity: 377)
- gl020 (complexity: 96)
- general (complexity: 103)
- xl150 (complexity: 520)
- valueMT (complexity: 121)
- systemMT (complexity: 84)
- sys4MT (complexity: 91)
- sys002 (complexity: 370)
- stockMT (complexity: 120)
- slpostingMT (complexity: 116)
- slinvoiceMT (complexity: 191)
- sldelinvnosMT (complexity: 116)
- slautogenMT (complexity: 189)
- SEND-MAIL-TEST-EXAMPLE (complexity: 52)
- salesMT (complexity: 135)
- purchMT (complexity: 136)
- plinvoiceMT (complexity: 191)
- plautogenMT (complexity: 189)
- paymentsMT (complexity: 161)
- otm5MT (complexity: 154)
- otm3MT (complexity: 154)
- nominalMT (complexity: 107)
- irspostingMT (complexity: 111)
- irsnominalMT (complexity: 153)
- irsfinalMT (complexity: 80)
- irsdfltMT (complexity: 93)
- glpostingMT (complexity: 116)
- glbatchMT (complexity: 114)
- finalMT (complexity: 91)
- dfltMT (complexity: 90)
- deliveryMT (complexity: 115)
- delfolioMT (complexity: 115)
- auditMT (complexity: 120)
- analMT (complexity: 113)
- acasirsub5 (complexity: 83)
- acasirsub4 (complexity: 92)
- acasirsub3 (complexity: 79)
- acasirsub1 (complexity: 145)
- acas032 (complexity: 101)
- acas030 (complexity: 106)
- acas029 (complexity: 106)
- acas026 (complexity: 106)
- acas023 (complexity: 103)
- acas022 (complexity: 115)
- acas019 (complexity: 106)
- acas017 (complexity: 103)
- acas016 (complexity: 108)
- acas015 (complexity: 115)
- acas014 (complexity: 103)
- acas013 (complexity: 116)
- acas012 (complexity: 116)
- acas011 (complexity: 161)
- acas010 (complexity: 71)
- acas008 (complexity: 81)
- acas007 (complexity: 103)
- acas006 (complexity: 101)
- acas005 (complexity: 118)
- acas004 (complexity: 105)
- acas000 (complexity: 71)
- takeon-2 (complexity: 59)
- takeon-1 (complexity: 59)
- ACAS (complexity: 62)

### Programs with Possible Dead Code (287)
stockconvert3, stockconvert2, stock, st060, st050, st040, st030, st020, st010, st000, dummmy, acasconvert1, sl970, sl960, sl950, sl940, sl930, sl920, sl910, sl900...

## Database Integration

### Programs Using SQL (45)
st010, valueMT, valueLD, SystemLD, sys4LD, StockLD, slpostingMT, slpostingLD, slinvoiceMT, slinvoiceLD, sldelinvnosMT, sldelinvnosLD, slautogenMT, slautogenLD, salesLD, purchLD, plinvoiceMT, plinvoiceLD, plautogenMT, plautogenLD, paymentsMT, paymentsLD, otm5LD, otm3LD, nominalLD, irspostingMT, irspostingLD, irsnominalMT, irsnominalLD, irsfinalLD...

### Database Tables Referenced (18)
DELIVERY, Flat, GLBATCH, GLPOSTING, IRSNL, IRSPOSTING, PLPAY, PSIRSPOST, PUAUTOGEN, PUDELINV, PUINV, SAAUTOGEN, SADELINV, SAINV, STOCKAUDIT, VALUEANAL, WS, superseding

## Migration Readiness Assessment

### Strengths
- Well-structured modular design with clear separation between modules
- Consistent use of copybooks for shared data structures
- 41.1% of programs avoid GO TO statements

### Challenges
- 135 programs require refactoring due to high complexity
- 218 programs lack proper error handling
- Mixed file-based and SQL-based data access patterns

### Recommendations
1. **Priority Refactoring**: Focus on the 20 most complex programs
2. **Error Handling**: Add comprehensive error handling to 218 programs
3. **GO TO Elimination**: Refactor 267 programs to use structured programming
4. **Data Access Layer**: Standardize data access patterns across the system
5. **Incremental Migration**: Start with low-complexity, well-isolated modules

## Next Steps

1. Generate detailed functional documentation for business logic understanding
2. Create subsystem boundaries based on dependency analysis
3. Develop migration strategy for each module
4. Establish testing framework for validation
5. Plan phased migration approach

---
*This report provides a comprehensive technical analysis of the ACAS COBOL system. For business-level documentation, refer to the functional analysis documents.*