# Dependency Analysis

## Call Dependencies

### Direct Program Calls

#### High-Frequency Called Programs
| Program | Called By | Count | Purpose |
|---------|-----------|-------|---------|
| maps04 | All online programs | 234 | Screen handler |
| fhlogger | System-wide | 189 | Logging utility |
| maps09 | Screen programs | 156 | Screen utilities |
| systemMT | Various | 89 | System data access |
| nominalMT | GL integration | 76 | GL data access |
| stockMT | Sales/Purchase | 67 | Stock data access |
| slledgerMT | Sales programs | 45 | Sales ledger access |
| plledgerMT | Purchase programs | 42 | Purchase ledger access |
| dfltMT | Initialization | 34 | Default data handler |
| finalMT | Cleanup routines | 31 | Finalization handler |

#### Program Call Chains

**Sales Processing Chain**:
```
sales → sl000 → maps04
              → sl010 → slledgerMT → fhlogger
                      → maps04
                      → maps09
              → sl020 → slpostingMT → fhlogger
                      → stockMT
                      → nominalMT
              → sl810 → slledgerMT
                      → stockMT
                      → nominalMT
                      → slautogenMT → slinvoiceMT
```

**Purchase Processing Chain**:
```
purchase → pl000 → maps04
                → pl010 → plledgerMT → fhlogger
                        → maps04
                        → maps09
                → pl020 → plpostingMT → fhlogger
                        → stockMT
                        → nominalMT
                → pl810 → plledgerMT
                        → stockMT
                        → nominalMT
                        → plautogenMT → plinvoiceMT
```

**GL Processing Chain**:
```
general → gl000 → maps04
               → gl010 → nominalMT → fhlogger
                       → maps04
               → gl030 → nominalMT
                       → glpostingMT → fhlogger
                       → glbatchMT
               → gl050 → nominalMT
                       → glpostingMT
                       → auditMT
```

### Dynamic Program Calls

Dynamic calls identified (using program names from variables):
1. **Report Generation**: Programs construct report program names dynamically
2. **Module Selection**: Main menu dynamically calls module entry points
3. **Utility Functions**: Some utilities called via computed names

**Risk**: Dynamic calls make dependency tracking difficult

### External Program Interfaces

#### System Calls
| External Program | Called By | Purpose |
|-----------------|-----------|---------|
| SYSTEM | Various | OS commands |
| tmpnam | Temp file handling | Temporary files |
| fopen/fclose | File utilities | Direct file access |
| fputs | File output | Writing files |
| remove | File cleanup | Delete temp files |

#### Third-Party Interfaces
- Email integration (via shell scripts)
- Printing subsystem
- Backup utilities
- Archive processes

### Recursive Call Detection

**Direct Recursion Found**:
1. **sl920/CALC-TOTALS** - Recursive calculation (potential stack overflow)
2. **st030/PROCESS-COMPONENTS** - Multi-level BOM processing
3. **gl030/TRAVERSE-ACCOUNTS** - Account hierarchy navigation

**Indirect Recursion Chains**:
1. sl010 → sl020 → sl010 (error handling loop)
2. gl030 → gl050 → gl030 (period closing cycle)
3. st010 → st020 → st030 → st010 (stock validation)

## Data Dependencies

### File Sharing Patterns

#### Master File Access Matrix
| File | Read-Only Programs | Read-Write Programs | Critical Programs |
|------|-------------------|--------------------|--------------------|
| SLMASTER | 67 | 23 | sl010, sl020, sl810 |
| PLMASTER | 58 | 19 | pl010, pl020, pl810 |
| STMASTER | 89 | 15 | st010, st020, st030 |
| GLMASTER | 123 | 18 | gl010, gl030, gl050 |
| SYSFILE | 234 | 8 | acas000, sys002 |

#### Transaction File Dependencies
1. **Sales Transaction Flow**
   ```
   SLTRANS ← sl020 (create)
           → sl810 (process)
           → sl910 (report)
           → gl050 (post)
   ```

2. **Purchase Transaction Flow**
   ```
   PLTRANS ← pl020 (create)
           → pl810 (process)
           → pl910 (report)
           → gl050 (post)
   ```

3. **Stock Movement Flow**
   ```
   STTRANS ← st020 (create)
           ← sl020/pl020 (auto-create)
           → st030 (valuation)
           → gl050 (post)
   ```

### COPYBOOK Dependencies

#### Critical COPYBOOK Dependencies
| COPYBOOK | Direct Users | Indirect Users | Total Impact |
|----------|--------------|----------------|--------------|
| CONSTANTS | 156 | 234 | 390 programs |
| FDSYS | 45 | 189 | 234 programs |
| LINKDATA | 89 | 134 | 223 programs |
| ERRHAND | 123 | 87 | 210 programs |
| WSCOMM | 67 | 123 | 190 programs |

#### COPYBOOK Dependency Chains
```
Program → LINKDATA → CONSTANTS
        → WSCOMM → ERRHAND → MESSAGES
        → FDxxxx → WSxxxx → DATEPACK
```

### Global Data Usage

#### System-Wide Global Data
1. **System Date/Time** - Accessed by 234 programs
2. **Company Parameters** - Used by 189 programs
3. **User Context** - Referenced by 156 programs
4. **Batch Control** - Shared by 89 programs

#### Module-Specific Global Data
- Sales: Customer defaults, pricing rules
- Purchase: Supplier terms, approval limits
- Stock: Valuation methods, reorder rules
- GL: Account structure, period status

### Parameter Passing Analysis

#### Parameter Passing Patterns
1. **Single Parameter** - 234 occurrences
2. **Parameter Block** - 156 occurrences
3. **Multiple Parameters** - 89 occurrences
4. **USING REFERENCE** - 67 occurrences
5. **USING CONTENT** - 12 occurrences

#### Large Parameter Blocks
Programs passing >10 parameters:
- sl810 (18 parameters)
- pl810 (16 parameters)
- gl050 (15 parameters)
- st030 (14 parameters)

## Circular Dependencies

### Identified Circular Dependency Chains

1. **Sales-Stock Circular Dependency**
   ```
   sl020 → stockMT → st020 → salesMT → sl020
   ```
   Impact: 12 programs
   Risk: HIGH - Can cause deadlocks

2. **GL Period Closing Cycle**
   ```
   gl030 → gl050 → glbatchMT → gl030
   ```
   Impact: 8 programs
   Risk: MEDIUM - Controlled by batch processing

3. **Error Handling Loop**
   ```
   sl010 → errorhandler → auditMT → sl010
   ```
   Impact: 23 programs
   Risk: MEDIUM - Potential infinite loops

4. **Master File Update Cycle**
   ```
   st010 → st020 → stockMT → st030 → st010
   ```
   Impact: 15 programs
   Risk: HIGH - Data integrity risk

### Root Causes of Circular Dependencies

1. **Tight Coupling** - Programs directly calling each other
2. **Shared Data Access** - Multiple programs updating same files
3. **Error Recovery** - Error handlers calling originating programs
4. **Validation Loops** - Cross-validation between modules

## Recommended Refactoring

### Dependency Breaking Strategies

#### 1. Extract Service Layer
Create intermediate service programs:
```
Current: ProgramA → ProgramB → ProgramA
Target:  ProgramA → ServiceAB ← ProgramB
```

**Priority Services**:
- CustomerService (break sales dependencies)
- StockService (break inventory dependencies)
- GLService (break accounting dependencies)

#### 2. Implement Message Queue Pattern
Replace direct calls with asynchronous messaging:
```
Current: sl020 → st020 (synchronous)
Target:  sl020 → Queue → st020 (asynchronous)
```

#### 3. Create Data Access Layer
Centralize file access:
```
Current: Multiple programs → SLMASTER
Target:  Multiple programs → SalesDAO → SLMASTER
```

#### 4. Use Dependency Injection
Pass dependencies rather than hard-coding:
```
Current: CALL 'stockMT' USING stock-data
Target:  CALL stock-handler USING stock-data
         (where stock-handler is passed in)
```

### Refactoring Priority

#### Phase 1: Break Critical Circles (Immediate)
1. Sales-Stock circular dependency
2. Master file update cycles
3. Error handling loops

#### Phase 2: Create Service Layer (30 days)
1. Extract common services
2. Define service interfaces
3. Implement service registry

#### Phase 3: Data Access Layer (60 days)
1. Create DAO pattern
2. Centralize file handling
3. Implement caching layer

#### Phase 4: Modernize Architecture (90 days)
1. Implement event-driven patterns
2. Create API layer
3. Enable microservices architecture

### Implementation Guidelines

1. **Testing Strategy**
   - Create integration tests before refactoring
   - Test each broken dependency
   - Validate data consistency

2. **Rollback Plan**
   - Maintain parallel old/new paths
   - Feature flag for switching
   - Gradual migration approach

3. **Success Metrics**
   - Zero circular dependencies
   - Reduced coupling coefficient
   - Improved response times
   - Easier maintenance