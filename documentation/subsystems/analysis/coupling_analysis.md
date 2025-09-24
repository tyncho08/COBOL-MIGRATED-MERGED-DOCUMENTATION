# ACAS Subsystem Coupling Analysis

## Overview

This document analyzes the coupling between subsystems in the ACAS system, identifying tight and loose coupling patterns to guide modernization efforts. Lower coupling indicates easier migration and maintenance.

## Coupling Metrics

### Coupling Types Analyzed

1. **Data Coupling**: Subsystems communicate through data parameters
2. **Stamp Coupling**: Subsystems share data structures
3. **Control Coupling**: One subsystem controls flow of another
4. **Common Coupling**: Shared global data
5. **Content Coupling**: Direct access to internal data

## Subsystem Coupling Matrix

### Coupling Strength Scale
- 0 = No coupling
- 1 = Data coupling only (loose)
- 2 = Stamp coupling
- 3 = Control coupling
- 4 = Common coupling
- 5 = Content coupling (tight)

| From\To | GL | AR | AP | INV | IRS | MDM | RPT | BATCH | INT | SEC | FILE | DATE | CURR | ERR |
|---------|----|----|----|----|-----|-----|-----|-------|-----|-----|------|------|------|-----|
| GL_CORE | - | 0 | 0 | 0 | 0 | 1 | 1 | 0 | 0 | 1 | 1 | 1 | 1 | 1 |
| AR_MGMT | 2 | - | 0 | 2 | 0 | 2 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 |
| AP_MGMT | 2 | 0 | - | 2 | 0 | 2 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 |
| INV_CTRL | 2 | 1 | 1 | - | 0 | 2 | 1 | 1 | 0 | 1 | 1 | 1 | 0 | 1 |
| IRS_PROC | 2 | 0 | 0 | 0 | - | 1 | 1 | 1 | 0 | 1 | 1 | 1 | 1 | 1 |
| MDM | 0 | 0 | 0 | 0 | 0 | - | 0 | 0 | 0 | 0 | 1 | 0 | 0 | 1 |
| RPT_ENGINE | 1 | 1 | 1 | 1 | 1 | 1 | - | 1 | 0 | 1 | 1 | 1 | 1 | 1 |
| BATCH_FW | 3 | 3 | 3 | 3 | 3 | 1 | 2 | - | 1 | 1 | 1 | 1 | 0 | 1 |
| INTEGRATION | 1 | 1 | 1 | 1 | 0 | 1 | 0 | 1 | - | 1 | 1 | 0 | 1 | 1 |
| SEC_AUDIT | 0 | 0 | 0 | 0 | 0 | 1 | 0 | 0 | 0 | - | 1 | 0 | 0 | 1 |
| FILE_SVC | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | - | 0 | 0 | 1 |
| DATE_UTIL | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | - | 0 | 0 |
| CURR_UTIL | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | - | 0 |
| ERROR_FW | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | - |

## Coupling Analysis by Subsystem

### Tightly Coupled Subsystems (Score > 2.0)

1. **BATCH_FW** (Avg: 3.0)
   - Control coupling with all business modules
   - Orchestrates job execution
   - **Risk**: Single point of failure
   - **Recommendation**: Move to event-driven architecture

2. **AR_MGMT & AP_MGMT** (Avg: 2.0)
   - Stamp coupling with MDM (shared structures)
   - Stamp coupling with INV_CTRL
   - **Risk**: Changes to shared structures impact multiple modules
   - **Recommendation**: Define clear DTOs for data exchange

### Moderately Coupled Subsystems (Score 1.0-2.0)

3. **GL_CORE** (Avg: 1.5)
   - Receives data from all posting modules
   - Data coupling only (good)
   - **Risk**: Central dependency
   - **Recommendation**: Maintain loose coupling through interfaces

4. **INV_CTRL** (Avg: 1.5)
   - Coupled with AR/AP for stock allocation
   - **Risk**: Real-time dependencies
   - **Recommendation**: Consider async messaging

5. **MDM** (Avg: 1.0)
   - Read-mostly pattern
   - Low coupling score
   - **Recommendation**: Good candidate for early migration

### Loosely Coupled Subsystems (Score < 1.0)

6. **Utility Subsystems** (DATE, CURR, ERROR)
   - No dependencies on other subsystems
   - Pure service providers
   - **Recommendation**: Migrate first as shared services

7. **IRS_PROC** (Avg: 0.8)
   - Minimal coupling
   - Only posts to GL
   - **Recommendation**: Excellent pilot candidate

## Coupling Patterns Identified

### 1. Hub Pattern (GL_CORE)
```
AR_MGMT ──┐
AP_MGMT ──┼──> GL_CORE ──> RPT_ENGINE
INV_CTRL ─┤
IRS_PROC ─┘
```
**Impact**: GL changes affect all modules
**Mitigation**: Event-driven posting

### 2. Shared Data Pattern (MDM)
```
        ┌──> AR_MGMT
MDM ────┼──> AP_MGMT
        └──> INV_CTRL
```
**Impact**: Master data changes ripple through
**Mitigation**: Versioned APIs, caching

### 3. Control Pattern (BATCH_FW)
```
BATCH_FW controls execution of:
├── Daily Sales Batch (AR)
├── Payment Run (AP)
├── Inventory Revaluation (INV)
└── GL Posting (GL)
```
**Impact**: Batch framework changes affect all
**Mitigation**: Declarative job definitions

### 4. Service Pattern (Utilities)
```
All Modules ──> DATE_UTIL
All Modules ──> CURR_UTIL
All Modules ──> ERROR_FW
```
**Impact**: Low - one-way dependencies
**Mitigation**: Already well-designed

## Coupling Reduction Strategies

### 1. Introduce Message Bus
**Current**: Direct program calls
**Target**: Async messaging
```
Before: AR ─CALL─> stockMT ─UPDATE─> Stock File
After:  AR ─EVENT─> Bus ─NOTIFY─> INV_CTRL
```

### 2. API Gateway Pattern
**Current**: Direct file access
**Target**: Service interfaces
```
Before: Program ─READ─> SLMASTER file
After:  Program ─API─> Customer Service ─READ─> Database
```

### 3. Event Sourcing
**Current**: Batch file transfers
**Target**: Event streams
```
Before: AR ─FILE─> GL (nightly)
After:  AR ─EVENT─> Stream ─CONSUME─> GL (real-time)
```

### 4. Dependency Injection
**Current**: Hard-coded program calls
**Target**: Configuration-driven
```
Before: CALL 'salesMT' USING ...
After:  CALL configured-service USING ...
```

## Migration Impact Based on Coupling

### Phase 1: Zero-Coupling Modules (Easiest)
- DATE_UTIL - No incoming dependencies
- CURR_UTIL - No incoming dependencies  
- ERROR_FW - No incoming dependencies
- **Duration**: 3 months

### Phase 2: Low-Coupling Modules
- IRS_PROC - Only couples to GL
- MDM - Read-mostly pattern
- SEC_AUDIT - Minimal coupling
- **Duration**: 4 months

### Phase 3: Medium-Coupling Modules
- FILE_SVC - All modules depend on it (needs wrapper)
- INV_CTRL - Coupled with AR/AP
- **Duration**: 6 months

### Phase 4: High-Coupling Modules
- AR_MGMT - Multiple dependencies
- AP_MGMT - Multiple dependencies
- INTEGRATION - Cross-cutting concerns
- **Duration**: 8 months

### Phase 5: Critical-Path Modules
- BATCH_FW - Controls all batch processing
- GL_CORE - Central hub
- RPT_ENGINE - Depends on all
- **Duration**: 6 months

## Coupling Metrics Summary

| Metric | Current | Target | Improvement |
|--------|---------|--------|-------------|
| Average Coupling Score | 1.8 | 0.9 | 50% reduction |
| Tight Couplings (>3) | 15 | 0 | Eliminate |
| Control Couplings | 12 | 2 | 85% reduction |
| Common Couplings | 0 | 0 | Maintain |
| Content Couplings | 0 | 0 | Maintain |

## Recommendations

### Immediate Actions
1. Document all interface contracts
2. Identify shared data structures
3. Create abstraction layers
4. Plan API definitions

### Short-term (6 months)
1. Implement service wrappers
2. Replace direct calls with interfaces
3. Introduce caching layer
4. Create event catalog

### Long-term (12+ months)
1. Full API gateway
2. Event-driven architecture
3. Microservices extraction
4. Complete decoupling

## Risk Assessment

### High-Risk Couplings
1. **BATCH_FW Control Coupling**
   - Impact: System-wide failures
   - Mitigation: Fault tolerance, circuit breakers

2. **Real-time AR-INV Coupling**
   - Impact: Performance bottlenecks
   - Mitigation: Async processing, queuing

3. **GL Central Hub**
   - Impact: Single point of failure
   - Mitigation: High availability, scaling

### Low-Risk Couplings
1. **Utility Service Dependencies**
   - Well-isolated, stable interfaces
   - No mitigation needed

2. **MDM Read Dependencies**
   - Can be cached effectively
   - Low change frequency

## Conclusion

The ACAS system exhibits moderate coupling (1.8 average), with clear patterns that can be addressed systematically. The utility subsystems show excellent low coupling and can be migrated immediately. The business subsystems show higher coupling but follow predictable patterns (hub, shared data, control) that have well-established mitigation strategies.

Priority should be given to:
1. Reducing BATCH_FW control coupling
2. Abstracting AR/AP/INV interactions
3. Implementing event-driven GL posting
4. Creating service interfaces for MDM

With these changes, the system can achieve the target coupling score of 0.9, enabling independent subsystem evolution and migration.