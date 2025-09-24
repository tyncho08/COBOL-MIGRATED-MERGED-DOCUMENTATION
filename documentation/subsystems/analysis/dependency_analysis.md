# ACAS Subsystem Dependency Analysis

## Overview

This document provides a comprehensive analysis of dependencies between subsystems, identifying coupling points, circular dependencies, and migration risks. This analysis is critical for planning the migration sequence and identifying areas requiring refactoring.

## Dependency Types

### 1. Data Dependencies
- **Master Data**: Read-only access to reference data
- **Transactional Data**: Access to transaction records
- **Derived Data**: Calculated or aggregated information

### 2. Functional Dependencies
- **Synchronous Calls**: Direct program invocations
- **Asynchronous Messages**: Batch file exchanges
- **Shared Libraries**: Common utility functions

### 3. Temporal Dependencies
- **Sequential Processing**: Must run in order
- **Prerequisite Data**: Requires prior completion
- **Batch Windows**: Time-based constraints

## Dependency Matrix

### Direct Dependencies (→ depends on)

| Subsystem | Dependencies | Dependency Count | Type |
|-----------|-------------|------------------|------|
| IRS_PROC | GL_CORE | 1 | Functional |
| DATE_UTIL | None | 0 | None |
| CURR_UTIL | None | 0 | None |
| ERROR_FW | None | 0 | None |
| MDM | None | 0 | None |
| FILE_SVC | ERROR_FW | 1 | Functional |
| SEC_AUDIT | MDM | 1 | Data |
| INV_CTRL | MDM, GL_CORE | 2 | Data, Functional |
| RPT_ENGINE | All business subsystems | 5 | Data |
| BATCH_FW | All subsystems | 13 | Temporal |
| AP_MGMT | MDM, INV_CTRL, GL_CORE | 3 | All types |
| AR_MGMT | MDM, INV_CTRL, GL_CORE | 3 | All types |
| INTEGRATION | Multiple subsystems | 4 | Functional |
| GL_CORE | All posting subsystems | 4 | Data |

### Reverse Dependencies (← depended on by)

| Subsystem | Depended On By | Reverse Count | Critical? |
|-----------|---------------|---------------|-----------|
| DATE_UTIL | GL, AR, AP, BATCH | 4 | Yes |
| CURR_UTIL | GL, AR, AP | 3 | Yes |
| ERROR_FW | All subsystems | 13 | Yes |
| MDM | AR, AP, INV, SEC | 4 | Yes |
| GL_CORE | None (endpoint) | 0 | No |
| FILE_SVC | All subsystems | 13 | Yes |
| SEC_AUDIT | None | 0 | No |
| INV_CTRL | AR, AP | 2 | Yes |
| RPT_ENGINE | None | 0 | No |
| BATCH_FW | None | 0 | No |

## Circular Dependencies

### Identified Circular Dependencies

1. **None Found** - The architecture shows good acyclic design

### Potential Circular Risks

1. **AR ↔ INV**: Order processing requires inventory, stock updates require sales
   - **Resolution**: Event-driven updates with clear ownership

2. **AP ↔ INV**: Purchase receipts update inventory, reorders create purchases
   - **Resolution**: Separate receipt processing from reorder generation

## Dependency Depth Analysis

### Dependency Chains

```
Level 0 (No dependencies):
- DATE_UTIL
- CURR_UTIL  
- ERROR_FW
- MDM

Level 1 (Utility dependencies only):
- FILE_SVC (depends on ERROR_FW)
- SEC_AUDIT (depends on MDM)

Level 2 (Simple business dependencies):
- IRS_PROC (depends on GL_CORE)
- INV_CTRL (depends on MDM, GL_CORE)

Level 3 (Complex dependencies):
- AR_MGMT (depends on MDM, INV_CTRL, GL_CORE)
- AP_MGMT (depends on MDM, INV_CTRL, GL_CORE)

Level 4 (Integration layer):
- INTEGRATION (depends on multiple)
- RPT_ENGINE (depends on all business)
- BATCH_FW (orchestrates all)

Level 5 (Terminal node):
- GL_CORE (receives from all)
```

## Critical Path Analysis

### High-Impact Dependencies

1. **MDM (Master Data)**
   - Impact: Cannot process transactions without master data
   - Risk: High - must migrate early and completely
   - Mitigation: Read-only cache during migration

2. **FILE_SVC (File Services)**
   - Impact: All data access depends on this
   - Risk: Very High - single point of failure
   - Mitigation: Parallel new/old access methods

3. **ERROR_FW (Error Framework)**
   - Impact: Error handling throughout system
   - Risk: Medium - graceful degradation possible
   - Mitigation: Dual error handling during transition

### Dependency Bottlenecks

| Bottleneck | Dependent Count | Impact | Resolution |
|-----------|----------------|---------|------------|
| FILE_SVC | 13 | All I/O operations | Implement caching layer |
| MDM | 4 | All transactions | Read replicas |
| GL_CORE | 4 | Financial reporting | Queue-based posting |
| ERROR_FW | 13 | Error handling | Distributed logging |

## Interface Coupling Analysis

### Tight Coupling (High Risk)

| Interface | Subsystems | Coupling Type | Migration Risk |
|-----------|-----------|---------------|----------------|
| Direct CALL | AR → stockMT | Synchronous | High |
| Direct CALL | AP → stockMT | Synchronous | High |
| Shared File | AR/AP/GL | Data coupling | Medium |
| Global Data | Via copybooks | Control coupling | High |

### Loose Coupling (Low Risk)

| Interface | Subsystems | Coupling Type | Migration Risk |
|-----------|-----------|---------------|----------------|
| Batch File | AR → GL | Temporal | Low |
| Batch File | AP → GL | Temporal | Low |
| Parameter | Utilities | Data | Low |

## Migration Sequence Optimization

### Recommended Migration Order

```
Phase 1: Zero Dependencies (Can migrate anytime)
├── DATE_UTIL
├── CURR_UTIL
└── ERROR_FW

Phase 2: Minimal Dependencies
├── MDM (master data)
├── FILE_SVC (with wrapper)
└── SEC_AUDIT

Phase 3: Isolated Business
└── IRS_PROC (only depends on GL)

Phase 4: Moderate Dependencies
└── INV_CTRL (prepare for AR/AP)

Phase 5: Complex Business
├── AR_MGMT (parallel)
└── AP_MGMT (parallel)

Phase 6: Infrastructure
├── BATCH_FW
├── INTEGRATION
└── RPT_ENGINE

Phase 7: Core System
└── GL_CORE (last, most dependent)
```

### Dependency Breaking Strategies

1. **Introduce Facades**
   ```
   Current: AR → Direct CALL → INV
   Target: AR → API Facade → INV
   ```

2. **Event-Driven Decoupling**
   ```
   Current: Synchronous updates
   Target: Publish events → Subscribe
   ```

3. **Data Replication**
   ```
   Current: Shared file access
   Target: Each subsystem owns copy
   ```

## Risk Assessment

### High-Risk Dependencies

| Dependency | Risk Level | Impact | Mitigation |
|-----------|-----------|---------|------------|
| All → FILE_SVC | Critical | System halt | Dual-path access |
| AR/AP → INV real-time | High | Transaction fail | Queue with retry |
| All → GL posting | Medium | Report delay | Batch tolerance |
| Batch sequencing | Medium | Process fail | Dependency manager |

### Dependency-Related Risks

1. **Cascade Failures**
   - Risk: FILE_SVC failure affects all
   - Mitigation: Circuit breakers, fallback

2. **Version Mismatches**
   - Risk: Interface changes break dependents
   - Mitigation: Version management, testing

3. **Performance Degradation**
   - Risk: Added layers slow response
   - Mitigation: Performance testing, caching

## Testing Strategies

### Dependency Testing Matrix

| Test Type | Focus | When |
|-----------|-------|------|
| Unit Tests | Mock dependencies | Development |
| Integration Tests | Real dependencies | Pre-deployment |
| Contract Tests | Interface compliance | Continuous |
| End-to-End Tests | Full dependency chain | Release |

### Critical Test Scenarios

1. **Subsystem Unavailable**
   - Test graceful degradation
   - Verify error handling
   - Check recovery procedures

2. **Slow Dependencies**
   - Test timeout handling
   - Verify async alternatives
   - Check user experience

3. **Data Inconsistencies**
   - Test reconciliation
   - Verify error detection
   - Check correction procedures

## Monitoring Requirements

### Dependency Health Metrics

| Metric | Threshold | Alert |
|--------|-----------|-------|
| Response Time | > 2 sec | Warning |
| Error Rate | > 1% | Critical |
| Availability | < 99.5% | Critical |
| Queue Depth | > 1000 | Warning |

### Dependency Dashboards

1. **Real-Time Monitoring**
   - Call volumes between subsystems
   - Response times per interface
   - Error rates by dependency

2. **Batch Monitoring**
   - Job dependencies met/missed
   - Processing times
   - Data volumes

## Future State Architecture

### Target Dependency Model

```
Microservices with API Gateway
├── Synchronous: REST/GraphQL
├── Asynchronous: Event streaming
└── Data: Service-specific databases

Dependency Injection
├── Configuration-driven
├── Service discovery
└── Circuit breakers

Monitoring
├── Distributed tracing
├── Service mesh
└── Dependency mapping
```

### Dependency Reduction Goals

1. Reduce coupling score by 50%
2. Eliminate synchronous chains > 3 deep
3. Achieve <5 dependencies per service
4. Enable independent deployment