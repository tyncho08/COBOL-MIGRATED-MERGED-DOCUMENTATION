# BATCH_FW Subsystem - Interface Documentation

## Overview

The BATCH_FW subsystem acts as the central orchestration layer for all batch processing in ACAS. It interfaces with every other subsystem to coordinate scheduled processing, period-end activities, and system maintenance tasks.

## Interface Architecture

```
                    ┌─────────────────┐
                    │  System Clock   │
                    │   & Scheduler   │
                    └────────┬────────┘
                             │
                             ▼
    ┌────────────────────────────────────────────────┐
    │                   BATCH_FW                      │
    │  ┌──────────┐  ┌──────────┐  ┌──────────┐     │
    │  │   Job    │  │  Status  │  │ Recovery │     │
    │  │ Control  │  │ Monitor  │  │ Manager  │     │
    │  └──────────┘  └──────────┘  └──────────┘     │
    └────────────────────┬───────────────────────────┘
                         │
     ┌──────────┬────────┴────────┬────────┬────────┐
     ▼          ▼                 ▼        ▼        ▼
┌─────────┐ ┌─────────┐    ┌─────────┐ ┌──────┐ ┌────────┐
│ GL_CORE │ │ AR_MGMT │    │ AP_MGMT │ │ INV  │ │  RPT   │
│         │ │         │    │         │ │ CTRL │ │ ENGINE │
└─────────┘ └─────────┘    └─────────┘ └──────┘ └────────┘
```

## Detailed Interface Specifications

### 1. System Clock Integration

**Interface ID**: BATCH_CLOCK  
**Type**: Event-driven  
**Direction**: Inbound to BATCH_FW  
**Frequency**: Continuous monitoring

#### Data Contract

```
TIME-TRIGGER-EVENT
├── TRIGGER-TIMESTAMP       (PIC 9(14)) YYYYMMDDHHMMSS
├── TRIGGER-TYPE           (PIC X(1))
│   "T" = Time-based
│   "D" = Date-based
│   "C" = Calendar-based
├── SCHEDULE-ID            (PIC X(10))
└── TRIGGER-PARAMETERS     (PIC X(100))
```

#### Processing Rules
- Evaluate all active schedules every minute
- Check for calendar exceptions (holidays, weekends)
- Apply timezone adjustments if configured
- Queue eligible jobs for execution

### 2. Job Submission Interface

**Interface ID**: BATCH_SUBMIT  
**Type**: Synchronous Call  
**Direction**: Inbound from all subsystems  
**Frequency**: On-demand

#### Data Contract

```
JOB-SUBMISSION-REQUEST
├── REQUEST-HEADER
│   ├── REQUESTING-USER     (PIC X(8))
│   ├── REQUEST-TIMESTAMP   (PIC 9(14))
│   ├── PRIORITY           (PIC 9(2)) 01-99
│   └── EXECUTION-MODE     (PIC X(1))
│       "I" = Immediate
│       "S" = Scheduled
│       "H" = Hold
└── JOB-DETAILS
    ├── JOB-ID             (PIC X(10))
    ├── PROGRAM-NAME       (PIC X(8))
    ├── PARAMETERS         (PIC X(200))
    ├── RESTART-POINT      (PIC X(20))
    └── NOTIFICATION-LIST  (PIC X(100))

JOB-SUBMISSION-RESPONSE
├── RESPONSE-CODE          (PIC X(2))
│   "00" = Success
│   "10" = Job not found
│   "20" = Not authorized
│   "30" = Window closed
│   "99" = System error
├── RUN-ID                 (PIC 9(10))
├── ESTIMATED-START        (PIC 9(14))
└── MESSAGE                (PIC X(80))
```

### 3. Module Batch Jobs Registry

**Interface ID**: BATCH_REGISTRY  
**Type**: Configuration  
**Direction**: Inbound from all subsystems

#### GL_CORE Batch Jobs

```
GL-BATCH-JOBS
├── GL900-MONTH-END
│   ├── PROGRAM: "gl900"
│   ├── SCHEDULE: "MONTH-END+0"
│   ├── DEPENDENCIES: ["SL900", "PL900", "ST900"]
│   └── CRITICAL: "Y"
├── GL910-YEAR-END
│   ├── PROGRAM: "gl910"
│   ├── SCHEDULE: "YEAR-END+0"
│   ├── DEPENDENCIES: ["GL900-12"]
│   └── CRITICAL: "Y"
└── GL050-TRIAL-BALANCE
    ├── PROGRAM: "gl050"
    ├── SCHEDULE: "DAILY-2300"
    ├── DEPENDENCIES: []
    └── CRITICAL: "N"
```

#### AR_MGMT Batch Jobs

```
AR-BATCH-JOBS
├── SL800-DAILY-PROCESS
│   ├── PROGRAM: "sl800"
│   ├── SCHEDULE: "DAILY-2000"
│   ├── DEPENDENCIES: []
│   └── CRITICAL: "Y"
├── SL810-AUTO-INVOICE
│   ├── PROGRAM: "sl810"
│   ├── SCHEDULE: "DAILY-2100"
│   ├── DEPENDENCIES: ["SL800"]
│   └── CRITICAL: "Y"
├── SL900-MONTH-END
│   ├── PROGRAM: "sl900"
│   ├── SCHEDULE: "MONTH-END-1"
│   ├── DEPENDENCIES: ["SL810"]
│   └── CRITICAL: "Y"
└── SL110-STATEMENTS
    ├── PROGRAM: "sl110"
    ├── SCHEDULE: "MONTHLY-01"
    ├── DEPENDENCIES: ["SL900"]
    └── CRITICAL: "N"
```

#### AP_MGMT Batch Jobs

```
AP-BATCH-JOBS
├── PL810-MATCH-INVOICES
│   ├── PROGRAM: "pl810"
│   ├── SCHEDULE: "DAILY-1900"
│   ├── DEPENDENCIES: []
│   └── CRITICAL: "N"
├── PL055-PAYMENT-RUN
│   ├── PROGRAM: "pl055"
│   ├── SCHEDULE: "WEEKLY-WED-1000"
│   ├── DEPENDENCIES: []
│   └── CRITICAL: "Y"
└── PL900-MONTH-END
    ├── PROGRAM: "pl900"
    ├── SCHEDULE: "MONTH-END-1"
    ├── DEPENDENCIES: ["PL810"]
    └── CRITICAL: "Y"
```

### 4. Job Status Monitoring

**Interface ID**: BATCH_MONITOR  
**Type**: Query/Response  
**Direction**: Bidirectional  
**Frequency**: Real-time

#### Data Contract

```
JOB-STATUS-QUERY
├── QUERY-TYPE             (PIC X(1))
│   "R" = Running jobs
│   "H" = History
│   "S" = Scheduled
│   "F" = Failed
├── RUN-ID                 (PIC 9(10)) Optional
├── JOB-ID                 (PIC X(10)) Optional
└── DATE-RANGE
    ├── FROM-DATE          (PIC 9(8))
    └── TO-DATE            (PIC 9(8))

JOB-STATUS-RESPONSE
├── RESPONSE-COUNT         (PIC 9(4))
└── JOB-STATUS-RECORDS (OCCURS 1-9999)
    ├── RUN-ID             (PIC 9(10))
    ├── JOB-ID             (PIC X(10))
    ├── STATUS             (PIC X(1))
    │   "Q" = Queued
    │   "R" = Running
    │   "C" = Complete
    │   "F" = Failed
    │   "H" = Hold
    ├── START-TIME         (PIC 9(14))
    ├── END-TIME           (PIC 9(14))
    ├── RECORDS-PROCESSED  (PIC 9(10))
    ├── ERROR-COUNT        (PIC 9(6))
    └── COMPLETION-PCT     (PIC 999)
```

### 5. Checkpoint and Recovery

**Interface ID**: BATCH_CHECKPOINT  
**Type**: File-based  
**Direction**: Bidirectional  
**Frequency**: As configured per job

#### Data Contract

```
CHECKPOINT-RECORD
├── CHECKPOINT-HEADER
│   ├── RUN-ID             (PIC 9(10))
│   ├── JOB-ID             (PIC X(10))
│   ├── CHECKPOINT-ID      (PIC 9(6))
│   ├── CHECKPOINT-TIME    (PIC 9(14))
│   └── PROGRAM-STATUS     (PIC X(1))
└── CHECKPOINT-DATA
    ├── LAST-KEY-PROCESSED (PIC X(50))
    ├── RECORD-COUNT       (PIC 9(10))
    ├── CONTROL-TOTALS     (PIC S9(13)V99 OCCURS 10)
    ├── PROGRAM-COUNTERS   (PIC 9(10) OCCURS 20)
    └── USER-DATA          (PIC X(500))
```

### 6. Period-End Orchestration

**Interface ID**: BATCH_PERIOD  
**Type**: Orchestration  
**Direction**: Outbound to all modules  
**Frequency**: Monthly/Yearly

#### Month-End Sequence

```
MONTH-END-ORCHESTRATION
├── PHASE-1: PRE-VALIDATION
│   ├── Check all daily batches complete
│   ├── Verify no open transactions
│   └── Confirm user authorization
├── PHASE-2: MODULE-CLOSURE
│   ├── Execute SL900 (Sales month-end)
│   ├── Execute PL900 (Purchase month-end)
│   ├── Execute ST900 (Stock month-end)
│   └── Wait for all completions
├── PHASE-3: GL-CLOSURE
│   ├── Execute GL900 (GL month-end)
│   ├── Generate trial balance
│   └── Create period journals
└── PHASE-4: NEW-PERIOD
    ├── Open new period
    ├── Reset counters
    └── Schedule recurring entries
```

### 7. Alert and Notification

**Interface ID**: BATCH_ALERT  
**Type**: Message Queue  
**Direction**: Outbound  
**Frequency**: Event-driven

#### Data Contract

```
BATCH-ALERT-MESSAGE
├── ALERT-HEADER
│   ├── ALERT-ID           (PIC X(10))
│   ├── ALERT-TIME         (PIC 9(14))
│   ├── SEVERITY           (PIC X(1))
│   │   "I" = Info
│   │   "W" = Warning
│   │   "E" = Error
│   │   "C" = Critical
│   └── SOURCE-RUN-ID      (PIC 9(10))
└── ALERT-DETAILS
    ├── JOB-ID             (PIC X(10))
    ├── MESSAGE            (PIC X(200))
    ├── ACTION-REQUIRED    (PIC X(1)) Y/N
    └── NOTIFICATION-LIST  (PIC X(100))
```

## Integration Patterns

### Pattern 1: Job Dependencies

```
DEPENDENCY-CHECK
  PERFORM UNTIL ALL-DEPENDENCIES-MET
    FOR EACH dependency IN job-dependencies
      CALL "BATCH_MONITOR" USING dependency-id
      IF dependency-status NOT = "C"
        SET dependency-not-met TO TRUE
      END-IF
    END-FOR
    IF dependency-not-met
      WAIT 60 SECONDS
    END-IF
  END-PERFORM
```

### Pattern 2: Restart Logic

```
RESTART-PROCESSING
  CALL "BATCH_CHECKPOINT" USING "READ" run-id
  IF checkpoint-found
    SET restart-mode TO TRUE
    MOVE checkpoint-last-key TO start-key
    MOVE checkpoint-count TO record-count
  ELSE
    SET normal-mode TO TRUE
    INITIALIZE processing-fields
  END-IF
```

### Pattern 3: Error Recovery

```
ERROR-RECOVERY
  ON ERROR
    ADD 1 TO retry-count
    IF retry-count <= max-retries
      WAIT retry-delay SECONDS
      RETRY OPERATION
    ELSE
      CALL "BATCH_ALERT" USING critical-alert
      SET job-status TO "F"
      STOP RUN
    END-IF
  END-ERROR
```

## Monitoring and Control

### Performance Metrics

```
JOB-PERFORMANCE-METRICS
├── RUNTIME-METRICS
│   ├── ACTUAL-RUNTIME      (PIC 9(6)) seconds
│   ├── AVERAGE-RUNTIME     (PIC 9(6))
│   ├── VARIANCE-PCT        (PIC S999V9)
│   └── TREND-INDICATOR     (PIC X(1)) U/D/S
├── RESOURCE-METRICS
│   ├── CPU-SECONDS         (PIC 9(8))
│   ├── IO-OPERATIONS       (PIC 9(10))
│   ├── MEMORY-PEAK-MB      (PIC 9(6))
│   └── TEMP-SPACE-MB       (PIC 9(6))
└── THROUGHPUT-METRICS
    ├── RECORDS-PER-SECOND  (PIC 9(6))
    ├── TRANSACTIONS-HOUR   (PIC 9(8))
    └── EFFICIENCY-SCORE    (PIC 999V9)
```

### Control Commands

```
BATCH-CONTROL-COMMANDS
├── PAUSE-JOB
│   └── Suspend execution maintaining state
├── RESUME-JOB
│   └── Continue from pause point
├── CANCEL-JOB
│   └── Terminate with rollback
├── PRIORITY-ADJUST
│   └── Change job priority dynamically
└── DEPENDENCY-OVERRIDE
    └── Force dependency satisfied
```

## Security Considerations

1. **Job Authorization**
   - User must have role to submit job
   - Special authorization for critical jobs
   - Audit trail of all submissions

2. **Parameter Validation**
   - Sanitize all job parameters
   - Prevent injection attacks
   - Validate against allowed values

3. **Resource Limits**
   - CPU time limits per job
   - Memory allocation caps
   - Disk space quotas

## Best Practices

1. **Job Design**
   - Include restart capability
   - Implement checkpoint logic
   - Handle partial processing

2. **Schedule Management**
   - Avoid overlapping critical jobs
   - Build in buffer time
   - Plan for failure scenarios

3. **Monitoring**
   - Set up proactive alerts
   - Monitor trending patterns
   - Review performance regularly

4. **Documentation**
   - Document all job parameters
   - Maintain dependency maps
   - Keep runbooks updated