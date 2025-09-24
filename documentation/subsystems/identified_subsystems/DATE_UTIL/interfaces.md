# DATE_UTIL Subsystem - Interface Documentation

## Overview

The DATE_UTIL subsystem provides standardized date and calendar services to all ACAS modules through a comprehensive API interface, ensuring consistent date handling and business day calculations across the entire system.

## Core API Interfaces

### Date Validation Interface

```
DATE-VALIDATION-REQUEST
├── INPUT-DATE-STRING       (PIC X(20))
├── INPUT-FORMAT           (PIC X(10))
├── VALIDATION-LEVEL       (PIC X(1))
└── CENTURY-HANDLING       (PIC X(1))

DATE-VALIDATION-RESPONSE
├── VALIDATION-STATUS      (PIC X(1))
├── STANDARD-DATE          (PIC 9(8))
├── ERROR-CODE             (PIC X(5))
├── ERROR-MESSAGE          (PIC X(80))
└── SUGGESTED-CORRECTION   (PIC X(20))
```

### Business Day Calculation Interface

```
BUSINESS-DAY-REQUEST
├── START-DATE             (PIC 9(8))
├── BUSINESS-DAYS-COUNT    (PIC S9(4))
├── CALENDAR-ID            (PIC X(3))
├── DIRECTION              (PIC X(1))
└── INCLUDE-START-DATE     (PIC X(1))

BUSINESS-DAY-RESPONSE
├── RESULT-DATE            (PIC 9(8))
├── ACTUAL-DAYS-ADDED      (PIC S9(4))
├── HOLIDAYS-SKIPPED       (PIC 9(3))
├── WEEKENDS-SKIPPED       (PIC 9(3))
└── CALCULATION-STATUS     (PIC X(1))
```

### Holiday Calendar Interface

```
HOLIDAY-DEFINITION
├── HOLIDAY-ID             (PIC X(6))
├── HOLIDAY-NAME           (PIC X(50))
├── HOLIDAY-TYPE           (PIC X(1))
├── HOLIDAY-DATE           (PIC 9(8))
├── CALENDAR-ID            (PIC X(3))
├── OBSERVANCE-RULE        (PIC X(10))
└── BUSINESS-IMPACT        (PIC X(1))
```

## Integration Patterns

All subsystems use standardized date service calls:

```
CALL "DATE-VALIDATE" USING
  input-date-string
  input-format
RETURNING
  validation-result
  standard-date
  error-code

CALL "BUSINESS-DAYS-ADD" USING
  start-date
  business-days
  calendar-id
RETURNING
  result-date
  status-code
```