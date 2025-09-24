# CURR_UTIL Subsystem - Interface Documentation

## Overview

The CURR_UTIL subsystem provides standardized currency and number processing interfaces for all ACAS modules, ensuring consistent financial calculations and currency handling across the entire system.

## Core API Interfaces

### Currency Conversion Interface

```
CURRENCY-CONVERSION-REQUEST
├── AMOUNT                 (PIC S9(13)V99)
├── FROM-CURRENCY          (PIC X(3))
├── TO-CURRENCY            (PIC X(3))
├── CONVERSION-DATE        (PIC 9(8))
├── RATE-TYPE              (PIC X(1))
└── ROUNDING-RULE          (PIC X(1))

CURRENCY-CONVERSION-RESPONSE
├── CONVERTED-AMOUNT       (PIC S9(13)V99)
├── EXCHANGE-RATE          (PIC 9(7)V9999)
├── RATE-SOURCE            (PIC X(10))
├── RATE-DATE              (PIC 9(8))
├── CONVERSION-STATUS      (PIC X(1))
└── ROUNDING-APPLIED       (PIC S9(3)V99)
```

### Number Formatting Interface

```
NUMBER-FORMAT-REQUEST
├── INPUT-NUMBER           (PIC S9(15)V9999)
├── FORMAT-TYPE            (PIC X(10))
├── DECIMAL-PLACES         (PIC 9(2))
├── THOUSAND-SEPARATOR     (PIC X(1))
├── CURRENCY-SYMBOL        (PIC X(3))
├── LOCALE-CODE            (PIC X(5))
└── NEGATIVE-FORMAT        (PIC X(5))

NUMBER-FORMAT-RESPONSE
├── FORMATTED-STRING       (PIC X(30))
├── FORMAT-STATUS          (PIC X(1))
├── ACTUAL-DECIMAL-PLACES  (PIC 9(2))
└── FORMAT-APPLIED         (PIC X(10))
```

### Financial Calculation Interface

```
FINANCIAL-CALC-REQUEST
├── CALCULATION-TYPE       (PIC X(10))
├── PRINCIPAL-AMOUNT       (PIC S9(13)V99)
├── INTEREST-RATE          (PIC 9(3)V9999)
├── TERM-PERIODS           (PIC 9(4))
├── COMPOUND-FREQUENCY     (PIC 9(2))
├── PAYMENT-FREQUENCY      (PIC 9(2))
└── CALCULATION-DATE       (PIC 9(8))

FINANCIAL-CALC-RESPONSE
├── CALCULATED-RESULT      (PIC S9(15)V99)
├── INTEREST-AMOUNT        (PIC S9(13)V99)
├── TOTAL-AMOUNT           (PIC S9(15)V99)
├── PAYMENT-AMOUNT         (PIC S9(11)V99)
├── CALCULATION-STATUS     (PIC X(1))
└── FORMULA-USED           (PIC X(20))
```

### Rounding and Precision Interface

```
ROUNDING-REQUEST
├── INPUT-AMOUNT           (PIC S9(15)V9999)
├── ROUNDING-METHOD        (PIC X(1))
├── DECIMAL-PLACES         (PIC 9(2))
├── ROUNDING-INCREMENT     (PIC 9(3)V9999)
└── CURRENCY-CODE          (PIC X(3))

ROUNDING-RESPONSE
├── ROUNDED-AMOUNT         (PIC S9(13)V99)
├── ROUNDING-DIFFERENCE    (PIC S9(7)V9999)
├── ROUNDING-STATUS        (PIC X(1))
└── METHOD-APPLIED         (PIC X(10))
```

## Standard Calculation Functions

### Basic Mathematical Operations
- **ADD-AMOUNTS**: Multi-precision addition
- **SUBTRACT-AMOUNTS**: Multi-precision subtraction
- **MULTIPLY-AMOUNTS**: Multi-precision multiplication
- **DIVIDE-AMOUNTS**: Multi-precision division with rounding
- **PERCENTAGE-CALC**: Percentage calculations
- **RATIO-CALC**: Ratio and proportion calculations

### Financial Functions
- **SIMPLE-INTEREST**: Simple interest calculations
- **COMPOUND-INTEREST**: Compound interest calculations
- **PRESENT-VALUE**: Present value calculations
- **FUTURE-VALUE**: Future value calculations
- **ANNUITY-CALC**: Annuity payment calculations
- **DEPRECIATION-CALC**: Depreciation calculations

### Currency Functions
- **CURRENCY-CONVERT**: Currency conversion with exchange rates
- **RATE-LOOKUP**: Exchange rate retrieval
- **MULTI-CURRENCY-ADD**: Addition of different currencies
- **REVALUATION**: Currency revaluation calculations