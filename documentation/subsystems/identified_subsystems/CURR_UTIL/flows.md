# CURR_UTIL Subsystem - Business Flow Documentation

## Overview

This document details the currency and number processing flows within the CURR_UTIL subsystem, showing how financial calculations, currency conversions, and number formatting are performed consistently across the ACAS system.

## Process Flow Diagrams

### 1. Currency Conversion Flow

```mermaid
flowchart TD
    Start([Conversion Request]) --> A[Validate Currency Codes]
    A --> B{Currencies Valid?}
    B -->|No| C[Return Currency Error]
    B -->|Yes| D[Check Conversion Date]
    
    D --> E{Date Valid?}
    E -->|No| F[Use Current Date]
    E -->|Yes| G[Lookup Exchange Rate]
    
    F --> G
    G --> H{Rate Found?}
    H -->|No| I[Use Default Rate]
    H -->|Yes| J[Perform Conversion]
    
    I --> K[Log Rate Warning]
    K --> J
    
    J --> L[Apply Rounding Rules]
    L --> M[Validate Result]
    M --> N[Return Converted Amount]
    
    C --> End([Conversion Complete])
    N --> End
```

### 2. Financial Calculation Flow

```mermaid
sequenceDiagram
    participant Client as Calling Module
    participant Calculator as CURR_UTIL
    parameter Validator as Input Validator
    participant Engine as Calculation Engine
    
    Client->>Calculator: Financial Calculation Request
    Calculator->>Validator: Validate Parameters
    Validator->>Calculator: Validation Result
    
    alt Parameters Valid
        Calculator->>Engine: Execute Calculation
        Engine->>Engine: Apply Formula
        Engine->>Engine: Handle Precision
        Engine->>Calculator: Return Result
        Calculator->>Client: Calculation Complete
    else Invalid Parameters
        Calculator->>Client: Parameter Error
    end
```

### 3. Number Formatting Flow

```mermaid
flowchart LR
    subgraph "Input Processing"
        A[Number Input] --> B[Validate Number]
        B --> C[Determine Format Type]
    end
    
    subgraph "Format Application"
        D[Apply Decimal Places] --> E[Add Thousand Separators]
        E --> F[Apply Currency Symbol]
        F --> G[Handle Negative Format]
    end
    
    subgraph "Output Generation"
        H[Generate String] --> I[Validate Length]
        I --> J[Return Formatted Result]
    end
    
    C --> D
    G --> H
```

## Business Rules in Flows

### Currency Conversion Rules
- **RULE_CURR_001**: Exchange rates updated daily from authorized sources
- **RULE_CURR_002**: Historical rates maintained for 5 years
- **RULE_CURR_003**: Default rate applied when specific rate unavailable
- **RULE_CURR_004**: Conversion amounts rounded to currency precision

### Financial Calculation Rules
- **RULE_CALC_001**: Interest calculations use 365-day year unless specified
- **RULE_CALC_002**: Compound interest calculated to 4 decimal places
- **RULE_CALC_003**: Payment calculations rounded to nearest cent
- **RULE_CALC_004**: Depreciation calculated using straight-line method default

### Number Formatting Rules
- **RULE_FMT_001**: Financial amounts display 2 decimal places default
- **RULE_FMT_002**: Thousand separators applied for amounts >999
- **RULE_FMT_003**: Negative amounts displayed in parentheses
- **RULE_FMT_004**: Currency symbols positioned according to locale

### Rounding Rules
- **RULE_RND_001**: Banker's rounding (round to even) used for financial calculations
- **RULE_RND_002**: Tax calculations round up to nearest cent
- **RULE_RND_003**: Currency conversions round to target currency precision
- **RULE_RND_004**: Interest calculations maintain maximum precision until final result