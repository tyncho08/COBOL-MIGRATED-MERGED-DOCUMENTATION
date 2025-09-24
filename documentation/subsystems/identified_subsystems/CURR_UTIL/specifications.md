# Subsystem: CURR_UTIL - Currency & Number Processing

## Executive Summary

**Purpose**: The CURR_UTIL subsystem provides standardized currency handling, number formatting, mathematical calculations, and financial computations for the ACAS system, ensuring consistent numeric processing, accurate currency conversions, and proper rounding across all business functions.

**Business Value**: Ensures accurate financial calculations, provides consistent currency handling, eliminates rounding errors, supports multi-currency operations, and maintains precision in all numeric computations.

**Key Users**:
- All ACAS subsystems (via calculation APIs)
- Finance team (currency conversions, calculations)
- International operations (multi-currency support)
- Reporting systems (number formatting)

**Criticality**: HIGH - Essential for all financial calculations and currency operations

## Functional Capabilities

### Core Functions

1. **Currency Conversion and Management**
   - Description: Handle currency conversions, exchange rate management, and multi-currency calculations
   - Business Rules: Exchange rate sources, conversion timing, rounding rules
   - Triggers: Foreign currency transactions, rate updates, period-end revaluation
   - Outcomes: Accurate currency conversions with proper exchange rate tracking

2. **Precision Number Calculations**
   - Description: Perform high-precision mathematical calculations for financial operations
   - Business Rules: Precision requirements, rounding methods, overflow handling
   - Triggers: Financial calculations, interest computations, tax calculations
   - Outcomes: Precise numeric results with controlled rounding

3. **Number Formatting and Display**
   - Description: Format numbers for display according to locale and business requirements
   - Business Rules: Locale-specific formatting, decimal places, thousand separators
   - Triggers: Report generation, user interface display, document output
   - Outcomes: Consistently formatted numbers meeting display requirements

4. **Financial Formula Processing**
   - Description: Execute complex financial formulas including interest, depreciation, and discounts
   - Business Rules: Formula definitions, parameter validation, result verification
   - Triggers: Loan calculations, asset depreciation, discount processing
   - Outcomes: Accurate financial formula results with audit trails

### Business Processes Supported

- **Financial Calculations**: Interest, depreciation, loan calculations
- **Currency Operations**: Multi-currency transactions and conversions
- **Tax Calculations**: Complex tax computations and compliance
- **Pricing Operations**: Discount calculations and price conversions
- **Reporting**: Number formatting for financial statements

## Interface Contracts

### Internal APIs/Services

- **ConvertCurrency**: [Amount, FromCurrency, ToCurrency, Date] → [ConvertedAmount, ExchangeRate]
- **CalculateInterest**: [Principal, Rate, Term, CompoundType] → [Interest, TotalAmount]
- **FormatNumber**: [Number, Format, Locale] → [FormattedString]
- **RoundAmount**: [Amount, RoundingRule, DecimalPlaces] → [RoundedAmount]

## Known Limitations
- **Real-time Rates**: Limited real-time exchange rate integration
- **Complex Formulas**: Basic financial formula library only
- **Historical Rates**: Limited historical exchange rate storage