# CETIP21 Interest Calculation Methods - Updated Implementation Guide

This document provides the complete implementation guide for the 6 CETIP21-compliant interest calculation methods as specified in the official CCB/CCE/NCE CETIP21 documentation. These methods apply to both PRICE (Tabela Price) and SAC amortization types with full regulatory compliance.

## Overview

**IMPORTANT CLARIFICATION**: This document covers interest calculation criteria, not amortization methods. The 6 calculation methods can be applied to any amortization type:

- **PRICE (Tabela Price)**: Fixed payment amount, varying principal/interest split
- **SAC (Sistema de Amortização Constante)**: Fixed principal, varying payment amount

The 6 calculation methods determine how interest factors are computed based on different day count conventions and calculation methodologies for fixed interest rates (or spread). These are used for prefixed or post-fixed assets in the Brazilian financial market.

## The 6 Interest Calculation Methods

### 1. 360 Days (Calendar Days)

**Base Formula (Compound)**: `Fator de Juros = (i/100 + 1)^(dcp/dct)`

**Linear Formula (Alternative)**: `Fator de Juros = 1 + (i × N)/36000`

Where:
- `i`: Annual interest rate in 360 calendar days (4 decimal places)
- `dcp`: Number of calendar days in the current period
- `dct`: Total number of calendar days in the reference period (usually equals dcp for simple cases)
- `N`: Number of calendar days (for linear calculation)

**Monthly Calculation (Matched Dates)**: `(i/100 + 1)^(N°Meses×30/360)`

**Monthly Calculation (Mismatched Dates)**: Requires pro-rata calculation (see Pro-rata section)

**Usage**: Calendar days calculation with 360-day year convention (corporate bonds, some commercial paper)

### 2. 365 Days (Calendar Days)

**Base Formula**: `Fator de Juros = (i/100 + 1)^(dcp/dct)`

Where:
- `i`: Annual interest rate in 365 calendar days (4 decimal places)
- `dcp`: Number of calendar days in the current period
- `dct`: Total number of calendar days in the reference period

**Monthly Calculation (Matched Dates)**: `(i/100 + 1)^(N°Meses×30/365)`

**Monthly Calculation (Mismatched Dates)**: Requires pro-rata calculation

**Usage**: Calendar days calculation with actual 365-day year convention (government securities, some bonds)

### 3. 252 Days (Business Days)

**Base Formula**: `Fator de Juros = (i/100 + 1)^(dup/dut)`

Where:
- `i`: Annual interest rate in 252 business days (4 decimal places)
- `dup`: Number of business days in the current period
- `dut`: Total number of business days in the reference period

**Monthly Calculation (Matched Dates)**: `(i/100 + 1)^(N°Meses×21/252)`

**Monthly Calculation (Mismatched Dates)**: Requires pro-rata calculation with business day adjustments

**Important**: Requires Brazilian business day calendar (excludes weekends and Brazilian holidays)

**Usage**: Interbank market instruments, DI-linked securities, most Brazilian fixed-income securities

### 4. 360 Months (30/360)

**Formula**: `Fator de Juros = (i/100 + 1)^(N°Meses×30/360)`

**Pro-rata Formula (Mismatched Dates)**:
```
Fator de Juros = ((i/100 + 1)^(30/360))^(dcp_pro-rata/dct_pro-rata) × ((i/100 + 1)^(N°Meses×30/360))^(dcp/dct)
```

Where:
- `i`: Annual interest rate based on 360-day year (4 decimal places)
- `N°Meses`: Number of complete months in the period
- Each month standardized as 30 days
- `dcp_pro-rata`: Calendar days in pro-rata period
- `dct_pro-rata`: Total calendar days in pro-rata reference period

**Usage**: Monthly payment cycles with standardized 30-day months (consumer loans, some mortgages)

### 5. 365 Months (30/365)

**Formula**: `Fator de Juros = (i/100 + 1)^(N°Meses×30/365)`

**Pro-rata Formula (Mismatched Dates)**:
```
Fator de Juros = ((i/100 + 1)^(30/365))^(dcp_pro-rata/dct_pro-rata) × ((i/100 + 1)^(N°Meses×30/365))^(dcp/dct)
```

Where:
- `i`: Annual interest rate based on 365-day year (4 decimal places)
- `N°Meses`: Number of complete months in the period
- Each month standardized as 30 days
- Uses actual 365-day year base

**Usage**: Monthly payment cycles with actual year convention (some government programs, standardized loans)

### 6. 252 Months (21/252)

**Formula**: `Fator de Juros = (i/100 + 1)^(N°Meses×21/252)`

**Pro-rata Formula (Mismatched Dates)**:
```
Fator de Juros = ((i/100 + 1)^(21/252))^(dup_pro-rata/dut_pro-rata) × ((i/100 + 1)^(N°Meses×21/252))^(dup/dut)
```

Where:
- `i`: Annual interest rate based on 252 business days (4 decimal places)
- `N°Meses`: Number of complete months in the period
- Each month standardized as 21 business days
- `dup_pro-rata`: Business days in pro-rata period
- `dut_pro-rata`: Total business days in pro-rata reference period

**Important**: Requires accurate business day counting with Brazilian holiday calendar

**Usage**: Monthly business-day-based instruments (corporate loans tied to DI rates, some bonds)

## Key Calculation Components

### Interest Factor Calculation
All schedules use the base formula structure:
```
Fator de Juros = (1 + i/100)^(time_factor)
```

The `time_factor` varies by schedule type:
- **Days-based**: `days_in_period / days_in_year`
- **Months-based**: `months × standard_days_per_month / days_in_year`

### Unit Interest Value
```
J = VNA × (Fator de Juros - 1)
```

Where:
- `J`: Unit interest value (8 decimal places, no rounding)
- `VNA`: Updated nominal value (8 decimal places, no rounding)
- `Fator de Juros`: Interest factor (9 decimal places, with rounding)

### Financial Interest Value
```
JVF = J × Q
```

Where:
- `JVF`: Financial interest value (2 decimal places, no rounding)
- `J`: Unit interest value (8 decimal places, no rounding)
- `Q`: Quantity of assets in custody

## Pro-rata Calculations

Pro-rata calculations are required when the issue date (or start of profitability date) doesn't align with payment anniversary dates. This is critical for CETIP21 compliance.

### When Pro-rata Applies

1. **Date Mismatching**: Issue date day ≠ maturity date day
2. **First Payment Calculation**: From issue to first anniversary
3. **Irregular Periods**: Any period that doesn't match the standard calculation cycle

### Calendar Days Pro-rata Formula

For methods 1, 2, 4, 5 (360/365 Days and Months):
```
C = ((NIm1/NIm0)^(dcp_pro-rata/dct_pro-rata)) × (NImn/NIm1)
```

Where:
- `NIm1`: Index number for month 1 (determined by complex CETIP21 rules)
- `NIm0`: Index number for month 0 (base month)
- `NImn`: Index number for calculation month
- `dcp_pro-rata`: Calendar days from issue date to first anniversary
- `dct_pro-rata`: Total calendar days in the anniversary period

### Business Days Pro-rata Formula

For methods 3, 6 (252 Days and Months):
```
C = ((NIm1/NIm0)^(dup_pro-rata/dut_pro-rata)) × (NImn/NIm1)
```

Where:
- `dup_pro-rata`: Business days from issue date to first anniversary
- `dut_pro-rata`: Total business days in the anniversary period
- Other variables same as calendar days version

### CETIP21 Pro-rata Index Determination Rules

**For monthly anniversary occurring in next month after issue**:
- Month 1 = Issue month
- Month 0 = Month immediately before issue month

**For monthly anniversary occurring in same month as issue**:
- Month 1 = Month immediately before issue month
- Month 0 = Second month before issue month

**Critical**: These rules ensure proper index-based corrections are applied for pricing mismatches.

### Delayed Publication Correction ("Correção Defasada")

**MANDATORY for IGP-DI, INPC, and IPCA indices**:

When payment/maturity dates fall before the 15th of the month:
- Use index number from **second month prior** to calculation month
- This accounts for index publication delays (typically published around day 10 ± 4)

**Implementation Rule**:
```
IF (payment_day < 15 AND index IN [IGP-DI, INPC, IPCA]) THEN
    use_index_month = calculation_month - 2
ELSE
    use_index_month = calculation_month - 1
```

**IGP-M Exception**: Always uses immediate prior month (published in same month)

## CETIP21 Precision Standards (MANDATORY)

- **Interest factors**: 9 decimal places WITH rounding (CETIP21 requirement)
- **Unit values**: 8 decimal places WITHOUT rounding (CETIP21 requirement)
- **Financial values**: 2 decimal places WITHOUT rounding (CETIP21 requirement)
- **Pro-rata ratios**: 9 decimal places WITHOUT rounding (CETIP21 requirement)
- **Interest rates**: Input with 4 decimal places maximum

### Critical Precision Implementation Notes:
- All rounding follows banker's rounding (round to even)
- Precision truncation (no rounding) means values are cut at specified decimal places
- Each calculation step must maintain intermediate precision before final rounding

## Date Matching and Validation Rules

### CETIP21 Date Matching Requirements

1. **Monthly Calculations**: For methods 4, 5, 6 (Months-based), issue date and maturity date should have matching days when possible
2. **Grace Period Handling**: For assets with grace periods, incorporation of interest must occur before first payment
3. **End-of-Month Rules**: Comprehensive handling for month-end dates
4. **Business Day Adjustments**: Payments on non-business days follow "Following Business Day" convention

### End-of-Month Date Matching Rules (CETIP21 Table)

| Issue Date | Maturity Date | Event Generation Rule |
|------------|---------------|----------------------|
| 28/02 | 28/02 | Day 28 each month |
| 28/02 | 29/02 | Last day each month |
| 28/02 | 30/mm | Last day each month |
| 28/02 | 31/mm | Last day each month |
| 29/02 | 29/02 | Day 29 each month* |
| 29/02 | 30/mm | Last day each month |
| 29/02 | 31/mm | Last day each month |
| 30/mm | 30/mm | Day 30 each month* |
| 30/mm | 31/mm | Last day each month |
| 31/mm | 31/mm | Last day each month |

*Except February: use day 28 (or 29 in leap years)

**Implementation Rule**: All combinations in this table are considered "matched dates" for CETIP21 purposes.

### Linear vs Compound Calculation Choice

**360-Day Linear** (`1 + (i × N)/36000`):
- Simpler calculation
- Less precise for longer periods
- Used for some short-term commercial paper

**360-Day Compound** (`(i/100 + 1)^(dcp/360)`):
- More precise
- Standard for most instruments
- Required for regulatory compliance

## Integration with Amortization Types

### PRICE (Tabela Price) Integration

```
1. Calculate interest factor using one of the 6 methods
2. Apply to current outstanding balance
3. Fixed payment amount = Principal + Interest
4. Principal portion increases over time
```

### SAC Integration

```
1. Calculate interest factor using one of the 6 methods
2. Apply to current outstanding balance
3. Fixed principal amount per period
4. Total payment = Fixed Principal + Variable Interest
```

## Implementation Considerations

### Brazilian Market Specifics

1. **Holiday Calendar**: Must include Brazilian national and financial market holidays
2. **DI Rate Integration**: 252-day methods often tied to DI (Depósito Interbancário) rates
3. **Regulatory Reporting**: CETIP21 precision requirements for regulatory submissions
4. **Tax Calculations**: IOF (Imposto sobre Operações Financeiras) integration

### Grace Period Implementation (CETIP21 Required)

**Grace Period with Interest Incorporation**:
```
VNE_adjusted = VNE + VNE × (Fator_de_Juros_Carencia - 1)
```

Where:
- `VNE`: Original nominal value
- `Fator_de_Juros_Carencia`: Interest factor for grace period
- `VNE_adjusted`: New basis for payment calculations

**Grace Period Rules**:
1. **Incorporation Date**: Must be exactly one period before first payment
2. **Date Matching**: Grace period end must match payment anniversary dates
3. **Calculation Method**: Use same interest calculation method as main payments
4. **Precision**: Apply 8 decimal places without rounding to adjusted VNE

### Index Data Integration Requirements

**Required Data Sources**:
- **IGP-M**: CETIP/FGV - published monthly around day 10
- **IGP-DI**: CETIP/FGV - published monthly around day 10 ± 4
- **INPC**: IBGE - published monthly around day 10 ± 4
- **IPCA**: IBGE - published monthly around day 10 ± 4
- **USD Commercial**: BACEN PTAX800 - daily

**Data Availability Rules**:
- Implement fallback mechanisms for delayed publications
- Cache index values for performance
- Validate index continuity and detect gaps

### Performance Optimization

1. **Pre-computed Factors**: Cache frequently used interest factors
2. **Business Day Calendars**: Pre-compute business day sequences
3. **Pro-rata Tables**: Pre-calculate common pro-rata scenarios
4. **Precision Optimization**: Balance precision requirements with calculation speed

## Application Examples

### Method Selection by Instrument Type

- **252 Days**: DI-linked CDB, CCB for interbank rates
- **360 Days**: Corporate bonds, commercial paper
- **365 Days**: Government bonds (some types), inflation-linked securities
- **252 Months**: Corporate loans with monthly payments tied to business day rates
- **360 Months**: Consumer loans, standardized monthly payment instruments
- **365 Months**: Government lending programs, some mortgages

### Typical Combinations

1. **Corporate Loan**: PRICE amortization + 252 Months interest calculation
2. **Government Bond**: SAC amortization + 365 Days interest calculation
3. **Commercial Paper**: Bullet payment + 360 Days (linear) calculation
4. **Consumer Credit**: PRICE amortization + 360 Months calculation

This calculation framework ensures consistency and regulatory compliance for Brazilian financial instruments under the CETIP21 system while providing flexibility for different market segments and instrument types.