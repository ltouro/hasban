# Hasban Architecture Documentation

## Overview

Hasban is a Haskell library and API for calculating amortization schedules using Brazilian financial methodologies. This document outlines the refactored architecture that addresses complexity issues and improves maintainability.

## Architectural Principles

### 1. Domain-Driven Design
- Clear separation between domain logic and technical concerns
- Rich domain model with proper type safety
- Business rules encoded in the type system

### 2. Layered Architecture
```
┌─────────────────────────────────────────┐
│               API Layer                 │
├─────────────────────────────────────────┤
│            Application Layer            │
├─────────────────────────────────────────┤
│              Domain Layer               │
├─────────────────────────────────────────┤
│           Infrastructure Layer          │
└─────────────────────────────────────────┘
```

### 3. Type Safety
- Newtype wrappers eliminate primitive obsession
- Smart constructors enforce business rules
- Compile-time guarantees for domain invariants

## Module Structure

### Domain Layer (`src/Domain/`)

#### `Domain.Types`
Core domain types with proper type safety:
- `CurrencyAmount`: Monetary values with validation
- `InterestRate`: Interest rates as percentages
- `PaymentNumber`: Payment sequence numbers
- `LoanTerms`: Number of payment periods

#### `Domain.Loan` 
Loan aggregate root with business rules:
- `Loan`: Validated loan entity
- `LoanRequest`: Input from external sources
- Comprehensive validation logic
- Commission calculations

#### `Domain.AmortizationSchedule`
Amortization calculation engine:
- `PaymentItem`: Individual payment details
- `AmortizationSchedule`: Complete payment schedule
- Support for both PRICE and SAC methods

#### `Domain.BrazilianTax`
Brazilian IOF tax calculations:
- IOF (Imposto sobre Operações Financeiras) implementation
- Compliance with Law 13.043/2014
- B3 methodology support

### Infrastructure Layer (`src/Infrastructure/`)

#### `Infrastructure.DateCalculator`
Financial date arithmetic:
- `FinancialDate`: Domain-specific date wrapper
- Payment schedule calculations
- Business day logic

#### `Infrastructure.InterestCalculator`
Interest calculation engine:
- Multiple day count conventions (360/365/252)
- Brazilian market standards
- Compound interest formulas

### Application Layer (`src/Application/`)

#### `Application.Errors`
Comprehensive error handling:
- Hierarchical error types
- Rich error context for debugging
- User-friendly error formatting

## Brazilian Financial Methodologies

### Amortization Methods

#### PRICE Method (Tabela Price)
- **Description**: Equal payment amounts (PMT constant)
- **Characteristics**: Higher interest payments at the beginning
- **Formula**: PMT = PV × [r(1+r)^n] / [(1+r)^n - 1]
- **Use Case**: Consumer loans, mortgages

#### SAC Method (Sistema de Amortização Constante)
- **Description**: Equal principal payments
- **Characteristics**: Decreasing payment amounts over time
- **Formula**: Principal payment = PV / n, Total payment = Principal + Interest
- **Use Case**: Corporate loans, financing

### Interest Calculation Modes

#### Day Count Conventions
1. **365/365 (Actual/Actual)**
   - Uses actual calendar days
   - Most accurate for irregular periods

2. **360/360 (30/360)**
   - Commercial year (360 days)
   - Standard for many financial products

3. **252/252 (Business Days)**
   - Brazilian standard using business days
   - Excludes weekends and holidays

### IOF Tax (Imposto sobre Operações Financeiras)

#### Tax Structure
- **Fixed IOF**: 0.38% applied once to principal
- **Daily IOF**: Variable rate applied daily
  - Individuals (Natural): 0.0082% per day
  - Companies (Legal): 0.0041% per day
- **Maximum Period**: 365 days

#### Calculation Formula
```
Total IOF = Fixed IOF + Daily IOF
Fixed IOF = Principal × 0.0038
Daily IOF = Principal × Daily Rate × min(Days, 365)
```

## Design Patterns and Best Practices

### 1. Smart Constructors
All domain types use smart constructors to enforce validation:
```haskell
mkCurrencyAmount :: Double -> Either String CurrencyAmount
mkInterestRate :: Double -> Either String InterestRate
```

### 2. Type-Safe Error Handling
Hierarchical error types provide specific context:
```haskell
data LoanCalculationError =
    ValidationErr ValidationError
  | CalculationErr CalculationError
  | BusinessRuleErr BusinessRuleError
  | SystemErr SystemError
```

### 3. Immutable Data Structures
All calculations produce new immutable values, ensuring:
- Thread safety
- Predictable behavior
- Easy testing and debugging

### 4. Functional Programming Principles
- Pure functions for calculations
- Monadic error handling
- Composition over inheritance

## Migration Strategy

### Phase 1: New Module Creation ✅
- Created new domain and infrastructure modules
- Maintained backward compatibility with legacy modules

### Phase 2: API Integration (In Progress)
- Update API layer to use new domain types
- Gradual migration of endpoints

### Phase 3: Legacy Module Removal
- Remove old modules once migration is complete
- Update tests and documentation

## Testing Strategy

### Unit Tests
- Property-based testing for mathematical functions
- Edge case validation for business rules
- Error condition testing

### Integration Tests
- End-to-end API testing with realistic data
- Brazilian regulation compliance tests
- Performance benchmarks

### Example Test Cases
```haskell
-- Test PRICE vs SAC payment differences
prop_price_vs_sac_payments :: LoanRequest -> Property

-- Test IOF calculation accuracy
prop_iof_calculation_accuracy :: PersonType -> CurrencyAmount -> Property

-- Test amortization schedule balancing
prop_schedule_balances_to_zero :: ValidLoan -> Property
```

## Performance Considerations

### Optimization Strategies
1. **Lazy Evaluation**: Payment schedules calculated on-demand
2. **Memoization**: Cache expensive calculations
3. **Precise Arithmetic**: Use appropriate precision for financial calculations

### Memory Management
- Strict evaluation for large schedules
- Streaming for very long-term loans
- Garbage collection considerations

## Regulatory Compliance

### Brazilian Standards
- **B3 (Brasil, Bolsa, Balcão)**: Exchange methodology compliance
- **ANBIMA**: Brazilian Financial and Capital Markets Association standards
- **BACEN**: Central Bank of Brazil regulations

### Audit Trail
- Comprehensive logging of calculations
- Error context preservation
- Input validation tracking

## Future Enhancements

### Planned Features
1. **Additional Amortization Methods**: 
   - American amortization
   - Bullet payments
   
2. **Enhanced Tax Support**:
   - Additional Brazilian taxes
   - Regional variations

3. **Performance Improvements**:
   - Parallel calculation support
   - GPU acceleration for large portfolios

4. **Integration Capabilities**:
   - External rate providers
   - Real-time tax rate updates

## Conclusion

The refactored architecture provides:
- **Clarity**: Clear separation of concerns and responsibilities
- **Maintainability**: Modular design allows for easy updates
- **Reliability**: Type safety and comprehensive error handling
- **Compliance**: Accurate implementation of Brazilian financial standards
- **Performance**: Efficient calculations with appropriate precision

This foundation supports both current requirements and future enhancements while maintaining the accuracy and reliability required for financial calculations.