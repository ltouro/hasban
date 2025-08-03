# Migration Guide: Old to New Architecture

## Overview

This guide helps you migrate from the old monolithic `LoanCalculator` module to the new modular architecture. The new design provides better type safety, clearer error handling, and improved maintainability while preserving all existing functionality.

## Key Changes

### 1. Type Safety Improvements

#### Old API (Primitive Types)
```haskell
-- Old: Using raw Double and Int types
data LoanQuery = LoanQuery
  { loanAmount :: Double
  , interestRate :: Double
  , loanTerms :: Int
  , ...
  }
```

#### New API (Type-Safe Wrappers)
```haskell
-- New: Using domain-specific types
data LoanRequest = LoanRequest
  { lrAmount :: Double  -- Still accepts raw input
  , lrInterestRate :: Double
  , lrLoanTerms :: Int
  , ...
  }

-- But internally uses type-safe domain types
data Loan = Loan
  { loanAmount :: CurrencyAmount
  , interestRate :: InterestRate
  , loanTerms :: LoanTerms
  , ...
  }
```

### 2. Error Handling

#### Old API (Simple Validation)
```haskell
-- Old: Basic validation errors
data LoanQueryValidationError =
    InvalidStartDate
  | InvalidGracePeriod
  | InvalidInterestRate
  | ...

validateAndCalculate :: LoanQuery -> Either [LoanQueryValidationError] LoanQueryResult
```

#### New API (Rich Error Context)
```haskell
-- New: Hierarchical error types with context
data LoanCalculationError =
    ValidationErr ValidationError
  | CalculationErr CalculationError
  | BusinessRuleErr BusinessRuleError
  | SystemErr SystemError

validateLoan :: LoanRequest -> Either [LoanValidationError] Loan
```

### 3. Date Handling

#### Old API
```haskell
-- Old: Custom Date type
data Date = Date Integer Int Int

addTerm :: Date -> Frequency -> Int -> Date
```

#### New API
```haskell
-- New: Financial domain-specific dates
newtype FinancialDate = FinancialDate Day

data PaymentSchedule = PaymentSchedule
  { frequency :: PaymentFrequency
  , periodsPerTerm :: Int
  }

addPaymentPeriods :: FinancialDate -> PaymentSchedule -> PaymentNumber -> FinancialDate
```

## Migration Steps

### Step 1: Update Imports

#### Replace Old Imports
```haskell
-- Remove these imports
import LoanCalculator
import Date
import InterestFactor
```

#### Add New Imports
```haskell
-- Add these imports
import Domain.Types
import Domain.Loan
import Domain.AmortizationSchedule
import Infrastructure.DateCalculator
import Infrastructure.InterestCalculator
import Application.Errors
```

### Step 2: Convert Data Types

#### Converting LoanQuery to LoanRequest
```haskell
-- Old code
oldLoanQuery :: LoanQuery
oldLoanQuery = LoanQuery
  { loanAmount = 10000.0
  , interestRate = 8.5
  , loanTerms = 12
  , paymentFrequency = Monthly
  , startDate = Date 2023 1 15
  , ...
  }

-- New code
newLoanRequest :: LoanRequest  
newLoanRequest = LoanRequest
  { lrAmount = 10000.0
  , lrInterestRate = 8.5
  , lrLoanTerms = 12
  , lrPaymentFrequency = Monthly
  , lrStartDate = financialDate  -- Convert Date to FinancialDate
  , ...
  }
  where
    financialDate = case mkFinancialDate 2023 1 15 of
      Right date -> date
      Left err -> error err  -- Handle appropriately in real code
```

#### Converting Calculation Results
```haskell
-- Old result access
oldCalculation :: LoanQuery -> Double
oldCalculation query = 
  case validateAndCalculate query of
    Right result -> resultNetValue result
    Left errors -> error "Validation failed"

-- New result access
newCalculation :: LoanRequest -> Either String Double
newCalculation request = do
  loan <- case validateLoan request of
    Right validLoan -> Right validLoan
    Left errors -> Left (show errors)
  let result = calculateAmortizationSchedule loan
  return $ unCurrencyAmount (netValue result)
```

### Step 3: Update Error Handling

#### Old Error Handling
```haskell
-- Old: Pattern matching on specific errors
handleOldErrors :: Either [LoanQueryValidationError] LoanQueryResult -> String
handleOldErrors (Left errors) = unlines $ map show errors
handleOldErrors (Right result) = "Success: " ++ show result
```

#### New Error Handling
```haskell
-- New: Use error formatting utilities
handleNewErrors :: Either [LoanValidationError] Loan -> String
handleNewErrors (Left errors) = 
  unlines $ map formatValidationError errors
handleNewErrors (Right loan) = 
  "Validated loan: " ++ show loan

formatValidationError :: LoanValidationError -> String
formatValidationError (InvalidLoanAmount msg) = "Invalid amount: " ++ msg
formatValidationError (InvalidInterestRate msg) = "Invalid rate: " ++ msg
-- ... handle other error types
```

### Step 4: Update API Layer

#### Old API Handler
```haskell
-- Old API handler
handleLoanQuery :: LoanQuery -> LoanQueryUnwrappedResult
handleLoanQuery req = case validateAndCalculate req of
  Right res -> LoanQueryUnwrappedResult (Just res) Nothing
  Left errors -> LoanQueryUnwrappedResult Nothing (Just errors)
```

#### New API Handler
```haskell
-- New API handler (suggested approach)
handleLoanRequest :: LoanRequest -> LoanCalculationResponse
handleLoanRequest req = case validateLoan req of
  Right validLoan -> 
    let result = calculateAmortizationSchedule validLoan
    in LoanCalculationResponse (Just result) Nothing
  Left errors -> 
    LoanCalculationResponse Nothing (Just errors)

data LoanCalculationResponse = LoanCalculationResponse
  { response :: Maybe AmortizationResult
  , errors :: Maybe [LoanValidationError]
  }
```

## Common Migration Patterns

### Pattern 1: Simple Calculation
```haskell
-- Old
calculateSimple :: Double -> Double -> Int -> Double
calculateSimple amount rate terms = 
  let query = LoanQuery { ... }  -- Fill in all fields
  in case validateAndCalculate query of
    Right result -> resultTotalPayment result
    Left _ -> 0

-- New
calculateSimple :: Double -> Double -> Int -> Either String Double
calculateSimple amount rate terms = do
  request <- buildLoanRequest amount rate terms
  loan <- case validateLoan request of
    Right validLoan -> Right validLoan
    Left errors -> Left (unlines $ map show errors)
  let result = calculateAmortizationSchedule loan
  return $ unCurrencyAmount (totalPayments result)
```

### Pattern 2: Schedule Generation
```haskell
-- Old
generateSchedule :: LoanQuery -> [AmortizationTableItem]
generateSchedule query = 
  case validateAndCalculate query of
    Right result -> paymentItems (resultSchedule result)
    Left _ -> []

-- New
generateSchedule :: LoanRequest -> Either String [PaymentItem]
generateSchedule request = do
  loan <- case validateLoan request of
    Right validLoan -> Right validLoan
    Left errors -> Left (show errors)
  let schedule = calculateAmortizationSchedule loan
  return $ paymentItems (schedule schedule)
```

### Pattern 3: Tax Calculation
```haskell
-- Old
calculateTax :: LoanQuery -> Double
calculateTax query = 
  case validateAndCalculate query of
    Right result -> resultTax result
    Left _ -> 0

-- New
calculateTax :: LoanRequest -> Either String Double
calculateTax request = do
  loan <- case validateLoan request of
    Right validLoan -> Right validLoan
    Left errors -> Left (show errors)
  let result = calculateAmortizationSchedule loan
  return $ unTaxAmount (totalTax result)
```

## Breaking Changes

### Removed Functions
- `calculate :: LoanQuery -> LoanQueryResult` (replaced by `calculateAmortizationSchedule`)
- `validateAndCalculate :: LoanQuery -> Either [LoanQueryValidationError] LoanQueryResult`
- Direct field access patterns (now require unwrapping newtypes)

### Changed Function Signatures
- All currency amounts now use `CurrencyAmount` instead of `Double`
- All interest rates now use `InterestRate` instead of `Double`
- Date handling now uses `FinancialDate` instead of custom `Date`

### New Required Conversions
```haskell
-- Converting between raw values and domain types
amount :: Double = 1000.0
currencyAmount :: CurrencyAmount = CurrencyAmount amount
backToDouble :: Double = unCurrencyAmount currencyAmount

rate :: Double = 8.5
interestRate :: InterestRate = InterestRate rate  
backToDouble :: Double = unInterestRate interestRate
```

## Testing Your Migration

### 1. Compilation Test
Ensure your code compiles with the new modules:
```bash
stack build  # or cabal build
```

### 2. Functional Test
Verify calculations produce the same results:
```haskell
-- Test that old and new produce same results
testMigration :: LoanQuery -> Bool
testMigration oldQuery = 
  let oldResult = case validateAndCalculate oldQuery of
        Right r -> resultTotalPayment r
        Left _ -> 0
      newRequest = convertLoanQueryToRequest oldQuery
      newResult = case calculateWithNewAPI newRequest of
        Right r -> r
        Left _ -> 0
  in abs (oldResult - newResult) < 0.01  -- Allow for small rounding differences
```

### 3. Error Handling Test
Verify error cases are properly handled:
```haskell
testErrorHandling :: LoanRequest -> Bool
testErrorHandling invalidRequest =
  case validateLoan invalidRequest of
    Left errors -> not (null errors)  -- Should have validation errors
    Right _ -> False  -- Should not validate
```

## Getting Help

### Common Issues

1. **Type Conversion Errors**
   - Use unwrapping functions: `unCurrencyAmount`, `unInterestRate`, etc.
   - Use smart constructors for validation: `mkCurrencyAmount`, `mkInterestRate`

2. **Date Conversion**
   - Convert old `Date` to `FinancialDate` using `mkFinancialDate`
   - Handle potential parsing errors appropriately

3. **Error Handling**
   - New error types are more specific - update pattern matching
   - Use error formatting functions for user-friendly messages

### Migration Checklist

- [ ] Updated all imports to new modules
- [ ] Converted `LoanQuery` to `LoanRequest`  
- [ ] Updated error handling patterns
- [ ] Converted date handling to `FinancialDate`
- [ ] Updated API handlers to use new types
- [ ] Added proper type conversions where needed
- [ ] Tested calculations produce same results
- [ ] Updated tests to use new error types
- [ ] Verified compilation with new modules

## Benefits After Migration

1. **Type Safety**: Compile-time prevention of common errors
2. **Better Errors**: More specific error messages for debugging
3. **Maintainability**: Clearer separation of concerns
4. **Performance**: Potential optimizations in new architecture
5. **Extensibility**: Easier to add new features and amortization methods

The migration effort is worthwhile for the improved reliability and maintainability of your loan calculation system.