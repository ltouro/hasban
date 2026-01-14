# Test Update Plan for CETIP21 Implementation

## Current State Analysis

### Documentation Analysis
- **PRICE_AMORTIZATION_SCHEDULES.md**: Details 6 CETIP21 interest calculation methods
- **IMPLEMENTATION_PLAN.md**: Comprehensive architecture plan with CETIP21 compliance requirements
- **CCB_CCE_NCE_CETIP21.pdf**: Official CETIP21 specification with precise formulas and requirements

### Current Test Analysis
- **Main test file**: `test/SpecTest.hs`
- **Current tests focus on**: Property-based testing with QuickCheck
- **Current coverage**: Tests validate basic loan calculation properties but lack CETIP21 compliance validation
- **Issue**: Current `InterestMode` enum has 6 modes that need mapping to new CETIP21 methods

## Required Test Updates

### 1. New Test Categories to Add

#### A. CETIP21 Precision Compliance Tests
**File**: `test/Precision/CETIP21Spec.hs`

```haskell
-- Test banker's rounding for interest factors (9 decimal places WITH rounding)
-- Test truncation for unit values (8 decimal places WITHOUT rounding)
-- Test truncation for financial values (2 decimal places WITHOUT rounding)
-- Test pro-rata precision (9 decimal places WITHOUT rounding)
-- Test input rate precision (4 decimal places maximum)
```

**Key Requirements**:
- Interest factors: 9 decimal places WITH banker's rounding (CETIP21 requirement)
- Unit values: 8 decimal places WITHOUT rounding (CETIP21 requirement)
- Financial values: 2 decimal places WITHOUT rounding (CETIP21 requirement)
- Pro-rata ratios: 9 decimal places WITHOUT rounding (CETIP21 requirement)

#### B. Interest Calculation Method Tests
**File**: `test/InterestCalculation/MethodsSpec.hs`

Test each of the 6 CETIP21 calculation methods:

1. **252 Days (Business Days)**: `(1 + i/100)^(dup/dut)` - Most common for Brazilian market
2. **360 Days (Calendar)**: `(1 + i/100)^(dcp/dct)` with linear option - Corporate bonds
3. **365 Days (Calendar)**: `(1 + i/100)^(dcp/dct)` - Government securities
4. **252 Months (21 business days/month)**: `(1 + i/100)^(N×21/252)` - Monthly business day cycles
5. **360 Months (30 calendar days/month)**: `(1 + i/100)^(N×30/360)` - Standardized monthly
6. **365 Months (30 calendar days/month)**: `(1 + i/100)^(N×30/365)` - Actual year monthly

**Test Cases**:
```haskell
-- Test matched dates calculations
-- Test mismatched dates with pro-rata
-- Test linear vs compound for 360-day method
-- Validate precision requirements for each method
```

#### C. Pro-rata Calculation Tests
**File**: `test/ProRata/CalculationSpec.hs`

```haskell
-- Test calendar day pro-rata formulas
-- Test business day pro-rata formulas
-- Test delayed correction scenarios for IGP-DI, INPC, IPCA
-- Test end-of-month date matching rules
-- Test CETIP21 index determination rules
```

**Critical CETIP21 Pro-rata Formula**:
```
C = ((NIm1/NIm0)^(dcp_pro-rata/dct_pro-rata)) × (NImn/NIm1)
```

**Delayed Correction Rules**:
- IGP-M: No delay (use previous month)
- IGP-DI, INPC, IPCA: Use second previous month when payment date < 15th

#### D. Grace Period Tests
**File**: `test/GracePeriod/IncorporationSpec.hs`

```haskell
-- Test interest incorporation one period before first payment
-- Test grace period with different calculation methods
-- Test adjusted VNE calculations
-- Test grace period precision requirements
```

**Grace Period Formula**:
```
VNE_adjusted = VNE + VNE × (Fator_de_Juros_Carencia - 1)
```

### 2. Current Test Modifications

#### A. Update SpecTest.hs Property Tests

**Changes Required**:
- Map existing `InterestMode` values to new CETIP21 calculation methods
- Update QuickCheck generators to include CETIP21-compliant parameters
- Add precision validation to existing property tests
- Update date generation to respect CETIP21 business day rules

**New InterestMode Mapping**:
```haskell
-- Old -> New
ThreeSixtyFiveMonth -> Days365Calendar + Months365Calendar
ThreeSixtyFiveDay   -> Days365Calendar
ThreeSixtyMonth     -> Days360Calendar + Months360Calendar
ThreeSixtyDay       -> Days360Calendar
TwoFiveTwoMonth     -> Days252Business + Months252Business
TwoFiveTwoDay       -> Days252Business
```

#### B. Enhanced LoanQuery Generator

```haskell
instance Arbitrary LoanQuery where
  arbitrary = do
    -- Existing parameters
    amount <- scaleDecimalPlaces 2 <$> choose (1000, 100000)
    interest <- scaleDecimalPlaces 4 <$> choose (0.01, 0.1)  -- CETIP21: max 4 decimal places
    terms <- choose (1, 240)

    -- New CETIP21 parameters
    calculationMethod <- elements [Days252Business, Days360Calendar Compound,
                                   Days365Calendar, Months252Business,
                                   Months360Calendar, Months365Calendar]
    amortizationType <- elements [PRICE, SAC]
    businessDayCalendar <- pure brazilianCalendar
    indexConfiguration <- arbitrary -- For IGP-M, IGP-DI, INPC, IPCA
    gracePeriod <- arbitrary
    endOfMonthRule <- elements [StrictDayMatching, EndOfMonthMatching, CETIP21Table]

    -- Generate dates respecting CETIP21 rules
    startDate <- arbitrary
    paymentStartDate <- arbitrary `suchThat` (respectsCETIP21DateRules startDate)

    return $ LoanQuery { ... }
```

### 3. Reference Test Cases from CETIP21 Documentation

#### A. Create Reference Test Suite
**File**: `test/Reference/CETIP21Spec.hs`

```haskell
-- Test cases from official CETIP21 formulas document
cetip21ReferenceTests :: [ReferenceTestCase]
cetip21ReferenceTests =
  [ ReferenceTestCase
      { testName = "252 Days - 22 business days at 25.2% annual"
      , method = Days252Business
      , interestRate = InterestRate 0.252
      , timePeriod = businessDaysPeriod 22
      , expectedFactor = 1.0208333333
      , tolerance = 1e-8
      }
  , ReferenceTestCase
      { testName = "360 Days Linear - 30 calendar days at 12% annual"
      , method = Days360Calendar Linear
      , interestRate = InterestRate 0.12
      , timePeriod = calendarDaysPeriod 30
      , expectedFactor = 1.01
      , tolerance = 1e-8
      }
  -- More reference cases from CETIP21 documentation
  ]
```

#### B. Integration Tests for Market Conventions
**File**: `test/Integration/MarketConventionsSpec.hs`

```haskell
-- Test Brazilian CETIP21 convention defaults
-- Test complete loan calculations with CETIP21 compliance
-- Test different combinations of interest methods and amortization types

spec_cetip21Convention :: Spec
spec_cetip21Convention = describe "CETIP21 Brazilian Convention" $ do
  it "uses correct default parameters" $ do
    let convention = cetip21Convention
    conventionName convention `shouldBe` "CETIP21-Brazil"
    defaultInterestMethod convention `shouldBe` Days252Business
    defaultAmortizationType convention `shouldBe` PRICE

  it "calculates corporate loan correctly" $ do
    -- PRICE amortization + 252 Months interest calculation
    let config = LoanConfiguration { ... }
    result <- calculateLoanSchedule config
    -- Validate against expected results
```

### 4. Business Day Calendar Tests
**File**: `test/Calendar/BusinessDaySpec.hs`

```haskell
-- Test Brazilian holiday calendar
-- Test business day counting
-- Test "Following Business Day" convention
-- Test weekend handling

spec_brazilianCalendar :: Spec
spec_brazilianCalendar = describe "Brazilian Business Day Calendar" $ do
  it "correctly identifies Brazilian holidays" $ do
    isBusinessDay brazilianCalendar (Date 2024 1 1) `shouldBe` False  -- New Year
    isBusinessDay brazilianCalendar (Date 2024 9 7) `shouldBe` False  -- Independence Day

  it "calculates business days correctly" $ do
    let startDate = Date 2024 1 15  -- Monday
    let endDate = Date 2024 1 19    -- Friday
    calculateBusinessDays brazilianCalendar startDate endDate `shouldBe` 5

  it "handles end-of-month rules per CETIP21 table" $ do
    -- Test CETIP21 end-of-month matching rules
    testEndOfMonthMatching (Date 2024 2 28) (Date 2024 2 29) `shouldBe` True
    testEndOfMonthMatching (Date 2024 2 29) (Date 2024 3 31) `shouldBe` True
```

### 5. Index Integration Tests
**File**: `test/Index/CorrectionSpec.hs`

```haskell
-- Test delayed publication handling for IGP-DI, INPC, IPCA
-- Test immediate publication for IGP-M
-- Test index validation and gap detection
-- Test index caching mechanisms

spec_delayedCorrection :: Spec
spec_delayedCorrection = describe "CETIP21 Delayed Index Correction" $ do
  it "applies delayed correction for IGP-DI when payment date < 15" $ do
    let paymentDate = Date 2024 3 10  -- Before 15th
    let correctedIndex = applyDelayedCorrection IGP_DI paymentDate originalIndex
    -- Should use index from January (second month prior)

  it "uses immediate correction for IGP-M" $ do
    let paymentDate = Date 2024 3 10
    let correctedIndex = applyDelayedCorrection IGP_M paymentDate originalIndex
    -- Should use index from February (immediate prior month)
```

## Implementation Priority

### Phase 1: Core CETIP21 Compliance (Weeks 1-2)
1. **Precision compliance tests** - Critical for regulatory compliance
2. **Interest calculation method tests** - Core calculation engine
3. **Update existing property tests** - Maintain current test coverage

### Phase 2: Advanced Features (Weeks 3-4)
1. **Pro-rata calculation tests** - Handle mismatched dates
2. **Grace period tests** - Interest incorporation scenarios
3. **Business day calendar tests** - Brazilian market specifics

### Phase 3: Integration & Reference (Weeks 5-6)
1. **Reference test cases** - Validate against CETIP21 documentation
2. **Index integration tests** - Delayed correction scenarios
3. **Market convention integration tests** - End-to-end validation

## Key Testing Strategies

### 1. Property-Based Testing
- Continue using QuickCheck but with CETIP21-compliant generators
- Generate test data that respects CETIP21 constraints
- Test mathematical properties hold across all calculation methods

### 2. Reference Testing
- Validate against official CETIP21 examples
- Ensure calculations match documented formulas exactly
- Test edge cases from official documentation

### 3. Precision Testing
- Verify exact decimal place requirements
- Test banker's rounding vs truncation correctly applied
- Validate intermediate precision preservation

### 4. Edge Case Testing
- Test end-of-month rules per CETIP21 table
- Test leap years and holiday handling
- Test date mismatching scenarios

### 5. Integration Testing
- Test complete loan calculations end-to-end
- Validate PRICE vs SAC amortization with all interest methods
- Test grace period with interest incorporation

## CETIP21 End-of-Month Matching Rules

Per official CETIP21 documentation, these combinations are considered "matched dates":

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

## Test Data Requirements

### Sample CETIP21 Calculation Examples
Based on the official documentation, include these reference calculations:

1. **252 Business Days**: 22 business days at 25.2% annual rate
2. **360 Calendar Days (Compound)**: 30 calendar days at 12% annual rate
3. **360 Calendar Days (Linear)**: Using formula `1 + (i × N)/36000`
4. **365 Calendar Days**: Standard government bond calculation
5. **252 Months**: 2 months with 21 business days each
6. **360/365 Months**: Monthly cycles with 30-day standardization

### Pro-rata Test Scenarios
1. Issue date 15/01, first anniversary 15/02 (matched)
2. Issue date 20/01, first anniversary 15/02 (mismatched - requires pro-rata)
3. End-of-month scenarios per CETIP21 table
4. Delayed correction scenarios for different indices

This comprehensive test plan ensures the new CETIP21-compliant implementation will be thoroughly validated while maintaining the existing property-based testing approach and adding detailed reference validation against official specifications.