# Test Coverage Analysis - Hasban

## Overview

| Metric | Value |
|---|---|
| Source modules (library) | 12 files, ~2,340 lines |
| Test modules | 7 spec files, ~2,260 lines |
| Test-to-source line ratio | 0.97 (high) |
| Testing framework | hspec + QuickCheck |
| Coverage tooling configured | No (HPC not enabled) |

---

## Module-by-Module Coverage Assessment

### Well-Tested Modules

| Module | Lines | Test File | Test Lines | Functions Tested |
|---|---|---|---|---|
| `Hasban.Loan` | 176 | `Hasban.LoanSpec` | 361 | `monthlyPayment`, `periodicPayment`, `totalInterest`, `generateSchedule` (via Amortization) |
| `Hasban.CreditCard` | 127 | `Hasban.CreditCardSpec` | 254 | `minimumPayment`, `analyzeBalance`, `payoffProjection`, `simulatePayoff`, `trueCostOfPurchase` |
| `Hasban.Amortization` | 281 | `Hasban.LoanSpec` | (shared) | `generateSchedule` for PRICE, SAC, Bullet; grace periods tested indirectly |
| `Precision.CETIP21` | 79 | `Precision.CETIP21Spec` | 155 | `applyPrecision` (all 5 PrecisionTypes), `roundToDecimalPlaces`, `truncateToDecimalPlaces`, `validateMaxDecimalPlaces` |

### Partially Tested Modules

| Module | Lines | Coverage Notes |
|---|---|---|
| `Hasban.Extension.Brazil.CETIP21` | 327 | The interest factor functions (`interestFactor252BDays`, `interestRate36XDays`, etc.) are tested **indirectly** through `InterestCalculation.MethodsSpec`, but that test file re-implements its own calculation functions rather than importing from the source module. The actual source module functions are **not directly tested**. |
| `Date` | 154 | Indirectly exercised by loan schedule generation tests, but **no dedicated tests** for `numberOfDays`, `numberOfMonths`, `numberOfWeekdays`, `addTerm`, `fixDate`, `previousMonthAniversary`, `nextMonthAniversary`, `isLeapYear`, `daysInMonth`, or `formatDate`. |

### Untested Modules

| Module | Lines | Exported Functions | Risk Level |
|---|---|---|---|
| **`Hasban.Validation`** | 149 | `validatePersonalLoan`, `validateCreditCard`, `validatePrincipal`, `validateRate`, `validateTerms`, `validateBalance` | **HIGH** |
| **`Hasban.Extension.Brazil.Tax`** | 99 | `calculateIOF`, `calculateIOFWithRates`, `iofForPayment`, `dailyIOFCapped`, `defaultIOFRates`, `zeroIOFRates` | **HIGH** |
| **`LoanApi`** | 438 | `hasbanServer`, `toPersonalLoan`, `handlePersonalLoan`, `handleCreditCard`, `handleNetValue`, JSON serialization | **HIGH** |
| **`Hasban` (main module)**| 95 (unique) | `findPrincipalForNetValue`, `formatSchedule`, `formatScheduleMarkdown`, `formatScheduleCSV` | **MEDIUM** |
| **`GoalSeek`** | 28 | `goalSeek` | **MEDIUM** |

---

## Detailed Gap Analysis

### 1. Validation Module - No Tests (HIGH PRIORITY)

**File:** `src/Hasban/Validation.hs` (149 lines, 0 tests)

The validation module is the gatekeeper for all user input but has zero test coverage. This is the highest-priority gap because invalid inputs flowing into calculation functions could produce silently wrong financial results.

**Untested functions and recommended test cases:**

- **`validatePersonalLoan`**: Valid loan passes; loan with negative principal fails; loan with rate > 10 fails; loan with 0 terms fails; loan with NaN fields fails; multiple simultaneous errors accumulate.
- **`validateCreditCard`**: Valid card passes; card with negative limit fails; card with invalid min payment fails.
- **`validatePrincipal`**: Boundary tests at 0, negative, 1e12+1, NaN, Infinity.
- **`validateRate`**: Boundary tests at -0.01, 0, 10.01, NaN, Infinity.
- **`validateTerms`**: Boundary tests at 0, 1, 600, 601.
- **`validateBalance`**: Negative balance fails; NaN fails; zero is valid.
- **Internal validators** (`validateLimitField`, `validateMinPaymentField`): All `MinPayment` constructors with boundary values.

### 2. Brazilian Tax (IOF) Module - No Tests (HIGH PRIORITY)

**File:** `src/Hasban/Extension/Brazil/Tax.hs` (99 lines, 0 tests)

IOF tax is a regulated Brazilian financial tax. Incorrect calculations could have legal/compliance consequences.

**Untested functions and recommended test cases:**

- **`calculateIOF`** / **`calculateIOFWithRates`**: Standard case ($10,000 principal, 180 days); verify base + daily components separately; verify day capping at 365.
- **`iofForPayment`**: Per-payment IOF for amortization schedules; verify day capping; verify with zero rates.
- **`dailyIOFCapped`**: Days below cap; days equal to cap; days above cap (should cap at `iofMaxDays`).
- **`defaultIOFRates`**: Verify base rate = 0.0038, daily rate = 0.000082, max days = 365.
- **`zeroIOFRates`**: All rates are zero; verify IOF calculation returns 0.

### 3. REST API Module - No Tests (HIGH PRIORITY)

**File:** `src/LoanApi.hs` (438 lines, 0 tests)

The API layer is the external-facing interface and contains parsing logic, validation integration, and response construction. Bugs here directly affect consumers.

**Untested areas and recommended test cases:**

- **Request parsing**: `toPersonalLoan` with valid/invalid amortization strings, frequency strings, fee types; `toCreditCard` with valid/invalid min payment types.
- **`parseAmortization`**: "price" -> `Price`, "sac" -> `Sac`, "bullet" -> `Bullet`, "invalid" -> `Left`.
- **`parseFrequency`**: All valid frequencies + invalid string.
- **`parseFee`**: "fixed" and "percentage" + invalid type.
- **`parseMinPayment`**: "fixed", "percent", "greater_of" with correct/missing optional fields.
- **Handler logic**: `handlePersonalLoan` produces correct response fields; `handleCreditCard` produces correct response; `handleNetValue` handles impossible percentage fees (>= 100%).
- **JSON round-trip**: `ApiResponse`, `PersonalLoanRequest`, `CreditCardRequest`, `NetValueRequest` serialize/deserialize correctly.
- **Integration tests**: Full HTTP request/response cycle with Servant test client (`hspec-wai` or `servant-client`).

### 4. Date Module - No Dedicated Tests (MEDIUM PRIORITY)

**File:** `src/Date.hs` (154 lines, 0 dedicated tests)

Date calculations underpin every payment schedule in the system. While exercised indirectly, edge cases around month boundaries, leap years, and custom frequencies are not explicitly tested.

**Untested functions and recommended test cases:**

- **`numberOfDays`**: Positive and negative date ranges; cross-year; same date = 0.
- **`numberOfMonths`**: Same month; cross-year; day adjustment (e.g., Jan 31 to Feb 28).
- **`numberOfWeekdays`**: Known week ranges; ranges containing weekends; single day.
- **`addTerm`**: Monthly across Feb boundary; biweekly; weekly; daily; yearly; `CustomDays`; `EveryXDays`.
- **`fixDate`**: Day before, on, and after `dayOfMonth`; December wraparound to next year.
- **`previousMonthAniversary`** / **`nextMonthAniversary`**: Standard cases; January wraparound; December wraparound.
- **`isLeapYear`** / **`daysInMonth`**: 2000 (leap), 1900 (not leap), 2024 (leap), 2023 (not leap); February days for each.
- **`formatDate`**: Single-digit month/day padding.
- **JSON round-trip**: `Date` and `Frequency` types.

### 5. Main Library Functions - No Tests (MEDIUM PRIORITY)

**File:** `src/Hasban.hs` - unique functions (lines 182-269, 0 tests)

- **`findPrincipalForNetValue`**: Fixed fees only; percentage fees only; mixed fees; percentage fees >= 100% returns `Nothing`; negative desired net.
- **`formatSchedule`** / **`formatScheduleMarkdown`** / **`formatScheduleCSV`**: Verify correct formatting of a known schedule; empty schedule; schedule with/without tax.

### 6. GoalSeek Module - No Tests (MEDIUM PRIORITY)

**File:** `src/GoalSeek.hs` (28 lines, 0 tests)

Numerical optimization that could fail silently (diverge, oscillate, or return incorrect results).

- **`goalSeek`**: Simple function (e.g., `f(x) = x - 5`) converges to 5; function with no root returns `Nothing` at max iterations; accuracy threshold is respected; zero iterations returns `Nothing`.

---

## Structural Test Issues

### Issue: Test modules re-implement source logic

Several CETIP21 test files (`Calendar.BusinessDaySpec`, `InterestCalculation.MethodsSpec`, `ProRata.DelayedCorrectionSpec`) define their own local `Date` type, their own `isLeapYear`, their own calculation functions, etc. rather than importing from the source modules. This means:

1. **The actual source module code is not being exercised.** The tests validate their own copies of the logic, not the production code.
2. **Drift risk**: If the source module is updated, tests won't catch regressions because they test different code.

**Recommendation**: Refactor these test modules to import from the source modules (`Date`, `Hasban.Extension.Brazil.CETIP21`) instead of re-implementing the logic locally.

### Issue: No HPC (Haskell Program Coverage) integration

The project has no coverage tooling configured. Running `stack test --coverage` or `cabal test --enable-coverage` would generate HPC reports with exact line-level and expression-level coverage data.

**Recommendation**: Add a CI step or Makefile target for:
```bash
stack test --coverage
stack hpc report hasban
```

### Issue: Property tests use loose tolerances

The QuickCheck property tests in `LoanSpec` use broad ranges (e.g., `principal <- choose (0.01, 1000000)`, `rate <- choose (0.0, 1.0)`) which is good for finding edge cases, but the assertions use large relative tolerances (`principal * 0.0001`) that could mask real precision bugs in financial calculations.

**Recommendation**: Add focused property tests with tighter tolerances for common loan parameter ranges.

---

## Recommended Priority Order for Improvement

| Priority | Area | Effort | Impact |
|---|---|---|---|
| **P0** | `Hasban.Validation` tests | Small | Prevents invalid data from reaching calculations |
| **P0** | `Hasban.Extension.Brazil.Tax` tests | Small | Brazilian regulatory compliance |
| **P1** | `LoanApi` handler unit tests | Medium | Validates the external-facing API |
| **P1** | Refactor CETIP21 tests to import source modules | Medium | Ensures tests actually exercise production code |
| **P2** | `Date` module dedicated tests | Small | Foundational correctness for all schedules |
| **P2** | `findPrincipalForNetValue` tests | Small | Goal-seek for net disbursement |
| **P2** | Enable HPC coverage reporting | Small | Provides concrete coverage metrics |
| **P3** | `LoanApi` integration tests (HTTP) | Large | End-to-end API validation |
| **P3** | `GoalSeek` tests | Small | Numerical convergence correctness |
| **P3** | Formatting function tests | Small | Output correctness |

---

## Summary

The codebase has a strong test-to-code ratio (~0.97) and thorough coverage of core calculation logic (loan amortization, credit card payoff, CETIP21 precision). However, there are significant gaps in:

1. **Input validation** (149 lines completely untested)
2. **Brazilian IOF tax calculations** (99 lines completely untested)
3. **REST API layer** (438 lines completely untested - the largest module)
4. **Date utilities** (154 lines with no dedicated tests)
5. **Test isolation** (CETIP21 test modules re-implement source logic instead of importing it)

Addressing priorities P0 and P1 would cover the most critical gaps with moderate effort.
