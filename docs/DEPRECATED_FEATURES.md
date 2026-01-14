# Deprecated Features - Migration Guide

This document lists features from the legacy API (`LoanCalculator.hs`, `InterestFactor.hs`) that were removed and do not yet have equivalents in the new Hasban API.

## Features to Implement in Future Versions

### 1. Commissions / Fees

**Old API:**
```haskell
data Commission = Commission
  { commissionType  :: CommissionType  -- Absolute | Percentage
  , commissionValue :: CurrencyAmount
  }

-- In LoanQuery:
commissions :: [Commission]

-- Functions:
totalCommissionValue :: LoanQuery -> Double
totalCommissionAbsolute :: LoanQuery -> Double
totalCommissionPercent :: LoanQuery -> Double
```

**Use Case:** Calculate upfront fees (origination fees, broker fees) that reduce net loan value.

**Suggested New API:**
```haskell
-- Add to Hasban.Types
data Fee = Fee FeeType Money
data FeeType = FixedFee | PercentageFee

-- Add to PersonalLoan
data PersonalLoan = PersonalLoan
  { ...
  , plFees :: [Fee]  -- Optional fees
  }

-- Add function
netDisbursement :: PersonalLoan -> Money
-- Returns principal minus fees
```

---

### 2. Grace Periods (Interest-Only Payments)

**Old API:**
```haskell
-- In LoanQuery:
firstIncorporationEvent   :: Maybe Date  -- When grace period ends
principalGracePeriodTerms :: PaymentNo   -- Number of interest-only payments
```

**Use Case:** Loans where initial payments are interest-only, then switch to full amortization. Common in:
- Student loans (grace period during school)
- Construction loans (interest-only during building)
- Promotional loans (low initial payments)

**Suggested New API:**
```haskell
data GracePeriod = GracePeriod
  { gpTerms :: Int              -- Number of interest-only payments
  , gpCapitalize :: Bool        -- Whether to add accrued interest to principal
  }

data PersonalLoan = PersonalLoan
  { ...
  , plGracePeriod :: Maybe GracePeriod
  }
```

---

### 3. Net Value Loan (Goal Seek)

**Old API:**
```haskell
asNetValueLoan :: LoanQuery -> Maybe LoanQuery
-- Finds the principal amount such that:
-- principal - taxes - commissions = desired_net_value
```

**Use Case:** Customer wants to receive exactly $10,000 after fees. What principal should the loan be?

**Suggested New API:**
```haskell
-- Add to Hasban.Loan
findPrincipalForNetValue
  :: Money           -- Desired net disbursement
  -> Rate            -- Interest rate
  -> Int             -- Terms
  -> [Fee]           -- Fees to deduct
  -> Maybe Money     -- Required principal

-- Note: GoalSeek.hs is preserved and can be used for this
```

---

### 4. Tax Integration in Payment Schedule

**Old API:**
```haskell
data AmortizationTableItem = AmortizationTableItem
  { ...
  , financialTax :: CurrencyAmount  -- IOF per payment
  }

-- Brazilian IOF calculation per payment:
brazilianIofTax :: Double -> Double -> PaymentNo -> CurrencyAmount -> Double
```

**Use Case:** Show taxes on each payment for regulatory compliance and customer transparency.

**Current State:** `Hasban.Extension.Brazil.Tax` exists but is not integrated into `generateSchedule`.

**Suggested New API:**
```haskell
-- Add tax field to Payment
data Payment = Payment
  { ...
  , paymentTax :: Maybe Money  -- Optional tax per payment
  }

-- Add configurable schedule generation
generateScheduleWithTax
  :: PersonalLoan
  -> (Money -> Int -> Money)  -- Tax calculator function
  -> [Payment]
```

---

### 5. Custom Payment Frequency (Every X Days)

**Old API:**
```haskell
data Frequency = ... | EveryXDays

-- In LoanQuery:
paymentEveryXDay :: Maybe PaymentNo  -- e.g., every 15 days
```

**Use Case:** Non-standard payment schedules (every 14 days, every 45 days, etc.)

**Current State:** `EveryXDays` exists in `Date.Frequency` but the new API doesn't fully support custom day counts.

**Suggested New API:**
```haskell
data Frequency =
    Monthly
  | Biweekly
  | Weekly
  | Daily
  | Yearly
  | CustomDays Int  -- Replace EveryXDays with explicit day count
```

---

### 6. CETIP21 Interest Modes in Loan Calculation

**Old API:**
```haskell
data InterestMode =
    ThreeSixtyFiveMonth  -- 365 days, monthly interest
  | ThreeSixtyFiveDay    -- 365 days, daily interest
  | ThreeSixtyMonth      -- 360 days, monthly interest
  | ThreeSixtyDay        -- 360 days, daily interest
  | TwoFiveTwoMonth      -- 252 business days, monthly
  | TwoFiveTwoDay        -- 252 business days, daily

-- In LoanQuery:
interestMode :: InterestMode

-- Interest calculation dispatch:
interestFactorForPaymentNo :: InterestMode -> InterestRate -> Date -> Date -> InterestRate
```

**Use Case:** Brazilian financial instruments require specific day-count conventions per CETIP21 regulations.

**Current State:** CETIP21 calculations exist in `Hasban.Extension.Brazil.CETIP21` but are not integrated into the main loan calculation flow.

**Suggested New API:**
```haskell
-- In Hasban.Extension.Brazil.CETIP21
data BrazilianLoan = BrazilianLoan
  { blBase :: PersonalLoan
  , blInterestMode :: CETIP21Mode
  , blIOFRates :: IOFRates
  }

generateBrazilianSchedule :: BrazilianLoan -> [Payment]
```

---

### 7. Pretty-Printed Table Output

**Old API:**
```haskell
instance Show AmortizationTable where
  -- Renders ASCII table with headers:
  -- #   Date        Interest      Principal     Remaining     Payment       Tax
```

**Use Case:** Human-readable output for debugging and reports.

**Suggested New API:**
```haskell
-- Add to Hasban or separate Hasban.Display module
formatSchedule :: [Payment] -> String
formatScheduleMarkdown :: [Payment] -> String
formatScheduleCSV :: [Payment] -> String
```

---

## Features Preserved

The following features from the old API ARE available in the new API:

| Old Feature | New Location |
|-------------|--------------|
| PRICE amortization | `Hasban.Amortization.generatePriceSchedule` |
| SAC amortization | `Hasban.Amortization.generateSacSchedule` |
| Payment calculation | `Hasban.Loan.monthlyPayment`, `periodicPayment` |
| Rate conversions | `Hasban.Loan.effectiveRate`, `periodicRate` |
| Input validation | `Hasban.Validation.*` |
| 252-day calculations | `Hasban.Extension.Brazil.CETIP21.*` |
| IOF tax calculation | `Hasban.Extension.Brazil.Tax.*` |
| Goal seek algorithm | `GoalSeek.goalSeek` (unchanged) |

---

## Deleted Files

- `src/LoanCalculator.hs` - Replaced by `Hasban.*` modules
- `src/InterestFactor.hs` - Replaced by `Hasban.Loan` + `Hasban.Extension.Brazil.CETIP21`

## Deleted API Endpoints

- `POST /loan` - Legacy loan calculation
- `POST /loan/net` - Legacy net value loan

## New API Endpoints

- `POST /hasban/personal` - Personal loan calculation
- `POST /hasban/creditcard` - Credit card analysis
