{-# LANGUAGE DeriveGeneric #-}

module Hasban.Loan (
  -- * Payment Calculations
  monthlyPayment
  , periodicPayment
  , totalInterest
  , totalPayments

  -- * Rate Conversions
  , effectiveRate
  , periodicRate
  , annualToMonthly
  , annualToPeriodic

  -- * Interest Factors
  , compoundFactor
  , simpleFactor
  , interestFactor

  -- * Fee Calculations
  , totalFees
  , feeAmount
  , netDisbursement
) where

import Hasban.Types

-- | Calculate monthly payment using PRICE (PMT) formula
-- PMT = P * [r(1+r)^n] / [(1+r)^n - 1]
monthlyPayment :: PersonalLoan -> Money
monthlyPayment loan = periodicPayment loan

-- | Calculate periodic payment based on loan frequency
periodicPayment :: PersonalLoan -> Money
periodicPayment loan =
  let principal = unMoney (plPrincipal loan)
      rate = periodicRate (plAnnualRate loan) (plFrequency loan)
      n = plTerms loan
  in case plAmortization loan of
    Price  -> Money $ pricePayment principal rate n
    Sac    -> Money $ sacFirstPayment principal rate n
    Bullet -> Money $ bulletPayment principal rate n

-- | PRICE (constant payment) formula
pricePayment :: Double -> Double -> Int -> Double
pricePayment principal rate n
  | rate == 0 = principal / fromIntegral n
  | otherwise =
      let r = rate
          factor = (1 + r) ^ n
      in principal * (r * factor) / (factor - 1)

-- | SAC first payment (decreasing payments)
sacFirstPayment :: Double -> Double -> Int -> Double
sacFirstPayment principal rate n =
  let amortization = principal / fromIntegral n
      interest = principal * rate
  in amortization + interest

-- | Bullet payment (interest only until maturity)
bulletPayment :: Double -> Double -> Int -> Double
bulletPayment principal rate _ = principal * rate

-- | Calculate total interest paid over loan lifetime
totalInterest :: PersonalLoan -> Money
totalInterest loan =
  let total = totalPayments loan
      principal = plPrincipal loan
  in Money $ unMoney total - unMoney principal

-- | Calculate total of all payments
totalPayments :: PersonalLoan -> Money
totalPayments loan =
  case plAmortization loan of
    Price -> Money $ unMoney (periodicPayment loan) * fromIntegral (plTerms loan)
    Sac   -> Money $ sumSacPayments loan
    Bullet -> Money $ sumBulletPayments loan

-- | Sum all SAC payments (arithmetic series)
sumSacPayments :: PersonalLoan -> Double
sumSacPayments loan =
  let principal = unMoney (plPrincipal loan)
      rate = periodicRate (plAnnualRate loan) (plFrequency loan)
      n = plTerms loan
      amortization = principal / fromIntegral n
      -- Sum of interest: rate * (P + P-A + P-2A + ... + A)
      -- = rate * n * (P + A) / 2 = rate * (n*P + n*A) / 2
      -- Simplified: sum of balances * rate + n * amortization
      totalInterest' = sum [rate * (principal - amortization * fromIntegral (i-1)) | i <- [1..n]]
  in principal + totalInterest'

-- | Sum all bullet payments
sumBulletPayments :: PersonalLoan -> Double
sumBulletPayments loan =
  let principal = unMoney (plPrincipal loan)
      rate = periodicRate (plAnnualRate loan) (plFrequency loan)
      n = plTerms loan
  in principal + (principal * rate * fromIntegral n)

-- ============================================
-- RATE CONVERSIONS
-- ============================================

-- | Convert annual rate to effective rate for payment frequency
effectiveRate :: Rate -> Frequency -> Rate
effectiveRate annual freq =
  let r = unRate annual
      periodsPerYear = frequencyPeriods freq
  in Rate $ ((1 + r) ** (1 / periodsPerYear)) - 1

-- | Get periodic rate from annual rate
periodicRate :: Rate -> Frequency -> Double
periodicRate annual freq = unRate $ effectiveRate annual freq

-- | Simple annual to monthly conversion (divide by 12)
annualToMonthly :: Rate -> Rate
annualToMonthly (Rate r) = Rate (r / 12)

-- | Convert annual rate to any period
annualToPeriodic :: Rate -> Frequency -> Rate
annualToPeriodic (Rate r) freq =
  let periods = frequencyPeriods freq
  in Rate (r / periods)

-- | Number of periods per year for each frequency
frequencyPeriods :: Frequency -> Double
frequencyPeriods Monthly   = 12
frequencyPeriods Biweekly  = 26
frequencyPeriods Weekly    = 52
frequencyPeriods Daily     = 365
frequencyPeriods Yearly    = 1
frequencyPeriods EveryXDays = 365  -- Default to daily
frequencyPeriods (CustomDays n) = 365 / fromIntegral n  -- Custom days

-- ============================================
-- INTEREST FACTORS
-- ============================================

-- | Compound interest factor: (1 + r)^n
compoundFactor :: Rate -> Int -> Double
compoundFactor (Rate r) n = (1 + r) ** fromIntegral n

-- | Simple interest factor: 1 + r*n
simpleFactor :: Rate -> Int -> Double
simpleFactor (Rate r) n = 1 + r * fromIntegral n

-- | Interest factor based on method
interestFactor :: InterestMethod -> Rate -> Int -> Double
interestFactor SimpleInterest rate n = simpleFactor rate n
interestFactor CompoundInterest rate n = compoundFactor rate n

-- ============================================
-- FEE CALCULATIONS
-- ============================================

-- | Calculate total fees for a loan
totalFees :: PersonalLoan -> Money
totalFees loan = Money $ sum [feeAmount' fee loan | fee <- plFees loan]

-- | Calculate amount for a single fee
feeAmount :: Fee -> PersonalLoan -> Money
feeAmount fee loan = Money $ feeAmount' fee loan

-- | Internal helper for fee calculation
feeAmount' :: Fee -> PersonalLoan -> Double
feeAmount' (Fee FixedFee (Money amount)) _ = amount
feeAmount' (Fee PercentageFee (Money pct)) loan =
  unMoney (plPrincipal loan) * pct

-- | Calculate net disbursement (principal minus fees)
netDisbursement :: PersonalLoan -> Money
netDisbursement loan =
  let principal = unMoney (plPrincipal loan)
      fees = unMoney (totalFees loan)
  in Money (principal - fees)
