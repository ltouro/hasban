{-# LANGUAGE DeriveGeneric #-}

module Infrastructure.IRRCalculator
  ( CashFlow(..)
  , IRRResult(..)
  , IRRError(..)
  , calculateIRR
  , calculateNPV
  , annualizeRate
  ) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Data.List (foldl')
import Data.Time (Day, diffDays)

import Domain.Types (CurrencyAmount, unCurrencyAmount)
import Infrastructure.DateCalculator (FinancialDate(..), daysBetween)

-- | Cash flow entry with date and amount
data CashFlow = CashFlow
  { cfDate   :: FinancialDate    -- ^ Date of cash flow
  , cfAmount :: CurrencyAmount   -- ^ Amount (negative for outflows, positive for inflows)
  } deriving (Eq, Show, Generic)

instance FromJSON CashFlow
instance ToJSON CashFlow

-- | Result of IRR calculation
data IRRResult = IRRResult
  { irrRate        :: Double  -- ^ Internal Rate of Return (as decimal)
  , irrRatePercent :: Double  -- ^ IRR as percentage
  , npvAtRate      :: Double  -- ^ Net Present Value at the calculated rate
  , iterations     :: Int     -- ^ Number of iterations used
  , converged      :: Bool    -- ^ Whether calculation converged
  } deriving (Eq, Show, Generic)

instance FromJSON IRRResult
instance ToJSON IRRResult

-- | Possible errors in IRR calculation
data IRRError =
    InsufficientCashFlows         -- ^ Need at least 2 cash flows
  | NoSignChange                  -- ^ Cash flows must have both positive and negative values
  | CalculationFailed String      -- ^ Numerical calculation failed
  | MaxIterationsExceeded Int     -- ^ Failed to converge within iteration limit
  deriving (Eq, Show, Generic)

instance FromJSON IRRError
instance ToJSON IRRError

-- | Calculate Internal Rate of Return for a series of cash flows
calculateIRR :: [CashFlow] -> Either IRRError IRRResult
calculateIRR cashflows = do
  validateCashFlows cashflows
  let yearlyCashFlows = convertToYearlyFractions cashflows
  rate <- findIRR yearlyCashFlows
  let npv = calculateNPV rate yearlyCashFlows
  return $ IRRResult
    { irrRate = rate
    , irrRatePercent = rate * 100
    , npvAtRate = npv
    , iterations = 0  -- TODO: track actual iterations
    , converged = True
    }

-- | Validate cash flows for IRR calculation
validateCashFlows :: [CashFlow] -> Either IRRError ()
validateCashFlows cashflows
  | length cashflows < 2 = Left InsufficientCashFlows
  | not hasSignChange = Left NoSignChange
  | otherwise = Right ()
  where
    amounts = map (unCurrencyAmount . cfAmount) cashflows
    hasSignChange = any (> 0) amounts && any (< 0) amounts

-- | Convert cash flows to yearly fractions from first date
convertToYearlyFractions :: [CashFlow] -> [(Double, Double)]  -- (years, amount)
convertToYearlyFractions [] = []
convertToYearlyFractions cashflows =
  let firstDate = cfDate (head cashflows)
      toYearlyFraction cf = 
        let days = daysBetween firstDate (cfDate cf)
            years = fromIntegral days / 365.0
            amount = unCurrencyAmount (cfAmount cf)
        in (years, amount)
  in map toYearlyFraction cashflows

-- | Find IRR using bisection method
findIRR :: [(Double, Double)] -> Either IRRError Double
findIRR cashflows = bisectionMethod cashflows (-0.99) 10.0 1e-6 100

-- | Bisection method for finding IRR
bisectionMethod :: [(Double, Double)] -> Double -> Double -> Double -> Int -> Either IRRError Double
bisectionMethod cashflows lowRate highRate tolerance maxIter = 
  bisectionLoop cashflows lowRate highRate tolerance maxIter 0

bisectionLoop :: [(Double, Double)] -> Double -> Double -> Double -> Int -> Int -> Either IRRError Double
bisectionLoop cashflows lowRate highRate tolerance maxIter currentIter
  | currentIter >= maxIter = Left (MaxIterationsExceeded currentIter)
  | abs (highRate - lowRate) < tolerance = Right midRate
  | npvLow * npvMid < 0 = bisectionLoop cashflows lowRate midRate tolerance maxIter (currentIter + 1)
  | npvMid * npvHigh < 0 = bisectionLoop cashflows midRate highRate tolerance maxIter (currentIter + 1)
  | otherwise = Left (CalculationFailed "No root found in interval")
  where
    midRate = (lowRate + highRate) / 2
    npvLow = calculateNPV lowRate cashflows
    npvMid = calculateNPV midRate cashflows
    npvHigh = calculateNPV highRate cashflows

-- | Calculate Net Present Value at given rate
calculateNPV :: Double -> [(Double, Double)] -> Double
calculateNPV rate cashflows = 
  foldl' (\acc (years, amount) -> acc + amount / ((1 + rate) ** years)) 0 cashflows

-- | Annualize a rate (e.g., convert monthly to yearly)
annualizeRate :: Double   -- ^ Rate per period
             -> Int       -- ^ Periods per year
             -> Double    -- ^ Annualized rate
annualizeRate periodRate periodsPerYear = 
  ((1 + periodRate) ** fromIntegral periodsPerYear) - 1

{- |
IRR Calculation Notes:

The Internal Rate of Return (IRR) is the discount rate that makes the Net Present Value (NPV)
of all cash flows equal to zero. It's a key metric for evaluating investment returns.

This implementation:
1. Validates that cash flows have both positive and negative values
2. Converts dates to yearly fractions for time-value calculations
3. Uses bisection method to find the rate where NPV = 0
4. Provides comprehensive error handling for edge cases

Common financial applications:
- Loan effective rate calculation
- Investment return analysis
- Project evaluation

The algorithm assumes:
- Cash flows occur on the exact dates specified
- Compounding is continuous
- No cash flows between specified dates

For Brazilian financial calculations, this is often used to determine the effective
annual rate that borrowers actually pay, including all fees and taxes.
-}