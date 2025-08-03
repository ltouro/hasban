{-# LANGUAGE DeriveGeneric #-}

module Infrastructure.InterestCalculator
  ( InterestFactor(..)
  , EffectiveRate(..)
  , calculateInterestFactor
  , calculateEffectiveRate
  , calculateYearlyRate
  , calculateDailyRate
  , calculateMonthlyRate
  , interestForPeriod
  , compoundRate
  , roundToDecimals
  ) where

import Domain.Types (InterestRate, InterestMode(..), InterestPeriod(..), CurrencyAmount, unInterestRate, unCurrencyAmount)

-- | Interest factor for a specific period calculation
newtype InterestFactor = InterestFactor Double
  deriving (Eq, Ord, Show, Read, Num, Fractional, Real, RealFrac)

-- | Effective interest rate (accounting for compounding)
newtype EffectiveRate = EffectiveRate Double
  deriving (Eq, Ord, Show, Read, Num, Fractional, Real, RealFrac)

-- | Calculate interest factor for a given period and interest mode
-- This follows B3 methodology for different day count conventions
calculateInterestFactor :: InterestMode -> InterestRate -> Integer -> InterestFactor
calculateInterestFactor mode rate periods =
  case mode of
    -- Monthly interest calculations
    ThreeSixtyFiveMonth -> monthlyInterestFactor mode rate periods
    ThreeSixtyMonth     -> monthlyInterestFactor mode rate periods  
    TwoFiveTwoMonth     -> monthlyInterestFactor mode rate periods
    -- Daily interest calculations
    ThreeSixtyFiveDay   -> dailyInterestFactor mode rate periods
    ThreeSixtyDay       -> dailyInterestFactor mode rate periods
    TwoFiveTwoDay       -> dailyInterestFactor mode rate periods

-- | Calculate effective interest rate for different modes
calculateEffectiveRate :: InterestMode -> InterestRate -> EffectiveRate
calculateEffectiveRate mode rate =
  let yearlyRate = calculateYearlyRate mode rate
  in EffectiveRate $ ((1 + yearlyRate) ** (1/12)) - 1

-- | Calculate yearly interest rate based on the interest mode
calculateYearlyRate :: InterestMode -> InterestRate -> Double
calculateYearlyRate mode rate =
  let rateValue = unInterestRate rate / 100  -- Convert percentage to decimal
  in case mode of
    ThreeSixtyFiveMonth -> compoundRate (rateValue / 30) 365
    ThreeSixtyFiveDay   -> compoundRate (rateValue / 30) 365
    _                   -> compoundRate rateValue 12

-- | Calculate daily interest rate
calculateDailyRate :: InterestMode -> InterestRate -> Double
calculateDailyRate mode rate =
  let rateValue = unInterestRate rate / 100
  in case mode of
    ThreeSixtyFiveDay   -> compoundRate (rateValue / 30) 1
    TwoFiveTwoDay      -> ((1 + calculateYearlyRate ThreeSixtyFiveDay rate) ** (1/252)) - 1
    ThreeSixtyDay      -> ((1 + rateValue) ** (1 / 30)) - 1
    ThreeSixtyFiveMonth -> ((1 + calculateYearlyRate ThreeSixtyFiveDay rate) ** (1/365)) - 1
    ThreeSixtyMonth    -> ((1 + rateValue) ** (1 / 30)) - 1
    TwoFiveTwoMonth    -> ((1 + rateValue) ** (1 / 21)) - 1

-- | Calculate monthly interest rate
calculateMonthlyRate :: InterestMode -> InterestRate -> Double
calculateMonthlyRate mode rate =
  let effectiveRate = calculateEffectiveRate mode rate
  in case effectiveRate of (EffectiveRate r) -> r

-- | Calculate compound rate: (1 + r)^n - 1
compoundRate :: Double -> Integer -> Double
compoundRate rate periods = ((1 + rate) ^ periods) - 1

-- | Calculate monthly interest factor
monthlyInterestFactor :: InterestMode -> InterestRate -> Integer -> InterestFactor
monthlyInterestFactor mode rate periods =
  let effectiveRate = calculateEffectiveRate mode rate
  in case effectiveRate of
    (EffectiveRate r) -> InterestFactor $ ((1 + r) ^ periods) - 1

-- | Calculate daily interest factor  
dailyInterestFactor :: InterestMode -> InterestRate -> Integer -> InterestFactor
dailyInterestFactor mode rate periods =
  let dailyRate = calculateDailyRate mode rate
  in InterestFactor $ ((1 + dailyRate) ^ periods) - 1

-- | Calculate interest amount for a principal over a period
interestForPeriod :: CurrencyAmount -> InterestFactor -> CurrencyAmount
interestForPeriod principal (InterestFactor factor) =
  let principalValue = unCurrencyAmount principal
      interestValue = roundToDecimals 8 (principalValue * factor)
  in CurrencyAmount interestValue

-- | Round to specified number of decimal places
-- Brazilian financial calculations typically use 8 decimal places
roundToDecimals :: Int -> Double -> Double
roundToDecimals decimals value =
  let factor = 10 ^ decimals
  in fromIntegral (round (value * factor)) / factor

{- |
Interest Calculation Notes:

This module implements Brazilian interest calculation methodologies as specified
in the B3 "Caderno de Fórmulas" (Formula Handbook).

Interest Modes:
1. ThreeSixtyFiveMonth/Day: Uses 365-day year (actual year)
2. ThreeSixtyMonth/Day: Uses 360-day year (commercial year)  
3. TwoFiveTwoMonth/Day: Uses 252 business days (Brazilian standard)

Key concepts:
- Nominal Rate: The stated interest rate
- Effective Rate: The actual rate accounting for compounding
- Interest Factor: The multiplier applied to principal to get interest

Precision: All calculations use 8 decimal places as per Brazilian standards.

References:
- B3 "Caderno de Fórmulas - CCB, CCE e NCE"
- Brazilian financial market conventions
- ANBIMA (Brazilian Financial and Capital Markets Association) standards
-}