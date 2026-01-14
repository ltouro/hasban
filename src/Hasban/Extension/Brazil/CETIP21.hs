{-# LANGUAGE DeriveGeneric #-}

{- |
Module      : Hasban.Extension.Brazil.CETIP21
Description : Brazilian CETIP21 compliant interest calculations
Copyright   : (c) 2024
License     : MIT

This module provides Brazilian-specific interest calculations following
CETIP21 standards, including 252 business day year conventions.

== CETIP21 Interest Calculation Methods

1. 252 Days (Business Days): Most common for Brazilian market
2. 360 Days (Calendar): Corporate bonds
3. 365 Days (Calendar): Government securities
4. 252 Months (21 business days/month): Monthly business day cycles
5. 360/365 Months (30 calendar days/month): Standardized monthly

== Precision Requirements

- Interest factors: 9 decimal places WITH banker's rounding
- Unit values: 8 decimal places WITHOUT rounding (truncation)
- Financial values: 2 decimal places WITHOUT rounding (truncation)
- Pro-rata ratios: 9 decimal places WITHOUT rounding (truncation)
- Input rates: 4 decimal places maximum
-}
module Hasban.Extension.Brazil.CETIP21 (
  -- * CETIP21 Interest Modes
  CETIP21Mode(..)
  , InterestMode(..)

  -- * Brazilian Loan Type
  , BrazilianLoan(..)
  , generateBrazilianSchedule

  -- * Interest Factor for Payment
  , interestFactorForPayment

  -- * 252 Business Day Calculations
  , interestFactor252BDays
  , interestFactor252Months21
  , interestFactor252MonthsIrregular

  -- * 360/365 Calendar Day Calculations
  , interestRate36XDays
  , interestRate36XMonths
  , interestRate36XMonthsIrregular
  , DaysInYear(..)

  -- * Precision Functions
  , roundCETIP21
  , truncateCETIP21
  , interestUnitValue
) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Hasban.Types (PersonalLoan(..), Payment(..), Money(..), Rate(..), Date(..))
import Hasban.Extension.Brazil.Tax (IOFRates, iofForPayment)
import Date (addTerm, numberOfDays, numberOfWeekdays)
import Hasban.Loan (periodicRate)

-- | CETIP21 interest calculation modes
data CETIP21Mode =
    Days252     -- ^ 252 business days per year
  | Months252   -- ^ 252 days with 21 business days per month
  deriving (Show, Eq, Generic)

instance FromJSON CETIP21Mode
instance ToJSON CETIP21Mode

-- | Full interest mode specification (like old LoanCalculator)
data InterestMode =
    ThreeSixtyFiveMonth  -- ^ 365 days, monthly interest
  | ThreeSixtyFiveDay    -- ^ 365 days, daily interest
  | ThreeSixtyMonth      -- ^ 360 days, monthly interest
  | ThreeSixtyDay        -- ^ 360 days, daily interest
  | TwoFiveTwoMonth      -- ^ 252 business days, monthly
  | TwoFiveTwoDay        -- ^ 252 business days, daily
  deriving (Show, Eq, Generic)

instance FromJSON InterestMode
instance ToJSON InterestMode

-- | Brazilian loan with CETIP21 compliance
data BrazilianLoan = BrazilianLoan
  { blBase         :: PersonalLoan   -- ^ Base loan configuration
  , blInterestMode :: InterestMode   -- ^ CETIP21 interest calculation mode
  , blIOFRates     :: Maybe IOFRates -- ^ Optional IOF tax rates
  } deriving (Show, Eq, Generic)

instance FromJSON BrazilianLoan
instance ToJSON BrazilianLoan

-- | Days in year convention
data DaysInYear = Days360 | Days365
  deriving (Show, Eq, Generic)

instance FromJSON DaysInYear
instance ToJSON DaysInYear

fromDaysInYear :: DaysInYear -> Integer
fromDaysInYear Days360 = 360
fromDaysInYear Days365 = 365

-- ============================================
-- 252 BUSINESS DAY CALCULATIONS
-- ============================================

-- | Interest factor for 252 business days
-- Formula: (1 + i)^(dut/252)^(dup/dut)
interestFactor252BDays
  :: Double    -- ^ Interest rate (decimal, e.g., 0.252 for 25.2%)
  -> Integer   -- ^ dut: Business days between emission and specific event
  -> Integer   -- ^ dup: Business days between emission and update date
  -> Double
interestFactor252BDays interestRate dut dup =
  let r1 = interestRate + 1
      factor1 = fromIntegral dut / 252
      factor2 = fromIntegral dup / fromIntegral dut
  in (r1 ** factor1) ** factor2

-- | Interest factor for 252 months (21 business days per month)
-- Used for matched emission dates
interestFactor252Months21
  :: Double    -- ^ Interest rate (decimal)
  -> Integer   -- ^ Number of months between emission and event
  -> Integer   -- ^ dup: Business days between emission and update date
  -> Double
interestFactor252Months21 interestRate months dup =
  let r1 = interestRate + 1
      dut = months * 21
      factor1 = fromIntegral dut / 252
      factor2 = fromIntegral dup / fromIntegral dut
  in (r1 ** factor1) ** factor2

-- | Interest factor for 252 months with irregular (unmatched) dates
-- Used when emission date doesn't align with payment anniversary
interestFactor252MonthsIrregular
  :: Double    -- ^ Interest rate (decimal)
  -> Integer   -- ^ dupProRata: Business days from emission to next anniversary
  -> Integer   -- ^ dutProRata: Business days between adjacent anniversaries
  -> Integer   -- ^ nMonths: Months between first anniversary and maturity
  -> Integer   -- ^ dup: Business days from first anniversary to update
  -> Integer   -- ^ dut: Business days from first anniversary to maturity
  -> Double
interestFactor252MonthsIrregular interestRate dupProRata dutProRata nMonths dup dut =
  let r1 = interestRate + 1
      factor1 = 21 / 252
      factor2 = fromIntegral dupProRata / fromIntegral dutProRata
      factor3 = fromIntegral nMonths * 21 / 252
      factor4 = fromIntegral dup / fromIntegral dut
  in ((r1 ** factor1) ** factor2) * ((r1 ** factor3) ** factor4)

-- ============================================
-- 360/365 CALENDAR DAY CALCULATIONS
-- ============================================

-- | Interest rate for 360/365 calendar days
-- Formula: (1 + i)^(dct/days_in_year)^(dcp/dct)
interestRate36XDays
  :: Double      -- ^ Interest rate (decimal)
  -> Integer     -- ^ dct: Calendar days between events
  -> Integer     -- ^ dcp: Calendar days to update/payment
  -> DaysInYear  -- ^ Year convention (360 or 365)
  -> Double
interestRate36XDays interestRate dct dcp daysInYear =
  let r1 = interestRate + 1
      factor1 = fromIntegral dct / fromIntegral (fromDaysInYear daysInYear)
      factor2 = fromIntegral dcp / fromIntegral dct
  in (r1 ** factor1) ** factor2

-- | Interest rate for 360/365 months (matched dates)
-- Uses 30 calendar days per month
interestRate36XMonths
  :: Double      -- ^ Interest rate (decimal)
  -> Integer     -- ^ Number of months
  -> Integer     -- ^ dcp: Calendar days pro-rata
  -> Integer     -- ^ dct: Calendar days total
  -> DaysInYear  -- ^ Year convention (360 or 365)
  -> Double
interestRate36XMonths r months dcp dct daysInYear =
  let r1 = r + 1
      factor1 = fromIntegral months / 12
      factor2 = fromIntegral dcp / fromIntegral dct
  in (r1 ** factor1) ** factor2

-- | Interest rate for 360/365 months with irregular dates
interestRate36XMonthsIrregular
  :: Double      -- ^ Interest rate (decimal)
  -> Integer     -- ^ dcpProRata: Calendar days pro-rata from emission
  -> Integer     -- ^ dctProRata: Calendar days between anniversaries
  -> Integer     -- ^ Number of months
  -> Integer     -- ^ dcp: Calendar days pro-rata
  -> Integer     -- ^ dct: Calendar days total
  -> DaysInYear  -- ^ Year convention (360 or 365)
  -> Double
interestRate36XMonthsIrregular r dcpProRata dctProRata months dcp dct daysInYear =
  let r1 = r + 1
      factor1 = 30 / fromIntegral (fromDaysInYear daysInYear)
      factor2 = fromIntegral dcpProRata / fromIntegral dctProRata
      factor3 = fromIntegral months * 30 / fromIntegral (fromDaysInYear daysInYear)
      factor4 = fromIntegral dcp / fromIntegral dct
  in ((r1 ** factor1) ** factor2) * ((r1 ** factor3) ** factor4)

-- ============================================
-- PRECISION FUNCTIONS (CETIP21 COMPLIANT)
-- ============================================

-- | Round to n decimal places (CETIP21: use 9 for interest factors)
roundCETIP21 :: Double -> Integer -> Double
roundCETIP21 num places =
  let f = 10 ^ places
  in fromIntegral (round (num * f) :: Integer) / f

-- | Truncate to n decimal places without rounding
-- CETIP21 requires truncation for unit values (8 places) and financial values (2 places)
truncateCETIP21 :: Double -> Integer -> Double
truncateCETIP21 num places =
  let f = 10 ^ places
  in fromIntegral (truncate (num * f) :: Integer) / f

-- | Calculate unit interest value (CETIP21: 8 decimal places, truncated)
interestUnitValue :: Double -> Double -> Double
interestUnitValue nominalValue interestFactor =
  truncateCETIP21 (nominalValue * interestFactor) 8

-- ============================================
-- BRAZILIAN LOAN SCHEDULE GENERATION
-- ============================================

-- | Generate payment schedule for Brazilian loan with CETIP21 interest modes
generateBrazilianSchedule :: BrazilianLoan -> [Payment]
generateBrazilianSchedule bloan = go 1 (unMoney (plPrincipal baseLoan))
  where
    baseLoan = blBase bloan
    mode = blInterestMode bloan
    iofRates = blIOFRates bloan
    n = plTerms baseLoan
    startDate = plStartDate baseLoan
    freq = plFrequency baseLoan
    principal = unMoney (plPrincipal baseLoan)
    annualRate = unRate (plAnnualRate baseLoan)

    -- Calculate payment amount using PRICE formula
    pmt = let rate = interestFactorForPayment mode annualRate startDate startDate freq 1 - 1
          in if rate == 0
             then principal / fromIntegral n
             else let factor = (1 + rate) ^ n
                  in principal * (rate * factor) / (factor - 1)

    go :: Int -> Double -> [Payment]
    go payNum balance
      | payNum > n = []
      | otherwise =
          let prevDate = if payNum == 1 then startDate else addTerm startDate freq (payNum - 1)
              currentDate = addTerm startDate freq payNum
              -- Interest factor for this period using CETIP21 mode
              intFactor = interestFactorForPayment mode annualRate startDate prevDate freq payNum
              interest = balance * (intFactor - 1)
              -- Last payment adjusts for rounding
              actualPmt = if payNum == n
                          then balance + interest
                          else pmt
              principalPaid = actualPmt - interest
              newBalance = max 0 (balance - principalPaid)
              -- Calculate IOF tax if rates provided
              daysFromStart = fromIntegral $ numberOfDays startDate currentDate
              tax = case iofRates of
                Just rates -> Just $ iofForPayment rates (Money principalPaid) daysFromStart
                Nothing -> Nothing
              payment = Payment
                { paymentNumber = payNum
                , paymentDate = currentDate
                , paymentInterest = Money interest
                , paymentPrincipal = Money principalPaid
                , paymentTotal = Money actualPmt
                , paymentBalance = Money newBalance
                , paymentTax = tax
                }
          in payment : go (payNum + 1) newBalance

-- | Calculate interest factor based on InterestMode
-- Returns compound factor for the period
interestFactorForPayment
  :: InterestMode  -- ^ Interest calculation mode
  -> Double        -- ^ Annual interest rate (decimal)
  -> Date          -- ^ Loan start date
  -> Date          -- ^ Previous payment date
  -> a             -- ^ Payment frequency (unused for now)
  -> Int           -- ^ Payment number
  -> Double        -- ^ Interest factor (1 + rate for the period)
interestFactorForPayment mode annualRate startDate prevDate _ payNum =
  case mode of
    ThreeSixtyFiveMonth ->
      -- Monthly compounding with 365-day year
      (1 + annualRate) ** (1/12)

    ThreeSixtyFiveDay ->
      -- Daily compounding with 365-day year
      let days = if payNum == 1
                 then 30  -- Assume ~30 days for first period
                 else fromIntegral $ abs $ numberOfDays startDate prevDate
      in (1 + annualRate) ** (fromIntegral days / 365)

    ThreeSixtyMonth ->
      -- Monthly compounding with 360-day year
      (1 + annualRate) ** (1/12)

    ThreeSixtyDay ->
      -- Daily compounding with 360-day year
      let days = if payNum == 1
                 then 30
                 else fromIntegral $ abs $ numberOfDays startDate prevDate
      in (1 + annualRate) ** (fromIntegral days / 360)

    TwoFiveTwoMonth ->
      -- 252 business days, monthly (21 business days)
      (1 + annualRate) ** (21/252)

    TwoFiveTwoDay ->
      -- 252 business days, actual business days counted
      let bdays = if payNum == 1
                  then 21  -- Assume ~21 business days for first period
                  else fromIntegral $ numberOfWeekdays startDate prevDate
      in (1 + annualRate) ** (fromIntegral bdays / 252)
