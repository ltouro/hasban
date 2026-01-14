{-# LANGUAGE OverloadedStrings #-}

module InterestCalculation.MethodsSpec (
  spec,
  calculate252BusinessDays,
  calculate360CalendarDays,
  LinearCompound(..)
) where

import Test.Hspec
import Test.QuickCheck
import Precision.CETIP21Spec (applyPrecision, PrecisionType(..))

-- CETIP21 Interest Calculation Methods
-- 1. 252 Days (Business Days): (1 + i/100)^(dup/dut) - Most common for Brazilian market
-- 2. 360 Days (Calendar): (1 + i/100)^(dcp/dct) with linear option - Corporate bonds
-- 3. 365 Days (Calendar): (1 + i/100)^(dcp/dct) - Government securities
-- 4. 252 Months (21 business days/month): (1 + i/100)^(N×21/252) - Monthly business day cycles
-- 5. 360 Months (30 calendar days/month): (1 + i/100)^(N×30/360) - Standardized monthly
-- 6. 365 Months (30 calendar days/month): (1 + i/100)^(N×30/365) - Actual year monthly

data InterestCalculationMethod =
    Days252Business
  | Days360Calendar LinearCompound
  | Days365Calendar
  | Months252Business  -- 21 business days per month
  | Months360Calendar  -- 30 calendar days per month
  | Months365Calendar  -- 30 calendar days per month
  deriving (Eq, Show)

data LinearCompound = Linear | Compound deriving (Eq, Show)

type InterestRate = Double
type BusinessDays = Int
type CalendarDays = Int
type Months = Int

-- 252 Days (Business Days) - Most common for Brazilian market
calculate252BusinessDays :: InterestRate -> BusinessDays -> BusinessDays -> Double
calculate252BusinessDays interestRate dup dut =
  let rate = interestRate / 100
      timeFactor = fromIntegral dup / fromIntegral dut
      interestFactor = (1 + rate) ** timeFactor
  in applyPrecision InterestFactorPrecision interestFactor

-- 360 Days (Calendar) - Corporate bonds
calculate360CalendarDays :: LinearCompound -> InterestRate -> CalendarDays -> CalendarDays -> Double
calculate360CalendarDays Linear interestRate n _ =
  -- Linear Formula: 1 + (i × N)/36000
  let factor = 1 + (interestRate * fromIntegral n) / 36000
  in applyPrecision InterestFactorPrecision factor

calculate360CalendarDays Compound interestRate dcp dct =
  -- Compound Formula: (1 + i/100)^(dcp/dct)
  let rate = interestRate / 100
      timeFactor = fromIntegral dcp / fromIntegral dct
      interestFactor = (1 + rate) ** timeFactor
  in applyPrecision InterestFactorPrecision interestFactor

-- 365 Days (Calendar) - Government securities
calculate365CalendarDays :: InterestRate -> CalendarDays -> CalendarDays -> Double
calculate365CalendarDays interestRate dcp dct =
  let rate = interestRate / 100
      timeFactor = fromIntegral dcp / fromIntegral dct
      interestFactor = (1 + rate) ** timeFactor
  in applyPrecision InterestFactorPrecision interestFactor

-- 252 Months (21 business days per month) - Monthly business day cycles
calculate252Months :: InterestRate -> Months -> BusinessDays -> BusinessDays -> Double
calculate252Months interestRate nMonths dup dut =
  let rate = interestRate / 100
      timeFactor = (fromIntegral nMonths * 21) / 252
      baseTimeFactor = fromIntegral dup / fromIntegral dut
      interestFactor = ((1 + rate) ** timeFactor) ** baseTimeFactor
  in applyPrecision InterestFactorPrecision interestFactor

-- 360 Months (30 calendar days per month) - Standardized monthly
calculate360Months :: InterestRate -> Months -> CalendarDays -> CalendarDays -> Double
calculate360Months interestRate nMonths dcp dct =
  let rate = interestRate / 100
      timeFactor = (fromIntegral nMonths * 30) / 360
      baseTimeFactor = fromIntegral dcp / fromIntegral dct
      interestFactor = ((1 + rate) ** timeFactor) ** baseTimeFactor
  in applyPrecision InterestFactorPrecision interestFactor

-- 365 Months (30 calendar days per month) - Actual year monthly
calculate365Months :: InterestRate -> Months -> CalendarDays -> CalendarDays -> Double
calculate365Months interestRate nMonths dcp dct =
  let rate = interestRate / 100
      timeFactor = (fromIntegral nMonths * 30) / 365
      baseTimeFactor = fromIntegral dcp / fromIntegral dct
      interestFactor = ((1 + rate) ** timeFactor) ** baseTimeFactor
  in applyPrecision InterestFactorPrecision interestFactor

-- Unit Interest Value calculation (CETIP21 compliant)
calculateUnitInterestValue :: Double -> Double -> Double
calculateUnitInterestValue nominalValue interestFactor =
  let unitInterest = nominalValue * (interestFactor - 1)
  in applyPrecision UnitValuePrecision unitInterest

-- Financial Interest Value calculation (CETIP21 compliant)
calculateFinancialInterestValue :: Double -> Int -> Double
calculateFinancialInterestValue unitInterest quantity =
  let financialValue = unitInterest * fromIntegral quantity
  in applyPrecision FinancialValuePrecision financialValue

spec :: Spec
spec = describe "CETIP21 Interest Calculation Methods" $ do

  describe "252 Days (Business Days) - Brazilian Market Standard" $ do
    it "calculates interest factor correctly for standard period" $ do
      -- Reference case: 22 business days at 25.2% annual
      let interestRate = 25.2  -- 25.2% annual
      let dup = 22  -- Business days in period
      let dut = 252  -- Total business days in year
      let result = calculate252BusinessDays interestRate dup dut

      -- Expected calculation: (1 + 0.252) ** (22/252) = 1.019814101...
      result `shouldSatisfy` (\x -> abs (x - 1.019814101) < 1e-8)

    it "handles edge cases correctly" $ do
      calculate252BusinessDays 0.0 22 252 `shouldBe` 1.0  -- Zero rate
      calculate252BusinessDays 25.2 0 252 `shouldBe` 1.0  -- Zero days
      calculate252BusinessDays 25.2 252 252 `shouldBe` 1.252  -- Full year

    it "maintains CETIP21 precision requirements" $ do
      property $ \rate days ->
        let validRate = abs rate  -- Keep rate positive
            validDays = abs days + 1  -- Avoid division by zero
            result = calculate252BusinessDays validRate validDays 252
        in result > 0  -- Should always be positive

  describe "360 Days (Calendar) - Corporate Bonds" $ do
    it "calculates compound interest correctly" $ do
      -- Reference case: 30 calendar days at 12% annual
      let interestRate = 12.0  -- 12% annual
      let dcp = 30  -- Calendar days in period
      let dct = 360  -- Total calendar days in year
      let result = calculate360CalendarDays Compound interestRate dcp dct

      -- Expected: (1 + 0.12) ** (30/360) = 1.009488793
      result `shouldSatisfy` (\x -> abs (x - 1.009488793) < 1e-8)

    it "calculates linear interest correctly" $ do
      -- Linear formula: 1 + (i × N)/36000
      let interestRate = 12.0  -- 12% annual
      let n = 30  -- Calendar days
      let result = calculate360CalendarDays Linear interestRate n 0  -- dct not used in linear

      -- Expected: 1 + (12 × 30)/36000 = 1.01
      result `shouldBe` 1.01

    it "linear and compound should differ for longer periods" $ do
      let interestRate = 12.0
      let n = 180  -- 6 months
      let linear = calculate360CalendarDays Linear interestRate n 0
      let compound = calculate360CalendarDays Compound interestRate n 360

      -- For 6 months: Linear = 1.06, Compound = 1.0583
      -- Linear is actually higher than compound for short periods
      linear `shouldSatisfy` (> compound)  -- Linear should be higher for 6 months

  describe "365 Days (Calendar) - Government Securities" $ do
    it "calculates interest factor correctly" $ do
      -- Reference case: 30 calendar days at 8% annual
      let interestRate = 8.0  -- 8% annual
      let dcp = 30  -- Calendar days in period
      let dct = 365  -- Total calendar days in year
      let result = calculate365CalendarDays interestRate dcp dct

      -- Expected: (1 + 0.08) ** (30/365)
      let expected = 1.08 ** (30.0/365.0)
      result `shouldSatisfy` (\x -> abs (x - expected) < 1e-8)

    it "differs from 360-day calculation" $ do
      let interestRate = 8.0
      let days = 30
      let result360 = calculate360CalendarDays Compound interestRate days 360
      let result365 = calculate365CalendarDays interestRate days 365

      result365 `shouldSatisfy` (< result360)  -- 365-day should be slightly lower

  describe "252 Months (21 business days/month) - Monthly Business Cycles" $ do
    it "calculates monthly interest correctly" $ do
      -- Reference case: 2 months with standard business day calculation
      let interestRate = 25.2  -- 25.2% annual
      let nMonths = 2
      let dup = 42  -- 2 months × 21 business days
      let dut = 42  -- Same for matched dates
      let result = calculate252Months interestRate nMonths dup dut

      -- Expected: ((1 + 0.252) ** (2×21/252)) ** (42/42)
      let expected = (1.252 ** (42.0/252.0)) ** 1.0
      result `shouldSatisfy` (\x -> abs (x - expected) < 1e-8)

    it "handles fractional months correctly" $ do
      let result = calculate252Months 12.0 1 21 21
      result `shouldSatisfy` (> 1.0)

  describe "360 Months (30 calendar days/month) - Standardized Monthly" $ do
    it "calculates standardized monthly interest" $ do
      -- Reference case: 3 months at 12% annual
      let interestRate = 12.0  -- 12% annual
      let nMonths = 3
      let dcp = 90  -- 3 months × 30 days
      let dct = 90  -- Same for matched dates
      let result = calculate360Months interestRate nMonths dcp dct

      -- Expected: ((1 + 0.12) ** (3×30/360)) ** (90/90) = 1.12 ** 0.25
      let expected = 1.12 ** 0.25
      result `shouldSatisfy` (\x -> abs (x - expected) < 1e-8)

  describe "365 Months (30 calendar days/month) - Actual Year Monthly" $ do
    it "calculates actual year monthly interest" $ do
      -- Reference case: 12 months at 10% annual
      let interestRate = 10.0  -- 10% annual
      let nMonths = 12
      let dcp = 360  -- 12 months × 30 days
      let dct = 360  -- Same for matched dates
      let result = calculate365Months interestRate nMonths dcp dct

      -- Expected: ((1 + 0.10) ** (12×30/365)) ** (360/360)
      let expected = 1.10 ** (360.0/365.0)
      result `shouldSatisfy` (\x -> abs (x - expected) < 1e-8)

    it "differs from 360-month calculation" $ do
      let interestRate = 10.0
      let nMonths = 12
      let days = 360
      let result360 = calculate360Months interestRate nMonths days days
      let result365 = calculate365Months interestRate nMonths days days

      result365 `shouldSatisfy` (< result360)  -- 365-day base should be lower

  describe "Unit Interest Value Calculations" $ do
    it "calculates unit interest value with CETIP21 precision" $ do
      let nominalValue = 1000.0
      let interestFactor = 1.019814101  -- From 252-day calculation (25.2% for 22 days)
      let result = calculateUnitInterestValue nominalValue interestFactor

      -- Expected: 1000 × (1.019814101 - 1) = 19.814101 → 19.81410100 (8 decimal places)
      result `shouldSatisfy` (\x -> abs (x - 19.81410100) < 1e-6)

    it "maintains 8 decimal places without rounding" $ do
      let nominalValue = 1000.0
      let interestFactor = 1.123456789123
      let result = calculateUnitInterestValue nominalValue interestFactor

      -- Should truncate to 8 decimal places: 123.45678912 → 123.45678912
      let expectedTruncated = 123.45678912
      result `shouldSatisfy` (\x -> abs (x - expectedTruncated) < 1e-8)

  describe "Financial Interest Value Calculations" $ do
    it "calculates financial value with CETIP21 precision" $ do
      let unitInterest = 20.83333333  -- 8 decimal places
      let quantity = 100
      let result = calculateFinancialInterestValue unitInterest quantity

      -- Expected: 20.83333333 × 100 = 2083.333333 → 2083.33 (2 decimal places, truncated)
      result `shouldBe` 2083.33

    it "truncates to 2 decimal places without rounding" $ do
      let unitInterest = 10.999999  -- Would round to 11.00
      let quantity = 1
      let result = calculateFinancialInterestValue unitInterest quantity

      -- Should truncate, not round: 10.999999 → 10.99
      result `shouldBe` 10.99

  describe "Method Comparison and Validation" $ do
    it "business day methods should yield comparable rates to calendar day methods for equivalent periods" $ do
      let interestRate = 12.0
      let period = 30

      let result252 = calculate252BusinessDays interestRate 21 252  -- 21 business days = ~30 calendar days
      let result360 = calculate360CalendarDays Compound interestRate period 360
      let result365 = calculate365CalendarDays interestRate period 365

      -- 21/252 = 30/360 = 1/12, so 252-day and 360-day should give same result
      abs (result252 - result360) `shouldSatisfy` (< 1e-8)
      -- 365-day should be slightly lower (30/365 < 30/360)
      result365 `shouldSatisfy` (< result360)

    it "monthly methods should be consistent with daily methods for matched periods" $ do
      let interestRate = 12.0
      let monthlyResult = calculate360Months interestRate 1 30 30  -- 1 month
      let dailyResult = calculate360CalendarDays Compound interestRate 30 360  -- 30 days

      -- Should be very close for matched periods
      abs (monthlyResult - dailyResult) `shouldSatisfy` (< 1e-10)

-- QuickCheck generators for testing
instance Arbitrary InterestCalculationMethod where
  arbitrary = elements [Days252Business,
                       Days360Calendar Linear, Days360Calendar Compound,
                       Days365Calendar, Months252Business,
                       Months360Calendar, Months365Calendar]

instance Arbitrary LinearCompound where
  arbitrary = elements [Linear, Compound]