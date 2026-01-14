{-# LANGUAGE OverloadedStrings #-}

module Precision.CETIP21Spec (
  spec,
  module Precision.CETIP21  -- Re-export from source module
) where

import Test.Hspec
import Test.QuickCheck
import Precision.CETIP21

-- Count decimal places in a Double (helper for tests)
countDecimalPlaces :: Double -> Int
countDecimalPlaces value =
  let str = show value
      afterDot = drop 1 $ dropWhile (/= '.') str
  in if '.' `elem` str
     then length $ takeWhile (/= 'e') afterDot  -- Handle scientific notation
     else 0

spec :: Spec
spec = describe "CETIP21 Precision Compliance" $ do

  describe "Interest Factor Precision (9 decimal places WITH banker's rounding)" $ do
    it "applies banker's rounding correctly for 0.5 cases" $ do
      -- Test banker's rounding: round to nearest even
      -- Using larger values to avoid floating point representation issues at very small decimals
      applyPrecision InterestFactorPrecision 1.1234567895 `shouldBe` 1.123456790  -- Round up (8 is even, so round to 9)
      applyPrecision InterestFactorPrecision 1.1234567885 `shouldBe` 1.123456788  -- Round down (8 is even)
      applyPrecision InterestFactorPrecision 1.1234567875 `shouldBe` 1.123456788  -- Round up (7 is odd, so round to 8)

    it "maintains exactly 9 decimal places" $ do
      property $ \x ->
        let result = applyPrecision InterestFactorPrecision (abs x)
            decimalPlaces = countDecimalPlaces result
        in decimalPlaces <= 9

    it "handles edge cases correctly" $ do
      applyPrecision InterestFactorPrecision 0.0 `shouldBe` 0.0
      applyPrecision InterestFactorPrecision 1.0 `shouldBe` 1.0
      applyPrecision InterestFactorPrecision 1.123456789123 `shouldBe` 1.123456789

  describe "Unit Value Precision (8 decimal places WITHOUT rounding)" $ do
    it "truncates without rounding" $ do
      applyPrecision UnitValuePrecision 1.123456789 `shouldBe` 1.12345678
      applyPrecision UnitValuePrecision 1.999999999 `shouldBe` 1.99999999
      applyPrecision UnitValuePrecision 2.000000001 `shouldBe` 2.00000000

    it "maintains exactly 8 decimal places or fewer" $ do
      property $ \x ->
        let result = applyPrecision UnitValuePrecision (abs x)
            decimalPlaces = countDecimalPlaces result
        in decimalPlaces <= 8

    it "handles unit interest value calculation" $ do
      let nominalValue = 1000.0
      let interestFactor = 1.02083333  -- Should be truncated to 8 places
      let unitInterest = nominalValue * (interestFactor - 1)
      let result = applyPrecision UnitValuePrecision unitInterest
      countDecimalPlaces result <= 8 `shouldBe` True

  describe "Financial Value Precision (2 decimal places WITHOUT rounding)" $ do
    it "truncates to 2 decimal places without rounding" $ do
      applyPrecision FinancialValuePrecision 123.999 `shouldBe` 123.99
      applyPrecision FinancialValuePrecision 100.001 `shouldBe` 100.00
      applyPrecision FinancialValuePrecision 99.995 `shouldBe` 99.99  -- No rounding, just truncation

    it "maintains exactly 2 decimal places or fewer" $ do
      property $ \x ->
        let result = applyPrecision FinancialValuePrecision (abs x)
            decimalPlaces = countDecimalPlaces result
        in decimalPlaces <= 2

    it "calculates financial interest value correctly" $ do
      let unitInterest = 20.83333333  -- 8 decimal places
      let quantity = 100
      let financialValue = unitInterest * fromIntegral quantity
      let result = applyPrecision FinancialValuePrecision financialValue
      result `shouldBe` 2083.33  -- Truncated to 2 decimal places

  describe "Pro-rata Precision (9 decimal places WITHOUT rounding)" $ do
    it "truncates to 9 decimal places without rounding" $ do
      applyPrecision ProRataPrecision 0.1234567899 `shouldBe` 0.123456789
      applyPrecision ProRataPrecision 0.9999999999 `shouldBe` 0.999999999

    it "maintains exactly 9 decimal places or fewer" $ do
      property $ \x ->
        let result = applyPrecision ProRataPrecision (abs x)
            decimalPlaces = countDecimalPlaces result
        in decimalPlaces <= 9

    it "calculates pro-rata ratios correctly" $ do
      let dcp_pro_rata = 15  -- Calendar days
      let dct_pro_rata = 30  -- Total calendar days
      let ratio = fromIntegral dcp_pro_rata / fromIntegral dct_pro_rata
      let result = applyPrecision ProRataPrecision ratio
      result `shouldBe` 0.5  -- Exact ratio

  describe "Input Rate Precision (4 decimal places maximum)" $ do
    it "validates input rates have maximum 4 decimal places" $ do
      applyPrecision InputRatePrecision 0.1234 `shouldBe` 0.1234  -- Valid
      applyPrecision InputRatePrecision 0.12345 `shouldBe` 0.1234  -- Truncated

    it "maintains maximum 4 decimal places" $ do
      property $ \x ->
        let result = applyPrecision InputRatePrecision (abs x)
            decimalPlaces = countDecimalPlaces result
        in decimalPlaces <= 4

    it "handles typical interest rates" $ do
      applyPrecision InputRatePrecision 0.252 `shouldBe` 0.252    -- 25.2% annual
      applyPrecision InputRatePrecision 0.12 `shouldBe` 0.12      -- 12% annual
      applyPrecision InputRatePrecision 0.08 `shouldBe` 0.08      -- 8% annual

  describe "CETIP21 Calculation Chain Precision" $ do
    it "maintains precision through complete calculation chain" $ do
      -- Simulate a complete CETIP21 calculation
      let inputRate = applyPrecision InputRatePrecision 0.252  -- Input: 25.2% annual
      let businessDays = 22
      let totalBusinessDays = 252

      -- Calculate interest factor (9 decimal places WITH rounding)
      let interestFactor = (1 + inputRate) ** (fromIntegral businessDays / fromIntegral totalBusinessDays)
      let preciseInterestFactor = applyPrecision InterestFactorPrecision interestFactor

      -- Calculate unit interest value (8 decimal places WITHOUT rounding)
      let nominalValue = 1000.0
      let unitInterest = nominalValue * (preciseInterestFactor - 1)
      let preciseUnitInterest = applyPrecision UnitValuePrecision unitInterest

      -- Calculate financial value (2 decimal places WITHOUT rounding)
      let quantity = 100
      let financialValue = preciseUnitInterest * fromIntegral quantity
      let preciseFinancialValue = applyPrecision FinancialValuePrecision financialValue

      -- Verify precision requirements
      countDecimalPlaces preciseInterestFactor <= 9 `shouldBe` True
      countDecimalPlaces preciseUnitInterest <= 8 `shouldBe` True
      countDecimalPlaces preciseFinancialValue <= 2 `shouldBe` True

  describe "Precision Error Boundaries" $ do
    it "handles very small numbers correctly" $ do
      applyPrecision InterestFactorPrecision 0.000000001 `shouldBe` 0.000000001
      applyPrecision UnitValuePrecision 0.00000001 `shouldBe` 0.00000001
      applyPrecision FinancialValuePrecision 0.01 `shouldBe` 0.01

    it "handles very large numbers correctly" $ do
      applyPrecision InterestFactorPrecision 999999.123456789 `shouldBe` 999999.123456789
      applyPrecision UnitValuePrecision 999999.12345678 `shouldBe` 999999.12345678
      applyPrecision FinancialValuePrecision 999999.99 `shouldBe` 999999.99

-- QuickCheck generator for testing precision with random values
instance Arbitrary PrecisionType where
  arbitrary = elements [InterestFactorPrecision, UnitValuePrecision,
                       FinancialValuePrecision, ProRataPrecision, InputRatePrecision]