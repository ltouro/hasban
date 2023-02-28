module Main where

import Test.Hspec
import Test.QuickCheck ( 
  choose
  , elements
  , within
  , listOf
  , suchThat
  , (.||.)
  , Arbitrary(arbitrary)
  , Testable( property) 
  )

import LoanCalculator(
  LoanQuery (..)
  , Commission (..)
  , Frequency (..)
  , LoanQuery
  , CommissionType(Absolute, Percentage)
  , AmortizationType(SAC, PRICE)
  , queryLoan
  , asNetValueLoan
  , LoanQueryResult (..)
  , AmortizationTable (..)
  , AmortizationTableItem (..)
  )

import Date (
  Frequency(..)
  , Date(..) )
  
main :: IO ()
main = hspec $ describe "loan properties" $ do
  it "calculates the net value of a loan within an acceptable range" $ do
    property $ \query ->
      let
        netQuery = asNetValueLoan query
        result = case netQuery of
          Nothing -> Nothing
          Just netQuery' -> Just (loanAmount query - resultNetValue (queryLoan netQuery'))
        lowerBound = -0.49
        upperBound = 0.49
      in case result of
        Nothing -> result `shouldBe` Nothing
        Just result' -> result' `shouldSatisfy` (\x -> x >= lowerBound && x <= upperBound)

  it "the remaining principal of a loan ends within an acceptable range" $ do
    property $ \query ->
      let remaining = remainingPrincipal $ last $ paymentItems $ resultSchedule (queryLoan query)
          lowerBound = -0.49
          upperBound = 0.49
      in remaining `shouldSatisfy` (\x -> x > lowerBound && x < upperBound)

  it "the net value is less than the loan amount" $ do
    property $ \query ->
      let
          res = queryLoan query
          liq = resultNetValue res
          amount = loanAmount query
      in liq `shouldSatisfy` (< amount)

  it "the net value is less than the loan amount" $ do
    property $ \query ->
      let
          res = queryLoan query
          liq = resultNetValue res
          amount = loanAmount query
      in liq `shouldSatisfy` (< amount)

  it "the total payment is greater than the principal amount" $ do
    property $ \query ->
      let
          res = queryLoan query
          total = resultTotalPayment res
          amount = loanAmount query
      in total `shouldSatisfy` (> amount)



instance Arbitrary LoanQuery where
  arbitrary = do
    amount <- scaleDecimalPlaces 2 <$> choose (1000, 100000)
    interest <- scaleDecimalPlaces 4 <$> choose (0.01, 0.1)
    terms <- choose (1, 240)
    paymentFreq <- elements [Monthly, Biweekly, Monthly]
    accrualFreq <- elements [Monthly, Monthly]
    startDate <- arbitrary
    graceInterest <- arbitrary `suchThat` (\x -> x < terms && x >= 0)
    gracePrincipal <- arbitrary `suchThat` (\x -> x < terms && x >= 0 && x >= graceInterest)
    commissions <- take 2 <$> listOf (Commission Percentage <$> (scaleDecimalPlaces 2 <$> choose (0, 10)))    
    amortizationType <- elements [PRICE, SAC]
    return $ LoanQuery amount interest Monthly terms paymentFreq accrualFreq startDate graceInterest gracePrincipal commissions amortizationType


-- Helper
scaleDecimalPlaces :: Int -> Double -> Double
scaleDecimalPlaces n x = fromIntegral (round (x * (10^n))) / (10.0^^n)

data Date = Date Int Int Int deriving (Eq, Show)
instance Arbitrary Date.Date where
  arbitrary = do
    -- Generate random values for year, month, and day
    year <- choose (2000, 2050)
    month <- choose (1, 12)
    day <- choose (1, daysInMonth year month)
    return $ Date.Date (fromIntegral year) month day

-- Helper function to get the number of days in a given month
daysInMonth :: Int -> Int -> Int
daysInMonth year month
  | month `elem` [1, 3, 5, 7, 8, 10, 12] = 31
  | month `elem` [4, 6, 9, 11] = 30
  | isLeapYear year = 29
  | otherwise = 28

-- Helper function to check if a year is a leap year
isLeapYear :: Int -> Bool
isLeapYear year
  | year `mod` 400 == 0 = True
  | year `mod` 100 == 0 = False
  | year `mod` 4 == 0 = True
  | otherwise = False