module Main where

import           Test.Hspec
import           Test.QuickCheck       (Arbitrary (arbitrary), Property,
                                        Testable (property), choose, elements,
                                        listOf, suchThat, within, (.||.))

import           LoanCalculator        (AmortizationTable (..),
                                        AmortizationTableItem (..),
                                        AmortizationType (PRICE, SAC),
                                        Commission (..),
                                        CommissionType (Absolute, Percentage),
                                        Frequency (..), InterestMode (..),
                                        LoanQuery (..), LoanQueryResult (..),
                                        PersonType(..),asNetValueLoan, calculate)

import           Date                  (Date (..), Frequency (..), addTerm,
                                        daysInMonth, isLeapYear)
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import LoanCalculator

main :: IO ()
main = hspec $ describe "loan properties" $ do

  it "the remaining principal of a loan ends within an acceptable range" $ do
    property $ \query ->
      let remaining = remainingPrincipal $ last $ paymentItems $ resultSchedule (calculate query)
          lowerBound = -0.49
          upperBound = 0.49
      in remaining `shouldSatisfy` (\x -> x > lowerBound && x < upperBound)

  it "the net value is less than the loan amount" $ do
    property $ \query ->
      let
          res = calculate query
          liq = resultNetValue res
          amount = loanAmount query
      in liq `shouldSatisfy` (< amount)

  it "the total payment is greater than the principal amount" $ do
    property $ \query ->
      let
          res = calculate query
          total = resultTotalPayment res
          amount = loanAmount query
      in total `shouldSatisfy` (> amount)

  it "calculates the net value of a loan within an acceptable range" $ do
    property $ \query ->
      let
        netQuery = asNetValueLoan query
        result = case netQuery of
          Nothing -> Nothing
          Just netQuery' -> Just (loanAmount query - resultNetValue (calculate netQuery'))
        lowerBound = -0.49
        upperBound = 0.49
      in case result of
        Nothing -> result `shouldBe` Nothing
        Just result' -> result' `shouldSatisfy` (\x -> x >= lowerBound && x <= upperBound)

instance Arbitrary LoanQuery where
  arbitrary = do
    amount <- scaleDecimalPlaces 2 <$> choose (2000, 25000)
    interest <- scaleDecimalPlaces 4 <$> choose (0.008, 0.8)
    terms <- choose (1, 80)
    mode <- elements [  ThreeSixtyFiveMonth, TwoFiveTwoMonth, ThreeSixtyMonth, ThreeSixtyDay, TwoFiveTwoDay ]
    paymentFreq <- case mode of
      ThreeSixtyFiveMonth -> elements [Monthly, Yearly]
      TwoFiveTwoMonth     -> elements [Monthly, Yearly]
      ThreeSixtyMonth     -> elements [Monthly, Yearly]
      _                   -> elements [Biweekly,Weekly,Daily]
    startDate <- arbitrary
    paymentStartDate <- arbitrary  `suchThat` (\x -> startDate <= x && x <= addTerm startDate Yearly 3)
    maybePaymentStartDate <- elements [Nothing, Just paymentStartDate]
    gracePeriodt <- arbitrary `suchThat` (\x -> x < terms && x >= 0)
    commissions <- take 2 <$> listOf (Commission Percentage <$> (scaleDecimalPlaces 2 <$> choose (0, 15)))
    paymentEveryX <- elements [  Nothing, Just 1, Just 2, Just 2, Just 3 ]
    amortizationType <- elements [PRICE, SAC]
    personType <- elements [Legal, Natural]
    return $ LoanQuery { loanAmount   = amount
      , interestMode              = mode
      , interestRate              = interest
      , loanTerms                 = terms
      , paymentFrequency          = paymentFreq
      , startDate                 = startDate
      , firstIncorporationEvent   = maybePaymentStartDate
      , principalGracePeriodTerms = gracePeriodt
      , paymentEveryX             = paymentEveryX
      , commissions               = commissions
      , amortizationType          = amortizationType
      , personType                = personType
      }

-- Helper
scaleDecimalPlaces :: Int -> Double -> Double
scaleDecimalPlaces n x = fromIntegral (round (x * (10^n))) / (10.0^^n)

data Date = Date Int Int Int deriving (Eq, Show)
instance Arbitrary Date.Date where
  arbitrary = do
    -- Generate random values for year, month, and day
    year <- choose (2020, 2023)
    month <- choose (1, 12)
    day <- choose (1, daysInMonth year month)
    return $ Date.Date (fromIntegral year) month day
