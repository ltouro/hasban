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
                                        asNetValueLoan, queryLoan)

import           Date                  (Date (..), Frequency (..), addTerm, daysInMonth, isLeapYear)
import           Test.Hspec.QuickCheck (modifyMaxSuccess)

main :: IO ()
main = hspec $ describe "loan properties" $ do
  modifyMaxSuccess (const 100) $ it "calculates the net value of a loan within an acceptable range" $ do
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

  modifyMaxSuccess (const 100) $ it "the remaining principal of a loan ends within an acceptable range" $ do
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
    paymentFreq <- elements [Monthly,Biweekly,Weekly,Daily]    
    startDate <- arbitrary
    -- mode <- elements [  ThreeSixtyFiveMonth, ThreeSixtyDay, TwoFiveTwoMonth, TwoFiveTwoDay ]
    paymentStartDate <- arbitrary  `suchThat` (\x -> startDate <= x && x <= addTerm startDate Yearly 3)
    maybePaymentStartDate <- elements [Nothing, Just paymentStartDate]
    gracePeriodt <- arbitrary `suchThat` (\x -> x < terms && x >= 0)
    commissions <- take 2 <$> listOf (Commission Percentage <$> (scaleDecimalPlaces 2 <$> choose (0, 15)))
    amortizationType <- elements [PRICE, SAC]
    return $ LoanQuery { loanAmount   = amount
      , interestMode              = ThreeSixtyFiveMonth
      , interestRate              = interest
      , loanTerms                 = terms
      , paymentFrequency          = paymentFreq
      , startDate                 = startDate
      , firstIncorporationEvent = maybePaymentStartDate
      , principalGracePeriodTerms = gracePeriodt
      , commissions               = commissions
      , amortizationType          = amortizationType
      }

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
