module Main where

import           Test.Hspec
import           Test.QuickCheck       (Arbitrary (arbitrary), Property,
                                        Testable (property), choose, elements,
                                        listOf, suchThat, within, (.||.))

import           Domain.Types          (AmortizationType (PRICE, SAC),
                                        Commission (..),
                                        CommissionType (Absolute, Percentage),
                                        PaymentFrequency (..), InterestMode (..),
                                        PersonType(..), CurrencyAmount(..), InterestRate(..),
                                        LoanTerms(..), PaymentNumber(..))
import           Domain.Loan           (LoanRequest(..), validateLoan)
import           Domain.AmortizationSchedule (AmortizationSchedule (..), PaymentItem (..),
                                        AmortizationResult (..), calculateAmortizationSchedule)
import           Infrastructure.DateCalculator (FinancialDate, mkFinancialDate, isLeapYear)
import           Application.LoanService (calculateNetValueLoan, NetValueResult(..))
import           Test.Hspec.QuickCheck (modifyMaxSuccess)

main :: IO ()
main = hspec $ describe "loan properties" $ do

  it "the remaining principal of a loan ends within an acceptable range" $ do
    property $ \request ->
      case validateLoan request of
        Right validLoan -> 
          let result = calculateAmortizationSchedule validLoan
              items = paymentItems (schedule result)
              remaining = unCurrencyAmount $ remainingPrincipal $ last items
              lowerBound = -0.49
              upperBound = 0.49
          in remaining `shouldSatisfy` (\x -> x > lowerBound && x < upperBound)
        Left _ -> True `shouldBe` True  -- Skip invalid loans

  it "the net value is less than the loan amount" $ do
    property $ \request ->
      case validateLoan request of
        Right validLoan -> 
          let result = calculateAmortizationSchedule validLoan
              liq = unCurrencyAmount $ netValue result
              amount = unCurrencyAmount $ loanAmount validLoan
          in liq `shouldSatisfy` (< amount)
        Left _ -> True `shouldBe` True  -- Skip invalid loans

  it "the total payment is greater than the principal amount" $ do
    property $ \request ->
      case validateLoan request of
        Right validLoan -> 
          let result = calculateAmortizationSchedule validLoan
              total = unCurrencyAmount $ totalPayments result
              amount = unCurrencyAmount $ loanAmount validLoan
          in total `shouldSatisfy` (> amount)
        Left _ -> True `shouldBe` True  -- Skip invalid loans

  it "calculates the net value of a loan within an acceptable range" $ do
    property $ \request ->
      case calculateNetValueLoan request of
        NetValueSuccess adjustedRequest result ->
          let originalAmount = lrAmount request
              actualNetValue = unCurrencyAmount $ netValue result
              difference = abs (originalAmount - actualNetValue)
              tolerance = 0.49
          in difference `shouldSatisfy` (<= tolerance)
        NetValueFailure _ -> True `shouldBe` True  -- Skip failed calculations

instance Arbitrary LoanRequest where
  arbitrary = do
    amount <- scaleDecimalPlaces 2 <$> choose (2000, 25000)
    interest <- scaleDecimalPlaces 4 <$> choose (0.008, 0.8)
    terms <- choose (1, 80)
    mode <- elements [ThreeSixtyFiveMonth, TwoFiveTwoMonth, ThreeSixtyMonth, ThreeSixtyDay, TwoFiveTwoDay]
    paymentFreq <- case mode of
      ThreeSixtyFiveMonth -> elements [Monthly, Yearly]
      TwoFiveTwoMonth     -> elements [Monthly, Yearly]
      ThreeSixtyMonth     -> elements [Monthly, Yearly]
      _                   -> elements [Biweekly, Weekly, Daily]
    startDate <- arbitrary
    paymentStartDate <- arbitrary `suchThat` (\x -> startDate <= x)
    maybePaymentStartDate <- elements [Nothing, Just paymentStartDate]
    gracePeriodt <- arbitrary `suchThat` (\x -> x < terms && x >= 0)
    commissions <- take 2 <$> listOf (Commission Percentage <$> (CommissionValue <$> scaleDecimalPlaces 2 <$> choose (0, 15)))
    paymentEveryX <- elements [Nothing, Just 1, Just 2, Just 2, Just 3]
    amortizationType <- elements [PRICE, SAC]
    personType <- elements [Legal, Natural]
    return $ LoanRequest 
      { lrAmount = amount
      , lrInterestMode = mode
      , lrInterestRate = interest
      , lrLoanTerms = terms
      , lrPaymentFrequency = paymentFreq
      , lrStartDate = startDate
      , lrFirstIncorporationEvent = maybePaymentStartDate
      , lrPrincipalGracePeriodTerms = gracePeriodt
      , lrPaymentEveryX = paymentEveryX
      , lrCommissions = commissions
      , lrAmortizationType = amortizationType
      , lrPersonType = personType
      }

-- Helper
scaleDecimalPlaces :: Int -> Double -> Double
scaleDecimalPlaces n x = fromIntegral (round (x * (10^n))) / (10.0^^n)

instance Arbitrary FinancialDate where
  arbitrary = do
    -- Generate random values for year, month, and day
    year <- choose (2020, 2023)
    month <- choose (1, 12)
    day <- choose (1, daysInMonth year month)
    case mkFinancialDate (fromIntegral year) month day of
      Right date -> return date
      Left _ -> arbitrary  -- Retry if invalid date

daysInMonth :: Int -> Int -> Int
daysInMonth year month
  | month `elem` [1, 3, 5, 7, 8, 10, 12] = 31
  | month `elem` [4, 6, 9, 11] = 30
  | isLeapYear (fromIntegral year) = 29
  | otherwise = 28
