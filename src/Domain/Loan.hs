{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Domain.Loan
  ( Loan(..)
  , LoanRequest(..)
  , LoanValidationError(..)
  , validateLoan
  , createLoan
  , calculateTotalCommissions
  , isGracePeriod
  , getInterestPeriod
  , getPaymentSchedule
  ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import Domain.Types
import Infrastructure.DateCalculator (FinancialDate, PaymentSchedule(..))

-- | Loan validation errors with specific context
data LoanValidationError =
    InvalidLoanAmount String
  | InvalidInterestRate String  
  | InvalidLoanTerms String
  | InvalidGracePeriod String
  | InvalidStartDate String
  | InvalidCommissions String
  | InvalidPaymentFrequency String
  | InvalidPaymentEveryX String
  deriving (Eq, Show, Generic)

instance FromJSON LoanValidationError
instance ToJSON LoanValidationError

-- | Loan request from external input (before validation)
data LoanRequest = LoanRequest
  { lrAmount                :: Double
  , lrInterestRate          :: Double
  , lrInterestMode          :: InterestMode
  , lrLoanTerms             :: Int
  , lrPaymentFrequency      :: PaymentFrequency
  , lrPaymentEveryX         :: Maybe Int
  , lrStartDate             :: FinancialDate
  , lrFirstIncorporationEvent :: Maybe FinancialDate
  , lrPrincipalGracePeriodTerms :: Int
  , lrCommissions           :: [Commission]
  , lrAmortizationType      :: AmortizationType
  , lrPersonType            :: PersonType
  } deriving (Show, Generic)

instance FromJSON LoanRequest
instance ToJSON LoanRequest

-- | Validated loan domain object
data Loan = Loan
  { loanAmount              :: CurrencyAmount
  , interestRate            :: InterestRate
  , interestMode            :: InterestMode
  , loanTerms               :: LoanTerms
  , paymentSchedule         :: PaymentSchedule
  , startDate               :: FinancialDate
  , firstIncorporationEvent :: Maybe FinancialDate
  , principalGracePeriodTerms :: PaymentNumber
  , commissions             :: [Commission]
  , amortizationType        :: AmortizationType
  , personType              :: PersonType
  } deriving (Show, Generic)

instance FromJSON Loan
instance ToJSON Loan

-- | Validate a loan request and create a validated Loan
validateLoan :: LoanRequest -> Either [LoanValidationError] Loan
validateLoan LoanRequest{..} = do
  -- Validate all fields and collect errors
  validAmount <- validateAmount lrAmount
  validRate <- validateRate lrInterestRate
  validTerms <- validateTerms lrLoanTerms
  validGracePeriod <- validateGracePeriod lrLoanTerms lrPrincipalGracePeriodTerms
  validCommissions <- validateCommissions lrCommissions lrAmount
  validPaymentEveryX <- validatePaymentEveryX lrPaymentEveryX
  validatePaymentFrequencyCompatibility lrInterestMode lrPaymentFrequency
  validateStartDate lrStartDate lrFirstIncorporationEvent
  
  -- Create payment schedule
  let schedule = PaymentSchedule lrPaymentFrequency (maybe 1 id lrPaymentEveryX)
  
  -- If all validations pass, create the loan
  Right $ Loan
    { loanAmount = validAmount
    , interestRate = validRate
    , interestMode = lrInterestMode
    , loanTerms = validTerms
    , paymentSchedule = schedule
    , startDate = lrStartDate
    , firstIncorporationEvent = lrFirstIncorporationEvent
    , principalGracePeriodTerms = PaymentNumber lrPrincipalGracePeriodTerms
    , commissions = lrCommissions
    , amortizationType = lrAmortizationType
    , personType = lrPersonType
    }

-- | Smart constructor that validates and creates a loan
createLoan :: LoanRequest -> Either [LoanValidationError] Loan
createLoan = validateLoan

-- Validation functions
validateAmount :: Double -> Either [LoanValidationError] CurrencyAmount
validateAmount amount
  | amount <= 1 = Left [InvalidLoanAmount "Loan amount must be greater than 1"]
  | otherwise = Right (CurrencyAmount amount)

validateRate :: Double -> Either [LoanValidationError] InterestRate  
validateRate rate
  | rate <= 0 = Left [InvalidInterestRate "Interest rate must be positive"]
  | rate > 100 = Left [InvalidInterestRate "Interest rate cannot exceed 100%"]
  | otherwise = Right (InterestRate rate)

validateTerms :: Int -> Either [LoanValidationError] LoanTerms
validateTerms terms
  | terms <= 0 = Left [InvalidLoanTerms "Loan terms must be positive"]
  | terms > 360 = Left [InvalidLoanTerms "Loan terms cannot exceed 360 months"]
  | otherwise = Right (LoanTerms terms)

validateGracePeriod :: Int -> Int -> Either [LoanValidationError] ()
validateGracePeriod totalTerms gracePeriod
  | gracePeriod < 0 = Left [InvalidGracePeriod "Grace period cannot be negative"]
  | gracePeriod >= totalTerms = Left [InvalidGracePeriod "Grace period must be less than total loan terms"]
  | otherwise = Right ()

validateCommissions :: [Commission] -> Double -> Either [LoanValidationError] ()
validateCommissions commissions' loanAmount'
  | totalCommissionValue >= loanAmount' = 
      Left [InvalidCommissions "Total commissions cannot exceed loan amount"]
  | otherwise = Right ()
  where
    totalCommissionValue = calculateAbsoluteCommissions commissions' + calculatePercentageCommissions commissions' loanAmount'

validatePaymentEveryX :: Maybe Int -> Either [LoanValidationError] ()
validatePaymentEveryX Nothing = Right ()
validatePaymentEveryX (Just x)
  | x <= 0 = Left [InvalidPaymentEveryX "Payment frequency multiplier must be positive"]
  | x > 12 = Left [InvalidPaymentEveryX "Payment frequency multiplier cannot exceed 12"]
  | otherwise = Right ()

validatePaymentFrequencyCompatibility :: InterestMode -> PaymentFrequency -> Either [LoanValidationError] ()
validatePaymentFrequencyCompatibility mode frequency =
  case mode of
    ThreeSixtyDay -> validateDailyCompatible frequency
    ThreeSixtyFiveDay -> validateDailyCompatible frequency  
    TwoFiveTwoDay -> validateDailyCompatible frequency
    _ -> validateMonthlyCompatible frequency
  where
    validateDailyCompatible freq
      | freq `elem` [Biweekly, Weekly, Daily] = Right ()
      | otherwise = Left [InvalidPaymentFrequency "Daily interest modes require daily, weekly, or biweekly payments"]
    
    validateMonthlyCompatible freq
      | freq `elem` [Monthly, Yearly] = Right ()
      | otherwise = Left [InvalidPaymentFrequency "Monthly interest modes require monthly or yearly payments"]

validateStartDate :: FinancialDate -> Maybe FinancialDate -> Either [LoanValidationError] ()
validateStartDate startDate' (Just incorporationDate)
  | startDate' > incorporationDate = Left [InvalidStartDate "Start date cannot be after incorporation event date"]
  | otherwise = Right ()
validateStartDate _ Nothing = Right ()

-- Helper functions for commission calculations
calculateAbsoluteCommissions :: [Commission] -> Double
calculateAbsoluteCommissions = 
  sum . map (\c -> case commissionType c of
    Absolute -> unCommissionValue (commissionValue c)
    _ -> 0)

calculatePercentageCommissions :: [Commission] -> Double -> Double
calculatePercentageCommissions commissions' amount =
  sum . map (\c -> case commissionType c of
    Percentage -> (unCommissionValue (commissionValue c) / 100) * amount
    _ -> 0) $ commissions'

-- | Calculate total commission value for a loan
calculateTotalCommissions :: Loan -> CurrencyAmount
calculateTotalCommissions loan =
  let absoluteTotal = calculateAbsoluteCommissions (commissions loan)
      percentageTotal = calculatePercentageCommissions (commissions loan) (unCurrencyAmount $ loanAmount loan)
  in CurrencyAmount (absoluteTotal + percentageTotal)

-- | Check if a payment number falls within the grace period
isGracePeriod :: Loan -> PaymentNumber -> Bool
isGracePeriod loan paymentNo =
  let gracePeriodEnd = principalGracePeriodTerms loan + getInitialPaymentCount loan - 1
  in paymentNo <= gracePeriodEnd

-- | Get the interest calculation period for a specific payment
getInterestPeriod :: Loan -> PaymentNumber -> InterestPeriod
getInterestPeriod loan paymentNo
  | paymentNo == 1 && hasIncorporationEvent loan && not (isRegularLoan loan) = Day
  | otherwise = case interestMode loan of
      ThreeSixtyFiveMonth -> Month
      ThreeSixtyMonth -> Month
      TwoFiveTwoMonth -> Month
      _ -> Day

-- | Get the payment schedule for the loan
getPaymentSchedule :: Loan -> PaymentSchedule
getPaymentSchedule = paymentSchedule

-- Helper functions
hasIncorporationEvent :: Loan -> Bool
hasIncorporationEvent loan = case firstIncorporationEvent loan of
  Just _ -> True
  Nothing -> False

isRegularLoan :: Loan -> Bool
isRegularLoan loan = case firstIncorporationEvent loan of
  Just incorporationDate -> startDate loan == incorporationDate
  Nothing -> True

getInitialPaymentCount :: Loan -> PaymentNumber
getInitialPaymentCount loan = if hasIncorporationEvent loan then 2 else 1