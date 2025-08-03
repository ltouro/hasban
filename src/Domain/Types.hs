{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Domain.Types where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

-- | Currency amount with proper type safety
newtype CurrencyAmount = CurrencyAmount Double
  deriving (Eq, Ord, Show, Read, Num, Fractional, Real, RealFrac, Generic, FromJSON, ToJSON)

-- | Interest rate as a percentage (e.g., 8.96 for 8.96%)
newtype InterestRate = InterestRate Double
  deriving (Eq, Ord, Show, Read, Num, Fractional, Real, RealFrac, Generic, FromJSON, ToJSON)

-- | Payment number in the amortization schedule
newtype PaymentNumber = PaymentNumber Int
  deriving (Eq, Ord, Show, Read, Num, Enum, Integral, Real, Generic, FromJSON, ToJSON)

-- | Number of terms in the loan
newtype LoanTerms = LoanTerms Int
  deriving (Eq, Ord, Show, Read, Num, Enum, Integral, Real, Generic, FromJSON, ToJSON)

-- | Tax amount
newtype TaxAmount = TaxAmount Double
  deriving (Eq, Ord, Show, Read, Num, Fractional, Real, RealFrac, Generic, FromJSON, ToJSON)

-- | Commission value
newtype CommissionValue = CommissionValue Double
  deriving (Eq, Ord, Show, Read, Num, Fractional, Real, RealFrac, Generic, FromJSON, ToJSON)

-- Smart constructors with validation
mkCurrencyAmount :: Double -> Either String CurrencyAmount
mkCurrencyAmount amount
  | amount <= 0 = Left "Currency amount must be positive"
  | otherwise = Right (CurrencyAmount amount)

mkInterestRate :: Double -> Either String InterestRate
mkInterestRate rate
  | rate <= 0 = Left "Interest rate must be positive"
  | rate > 100 = Left "Interest rate cannot exceed 100%"
  | otherwise = Right (InterestRate rate)

mkLoanTerms :: Int -> Either String LoanTerms
mkLoanTerms terms
  | terms <= 0 = Left "Loan terms must be positive"
  | terms > 360 = Left "Loan terms cannot exceed 360 months"
  | otherwise = Right (LoanTerms terms)

-- Conversion functions
unCurrencyAmount :: CurrencyAmount -> Double
unCurrencyAmount (CurrencyAmount amount) = amount

unInterestRate :: InterestRate -> Double
unInterestRate (InterestRate rate) = rate

unPaymentNumber :: PaymentNumber -> Int
unPaymentNumber (PaymentNumber num) = num

unLoanTerms :: LoanTerms -> Int
unLoanTerms (LoanTerms terms) = terms

unTaxAmount :: TaxAmount -> Double
unTaxAmount (TaxAmount amount) = amount

unCommissionValue :: CommissionValue -> Double
unCommissionValue (CommissionValue value) = value

-- | Person type for tax calculations
data PersonType = Legal | Natural
  deriving (Eq, Show, Read, Generic)

instance FromJSON PersonType
instance ToJSON PersonType

-- | Commission type
data CommissionType = Absolute | Percentage
  deriving (Show, Generic)

instance FromJSON CommissionType
instance ToJSON CommissionType

-- | Commission structure
data Commission = Commission
  { commissionType  :: CommissionType
  , commissionValue :: CommissionValue
  } deriving (Show, Generic)

instance FromJSON Commission
instance ToJSON Commission

-- | Amortization methods
data AmortizationType = PRICE | SAC
  deriving (Enum, Show, Generic)

instance FromJSON AmortizationType
instance ToJSON AmortizationType

-- | Interest calculation modes for Brazilian market
data InterestMode =
    ThreeSixtyFiveMonth  -- ^ 365 days, monthly interest
  | ThreeSixtyFiveDay    -- ^ 365 days, daily interest  
  | ThreeSixtyMonth      -- ^ 360 days, monthly interest
  | ThreeSixtyDay        -- ^ 360 days, daily interest
  | TwoFiveTwoMonth      -- ^ 252 business days, monthly interest
  | TwoFiveTwoDay        -- ^ 252 business days, daily interest
  deriving (Show, Generic, Eq)

instance FromJSON InterestMode
instance ToJSON InterestMode

-- | Payment frequency
data PaymentFrequency =
    Monthly
  | Biweekly  
  | Weekly
  | Daily
  | Yearly
  deriving (Enum, Show, Generic, Eq)

instance FromJSON PaymentFrequency
instance ToJSON PaymentFrequency

-- | Interest calculation period
data InterestPeriod = Day | Month
  deriving (Eq, Show)