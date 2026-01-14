{-# LANGUAGE DeriveGeneric #-}

module Hasban.Validation (
  -- * Validation Results
  ValidationError(..)
  , ValidationResult

  -- * Loan Validation
  , validatePersonalLoan
  , validateCreditCard

  -- * Field Validators
  , validatePrincipal
  , validateRate
  , validateTerms
  , validateBalance
) where

import Hasban.Types
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

-- | Validation errors
data ValidationError
  = InvalidPrincipal String
  | InvalidRate String
  | InvalidTerms String
  | InvalidBalance String
  | InvalidLimit String
  | InvalidMinPayment String
  | InvalidFrequency String
  deriving (Show, Eq, Generic)

instance FromJSON ValidationError
instance ToJSON ValidationError

-- | Validation result type
type ValidationResult a = Either [ValidationError] a

-- | Validate a personal loan
validatePersonalLoan :: PersonalLoan -> ValidationResult PersonalLoan
validatePersonalLoan loan =
  let errors = concat
        [ validatePrincipalField (plPrincipal loan)
        , validateRateField (plAnnualRate loan)
        , validateTermsField (plTerms loan)
        ]
  in if null errors
     then Right loan
     else Left errors

-- | Validate a credit card
validateCreditCard :: CreditCard -> ValidationResult CreditCard
validateCreditCard card =
  let errors = concat
        [ validateLimitField (ccLimit card)
        , validateRateField (ccAnnualRate card)
        , validateMinPaymentField (ccMinPayment card)
        ]
  in if null errors
     then Right card
     else Left errors

-- ============================================
-- FIELD VALIDATORS
-- ============================================

-- | Validate principal amount
validatePrincipal :: Double -> ValidationResult Money
validatePrincipal amount
  | amount <= 0 = Left [InvalidPrincipal "Principal must be positive"]
  | amount > 1e12 = Left [InvalidPrincipal "Principal exceeds maximum (1 trillion)"]
  | isNaN amount = Left [InvalidPrincipal "Principal cannot be NaN"]
  | isInfinite amount = Left [InvalidPrincipal "Principal cannot be infinite"]
  | otherwise = Right (Money amount)

-- | Validate interest rate (as decimal)
validateRate :: Double -> ValidationResult Rate
validateRate rate
  | rate < 0 = Left [InvalidRate "Rate cannot be negative"]
  | rate > 10 = Left [InvalidRate "Rate exceeds 1000% (sanity check)"]
  | isNaN rate = Left [InvalidRate "Rate cannot be NaN"]
  | isInfinite rate = Left [InvalidRate "Rate cannot be infinite"]
  | otherwise = Right (Rate rate)

-- | Validate number of terms
validateTerms :: Int -> ValidationResult Int
validateTerms terms
  | terms <= 0 = Left [InvalidTerms "Terms must be positive"]
  | terms > 600 = Left [InvalidTerms "Terms exceed maximum (50 years monthly)"]
  | otherwise = Right terms

-- | Validate balance amount
validateBalance :: Double -> ValidationResult Money
validateBalance balance
  | balance < 0 = Left [InvalidBalance "Balance cannot be negative"]
  | isNaN balance = Left [InvalidBalance "Balance cannot be NaN"]
  | isInfinite balance = Left [InvalidBalance "Balance cannot be infinite"]
  | otherwise = Right (Money balance)

-- ============================================
-- INTERNAL VALIDATORS
-- ============================================

validatePrincipalField :: Money -> [ValidationError]
validatePrincipalField (Money amount)
  | amount <= 0 = [InvalidPrincipal "Principal must be positive"]
  | amount > 1e12 = [InvalidPrincipal "Principal exceeds maximum"]
  | isNaN amount = [InvalidPrincipal "Principal cannot be NaN"]
  | isInfinite amount = [InvalidPrincipal "Principal cannot be infinite"]
  | otherwise = []

validateRateField :: Rate -> [ValidationError]
validateRateField (Rate rate)
  | rate < 0 = [InvalidRate "Rate cannot be negative"]
  | rate > 10 = [InvalidRate "Rate exceeds 1000%"]
  | isNaN rate = [InvalidRate "Rate cannot be NaN"]
  | isInfinite rate = [InvalidRate "Rate cannot be infinite"]
  | otherwise = []

validateTermsField :: Int -> [ValidationError]
validateTermsField terms
  | terms <= 0 = [InvalidTerms "Terms must be positive"]
  | terms > 600 = [InvalidTerms "Terms exceed maximum"]
  | otherwise = []

validateLimitField :: Money -> [ValidationError]
validateLimitField (Money limit)
  | limit <= 0 = [InvalidLimit "Credit limit must be positive"]
  | limit > 1e9 = [InvalidLimit "Credit limit exceeds maximum"]
  | isNaN limit = [InvalidLimit "Credit limit cannot be NaN"]
  | isInfinite limit = [InvalidLimit "Credit limit cannot be infinite"]
  | otherwise = []

validateMinPaymentField :: MinPayment -> [ValidationError]
validateMinPaymentField minPmt =
  case minPmt of
    FixedAmount (Money amt)
      | amt <= 0 -> [InvalidMinPayment "Fixed minimum must be positive"]
      | otherwise -> []
    PercentOfBalance (Rate pct)
      | pct <= 0 -> [InvalidMinPayment "Percent must be positive"]
      | pct > 1 -> [InvalidMinPayment "Percent cannot exceed 100%"]
      | otherwise -> []
    GreaterOf (Money fixed) (Rate pct)
      | fixed <= 0 -> [InvalidMinPayment "Fixed amount must be positive"]
      | pct <= 0 -> [InvalidMinPayment "Percent must be positive"]
      | pct > 1 -> [InvalidMinPayment "Percent cannot exceed 100%"]
      | otherwise -> []
