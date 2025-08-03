{-# LANGUAGE DeriveGeneric #-}

module Application.Errors
  ( LoanCalculationError(..)
  , ValidationError(..)
  , CalculationError(..)
  , BusinessRuleError(..)
  , SystemError(..)
  , ErrorContext(..)
  , mkErrorWithContext
  , formatError
  , isValidationError
  , isCalculationError
  , isBusinessRuleError
  , isSystemError
  ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Time (UTCTime)

import Domain.Types (CurrencyAmount, InterestRate, PaymentNumber)
import Domain.Loan (LoanValidationError)

-- | Top-level error type that encompasses all possible failures
data LoanCalculationError =
    ValidationErr ValidationError
  | CalculationErr CalculationError
  | BusinessRuleErr BusinessRuleError
  | SystemErr SystemError
  deriving (Eq, Show, Generic)

instance FromJSON LoanCalculationError
instance ToJSON LoanCalculationError

-- | Validation errors for input data
data ValidationError =
    InvalidLoanData [LoanValidationError]
  | MissingRequiredField String
  | InvalidDateFormat String String  -- field name, invalid value
  | InvalidNumericValue String Double String  -- field name, value, reason
  | InvalidEnumValue String String [String]  -- field name, invalid value, valid options
  deriving (Eq, Show, Generic)

instance FromJSON ValidationError
instance ToJSON ValidationError

-- | Calculation errors during mathematical operations
data CalculationError =
    InterestCalculationFailed String  -- reason
  | AmortizationCalculationFailed PaymentNumber String  -- payment number, reason
  | TaxCalculationFailed PaymentNumber String  -- payment number, reason
  | IRRCalculationFailed String  -- reason
  | GoalSeekFailure String Int  -- reason, iterations attempted
  | NumericalInstability String Double  -- operation, problematic value
  | DivisionByZero String  -- operation context
  deriving (Eq, Show, Generic)

instance FromJSON CalculationError
instance ToJSON CalculationError

-- | Business rule violations
data BusinessRuleError =
    InsufficientLoanAmount CurrencyAmount CurrencyAmount  -- requested, minimum
  | ExcessiveInterestRate InterestRate InterestRate  -- requested, maximum
  | IncompatibleAmortizationSettings String  -- description
  | GracePeriodViolation PaymentNumber PaymentNumber  -- requested, maximum allowed
  | CommissionLimitExceeded CurrencyAmount CurrencyAmount  -- total commissions, loan amount
  | PaymentScheduleConflict String  -- description
  | BrazilianRegulationViolation String String  -- regulation name, violation description
  deriving (Eq, Show, Generic)

instance FromJSON BusinessRuleError
instance ToJSON BusinessRuleError

-- | System-level errors
data SystemError =
    InternalCalculationError String  -- error description
  | ConfigurationError String  -- configuration issue
  | ExternalServiceError String String  -- service name, error
  | DataCorruption String  -- corruption description
  | ResourceExhaustion String  -- resource type
  deriving (Eq, Show, Generic)

instance FromJSON SystemError
instance ToJSON SystemError

-- | Error context for debugging and logging
data ErrorContext = ErrorContext
  { errorTimestamp :: Maybe UTCTime
  , errorLocation  :: String  -- module.function
  , errorInput     :: Maybe String  -- serialized input that caused error
  , errorTrace     :: [String]  -- call stack or operation trace
  } deriving (Eq, Show, Generic)

instance FromJSON ErrorContext
instance ToJSON ErrorContext

-- | Create an error with context information
mkErrorWithContext :: String -> [String] -> LoanCalculationError -> (LoanCalculationError, ErrorContext)
mkErrorWithContext location trace err =
  let context = ErrorContext
        { errorTimestamp = Nothing  -- Would be set by caller with current time
        , errorLocation = location
        , errorInput = Nothing  -- Would be set by caller if available
        , errorTrace = trace
        }
  in (err, context)

-- | Format error for user-friendly display
formatError :: LoanCalculationError -> String
formatError err = case err of
  ValidationErr validationErr -> formatValidationError validationErr
  CalculationErr calcErr -> formatCalculationError calcErr
  BusinessRuleErr businessErr -> formatBusinessRuleError businessErr
  SystemErr systemErr -> formatSystemError systemErr

formatValidationError :: ValidationError -> String
formatValidationError err = case err of
  InvalidLoanData errors ->
    "Invalid loan data: " ++ unlines (map show errors)
  MissingRequiredField field ->
    "Missing required field: " ++ field
  InvalidDateFormat field value ->
    "Invalid date format in field '" ++ field ++ "': " ++ value ++ ". Expected YYYY-MM-DD format."
  InvalidNumericValue field value reason ->
    "Invalid numeric value for '" ++ field ++ "' (" ++ show value ++ "): " ++ reason
  InvalidEnumValue field value validOptions ->
    "Invalid value '" ++ value ++ "' for field '" ++ field ++ "'. Valid options: " ++ show validOptions

formatCalculationError :: CalculationError -> String
formatCalculationError err = case err of
  InterestCalculationFailed reason ->
    "Interest calculation failed: " ++ reason
  AmortizationCalculationFailed paymentNo reason ->
    "Amortization calculation failed for payment " ++ show (unPaymentNumber paymentNo) ++ ": " ++ reason
  TaxCalculationFailed paymentNo reason ->
    "Tax calculation failed for payment " ++ show (unPaymentNumber paymentNo) ++ ": " ++ reason
  IRRCalculationFailed reason ->
    "Internal Rate of Return calculation failed: " ++ reason
  GoalSeekFailure reason iterations ->
    "Goal seek failed after " ++ show iterations ++ " iterations: " ++ reason
  NumericalInstability operation value ->
    "Numerical instability in " ++ operation ++ " with value " ++ show value
  DivisionByZero operation ->
    "Division by zero in " ++ operation

formatBusinessRuleError :: BusinessRuleError -> String
formatBusinessRuleError err = case err of
  InsufficientLoanAmount requested minimum ->
    "Loan amount " ++ show (unCurrencyAmount requested) ++ " is below minimum " ++ show (unCurrencyAmount minimum)
  ExcessiveInterestRate requested maximum ->
    "Interest rate " ++ show (unInterestRate requested) ++ "% exceeds maximum " ++ show (unInterestRate maximum) ++ "%"
  IncompatibleAmortizationSettings description ->
    "Incompatible amortization settings: " ++ description
  GracePeriodViolation requested maximum ->
    "Grace period " ++ show (unPaymentNumber requested) ++ " exceeds maximum " ++ show (unPaymentNumber maximum)
  CommissionLimitExceeded totalCommissions loanAmount ->
    "Total commissions " ++ show (unCurrencyAmount totalCommissions) ++ 
    " exceed loan amount " ++ show (unCurrencyAmount loanAmount)
  PaymentScheduleConflict description ->
    "Payment schedule conflict: " ++ description
  BrazilianRegulationViolation regulation violation ->
    "Brazilian regulation violation (" ++ regulation ++ "): " ++ violation

formatSystemError :: SystemError -> String
formatSystemError err = case err of
  InternalCalculationError description ->
    "Internal calculation error: " ++ description
  ConfigurationError issue ->
    "Configuration error: " ++ issue
  ExternalServiceError service error ->
    "External service error (" ++ service ++ "): " ++ error
  DataCorruption description ->
    "Data corruption detected: " ++ description
  ResourceExhaustion resource ->
    "Resource exhaustion: " ++ resource

-- | Type predicates for error handling
isValidationError :: LoanCalculationError -> Bool
isValidationError (ValidationErr _) = True
isValidationError _ = False

isCalculationError :: LoanCalculationError -> Bool
isCalculationError (CalculationErr _) = True
isCalculationError _ = False

isBusinessRuleError :: LoanCalculationError -> Bool
isBusinessRuleError (BusinessRuleErr _) = True
isBusinessRuleError _ = False

isSystemError :: LoanCalculationError -> Bool
isSystemError (SystemErr _) = True
isSystemError _ = False