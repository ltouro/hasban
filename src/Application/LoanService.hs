{-# LANGUAGE DeriveGeneric #-}

module Application.LoanService
  ( NetValueResult(..)
  , calculateNetValueLoan
  , calculateStandardLoan
  , LoanCalculationService(..)
  ) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

import Domain.Types (CurrencyAmount(..), unCurrencyAmount)
import Domain.Loan (LoanRequest(..), Loan, validateLoan)
import Domain.AmortizationSchedule (AmortizationResult, calculateAmortizationSchedule, calculateNetValue)
import Infrastructure.GoalSeeker (goalSeek, defaultGoalSeekConfig, GoalSeekResult(..), GoalSeekError)

-- | Result of net value loan calculation
data NetValueResult =
    NetValueSuccess LoanRequest AmortizationResult  -- ^ Adjusted loan request and result
  | NetValueFailure GoalSeekError                   -- ^ Goal seek failed
  deriving (Show, Generic)

instance FromJSON NetValueResult
instance ToJSON NetValueResult

-- | Loan calculation service interface
data LoanCalculationService = LoanCalculationService
  { calculateLoan :: LoanRequest -> Either [String] AmortizationResult
  , calculateNetLoan :: LoanRequest -> NetValueResult
  }

-- | Calculate standard loan amortization
calculateStandardLoan :: LoanRequest -> Either [String] AmortizationResult
calculateStandardLoan request = 
  case validateLoan request of
    Right validLoan -> Right $ calculateAmortizationSchedule validLoan
    Left errors -> Left $ map show errors

-- | Calculate loan amount that produces a specific net value
-- This uses goal seek to find the loan amount that, after all taxes and commissions,
-- provides a net value equal to the requested loan amount
calculateNetValueLoan :: LoanRequest -> NetValueResult
calculateNetValueLoan request = 
  let targetNetValue = lrAmount request
      adjustedRequestFunction amount = 
        let adjustedRequest = request { lrAmount = amount }
        in case validateLoan adjustedRequest of
          Right validLoan -> 
            let result = calculateAmortizationSchedule validLoan
                netValue = calculateNetValue validLoan (schedule result)
            in unCurrencyAmount netValue
          Left _ -> 0  -- Return 0 for invalid loans
      
      goalFunction amount = adjustedRequestFunction amount - targetNetValue
      
  in case goalSeek defaultGoalSeekConfig goalFunction 0 (targetNetValue * 1.5) of
    Right goalResult -> 
      let adjustedAmount = solution goalResult
          adjustedRequest = request { lrAmount = adjustedAmount }
      in case validateLoan adjustedRequest of
        Right validLoan -> 
          let result = calculateAmortizationSchedule validLoan
          in NetValueSuccess adjustedRequest result
        Left _ -> NetValueFailure (toGoalSeekError "Invalid adjusted loan")
    Left err -> NetValueFailure err

-- Helper to convert string errors to GoalSeekError
toGoalSeekError :: String -> GoalSeekError
toGoalSeekError msg = error ("Goal seek error: " ++ msg)  -- Simplified for now

-- | Create a default loan calculation service
defaultLoanService :: LoanCalculationService
defaultLoanService = LoanCalculationService
  { calculateLoan = calculateStandardLoan
  , calculateNetLoan = calculateNetValueLoan
  }