{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Api.LoanApi
  ( LoanAPI
  , loanServer
  , LoanCalculationResponse(..)
  , NetLoanResponse(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Servant

import Domain.Types (CurrencyAmount)
import Domain.Loan (LoanRequest, LoanValidationError, validateLoan)
import Domain.AmortizationSchedule (AmortizationResult, calculateAmortizationSchedule)
import Application.LoanService (calculateNetValueLoan, NetValueResult(..))

-- | API response for loan calculations
data LoanCalculationResponse = LoanCalculationResponse
  { response :: Maybe AmortizationResult
  , errors   :: Maybe [LoanValidationError]
  } deriving (Show, Generic)

instance FromJSON LoanCalculationResponse
instance ToJSON LoanCalculationResponse

-- | API response for net value loan calculations
data NetLoanResponse = NetLoanResponse
  { queryAmount :: CurrencyAmount
  , result      :: LoanCalculationResponse
  } deriving (Show, Generic)

instance FromJSON NetLoanResponse
instance ToJSON NetLoanResponse

-- | Loan calculation API endpoints
type LoanAPI = "loan" :>
  (         ReqBody '[JSON] LoanRequest :> Post '[JSON] LoanCalculationResponse
  :<|> "net" :> ReqBody '[JSON] LoanRequest :> Post '[JSON] (Maybe NetLoanResponse)
  )

-- | Handle standard loan calculation request
handleLoanCalculation :: LoanRequest -> LoanCalculationResponse
handleLoanCalculation request = 
  case validateLoan request of
    Right validLoan -> 
      let result = calculateAmortizationSchedule validLoan
      in LoanCalculationResponse (Just result) Nothing
    Left errors -> 
      LoanCalculationResponse Nothing (Just errors)

-- | Handle net value loan calculation request  
handleNetLoanCalculation :: LoanRequest -> Maybe NetLoanResponse
handleNetLoanCalculation request =
  case calculateNetValueLoan request of
    NetValueSuccess adjustedRequest result ->
      let response = handleLoanCalculation adjustedRequest
      in Just $ NetLoanResponse (lrAmount adjustedRequest) response
    NetValueFailure _ -> Nothing
  where
    -- Extract amount from LoanRequest (this is a simplified accessor)
    lrAmount req = CurrencyAmount 0 -- TODO: proper extraction

-- | Loan API server implementation
loanServer :: Server LoanAPI
loanServer = handleLoanCalculation :<|> handleNetLoanCalculation