{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module LoanApi
    ( LoanAPI
    , loanServer
    ) where

import           Data.Aeson     (FromJSON, ToJSON)
import           Debug.Trace
import           GHC.Generics   (Generic)
import           LoanCalculator (LoanQuery (loanAmount), LoanQueryResult,
                                 LoanQueryValidationError, asNetValueLoan,
                                 CurrencyAmount,
                                 validateAndCalculate)
import           Servant

data NetResult = NetResult
    { queryAmount  :: CurrencyAmount
    , result :: LoanQueryUnwrappedResult
    }
        deriving (Show, Generic)
instance FromJSON NetResult
instance ToJSON NetResult
type LoanAPI  = "loan" :>
        (
                           ReqBody '[JSON] LoanQuery :> Post '[JSON] LoanQueryUnwrappedResult
        :<|> "net" :>   ReqBody '[JSON] LoanQuery :> Post '[JSON] (Maybe NetResult)
        )

data LoanQueryUnwrappedResult = LoanQueryUnwrappedResult{
    response :: Maybe LoanQueryResult
    , errors :: Maybe [LoanQueryValidationError]
} deriving (Show, Generic)
instance FromJSON LoanQueryUnwrappedResult
instance ToJSON LoanQueryUnwrappedResult

handleLoanQuery :: LoanQuery -> LoanQueryUnwrappedResult
handleLoanQuery req = case validateAndCalculate req of
    Right res -> trace (show res) $ LoanQueryUnwrappedResult (Just res) Nothing
    Left errors' -> trace (show errors') $ LoanQueryUnwrappedResult Nothing (Just errors')

handleNetLoanQuery :: LoanQuery -> Maybe NetResult
handleNetLoanQuery l = case asNetValueLoan l of
    Just query' -> case handleLoanQuery query' of
        res -> Just (NetResult (loanAmount query') res)        
    Nothing -> Nothing

loanServer :: Server LoanAPI
loanServer = loan :<|> loanNet
  where
    loan l = return (handleLoanQuery l)
    loanNet l = return (handleNetLoanQuery l)
