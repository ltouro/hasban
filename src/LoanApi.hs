{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module LoanApi
    ( LoanAPI
    , loanServer
    ) where

import           Data.Aeson     (FromJSON, ToJSON)
import           GHC.Generics   (Generic)
import           LoanCalculator (LoanQuery, LoanQueryResult, asNetValueLoan,
                                 queryLoan)
import           Servant

data NetResult = NetResult
    { query  :: LoanQuery
    , result :: LoanQueryResult
    }
        deriving (Show, Generic)
instance FromJSON NetResult
instance ToJSON NetResult
type LoanAPI  = "loan" :>
        (
                           ReqBody '[JSON] LoanQuery :> Post '[JSON] LoanQueryResult
        :<|> "net" :>   ReqBody '[JSON] LoanQuery :> Post '[JSON] (Maybe NetResult)
        )

handleLoanQuery :: LoanQuery -> LoanQueryResult
handleLoanQuery = queryLoan

handleNetLoanQuery :: LoanQuery -> Maybe NetResult
handleNetLoanQuery l = case asNetValueLoan l of
    Just query' -> Just NetResult { query = query', result = queryLoan query' }
    Nothing -> Nothing
loanServer :: Server LoanAPI
loanServer = loan :<|> loanNet
  where
    loan l = return (handleLoanQuery l)
    loanNet l = return (handleNetLoanQuery l)
