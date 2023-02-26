{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module LoanApi
    ( LoanAPI
    , loanServer
    ) where

import           Data.Aeson     (FromJSON, ToJSON)
import           GHC.Generics   (Generic)
import           LoanCalculator (LoanQuery, LoanQueryResult, asLiquidValueLoan,
                                 queryLoan)
import           Servant

data LiquidResult = LiquidResult
    { query  :: LoanQuery
    , result :: LoanQueryResult
    }
        deriving (Show, Generic)
instance FromJSON LiquidResult
instance ToJSON LiquidResult
type LoanAPI  = "loan" :>
        (
                           ReqBody '[JSON] LoanQuery :> Post '[JSON] LoanQueryResult
        :<|> "liquid" :>   ReqBody '[JSON] LoanQuery :> Post '[JSON] LiquidResult
        )

handleLoanQuery :: LoanQuery -> LoanQueryResult
handleLoanQuery = queryLoan

handleLiquidLoanQuery :: LoanQuery -> LiquidResult
handleLiquidLoanQuery l =
    let query'  = asLiquidValueLoan l
        result' = queryLoan query'
    in  LiquidResult { query = query', result = result' }

loanServer :: Server LoanAPI
loanServer = loan :<|> loanLiquid
  where
    loan l = return (handleLoanQuery l)
    loanLiquid l = return (handleLiquidLoanQuery l)
