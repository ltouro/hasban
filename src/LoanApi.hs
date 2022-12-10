{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveGeneric     #-}

module LoanApi where

import           Data.Aeson
import           Data.Proxy
import           GHC.Generics
import           LoanCalculator                 ( LoanQuery
                                                , LoanQueryResult
                                                , asLiquidValueLoan
                                                , queryLoan
                                                )
import           Servant.API

data LiquidResult = LiquidResult
    { liquidResultQuery  :: LoanQuery
    , liquidResultResult :: LoanQueryResult
    }
    deriving (Show, Generic)
instance FromJSON LiquidResult
instance ToJSON LiquidResult

type LoanAPI  = "loan"   :> ReqBody '[JSON] LoanQuery :> Post '[JSON] LoanQueryResult
           :<|> "liquid" :> ReqBody '[JSON] LoanQuery :> Post '[JSON] LiquidResult

handleLoanQuery :: LoanQuery -> LoanQueryResult
handleLoanQuery = queryLoan

handleLiquidLoanQuery :: LoanQuery -> LiquidResult
handleLiquidLoanQuery l =
    let query  = asLiquidValueLoan l
        result = queryLoan query
    in  LiquidResult { liquidResultQuery = query, liquidResultResult = result }

loanApi :: Proxy LoanAPI
loanApi = Proxy
