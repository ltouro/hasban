{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Main where

-- import           HelloApi
import           LoanApi
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger       (withStdoutLogger)
import           Servant

type API = LoanAPI -- :<|> HelloAPI 

mainApi :: Proxy API
mainApi = Proxy

server :: Server API
server = loan :<|> liquid -- :<|> hello :<|> user
    where
        -- hello = return "Hello world"
        -- user n a = return $ User n a
        loan l = return (handleLoanQuery l)
        liquid l = return (handleLiquidLoanQuery l)

app :: Application
app = serve mainApi server

main :: IO ()
main = do
    withStdoutLogger $ \aplogger -> do
        let settings = setPort 8080 $ setLogger aplogger defaultSettings
        runSettings settings app