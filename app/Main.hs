{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Main where

import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.Logger     (NoLoggingT (..), runStderrLoggingT)
import           GHC.Generics
import           LoanApi                  (LoanAPI, loanServer)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger       (withStdoutLogger)
import           Servant

type API = LoanAPI

mainApi :: Proxy API
mainApi = Proxy

app :: Application
app = serve mainApi loanServer


main :: IO ()
main = do
    _ <- putStrLn "LoanCalculator API started on port 8081"
    withStdoutLogger $ \aplogger -> do
        let settings = setPort 8081 $ setLogger aplogger defaultSettings
        runSettings settings app