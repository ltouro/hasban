{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Logger         (NoLoggingT (..), runStderrLoggingT)
import           LoanApi                      (LoanAPI, loanServer)
import           UserApi                      (UserAPI, userServer, migrateUsers)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger           (withStdoutLogger)
import           Servant
import           Database.Persist.Sqlite      ( ConnectionPool, createSqlitePool
                                            , runSqlPool, runSqlPersistMPool
                                            , runMigration, selectFirst, (==.)
                                            , insert, entityVal)
import           GHC.Generics
import           Data.String.Conversions  (cs)

type API = UserAPI :<|> LoanAPI

mainApi :: Proxy API
mainApi = Proxy

app :: ConnectionPool -> Application
app pool = serve mainApi $ 
        (userServer pool)
  :<|>  loanServer

mkApp :: FilePath -> IO Application
mkApp sqliteFile = do
  pool <- runStderrLoggingT $ do
    createSqlitePool (cs sqliteFile) 5
  runSqlPool (runMigration migrateUsers) pool
  _ <- putStrLn "Users migrated"
  return $ app pool
  
main :: IO ()
main = do
    _ <- putStrLn "Hasban started on port 8080"
    withStdoutLogger $ \aplogger -> do
        let settings = setPort 8080 $ setLogger aplogger defaultSettings        
        runSettings settings =<< mkApp "sqlite.db"    