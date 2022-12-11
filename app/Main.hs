{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Main where

import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.Logger     (NoLoggingT (..), runStderrLoggingT)
import           Data.String.Conversions  (cs)
import           Database.Persist.Sqlite  (ConnectionPool, createSqlitePool,
                                           entityVal, insert, runMigration,
                                           runSqlPersistMPool, runSqlPool,
                                           selectFirst, (==.))
import           GHC.Generics
import           LoanApi                  (LoanAPI, loanServer)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger       (withStdoutLogger)
import           Servant
import           UserApi                  (UserAPI, migrateUsers, userServer)

type API = UserAPI :<|> LoanAPI

mainApi :: Proxy API
mainApi = Proxy

app :: ConnectionPool -> Application
app pool = serve mainApi $
        userServer pool
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
