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
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module UserApi (
    UserAPI,
    userServer,
    migrateUsers
  )where

import           Control.Monad.IO.Class         (liftIO)
import           Data.Text
import           Servant
import           Database.Persist               (Key)
import           Database.Persist.Sql.Migration (Migration)
import           Database.Persist.Sqlite        (ConnectionPool
                                                , runSqlPersistMPool                                            
                                                , insert
                                                , selectFirst
                                                , entityVal
                                                , (==.))

import           Database.Persist.TH            (mkMigrate, mkPersist
                                                , persistLowerCase, share
                                                , sqlSettings)
import           GHC.Generics

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
    name     Text
    age      Int
    deriving Eq Show Generic
|]

migrateUsers :: Migration
migrateUsers = migrateAll
type UserAPI  = "user" :> 
          (
                Capture "name" Text :> Get '[JSON] (Maybe User)
          :<|>  ReqBody '[JSON] User :> Post '[JSON] (Maybe (Key User))
          )
          
userServer :: ConnectionPool -> Server UserAPI
userServer pool = usersGet :<|> usersPost
    where
        usersGet  n  = liftIO $ userGet pool n        
        usersPost u =  liftIO $ userAdd pool u
        
userGet :: ConnectionPool -> Text -> IO (Maybe User)
userGet pool name = flip runSqlPersistMPool pool $ do
  mUser <- selectFirst [UserName ==. name] []
  return $ entityVal <$> mUser

userAdd :: ConnectionPool -> User -> IO (Maybe (Key User))
userAdd pool newUser = flip runSqlPersistMPool pool $ do
    created <- selectFirst [UserName ==. (userName newUser)] []
    case created of
      Nothing -> Just <$> insert newUser
      Just _ -> return Nothing