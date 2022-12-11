{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TypeOperators     #-}
module HelloApi where

import Data.Aeson ( ToJSON, FromJSON )
import Data.Proxy ( Proxy(..) )
import           Data.Text                  (Text)
import GHC.Generics ( Generic )
import Servant.API
    ( type (:>), JSON, type (:<|>), Capture, PlainText, Get )

data User = User
    { name :: Text
    , age  :: Int
    } deriving (Eq, Show, Read, Generic)
instance FromJSON User
instance ToJSON User

type HelloAPI  = Get '[PlainText] Text
            :<|> "user" :> Capture "name" Text :> Capture "age" Int :> Get '[JSON] User

helloApi :: Proxy HelloAPI
helloApi = Proxy