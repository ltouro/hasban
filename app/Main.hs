{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where 
	
import Servant ( PlainText
               , Get
               , ServerT
               , hoistServer -- Servant function to make a custom monad with with Servant.
               , Proxy(..)
               , type (:>)      -- Syntax for importing type operator
               , type (:<|>)
               , (:<|>)(..)
               )
import Servant.Server (Handler, Application, serve)
import Network.Wai.Handler.Warp (run)
import Control.Monad.Reader (runReader, Reader)

-- Define the API's type by specifying the endpoint and its return type
type HelloWorldAPI = "hello" :> Get '[PlainText] Text

-- Define the handler function that will be called when the endpoint is called
helloWorldHandler :: Server HelloWorldAPI
helloWorldHandler = return "Hello, World!"

-- Define the main function that runs the API server
main :: IO ()
main = run 8080 $ serve (Proxy :: Proxy HelloWorldAPI) helloWorldHandler