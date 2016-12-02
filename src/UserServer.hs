{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module UserServer where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson.Compat
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html

type UserAPI1 = "users" :> Get '[JSON] [User]
--type AddUserAPI1 = "adduser" :> ReqBody '[JSON] User :> Post '[JSON] 

data User = User {
  username :: String,
  password :: String
} deriving (Eq, Show, Generic)

instance ToJSON User

users :: [User]
users =
  [ User "Admin" "password",
    User "John" "john",
    User "Mary" "mary"
  ]

server' :: Server UserAPI1
server' = return users

userAPI :: Proxy UserAPI1
userAPI = Proxy

app :: Application
app = serve userAPI server'

startServer' :: IO ()
startServer' = run 2000 app
