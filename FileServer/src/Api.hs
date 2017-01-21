{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Api where

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
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Data.Text
import qualified Data.Aeson.Parser
import Models
import Servant.API

-- filepush is where the body of the request is a json file with a file in it in a bytestring, it returns Just the name of the file that has been successfully uploaded or nothing.
type Api = "filepush" :> ReqBody '[JSON] File :> Get '[JSON] (Maybe (Key File))
 -- :<|> "filepull" :> Capture "filename" Text :> Get '[JSON] (Maybe (Key File))


api :: Proxy Api
api = Proxy
