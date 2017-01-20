{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api where

import Data.Proxy
import Data.Text

import Database.Persist

import Models

import Servant.API



type Api =
       "file" :> "add" :> ReqBody '[JSON] File1 :> Post '[JSON] (Maybe (Key File1))
  :<|> "file" :> "get" :> Capture "name" Text  :> Get  '[JSON] (Maybe File1)

api :: Proxy Api
api = Proxy
