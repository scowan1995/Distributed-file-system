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
       "file" :> "add" :> Capture "name" Text :> Post '[JSON] (Maybe Cluster)
  :<|> "file" :> "get" :> Capture "name" Text  :> Get  '[JSON] (Maybe Filelocation)
  :<|> "addCluster" :> Capture "ip" Text :> Post '[JSON] (Maybe (Key Cluster))

api :: Proxy Api
api = Proxy
