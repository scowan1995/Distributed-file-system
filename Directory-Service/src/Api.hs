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
  :<|> "addcluster" :> Capture "ip" Text :> Capture "port" Int :> Get '[JSON] Bool
  :<|> "makeMePrimary" :> Capture "oldip" Text :> Capture "oldport" Int :> Capture "newip" Text :> Capture "newport" Int :> Get '[JSON] ()
  :<|> "addMeToGroup" :> ReqBody '[JSON] Cluster :> Get '[JSON] Cluster
  :<|> "createGroup" :> ReqBody '[JSON] Cluster :> Get '[JSON] (Key Groups)


api :: Proxy Api
api = Proxy
