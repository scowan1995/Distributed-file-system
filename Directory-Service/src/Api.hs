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



type DSApi =
       "file" :> "add" :> Capture "name" Text :> Post '[JSON] (Maybe Server')
  :<|> "file" :> "get" :> Capture "name" Text  :> Get  '[JSON] (Maybe Filelocation)
  :<|> "makeMePrimary" :> Capture "oldip" Text :> Capture "oldport" Int :> Capture "newip" Text :> Capture "newport" Int :> Get '[JSON] ()
  :<|> "addMeToGroup" :> ReqBody '[JSON] Server' :> Get '[JSON] Server'
  :<|> "createGroup" :> ReqBody '[JSON] Server' :> Get '[JSON] Bool

--  :<|> "addcluster" :> Capture "ip" Text :> Capture "port" Int :> Get '[JSON] Bool

api :: Proxy DSApi
api = Proxy
