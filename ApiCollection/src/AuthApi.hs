{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module AuthApi where

import Data.Aeson
import Data.Aeson.TH
import GHC.Generics
import Data.Proxy
import Data.Text
import Database.Persist
import Servant


data User = User {
	  username :: Text
	, password :: Text
} deriving (Eq, Read, Show)

instance FromJSON User where
  parseJSON = withObject "User" $ \ v ->
    User <$> v .: "name"
         <*> v .: "password"

instance ToJSON User where
  toJSON (User name password) =
    object [ "name" .= name
           , "password"  .= password  ]


type Api =
       "user" :> "add" :> ReqBody '[JSON] User :> Post '[JSON] (Maybe (Key
       User))
         :<|> "user" :> "get" :> Capture "name" Text  :> Get  '[JSON] (Maybe
         User)

-- api :: Proxy Api
-- api = Proxy
