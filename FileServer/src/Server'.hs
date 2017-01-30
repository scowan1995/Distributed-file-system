{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Server' where


import Prelude ()
import Prelude.Compat
import Data.Aeson
import Data.Text
import Data.ByteString
import Database.Persist.TH
import GHC.Generics
import Data.Proxy
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client
import Database.Persist


data Server' = Server'
  {
       primaryIP :: Text
    ,  primaryPort :: Int
  } deriving (Eq, Read, Show, Generic)
derivePersistField "Server'"

instance FromJSON Server'
instance ToJSON Server'
