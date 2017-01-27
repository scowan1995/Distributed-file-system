{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric              #-}

module Models where

import Prelude ()
import Prelude.Compat

import Data.Aeson
import Data.Text
import Data.ByteString
import Cluster
import Database.Persist.TH
import GHC.Generics


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
File
  datum String
  name String
  deriving Eq Read Show
ReplicationServer
  primary Cluster
  group Cluster
  deriving Eq Read Show
|]
 -- primry = main server holding rpimary copies
 -- group = The servers in that group holding backups
 -- Cluster bad name, basically just the details of a server (port, ip)

instance FromJSON ReplicationServer where
  parseJSON = withObject "ReplicationServer" $ \ v ->
    ReplicationServer <$> v .: "primary"
         <*> v .: "group"

instance ToJSON ReplicationServer where
  toJSON (ReplicationServer primary group) =
    object [ "primary" .= primary
            , "group" .= group ]


instance FromJSON File where
  parseJSON = withObject "File" $ \ v ->
    File <$> v .: "datum"
         <*> v .: "name"

instance ToJSON File where
  toJSON (File datum name) =
    object [ "datum" .= datum
    , "name" .= name ]
