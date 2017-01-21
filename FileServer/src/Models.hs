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

import Database.Persist.TH
import GHC.Generics

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
File
  datum String
  name String
  deriving Eq Read Show
ReplicationServer
  primary Bool
  ip  Text
  deriving Eq Read Show
|]

instance FromJSON ReplicationServer where
  parseJSON = withObject "ReplicationServer" $ \ v ->
    ReplicationServer <$> v .: "primary"
         <*> v .: "ip"

instance ToJSON ReplicationServer where
  toJSON (ReplicationServer primary ip) =
    object [ "primary" .= primary
            , "ip" .= ip ]


instance FromJSON File where
  parseJSON = withObject "File" $ \ v ->
    File <$> v .: "datum"
         <*> v .: "name"

instance ToJSON File where
  toJSON (File datum name) =
    object [ "datum" .= datum
    , "name" .= name ]
