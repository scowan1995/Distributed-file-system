{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import Data.Aeson
import Data.Text

import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Cluster
  primaryIP Text
  primaryPort Int
  deriving Eq Read Show
Groups
  primary Cluster
  size Int
Filelocation
  filename Text
  cluster  Cluster
  isLocked Bool
  deriving Eq Read Show
|]

instance FromJSON Filelocation where
  parseJSON = withObject "Filelocation" $ \ v ->
    Filelocation <$> v .: "filename"
         <*> v .: "cluster"
         <*> v .: "isLocked"

instance ToJSON Filelocation where
  toJSON (Filelocation filename cluster isLocked) =
    object [ "filename" .= filename
            , "cluster" .= cluster
            , "isLocked" .= isLocked ]

instance FromJSON Cluster where
    parseJSON = withObject "Cluster" $ \ v ->
      Cluster <$> v .: "primaryIP"
        <*> v .: "primaryPort"


instance ToJSON Cluster where
  toJSON (Cluster primaryIP primaryPort) =
      object [ "primaryIP" .= primaryIP
              , "primaryPort" .= primaryPort]
