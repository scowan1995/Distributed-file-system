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
  deriving Eq Read Show
Filelocation
  filename Text
  cluster  Text
  deriving Eq Read Show
|]

instance FromJSON Filelocation where
  parseJSON = withObject "Filelocation" $ \ v ->
    Filelocation <$> v .: "filename"
         <*> v .: "cluster"

instance ToJSON Filelocation where
  toJSON (Filelocation filename cluster) =
    object [ "filename" .= filename
            , "cluster" .= cluster ]

instance FromJSON Cluster where
    parseJSON = withObject "Cluster" $ \ v ->
      Cluster <$> v .: "primaryIP"


instance ToJSON Cluster where
  toJSON (Cluster primaryIP) =
      object [ "primaryIP" .= primaryIP]
