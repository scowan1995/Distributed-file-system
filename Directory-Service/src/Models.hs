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
Server'
  primaryIP Text
  primaryPort Int
  deriving Eq Read Show
Groups
  primary Server'
  size Int
Filelocation
  filename Text
  server'  Server'
  isLocked Bool
  deriving Eq Read Show
|]

instance FromJSON Filelocation where
  parseJSON = withObject "Filelocation" $ \ v ->
    Filelocation <$> v .: "filename"
         <*> v .: "server'"
         <*> v .: "isLocked"

instance ToJSON Filelocation where
  toJSON (Filelocation filename server' isLocked) =
    object [ "filename" .= filename
            , "server'" .= server'
            , "isLocked" .= isLocked ]

instance FromJSON Server' where
    parseJSON = withObject "Server'" $ \ v ->
      Server' <$> v .: "primaryIP"
        <*> v .: "primaryPort"


instance ToJSON Server' where
  toJSON (Server' primaryIP primaryPort) =
      object [ "primaryIP" .= primaryIP
              , "primaryPort" .= primaryPort]
