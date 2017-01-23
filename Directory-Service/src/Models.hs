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
Filelocation
  filename Text
  ip  Text
  deriving Eq Read Show
|]

instance FromJSON Filelocation where
  parseJSON = withObject "Filelocation" $ \ v ->
    Filelocation <$> v .: "filename"
         <*> v .: "ip"

instance ToJSON Filelocation where
  toJSON (Filelocation filename ip) =
    object [ "filename" .= filename
            , "ip" .= ip ]
