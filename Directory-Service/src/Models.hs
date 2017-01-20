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
File1
  filename Text
  ip  Text
  deriving Eq Read Show
|]

instance FromJSON File1 where
  parseJSON = withObject "File1" $ \ v ->
    File1 <$> v .: "filename"
         <*> v .: "ip"

instance ToJSON File1 where
  toJSON (File1 filename ip) =
    object [ "filename" .= filename
            , "ip" .= ip ]
