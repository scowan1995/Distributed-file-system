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

module Cluster where


import Prelude ()
import Prelude.Compat
import Data.Aeson
import Data.Text
import Data.ByteString
import Database.Persist.TH
import GHC.Generics


data Cluster = Cluster
  {
       primaryIP :: Text
    ,  primaryPort :: Int
  } deriving (Eq, Read, Show, Generic)
derivePersistField "Cluster"

instance FromJSON Cluster
instance ToJSON Cluster
