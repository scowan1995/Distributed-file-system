{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Api where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Proxy
import           Data.Text
import           Database.Persist
import           GHC.Generics
import           Servant


type Api = "list" :> Get '[JSON] Text -- returns a list of the files and dirs in a dir
      :<|> "cd" :> Capture "dir" Text :> Get '[JSON] Text

api :: Proxy Api
api = Proxy
