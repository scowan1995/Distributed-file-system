{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson.Compat
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Client
import System.Directory
import Data.Text
import qualified Data.Aeson.Parser
import Models
import Cluster
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API

-- filepush is where the body of the request is a json file with a file in it in a bytestring, it returns Just the name of the file that has been successfully uploaded or nothing.
type Api = "filepush" :> ReqBody '[JSON] File :> Post '[JSON] (Maybe Bool)
  :<|> "filepull" :> QueryParam "filename" String :> Get '[JSON] (Maybe File)

data Groups = Groups
  { primary :: Cluster
  , size :: Int
  } deriving (Eq, Read, Show, Generic)

instance FromJSON Groups
instance ToJSON Groups

data Filelocation = Filelocation
  {  filename :: Text
  ,  cluster :: Cluster
  ,  isLocked :: Bool

  } deriving (Eq, Read, Show, Generic)


instance FromJSON Filelocation
instance ToJSON Filelocation

type DSApi =
     "file" :> "add" :> Capture "name" Text :> Post '[JSON] (Maybe Cluster)
  :<|> "file" :> "get" :> Capture "name" Text  :> Get  '[JSON] (Maybe Filelocation)
  :<|> "addcluster" :> Capture "ip" Text :> Capture "port" Int :> Get '[JSON] Bool
  :<|> "makeMePrimary" :> Capture "oldip" Text :> Capture "oldport" Int :> Capture "newip" Text :> Capture "newport" Int :> Get '[JSON] ()
  :<|> "addMeToGroup" :> ReqBody '[JSON] Cluster :> Get '[JSON] Cluster
  :<|> "createGroup" :> ReqBody '[JSON] Cluster :> Get '[JSON] Bool


apiDS :: Proxy DSApi
apiDS = Proxy

fileadd :: Text -> ClientM (Maybe Cluster)

fileget :: Text -> ClientM (Maybe Filelocation)

addCluster :: Text -> Int -> ClientM Bool

makeMePrimary :: Text -> Int -> Text -> Int -> ClientM ()

addMeToGroup :: Cluster -> ClientM Cluster

createGroup :: Cluster -> ClientM Bool

(fileadd :<|> fileget :<|> addCluster :<|> makeMePrimary :<|> addMeToGroup :<|> createGroup) = client apiDS

beGroup :: Text -> Int -> IO ()
beGroup ip port = do
  manager <- newManager defaultManagerSettings
  clus <- runClientM (createGroup (Cluster ip port)) (ClientEnv manager (BaseUrl Http "localhost" 3003 ""))
  case clus of
    Left e -> putStrLn $ "Error adding cluster: \n" ++ show clus
    Right _ -> putStrLn $ "Group successfully created: \n" ++ show ip ++ " " ++ show port

joinAGroup :: Cluster -> IO ()
joinAGroup c = do
  manager <- newManager defaultManagerSettings
  clus <- runClientM (addMeToGroup c) (ClientEnv manager (BaseUrl Http "localhost" 3003 ""))
  case clus of
    Left e -> putStrLn $ "Error joining group: " ++ show e
    Right _ -> putStrLn $ "successfully joined: " ++ show clus

api :: Proxy Api
api = Proxy
