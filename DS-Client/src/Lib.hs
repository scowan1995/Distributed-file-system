{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lib where


import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client
import Data.Text
import Database.Persist

data File = File
  { datum        :: String
  , name         :: String
  } deriving (Eq, Read, Show, Generic)

instance FromJSON File
instance ToJSON File

data ReplicationServer = ReplicationServer
  {   primary :: Bool
    , ip :: Text

  } deriving (Eq, Read, Show, Generic)

instance FromJSON ReplicationServer
instance ToJSON ReplicationServer


data Cluster = Cluster
  {
      primaryIP :: Text
  } deriving (Eq, Read, Show, Generic)

instance FromJSON Cluster
instance ToJSON Cluster

data Filelocation = Filelocation
  {
      filename :: Text
    , cluster :: Text
  } deriving (Eq, Read, Show, Generic)

instance FromJSON Filelocation
instance ToJSON Filelocation




type FSApi = "filepush" :> ReqBody '[JSON] File :> Post '[JSON] (Maybe Bool)
  :<|> "filepull" :> QueryParam "filename" String :> Get '[JSON] (Maybe File)

type DSApi =
       "file" :> "add" :> Capture "name" Text :> Post '[JSON] (Maybe Cluster)
  :<|> "file" :> "get" :> Capture "name" Text  :> Get  '[JSON] (Maybe Filelocation)
  :<|> "addCluster" :> Capture "ip" Text :> Post '[JSON] (Maybe Bool)

apiFS :: Proxy FSApi
apiFS = Proxy

apiDS :: Proxy DSApi
apiDS = Proxy

fileadd :: Text -> ClientM (Maybe Cluster)

fileget :: Text -> ClientM (Maybe Filelocation)

addCluster :: Text -> ClientM (Maybe Bool)

filepush :: File -> ClientM (Maybe Bool)

filepull :: Maybe String -> ClientM (Maybe File)

(filepush :<|> filepull) = client apiFS

(fileadd :<|> fileget :<|> addCluster) = client apiDS

queriesFS :: ClientM (Maybe Bool, Maybe Bool, Maybe File)
queriesFS = do
  a0 <- filepush(File "Contents1" "name1")
  a1 <- filepush(File "Contents2" "name2")
  a2 <- filepull(Just "name1")
  return (a0, a1, a2)

queriesDS :: ClientM (Maybe Bool, Maybe Bool,Maybe Cluster, Maybe Cluster, Maybe Filelocation, Maybe Filelocation)
queriesDS = do
  ac0 <- addCluster "10.0.1.1"
  ac1 <- addCluster "google.ie"
  fa0 <- fileadd "myfile :)"
  fa1 <- fileadd "your file >:("
  fg0 <- fileget "myfile :)"
  fg1 <- fileget "your file >:("
  return (ac0, ac1, fa0, fa1, fg0, fg1)


runFS :: IO ()
runFS = do
  manager <- newManager defaultManagerSettings
  res <-  runClientM queriesFS (ClientEnv manager (BaseUrl Http "localhost" 3002 ""))
  case res of
    Left e -> putStrLn $ "Error: " ++ show e
    Right (b0, b1, f) -> do
      print b0
      print b1
      print f

runDS :: IO ()
runDS = do
  manager <- newManager defaultManagerSettings
  res <- runClientM queriesDS (ClientEnv manager (BaseUrl Http "localhost" 3003 ""))
  case res of
    Left e -> putStrLn $ "Error: " ++ show e
    Right (a, b, c, d, e, f) -> do
      print a
      print b
      print c
      print d
      print e
      print f
