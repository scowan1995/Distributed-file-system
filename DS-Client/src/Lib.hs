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

data Filelocation = Filelocation
  {
      filename :: Text
    , cluster :: Text
  } deriving (Eq, Read, Show, Generic)




type FSApi = "filepush" :> ReqBody '[JSON] File :> Post '[JSON] (Maybe Bool)
  :<|> "filepull" :> QueryParam "filename" String :> Get '[JSON] (Maybe File)

type DSApi =
       "file" :> "add" :> Capture "name" Text :> Post '[JSON] (Maybe Cluster)
  :<|> "file" :> "get" :> Capture "name" Text  :> Get  '[JSON] (Maybe Filelocation)
  :<|> "addCluster" :> Capture "ip" Text :> Post '[JSON] (Maybe (Key Cluster))

apiFS :: Proxy FSApi
apiFS = Proxy

filepush :: File -> ClientM (Maybe Bool)

filepull :: Maybe String -> ClientM (Maybe File)

(filepush :<|> filepull) = client apiFS

queries :: ClientM (Maybe Bool, Maybe Bool, Maybe File)
queries = do
  a0 <- filepush(File "Contents1" "name1")
  a1 <- filepush(File "Contents2" "name2")
  a2 <- filepull(Just "name1")
  return (a0, a1, a2)

run :: IO ()
run = do
  manager <- newManager defaultManagerSettings
  res <-  runClientM queries (ClientEnv manager (BaseUrl Http "localhost" 3002 ""))
  case res of
    Left e -> putStrLn $ "Error: " ++ show e
    Right (b0, b1, f) -> do
      print b0
      print b1
      print f
