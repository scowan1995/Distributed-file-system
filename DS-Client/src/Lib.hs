{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
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
    ,  primaryPort :: Int
  } deriving (Eq, Read, Show, Generic)

instance FromJSON Cluster
instance ToJSON Cluster

data Filelocation = Filelocation
  {
      filename :: Text
    , cluster :: Cluster
  } deriving (Eq, Read, Show, Generic)

instance FromJSON Filelocation
instance ToJSON Filelocation




type FSApi = "filepush" :> ReqBody '[JSON] File :> Post '[JSON] (Maybe Bool)
  :<|> "filepull" :> QueryParam "filename" String :> Get '[JSON] (Maybe File)


type DSApi = "file" :> "add" :> Capture "name" Text :> Post '[JSON] (Maybe Cluster)
    :<|> "file" :> "get" :> Capture "name" Text  :> Get  '[JSON] (Maybe Filelocation)
    :<|> "addcluster" :> Capture "ip" Text :> Capture "port" Int :> Get '[JSON] Bool

apiFS :: Proxy FSApi
apiFS = Proxy

apiDS :: Proxy DSApi
apiDS = Proxy

fileadd :: Text -> ClientM (Maybe Cluster)

fileget :: Text -> ClientM (Maybe Filelocation)

addCluster :: Text -> Int -> ClientM Bool

filepush :: File -> ClientM (Maybe Bool)

filepull :: Maybe String -> ClientM (Maybe File)

(filepush :<|> filepull) = client apiFS

(fileadd :<|> fileget :<|> addCluster) = client apiDS

queriesFS :: ClientM (Maybe Bool, Maybe Bool, Maybe File)
queriesFS = do
  a0 <- filepush (File "Contents1" "name1")
  a1 <- filepush (File "Contents2" "name2")
  a2 <- filepull (Just "name1")
  return (a0, a1, a2)

queriesDS :: ClientM (Bool, Bool, Maybe Cluster, Maybe Cluster, Maybe Filelocation, Maybe Filelocation)
queriesDS = do
  ac0 <- addCluster "10.0.1.1" 3002
  ac1 <- addCluster "google.ie" 3002
  fa0 <- fileadd "myfile :)"
  fa1 <- fileadd "your file >:("
  fg0 <- fileget "myfile :)"
  fg1 <- fileget "your file >:("
  return (ac0, ac1, fa0, fa1, fg0, fg1)

-- uploadFile
-- downloadFile
-- reuploadFile

type FileName = String

--downloads a file to a filepath and locks file
downloadFile :: FileName -> FilePath -> IO ()
downloadFile fname fp = do
  manager <- newManager defaultManagerSettings
  fileloc <- runClientM (fileget fname) (ClientEnv manager (BaseUrl Http "localhost" 3003 ""))
  case fileloc of
    Nothing -> putStrLn "file does not exist. Did you spell the name correctly?"
    Just (Filelocation name (Cluster ip port)) -> do
      file <- pullfile -- finsih off

uf :: File -> IO ()
uf f@(File datum name) = do
  manager <- newManager defaultManagerSettings
  cluster <- runClientM (fileadd (pack name)) (ClientEnv manager (BaseUrl Http "localhost" 3003 ""))
  case cluster of
    Left e -> putStrLn $ "Error in uf: " ++ show e
    Right (Just c) -> do
      q <- pushfile c f
      if q then putStrLn $ show f ++ " pushed successfuly"
        else putStrLn "push unsuccessful, try again later"


-- This takes a Cluster and a file and uploads the file to that cluster
pushfile :: Cluster -> File -> IO Bool
pushfile (Cluster ip port) f = do
  manager <- newManager defaultManagerSettings
  push_res <- runClientM (filepush f) (ClientEnv manager (BaseUrl Http (unpack ip) port ""))
  case push_res of
    Left e -> do
      putStrLn $ "Error in pushfile: " ++ show e
      return False
    Right (Just b) -> return b

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
