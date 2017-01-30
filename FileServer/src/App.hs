{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module App where

import           Control.Monad.IO.Class
import           Control.Monad.Logger (runStderrLoggingT)
import Control.Exception

import           Data.String.Conversions

import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite

import           Network.Wai
import           Network.Wai.Handler.Warp as Warp

import           Servant
import System.IO
-- import Server'
import Servant.Client
import Network.HTTP.Client (newManager, defaultManagerSettings)
import           Data.Text

import           Api
import           Models

notifyDS :: Text -> Int -> IO Bool
notifyDS port ip = do
  manager <- newManager defaultManagerSettings
  x <- runClientM ( createGroup (Server' (pack ip) port)) (ClientEnv manager (BaseUrl Http "localhost" 3003 ""))
  case x of
    Left e -> do
      putStrLn $ "Error in notifyDs in Fileserver: " ++ show e
      return False
    Right x -> return True

askToJoin :: Server' -> Text -> Int -> IO ( Maybe [Server'])
askToJoin s@(Server' sip sport) ip port = do
  manager <- newManager defaultManagerSettings
  x <- runClientM (letmejoin sip sport) (ClientEnv manager (BaseUrl Http ip port))
  case x of
    Left e -> do
      putStrLn $ "Error in ask to join in fileserver: " ++ show e
      return Nothing
    Right s -> return $ Just s

server :: ConnectionPool -> Server FSApi
server pool =
  filePushH :<|> filePullH :<|> beGroupH :<|>joinAGroupH :<|> addToGroupH
  where
    filePullH fname = liftIO $ filePull fname
    filePushH f = liftIO $ filePush f
    beGroupH ip port = liftIO $ beAGroup ip port
    joinAGroupH s = liftIO $ joinAGroup s
    addToGroupH s = liftIO $ addMeToGroup s

    filePull :: Maybe String -> IO (Maybe File)
    filePull (Just f) = flip runSqlPersistMPool pool $ do
      mFile <- (selectFirst [FileName ==. f] [])
      return $ entityVal <$> mFile
    filePull Nothing = return Nothing


    filePush :: File -> IO (Maybe Bool)
    filePush f = flip runSqlPersistMPool pool $ do
      exists <- selectFirst [FileName ==. (fileName f)] []
      case exists of
        Nothing -> do
          Just <$> insert f
          return $ Just True
        Just _ -> return Nothing

-- be a group :
-- send http request to DS to make a group
-- set Primary table ip and port
-- inclue ones self in the ReplicationServer table
    beAGroup :: String -> Int -> IO ()
    beAGroup ip port = flip runSqlPersistMPool pool $ do
      res <- insert (Primary ip port)
      res1 <- insert (ReplicationServer $ Server' (pack ip) port)
      notifyDS (pack ip) port
      return ()

-- join a group:
-- ask DS for a group to join
-- send join message to FileServer specified by DS
-- recieve list of servers in group
-- add main server as your primary
-- add other servers to replication list
    joinGroup :: Server' -> IO ()
    joinGroup s@(Server' ip port) = flip runSqlPersistMPool pool $ do
      prime <- askForGroup s
      res <- insert (Primary (primaryIP prime) (primaryPort prime))
      s <- askToJoin s (primaryIP prime) (primaryPort prime)
      case s of
        Nothing -> return False
        Just x -> Prelude.foldr (insert ReplicationServer) x










app :: ConnectionPool -> Application
app pool = serve api $ server pool

mkApp :: FilePath -> IO Application
mkApp sqliteFile = do
    pool <- runStderrLoggingT $ do
      createSqlitePool (cs sqliteFile) 5

    runSqlPool (runMigration migrateAll) pool
    return $ app pool

run :: FilePath -> String -> String -> IO ()
run sqliteFile port  command = do
    Warp.run (read port) =<< mkApp sqliteFile
    putStrLn command
