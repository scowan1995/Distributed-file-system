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
import Control.Monad
import Data.Maybe
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

askForGroup :: Server' -> IO (Maybe Server')
askForGroup s1@(Server' oip oport) = do
  manager <- newManager defaultManagerSettings
  x <- runClientM (addMeToGroup s1) (ClientEnv manager (BaseUrl Http "localhost" 3003 ""))
  case x of
    Left e -> do
      putStrLn $ "Error is askForGroup in FileServer: " ++ show e
      return Nothing
    Right s -> return s


notifyDS :: Text -> Int -> IO Bool
notifyDS ip port = do
  manager <- newManager defaultManagerSettings
  x <- runClientM ( createGroup (Server' ip port)) (ClientEnv manager (BaseUrl Http "localhost" 3003 ""))
  case x of
    Left e -> do
    --   putStrLn $ "Error in notifyDs in Fileserver: " ++ show e
      return False
    Right x -> return True

askToJoin :: Server' -> Text -> Int -> IO ( Maybe [Server'])
askToJoin s@(Server' sip sport) ip port = do
  manager <- newManager defaultManagerSettings
  x <- runClientM (letmejoin (unpack sip) sport) (ClientEnv manager (BaseUrl Http (unpack ip) port ""))
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
    addToGroupH ip port = liftIO $ addMeToGroup ip port

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
    beAGroup :: String -> Int -> IO Bool
    beAGroup ip port = flip runSqlPersistMPool pool $ do
      res <- insert (Primary ip port)
      res1 <- insert ((Server' (pack ip) port))
      notifyDS (pack ip) port
      return True


-- join a group:
-- ask DS for a group to join
-- send join message to FileServer specified by DS
-- recieve list of servers in group
-- add main server as your primary
-- add other servers to replication list
    joinAGroup :: Server' -> IO ()
    joinAGroup s@(Server' ip port) = flip runSqlPersistMPool pool $ do
      prime <- askForGroup s
      case prime of
        Nothing -> return ()
        Just y -> do
          res <- insert (Primary (primaryIP y) (primaryPort y))
          s <- askToJoin s (primaryIP prime) (primaryPort prime)
          case s of
            Nothing -> return ()
            Just x -> do
              mapM  insert x
              return ()


-- add server to ReplicationServer
    addMeToGroup :: Text -> Int -> IO ([Server'])
    addMeToGroup ip port = flip runSqlPersistMPool pool $ do
      x <- insert (Server' ip port)
      ls <- selectList [Server'PrimaryIP !=. ip] []
      return ls











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
