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

import           Data.Text

import           Api
import           Models

server :: ConnectionPool -> Server Api
server pool =
  filePushH :<|> filePullH
  where
    filePullH fname = liftIO $ filePull fname
    filePushH f = liftIO $ filePush f

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




app :: ConnectionPool -> Application
app pool = serve api $ server pool

mkApp :: FilePath -> IO Application
mkApp sqliteFile = do
    pool <- runStderrLoggingT $ do
      createSqlitePool (cs sqliteFile) 5

    runSqlPool (runMigration migrateAll) pool
    return $ app pool

run :: FilePath -> IO ()
run sqliteFile =
  Warp.run 3002 =<< mkApp sqliteFile
