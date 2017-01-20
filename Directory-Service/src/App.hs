{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module App where

import           Control.Monad.IO.Class
import           Control.Monad.Logger (runStderrLoggingT)

import           Data.String.Conversions

import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite

import           Network.Wai
import           Network.Wai.Handler.Warp as Warp

import           Servant

import           Data.Text

import           Api
import           Models

server :: ConnectionPool -> Server Api
server pool =
  fileAddH :<|> fileGetH

  where
    fileAddH f = liftIO $ fileAdd f
    fileGetH f = liftIO $ fileGet f

    fileGet :: Text -> IO (Maybe File1)
    fileGet fname = flip runSqlPersistMPool pool $ do
      mFile <- selectFirst [File1Filename ==. fname] []
      return $ entityVal <$> mFile

    fileAdd :: File1 -> IO (Maybe (Key File1))
    fileAdd f = flip runSqlPersistMPool pool $ do
      exists <- selectFirst [File1Filename ==. (file1Filename f)] []
      case exists of
        Nothing -> Just <$> insert f
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
  Warp.run 3000 =<< mkApp sqliteFile
