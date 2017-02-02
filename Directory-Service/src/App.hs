{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module App where

import           Control.Monad.IO.Class
import           Control.Monad.Logger (runStderrLoggingT)
import           Data.Text
import           Data.String.Conversions
import           Data.Time.Clock.POSIX
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite
import           Network.Wai
import           Network.Wai.Handler.Warp as Warp
import           Servant
import           Api
import           Models
import Data.Maybe

server :: ConnectionPool -> Server DSApi
server pool =
  fileAddH :<|> fileGetH
  :<|> makeMePrimaryH :<|> addMeToGroupH :<|> createGroupH

  where
    fileAddH f = liftIO $ fileAdd f
    fileGetH f = liftIO $ fileGet f
    addServer'H ip port = liftIO $ addServer' ip port
    makeMePrimaryH o_ip o_port n_ip n_port = liftIO $ makeMePrimary (Server' o_ip o_port) (Server' n_ip n_port)
    addMeToGroupH c = liftIO $ addMeToGroup c
    createGroupH c = liftIO $ createGroup c

    fileGet :: Text -> IO (Maybe Filelocation)
    fileGet fname = flip runSqlPersistMPool pool $ do
      mFile <- selectFirst [FilelocationFilename ==. fname, FilelocationIsLocked ==. False] []
      case mFile of
        Nothing -> return Nothing
        Just x -> return $ entityVal <$> mFile


    fileAdd :: Text -> IO (Maybe Server')
    fileAdd f = flip runSqlPersistMPool pool $ do
      c <- selectFirst [Server'PrimaryIP !=. f] []
      insert (Filelocation f (entityVal $ fromJust c) False)
      return $ entityVal <$> c

    addServer' :: Text -> Int -> IO Bool
    addServer' ip port = flip runSqlPersistMPool pool $ do
      exists <- selectFirst [Server'PrimaryIP ==. ip] []
      case exists of
        Nothing -> do
          Just <$> insert (Server' ip port)
          return True
        Just x -> return False


        -- old primary server', new primary server'
    makeMePrimary :: Server' -> Server' -> IO ()
    makeMePrimary c_old c_new = flip runSqlPersistMPool pool $ do
      updateWhere [GroupsPrimary ==. c_old] [GroupsPrimary =. c_new]

    addMeToGroup :: Server' -> IO (Maybe Server')
    addMeToGroup c = flip runSqlPersistMPool pool $ do
      gs <- selectFirst [GroupsSize !=. 0] [Desc GroupsSize] -- pick the group with the least backup servers
      case gs of
        Nothing -> return Nothing
        Just g -> do
          updateWhere [GroupsPrimary ==. (groupsPrimary (entityVal g))][GroupsSize +=. 1]
          return $ Just $ groupsPrimary (entityVal g)

    createGroup :: Server' -> IO Bool
    createGroup c = flip runSqlPersistMPool pool $ do
      insert (Groups c 1)
      return True


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
  Warp.run 3003 =<< mkApp sqliteFile
