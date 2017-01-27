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

server :: ConnectionPool -> Server Api
server pool =
  fileAddH :<|> fileGetH :<|> addClusterH
  :<|> makeMePrimaryH :<|> addMeToGroupH :<|> createGroupH

  where
    fileAddH f = liftIO $ fileAdd f
    fileGetH f = liftIO $ fileGet f
    addClusterH ip port = liftIO $ addCluster ip port
    makeMePrimaryH o_ip o_port n_ip n_port = liftIO $ makeMePrimary (Cluster o_ip o_port) (Cluster n_ip n_port)
    addMeToGroupH c = liftIO $ addMeToGroup c
    createGroupH c = liftIO $ createGroup c

    fileGet :: Text -> IO (Maybe Filelocation)
    fileGet fname = flip runSqlPersistMPool pool $ do
      mFile <- selectFirst [FilelocationFilename ==. fname] []
      case isLocked mFile of
        True -> return Nothing
        False -> return $  entityVal <$> mFile


    fileAdd :: Text -> IO (Maybe Cluster)
    fileAdd f = flip runSqlPersistMPool pool $ do
      c <- selectFirst [ClusterPrimaryIP !=. f] []
      insert (Filelocation f (entityVal $ fromJust c))
      return $ entityVal <$> c

    addCluster :: Text -> Int -> IO Bool
    addCluster ip port = flip runSqlPersistMPool pool $ do
      exists <- selectFirst [ClusterPrimaryIP ==. ip] []
      case exists of
        Nothing -> do
          Just <$> insert (Cluster ip port)
          return True
        Just x -> return False


        -- old primary cluster, new primary cluster
    makeMePrimary :: Cluster -> Cluster -> IO ()
    makeMePrimary c_old c_new = flip runSqlPersistMPool pool $ do
      updateWhere [GroupsPrimary ==. c_old] [GroupsPrimary =. c_new]

    addMeToGroup :: Cluster -> IO Cluster
    addMeToGroup c = flip runSqlPersistMPool pool $ do
      gs <- selectFirst [GroupsSize !=. 0] [Desc GroupsSize] -- pick the group with the least backup servers
      case gs of
        Nothing -> return (Cluster "localhost" 99999)
        Just g -> do
          updateWhere [GroupsPrimary ==. (groupsPrimary (entityVal g))][GroupsSize +=. 1]
          return $ groupsPrimary (entityVal g)

    createGroup :: Cluster -> IO Bool
    createGroup c = flip runSqlPersistMPool pool $ do
      insert (Groups c 1)
      return True



      {-
      exists <- selectFirst [FilelocationFilename ==. f] []
      case exists of
        Nothing -> do
          c <- selectFirst [ClusterPrimaryIP !=. f] []
          q <- entityVal <$> c
          case q of
            Just clus -> do
              return $ insert (Filelocation f (clusterCluster clus))
              return $ Just $ clusterPrimaryIP clus
            Nothing -> return Nothing
        Just _ -> return Nothing--}



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
