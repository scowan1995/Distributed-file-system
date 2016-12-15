{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module App where

import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Logger (runStderrLoggingT)
import Data.String.Conversions
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import Network.Wai
import Network.Wai.Handler.Warp as Warp
import Servant
import Data.Text as T
import Model
import Data.Foldable as F
import Api
import System.FilePath.Posix
import System.Directory

toText :: [FilePath] -> Text
toText = F.foldMap pack

server :: ConnectionPool -> Server Api
server pool =
  showDirH :<|> changeDirH
  where
    showDirH = liftIO $ showDir
    changeDirH dir = return $ changeDir dir

    showDir :: IO Text
    showDir = do
      res <- (getCurrentDirectory >>= getDirectoryContents)
      return $ toText res

    changeDir :: String -> Handler T.Text
    changeDir dir = T.pack $ setCurrentDirectory dir



app :: ConnectionPool -> Application
app pool = serve api $ server pool

mkApp :: FilePath -> IO Application
mkApp sqliteFile = do
  pool <- runStderrLoggingT $ do
    createSqlitePool (cs sqliteFile) 5
  runSqlPool (runMigration migrateAll) pool
  return $ app pool

run :: FilePath -> IO ()
run sqliteFile = do
  setCurrentDirectory "pseudo-file-system"
  Warp.run 3000 =<< mkApp sqliteFile
