{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module App where

import           Api
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger     (runStderrLoggingT)
import           Data.Foldable            as F
import           Data.String.Conversions
import           Data.Text                as T
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite
import           Model
import           Network.Wai
import           Network.Wai.Handler.Warp as Warp
import           Servant
import           System.Directory
import           System.FilePath.Posix

toText :: [FilePath] -> Text
toText = F.foldMap pack

server :: ConnectionPool -> Server Api
server pool = showDir
          :<|> changeDir

  where
    -- showDirH = liftIO $ showDir
   -- changeDirH dir = return $ changeDir dir

    showDir :: Handler Text
    showDir = liftIO $ do
       res <- (getCurrentDirectory >>= getDirectoryContents)
       return $ toText res

    changeDir :: Text -> Handler T.Text
    changeDir dir = do
      liftIO $ setCurrentDirectory $ T.unpack dir
      return ""



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
