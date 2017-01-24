{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import App
import qualified Lackey as L
import Api
import System.IO
import Data.Text

ruby :: Text
ruby = L.rubyForAPI api

main :: IO ()
main = do
  writeFile "./Client/methods.rb" (unpack ruby)
  run "sqlite.db"
