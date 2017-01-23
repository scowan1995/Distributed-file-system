{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Servant.JS
import App
import Api

main :: IO ()
main = do
  writeJSForAPI api jquery "Client/api.js"
  run "sqlite.db"
