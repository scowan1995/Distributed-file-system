module Main where

import App
import System.Environment

main :: IO ()
main = do
  [port, command] <- getArgs
  run "sqlite.db" port command
