module Main where

import App

main :: IO ()
main = do
  p <- getLine
  run "sqlite.db" (read p)
