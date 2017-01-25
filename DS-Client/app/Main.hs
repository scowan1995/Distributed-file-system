module Main where

import Lib

main :: IO ()
main = do
  uf (File "this is my contents" "this is my gun")
