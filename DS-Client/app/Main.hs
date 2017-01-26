module Main where

import Lib

main :: IO ()
main = do
  createCluster "localhost" 3002
  uf (File "this is my contents" "this is my gun")
  downloadFile "this is my gun" "/Users/Sean/College/Distributed-file-system"
