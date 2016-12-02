module Lib
    ( someFunc
    ) where
import UserServer (startServer')

someFunc :: IO ()
someFunc = startServer'
