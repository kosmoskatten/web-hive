module Main
    ( main
    ) where

import Network.Hive

main :: IO ()
main = hive defaultHiveConfig $ do
    defaultRoute `handledBy` serveDirectory staticDirectory

staticDirectory :: FilePath
staticDirectory = "example-programs/random-quote/static"
