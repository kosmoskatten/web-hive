module Main
    ( main
    ) where

import Network.Hive (hive, defaultHiveConfig)

main :: IO ()
main = hive defaultHiveConfig $ return ()
