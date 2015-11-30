module Network.Hive 
    ( HiveConfig (..)
    , hive
    , defaultHiveConfig
    ) where

import Network.Hive.EndPoint (Hive, runHive)

data HiveConfig
    = HiveConfig
        { port :: !Int
        }
        deriving Show

hive :: HiveConfig -> Hive () -> IO ()
hive = undefined

defaultHiveConfig :: HiveConfig
defaultHiveConfig =
    HiveConfig
      { port = 8888 }
