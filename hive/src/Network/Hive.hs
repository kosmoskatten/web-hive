module Network.Hive 
    ( HiveConfig (..)
    , hive
    , defaultHiveConfig

    -- Re-export of stuff from EndPoint.
    , Hive
    , get
    , defaultRoute
    , handledBy

    -- Re-export of stuff from Handler.
    , Handler
    ) where

import Network.Hive.EndPoint ( Hive
                             , get
                             , defaultRoute
                             , handledBy
                             , runHive
                             )
import Network.Hive.Handler (Handler)
import System.Log.FastLogger ( LoggerSet
                             , defaultBufSize
                             , newStdoutLoggerSet
                             , toLogStr
                             , pushLogStrLn
                             )
import Text.Printf (printf)

-- | Configuration of Hive.
data HiveConfig
    = HiveConfig
        { port :: !Int
        }
        deriving Show

hive :: HiveConfig -> Hive () -> IO ()
hive config hive = do
    loggerSet <- newStdoutLoggerSet defaultBufSize
    logStrLn loggerSet $ printf "Hive started"

-- | Default configuration of Hive. Override the fields in order to change
-- the configuration.
defaultHiveConfig :: HiveConfig
defaultHiveConfig =
    HiveConfig
      { port = 8888 }

logStrLn :: LoggerSet -> String -> IO ()
logStrLn loggerSet str = pushLogStrLn loggerSet $ toLogStr str
