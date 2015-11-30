{-# LANGUAGE OverloadedStrings #-}
module Network.Hive
    ( HiveConfig (..)
    , hive
    , defaultHiveConfig

    -- Re-export of stuff from EndPoint.
    , Hive
    , Accept (..)
    , (</>)
    , (</:>)
    , accepts
    , get
    , defaultRoute
    , handledBy

    -- Re-export of stuff from Handler.
    , Handler
    , liftIO
    ) where

import Control.Monad (msum)
import Network.Hive.EndPoint ( Hive
                             , Accept (..)
                             , HttpEndPoint (..)
                             , WsEndPoint (..)
                             , (</>)
                             , (</:>)
                             , accepts
                             , get
                             , defaultRoute
                             , separateEndPoints
                             , handledBy
                             , runHive
                             )
import Network.Hive.Handler (Handler, runHandler, liftIO)
import Network.Hive.Matcher (HttpMatch (..), matchHttp)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import System.Log.FastLogger ( LoggerSet
                             , defaultBufSize
                             , newStdoutLoggerSet
                             , toLogStr
                             , pushLogStrLn
                             )
import Text.Printf (printf)

import qualified Network.WebSockets as WS

-- | Configuration of Hive.
data HiveConfig
    = HiveConfig
        { port :: !Int
        }
        deriving Show

-- | Entry point to start a Hive application server.
hive :: HiveConfig -> Hive () -> IO ()
hive config hive' = do
    loggerSet <- newStdoutLoggerSet defaultBufSize
    let (httpEndPoints, wsEndPoints) = separateEndPoints $ runHive hive'
    logStrLn loggerSet $ 
        printf "Hive started. %d http endpoints, %d ws endpoints"
               (length httpEndPoints) (length wsEndPoints)
    run (port config) $ 
        websocketsOr WS.defaultConnectionOptions
                     (webSocketService loggerSet wsEndPoints)
                     (httpService loggerSet httpEndPoints)

webSocketService :: LoggerSet -> [WsEndPoint] -> WS.ServerApp
webSocketService = undefined

httpService :: LoggerSet -> [HttpEndPoint] -> Application
httpService loggerSet endPoints request respReceived =
    case msum $ map (matchHttp request) endPoints of
        Just match -> do
            let handler = httpHandler $ endPointHttp match
            runHandler handler
            respReceived $ responseLBS status200 [] "Hepp"
        Nothing    -> do
            logStrLn loggerSet $ printf "Can't find a handler"
            respReceived $ responseLBS status500 [] "Oy wey!"

-- | Default configuration of Hive. Override the fields in order to change
-- the configuration.
defaultHiveConfig :: HiveConfig
defaultHiveConfig =
    HiveConfig
      { port = 8888 }

logStrLn :: LoggerSet -> String -> IO ()
logStrLn loggerSet str = pushLogStrLn loggerSet $ toLogStr str
