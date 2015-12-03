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
    , HandlerResponse
    , capture
    , respond
    , respondText
    , queryValue
    , queryValues
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
import Network.Hive.Handler ( Handler
                            , HandlerResponse (..)
                            , Context (..)
                            , runHandler
                            , capture
                            , respond
                            , respondText
                            , queryValue
                            , queryValues
                            , liftIO
                            )
import Network.Hive.Logger ( LoggerStream (..)
                           , LogLevel (..)
                           , LoggerSet
                           , createLogger
                           , logWithLevel
                           ) 
import Network.Hive.Matcher (HttpMatch (..), matchHttp)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Text.Printf (printf)

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Network.WebSockets as WS

-- | Configuration of Hive.
data HiveConfig
    = HiveConfig
        { port         :: !Int
        , loggerStream :: !LoggerStream
        }
        deriving Show

-- | Entry point to start a Hive application server.
hive :: HiveConfig -> Hive () -> IO ()
hive config hive' = do
    logger <- createLogger $ loggerStream config
    let (httpEndPoints, wsEndPoints) = separateEndPoints $ runHive hive'

    logWithLevel logger Info $
        printf "Hive started. Listening on port: %d" (port config)
    logWithLevel logger Info $
        printf "Started with %d HTTP endpoints" (length httpEndPoints)
    logWithLevel logger Info $
        printf "Started with %d WebSocket endpoints" (length wsEndPoints)

    run (port config) $ 
        websocketsOr WS.defaultConnectionOptions
                     (webSocketService logger wsEndPoints)
                     (httpService logger httpEndPoints)

webSocketService :: LoggerSet -> [WsEndPoint] -> WS.ServerApp
webSocketService = undefined

httpService :: LoggerSet -> [HttpEndPoint] -> Application
httpService logger endPoints req respReceived =
    case msum $ map (matchHttp req) endPoints of
        Just match -> do
            let handler = httpHandler $ endPointHttp match
                context = Context
                            { captureMap = captureHttp match
                            , request    = req
                            , loggerSet  = logger
                            }
            HandlerResponse response <- runHandler handler context
            respReceived response
        Nothing    -> do
            let msg = printf "No handler found for: %s"
                             (show $ rawPathInfo req)
            logWithLevel logger Error msg
            respReceived $ responseLBS status500 [] $ LBS.pack msg

-- | Default configuration of Hive. Override the fields in order to change
-- the configuration.
defaultHiveConfig :: HiveConfig
defaultHiveConfig =
    HiveConfig
      { port         = 8888
      , loggerStream = Stdout 
      }

