{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE DisambiguateRecordFields  #-}
module Network.Hive
    ( HiveConfig (..)
    , hive
    , defaultHiveConfig

    -- Re-export of stuff from CaptureMap.
    , capture

    -- Re-export of stuff from EndPoint.
    , Hive
    , HttpMethod (..)
    , Guard (..)
    , (</>)
    , (</:>)
    , (<!>)
    , (==>)
    , (~~>)
    , guardedBy
    , match
    , matchAll
    , handledBy
    , servedBy
    , webSocket

    -- Re-export of stuff from Handler.
    , Handler
    , StatusCode (..)
    , HandlerResponse
    , bodyByteString
    , bodyJSON
    , bodyStream
    , redirectTo
    , respondWith
    , respondByteString
    , respondFile
    , respondJSON
    , respondText
    , serveDirectory
    , queryValue
    , queryValues
    , liftIO

    -- Re-export of stuff from Server.
    , Server
    , acceptRequest
    , rejectRequest

    -- Re-export of stuff from ConnectedServer.
    , ConnectedServer
    , DataMessage (..)
    , WebSocketsData
    , receiveDataMessage
    , sendBinaryMessage
    , sendTextMessage
    , forkPingThread

    -- Re-export of stuff from Logger.
    , LoggerStream (..)
    , logInfo
    , logWarning
    , logError
    ) where

import Control.Exception (Exception, SomeException, catch)
import Control.Monad (msum)
import Data.Time (NominalDiffTime, getCurrentTime, diffUTCTime)
import Network.Hive.CaptureMap (capture)
import Network.Hive.ConnectedServer ( ConnectedServer
                                    , DataMessage (..)
                                    , WebSocketsData
                                    , receiveDataMessage
                                    , sendBinaryMessage
                                    , sendTextMessage
                                    , forkPingThread
                                    )
import Network.Hive.EndPoint ( Hive
                             , Guard (..)
                             , HttpEndPoint (..)
                             , HttpMethod (..)
                             , WsEndPoint (..)
                             , (</>)
                             , (</:>)
                             , (<!>)
                             , (==>)
                             , (~~>)
                             , guardedBy
                             , match
                             , matchAll
                             , separateEndPoints
                             , handledBy
                             , servedBy
                             , webSocket
                             , runHive
                             )
import Network.Hive.Handler ( Handler
                            , StatusCode (..)
                            , HandlerResponse (..)
                            , Context (..)
                            , runHandler
                            , defaultErrorHandler
                            , bodyByteString
                            , bodyJSON
                            , bodyStream
                            , redirectTo
                            , respondWith
                            , respondByteString
                            , respondFile
                            , respondJSON
                            , respondText
                            , serveDirectory
                            , queryValue
                            , queryValues
                            , liftIO
                            )
import Network.Hive.Logger ( LoggerStream (..)
                           , LogLevel (..)
                           , LoggerSet
                           , createLogger
                           , logWithLevel
                           , logInfo
                           , logWarning
                           , logError
                           )
import Network.Hive.Matcher ( HttpMatch (..)
                            , WsMatch (..)
                            , matchHttp
                            , matchWebSocket
                            )
import Network.Hive.Server ( Server
                           , ServerContext (..)
                           , runServer
                           , acceptRequest
                           , rejectRequest
                           )
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Text.Printf (printf)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Network.WebSockets as WS

-- | Configuration of Hive.
data HiveConfig
    = HiveConfig
        { port         :: !Int
        , loggerStream :: !LoggerStream
        , errorHandler :: forall e. Exception e =>
                              e -> Handler HandlerResponse
        }

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

    -- Start Warp with the WebSocket and HTTP services.
    run (port config) $
        websocketsOr WS.defaultConnectionOptions
                     (webSocketService logger wsEndPoints)
                     (httpService logger config httpEndPoints)

-- | Callback action that is invoked for each WebSocket request.
webSocketService :: LoggerSet -> [WsEndPoint] -> WS.ServerApp
webSocketService logger endPoints pend = do
    -- Try finding a server matching the request.
    let requestHead = WS.pendingRequest pend
    case msum $ map (matchWebSocket requestHead) endPoints of
        Just theMatch -> do
            let server  = wsServer $ endPointWs theMatch
                context = ServerContext
                            { captureMap = captureWs theMatch
                            , loggerSet  = logger
                            , pendConn   = pend
                            }
            runServer server context

        -- No server is found. Log error and reject request.
        Nothing       -> do
            let msg = printf "No server found for: %s"
                             (BS.unpack $ WS.requestPath requestHead)
            logWithLevel logger Error msg
            WS.rejectRequest pend "No server found"

-- | Callback action that is invoked for each HTTP request.
httpService :: LoggerSet -> HiveConfig -> [HttpEndPoint] -> Application
httpService logger config endPoints req respReceived = do
    start <- getCurrentTime
    resp  <- findAndExecHandler
    stop  <- getCurrentTime
    let duration = stop `diffUTCTime` start
        logStr   = mkHandlerLogStr req resp duration
    logWithLevel logger Info logStr

    -- Return control back to Wai.
    respReceived resp
    where
      -- Find and execute a matching handler.
      findAndExecHandler :: IO Response
      findAndExecHandler =
        -- Try finding a handler matching the request.
        case msum $ map (matchHttp req) endPoints of
            Just theMatch -> do
                let handler = httpHandler $ endPointHttp theMatch
                    context = Context
                                { captureMap = captureHttp theMatch
                                , request    = req
                                , loggerSet  = logger
                                }

                -- Try run the handler. It may generate exception though.
                HandlerResponse response <-
                    runHandler handler context `catch` exceptions context

                return response

            -- No handler found. Log error and respond with code 500.
            Nothing    -> do
                let msg = printf "No handler found for: %s"
                                 (BS.unpack $ rawPathInfo req)
                logWithLevel logger Error msg
                return $ responseLBS status500 [] $ LBS.pack msg

      -- Handle all exceptions by executing the reqistered error handler.
      exceptions :: Context -> SomeException -> IO HandlerResponse
      exceptions context e = do
          let errorHandler' = errorHandler config
          runHandler (errorHandler' e) context

-- | Default configuration of Hive. Override the fields in order to change
-- the configuration.
defaultHiveConfig :: HiveConfig
defaultHiveConfig =
    HiveConfig
      { port         = 8888
      , loggerStream = Stdout
      , errorHandler = defaultErrorHandler
      }

-- | Contruct a log string from the parameters.
mkHandlerLogStr :: Request -> Response -> NominalDiffTime -> String
mkHandlerLogStr req resp dur =
    let requestMethod' = BS.unpack $ requestMethod req
        rawPathInfo'   = BS.unpack $ rawPathInfo req
        statusCode'    = statusCode $ responseStatus resp
        duration       = show dur
    in printf "%s %s %d %s" requestMethod' rawPathInfo' statusCode' duration
