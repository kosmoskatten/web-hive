{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | ConnectedServer is the monad for a connected WebSocket server
-- application. Provides API for the connected state.
module Network.Hive.ConnectedServer
    ( ConnectedServer
    , ConnectedServerContext (..)
    , DataMessage (..)
    , WebSocketsData
    , runConnectedServer
    , receiveDataMessage
    , sendBinaryMessage
    , sendTextMessage
    , forkPingThread
    ) where

import Control.Monad.Reader ( ReaderT
                            , MonadReader
                            , MonadIO
                            , runReaderT
                            , ask
                            , liftIO
                            )
import Network.Hive.CaptureMap (CaptureMap, CaptureBearer (..))
import Network.Hive.Logger ( LoggerSet
                           , LogBearer (..)
                           )
import Network.WebSockets ( Connection
                          , DataMessage (..)
                          , WebSocketsData
                          )

import qualified Network.WebSockets as WS

-- | The runtime context for a connected server.
data ConnectedServerContext
    = ConnectedServerContext
        { captureMap' :: !CaptureMap
        , loggerSet'  :: !LoggerSet
        , connection  :: !Connection
        }

-- | LogBearer instance for the ConnectedServerContext.
instance LogBearer ConnectedServerContext where
    getLoggerSet = loggerSet'

-- | CaptureBearer instance for the ConnectedServerContext.
instance CaptureBearer ConnectedServerContext where
    getCaptureMap = captureMap'

-- | The ConnectedServer monad.
newtype ConnectedServer a
    = ConnectedServer { extrReader :: ReaderT ConnectedServerContext IO a }
    deriving ( Functor, Applicative, Monad
             , MonadReader ConnectedServerContext, MonadIO )

-- | Invoke a connected server.
runConnectedServer :: ConnectedServer a -> ConnectedServerContext -> IO a
runConnectedServer action = runReaderT (extrReader action)

-- | Receive some data as a DataMessage
receiveDataMessage :: ConnectedServer DataMessage
receiveDataMessage = do
    conn <- connection <$> ask
    liftIO $ WS.receiveDataMessage conn

-- | Send a binary message.
sendBinaryMessage :: WebSocketsData a => a -> ConnectedServer ()
sendBinaryMessage message = do
    conn <- connection <$> ask
    liftIO $ WS.sendBinaryData conn message

-- | Send a text message.
sendTextMessage :: WebSocketsData a => a -> ConnectedServer ()
sendTextMessage message = do
    conn <- connection <$> ask
    liftIO $ WS.sendTextData conn message

-- | Utility function to fork a thread to send ping messages every
-- n second.
forkPingThread :: Int -> ConnectedServer ()
forkPingThread interval = do
    conn <- connection <$> ask
    liftIO $ WS.forkPingThread conn interval
