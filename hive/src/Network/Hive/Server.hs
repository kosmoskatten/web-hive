{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Server is a module implementing a monad and an API to help write
-- WebSocket serving functions.
module Network.Hive.Server
    ( Server
    , ServerContext (..)
    , runServer
    , acceptRequest
    , rejectRequest
    ) where

import Control.Monad.Reader ( ReaderT
                          , MonadReader
                          , MonadIO
                          , runReaderT
                          , ask
                          , liftIO
                          )
import Data.ByteString (ByteString)
import Network.Hive.CaptureMap (CaptureMap, CaptureBearer (..))
import Network.Hive.ConnectedServer ( ConnectedServer
                                    , ConnectedServerContext (..)
                                    , runConnectedServer
                                    )
import Network.Hive.Logger ( LoggerSet
                           , LogBearer (..)
                           )
import Network.WebSockets (PendingConnection)

import qualified Network.WebSockets as WS

-- | The runtime context for a (pending) server.
data ServerContext
    = ServerContext
       { captureMap :: !CaptureMap
       , loggerSet  :: !LoggerSet
       , pendConn   :: !PendingConnection
       }

-- | LogBearer instance for ServerContext.
instance LogBearer ServerContext where
    getLoggerSet = loggerSet

-- | CaptureBearer instance for ServerContext.
instance CaptureBearer ServerContext where
    getCaptureMap = captureMap

-- | The Server monad, in which server actions are performed.
newtype Server a
    = Server { extrReader :: ReaderT ServerContext IO a}
    deriving ( Applicative, Functor, Monad
             , MonadReader ServerContext, MonadIO )

-- | Invoke a server.
runServer :: Server a -> ServerContext -> IO a
runServer action = runReaderT (extrReader action)

-- | Accept a pending request and transfer in to the connected state.
acceptRequest :: ConnectedServer () -> Server ()
acceptRequest action = do
    state <- ask
    conn  <- liftIO $ WS.acceptRequest (pendConn state)
    let context = ConnectedServerContext
                    { captureMap' = captureMap state
                    , loggerSet'  = loggerSet state
                    , connection  = conn
                    }
    liftIO $ runConnectedServer action context

-- | Reject a pending request.
rejectRequest :: ByteString -> Server ()
rejectRequest message = do
    state <- ask
    liftIO $ WS.rejectRequest (pendConn state) message
