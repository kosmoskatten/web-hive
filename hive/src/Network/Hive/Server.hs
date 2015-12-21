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

import Control.Monad.State( StateT
                          , MonadState
                          , MonadIO
                          , evalStateT
                          , get
                          , liftIO
                          )
import Data.ByteString (ByteString)
import Network.Hive.ConnectedServer ( ConnectedServer
                                    , ConnectedServerContext (..)
                                    , runConnectedServer
                                    )
import Network.Hive.Logger ( LoggerSet
                           , LogBearer (..)
                           )
import Network.Hive.Types (CaptureMap)
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

-- | The Server monad, in which server actions are performed.
newtype Server a
    = Server { extrState :: StateT ServerContext IO a}
    deriving ( Applicative, Functor, Monad
             , MonadState ServerContext, MonadIO )

-- | Invoke a server.
runServer :: Server a -> ServerContext -> IO a
runServer action = evalStateT (extrState action)

-- | Accept a pending request and transfer in to the connected state.
acceptRequest :: ConnectedServer () -> Server ()
acceptRequest action = do
    state <- get
    conn  <- liftIO $ WS.acceptRequest (pendConn state)
    let context = ConnectedServerContext
                    { loggerSet'  = loggerSet state
                    , connection  = conn
                    }
    liftIO $ runConnectedServer action context

-- | Reject a pending request.
rejectRequest :: ByteString -> Server ()
rejectRequest message = do
    state <- get
    liftIO $ WS.rejectRequest (pendConn state) message
