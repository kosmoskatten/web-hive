{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Server is a module implementing a monad and an API to help write
-- WebSocket serving functions.
module Network.Hive.Server
    ( Server
    , ServerContext (..)
    , DataMessage (..)
    , runServer
    , acceptRequest
    , rejectRequest
    , receiveDataMessage
    , sendBinaryMessage
    , sendTextMessage
    ) where

import Control.Monad.State.Strict ( StateT
                                  , MonadState
                                  , MonadIO
                                  , evalStateT
                                  , get
                                  , put
                                  , liftIO
                                  )
import Data.ByteString (ByteString)
import Network.Hive.Logger ( LoggerSet
                           , LogBearer (..)
                           , logErrorM
                           )
import Network.Hive.Types (CaptureMap)
import Network.WebSockets ( Connection
                          , DataMessage (..)
                          , PendingConnection
                          )

import qualified Data.ByteString.Lazy as LBS
import qualified Network.WebSockets as WS

data ServerContext
    = ServerContext
       { captureMap :: !CaptureMap
       , loggerSet  :: !LoggerSet
       , pendConn   :: !PendingConnection
       , conn       :: !(Maybe Connection)
       }

instance LogBearer ServerContext where
    getLoggerSet = loggerSet

-- | The Server monad, in which server actions are performed.
newtype Server a =
    Server { extrState :: StateT ServerContext IO a}
    deriving (Applicative, Functor, Monad
             , MonadState ServerContext, MonadIO)

-- | Invoke a server.
runServer :: Server a -> ServerContext -> IO a
runServer action = evalStateT (extrState action)

-- | Accept a pending request.
acceptRequest :: Server ()
acceptRequest = do
    state   <- get
    newConn <- liftIO $ WS.acceptRequest (pendConn state)
    put $ state { conn = Just newConn }

-- | Reject a pending request.
rejectRequest :: ByteString -> Server ()
rejectRequest message = do
    state <- get
    liftIO $ WS.rejectRequest (pendConn state) message

-- | Receive some data as a DataMessage
receiveDataMessage :: Server DataMessage
receiveDataMessage = do
    state <- get
    case conn state of
        Just theConn ->
          liftIO $ WS.receiveDataMessage theConn
        Nothing      -> do
          -- TODO: This should really be changed to exception handling.
          logErrorM "No valid WebSocket connection at receiveDataMessage"
          return $ Binary LBS.empty

-- | Send a binary message.
sendBinaryMessage :: LBS.ByteString -> Server ()
sendBinaryMessage message = do
    state <- get
    case conn state of
        Just theConn ->
          liftIO $ WS.sendBinaryData theConn message
        Nothing      ->
          -- TODO: This should really be changed to exception handling.
          logErrorM "No valid WebSocket connection at sendBinaryMessage"

-- | Send a text message.
sendTextMessage :: LBS.ByteString -> Server ()
sendTextMessage message = do
    state <- get
    case conn state of
        Just theConn ->
          liftIO $ WS.sendTextData theConn message
        Nothing      ->
          -- TODO: This should really be changed to exception handling.
          logErrorM "No valid WebSocket connection at sendBinaryMessage"

