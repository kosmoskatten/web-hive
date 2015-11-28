{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Server is a module implementing a monad and an API to help write
-- WebSocket serving functions.
module Network.Hive.Server
    ( Server
    , runServer
    ) where

import Control.Monad.Reader (ReaderT, MonadReader, MonadIO, runReaderT)

data Context = Context

-- | The Server monad, in which server actions are performed.
newtype Server a = 
    Server { extrReader :: ReaderT Context IO a}
    deriving (Applicative, Functor, Monad, MonadReader Context, MonadIO)

-- | Invoke a server.
runServer :: Server a -> IO a
runServer action = runReaderT (extrReader action) Context
