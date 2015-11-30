{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Handler is a module implementing a monad and an API to help write
-- HTTP handler functions.
module Network.Hive.Handler
    ( Handler
    , HandlerResponse (..)
    , runHandler
    , liftIO
    ) where

import Control.Monad.Reader ( ReaderT
                            , MonadReader
                            , MonadIO
                            , runReaderT
                            , liftIO
                            )

-- | Response type for a handler. Just a thin wrapper on top of Wai's
-- Response type.
newtype HandlerResponse = HandlerResponse Int

data Context = Context

-- | The Handler monad, in which handler actions are performed.
newtype Handler a = 
    Handler { extrReader :: ReaderT Context IO a}
    deriving (Applicative, Functor, Monad, MonadReader Context, MonadIO)

-- | Invoke a handler.
runHandler :: Handler a -> IO a
runHandler action = runReaderT (extrReader action) Context
