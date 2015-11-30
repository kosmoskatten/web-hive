{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Handler is a module implementing a monad and an API to help write
-- HTTP handler functions.
module Network.Hive.Handler
    ( Handler
    , Context (..)
    , HandlerResponse (..)
    , runHandler
    , capture
    , liftIO
    ) where

import Control.Monad.Reader ( ReaderT
                            , MonadReader
                            , MonadIO
                            , ask
                            , runReaderT
                            , liftIO
                            )
import Data.Maybe (fromJust)
import Data.Text (Text)
import Network.Hive.Types (CaptureMap)
import Network.Wai (Response, Request)
import System.Log.FastLogger (LoggerSet)

import qualified Data.Map.Lazy as Map

-- | Response type for a handler. Just a thin wrapper on top of Wai's
-- Response type.
newtype HandlerResponse = HandlerResponse Response

data Context 
    = Context
        { captureMap :: !CaptureMap
        , request    :: !Request
        , loggerSet  :: !LoggerSet
        }

-- | The Handler monad, in which handler actions are performed.
newtype Handler a = 
    Handler { extrReader :: ReaderT Context IO a}
    deriving (Applicative, Functor, Monad, MonadReader Context, MonadIO)

-- | Invoke a handler.
runHandler :: Handler a -> Context -> IO a
runHandler action = runReaderT (extrReader action)

-- | Get the value of a capture. Will throw exception if capture is not
-- present.
capture :: Text -> Handler Text
capture text = fromJust . Map.lookup text . captureMap <$> ask
