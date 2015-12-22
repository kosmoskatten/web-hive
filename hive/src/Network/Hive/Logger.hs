-- | Logger helper functions wrapping the FastLogger package.
module Network.Hive.Logger
    ( LoggerStream (..)
    , LogLevel (..)
    , LogBearer (..)
    , LoggerSet
    , createLogger
    , logWithLevel
    , logInfo
    , logWarning
    , logError
    ) where

import Control.Monad.Reader ( MonadReader
                            , MonadIO
                            , ask
                            , liftIO
                            )
import Data.Monoid ((<>))
import Data.Time (getZonedTime)
import System.Log.FastLogger ( LoggerSet
                             , defaultBufSize
                             , newStdoutLoggerSet
                             , newStderrLoggerSet
                             , newFileLoggerSet
                             , pushLogStrLn
                             , toLogStr
                             )

-- | Define the type of logger stream.
data LoggerStream
    = Stdout
    | Stderr
    | ToFile !FilePath
    deriving Show

-- | Define the log level.
data LogLevel
    = Info
    | Warning
    | Error
    deriving Show

-- | Typeclass for MonadReader states having a LoggerSet. Needed for the
-- common implementation of logger functions.
class LogBearer a where
    getLoggerSet :: a -> LoggerSet

-- | Create a new LoggerSet using the LoggerStream specification.
createLogger :: LoggerStream -> IO LoggerSet
createLogger Stdout        = newStdoutLoggerSet defaultBufSize
createLogger Stderr        = newStderrLoggerSet defaultBufSize
createLogger (ToFile file) = newFileLoggerSet defaultBufSize file

-- | Push a log message, with timestamp and the specified log level.
logWithLevel :: LoggerSet -> LogLevel -> String -> IO ()
logWithLevel loggerSet logLevel msg = do
    now <- getZonedTime
    let logStr = toLogStr (show now) <> toLogStr " ("
                                     <> toLogStr (show logLevel)
                                     <> toLogStr "): "
                                     <> toLogStr msg
    pushLogStrLn loggerSet logStr

-- | Push an Info log.
logInfo :: (LogBearer a, MonadIO m, MonadReader a m) => String -> m ()
logInfo = logIt Info

-- | Push a Warning log.
logWarning :: (LogBearer a, MonadIO m, MonadReader a m) => String -> m ()
logWarning = logIt Warning

-- | Push an Error log.
logError :: (LogBearer a, MonadIO m, MonadReader a m) => String -> m ()
logError = logIt Error

logIt :: (LogBearer a, MonadIO m, MonadReader a m) =>
         LogLevel -> String -> m ()
logIt level str = do
    loggerSet <- getLoggerSet <$> ask
    liftIO $ logWithLevel loggerSet level str

