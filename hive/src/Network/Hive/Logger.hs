-- | Logger helper functions wrapping the FastLogger package.
module Network.Hive.Logger
    ( LoggerStream (..)
    , LogLevel (..)
    , LogBearer (..)
    , LoggerSet
    , createLogger
    , logWithLevel
    , logInfoM
    , logWarningM
    , logErrorM
    ) where

import Control.Monad.State ( MonadState
                           , MonadIO
                           , get
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

-- | Typeclass for MonadState states having a LoggerSet. Needed for the
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
logInfoM :: (LogBearer a, MonadIO m, MonadState a m) => String -> m ()
logInfoM str = do
    loggerSet <- getLoggerSet <$> get
    liftIO $ logWithLevel loggerSet Info str

-- | Push a Warning log.
logWarningM :: (LogBearer a, MonadIO m, MonadState a m) => String -> m ()
logWarningM str = do
    loggerSet <- getLoggerSet <$> get
    liftIO $ logWithLevel loggerSet Warning str

-- | Push an Error log.
logErrorM :: (LogBearer a, MonadIO m, MonadState a m) => String -> m ()
logErrorM str = do
    loggerSet <- getLoggerSet <$> get
    liftIO $ logWithLevel loggerSet Error str

