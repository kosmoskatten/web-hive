-- | Logger helper functions wrapping the FastLogger package.
module Network.Hive.Logger
    ( LoggerStream (..)
    , LogLevel (..)
    , LoggerSet
    , createLogger
    , logWithLevel
    ) where

import Data.Monoid ((<>))
import Data.Time (getCurrentTime)
import System.Log.FastLogger ( LoggerSet
                             , defaultBufSize
                             , newStdoutLoggerSet
                             , newStderrLoggerSet
                             , pushLogStrLn
                             , toLogStr
                             )

-- | Define the type of logger stream.
data LoggerStream
    = Stdout
    | Stderr
    deriving Show

-- | Define the log level.
data LogLevel
    = Info
    | Warning
    | Error
    deriving Show

-- | Create a new LoggerSet using the LoggerStream specification.
createLogger :: LoggerStream -> IO LoggerSet
createLogger Stdout = newStdoutLoggerSet defaultBufSize
createLogger Stderr = newStderrLoggerSet defaultBufSize

-- | Push a log message, with timestamp and the specified log level.
logWithLevel :: LoggerSet -> LogLevel -> String -> IO ()
logWithLevel loggerSet logLevel msg = do
    now <- getCurrentTime
    let logStr = toLogStr (show now) <> toLogStr " ("
                                     <> toLogStr (show logLevel)
                                     <> toLogStr "): "
                                     <> toLogStr msg
    pushLogStrLn loggerSet logStr

