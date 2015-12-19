{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
-- | Handler is a module implementing a monad and an API to help write
-- HTTP handler functions.
module Network.Hive.Handler
    ( Handler
    , StatusCode (..)
    , Context (..)
    , HandlerResponse (..)
    , runHandler
    , defaultErrorHandler
    , bodyJSON
    , capture
    , redirectTo
    , respondWith
    , respondFile
    , respondJSON
    , respondText
    , serveDirectory
    , queryValue
    , queryValues
    , logInfo
    , logWarning
    , logError
    , liftIO
    ) where

import Control.Exception (Exception (..))
import Control.Monad.Reader ( ReaderT
                            , MonadReader
                            , MonadIO
                            , ask
                            , runReaderT
                            , liftIO
                            )
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8Builder)
import Network.Hive.Logger ( LoggerSet
                           , LogLevel (..)
                           , logWithLevel
                           )
import Network.Hive.Types (CaptureMap)
import Network.HTTP.Types ( Status
                          , hContentType
                          , hLocation
                          , status200
                          , status201
                          , status301
                          , status404
                          , status500
                          )
import Network.Wai ( Response
                   , Request (..)
                   , lazyRequestBody
                   , responseBuilder
                   , responseFile
                   , responseLBS
                   )

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map.Lazy as Map
import qualified Network.Hive.QueryLookup as QL

-- | Response type for a handler. Just a thin wrapper on top of Wai's
-- Response type.
newtype HandlerResponse = HandlerResponse Response

-- | Status codes for the response generating functions.
data StatusCode
    = Ok
    | Created
    | MovedPermanently
    | NotFound
    deriving Show

-- | The context data for a running Handler.
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

-- | The default error handler. Just write the received exception on the
-- error log and respond with code 500 and the exception as body.
defaultErrorHandler :: Exception e => e -> Handler HandlerResponse
defaultErrorHandler excp = do
    let str      = displayException excp
        headers  = [(hContentType, "text/plain")]
        response = responseLBS status500 headers $ LBS.pack str
    logError str
    respondWith response

-- | Get the body as a JSON object. Will throw exception if not possible
-- to decode to the requested object.
bodyJSON :: FromJSON a => Handler a
bodyJSON = do
    req  <- request <$> ask
    body <- liftIO $ lazyRequestBody req
    return (fromJust $ decode body)

-- | Get the value of a capture. Will throw exception if capture is not
-- present.
capture :: Text -> Handler Text
capture text = fromJust . Map.lookup text . captureMap <$> ask

-- | Redirect using HTTP response code 301 to the specified path.
redirectTo :: ByteString -> Handler HandlerResponse
redirectTo to = do
    let headers  = [(hLocation, to)]
        statusCode = toStatus MovedPermanently
        response = responseLBS statusCode headers LBS.empty
    respondWith response

-- | Generic respond function. Encapsulate a Wai Response.
respondWith :: Response -> Handler HandlerResponse
respondWith = return . HandlerResponse

-- | Respond with a specific file. Useful in case of re-routing the root
-- endpoint to the application start file. The low level implementation of
-- this function is by Wai's responseFile function.
respondFile :: FilePath -> Handler HandlerResponse
respondFile file = do
    let response = responseFile status200 [] file Nothing
    respondWith response

-- | Respond with a JSON data structure. The response is marked as content 
-- type "application/json".
respondJSON :: ToJSON a => StatusCode -> a -> Handler HandlerResponse
respondJSON !sc !obj = do
    let headers    = [(hContentType, "application/json")]
        statusCode = toStatus sc
        response   = responseLBS statusCode headers $ encode obj
    respondWith response

-- | Respond UTF-8 encoded text. The response is marked as content 
-- type "text/plain".
respondText :: StatusCode -> Text -> Handler HandlerResponse
respondText !sc !text = do
    let headers    = [(hContentType, "text/plain")]
        statusCode = toStatus sc
        response   = responseBuilder statusCode headers $ 
                                     encodeUtf8Builder text
    respondWith response

-- | Serve the given directory, which is assumed to be on top of the
-- current working directory or an absolute path. The path in the request
-- is put on top of the served directory. The low level implementation of
-- this function is by Wai's responseFile function.
serveDirectory :: FilePath -> Handler HandlerResponse
serveDirectory directory = do
    requestPath <- BS.unpack . rawPathInfo . request <$> ask
    let fullPath = directory `mappend` "/" `mappend` requestPath
        response = responseFile status200 [] fullPath Nothing
    respondWith response
                                
-- | Fetch the first - if any - query value for the given key.
queryValue :: Text -> Handler (Maybe Text)
queryValue key = QL.queryValue key . queryString . request <$> ask

-- | Fetch all - if any - qery values for the given key.
queryValues :: Text -> Handler [Text]
queryValues key = QL.queryValues key . queryString . request <$> ask

-- | Log a string on the Info level.
logInfo :: String -> Handler ()
logInfo = logIt Info

-- | Log a string on the Warning level.
logWarning :: String -> Handler ()
logWarning = logIt Warning

-- | Log a string on the Error level.
logError :: String -> Handler ()
logError = logIt Error

logIt :: LogLevel -> String -> Handler ()
logIt level str = do
    logger <- loggerSet <$> ask
    liftIO $ logWithLevel logger level str

toStatus :: StatusCode -> Status
toStatus Ok               = status200
toStatus Created          = status201
toStatus MovedPermanently = status301
toStatus NotFound         = status404
