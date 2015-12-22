-- | Functionality to determine if a specified EndPoint is matching an
-- incoming request.
module Network.Hive.Matcher
    ( HttpMatch (..)
    , WsMatch (..)
    , matchHttp
    , matchWebSocket
    , matchRequestPath
    ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Network.Hive.CaptureMap ( CaptureMap
                               , empty
                               , insert
                               )
import Network.Hive.EndPoint ( HttpEndPoint (..)
                             , WsEndPoint (..)
                             , Path (..)
                             , methodMatchAll
                             )
import Network.Wai (Request (..))
import Network.WebSockets (RequestHead, requestPath)

import qualified Data.ByteString.Char8 as BS

-- | The result if a HTTP match is made.
data HttpMatch
    = HttpMatch
        { captureHttp  :: CaptureMap
        , endPointHttp :: HttpEndPoint
        , requestHttp  :: Request
        }

-- | The result if a Websocket match is made.
data WsMatch
    = WsMatch
        { captureWs  :: CaptureMap
        , endPointWs :: WsEndPoint
        }

-- | Match a HTTP request with a HttpEndPoint.
matchHttp :: Request -> HttpEndPoint -> Maybe HttpMatch
matchHttp request endPoint
    -- If the EndPoint's method is "MATCHALL" there's always a match.
    | httpMethod endPoint == methodMatchAll =
        Just HttpMatch
               { captureHttp  = empty
               , endPointHttp = endPoint
               , requestHttp  = request
               }
    -- If the request's method and the EndPoint's method is different
    -- there's always a mismatch.
    | httpMethod endPoint /= requestMethod request = Nothing

    -- In this case we need to match the request path.
    | otherwise =
        case matchRequestPath (pathInfo request) (httpPath endPoint) of
            Just cm -> Just HttpMatch
                              { captureHttp  = cm
                              , endPointHttp = endPoint
                              , requestHttp  = request
                              }
            Nothing -> Nothing

-- | Match a WebSocket request with a WsEndpoint.
matchWebSocket :: RequestHead -> WsEndPoint -> Maybe WsMatch
matchWebSocket request endPoint =
    case matchRequestPath (splitPath $ requestPath request)
                          (wsPath endPoint) of
        Just cm -> Just WsMatch
                          { captureWs  = cm
                          , endPointWs = endPoint
                          }
        Nothing -> Nothing

-- | Matching the path segments from the request with the EndPoints Path
-- list. All non capture parts must match exactly and the lists must be of
-- equal length. The capture are saved as key/value pairs in the CapturMap.
matchRequestPath :: [Text] -> [Path] -> Maybe CaptureMap
matchRequestPath = match empty
    where
      match :: CaptureMap -> [Text] -> [Path] -> Maybe CaptureMap
      match cm [] []                 = Just cm
      match _ [] _                   = Nothing
      match _ _ []                   = Nothing
      match cm (t:ts) (Path p:ps)
        | t == p                     = match cm ts ps
        | otherwise                  = Nothing
      match cm (t:ts) (Capture c:ps) = match (insert c t cm) ts ps

-- | Split a raw ByteString request path to a list of Text path segments.
splitPath :: ByteString -> [Text]
splitPath = map decodeUtf8 . filter (not . BS.null) . BS.splitWith (== '/')

