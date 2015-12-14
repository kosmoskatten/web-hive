-- | Functionality to determine if a specified EndPoint is matching an
-- incoming request.
module Network.Hive.Matcher
    ( HttpMatch (..) 
    , matchHttp 
    , matchRequestPath
    ) where

import Data.Text (Text)
import Network.Hive.EndPoint ( HttpEndPoint (..)
                             , Path (..)
                             , methodMatchAll
                             )
import Network.Hive.Types (CaptureMap)
import Network.Wai (Request (..))

import qualified Data.Map.Lazy as Map

-- | The result if a match is made.
data HttpMatch
    = HttpMatch
        { captureHttp  :: CaptureMap
        , endPointHttp :: HttpEndPoint
        , requestHttp  :: Request
        }

-- | Match a request with an EndPoint.
matchHttp :: Request -> HttpEndPoint -> Maybe HttpMatch
matchHttp request endPoint
    -- If the EndPoint's method is "MATCHALL" there's always a match.
    | httpMethod endPoint == methodMatchAll =
        Just HttpMatch
               { captureHttp  = Map.empty
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

-- | Matching the path segments from the request with the EndPoints Path
-- list. All non capture parts must match exactly and the lists must be of
-- equal length. The capture are saved as key/value pairs in the CapturMap.
matchRequestPath :: [Text] -> [Path] -> Maybe CaptureMap
matchRequestPath = match Map.empty
    where
      match :: CaptureMap -> [Text] -> [Path] -> Maybe CaptureMap
      match cm [] []                 = Just cm
      match _ [] _                   = Nothing
      match _ _ []                   = Nothing
      match cm (t:ts) (Path p:ps)
        | t == p                     = match cm ts ps
        | otherwise                  = Nothing
      match cm (t:ts) (Capture c:ps) = match (Map.insert c t cm) ts ps
