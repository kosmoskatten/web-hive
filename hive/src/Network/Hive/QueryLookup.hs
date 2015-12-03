module Network.Hive.QueryLookup
    ( queryValue
    , queryValues
    ) where

import Data.ByteString (ByteString)
import Data.List (foldl')
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.HTTP.Types (Query, QueryItem)

-- | Fetch the first - if any - value for the given key.
queryValue :: Text -> Query -> Maybe Text
queryValue key query =
    let keyBs = encodeUtf8 key
    in case lookup keyBs query of
           Just (Just value) -> Just (decodeUtf8 value)
           _                 -> Nothing

-- | Fetch all - if any - values for the given key. In the same order as
-- for the original query string.
queryValues :: Text -> Query -> [Text]
queryValues key = reverse . foldl' (g $ encodeUtf8 key) []
    where
      g :: ByteString -> [Text] -> QueryItem -> [Text]
      g keyBs vs (k, Just v)
        | k == keyBs      = decodeUtf8 v:vs
        | otherwise       = vs
      g _ vs (_, Nothing) = vs
