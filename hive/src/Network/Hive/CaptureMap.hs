-- | CaptureMap with monadic access.
module Network.Hive.CaptureMap
    ( CaptureMap
    , CaptureBearer (..)
    , empty
    , insert
    , capture
    ) where

import Control.Monad.Reader ( MonadReader
                            , ask
                            )
import Data.Map.Lazy (Map)
import Data.Maybe (fromJust)
import Data.Text (Text)

import qualified Data.Map.Lazy as Map

-- | CaptureMap is a map from Text to Text.
type CaptureMap = Map Text Text

-- | Typeclass for types having a CaptureMap.
class CaptureBearer a where
    getCaptureMap :: a -> CaptureMap

-- | Give back an empty CaptureMap.
empty :: CaptureMap
empty = Map.empty

-- | Insert a key/value pair to the CaptureMap.
insert :: Text -> Text -> CaptureMap -> CaptureMap
insert = Map.insert

-- | Lookup a value from the CaptureMap. If not found an exception
-- will be thrown.
capture :: (CaptureBearer a, MonadReader a m) => Text -> m Text
capture label = fromJust . Map.lookup label . getCaptureMap <$> ask
