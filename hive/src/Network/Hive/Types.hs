-- | Common types for the Hive library.
module Network.Hive.Types
    ( CaptureMap
    ) where

import Data.Map.Lazy (Map)
import Data.Text (Text)

type CaptureMap = Map Text Text
