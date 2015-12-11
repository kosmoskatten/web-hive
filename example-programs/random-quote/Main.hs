{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main
    ( Quote (..) 
    , main
    ) where

import Data.Aeson (FromJSON, ToJSON, decode)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.Hive
import System.Random (randomRIO)

import qualified Data.ByteString.Lazy as LBS

-- | The data type for representing a quote.
data Quote
    = Quote
        { author :: !Text
        , quote  :: !Text
        , image  :: !Text
        }
    deriving (Show, Generic)

instance FromJSON Quote where
instance ToJSON Quote where

-- | A simple web service to provide random quotes. The quotes are provided
-- in a text file and are read at program startup.
--
-- The service is providing a REST API to fetch new random quotes. A simple
-- web GUI is implemented to visualize the quotes.
main :: IO ()
main = do
    -- Decode the quote and make the selector to be used.
    Just quotes <- decodeFromFile $ staticDirectory `mappend` "/quotes.json"
    let selector = mkRandomSelector quotes

    -- Start Hive with its default configuration.
    hive defaultHiveConfig $ do
        -- Catch the root of the service. Respond with the index.html file.
        get `accepts` Anything
            `handledBy` do
                respondFile $ staticDirectory `mappend` "/index.html"

        -- The REST call to fetch a new random quote.
        get </> "random-quote" 
            `accepts` Anything
            `handledBy` do
                theQuote <- liftIO selector
                respondJSON theQuote

        -- By default, serve the static directory. Used for static files
        -- like Javascripts and images.
        defaultRoute `handledBy` serveDirectory staticDirectory

-- | The static directory path. Assumes that the serivice is started from
-- the repository root directory.
staticDirectory :: FilePath
staticDirectory = "example-programs/random-quote/site"

-- | Decode the quote file.
decodeFromFile :: FilePath -> IO (Maybe [Quote])
decodeFromFile file = decode <$> LBS.readFile file

-- | Make a random selection action to select a quote from the list.
mkRandomSelector :: [Quote] -> IO Quote
mkRandomSelector qs =
    let maxIndex = length qs - 1
    in do
        index <- randomRIO (0, maxIndex)
        return $ qs !! index
