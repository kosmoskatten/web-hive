{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import Data.Aeson
import Data.Hashable (hashWithSalt)
import Data.Text (Text)
import Data.Time (ZonedTime, getZonedTime)
import GHC.Generics (Generic)
import Network.Hive
import System.Random (randomRIO)

import qualified Data.Text as T

-- | Data created by the client for a new note.
data NewNote = NewNote { newNote :: !Text }
    deriving (Generic, Show)

data Note 
    = Note
      { note        :: !Text
      , timeStamp   :: !ZonedTime
      , resourceId  :: !Text
      , resourceUrl :: !Text
      }
    deriving (Generic, Show)

instance FromJSON NewNote
instance ToJSON Note

main :: IO ()
main =
    hive defaultHiveConfig $ do
        -- Match a GET on the site root. Redirect to index.html.
        match GET `guardedBy` None
                  `handledBy` redirectTo "index.html"

        match POST </> "note" 
                   `guardedBy` None
                   `handledBy` (respondJSON =<< handleNewNote =<< bodyJSON)

        -- The match all clause will do static serving
        matchAll `handledBy` serveDirectory siteDir

handleNewNote :: NewNote -> Handler Note
handleNewNote n = do
    logInfo $ show n
    now     <- liftIO $ getZonedTime
    noteId' <- liftIO (T.pack . show <$> noteId (newNote n))
    return Note { note        = newNote n
                , timeStamp   = now
                , resourceId  = noteId'
                , resourceUrl = "/note/" `mappend` noteId'
                }

siteDir :: FilePath
siteDir = "example-programs/note-it/site"

-- | A dumb hash to identify a Note. Based on the note text and a random
-- hash salt.
noteId :: Text -> IO Int
noteId t = do
    salt <- randomRIO (minBound, maxBound)
    return (abs $ hashWithSalt salt t)
