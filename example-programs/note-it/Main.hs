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

-- | Data created by the client for a new note.
data NewNote = NewNote { newNote :: !Text }
    deriving (Generic, Show)

data Note
    = Note
      { note       :: !Text
      , timeStamp  :: !ZonedTime
      , resourceId :: !Text
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
    now <- liftIO $ getZonedTime
    return Note { note       = newNote n
                , timeStamp  = now
                , resourceId = "/note/123456"
                }

siteDir :: FilePath
siteDir = "example-programs/note-it/site"

-- | A dumb hash to identify a Note.
nodeId :: Text -> IO Int
nodeId = undefined
