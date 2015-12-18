{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import Control.Concurrent.STM ( TVar
                              , atomically
                              , modifyTVar'
                              , newTVarIO
                              , readTVar
                              , readTVarIO
                              )
import Data.Aeson
import Data.Hashable (hashWithSalt)
import Data.Text (Text)
import Data.Time (ZonedTime, getZonedTime)
import GHC.Generics (Generic)
import Network.Hive
import System.Random (randomRIO)

import qualified Data.Text as T

-- | The database of notes.
newtype Database = Database (TVar [Note])

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
main = do
    db <- Database <$> newTVarIO []
    hive defaultHiveConfig $ do
        -- Match a GET on the site root. Redirect to index.html.
        match GET `guardedBy` None
                  `handledBy` redirectTo "index.html"

        -- Match a GET request on the note resource. List all notes.
        match GET </> "note"
                  `guardedBy` None
                  `handledBy` (respondJSON =<< listNotes db)

        -- Match a POST request on the note resource. Create a new note.
        match POST </> "note" 
                   `guardedBy` None
                   `handledBy` (respondJSON      =<< 
                                handleNewNote db =<< 
                                bodyJSON)

        -- Match a DELETE request on a specific note. Delete the note.
        match DELETE </> "note" </:> "id"
                     `guardedBy` None
                     `handledBy` deleteNote db

        -- The match all clause will do static serving
        matchAll `handledBy` serveDirectory siteDir

-- | The database list is built in reverse order, so when listing it
-- there's need for list reverse.
listNotes :: Database -> Handler [Note]
listNotes (Database db) = do
    notes <- liftIO $ readTVarIO db
    return $ reverse notes

handleNewNote :: Database -> NewNote -> Handler Note
handleNewNote (Database db) n = do
    logInfo $ show n
    now     <- liftIO $ getZonedTime
    noteId' <- liftIO (T.pack . show <$> noteId (newNote n))
    let theNote = Note { note        = newNote n
                       , timeStamp   = now
                       , resourceId  = noteId'
                       , resourceUrl = "/note/" `mappend` noteId'
                       }
    liftIO $ addNote (\ns -> theNote:ns)
    return theNote
    where
      addNote :: ([Note] -> [Note]) -> IO ()
      addNote g = atomically $ modifyTVar' db g

deleteNote :: Database -> Handler HandlerResponse
deleteNote (Database db) = do
    noteId    <- capture "id"
    isDeleted <- liftIO $ maybeDeleteNote noteId
    if isDeleted 
        then respondText ""
        else respondText "Noes ..."
    where
      maybeDeleteNote :: Text -> IO Bool
      maybeDeleteNote nid =
          atomically $ do
              let deleteFunc = (filter $ \n -> nid /= resourceId n)
              isFound <- any (\n -> nid == resourceId n) <$> readTVar db
              if isFound 
                  then do modifyTVar' db deleteFunc
                          return True
                  else return False

siteDir :: FilePath
siteDir = "example-programs/note-it/site"

-- | A dumb hash to identify a Note. Based on the note text and a random
-- hash salt.
noteId :: Text -> IO Int
noteId t = do
    salt <- randomRIO (minBound, maxBound)
    return (abs $ hashWithSalt salt t)
