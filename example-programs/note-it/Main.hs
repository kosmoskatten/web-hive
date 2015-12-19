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
import GHC.Generics (Generic)
import Network.Hive
import System.Random (randomRIO)

import qualified Data.Text as T

-- | The database of notes. Just a TVar protected list of notes.
newtype Database = Database (TVar [Note])

-- | Object created by the client for a new note.
data NewNote = NewNote { newNote :: !Text }
    deriving (Generic, Show)

-- | A representation of a note.
data Note 
    = Note
      { note        :: !Text
      , resourceId  :: !Text
      , resourceUrl :: !Text
      }
    deriving (Generic, Show)

instance FromJSON NewNote
instance ToJSON Note

-- | Example program that is implementing a simple "notes" application.
main :: IO ()
main = do
    db <- Database <$> newTVarIO []
    hive defaultHiveConfig $ do
        -- Match a GET on the site root. Redirect to index.html.
        match GET <!> None
                  ==> redirectTo "index.html"

        -- Match a GET request on the note resource. List all notes.
        match GET </> "note" <!> None
                  ==> (respondJSON Ok =<< listNotes db)

        -- Match a POST request on the note resource. Create a new note.
        match POST </> "note" <!> None
                   ==> (respondJSON Ok =<< handleNewNote db =<< bodyJSON)

        -- Match a DELETE request on a specific note. Delete the note.
        match DELETE </> "note" </:> "id" <!> None
                     ==> deleteNote db

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
    noteId <- liftIO (T.pack . show <$> mkNoteId (newNote n))
    let theNote = Note { note        = newNote n
                       , resourceId  = noteId
                       , resourceUrl = "/note/" `mappend` noteId
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
        then respondText Ok ""
        else respondText NotFound "Noes ..."
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
mkNoteId :: Text -> IO Int
mkNoteId t = do
    salt <- randomRIO (minBound, maxBound)
    return (abs $ hashWithSalt salt t)
