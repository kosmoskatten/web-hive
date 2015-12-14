{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import Network.Hive

main :: IO ()
main =
    hive defaultHiveConfig $ do
        match GET `guardedBy` None
                  `handledBy` redirectTo "index.html"

        matchAll `handledBy` serveDirectory siteDir

siteDir :: FilePath
siteDir = "example-programs/note-it/site"
