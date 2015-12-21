{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import Control.Monad (forever)
import Network.Hive

import qualified Data.ByteString.Lazy.Char8 as LBS

main :: IO ()
main =
    hive defaultHiveConfig $ do
        -- Redirect root to index.html.
        match GET <!> None ==> redirectTo "index.html"

        -- Serve static files.
        matchAll ==> serveDirectory siteDir

        -- An echo WebSocket server.
        webSocket </> "echo" `servedBy` echoServer

echoServer :: Server ()
echoServer = do
    acceptRequest
    forever $ do
        msg <- receiveDataMessage
        case msg of
            Binary bMsg -> do
              logInfo $ "Got binary: " `mappend` LBS.unpack bMsg
              sendBinaryMessage bMsg

            Text   tMsg -> do
              logInfo $ "Got text: " `mappend` LBS.unpack tMsg
              sendTextMessage tMsg

siteDir :: FilePath
siteDir = "example-programs/websocket-echo/site"
