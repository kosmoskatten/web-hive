{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import Network.Hive ( Accept (..)
                    , Handler
                    , (</>), (</:>)
                    , accepts
                    , defaultRoute
                    , get
                    , handledBy
                    , hive
                    , defaultHiveConfig
                    , capture
                    , liftIO
                    )
import Text.Printf (printf)

main :: IO ()
main = hive defaultHiveConfig $ do
    get </> "hello" </:> "hive" `accepts` Anything `handledBy` helloHandler
    defaultRoute `handledBy` defaultHandler

helloHandler :: Handler ()
helloHandler = do
    name <- capture "hive"
    liftIO $ putStrLn $ printf "Hello %s!" (show name)

defaultHandler :: Handler ()
defaultHandler = liftIO $ putStrLn "defaultHandler"
