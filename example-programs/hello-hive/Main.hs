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
                    , liftIO
                    )

main :: IO ()
main = hive defaultHiveConfig $ do
    get </> "hello" </:> "hive" `accepts` Anything `handledBy` helloHandler
    defaultRoute `handledBy` defaultHandler

helloHandler :: Handler ()
helloHandler = liftIO $ putStrLn "helloHandler"

defaultHandler :: Handler ()
defaultHandler = liftIO $ putStrLn "defaultHandler"
