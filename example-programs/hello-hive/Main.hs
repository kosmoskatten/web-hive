{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import Network.Hive ( Accept (..)
                    , Handler
                    , HandlerResponse
                    , (</>), (</:>)
                    , accepts
                    , defaultRoute
                    , get
                    , handledBy
                    , hive
                    , defaultHiveConfig
                    , capture
                    , respondText
                    , liftIO
                    )
import Text.Printf (printf)

main :: IO ()
main = hive defaultHiveConfig $ do
    get </> "hello" </:> "hive" `accepts` Anything `handledBy` helloHandler
    defaultRoute `handledBy` defaultHandler

helloHandler :: Handler HandlerResponse
helloHandler = do
    name <- capture "hive"
    respondText $ "Hello " `mappend` name `mappend` "!"

defaultHandler :: Handler HandlerResponse
defaultHandler = respondText "You hit the defaultHandler"
