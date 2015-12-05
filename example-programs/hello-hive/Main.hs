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
                    , queryValue
                    )

-- | Example program exposing two endpoints:
-- /hello/<name> which is taking a name as a capture.
-- /hello-q which is taking a name in a query parameter "name".
--
-- Try:
-- > curl http://localhost:8888
-- > You hit the default handler
--
-- > curl http://localhost:8888/hello/Hive
-- > Hello Hive
--
-- > curl http://localhost:8888/hello-q
-- > Hello Q anonymous
--
-- > curl http://localhost:8888/hello-q?name=Hive
-- > Hello Q Hive 
main :: IO ()
main = hive defaultHiveConfig $ do
    -- /hello/<name>
    -- Handler implemented as a separate function.
    get </> "hello" </:> "name" 
        `accepts` Anything 
        `handledBy` helloHandler

    -- /hello-q
    -- Handler implemented as an inline action.
    get </> "hello-q"
        `accepts` Anything
        `handledBy` do
            maybeName <- queryValue "name"
            case maybeName of
                Just name -> respondText $ "Hello Q " `mappend` name
                                                      `mappend` "\n"
                Nothing   -> respondText "Hello Q anonymous\n"

    -- Default route. Catches all and must be the last route.
    defaultRoute `handledBy` respondText "You hit the default handler\n"

helloHandler :: Handler HandlerResponse
helloHandler = do
    name <- capture "name"
    respondText $ "Hello " `mappend` name `mappend` "\n"
