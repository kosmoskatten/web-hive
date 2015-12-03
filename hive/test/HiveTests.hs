{-# LANGUAGE OverloadedStrings #-}
module HiveTests
    ( shallResp500Test
    , shallRouteTargetTest
    , shallCaptureTest
    , shallBeContentTypeTextTest
    ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Exception (bracket)
import Network.Hive ( Hive
                    , Accept (..)
                    , HiveConfig (..)
                    , (</>), (</:>)
                    , accepts
                    , capture
                    , defaultHiveConfig
                    , defaultRoute
                    , get
                    , handledBy
                    , hive
                    , respondText
                    )
import Network.HTTP.Client (Response)
import Network.HTTP.Types (Status (..), ResponseHeaders, hContentType)
import Test.HUnit
import Text.Printf

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Network.HTTP.Client as C

-- | Access to a non-existing route shall result in a 500/Internal
-- Server Error.
shallResp500Test :: Assertion
shallResp500Test = do
    let thePort = basePort
    withHive thePort theHive $ do
        (sc, _) <- httpGet thePort "/"
        500 @=? sc
    where
      theHive :: Hive ()
      theHive = return ()

-- | Access to exact routes shall match exactly, other shall be handled by
-- the default route.
shallRouteTargetTest :: Assertion
shallRouteTargetTest = do
    let thePort = basePort + 1
    withHive thePort theHive $ do
        (stat1, resp1) <- httpGet thePort "/hello/world"
        (stat2, resp2) <- httpGet thePort "/deep/purple"
        (stat3, resp3) <- httpGet thePort "/just/random"
        (stat4, resp4) <- httpGet thePort "/"
        (stat5, resp5) <- httpGet thePort ""
        (200, "Hello, world!") @=? (stat1, C.responseBody resp1)
        (200, "Deep Purple")   @=? (stat2, C.responseBody resp2)
        (200, "default")       @=? (stat3, C.responseBody resp3)
        (200, "I'm root!")     @=? (stat4, C.responseBody resp4)
        (200, "I'm root!")     @=? (stat5, C.responseBody resp5)
    where
      theHive :: Hive ()
      theHive = do
          get </> "hello" </> "world" 
              `accepts` Anything
              `handledBy` respondText "Hello, world!"
          get </> "deep" </> "purple"
              `accepts` Anything
              `handledBy` respondText "Deep Purple"
          get `accepts` Anything `handledBy` respondText "I'm root!"
          defaultRoute `handledBy` respondText "default"

-- | Access to captures shall match and be captured.
shallCaptureTest :: Assertion
shallCaptureTest = do
    let thePort = basePort + 2
    withHive thePort theHive $ do
        (stat1, resp1) <- httpGet thePort "/anything"
        (stat2, resp2) <- httpGet thePort "/give/tarzan/a/banana"
        (200, "You said anything")    @=? (stat1, C.responseBody resp1)
        (200, "tarzan want a banana") @=? (stat2, C.responseBody resp2)
    where
      theHive :: Hive ()
      theHive = do
          get </:> "word" 
              `accepts` Anything
              `handledBy` do
                  word <- capture "word"
                  respondText $ "You said " `mappend` word

          get </> "give" </:> "name" </> "a" </:> "fruit"
              `accepts` Anything
              `handledBy` do
                  name  <- capture "name"
                  fruit <- capture "fruit"
                  respondText $ name `mappend` " want a " `mappend` fruit

-- | Shall report content type as text/plain.
shallBeContentTypeTextTest :: Assertion
shallBeContentTypeTextTest = do
    let thePort = basePort + 3
    withHive thePort theHive $ do
        (stat, resp) <- httpGet thePort "/"
        (200, Just "text/plain") @=? 
            (stat, contentType $ C.responseHeaders resp)
    where
      theHive :: Hive ()
      theHive = get `accepts` Anything `handledBy` respondText ""

httpGet :: Int -> String -> IO (Int, Response LBS.ByteString)
httpGet p url = do
    req     <- C.parseUrl $ printf "http://localhost:%d%s" p url
    let req' = req { C.checkStatus = \_ _ _ -> Nothing }
    manager <- C.newManager C.defaultManagerSettings
    resp    <- C.httpLbs req' manager
    return (statusCode $ C.responseStatus resp, resp)

-- | Run Hive in a separate thread. Cancel the Hive thread as soon as the
-- testing actions has finished.
withHive :: Int -> Hive () -> Assertion -> IO ()
withHive port' hive' action = 
    bracket startHive cancel $ \_ -> threadDelay 100000 >> action
    where
      startHive :: IO (Async ())
      startHive = do
          let config = defaultHiveConfig { port = port' }
          async $ hive config hive'

contentType :: ResponseHeaders -> Maybe BS.ByteString
contentType = lookup hContentType

basePort :: Int
basePort = 8888
