{-# LANGUAGE OverloadedStrings #-}
module HiveTests
    ( shallResp500Test
    , shallRouteTargetTest
    , shallCaptureTest
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
        (stat1, body1) <- httpGet thePort "/hello/world"
        (stat2, body2) <- httpGet thePort "/deep/purple"
        (stat3, body3) <- httpGet thePort "/just/random"
        (stat4, body4) <- httpGet thePort "/"
        (stat5, body5) <- httpGet thePort ""
        (200, "Hello, world!") @=? (stat1, C.responseBody body1)
        (200, "Deep Purple")   @=? (stat2, C.responseBody body2)
        (200, "default")       @=? (stat3, C.responseBody body3)
        (200, "I'm root!")     @=? (stat4, C.responseBody body4)
        (200, "I'm root!")     @=? (stat5, C.responseBody body5)
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
                  
shallCaptureTest :: Assertion
shallCaptureTest = do
    let thePort = basePort + 2
    withHive thePort theHive $ do
        (stat1, body1) <- httpGet thePort "/anything"
        (stat2, body2) <- httpGet thePort "/give/tarzan/a/banana"
        (200, "You said anything")    @=? (stat1, C.responseBody body1)
        (200, "tarzan want a banana") @=? (stat2, C.responseBody body2)
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
