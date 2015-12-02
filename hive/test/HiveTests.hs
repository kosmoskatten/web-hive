{-# LANGUAGE OverloadedStrings #-}
module HiveTests
    ( shallResp500NoEndPointTest
    , shallRouteTargetTest
    ) where

import Data.ByteString.Lazy (ByteString)
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
import Network.HTTP.Types (Status (..))
import Test.HUnit
import Text.Printf

import qualified Network.HTTP.Client as C

-- | Access to a non-existing route shall result in a 500/Internal
-- Server Error.
shallResp500NoEndPointTest :: Assertion
shallResp500NoEndPointTest = do
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
        resp1 <- httpGet thePort "/hello/world"
        resp2 <- httpGet thePort "/deep/purple"
        resp3 <- httpGet thePort "/just/random"
        resp4 <- httpGet thePort "/"
        resp5 <- httpGet thePort ""
        (200, "Hello, world!") @=? resp1
        (200, "Deep Purple")   @=? resp2
        (200, "default")       @=? resp3
        (200, "I'm root!")     @=? resp4
        (200, "I'm root!")     @=? resp5
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
                                     
httpGet :: Int -> String -> IO (Int, ByteString)
httpGet p url = do
    req     <- C.parseUrl $ printf "http://localhost:%d%s" p url
    let req' = req { C.checkStatus = \_ _ _ -> Nothing }
    manager <- C.newManager C.defaultManagerSettings
    resp    <- C.httpLbs req' manager
    return (statusCode $ C.responseStatus resp, C.responseBody resp)

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

basePort :: Int
basePort = 8888