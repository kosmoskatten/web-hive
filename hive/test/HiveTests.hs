module HiveTests
    ( shallResp500NoEndPointTest
    ) where

import Data.ByteString.Lazy (ByteString)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Exception (bracket)
import Network.Hive (Hive, HiveConfig (..), defaultHiveConfig, hive)
import Network.HTTP.Types (Status (..))
import Test.HUnit
import Text.Printf

import qualified Network.HTTP.Client as C

-- | Access to a non-specified resource shall result in a 500/Internal
-- Server Error.
shallResp500NoEndPointTest :: Assertion
shallResp500NoEndPointTest = do
    let thePort = basePort
    withHive thePort theHive $ do
        (sc, _) <- get thePort "/"
        500 @=? sc
    where
       theHive :: Hive ()
       theHive = return ()

get :: Int -> String -> IO (Int, ByteString)
get p url = do
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
