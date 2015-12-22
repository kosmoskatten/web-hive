{-# LANGUAGE OverloadedStrings #-}
module HiveTests
    ( shallResp500Test
    , shallRouteTargetTest
    , shallCaptureTest
    , shallReturnSingleQueryValueTest
    , shallReturnTwoQueryValuesTest
    , shallReturnManyQueryValuesTest
    , shallBeContentTypeTextTest
    , shallInvokeErrorHandlerTest
    , shallDifferentiateMethodsTest

    -- WebSocket specific tests
    , shallRouteWsTargetTest
    ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Exception (bracket)
import Data.Maybe (fromJust)
import Network.Hive ( Hive
                    , HttpMethod (..)
                    , Guard (..)
                    , HiveConfig (..)
                    , StatusCode (..)
                    , (</>), (</:>)
                    , (~~>)
                    , acceptRequest
                    , guardedBy
                    , capture
                    , defaultHiveConfig
                    , match
                    , matchAll
                    , handledBy
                    , hive
                    , receiveDataMessage
                    , respondText
                    , sendTextMessage
                    , servedBy
                    , queryValue
                    , queryValues
                    , webSocket
                    )
import Network.HTTP.Client (Response)
import Network.HTTP.Types ( Status (..)
                          , ResponseHeaders
                          , hContentType
                          , methodDelete
                          , methodPost
                          , methodPut
                          )
import Network.WebSockets (DataMessage (..))
import Test.HUnit
import Text.Printf

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import qualified Network.HTTP.Client as C
import qualified Network.WebSockets as WS

-- | Access to a non-existing route shall result in a 500/Internal
-- Server Error.
shallResp500Test :: Assertion
shallResp500Test = do
    let thePort = basePort
    withHive thePort theHive $ do
        (stat, resp) <- httpGet thePort "/"
        (500, "No handler found for: /") @=? (stat, C.responseBody resp)
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
          match GET </> "hello" </> "world"
                    `guardedBy` None
                    `handledBy` respondText Ok "Hello, world!"
          match GET </> "deep" </> "purple"
                    `guardedBy` None
                    `handledBy` respondText Ok "Deep Purple"
          match GET `guardedBy` None
                    `handledBy` respondText Ok "I'm root!"
          matchAll  `handledBy` respondText Ok "default"

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
          match GET </:> "word"
                    `guardedBy` None
                    `handledBy` do
                        word <- capture "word"
                        respondText Ok $ "You said " `mappend` word

          match GET </> "give" </:> "name" </> "a" </:> "fruit"
                    `guardedBy` None
                    `handledBy` do
                        name  <- capture "name"
                        fruit <- capture "fruit"
                        respondText Ok $
                            name `mappend` " want a " `mappend` fruit

-- | Test access of a single query value.
shallReturnSingleQueryValueTest :: Assertion
shallReturnSingleQueryValueTest = do
    let thePort = basePort + 3
    withHive thePort theHive $ do
        (stat, resp) <- httpGet thePort "?foo=bar"
        (200, "Got bar") @=? (stat, C.responseBody resp)
    where
      theHive :: Hive ()
      theHive = match GET `guardedBy` None
                          `handledBy` do
                              value <- fromJust <$> queryValue "foo"
                              respondText Ok $ "Got " `mappend` value

-- | Test access of two different query values.
shallReturnTwoQueryValuesTest :: Assertion
shallReturnTwoQueryValuesTest = do
    let thePort = basePort + 4
    withHive thePort theHive $ do
        (stat, resp) <- httpGet thePort "?foo=bar&fie=baz"
        (200, "Got bar baz") @=? (stat, C.responseBody resp)
    where
      theHive :: Hive ()
      theHive = match GET `guardedBy` None
                          `handledBy` do
                              value1 <- fromJust <$> queryValue "foo"
                              value2 <- fromJust <$> queryValue "fie"
                              respondText Ok $ "Got " `mappend` value1
                                                      `mappend` " "
                                                      `mappend` value2

-- | Test access of many values to the same query variable.
shallReturnManyQueryValuesTest :: Assertion
shallReturnManyQueryValuesTest = do
    let thePort = basePort + 5
    withHive thePort theHive $ do
        (stat, resp) <- httpGet thePort "?foo=bar&foo=fie"
        (200, "Got bar fie") @=? (stat, C.responseBody resp)
    where
      theHive :: Hive ()
      theHive = match GET `guardedBy` None
                          `handledBy` do
                              [value1, value2] <- queryValues "foo"
                              respondText Ok $ "Got " `mappend` value1
                                                      `mappend` " "
                                                      `mappend` value2

-- | Shall report content type as text/plain.
shallBeContentTypeTextTest :: Assertion
shallBeContentTypeTextTest = do
    let thePort = basePort + 6
    withHive thePort theHive $ do
        (stat, resp) <- httpGet thePort "/"
        (200, Just "text/plain") @=?
            (stat, contentType $ C.responseHeaders resp)
    where
      theHive :: Hive ()
      theHive = match GET `guardedBy` None `handledBy` respondText Ok ""

-- | Shall invoke the default error handler when the handler is throwing an
-- exception.
shallInvokeErrorHandlerTest :: Assertion
shallInvokeErrorHandlerTest = do
    let thePort = basePort + 7
    withHive thePort theHive $ do
        (stat, resp) <- httpGet thePort "/"
        (500, "divide by zero") @=? (stat, C.responseBody resp)
    where
      theHive :: Hive ()
      theHive = match GET `guardedBy` None `handledBy` do
          let str = show $ (1 :: Int) `div` 0
          respondText Ok $ T.pack str

-- | Shall differentiate on method on the same resource.
shallDifferentiateMethodsTest :: Assertion
shallDifferentiateMethodsTest = do
    let thePort = basePort + 8
    withHive thePort theHive $ do
        (stat1, resp1) <- httpGet thePort "/resource"
        (stat2, resp2) <- httpDelete thePort "/resource"
        (stat3, resp3) <- httpPost thePort "/resource" LBS.empty
        (stat4, resp4) <- httpPut thePort "/resource" LBS.empty
        (200, "GET")    @=? (stat1, C.responseBody resp1)
        (200, "DELETE") @=? (stat2, C.responseBody resp2)
        (201, "POST")   @=? (stat3, C.responseBody resp3)
        (201, "PUT")    @=? (stat4, C.responseBody resp4)
    where
      theHive :: Hive ()
      theHive = do
          match GET </> "resource"
                    `guardedBy` None
                    `handledBy` respondText Ok "GET"
          match DELETE </> "resource"
                       `guardedBy` None
                       `handledBy` respondText Ok "DELETE"
          match POST </> "resource"
                     `guardedBy` None
                     `handledBy` respondText Created "POST"
          match PUT </> "resource"
                    `guardedBy` None
                    `handledBy` respondText Created "PUT"

-- | Shall correctly route to WebSocket targets.
shallRouteWsTargetTest :: Assertion
shallRouteWsTargetTest = do
    let thePort = basePort + 9
    withHive thePort theHive $ do
        resp1 <-
          WS.runClient "localhost" thePort "/echo" $ \conn -> do
              WS.sendTextData conn ("hello" :: LBS.ByteString)
              WS.receiveDataMessage conn

        resp2 <-
          WS.runClient "localhost" thePort "/echo/reversed" $ \conn -> do
              WS.sendTextData conn ("hello" :: LBS.ByteString)
              WS.receiveDataMessage conn

        Text "hello" @=? resp1
        Text "olleh" @=? resp2
    where
      theHive :: Hive ()
      theHive = do
          webSocket </> "echo" `servedBy` do
            acceptRequest $ do
              msg <- receiveDataMessage
              case msg of
                Text tMsg -> sendTextMessage tMsg
                _         -> sendTextMessage "???"

          webSocket </> "echo" </> "reversed" ~~> do
            acceptRequest $ do
              msg <- receiveDataMessage
              case msg of
                Text tMsg -> sendTextMessage $ LBS.reverse tMsg
                _         -> sendTextMessage "???"

httpGet :: Int -> String -> IO (Int, Response LBS.ByteString)
httpGet p url = do
    req     <- C.parseUrl $ printf "http://localhost:%d%s" p url
    let req' = req { C.checkStatus = \_ _ _ -> Nothing }
    manager <- C.newManager C.defaultManagerSettings
    resp    <- C.httpLbs req' manager
    return (statusCode $ C.responseStatus resp, resp)

httpDelete :: Int -> String -> IO (Int, Response LBS.ByteString)
httpDelete p url = do
    req     <- C.parseUrl $ printf "http://localhost:%d%s" p url
    let req' = req { C.method      = methodDelete
                   , C.checkStatus = \_ _ _ -> Nothing
                   }
    manager <- C.newManager C.defaultManagerSettings
    resp    <- C.httpLbs req' manager
    return (statusCode $ C.responseStatus resp, resp)

httpPost :: Int -> String -> LBS.ByteString
         -> IO (Int, Response LBS.ByteString)
httpPost p url body = do
    req     <- C.parseUrl $ printf "http://localhost:%d%s" p url
    let req' = req { C.method      = methodPost
                   , C.checkStatus = \_ _ _ -> Nothing
                   , C.requestBody = C.RequestBodyLBS body
                   }
    manager <- C.newManager C.defaultManagerSettings
    resp    <- C.httpLbs req' manager
    return (statusCode $ C.responseStatus resp, resp)

httpPut :: Int -> String -> LBS.ByteString
        -> IO (Int, Response LBS.ByteString)
httpPut p url body = do
    req     <- C.parseUrl $ printf "http://localhost:%d%s" p url
    let req' = req { C.method      = methodPut
                   , C.checkStatus = \_ _ _ -> Nothing
                   , C.requestBody = C.RequestBodyLBS body
                   }
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
