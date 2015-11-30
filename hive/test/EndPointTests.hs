{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings        #-}
module EndPointTests
    ( getTest
    , postTest
    , putTest
    , deleteTest
    , defaultRouteTest
    , webSocketTest
    , simplePathTest
    , simpleCaptureTest
    , complexPathTest
    , guardedHttpRouteTest
    , rightNumberOfEndPointsTest
    , separateEndPointsTest
    ) where

import Network.Hive.Handler (respondText)
import Network.Hive.EndPoint ( Accept (..) 
                             , Hive
                             , HttpRoute (..)
                             , WsRoute (..)
                             , Path (..)
                             , runHive
                             , separateEndPoints
                             , accepts
                             , handledBy
                             , servedBy
                             , (</>)
                             , (</:>)
                             , get
                             , post
                             , put
                             , delete
                             , defaultRoute
                             , webSocket
                             , methodDefault
                             )
import Network.HTTP.Types ( methodGet
                          , methodPost
                          , methodPut
                          , methodDelete
                          )
import Test.HUnit

-- | Verify that the get function generate the expected data structure.
getTest :: Assertion
getTest = 
    HttpRoute { method = methodGet
              , path   = []
              } @=? get

-- | Verify that the post function generate the expected data structure.
postTest :: Assertion
postTest =
    HttpRoute { method = methodPost
              , path   = []
              } @=? post

-- | Verify that the put function generate the expected data structure.
putTest :: Assertion
putTest =
    HttpRoute { method = methodPut
              , path   = []
              } @=? put

-- | Verify that the delete function generate the expected data structure.
deleteTest :: Assertion
deleteTest =
    HttpRoute { method = methodDelete
              , path   = []
              } @=? delete

-- | Verify that the defaultRoute function generate the expected data
-- structure.
defaultRouteTest :: Assertion
defaultRouteTest =
    ( HttpRoute { method = methodDefault
                , path   = []
                }
    , Anything ) @=? defaultRoute


-- | Verify that the webSocket function generate the expected data
-- structure.
webSocketTest :: Assertion
webSocketTest = WsRoute [] @=? webSocket

-- | Build a simple path "/foo".
simplePathTest :: Assertion
simplePathTest =
    HttpRoute { method = methodGet
              , path   = [ Path "foo" ]
              } @=? get </> "foo"

-- | Build a simple capture path "/foo" where "foo" is a capture.
simpleCaptureTest :: Assertion
simpleCaptureTest =
    HttpRoute { method = methodGet
              , path   = [ Capture "foo" ]
              } @=? get </:> "foo"

-- | Build a more complex path "/foo/123/bar/456" where "123" and "456" are
-- captures.
complexPathTest :: Assertion
complexPathTest =
    HttpRoute { method = methodGet
              , path   = [ Path "foo", Capture "123"
                         , Path "bar", Capture "456" ]
              } @=? get </> "foo" </:> "123" </> "bar" </:> "456"

-- | Build a guarded http route.
guardedHttpRouteTest :: Assertion
guardedHttpRouteTest =
    (HttpRoute { method = methodGet
               , path   = [ Path "foo" ]
               }, Anything) @=? get </> "foo" `accepts` Anything

-- | Count the number of EndPoints in the hive. Shall be five.
rightNumberOfEndPointsTest :: Assertion
rightNumberOfEndPointsTest = 5 @=? (length $ runHive sampleHive)

-- | Separate the EndPoints.
separateEndPointsTest :: Assertion
separateEndPointsTest = do
    let endPoints = runHive sampleHive
        (hs, ws)  = separateEndPoints endPoints
    3 @=? length hs
    2 @=? length ws

-- | An example hive for testing.
sampleHive :: Hive ()
sampleHive = do
    get </> "http1" `accepts` Anything 
                    `handledBy` (respondText "")
    webSocket </> "webSocket1" `servedBy` return ()
    post </> "http2" `accepts` Anything
                     `handledBy` (respondText "")
    webSocket </> "webSocket2" `servedBy` return ()
    put </> "http3" `accepts` Anything
                    `handledBy` (respondText "")
