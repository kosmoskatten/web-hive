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
import Network.Hive.EndPoint ( Guard (..) 
                             , Hive
                             , HttpMethod (..)
                             , HttpRoute (..)
                             , WsRoute (..)
                             , Path (..)
                             , runHive
                             , separateEndPoints
                             , guardedBy
                             , handledBy
                             , servedBy
                             , (</>)
                             , (</:>)
                             , match
                             , matchAll
                             , webSocket
                             , methodMatchAll
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
              } @=? match GET

-- | Verify that the post function generate the expected data structure.
postTest :: Assertion
postTest =
    HttpRoute { method = methodPost
              , path   = []
              } @=? match POST

-- | Verify that the put function generate the expected data structure.
putTest :: Assertion
putTest =
    HttpRoute { method = methodPut
              , path   = []
              } @=? match PUT

-- | Verify that the delete function generate the expected data structure.
deleteTest :: Assertion
deleteTest =
    HttpRoute { method = methodDelete
              , path   = []
              } @=? match DELETE

-- | Verify that the defaultRoute function generate the expected data
-- structure.
defaultRouteTest :: Assertion
defaultRouteTest =
    ( HttpRoute { method = methodMatchAll
                , path   = []
                }
    , None ) @=? matchAll


-- | Verify that the webSocket function generate the expected data
-- structure.
webSocketTest :: Assertion
webSocketTest = WsRoute [] @=? webSocket

-- | Build a simple path "/foo".
simplePathTest :: Assertion
simplePathTest =
    HttpRoute { method = methodGet
              , path   = [ Path "foo" ]
              } @=? match GET </> "foo"

-- | Build a simple capture path "/foo" where "foo" is a capture.
simpleCaptureTest :: Assertion
simpleCaptureTest =
    HttpRoute { method = methodGet
              , path   = [ Capture "foo" ]
              } @=? match GET </:> "foo"

-- | Build a more complex path "/foo/123/bar/456" where "123" and "456" are
-- captures.
complexPathTest :: Assertion
complexPathTest =
    HttpRoute { method = methodGet
              , path   = [ Path "foo", Capture "123"
                         , Path "bar", Capture "456" ]
              } @=? match GET </> "foo" </:> "123" </> "bar" </:> "456"

-- | Build a guarded http route.
guardedHttpRouteTest :: Assertion
guardedHttpRouteTest =
    (HttpRoute { method = methodGet
               , path   = [ Path "foo" ]
               }, None) @=? match GET </> "foo" `guardedBy` None

-- | Count the number of EndPoints in the hive. Shall be five.
rightNumberOfEndPointsTest :: Assertion
rightNumberOfEndPointsTest = 5 @=? length (runHive sampleHive)

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
    match GET </> "http1" `guardedBy` None 
                          `handledBy` respondText ""
    webSocket </> "webSocket1" `servedBy` return ()
    match POST </> "http2" `guardedBy` None
                           `handledBy` respondText ""
    webSocket </> "webSocket2" `servedBy` return ()
    match PUT </> "http3" `guardedBy` None
                          `handledBy` respondText ""
