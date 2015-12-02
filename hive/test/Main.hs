module Main 
    ( main
    ) where

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)

import HiveTests
import EndPointTests
import MatcherTests

main :: IO ()
main = defaultMain testSuite

testSuite :: [Test]
testSuite =
    [ testGroup "Hive Test"
        [ testCase "Test 500 if no endpoints" shallResp500NoEndPointTest
        ]    
    , testGroup "EndPoint Tests"
        [ testCase "Test get" getTest
        , testCase "Test post" postTest
        , testCase "Test put" putTest
        , testCase "Test delete" deleteTest
        , testCase "Test defaultRoute" defaultRouteTest
        , testCase "Test webSocket" webSocketTest
        , testCase "Test simple path" simplePathTest
        , testCase "Test simple capture" simpleCaptureTest
        , testCase "Test complex path" complexPathTest
        , testCase "Test guarded route" guardedHttpRouteTest
        , testCase "Test # end points" rightNumberOfEndPointsTest
        , testCase "Test separating end points" separateEndPointsTest
        ]
    , testGroup "Matcher Tests"
        [ testCase "Test match empty path" matchEmptyPathTest
        , testCase "Test match path of L1" matchL1PathTest
        , testCase "Test match path of L2" matchL2PathTest
        , testCase "Test match capture of L1" matchL1CaptureTest
        , testCase "Test match capture of L2" matchL2CaptureTest
        , testCase "Test match mixed of L3" matchL3MixedTest
        , testCase "Test length mismatch 1" mismatchEmptyLeftPathTest
        , testCase "Test length mismatch 2" mismatchEmptyRightPathTest
        , testCase "Test path mismatch" mismatchUnequalPathTest
        ]
    ]
