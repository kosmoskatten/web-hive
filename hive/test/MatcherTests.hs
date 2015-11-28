{-# LANGUAGE OverloadedStrings #-}
module MatcherTests
    ( matchEmptyPathTest
    , matchL1PathTest
    , matchL2PathTest
    , matchL1CaptureTest
    , matchL2CaptureTest
    , matchL3MixedTest
    , mismatchEmptyLeftPathTest
    , mismatchEmptyRightPathTest
    , mismatchUnequalPathTest
    ) where

import Network.Hive.EndPoint (Path (..))
import Network.Hive.Matcher (matchRequestPath)
import Test.HUnit

import qualified Data.Map.Lazy as Map

-- | A match with empty lists on both sides shall match, and give
-- a resulting empty CaptureMap as result.
matchEmptyPathTest :: Assertion
matchEmptyPathTest = do
    let Just match = matchRequestPath [] []
    0 @=? Map.size match

-- | Match a path of length 1. No captures.
matchL1PathTest :: Assertion
matchL1PathTest = do
    let Just match = matchRequestPath ["foo"] [Path "foo"]
    0 @=? Map.size match

-- | Match a path of length 2. No captures.
matchL2PathTest :: Assertion
matchL2PathTest = do
    let Just match = matchRequestPath ["foo", "bar"]
                                      [Path "foo", Path "bar"]
    0 @=? Map.size match

-- | Match a path of length 1. One capture.
matchL1CaptureTest :: Assertion
matchL1CaptureTest = do
    let Just match = matchRequestPath ["foo"] [Capture "cap1"]
    1 @=? Map.size match
    Just "foo" @=? Map.lookup "cap1" match

-- | Match a path of length 2. Two captures.
matchL2CaptureTest :: Assertion
matchL2CaptureTest = do
    let Just match = matchRequestPath ["foo", "bar"]
                                      [Capture "cap1", Capture "cap2"]
    2 @=? Map.size match
    Just "foo" @=? Map.lookup "cap1" match
    Just "bar" @=? Map.lookup "cap2" match

-- | Match a path of lenght 3. One capture, in the middle of the path.
matchL3MixedTest :: Assertion
matchL3MixedTest = do
    let Just match = matchRequestPath ["foo", "123", "bar"]
                                      [ Path "foo"
                                      , Capture "cap1"
                                      , Path "bar" ]
    1 @=? Map.size match
    Just "123" @=? Map.lookup "cap1" match

-- | Lists of unequal length shall always mismatch. In this case it's empty
-- on the left side.
mismatchEmptyLeftPathTest :: Assertion
mismatchEmptyLeftPathTest = Nothing @=? matchRequestPath [] [Path "foo"]

-- | Lists of unequal length shall always mismatch. In this case it's empty
-- on the right side.
mismatchEmptyRightPathTest :: Assertion
mismatchEmptyRightPathTest = Nothing @=? matchRequestPath ["foo"] []

-- | List of equal length, but with mismatching path. No captures.
mismatchUnequalPathTest :: Assertion
mismatchUnequalPathTest = do
    let match = matchRequestPath ["foo"] [Path "bar"]
    Nothing @=? match
