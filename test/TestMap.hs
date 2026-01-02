{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE CPP, TemplateHaskell, ScopedTypeVariables #-}
module TestMap (mapTestGroup) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH

import qualified Data.Interval as Interval
import qualified Data.IntervalSet as IntervalSet

import TestIntervalSet ()
import qualified Data.Map.Lazy as LMap
import Data.Map.Lazy.Interval

lazyArbitraryKey :: LMap.Map k a -> Gen k
lazyArbitraryKey m = elements (LMap.keys m)

prop_member_interval_restriction :: Property
prop_member_interval_restriction =
  forAll arbitrary $ \(m' :: LMap.Map Rational Integer) ->
    -- map must be non empty to get an arbitrary key
    let m = LMap.insert 0 0 m' in
    forAll (lazyArbitraryKey m) $ \k ->
      forAll arbitrary $ \i ->
        Interval.member k i == LMap.member k (restrictInterval m i)

prop_restrictInterval_same_as_filterKeys :: Property
prop_restrictInterval_same_as_filterKeys =
  forAll arbitrary $ \(m :: LMap.Map Rational Integer) ->
    forAll arbitrary $ \i ->
      restrictInterval m i ===
        -- filterKeys is only in 0.8 so we use filterWithKey
        LMap.filterWithKey (\k _ -> Interval.member k i) m


prop_member_intervals_restriction :: Property
prop_member_intervals_restriction =
  forAll arbitrary $ \(m' :: LMap.Map Rational Integer) ->
    -- map must be non empty to get an arbitrary key
    let m = LMap.insert 0 0 m' in
    forAll (lazyArbitraryKey m) $ \k ->
      forAll arbitrary $ \is ->
        IntervalSet.member k is == LMap.member k (restrictIntervals m is)

prop_restrictIntervals_same_as_filterKeys :: Property
prop_restrictIntervals_same_as_filterKeys =
  forAll arbitrary $ \(m :: LMap.Map Rational Integer) ->
    forAll arbitrary $ \is ->
      restrictIntervals m is ===
        LMap.filterWithKey (\k _ -> IntervalSet.member k is) m

prop_not_member_interval_deletion :: Property
prop_not_member_interval_deletion =
  forAll arbitrary $ \(m' :: LMap.Map Rational Integer) ->
    -- map must be non empty to get an arbitrary key
    let m = LMap.insert 0 0 m' in
    forAll (lazyArbitraryKey m) $ \k ->
      forAll arbitrary $ \i ->
        Interval.member k i == LMap.notMember k (deleteInterval i m)

prop_deleteInterval_same_as_filterKeys :: Property
prop_deleteInterval_same_as_filterKeys =
  forAll arbitrary $ \(m :: LMap.Map Rational Integer) ->
    forAll arbitrary $ \i ->
      deleteInterval i m ===
        LMap.filterWithKey (\k _ -> not (Interval.member k i)) m

prop_not_member_intervals_deletion :: Property
prop_not_member_intervals_deletion =
  forAll arbitrary $ \(m' :: LMap.Map Rational Integer) ->
    -- map must be non empty to get an arbitrary key
    let m = LMap.insert 0 0 m' in
    forAll (lazyArbitraryKey m) $ \k ->
      forAll arbitrary $ \is ->
        IntervalSet.member k is == LMap.notMember k (deleteIntervals is m)

prop_deleteIntervals_same_as_filterKeys :: Property
prop_deleteIntervals_same_as_filterKeys =
  forAll arbitrary $ \(m :: LMap.Map Rational Integer) ->
    forAll arbitrary $ \is ->
      deleteIntervals is m ===
        LMap.filterWithKey (\k _ -> not (IntervalSet.member k is)) m

------------------------------------------------------------------------
-- Test harness

mapTestGroup :: TestTree
mapTestGroup = $(testGroupGenerator)
