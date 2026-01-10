module BenchInterval where

import Test.Tasty.Bench

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Interval
  ((<=..<=), (<..<), (<=..<), (<..<=), Interval, Extended(..))
import qualified Data.Interval as Interval
import qualified Data.IntervalSet as IntervalSet
import Data.IntervalSet (IntervalSet)

------------------------- Alternative implementations --------------------------

-- The following implementations are alternatives that were tried but are slower
-- than the implementations exported by the library.

-- same semantics as Interval.withoutKeysFromInterval
withoutKeysFromIntervalComplement ::  Ord k => Interval k -> Map k a -> Map k a
withoutKeysFromIntervalComplement is m =
  IntervalSet.restrictKeysToIntervals m $
            IntervalSet.complement (IntervalSet.singleton is)


------------------------- Example data for benchmarks --------------------------

minMap, maxMap :: Rational
minMap = 0
maxMap = 1000000

largeMap :: Map.Map Rational Rational
largeMap = Map.fromList [(i, i) | i <- [minMap .. maxMap]]

largeInterval :: Interval Rational
largeInterval = 250000 <=..<= 750000

pointInterval :: Interval Rational
pointInterval = 500000 <=..<= 500000

---------------------------------- Benchmarks ----------------------------------

benchRestrictKeysToInterval :: Ord k => Map k a -> Interval k -> [Benchmark]
benchRestrictKeysToInterval m i =
    [ bench "Interval.restrictKeysToInterval" $
      whnf (Interval.restrictKeysToInterval m) i
    , bench "Map.filterKeys" $
      whnf (Map.filterWithKey (\k _ -> Interval.member k i)) m
    ]

benchWithoutKeysFromInterval :: Ord k => Interval k -> Map k a -> [Benchmark]
benchWithoutKeysFromInterval i m =
  [ bench "Interval.withoutKeysFromInterval" $
      whnf (Interval.withoutKeysFromInterval i) m
  , bench "withoutKeysFromIntervalComplement" $
      whnf (withoutKeysFromIntervalComplement i) m
  , bench "Map.filterKeys" $
      whnf (Map.filterWithKey
          (\k _ -> not (Interval.member k i)))
        m
  ]

bgroupInterval :: Benchmark
bgroupInterval =
  bgroup "Interval"
    [
      bgroup "restrictKeysToInterval" $ benchRestrictKeysToInterval largeMap largeInterval,
      bgroup "withoutKeysFromInterval"
      [ bgroup "large interval" $
          benchWithoutKeysFromInterval largeInterval largeMap
      , bgroup "point interval" $
          benchWithoutKeysFromInterval pointInterval largeMap
      ]
    ]
