module BenchIntervalSet where

import Test.Tasty.Bench

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Interval
  ((<=..<=), (<..<), (<=..<), (<..<=), Interval, Extended(..))
import qualified Data.Interval as Interval
import qualified Data.IntervalSet as IntervalSet
import Data.IntervalSet (IntervalSet)

------------------------- Alternative implementations --------------------------

-- The following implementations are alternatives that were tried but are slower
-- than the implementations exported by the library.

-- same semantics as IntervalSet.restrictKeysToIntervals
restrictKeysToIntervalsSplit :: Ord k => Map k a -> IntervalSet k -> Map k a
restrictKeysToIntervalsSplit m is = snd $ IntervalSet.foldr f (m, Map.empty) is
  where
  f i (lt, acc) =
    let (lti, mi, _) = Interval.splitInterval i lt in
    (lti, Map.union acc mi)

-- same semantics as IntervalSet.restrictKeysToIntervals
restrictKeysToIntervalsFoldr :: Ord k => Map k a -> IntervalSet k -> Map k a
restrictKeysToIntervalsFoldr m is =
    let f i acc = Map.union acc (Interval.restrictKeysToInterval m i) in
    IntervalSet.foldr f Map.empty is


-- same semantics as IntervalSet.withoutKeysFromIntervals
withoutKeysFromIntervalsComplement :: Ord k
  => IntervalSet k -> Map k a -> Map k a
withoutKeysFromIntervalsComplement is m =
  IntervalSet.foldr Interval.withoutKeysFromInterval m is

-- same semantics as IntervalSet.intersectIntervals
intersectIntervalsFoldr :: Ord k
  => Set k -> IntervalSet k -> Set k
intersectIntervalsFoldr s is =
    let f i acc = Set.union acc (Interval.intersectInterval s i) in
    IntervalSet.foldr f Set.empty is


------------------------- Example data for benchmarks --------------------------

minMap, maxMap :: Rational
minMap = 0
maxMap = 1000000

largeMap :: Map Rational Rational
largeMap = Map.fromList [(i, i) | i <- [minMap .. maxMap]]

largeSet :: Set Rational
largeSet = Set.fromList [minMap .. maxMap]


alternate ::
  Rational -> -- min
  Rational -> -- max
  Int ->      -- number of parts
  IntervalSet Rational
alternate minV maxV n =
  let step = (maxV - minV) / fromIntegral n in
  IntervalSet.fromList
  [ Finite (minV + step * fromIntegral i)
    Interval.<..<=
    Finite (minV + step * (fromIntegral i + 1 / 2))
  | i <- [0..n-1]]

---------------------------------- Benchmarks ----------------------------------

benchRestrictKeysToInterval :: Ord k => Map k a -> Interval k -> [Benchmark]
benchRestrictKeysToInterval m i =
    [ bench "Interval.restrictKeysToInterval" $
      whnf (Interval.restrictKeysToInterval m) i
    , bench "Map.filterKeys" $
      whnf (Map.filterWithKey (\k _ -> Interval.member k i)) m
    ]

benchRestrictKeysToIntervals ::
  Ord k => Bool -> Map k a -> IntervalSet k -> [Benchmark]
benchRestrictKeysToIntervals b m i =
  [ bench "IntervalSet.restrictKeysToIntervals" $
      whnf (IntervalSet.restrictKeysToIntervals m) i
  , bench "Map.filterKeys" $ whnf (Map.filterWithKey
              (\k _ -> IntervalSet.member k i)) m
  , bench "restrictKeysToIntervalsFoldr" $ whnf (restrictKeysToIntervalsFoldr m) i
  ] ++
  if b then
    [bench "restrictKeysToIntervalsSplit" $
      whnf (restrictKeysToIntervalsSplit m) i]
  else []

foldFilterThreshold :: Float
foldFilterThreshold = 0.25

benchRestrictKeysToIntervalsPercentage ::
     Float
  -> Benchmark
benchRestrictKeysToIntervalsPercentage a =
  bgroup ("num of intervals / map size = " ++ show a) $
    benchRestrictKeysToIntervals
       (a < foldFilterThreshold)
    -- only activate splitIntervals below the foldFilterThreshold
    -- as it gets way too slow above the threhold
       largeMap
      (alternate minMap maxMap (round numberOfIntervals))
  where
    numberOfIntervals = (maxMap - minMap) * toRational a

benchWithoutKeysFromIntervals :: Ord k => IntervalSet k -> Map k a -> [Benchmark]
benchWithoutKeysFromIntervals is m =
  [ bench "IntervalSet.withoutKeysFromIntervals" $
      whnf (IntervalSet.withoutKeysFromIntervals is) m
  , bench "withoutKeysFromIntervalsComplement" $
      whnf (withoutKeysFromIntervalsComplement is) m
  , bench "Map.filterKeys" $
      whnf (Map.filterWithKey
          (\k _ -> not (IntervalSet.member k is)))
        m
  ]

benchWithoutKeysFromIntervalsPercentage ::
     Float -- ^  number of intervals / map size
  -> Benchmark
benchWithoutKeysFromIntervalsPercentage n =
  bgroup ("num of intervals / map size = " ++ show n ) $
    benchWithoutKeysFromIntervals
      (alternate minMap maxMap (round numberOfIntervals)) largeMap
  where
    numberOfIntervals = (maxMap - minMap) * toRational n


benchIntersectIntervals :: Ord k => Set k -> IntervalSet k ->  [Benchmark]
benchIntersectIntervals s is =
  [ bench "IntervalSet.intersectIntervals" $
      whnf (IntervalSet.intersectIntervals s) is
  , bench "intersectIntervalsFoldr" $
      whnf (intersectIntervalsFoldr s) is
  ]

benchIntersectIntervalsPercentage ::
     Float -- ^  number of intervals / set size
  -> Benchmark
benchIntersectIntervalsPercentage a =
  bgroup ("num of intervals / set size = " ++ show a ) $
    benchIntersectIntervals largeSet
      (alternate minMap maxMap (round numberOfIntervals))
  where
    numberOfIntervals = (maxMap - minMap) * toRational a

-- these percentages aims to determine the fold/filter threshold when filtering
-- starts to be more efficient than folding
intervalPercentages :: [Float]
intervalPercentages =
     [10 ^^ n | n <- [-6 .. -2]]
  ++ [n/10 | n <- [1 .. 10]]
  ++ [2,4]

bgroupIntervalSet :: Benchmark
bgroupIntervalSet =
  bgroup "IntervalSet"
    [
      bgroup "withoutKeysFromIntervals"
        [ benchWithoutKeysFromIntervalsPercentage (10 ^^ n) | n <- [-6 .. 0]],
      bgroup "restrictKeysToIntervals" $
        benchRestrictKeysToIntervalsPercentage <$> intervalPercentages,
      bgroup "intersectIntervals" $
        benchIntersectIntervalsPercentage <$> intervalPercentages
    ]
