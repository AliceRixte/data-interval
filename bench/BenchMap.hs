module BenchMap where

import Criterion

import qualified Data.Map.Lazy.Interval as LMap
import Data.Interval ((<=..<=), (<..<), (<=..<), (<..<=), Interval)
import qualified Data.Interval as Interval
import qualified Data.IntervalSet as IntervalSet

largeMap :: LMap.Map Rational Int
largeMap = LMap.fromList [(fromIntegral i, i) | i <- [1..1000000]]

largeInterval :: Interval Rational
largeInterval = 250000 <=..<= 750000

pointInterval :: Interval Rational
pointInterval = 500000 <=..<= 500000

largeIntervalSet :: IntervalSet.IntervalSet Rational
largeIntervalSet =
  IntervalSet.fromList
    [ 100000 <=..<= 200000
    , 300000 <..< 400000
    , 500000 <=..< 600000
    , 700000 <..<= 800000
    ]

manyIntervals :: IntervalSet.IntervalSet Rational
manyIntervals =
  IntervalSet.fromList
    [ fromIntegral i <=..<= fromIntegral i | i <- [100000..900000]
    ]

bgroupMap =
  bgroup "Map"
    [ bench "restrictInterval)" $
        whnf (LMap.restrictInterval largeMap) largeInterval

    , bench "restrict filterKeys" $
        whnf (LMap.filterWithKey (\k _ -> Interval.member k largeInterval))
          largeMap

    , bench "restrictIntervals (few big intervals)" $
        whnf (LMap.restrictIntervals largeMap) largeIntervalSet
    , bench "restrictIntervals filterKeys  (few big intervals)" $
        whnf (LMap.filterWithKey
                (\k _ -> IntervalSet.member k largeIntervalSet))
             largeMap
    , bench "restrictInterval (many small intervals)" $
        whnf (LMap.restrictIntervals largeMap) manyIntervals
    , bench "restrictIntervals filterKeys (many small intervals)" $
        whnf (LMap.filterWithKey
                (\k _ -> IntervalSet.member k manyIntervals))
          largeMap
    , bench "deleteInterval (large)" $
        whnf (LMap.deleteInterval largeInterval) largeMap
    , bench "deleteInterval filterKeys (large)" $
        whnf (LMap.filterWithKey
                (\k _ -> not (Interval.member k largeInterval)))
          largeMap
    , bench "deleteInterval (point)" $
        whnf (LMap.deleteInterval pointInterval) largeMap
    , bench "deleteInterval filterKeys (point)" $
        whnf (LMap.filterWithKey
                (\k _ -> not (Interval.member k pointInterval)))
          largeMap
    , bench "deleteIntervals" $
        whnf (LMap.deleteIntervals largeIntervalSet) largeMap
    , bench "deleteIntervals filterKeys" $
        whnf (LMap.filterWithKey
                (\k _ -> not (IntervalSet.member k largeIntervalSet)))
          largeMap
    ]


