{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Map.Lazy.Interval
-- Copyright   :  (c) Alice Rixte 2025
-- License     :  BSD-style
--
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
--
-- Manipulate the keys of a "Map" using "Interval"s and "IntervalSet"s
-----------------------------------------------------------------------------

module Data.Map.Lazy.Interval
  ( module Map
  , restrictInterval
  , restrictIntervals
  , deleteInterval
  , deleteIntervals
  ) where

import Data.Map.Lazy as Map
import qualified Data.Interval.Internal as I
import Data.Interval.Internal (Interval(..))
import qualified Data.IntervalSet as IS
import Data.IntervalSet (IntervalSet)


-- | \(O(\log n)\). Restrict a 'Map' to the keys contained in a given
-- 'Interval'.
--
-- >>> restrictInterval m i == filterKeys (\k -> Interval.member k i) m
--
-- [Usage:]
--
-- >>> m = Map.fromList [(-2.5,0),(3.1,1),(5,2), (8.5,3)] :: Map Rational Int
-- >>> restrictInterval m (3 <=..< 8.5)
-- fromList [(31 % 10,1),(5 % 1,2)]
--
-- [Performance:]
-- This functions performs better than 'filterKeys' which is \(O(n)\).
--
restrictInterval :: Ord k => Map k a -> Interval k -> Map k a
restrictInterval m = \case
  Whole -> m
  Empty -> Map.empty
  Point k -> maybe Map.empty (Map.singleton k) (Map.lookup k m)
  LessThan k -> fst $ Map.split k m
  LessOrEqual k -> let (lt, eq, _) = Map.splitLookup k m in
    maybe lt (\a -> Map.insert k a lt) eq
  GreaterThan k -> snd $ Map.split k m
  GreaterOrEqual k -> let (_, eq, gt) = Map.splitLookup k m in
    maybe gt (\a -> Map.insert k a gt) eq
  BothClosed lk uk ->
    restrictInterval (restrictInterval m (I.GreaterOrEqual lk))
                                         (I.LessOrEqual uk)
  LeftOpen lk uk ->
    restrictInterval (restrictInterval m (I.GreaterThan lk))
                                         (I.LessOrEqual uk)
  RightOpen lk uk ->
    restrictInterval (restrictInterval m (I.GreaterOrEqual lk))
                                         (I.LessThan uk)
  BothOpen lk uk ->
    restrictInterval (restrictInterval m (I.GreaterThan lk))
                                         (I.LessThan uk)

-- | \(O(n \log n)\).
--
-- Restrict a 'Map' to the keys contained in a given 'Interval'.
--
-- >>> restrictIntervals m i == filterKeys (\k -> IntervalSet.member k i) m
--
-- [Usage:]
--
-- >>> m = Map.fromList [(-2.5,0),(3.1,1),(5,2), (8.5,3)] :: Map Rational Int
-- >>> restrictIntervals m (IntervalSet.fromList [ -inf <..<= 3, 8 <..< inf])
-- fromList [((-5) % 2,0),(17 % 2,3)]
--
-- [Performance:]
-- In most cases, this function performs better than 'filterKeys', especially
-- when the 'IntervalSet' contains few large intervals. However, in cases where
-- the 'IntervalSet' consists of many small intervals and covers a significant
-- portion of the map, 'filterKeys' may outperform 'restrictIntervals' (see
-- benchmark).
--
restrictIntervals :: Ord k => Map k a -> IntervalSet k -> Map k a
restrictIntervals m is = IS.foldr f Map.empty is
  where
    f i acc = Map.union acc (restrictInterval m i)

-- complexity proof:
--
-- This is not a tight bound, and a better analysis should be done.
--
-- Map.union is O(m log(n/m + 1)) where m <= n, and we know that all intevals
-- within the IntervalSet are disjoint. So the complexity is, provided that the
-- accumulator size is bigger than the current interval restriction size:
--
--   O( Σ m log(n/m + 1))
--
-- where i is the number of intervals in the IntervalSet, and n is the size of
-- the original map, and m is the size of the current interval restriction.
--
-- But log(n/m + 1) < log(n + 1) for all m >= 1, so we have
--
-- Σ m log(n/m + 1)
-- < Σ m log(n + 1)
-- = log(n + 1) Σ m
-- < n log(n + 1)
--
-- since the sum of sizes of all interval restrictions is less than n.


-- | \(O(n)\). Delete keys contained in a given 'Interval' from a 'Map'.
--
-- >>> deleteInterval i m == filterKeys (\k -> not (Interval.member k i)) m
--
-- [Usage:]
--
-- >>> m = Map.fromList [(-2.5,0),(3.1,1),(5,2), (8.5,3)] :: Map Rational Int
-- >>> deleteInterval (3 <=..< 8.5) m
-- fromList [((-5) % 2,0),(17 % 2,3)]
--
-- [Performance:] In practice, this function performs a lot better than
-- 'filterKeys' (see benchmark).
--
deleteInterval :: Ord k => Interval k -> Map k a -> Map k a
deleteInterval i m = restrictIntervals m (IS.complement (IS.singleton i))

-- | \(O(n \log n)\).
--
--  Delete keys contained in a given 'IntervalSet' from a 'Map'.
--
-- >>> deleteIntervals i m == filterKeys (\k -> not (IntervalSet.member k i)) m
--
-- [Usage:]
--
-- >>> m = Map.fromList [(-2.5,0),(3.1,1),(5,2), (8.5,3)] :: Map Rational Int
-- >>> deleteIntervals (IntervalSet.fromList [ -inf <..<= 3, 8 <..< inf]) m
-- fromList [(31 % 10,1),(5 % 1,2)]
--
-- See performance note of 'restrictIntervals'.
deleteIntervals :: Ord k => IntervalSet k -> Map k a -> Map k a
deleteIntervals is m = restrictIntervals m (IS.complement is)






