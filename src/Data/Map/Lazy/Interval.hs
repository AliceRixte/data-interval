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

-- | \(O(i n)\), where \(i\) is the 'IntervalSet' size and \(n\) is the 'Map'
-- size.
--
-- Restrict a 'Map' to the keys contained in a given 'Interval'.
restrictIntervals :: Ord k => Map k a -> IntervalSet k -> Map k a
restrictIntervals m is = IS.foldr f Map.empty is
  where
    f i acc = Map.union acc (restrictInterval m i)

-- | \(O(n)\). Delete keys contained in a given 'Interval' from a 'Map'.
deleteInterval :: Ord k => Interval k -> Map k a -> Map k a
deleteInterval i m = restrictIntervals m (IS.complement (IS.singleton i))

-- | \(O(i n)\), where \(i\) is the 'IntervalSet' size and \(n\) is the 'Map'
-- size.
--
--  Delete keys contained in a given 'IntervalSet' from a 'Map'.
deleteIntervals :: Ord k => IntervalSet k -> Map k a -> Map k a
deleteIntervals is m = restrictIntervals m (IS.complement is)






