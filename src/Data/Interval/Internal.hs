{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, LambdaCase, ScopedTypeVariables #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE RoleAnnotations #-}

module Data.Interval.Internal
  ( Boundary(..)
  , Interval
  , lowerBound'
  , upperBound'
  , interval
  , empty
  , restrictKeysToInterval
  , withoutKeysFromInterval
  , splitInterval
  , intersectInterval
  , differenceInterval
  , setSplitInterval
  ) where

import Control.DeepSeq
import Data.Data
import Data.ExtendedReal
import Data.Hashable
import Data.Int
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics (Generic)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

-- | Boundary of an interval may be
-- open (excluding an endpoint) or closed (including an endpoint).
--
-- @since 2.0.0
data Boundary
  = Open
  | Closed
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic, Data)

instance NFData Boundary

instance Hashable Boundary

-- | The intervals (/i.e./ connected and convex subsets) over a type @r@.
data Interval r
  = Whole
  | Empty
  | Point !r
  | LessThan !r
  | LessOrEqual !r
  | GreaterThan !r
  | GreaterOrEqual !r
  -- For constructors below
  -- the first argument is strictly less than the second one
  | BothClosed !r !r
  | LeftOpen !r !r
  | RightOpen !r !r
  | BothOpen !r !r
  deriving
    ( Eq
    , Ord
      -- ^ Note that this Ord is derived and not semantically meaningful.
      -- The primary intended use case is to allow using 'Interval'
      -- in maps and sets that require ordering.
    )

peekInterval :: (Applicative m, Monad m, Ord r) => m Int8 -> m r -> m r -> m (Interval r)
peekInterval tagM x y = do
  tag <- tagM
  case tag of
    0 -> pure Whole
    1 -> pure Empty
    2 -> Point           <$> x
    3 -> LessThan        <$> x
    4 -> LessOrEqual     <$> x
    5 -> GreaterThan     <$> x
    6 -> GreaterOrEqual  <$> x
    7 -> wrap BothClosed <$> x <*> y
    8 -> wrap LeftOpen   <$> x <*> y
    9 -> wrap RightOpen  <$> x <*> y
    _ -> wrap BothOpen   <$> x <*> y

-- | Enforce the internal invariant
-- of 'BothClosed' / 'LeftOpen' / 'RightOpen' / 'BothOpen'.
wrap :: Ord r => (r -> r -> Interval r) -> r -> r -> Interval r
wrap f x y
  | x < y = f x y
  | otherwise = Empty

pokeInterval :: Applicative m => (Int8 -> m ()) -> (r -> m ()) -> (r -> m ()) -> Interval r -> m ()
pokeInterval tag actX actY = \case
  Whole            -> tag (0 :: Int8)
  Empty            -> tag (1 :: Int8)
  Point          x -> tag (2 :: Int8) *> actX x
  LessThan       x -> tag (3 :: Int8) *> actX x
  LessOrEqual    x -> tag (4 :: Int8) *> actX x
  GreaterThan    x -> tag (5 :: Int8) *> actX x
  GreaterOrEqual x -> tag (6 :: Int8) *> actX x
  BothClosed   x y -> tag (7 :: Int8) *> actX x *> actY y
  LeftOpen     x y -> tag (8 :: Int8) *> actX x *> actY y
  RightOpen    x y -> tag (9 :: Int8) *> actX x *> actY y
  BothOpen     x y -> tag (10 :: Int8) *> actX x *> actY y

instance (Storable r, Ord r) => Storable (Interval r) where
  sizeOf _ = 3 * sizeOf (undefined :: r)
  alignment _ = alignment (undefined :: r)
  peek ptr = peekInterval
    (peek $ castPtr ptr)
    (peek $ castPtr ptr `advancePtr` 1)
    (peek $ castPtr ptr `advancePtr` 2)
  poke ptr = pokeInterval
    (poke $ castPtr ptr)
    (poke $ castPtr ptr `advancePtr` 1)
    (poke $ castPtr ptr `advancePtr` 2)

-- | Lower endpoint (/i.e./ greatest lower bound) of the interval,
-- together with 'Boundary' information.
-- The result is convenient to use as an argument for 'interval'.
lowerBound' :: Interval r -> (Extended r, Boundary)
lowerBound' = \case
  Whole            -> (NegInf,   Open)
  Empty            -> (PosInf,   Open)
  Point r          -> (Finite r, Closed)
  LessThan{}       -> (NegInf,   Open)
  LessOrEqual{}    -> (NegInf,   Open)
  GreaterThan r    -> (Finite r, Open)
  GreaterOrEqual r -> (Finite r, Closed)
  BothClosed p _   -> (Finite p, Closed)
  LeftOpen p _     -> (Finite p, Open)
  RightOpen p _    -> (Finite p, Closed)
  BothOpen p _     -> (Finite p, Open)

-- | Upper endpoint (/i.e./ least upper bound) of the interval,
-- together with 'Boundary' information.
-- The result is convenient to use as an argument for 'interval'.
upperBound' :: Interval r -> (Extended r, Boundary)
upperBound' = \case
  Whole            -> (PosInf,   Open)
  Empty            -> (NegInf,   Open)
  Point r          -> (Finite r, Closed)
  LessThan r       -> (Finite r, Open)
  LessOrEqual r    -> (Finite r, Closed)
  GreaterThan{}    -> (PosInf,   Open)
  GreaterOrEqual{} -> (PosInf,   Open)
  BothClosed _ q   -> (Finite q, Closed)
  LeftOpen _ q     -> (Finite q, Closed)
  RightOpen _ q    -> (Finite q, Open)
  BothOpen _ q     -> (Finite q, Open)

type role Interval nominal

instance (Ord r, Data r) => Data (Interval r) where
  gfoldl k z x   = z interval `k` lowerBound' x `k` upperBound' x
  toConstr _     = intervalConstr
  gunfold k z c  = case constrIndex c of
    1 -> k (k (z interval))
    _ -> error "gunfold"
  dataTypeOf _   = intervalDataType
  dataCast1 f    = gcast1 f

intervalConstr :: Constr
intervalConstr = mkConstr intervalDataType "interval" [] Prefix

intervalDataType :: DataType
intervalDataType = mkDataType "Data.Interval.Internal.Interval" [intervalConstr]

instance NFData r => NFData (Interval r) where
  rnf = \case
    Whole            -> ()
    Empty            -> ()
    Point r          -> rnf r
    LessThan r       -> rnf r
    LessOrEqual r    -> rnf r
    GreaterThan r    -> rnf r
    GreaterOrEqual r -> rnf r
    BothClosed p q   -> rnf p `seq` rnf q
    LeftOpen p q     -> rnf p `seq` rnf q
    RightOpen p q    -> rnf p `seq` rnf q
    BothOpen p q     -> rnf p `seq` rnf q

instance Hashable r => Hashable (Interval r) where
  hashWithSalt s = \case
    Whole            -> s `hashWithSalt`  (1 :: Int)
    Empty            -> s `hashWithSalt`  (2 :: Int)
    Point r          -> s `hashWithSalt`  (3 :: Int) `hashWithSalt` r
    LessThan r       -> s `hashWithSalt`  (4 :: Int) `hashWithSalt` r
    LessOrEqual r    -> s `hashWithSalt`  (5 :: Int) `hashWithSalt` r
    GreaterThan r    -> s `hashWithSalt`  (6 :: Int) `hashWithSalt` r
    GreaterOrEqual r -> s `hashWithSalt`  (7 :: Int) `hashWithSalt` r
    BothClosed p q   -> s `hashWithSalt`  (8 :: Int) `hashWithSalt` p `hashWithSalt` q
    LeftOpen p q     -> s `hashWithSalt`  (9 :: Int) `hashWithSalt` p `hashWithSalt` q
    RightOpen p q    -> s `hashWithSalt` (10 :: Int) `hashWithSalt` p `hashWithSalt` q
    BothOpen p q     -> s `hashWithSalt` (11 :: Int) `hashWithSalt` p `hashWithSalt` q

-- | empty (contradicting) interval
empty :: Ord r => Interval r
empty = Empty

-- | smart constructor for 'Interval'
interval
  :: (Ord r)
  => (Extended r, Boundary) -- ^ lower bound and whether it is included
  -> (Extended r, Boundary) -- ^ upper bound and whether it is included
  -> Interval r
interval = \case
  (NegInf, _) -> \case
    (NegInf, _) -> Empty
    (Finite r, Open) -> LessThan r
    (Finite r, Closed) -> LessOrEqual r
    (PosInf, _) -> Whole
  (Finite p, Open) -> \case
    (NegInf, _) -> Empty
    (Finite q, Open)
      | p < q -> BothOpen p q
      | otherwise -> Empty
    (Finite q, Closed)
      | p < q -> LeftOpen p q
      | otherwise -> Empty
    (PosInf, _) -> GreaterThan p
  (Finite p, Closed) -> \case
    (NegInf, _) -> Empty
    (Finite q, Open)
      | p < q -> RightOpen p q
      | otherwise -> Empty
    (Finite q, Closed) -> case p `compare` q of
      LT -> BothClosed p q
      EQ -> Point p
      GT -> Empty
    (PosInf, _) -> GreaterOrEqual p
  (PosInf, _) -> const Empty
{-# INLINE interval #-}

-- internal helper  for splitInterval
maybeInsert :: Ord k => k -> Maybe a -> Map k a -> Map k a
maybeInsert = maybe id . Map.insert

-- | \(O(\log n)\). Split a 'Map' into three 'Map's, such that, respectively,
--
-- 1. the keys are less than the interval
-- 2. the keys are contained in the interval
-- 3. the keys are greater than the interval
--
-- [Usage:]
--
-- >>> m = Map.fromList [(-2.5,0),(3.1,1),(5,2), (8.5,3)] :: Map Rational Int
-- >>> splitInterval (3 <=..< 8.5) m
-- (fromList [((-5) % 2,0)]
-- ,fromList [(31 % 10,1),(5 % 1,2)]
-- ,fromList [(17 % 2,3)]
-- )
--
splitInterval :: Ord k
  => Interval k -> Map k a -> (Map k a, Map k a, Map k a)
splitInterval i m = case i of
  Whole -> (Map.empty, m, Map.empty)
  Empty -> (m, Map.empty, m)
  Point k -> let (lt, eq, gt) = Map.splitLookup k m in
    (lt, maybe Map.empty (Map.singleton k) eq, gt)
  LessThan k -> let (lt, eq, gt) = Map.splitLookup k m in
    (Map.empty, lt, maybeInsert k eq gt)
  LessOrEqual k -> let (lt, eq, gt) = Map.splitLookup k m in
    (Map.empty, maybeInsert k eq lt, gt)
  GreaterThan k -> let (lt, eq, gt) = Map.splitLookup k m in
    (maybeInsert k eq lt, gt, mempty)
  GreaterOrEqual k -> let (lt, eq, gt) = Map.splitLookup k m in
    (lt, maybeInsert k eq gt, mempty)
  BothClosed lk uk ->
    let (lt, lb, gt) = Map.splitLookup lk m
        (lt', ub, gt') = Map.splitLookup uk gt
    in
    (lt, maybeInsert lk lb (maybeInsert uk ub lt'), gt')
  LeftOpen lk uk ->
    let (lt, lb, gt) = Map.splitLookup lk m
        (lt', ub, gt') = Map.splitLookup uk gt
    in
    (maybeInsert lk lb lt, maybeInsert uk ub lt', gt')
  RightOpen lk uk ->
    let (lt, lb, gt) = Map.splitLookup lk m
        (lt', ub, gt') = Map.splitLookup uk gt
    in
    (lt, maybeInsert lk lb lt', maybeInsert uk ub gt')
  BothOpen lk uk ->
    let (lt, lb, gt) = Map.splitLookup lk m
        (lt', ub, gt') = Map.splitLookup uk gt
    in
    (maybeInsert lk lb lt, lt', maybeInsert uk ub gt')

-- | \(O(\log n)\). Restrict a 'Map' to the keys contained in a given
-- 'Interval'.
--
-- >>> restrictKeysToInterval m i == filterKeys (\k -> Interval.member k i) m
--
-- [Usage:]
--
-- >>> m = Map.fromList [(-2.5,0),(3.1,1),(5,2), (8.5,3)] :: Map Rational Int
-- >>> restrictKeysToInterval m (3 <=..< 8.5)
-- fromList [(31 % 10,1),(5 % 1,2)]
--
-- [Performance:]
-- This outperforms 'filterKeys' which is \(O(n)\).
--
restrictKeysToInterval :: Ord k => Map k a -> Interval k -> Map k a
restrictKeysToInterval m i = let (_, mid , _) = splitInterval i m in mid


-- internal helper for withoutKeysFromInterval
withoutKeysFromIntervalSplitUnion :: Ord k => Interval k -> Map k a -> Map k a
withoutKeysFromIntervalSplitUnion i m = lt `Map.union` rt
  where (lt, _, rt) = splitInterval i m

-- | \(O(n)\). Delete keys contained in a given 'Interval' from a 'Map'.
--
-- >>> withoutKeysFromInterval i m == filterKeys (\k -> Interval.notMember k i) m
--
-- [Usage:]
--
-- >>> m = Map.fromList [(-2.5,0),(3.1,1),(5,2), (8.5,3)] :: Map Rational Int
-- >>> withoutKeysFromInterval (3 <=..< 8.5) m
-- fromList [((-5) % 2,0),(17 % 2,3)]
--
-- [Performance:] In practice, this outperforms 'filterKeys'.
--
withoutKeysFromInterval :: Ord k => Interval k -> Map k a -> Map k a
withoutKeysFromInterval i m = case i of
  Whole -> Map.empty
  Empty -> m
  Point k -> Map.delete k m
  LessThan k -> restrictKeysToInterval m (GreaterOrEqual k)
  LessOrEqual k -> restrictKeysToInterval m (GreaterThan k)
  GreaterThan k -> restrictKeysToInterval m (LessOrEqual k)
  GreaterOrEqual k -> restrictKeysToInterval m (LessThan k)
  _ -> withoutKeysFromIntervalSplitUnion i m


------------------------------------------------------------------------------

-- | \(O(\log n)\). Restrict a 'Set' to the keys contained in a given
-- 'Interval'.
--
-- >>> intersectInterval s i == Set.filter (\k -> Interval.member k i) s
--
-- [Usage:]
--
-- >>> s = Set.fromList [-2.5, 3.1, 5 , 8.5] :: Set Rational
-- >>> intersectInterval s (3 <=..< 8.5)
-- fromList [31 % 10,5 % 1]
--
-- [Performance:] This outperforms 'Set.filter' which is \(O(n)\).
--
intersectInterval :: Ord k => Set k -> Interval k -> Set k
intersectInterval s i = let (_, mid , _) = setSplitInterval i s in mid

-- internal helper for setSplitInterval
insertWhenFound :: Ord k => Bool -> k -> Set k -> Set k
insertWhenFound False _ s = s
insertWhenFound True  k s = Set.insert k s

-- | \(O(\log n)\). Split a 'Set' into three 'Set's, such that, respectively,
--
-- 1. the keys are less than the interval
-- 2. the keys are contained in the interval
-- 3. the keys are greater than the interval
--
-- [Usage:]
--
-- >>> s = Set.fromList [-2.5, 3.1, 5 , 8.5] :: Set Rational
-- >>> setSplitInterval (3 <=..< 8.5) s
--  (fromList [(-5) % 2]
--  ,fromList [31 % 10,5 % 1]
--  ,fromList [17 % 2]
--  )
setSplitInterval :: Ord k
  => Interval k -> Set k -> (Set k, Set k, Set k)
setSplitInterval i s = case i of
  Whole -> (Set.empty, s, Set.empty)
  Empty -> (s, Set.empty, s)
  Point k -> let (lt, eq, gt) = Set.splitMember k s in
    (lt, if eq then Set.singleton k else Set.empty, gt)
  LessThan k -> let (lt, eq, gt) = Set.splitMember k s in
    (Set.empty, lt, insertWhenFound eq k gt)
  LessOrEqual k -> let (lt, eq, gt) = Set.splitMember k s in
    (Set.empty, insertWhenFound eq k lt, gt)
  GreaterThan k -> let (lt, eq, gt) = Set.splitMember k s in
    (insertWhenFound eq k lt, gt, Set.empty)
  GreaterOrEqual k -> let (lt, eq, gt) = Set.splitMember k s in
    (lt, insertWhenFound eq k gt, Set.empty)
  BothClosed lk uk ->
    let (lt, lb, gt) = Set.splitMember lk s
        (lt', ub, gt') = Set.splitMember uk gt
    in
    (lt, insertWhenFound lb lk (insertWhenFound ub uk lt'), gt')
  LeftOpen lk uk ->
    let (lt, lb, gt) = Set.splitMember lk s
        (lt', ub, gt') = Set.splitMember uk gt
    in
    (insertWhenFound lb lk lt, insertWhenFound ub uk lt', gt')
  RightOpen lk uk ->
    let (lt, lb, gt) = Set.splitMember lk s
        (lt', ub, gt') = Set.splitMember uk gt
    in
    (lt, insertWhenFound lb lk lt', insertWhenFound ub uk gt')
  BothOpen lk uk ->
    let (lt, lb, gt) = Set.splitMember lk s
        (lt', ub, gt') = Set.splitMember uk gt
    in
    (insertWhenFound lb lk lt, lt', insertWhenFound ub uk gt')

-- internal helper for differenceInterval
differenceIntervalSplitUnion :: Ord k => Set k -> Interval k -> Set k
differenceIntervalSplitUnion s i = lt `Set.union` rt
  where (lt, _, rt) = setSplitInterval i s

-- | \(O(n)\). Delete keys contained in a given 'Interval' from a 'Set'.
--
-- >>> differenceInterval i s == Set.filter (\k -> Interval.notMember k i) s
--
-- [Usage:]
--
-- >>> s = Set.fromList [-2.5, 3.1, 5 , 8.5] :: Set Rational
-- >>> differenceInterval s (3 <=..< 8.5)
-- fromList [(-5) % 2,17 % 2]
--
-- [Performance:] In practice, this outperforms 'Set.filter'.
--
differenceInterval :: Ord k => Set k -> Interval k -> Set k
differenceInterval s i = case i of
  Whole -> Set.empty
  Empty -> s
  Point k -> Set.delete k s
  LessThan k -> intersectInterval s (GreaterOrEqual k)
  LessOrEqual k -> intersectInterval s (GreaterThan k)
  GreaterThan k -> intersectInterval s (LessOrEqual k)
  GreaterOrEqual k -> intersectInterval s (LessThan k)
  _ -> differenceIntervalSplitUnion s i
