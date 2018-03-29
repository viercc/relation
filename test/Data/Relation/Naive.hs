{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Data.Relation.Naive(
  -- * Naive but surely correct implementation.
  NaiveRel(..),
  -- * Queries
  null, size, member,
  lookupL, lookupR, dom, ran, domSize, ranSize,
  -- * Construction
  empty, singleton, identity, full,
  -- * Relation-algebraic operation
  union, difference, intersection, compose, transpose,
  -- * Operators
  (#.#), (.#), (#.), (-.#), (#.-),
  -- * Map/Filter
  firstMapMonotonic, secondMapMonotonic,
  firstBind, secondBind,
  filter, partition,
  -- * Conversion
  fromSet, toSet, fromList, toList,
  toAscList, fromAscList, fromDistinctAscList
) where

import           Prelude       hiding (filter, null)

import           Data.Set      (Set)
import qualified Data.Set      as Set

import           Control.Arrow (first, second)

import           Control.DeepSeq

newtype NaiveRel a b = NaiveRel { impl :: Set (a,b) }
  deriving (Show, Eq, Ord)

instance (NFData a, NFData b) => NFData (NaiveRel a b) where
  rnf r = rnf (impl r)

-- * Queries

null :: NaiveRel a b -> Bool
null r = Set.null (impl r)
size :: NaiveRel a b -> Int
size r = Set.size (impl r)
member :: (Ord a, Ord b) => a -> b -> NaiveRel a b -> Bool
member a b r = Set.member (a,b) (impl r)
lookupL :: (Ord a, Ord b) => a -> NaiveRel a b -> Set b
lookupL a r =
  Set.mapMonotonic snd $
  Set.takeWhileAntitone ((<= a) . fst) $
  Set.dropWhileAntitone ((< a) . fst) $
  impl r
lookupR :: (Ord a, Ord b) => NaiveRel a b -> b -> Set a
lookupR r b = Set.mapMonotonic fst $ Set.filter ((b ==) . snd) $ impl r
dom :: (Ord a) => NaiveRel a b -> Set a
dom r = Set.map fst (impl r)
ran :: (Ord b) => NaiveRel a b -> Set b
ran r = Set.map snd (impl r)
domSize :: (Ord a) => NaiveRel a b -> Int
domSize r = Set.size (dom r)
ranSize :: (Ord b) => NaiveRel a b -> Int
ranSize r = Set.size (ran r)

-- * Construction

empty :: NaiveRel a b
empty = NaiveRel Set.empty
singleton :: a -> b -> NaiveRel a b
singleton a b = NaiveRel (Set.singleton (a,b))
identity :: Set a -> NaiveRel a a
identity as = NaiveRel (Set.mapMonotonic (\a -> (a,a)) as)
full :: Set a -> Set b -> NaiveRel a b
full as bs = NaiveRel $ Set.fromDistinctAscList ps
  where
    ps = [ (a,b) | a <- Set.toAscList as, b <- Set.toAscList bs ]

-- * Relation-algebraic operation

union :: (Ord a, Ord b) => NaiveRel a b -> NaiveRel a b -> NaiveRel a b
union (NaiveRel r) (NaiveRel s) = NaiveRel (Set.union r s)
difference :: (Ord a, Ord b) => NaiveRel a b -> NaiveRel a b -> NaiveRel a b
difference (NaiveRel r) (NaiveRel s) = NaiveRel (Set.difference r s)
intersection :: (Ord a, Ord b) => NaiveRel a b -> NaiveRel a b -> NaiveRel a b
intersection (NaiveRel r) (NaiveRel s) = NaiveRel (Set.intersection r s)
compose :: (Ord a, Ord b, Ord c) => NaiveRel a b -> NaiveRel b c -> NaiveRel a c
compose (NaiveRel r) (NaiveRel s) = NaiveRel (Set.fromList t)
  where
    t = [ (a,c) | (a,b) <- Set.toAscList r, (b',c) <- Set.toAscList s, b==b' ]
transpose :: (Ord a, Ord b) => NaiveRel a b -> NaiveRel b a
transpose (NaiveRel r) = NaiveRel $ Set.map swap r
  where
    swap (a,b) = (b,a)

-- * Operators

-- | @= compose@
(#.#) :: (Ord a, Ord b, Ord c) => NaiveRel a b -> NaiveRel b c -> NaiveRel a c
(#.#) = compose

infixl 6 #.#

-- | @= lookupL@
(.#) :: (Ord a, Ord b) => a -> NaiveRel a b -> Set b
(.#) = lookupL

infix 6 .#

-- | @= lookupR@
(#.) :: (Ord a, Ord b) => NaiveRel a b -> b -> Set a
(#.) = lookupR

infix 6 #.

-- | Returns the unions of @a .# r@, for each element @a@ in given set.
(-.#) :: (Ord a, Ord b) => Set a -> NaiveRel a b -> Set b
as -.# r = ran (as' #.# r)
  where
    as' = full (Set.singleton ()) as

infix 6 -.#

-- | Returns the unions of @r #. b@, for each element @b@ in given set.
(#.-) :: (Ord a, Ord b) => NaiveRel a b -> Set b -> Set a
r #.- bs = dom (r #.# bs')
  where
    bs' = full bs (Set.singleton ())

infix 6 #.-


-- * Map/Filter
--
-- Here, a function @f@ is Monotonic means:
--
-- @forall x y. x < y ==> f x < f y@
--
-- To perform general map, you should use toList_ first,
-- then map and convert back by fromList_.

-- | O(a). The precondition is not checked.
firstMapMonotonic :: (a -> a') -> NaiveRel a b -> NaiveRel a' b
firstMapMonotonic f = NaiveRel . Set.mapMonotonic (first f) . impl

-- | O(n). The precondition is not checked.
secondMapMonotonic :: (b -> b') -> NaiveRel a b -> NaiveRel a b'
secondMapMonotonic f = NaiveRel . Set.mapMonotonic (second f) . impl

-- O(a \* n), where a is the domSize of output.
firstBind :: (Ord a, Ord b, Ord c) => (b -> Set a) -> NaiveRel b c -> NaiveRel a c
firstBind f (NaiveRel r) = NaiveRel $ Set.fromList s
  where
    s = [(a,c) | (b,c) <- Set.toList r
               , a <- Set.toList (f b)
               ]

-- O(n \* c), where c is the codSize of output.
secondBind :: (Ord a, Ord b, Ord c) => NaiveRel a b -> (b -> Set c) -> NaiveRel a c
secondBind (NaiveRel r) f = NaiveRel $ Set.fromList s
  where
    s = [(a,c) | (a,b) <- Set.toList r
               , c <- Set.toList (f b)
               ]
-- | O(n).
filter :: (a -> b -> Bool) -> NaiveRel a b -> NaiveRel a b
filter p = NaiveRel . Set.filter (uncurry p) . impl

-- | O(n).
partition :: (a -> b -> Bool) -> NaiveRel a b -> (NaiveRel a b, NaiveRel a b)
partition p (NaiveRel r) =
  let (r1, r2) = Set.partition (uncurry p) r
  in (NaiveRel r1, NaiveRel r2)

-- * Conversion

fromSet :: (Ord a, Ord b) => Set (a,b) -> NaiveRel a b
fromSet = NaiveRel

toSet :: (Ord a, Ord b) => NaiveRel a b -> Set (a,b)
toSet = impl

fromList :: (Ord a, Ord b) => [(a,b)] -> NaiveRel a b
fromList = NaiveRel . Set.fromList

toList :: NaiveRel a b -> [(a,b)]
toList = Set.toList . impl

fromAscList :: (Eq a, Eq b) => [(a,b)] -> NaiveRel a b
fromAscList = NaiveRel . Set.fromAscList

fromDistinctAscList :: [(a,b)] -> NaiveRel a b
fromDistinctAscList = NaiveRel . Set.fromDistinctAscList

toAscList :: NaiveRel a b -> [(a,b)]
toAscList (NaiveRel r) = Set.toAscList r
