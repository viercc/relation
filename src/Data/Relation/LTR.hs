{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Data.Relation.LTR(
  -- * Relation type
  Rel(),
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
  toAscList, fromAscList, fromDistinctAscList,
  fromMap
) where

import           Prelude                     hiding (filter, null)

import           Data.Foldable               (fold)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Set                    (Set)
import qualified Data.Set                    as Set

import           Data.Relation.Internal.Bare

newtype Rel a b = Rel { impl :: Rel_ a b }
                deriving (Eq, Ord)

instance (Show a, Show b) => Show (Rel a b) where
    showsPrec p (Rel r) = showsPrec_ p (toList_ r)

-- * Queries

-- | O(1)
null :: Rel a b -> Bool
null r = Map.null (impl r)
-- | O(a)
size :: Rel a b -> Int
size r = size_ (impl r)
-- | O(log (a \* b))
member :: (Ord a, Ord b) => a -> b -> Rel a b -> Bool
member a b r = member_ a b (impl r)
-- | O(log a)
lookupL :: (Ord a, Ord b) => a -> Rel a b -> Set b
lookupL a r = slice_ a (impl r)
-- | O(a \* log b)
lookupR :: (Ord a, Ord b) => Rel a b -> b -> Set a
lookupR r b = revslice_ (impl r) b
-- | O(a)
dom :: (Ord a) => Rel a b -> Set a
dom r = Map.keysSet (impl r)
-- | O(n \* log n)
ran :: (Ord b) => Rel a b -> Set b
ran r = fold (impl r)
-- | O(1)
domSize :: (Ord a) => Rel a b -> Int
domSize r = Map.size (impl r)
-- | O(n \* log n)
ranSize :: (Ord b) => Rel a b -> Int
ranSize r = Set.size (ran r)

-- * Construction

-- | O(1)
empty :: Rel a b
empty = Rel Map.empty
-- | O(1)
singleton :: a -> b -> Rel a b
singleton a b = Rel (Map.singleton a (Set.singleton b))
-- | O(a) (n == a)
identity :: Set a -> Rel a a
identity as = Rel (Map.fromSet Set.singleton as)
-- | O(a)
full :: Set a -> Set b -> Rel a b
full as bs = Rel (Map.fromSet (const bs) as)

-- * Relation-algebraic operation

-- | O(n1 + n2)
union :: (Ord a, Ord b) => Rel a b -> Rel a b -> Rel a b
union (Rel r) (Rel s) = Rel (union_ r s)
-- | O(n1 + n2)
difference :: (Ord a, Ord b) => Rel a b -> Rel a b -> Rel a b
difference (Rel r) (Rel s) = Rel (difference_ r s)
-- | O(n1 + n2)
intersection :: (Ord a, Ord b) => Rel a b -> Rel a b -> Rel a b
intersection (Rel r) (Rel s) = Rel (intersection_ r s)
-- | O(n + a \* b' \* c' \* log b')
--
--   where
--   >   n = size r
--   >   a = domSize r
--   >   b' = domSize s
--   >   c' = ranSize s
compose :: (Ord a, Ord b, Ord c) => Rel a b -> Rel b c -> Rel a c
compose (Rel r) (Rel s) = Rel (compose_ r s)

-- | O(n \* log a)
transpose :: (Ord a, Ord b) => Rel a b -> Rel b a
transpose (Rel r) = Rel (transpose_ r)

-- * Operators

-- | @= compose@
(#.#) :: (Ord a, Ord b, Ord c) => Rel a b -> Rel b c -> Rel a c
(#.#) = compose

infixl 6 #.#

-- | @= lookupL@
(.#) :: (Ord a, Ord b) => a -> Rel a b -> Set b
(.#) = lookupL

infix 6 .#

-- | @= lookupR@
(#.) :: (Ord a, Ord b) => Rel a b -> b -> Set a
(#.) = lookupR

infix 6 #.

-- | Returns the unions of @a .# r@, for each element @a@ in given set.
(-.#) :: (Ord a, Ord b) => Set a -> Rel a b -> Set b
as -.# r = ran (as' #.# r)
  where
    as' = full (Set.singleton ()) as

infix 6 -.#

-- | Returns the unions of @r #. b@, for each element @b@ in given set.
(#.-) :: (Ord a, Ord b) => Rel a b -> Set b -> Set a
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
firstMapMonotonic :: (a -> a') -> Rel a b -> Rel a' b
firstMapMonotonic f = Rel . firstMapMonotonic_ f . impl

-- | O(n). The precondition is not checked.
secondMapMonotonic :: (b -> b') -> Rel a b -> Rel a b'
secondMapMonotonic f = Rel . secondMapMonotonic_ f . impl

-- O(a \* n), where a is the domSize of output.
firstBind :: (Ord a, Ord b, Ord c) => (b -> Set a) -> Rel b c -> Rel a c
firstBind f (Rel r) = Rel (firstBind_ f r)

-- O(n \* c), where c is the codSize of output.
secondBind :: (Ord c) => Rel a b -> (b -> Set c) -> Rel a c
secondBind (Rel r) f = Rel (secondBind_ r f)

-- | O(n).
filter :: (a -> b -> Bool) -> Rel a b -> Rel a b
filter p = Rel . filter_ p . impl

-- | O(n).
partition :: (a -> b -> Bool) -> Rel a b -> (Rel a b, Rel a b)
partition p (Rel r) =
  let (r1, r2) = partition_ p r
  in (Rel r1, Rel r2)

-- * Conversion

-- | O(n)
fromSet :: (Ord a, Ord b) => Set (a,b) -> Rel a b
fromSet = Rel . fromSet_

-- | O(n)
toSet :: (Ord a, Ord b) => Rel a b -> Set (a,b)
toSet (Rel r) = toSet_ r

-- | O(n \* log n).
--   Converts from a list of pairs.
fromList :: (Ord a, Ord b) => [(a,b)] -> Rel a b
fromList = Rel . fromList_

-- | Converts to a list of pairs, through @Set (a,b)@.
toList :: Rel a b -> [(a,b)]
toList = toList_ . impl

-- | O(n). Convert from sorted list.
--   The precondition is not checked.
fromAscList :: (Eq a, Eq b) => [(a,b)] -> Rel a b
fromAscList = Rel . fromAscList_

-- | O(n). Convert from sorted, distinct list.
--   The precondition is not checked.
fromDistinctAscList :: (Eq a) => [(a,b)] -> Rel a b
fromDistinctAscList = Rel . fromDistinctAscList_

-- | O(n).
toAscList :: Rel a b -> [(a,b)]
toAscList (Rel r) = toAscList_ r

-- | O(n).
fromMap :: Map a b -> Rel a b
fromMap = Rel . fromMap_
