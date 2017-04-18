{-|
Relations are associations between two elements and
can be modeled as Set of pair of two element @(a, b)@.
This module provides some relation operation by the interfaces
similar to "Data.Map" or "Data.Set".

As internal representation of @'Rel' a b@, this module uses @'Map' a ('Set' b)@
and its \'reversed\' version, @Map b (Set a)@. Redundant two maps are maintained
consistently when each operation applies, and used for efficient lookup from both side.
If you use only one side of lookup, other map are not really computed in the sence of
lazy evaluation.

-}
module Data.Relation.Both(
  -- * The Relation type
  Rel(),
  -- * Query
  null, size, member,
  lookupL, lookupR,
  dom, cod, domSize, codSize,
  -- * Construction
  empty, singleton,
  insert,
  delete,
  -- * Operators
  (.#), (#.),
  (#.#),
  (-.#), (#.-),
  -- * Combine
  union, difference, intersection,
  compose,
  transpose,
  -- * Map/Filter
  firstMapMonotonic, secondMapMonotonic,
  filter, partition,
  -- * Conversion
  toList, fromList,
  toAscList, fromAscList, fromDistinctAscList,
  toSet, fromSet,
  fromMap
) where

import           Prelude                     hiding (filter, null)

import           Data.Map.Lazy               (Map)
import qualified Data.Map.Lazy               as Map
import           Data.Set                    (Set)
import qualified Data.Set                    as Set

import           Data.Relation.Internal.Bare
import           Data.Relation.Internal.Both

---- operations to Rel

-- * Query

-- |O(1). Tests if it is empty.
null :: Rel a b -> Bool
null r = Map.null (rmap r)

-- |O(n). The number of associations.
size :: Rel a b -> Int
size r = size_ (rmap r)

-- |O(log n). Is the @(a,b)@ a member of the relation?
member :: (Ord a, Ord b) => a -> b -> Rel a b -> Bool
member a b r = member_ a b (rmap r)

-- |O(log a). Lookup the associations from left. Get all @b@
-- in the @(a,b)@ of members of relation.
lookupL :: (Ord a) => a -> Rel a b -> Set b
lookupL a r = slice_ a (rmap r)

-- |O(log b). Lookup the associations from right. Get all @a@
-- in the @(a,b)@ of members of relation.
lookupR :: (Ord b) => Rel a b -> b -> Set a
lookupR r b = slice_ b (lmap r)

-- |O(a). Set of @a@ from all @(a,b)@ in the relation.
dom :: Rel a b -> Set a
dom r = Map.keysSet (rmap r)

-- |O(b). Set of @b@ from all @(a,b)@ in the relation.
cod :: Rel a b -> Set b
cod r = Map.keysSet (lmap r)

-- |O(1). Size of 'dom'.
domSize :: Rel a b -> Int
domSize r = Map.size (rmap r)

-- |O(1). Size of 'cod'.
codSize :: Rel a b -> Int
codSize r = Map.size (lmap r)

-- * Construction

-- |O(1). Empty relation.
empty :: Rel a b
empty = Rel Map.empty Map.empty

-- |O(1). A relation only contains pair @(a,b)@
singleton :: a -> b -> Rel a b
singleton a b = Rel (Map.singleton a (Set.singleton b))
                    (Map.singleton b (Set.singleton a))

-- |O(log n). Insert new pair @(a,b)@ to relation.
insert :: (Ord a, Ord b) => a -> b -> Rel a b -> Rel a b
insert a b (Rel rr rl) = Rel (insert_ a b rr) (insert_ b a rl)

-- |O(log n). Remove pair @(a,b)@ from relation.
delete :: (Ord a, Ord b) => a -> b -> Rel a b -> Rel a b
delete a b (Rel rr rl) = Rel (delete_ a b rr) (delete_ b a rl)

-- operators
-- | same as 'compose'.
(#.#) :: (Ord a, Ord b, Ord c) => Rel a b -> Rel b c -> Rel a c
(#.#) = compose

infixl 6 #.#

-- | same as 'lookupL'.
(.#) :: (Ord a) => a -> Rel a b -> Set b
(.#) = lookupL

infix 6 .#

-- | same as 'lookupR'.
(#.) :: (Ord b) => Rel a b -> b -> Set a
(#.) = lookupR

infix 6 #.

-- | Returns the unions of @a .# r@, for each element @a@ in given set.
(-.#) :: (Ord a, Ord b) => Set a -> Rel a b -> Set b
as -.# r = cod (as' #.# r)
  where
    as' = fromSet $ Set.mapMonotonic (\a -> ((), a)) as

infix 6 -.#

-- | Returns the unions of @r #. b@, for each element @b@ in given set.
(#.-) :: (Ord a, Ord b) => Rel a b -> Set b -> Set a
r #.- bs = dom (r #.# bs')
  where
    bs' = fromSet $ Set.mapMonotonic (\b -> (b, ())) bs

infix 6 #.-

-- * Combine

-- |O(n + n'). Compute union.
union :: (Ord a, Ord b) => Rel a b -> Rel a b -> Rel a b
union (Rel rr rl) (Rel sr sl) = Rel (union_ rr sr) (union_ rl sl)

-- |O(n + n'). Compute difference.
difference :: (Ord a, Ord b) => Rel a b -> Rel a b -> Rel a b
difference (Rel rr rl) (Rel sr sl) = Rel (difference_ rr sr) (difference_ rl sl)

-- |O(n + n'). Compute intersection.
intersection :: (Ord a, Ord b) => Rel a b -> Rel a b -> Rel a b
intersection (Rel rr rl) (Rel sr sl) = Rel (intersection_ rr sr) (intersection_ rl sl)

-- |O(a \* (b + b') \* c'). Compose two relation.
compose :: (Ord a, Ord b, Ord c) => Rel a b -> Rel b c -> Rel a c
compose (Rel rr rl) (Rel sr sl) = Rel (compose_ rr sr) (compose_ sl rl)

-- | O(1). Transposes relation.
transpose :: Rel a b -> Rel b a
transpose (Rel rr rl) = Rel rl rr

-- * Map/Filter
--
-- Here, the definition that a function @f@ is /Monotonic/ is following:
--
-- > forall x y. x < y ==> f x < f y
--
-- To perform generic mapping, you should use toList_ first,
-- then map and convert back by fromList_.

-- | O(n). The precondition is not checked.
firstMapMonotonic :: (a -> a') -> Rel a b -> Rel a' b
firstMapMonotonic f (Rel rr rl) =
  Rel (firstMapMonotonic_ f rr) (secondMapMonotonic_ f rl)

-- | O(n). The precondition is not checked.
secondMapMonotonic :: (b -> b') -> Rel a b -> Rel a b'
secondMapMonotonic f (Rel rr rl) =
  Rel (secondMapMonotonic_ f rr) (firstMapMonotonic_ f rl)

-- | O(n).
filter :: (a -> b -> Bool) -> Rel a b -> Rel a b
filter p (Rel rr rl) = Rel (filter_ p rr) (filter_ (flip p) rl)

-- | O(n).
partition :: (a -> b -> Bool) -> Rel a b -> (Rel a b, Rel a b)
partition p (Rel rr rl) =
  let (rr1, rr2) = partition_ p rr
      (rl1, rl2) = partition_ (flip p) rl
  in (Rel rr1 rl1, Rel rr2 rl2)

-- * Conversion

-- | O(n). Convert to list.
toList :: Rel a b -> [(a,b)]
toList = toAscList

-- | O(n\*log(n)). Convert from list.
fromList :: (Ord a, Ord b) => [(a,b)] -> Rel a b
fromList = fromR . fromList_

-- | O(n).
toAscList :: Rel a b -> [(a,b)]
toAscList = toAscList_ . rmap

-- | O(n). Convert from sorted list.
fromAscList :: (Ord a, Ord b) => [(a,b)] -> Rel a b
fromAscList = fromR . fromAscList_

-- | O(n). Convert from sorted list.
fromDistinctAscList :: (Ord a, Ord b) => [(a,b)] -> Rel a b
fromDistinctAscList = fromR . fromDistinctAscList_

-- | O(1). Treat @Rel a b@ as @Set (a,b)@.
toSet :: Rel a b -> Set (a,b)
toSet = toSet_ . rmap

-- | O(n).
fromSet :: (Eq a, Ord b) => Set (a,b) -> Rel a b
fromSet = fromR . fromSet_

-- | O(n).
fromMap :: (Ord b) => Map a b -> Rel a b
fromMap = fromR . fromMap_

-- O(n).
fromR :: (Ord b) => Rel_ a b -> Rel a b
fromR r = Rel r (transpose_ r)
