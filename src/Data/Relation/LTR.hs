module Data.Relation.LTR(
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

import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Set                    (Set)
import qualified Data.Set                    as Set

import           Data.Relation.Internal.Bare
import           Data.Relation.Internal.LTR

-- * Query

-- |O(1). Tests if it is empty.
null :: Rel a b -> Bool
null r = Map.null (impl r)

-- |O(a). The number of associations.
size :: Rel a b -> Int
size r = size_ (impl r)

-- | O(log n). Is the @(a,b)@ a member of the relation?
member :: (Ord a, Ord b) => a -> b -> Rel a b -> Bool
member a b r = member_ a b (impl r)

-- | O(log a). Lookup the associations from left. Get all @b@
-- in the @(a,b)@ of members of relation.
lookupL :: (Ord a) => a -> Rel a b -> Set b
lookupL a r = slice_ a (impl r)

-- | O(a \* log b). Lookup the associations from right. Get all @a@
-- in the @(a,b)@ of members of relation.
--
-- Please be careful, because it is slower than @lookupL@.
lookupR :: (Ord b) => Rel a b -> b -> Set a
lookupR r b = revslice_ (impl r) b

-- | O(a). Set of @a@ from all @(a,b)@ in the relation.
dom :: Rel a b -> Set a
dom r = Map.keysSet (impl r)

-- | O(n). Set of @b@ from all @(a,b)@ in the relation.
--
-- Please be careful, because it is slower than @dom@.
cod :: (Ord b) => Rel a b -> Set b
cod r = foldMap id (impl r)

-- | O(1). Size of 'dom'.
domSize :: Rel a b -> Int
domSize r = Map.size (impl r)

-- | O(n). Size of 'cod'.
--
-- Please be careful, because it is slower than @domSize@.
codSize :: (Ord b) => Rel a b -> Int
codSize r = Set.size (cod r)

-- * Construction

-- | O(1). Empty relation.
empty :: Rel a b
empty = Rel Map.empty

-- | O(1). A relation only contains pair @(a,b)@
singleton :: a -> b -> Rel a b
singleton a b = Rel (Map.singleton a (Set.singleton b))

-- | O(log n). Insert new pair @(a,b)@ to relation.
insert :: (Ord a, Ord b) => a -> b -> Rel a b -> Rel a b
insert a b (Rel r) = Rel (insert_ a b r)

-- | O(log n). Remove pair @(a,b)@ from relation.
delete :: (Ord a, Ord b) => a -> b -> Rel a b -> Rel a b
delete a b (Rel r) = Rel (delete_ a b r)

-- * Operators

-- | same as 'compose'.
(#.#) :: (Ord b, Ord c) => Rel a b -> Rel b c -> Rel a c
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
    as' = Rel (Map.singleton () as)

infix 6 -.#

-- | Returns the unions of @r #. b@, for each element @b@ in given set.
(#.-) :: (Ord b) => Rel a b -> Set b -> Set a
r #.- bs = dom (r #.# bs')
  where
    bs' = fromSet $ Set.mapMonotonic (\b -> (b, ())) bs

infix 6 #.-

-- * Combine

-- | O(n + n'). Compute union.
union :: (Ord a, Ord b) => Rel a b -> Rel a b -> Rel a b
union (Rel r) (Rel s) = Rel (union_ r s)

-- | O(n + n'). Compute difference.
difference :: (Ord a, Ord b) => Rel a b -> Rel a b -> Rel a b
difference (Rel r) (Rel s) = Rel (difference_ r s)

-- | O(n + n'). Compute intersection.
intersection :: (Ord a, Ord b) => Rel a b -> Rel a b -> Rel a b
intersection (Rel r) (Rel s) = Rel (intersection_ r s)

-- | O(a \* (b + b') \* c'). Compose two relation.
compose :: (Ord b, Ord c) => Rel a b -> Rel b c -> Rel a c
compose (Rel r) (Rel s) = Rel (compose_ r s)

-- | O(1). Transposes relation.
transpose :: (Ord b) => Rel a b -> Rel b a
transpose (Rel r) = Rel (transpose_ r)

-- * Map/Filter
--
-- Here, the definition that a function @f@ is /Monotonic/ is following:
--
-- > forall x y. x < y ==> f x < f y
--
-- To perform generic mapping, you should use toList_ first,
-- then map and convert back by fromList_.

-- | O(a). The precondition is not checked.
firstMapMonotonic :: (a -> a') -> Rel a b -> Rel a' b
firstMapMonotonic f = Rel . firstMapMonotonic_ f . impl

-- | O(n). The precondition is not checked.
secondMapMonotonic :: (b -> b') -> Rel a b -> Rel a b'
secondMapMonotonic f = Rel . secondMapMonotonic_ f . impl

-- | O(n).
filter :: (a -> b -> Bool) -> Rel a b -> Rel a b
filter p = Rel . filter_ p . impl

-- | O(n).
partition :: (a -> b -> Bool) -> Rel a b -> (Rel a b, Rel a b)
partition p (Rel r) =
  let (r1, r2) = partition_ p r
  in (Rel r1, Rel r2)

-- * Conversion

-- | O(n). Convert to list.
toList :: Rel a b -> [(a,b)]
toList = toAscList

-- | O(n\*log(n)). Convert from list.
fromList :: (Ord a, Ord b) => [(a,b)] -> Rel a b
fromList = Rel . fromList_

-- | O(n).
toAscList :: Rel a b -> [(a,b)]
toAscList (Rel r) = toAscList_ r

-- | O(n). Convert from sorted list.
--   The precondition is not checked.
fromAscList :: (Eq a, Eq b) => [(a,b)] -> Rel a b
fromAscList = Rel . fromAscList_

-- | O(n). Convert from sorted, distinct list.
--   The precondition is not checked.
fromDistinctAscList :: (Eq a) => [(a,b)] -> Rel a b
fromDistinctAscList = Rel . fromDistinctAscList_

-- | O(n). Treat @Rel a b@ as @Set (a,b)@.
toSet :: Rel a b -> Set (a, b)
toSet (Rel r) = toSet_ r

-- | O(n).
fromSet :: (Eq a) => Set (a,b) -> Rel a b
fromSet = Rel . fromSet_

-- | O(n).
fromMap :: Map a b -> Rel a b
fromMap = Rel . fromMap_
