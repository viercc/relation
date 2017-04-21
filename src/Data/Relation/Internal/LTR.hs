module Data.Relation.Internal.LTR where

import           Prelude                     hiding (filter, null)

import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import qualified Data.Set                    as Set

import           Data.Relation.Class
import           Data.Relation.Internal.Bare

newtype Rel a b = Rel { impl :: Rel_ a b }
                deriving (Eq, Ord)

instance (Show a, Show b) => Show (Rel a b) where
    show (Rel r) = "fromList " ++ show (toAscList_ r)

instance Relation Rel where
  -- | O(n)
  fromSet = Rel . fromSet_
  -- | O(n)
  toSet (Rel r) = toSet_ r

  -- | O(1)
  null r = Map.null (impl r)
  -- | O(a)
  size r = size_ (impl r)
  -- | O(log n)
  member a b r = member_ a b (impl r)
  -- | O(log a)
  lookupL a r = slice_ a (impl r)
  -- | O(a \* log b)
  lookupR r b = revslice_ (impl r) b
  -- | O(a)
  dom r = Map.keysSet (impl r)
  -- | O(n)
  cod r = foldMap id (impl r)

  -- | O(1)
  empty = Rel Map.empty
  -- | O(1)
  singleton a b = Rel (Map.singleton a (Set.singleton b))
  -- | O(n)
  identity as = Rel (Map.fromSet (Set.singleton) as)
  -- | O(a)
  full as bs = Rel (Map.fromSet (const bs) as)

  -- | O(n)
  union (Rel r) (Rel s) = Rel (union_ r s)
  -- | O(n)
  difference (Rel r) (Rel s) = Rel (difference_ r s)
  -- | O(n)
  intersection (Rel r) (Rel s) = Rel (intersection_ r s)
  -- | O(a \* (b + b') \* c)
  compose (Rel r) (Rel s) = Rel (compose_ r s)
  -- | O(n \* log a)
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

-- | O(n).
fromMap :: Map a b -> Rel a b
fromMap = Rel . fromMap_
