module Data.Relation.Internal.RTL where

import           Prelude                     hiding (filter, null)

import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Tuple                  (swap)

import           Data.Relation.Class
import           Data.Relation.Internal.Bare

newtype Rel a b = Rel { impl :: Rel_ b a }
                deriving (Eq, Ord)

instance (Show a, Show b) => Show (Rel a b) where
    show (Rel r) = "fromList " ++ show (swap <$> toAscList_ r)

instance Relation Rel where
  -- | O(n \* log b)
  fromSet = Rel . transpose_ . fromSet_
  -- | O(n \* log b)
  toSet = toSet_ . transpose_ . impl

  -- | O(1)
  null r = Map.null (impl r)
  -- | O(a)
  size r = size_ (impl r)
  -- | O(log n)
  member a b r = member_ b a (impl r)
  -- | O(b \* log a)
  lookupL a r = revslice_ (impl r) a
  -- | O(log b)
  lookupR r b = slice_ b (impl r)
  -- | O(n)
  dom r = foldMap id (impl r)
  -- | O(b)
  cod r = Map.keysSet (impl r)

  -- | O(1)
  empty = Rel Map.empty
  -- | O(1)
  singleton a b = Rel (Map.singleton b (Set.singleton a))
  -- | O(n)
  identity as = Rel (Map.fromSet (Set.singleton) as)
  -- | O(a)
  full as bs = Rel (Map.fromSet (const as) bs)

  -- | O(n)
  union (Rel r) (Rel s) = Rel (union_ r s)
  -- | O(n)
  difference (Rel r) (Rel s) = Rel (difference_ r s)
  -- | O(n)
  intersection (Rel r) (Rel s) = Rel (intersection_ r s)
  -- | O(a \* (b + b') \* c)
  compose (Rel r) (Rel s) = Rel (compose_ s r)
  -- | O(n \* log b)
  transpose (Rel r) = Rel (transpose_ r)

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
firstMapMonotonic f = Rel . secondMapMonotonic_ f . impl

-- | O(b). The precondition is not checked.
secondMapMonotonic :: (b -> b') -> Rel a b -> Rel a b'
secondMapMonotonic f = Rel . firstMapMonotonic_ f . impl

-- | O(n).
filter :: (a -> b -> Bool) -> Rel a b -> Rel a b
filter p = Rel . filter_ (flip p) . impl

-- | O(n).
partition :: (a -> b -> Bool) -> Rel a b -> (Rel a b, Rel a b)
partition p (Rel r) =
  let (r1, r2) = partition_ (flip p) r
  in (Rel r1, Rel r2)

-- * Conversion

-- | O(n \* log b).
toAscList :: (Ord a, Ord b) => Rel a b -> [(a,b)]
toAscList = toAscList' . transpose

-- | O(n \* log b). Convert from sorted list.
--   The precondition is not checked.
fromAscList :: (Ord a, Ord b) => [(a,b)] -> Rel a b
fromAscList = transpose . fromAscList'

-- | O(n \* log b). Convert from sorted, distinct list.
--   The precondition is not checked.
fromDistinctAscList :: (Ord a, Ord b) => [(a,b)] -> Rel a b
fromDistinctAscList = transpose . fromDistinctAscList'

-- | O(n).
fromMap :: (Ord a, Ord b) => Map a b -> Rel a b
fromMap = transpose . fromMap'

-- * Swapped conversion. Due to internal representaion,
--   This is more efficient.

-- | O(n).
toAscList' :: Rel a b -> [(b,a)]
toAscList' (Rel r) = toAscList_ r

-- | O(n). Convert from sorted list.
--   The precondition is not checked.
fromAscList' :: (Eq a, Eq b) => [(b,a)] -> Rel a b
fromAscList' = Rel . fromAscList_

-- | O(n). Convert from sorted, distinct list.
--   The precondition is not checked.
fromDistinctAscList' :: (Eq b) => [(b,a)] -> Rel a b
fromDistinctAscList' = Rel . fromDistinctAscList_

-- | O(n). Treat @Rel a b@ as @Set (a,b)@.
toSet' :: Rel a b -> Set (b,a)
toSet' (Rel r) = toSet_ r

-- | O(n).
fromSet' :: (Eq b) => Set (b,a) -> Rel a b
fromSet' = Rel . fromSet_

-- | O(n).
fromMap' :: Map b a -> Rel a b
fromMap' = Rel . fromMap_
