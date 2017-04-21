module Data.Relation.Internal.Both where

import           Prelude                     hiding (filter, null)

import           Data.Map.Lazy               (Map)
import qualified Data.Map.Lazy               as Map
import           Data.Set                    (Set)
import qualified Data.Set                    as Set

import           Data.Relation.Class
import           Data.Relation.Internal.Bare

data Rel a b = Rel {
                 rmap :: Rel_ a b,
                 lmap :: Rel_ b a
               }

instance (Eq a, Eq b) => Eq (Rel a b) where
    r == s = (rmap r) == (rmap s)

instance (Show a, Show b) => Show (Rel a b) where
    show r = "fromList " ++ show (toList_ (rmap r))

instance Relation Rel where
  -- | O(n)
  fromSet = fromR . fromSet_
  -- | O(n)
  toSet = toSet_ . rmap

  -- | O(1)
  null r = Map.null (rmap r)
  -- | O(a)
  size r = size_ (rmap r)
  -- | O(log n)
  member a b r = member_ a b (rmap r)
  -- | O(log a)
  lookupL a r = slice_ a (rmap r)
  -- | O(log b)
  lookupR r b = slice_ b (lmap r)
  -- | O(a)
  dom r = Map.keysSet (rmap r)
  -- | O(b)
  cod r = Map.keysSet (lmap r)

  -- | O(1)
  empty = Rel Map.empty Map.empty
  -- | O(1)
  singleton a b = Rel (Map.singleton a (Set.singleton b))
                      (Map.singleton b (Set.singleton a))
  -- | O(n)
  identity as =
    let idA = Map.fromSet (Set.singleton) as
    in Rel idA idA
  -- | O(a+b)
  full as bs = Rel (Map.fromSet (const bs) as)
                   (Map.fromSet (const as) bs)

  -- | O(n)
  union (Rel rr rl) (Rel sr sl) = Rel (union_ rr sr) (union_ rl sl)
  -- | O(n)
  difference (Rel rr rl) (Rel sr sl) = Rel (difference_ rr sr) (difference_ rl sl)
  -- | O(n)
  intersection (Rel rr rl) (Rel sr sl) = Rel (intersection_ rr sr) (intersection_ rl sl)
  -- | O(a \* (b + b') \* c)
  compose (Rel rr rl) (Rel sr sl) = Rel (compose_ rr sr) (compose_ sl rl)
  -- | O(1)
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

-- | O(n).
toAscList :: Rel a b -> [(a,b)]
toAscList = toAscList_ . rmap

-- | O(n). Convert from sorted list.
fromAscList :: (Ord a, Ord b) => [(a,b)] -> Rel a b
fromAscList = fromR . fromAscList_

-- | O(n). Convert from sorted list.
fromDistinctAscList :: (Ord a, Ord b) => [(a,b)] -> Rel a b
fromDistinctAscList = fromR . fromDistinctAscList_

-- | O(n).
fromMap :: (Ord b) => Map a b -> Rel a b
fromMap = fromR . fromMap_

-- O(n).
fromR :: (Ord b) => Rel_ a b -> Rel a b
fromR r = Rel r (transpose_ r)
