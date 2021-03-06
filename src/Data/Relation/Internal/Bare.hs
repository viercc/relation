module Data.Relation.Internal.Bare(
  -- * Internal representation
  Rel_,
  showsPrec_,
  slice_, sliceSet_, revslice_,
  size_, member_,
  insert_, delete_,
  union_, difference_, intersection_,
  compose_, transpose_,
  firstMapMonotonic_, secondMapMonotonic_, filter_, partition_,
  firstBind_, secondBind_,
  toList_, fromList_,
  toAscList_, fromAscList_, fromDistinctAscList_,
  toSet_, fromSet_, fromMap_
) where

import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as Map
import           Data.Map.Merge.Strict
import           Data.Monoid
import           Data.Set              (Set, (\\))
import qualified Data.Set              as Set

-- | internal representation of relation
type Rel_ a b = Map a (Set b)

nonNullSet :: Set a -> Maybe (Set a)
nonNullSet set = if Set.null set then Nothing else Just set

showsPrec_ :: (Show a, Show b) => Int -> [(a, b)] -> ShowS
showsPrec_ p r =
  showParen (p > 10) $ ("fromList " ++) . showsPrec p r

-- * Operations to Rel_

-- O(log a)
slice_ :: (Ord a) => a -> Rel_ a b -> Set b
slice_ = Map.findWithDefault Set.empty

-- O
sliceSet_ :: (Ord a, Ord b) => Set a -> Rel_ a b -> Set b
sliceSet_ as r
  | Set.size as <= 100 =
      foldMap (`slice_` r) as
  | otherwise =
      Set.unions . Map.elems $
        Map.intersectionWith const r (Map.fromSet id as)

-- O(a * log b)
revslice_ :: (Ord b) => Rel_ a b -> b -> Set a
revslice_ r b =
  Set.fromDistinctAscList
    [a | (a, r_a) <- Map.toAscList r,
         b `Set.member` r_a ]

-- O(a)
size_ :: Rel_ a b -> Int
size_ = getSum . foldMap (Sum . Set.size)

-- O(log n)
member_ :: (Ord a, Ord b) => a -> b -> Rel_ a b -> Bool
member_ a b r = Set.member b (slice_ a r)

-- O(log n)
insert_ :: (Ord a, Ord b) => a -> b -> Rel_ a b -> Rel_ a b
insert_ a b = Map.insertWith Set.union a (Set.singleton b)

-- O(log n)
delete_ :: (Ord a, Ord b) => a -> b -> Rel_ a b -> Rel_ a b
delete_ a b = Map.update deletingB a
  where deletingB r_a = nonNullSet $ Set.delete b r_a

-- O(n)
union_ :: (Ord a, Ord b) => Rel_ a b -> Rel_ a b -> Rel_ a b
union_ = Map.unionWith Set.union

-- O(n)
difference_ :: (Ord a, Ord b) => Rel_ a b -> Rel_ a b -> Rel_ a b
difference_ = Map.differenceWith diff
  where
    diff r_a s_a = nonNullSet (r_a \\ s_a)

-- O(n).
intersection_ :: (Ord a, Ord b) => Rel_ a b -> Rel_ a b -> Rel_ a b
intersection_ =
  merge dropMissing
        dropMissing
        (zipWithMaybeMatched (\_ bs bs' -> nonNullSet (Set.intersection bs bs')))

-- O(n+a*b'*c'*log b').
compose_ :: (Ord b, Ord c) => Rel_ a b -> Rel_ b c -> Rel_ a c
compose_ r s =
  Map.mapMaybe (nonNullSet . (`sliceSet_` s)) r

transpose_ :: (Ord b) => Rel_ a b -> Rel_ b a
transpose_ r =
  Map.map (Set.fromDistinctAscList . ($ [])) $
  Map.unionsWith (.) $
  tr1 <$> Map.toAscList r
  where
    tr1 (a, bs) = Map.fromSet (const (a:)) bs

-- * Map/Filter
--
-- Here, the definition that a function @f@ is /Monotonic/ is following:
--
-- > forall x y. x < y ==> f x < f y
--
-- To perform generic mapping, you should use toList_ first,
-- then map and convert back by fromList_.

-- O(a). The precondition is not checked.
firstMapMonotonic_ :: (a -> a') -> Rel_ a b -> Rel_ a' b
firstMapMonotonic_ = Map.mapKeysMonotonic

-- O(n). The precondition is not checked.
secondMapMonotonic_ :: (b -> b') -> Rel_ a b -> Rel_ a b'
secondMapMonotonic_ f = Map.map (Set.mapMonotonic f)

-- O(a \* n), where a is the domSize of output.
firstBind_ :: (Ord a, Ord c) => (b -> Set a) -> Rel_ b c -> Rel_ a c
firstBind_ f r = transpose_ (secondBind_ (transpose_ r) f)

-- O(n \* c), where c is the codSize of output.
secondBind_ :: (Ord c) => Rel_ a b -> (b -> Set c) -> Rel_ a c
secondBind_ r f = Map.map (foldMap f) r

-- O(n).
filter_ :: (a -> b -> Bool) -> Rel_ a b -> Rel_ a b
filter_ p = Map.mapMaybeWithKey (\a -> nonNullSet . Set.filter (p a))

-- O(n).
partition_ :: (a -> b -> Bool) -> Rel_ a b -> (Rel_ a b, Rel_ a b)
partition_ p r =
  let parts = Map.mapWithKey (Set.partition . p) r
      r1 = Map.mapMaybe (nonNullSet . fst) parts
      r2 = Map.mapMaybe (nonNullSet . snd) parts
  in (r1, r2)

-- * Conversion

-- O(n).
toList_ :: Rel_ a b -> [(a,b)]
toList_ = toAscList_

-- O(n*log(n)).
fromList_ :: (Ord a, Ord b) => [(a,b)] -> Rel_ a b
fromList_ = fromSet_ . Set.fromList

-- O(n).
toAscList_ :: Rel_ a b -> [(a,b)]
toAscList_ r = [(a,b) | (a,r_a) <- Map.toAscList r,
                        b <- Set.toAscList r_a ]

-- O(n).
fromAscList_ :: (Eq a, Eq b) => [(a,b)] -> Rel_ a b
fromAscList_ =
  Map.map Set.fromAscList . Map.fromDistinctAscList . group'

-- O(n).
fromDistinctAscList_ :: (Eq a) => [(a,b)] -> Rel_ a b
fromDistinctAscList_ =
  Map.map Set.fromDistinctAscList . Map.fromDistinctAscList . group'

-- O(n).
toSet_ :: Rel_ a b -> Set (a,b)
toSet_ = Set.fromDistinctAscList . toAscList_

-- O(n).
fromSet_ :: (Eq a) => Set (a,b) -> Rel_ a b
fromSet_ = fromDistinctAscList_ .  Set.toAscList

-- O(n). (Note: @n==a@ in this case.)
fromMap_ :: Map a b -> Rel_ a b
fromMap_ = Map.map Set.singleton

-- * Utility

group' :: (Eq a) => [(a,b)] -> [(a,[b])]
group' [] = []
group' ((a,b):ps) =
      let (agroup,rest) = span (\p -> fst p == a) ps
          bs = fmap snd agroup
      in (a,b:bs) : group' rest
