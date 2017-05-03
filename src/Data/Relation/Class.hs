module Data.Relation.Class(
  -- * The Relation class
  Relation(..),
  -- * Derived operations
  insert, delete,
  domSize, codSize,
  fromList, toList,
  -- * Operators
  (#.#),
  (.#), (#.),
  (-.#), (#.-),
) where

import           Prelude    hiding (filter, null)

import           Data.Set   (Set)
import qualified Data.Set   as Set
import           Data.Tuple (swap)

class Relation t where
  -- | Returns a relation from a set of pairs @(a,b)@,
  --   which is a member of returned relation.
  fromSet :: (Ord a, Ord b) => Set (a,b) -> t a b

  -- | Returns a set of pairs @(a,b)@,
  --   which is a member of a given relation.
  toSet :: (Ord a, Ord b) => t a b -> Set (a,b)

  -- * Construction

  -- | Empty relation.
  empty     :: t a b

  -- | @singleton a b@ is a relation which contains only one
  --   pair @(a,b).
  singleton :: a -> b -> t a b

  -- | A relation which contains pairs of form @(a,a)@ for each
  --   element @a@ of given set.
  --
  -- > identity (Set.fromList [0,1,2]) == fromList [(0,0), (1,1), (2,2)]
  identity  :: Set a -> t a a

  -- | @full aSet bSet@ is a relation which contains every pair @(a,b)@,
  --  where @a@ is an element of @aSet@, @b@ is an element of @bSet@.
  --
  -- > full (Set.fromList [0,1,2]) (Set.fromList "ab") ==
  -- >   fromList [(0,'a'), (0,'b'), (1,'a'), (1,'b'), (2,'a'), (2,'b')]
  full      :: Set a -> Set b -> t a b

  -- | Tests given relation is empty.
  null :: (Ord a, Ord b) => t a b -> Bool
  null = Set.null . toSet

  -- | Returns a number of distinct members @(a,b)@.
  size :: (Ord a, Ord b) => t a b -> Int
  size = Set.size . toSet

  -- | @member a b r@ tests @(a,b)@ is a member of a relation @r@.
  member :: (Ord a, Ord b) => a -> b -> t a b -> Bool
  member a b = Set.member (a,b) . toSet

  -- | Looks up a relation from left. @lookupL a r@ a set of all @b@
  --   such that @member a b r@ is true.
  lookupL :: (Ord a, Ord b) => a -> t a b -> Set b
  lookupL a r = Set.mapMonotonic snd . Set.filter ((==a).fst) $ toSet r

  -- | Looks up a relation from right. @lookupR r b@ a set of all @a@
  --   such that @member a b r@ is true.
  lookupR :: (Ord a, Ord b) => t a b -> b -> Set a
  lookupR r b = Set.mapMonotonic fst . Set.filter ((==b).snd) $ toSet r

  -- | Domain of a relation.
  dom :: (Ord a, Ord b) => t a b -> Set a
  dom = Set.map fst . toSet

  -- | Range of a relation.
  cod :: (Ord a, Ord b) => t a b -> Set b
  cod = Set.map snd . toSet

  -- | Union of two relations.
  --
  -- > member a b (union r s) <==>
  -- >  member a b r || member a b s
  union :: (Ord a, Ord b) => t a b -> t a b -> t a b
  union r s = fromSet (Set.union (toSet r) (toSet s))

  -- | Intersection of two relations.
  --
  -- > member a b (intersection r s) <==>
  -- >  member a b r && member a b s
  intersection :: (Ord a, Ord b) => t a b -> t a b -> t a b
  intersection r s = fromSet (Set.intersection (toSet r) (toSet s))

  -- | Difference of two relations.
  --
  -- > member a b (difference r s) <==>
  -- >  member a b r && not (member a b s)
  difference :: (Ord a, Ord b) => t a b -> t a b -> t a b
  difference r s = fromSet (Set.difference (toSet r) (toSet s))

  -- | Composition of relations.
  --
  -- > member a c (compose r s) <==>
  -- >   ∃b. member a b r && member b c s
  compose :: (Ord a, Ord b, Ord c) => t a b -> t b c -> t a c
  compose r s = fromSet (compose' (toSet r) (toSet s))

  -- | Opposite relation.
  --
  -- > member a b r <==> member b a (transpose r)
  transpose :: (Ord a, Ord b) => t a b -> t b a
  transpose = fromList . fmap swap . toList

-- * Derived operations

-- | Inserts a pair @(a,b)@ to a relation.
insert :: (Relation t, Ord a, Ord b) => a -> b -> t a b -> t a b
insert a b r = union (singleton a b) r

-- | Deletes a pair @(a,b)@ from a relation.
delete :: (Relation t, Ord a, Ord b) => a -> b -> t a b -> t a b
delete a b r = difference r (singleton a b)

-- | Size of 'dom'.
domSize :: (Relation t, Ord a, Ord b) => t a b -> Int
domSize = Set.size . dom

-- | Size of 'cod'.
codSize :: (Relation t, Ord a, Ord b) => t a b -> Int
codSize = Set.size . cod

-- | Converts from a list of pairs, by converting to @Set@ beforehand.
fromList :: (Relation t, Ord a, Ord b) => [(a,b)] -> t a b
fromList = fromSet . Set.fromList

-- | Converts to a list of pairs, through @Set (a,b)@.
toList :: (Relation t, Ord a, Ord b) => t a b -> [(a,b)]
toList = Set.toList . toSet

-- * Operators

-- | same as 'compose'.
(#.#) :: (Relation t, Ord a, Ord b, Ord c) => t a b -> t b c -> t a c
(#.#) = compose

infixl 6 #.#

-- | same as 'lookupL'.
(.#) :: (Relation t, Ord a, Ord b) => a -> t a b -> Set b
(.#) = lookupL

infix 6 .#

-- | same as 'lookupR'.
(#.) :: (Relation t, Ord a, Ord b) => t a b -> b -> Set a
(#.) = lookupR

infix 6 #.

-- | Returns the unions of @a .# r@, for each element @a@ in given set.
(-.#) :: (Relation t, Ord a, Ord b) => Set a -> t a b -> Set b
as -.# r = cod (as' #.# r)
  where
    as' = full (Set.singleton ()) as

infix 6 -.#

-- | Returns the unions of @r #. b@, for each element @b@ in given set.
(#.-) :: (Relation t, Ord a, Ord b) => t a b -> Set b -> Set a
r #.- bs = dom (r #.# bs')
  where
    bs' = full bs (Set.singleton ())

infix 6 #.-

-- * Local definitions
compose' :: (Ord a, Ord b, Ord c) => Set (a,b) -> Set (b,c) -> Set (a,c)
compose' r s = Set.fromList $
  [(a,c) | (a, b) <- Set.toAscList r,
           (b',c) <- Set.toAscList s,
           b == b']
