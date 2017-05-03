module Data.Relation.Naive(
  -- * Naive but surely correct implementation.
  NaiveRel(..)
) where

import           Data.Relation.Class
import           Data.Set            (Set)
import qualified Data.Set            as Set

newtype NaiveRel a b = NaiveRel (Set (a,b))
  deriving (Show, Eq, Ord)

instance Relation NaiveRel where
  fromSet = NaiveRel
  toSet (NaiveRel s) = s

  empty = NaiveRel Set.empty
  singleton a b = NaiveRel (Set.singleton (a,b))
  identity = NaiveRel . Set.mapMonotonic (\x -> (x,x))
  full as bs =
    NaiveRel . Set.fromDistinctAscList $
      [(a,b)| a <- Set.toAscList as,
              b <- Set.toAscList bs]
