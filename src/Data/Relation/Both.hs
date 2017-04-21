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
  -- * Map/Filter
  firstMapMonotonic, secondMapMonotonic,
  filter, partition,
  -- * Conversion
  toAscList, fromAscList, fromDistinctAscList,
  fromMap
) where

import           Prelude                     hiding (filter, null)

import           Data.Relation.Internal.Both
