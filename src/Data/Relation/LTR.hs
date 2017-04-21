module Data.Relation.LTR(
  -- * The Relation class
  module Data.Relation.Class,
  -- * The Relation type
  Rel(),
  -- * Map/Filter
  firstMapMonotonic, secondMapMonotonic,
  filter, partition,
  -- * Conversion
  toAscList, fromAscList, fromDistinctAscList,
  fromMap
) where

import           Prelude                    hiding (filter, null)

import           Data.Relation.Class
import           Data.Relation.Internal.LTR
