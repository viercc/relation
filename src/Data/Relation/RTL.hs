module Data.Relation.RTL(
  -- * The Relation class
  module Data.Relation.Class,
  -- * The Relation type
  Rel(),
  -- * Map/Filter
  firstMapMonotonic, secondMapMonotonic,
  filter, partition,
  -- * Conversion
  toAscList, fromAscList, fromDistinctAscList,
  fromMap,
  toAscList', fromAscList', fromDistinctAscList',
  toSet', fromSet',
  fromMap'
) where

import           Prelude                    hiding (filter, null)

import           Data.Relation.Class
import           Data.Relation.Internal.RTL
