{- |
This module only re-exports the names which can be imported from
"Data.Relation.LTR", but in reduced API.
-}
module Data.Relation(
  -- * The relation class
  Relation(..),
  -- * Derived operations
  insert, delete,
  domSize, codSize,
  fromList, toList,
  -- * Operators
  (#.#),
  (.#), (#.),
  (-.#), (#.-),
  -- * Implementation ("Data.Relation.LTR")
  Rel(),
  filter,
  toAscList,
  fromAscList,
) where

import           Prelude           hiding (filter)

import           Data.Relation.LTR
