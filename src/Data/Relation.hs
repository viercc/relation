module Data.Relation(
  module Data.Relation.LTR,
  mirrorRight,
  mirrorLeft
) where

import           Data.Relation.LTR
import           Data.Relation.RTL

import qualified Data.Relation.Internal.LTR as LTR
import qualified Data.Relation.Internal.RTL as RTL

mirrorRight :: Data.Relation.LTR.Rel a b -> Data.Relation.RTL.Rel b a
mirrorRight (LTR.Rel r) = RTL.Rel r

mirrorLeft :: Data.Relation.RTL.Rel a b -> Data.Relation.LTR.Rel b a
mirrorLeft (RTL.Rel r) = LTR.Rel r
