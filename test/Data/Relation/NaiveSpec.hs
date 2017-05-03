module Data.Relation.NaiveSpec(spec) where

import           Test.Hspec

import           Common
import           Data.Relation.Naive

spec :: Spec
spec =
  do spec_compose t
     spec_transpose t
  where
    t = Proxy :: Proxy NaiveRel

