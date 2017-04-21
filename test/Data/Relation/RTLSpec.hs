module Data.Relation.RTLSpec(spec) where

import           Test.Hspec

import           Common
import           Data.Relation.RTL

spec :: Spec
spec = spec_all (Proxy :: Proxy Rel)
