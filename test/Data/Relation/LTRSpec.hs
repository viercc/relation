module Data.Relation.LTRSpec(spec) where

import           Test.Hspec

import           Common
import           Data.Relation.LTR

spec :: Spec
spec = spec_all (Proxy :: Proxy Rel)
