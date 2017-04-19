{-# OPTIONS_GHC -Werror -fno-warn-orphans #-}
{-
 We must be cautious to missing defined 'prop_*' in exported 'spec', so turned
every warning (including unused definition) into error. But we also need to
define an orphan instance of Arbitrary, so that is explicitly allowed.
-}
module Data.Relation.RTLSpec(spec) where

import           Test.Hspec
import           Test.QuickCheck

import           Common

import           Data.Relation.RTL
import           Data.Set          (Set)
import qualified Data.Set          as Set

instance (Arbitrary a, Arbitrary b, Ord a, Ord b) =>
         Arbitrary (Rel a b) where
  arbitrary = fromList <$> relDataOf arbitrary
  shrink r = fromSet <$> shrink (toSet r)

prop_bijection :: Set (A,B) -> Rel A B -> Property
prop_bijection = isBijection fromSet toSet

prop_union, prop_intersection, prop_difference ::
  Set (A,B) -> Set (A,B) -> Property
prop_union        = commutes2 fromSet Set.union union
prop_intersection = commutes2 fromSet Set.intersection intersection
prop_difference   = commutes2 fromSet Set.difference difference

prop_compose_unit :: Rel A B -> Property
prop_compose_unit r =
  let idA = fromSet $ idRel (dom r)
      idB = fromSet $ idRel (cod r)
  in (idA #.# r === r) .&&. (r #.# idB === r)

prop_compose_empty ::
  Rel A B -> Property
prop_compose_empty r =
  let z  = empty :: Rel A B
  in (r #.# empty === z) .&&. (empty #.# r === z)

prop_compose_singleton_eq ::
  A -> B -> C -> Property
prop_compose_singleton_eq a b c =
  let r = singleton a b
      s = singleton b c
  in r #.# s === singleton a c

prop_compose_singleton_neq ::
  A -> B -> B -> C -> Property
prop_compose_singleton_neq a b b' c =
  let r = singleton a b
      s = singleton b' c
  in (b /= b') ==> (r #.# s === empty)

prop_compose_associative ::
  Rel A B -> Rel B C -> Rel C D -> Property
prop_compose_associative r s t =
  (r #.# s) #.# t === r #.# (s #.# t)

prop_compose_distributive ::
  Rel A B -> Rel B C -> Rel B C -> Property
prop_compose_distributive r s t =
  (r #.# (s `union` t) === (r #.# s) `union` (r #.# t))

prop_transpose_involutive ::
  Rel A B -> Property
prop_transpose_involutive r = r === transpose (transpose r)

prop_transpose_involutive_on_compose ::
  Rel A B -> Rel B C -> Property
prop_transpose_involutive_on_compose r s =
  transpose (r #.# s) === transpose s #.# transpose r

spec :: Spec
spec = do
  describe "Conversion between 'Data.Set (a,b)'" $ do
    it "has a bijection 'fromSet' and its inverse 'toSet'." $
      property prop_bijection
    it "has same empty." $
      toSet (empty :: Rel A B) `shouldBe` Set.empty
    it "has same singleton." $
      let a = A 1
          b = B 2
      in toSet (singleton a b) `shouldBe` Set.singleton (a, b)
    it "preserves union operation." $
      property prop_union
    it "preserves intersection operation." $
      property prop_intersection
    it "preserves difference operation." $
      property prop_difference
  describe "compose" $ do
    it "has identity relation." $
      property prop_compose_unit
    it "has absorbing element, namely 'empty'." $
      property prop_compose_empty
    context "When composing (singleton a b) and (singleton b' c)" $ do
      it "returns (singleton a c) if (b == b')" $
        property prop_compose_singleton_eq
      it "returns empty           if (b /= b')" $
        property prop_compose_singleton_neq
    it "satisfies associativity law." $
      property prop_compose_associative
    it "distributes over union." $
      property prop_compose_distributive
  describe "transpose" $ do
    it "is involution (its inverse is itself)." $
      property prop_transpose_involutive
    it "is involution on the monoid formed by composition" $
      property prop_transpose_involutive_on_compose
