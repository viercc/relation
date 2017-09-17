{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Data.Relation.LTRSpec(
  spec
) where

import           Test.Hspec
import           Test.QuickCheck

import           Data.Set            (Set)

import           Data.Relation.LTR   as LTR
import           Data.Relation.Naive (NaiveRel)
import qualified Data.Relation.Naive as Naive

-- * Sample Element Types

newtype A = A Int
          deriving (Show, Eq, Ord, Enum, Arbitrary)

newtype B = B Int
          deriving (Show, Eq, Ord, Enum, Arbitrary)

newtype C = C Int
          deriving (Show, Eq, Ord, Enum, Arbitrary)

newtype D = D Int
          deriving (Show, Eq, Ord, Enum, Arbitrary)

-- * Utility Functions

sqrti :: (Integral a) => a -> a
sqrti = floor . (sqrt :: Double -> Double) . fromIntegral

relDataOf :: Gen (a,b) -> Gen [(a,b)]
relDataOf gen = listOf (scale sqrti gen)

isBijection :: (Eq a, Show a, Eq b, Show b) =>
  (a -> b) -> (b -> a) -> a -> b -> Property
isBijection f g = \a b -> a === g (f a) .&&. b === f (g b)

instance (Arbitrary a, Ord a, Arbitrary b, Ord b) =>
         Arbitrary (Rel a b) where
  arbitrary = LTR.fromList <$> relDataOf arbitrary
  shrink r = LTR.fromSet <$> shrink (LTR.toSet r)

instance (Arbitrary a, Ord a, Arbitrary b, Ord b) =>
         Arbitrary (NaiveRel a b) where
  arbitrary = Naive.fromList <$> relDataOf arbitrary
  shrink r = Naive.fromSet <$> shrink (Naive.toSet r)

fromNaive :: (Ord a, Ord b) => NaiveRel a b -> Rel a b
fromNaive = LTR.fromSet . Naive.toSet

toNaive :: (Ord a, Ord b) => Rel a b -> NaiveRel a b
toNaive   = Naive.fromSet . LTR.toSet

class Iso t u where
  to :: t -> u
  from :: u -> t

instance (Ord a, Ord b) => Iso (Rel a b) (NaiveRel a b) where
  to = toNaive
  from = fromNaive

instance Iso a a where
  to = id
  from = id

class Eqv t u where
  eqv :: t -> u -> Property
  default eqv :: (t ~ u, Show t, Eq t) => t -> u -> Property
  eqv = (===)

instance (Show a, Ord a, Show b, Ord b) => Eqv (Rel a b) (NaiveRel a b) where
  eqv r s = toNaive r === s

instance (Show ti, Arbitrary ti, Iso ti ui, Eqv t u) =>
         Eqv (ti -> t) (ui -> u) where
  eqv f g = property $ \ti -> f ti `eqv` g (to ti)

instance Eqv A A
instance Eqv B B
instance Eqv C C
instance Eqv D D
instance Eqv Int Int
instance Eqv Bool Bool
instance (Show a, Eq a) => Eqv (Set a) (Set a)

-- # Equivalence to naive implementation

prop_naive_bijection :: NaiveRel A B -> Rel A B -> Property
prop_naive_bijection naive r = isBijection fromNaive toNaive naive r

spec_compatibility :: Spec
spec_compatibility =
  context "Equivalence to naive implementation NaiveRel" $ do
    it "has bijection onto NaiveRel" $
      property prop_naive_bijection
    context "Construction" $ do
      it "empty" $
        LTR.empty @A @B `eqv` Naive.empty @A @B
      it "singleton" $
        LTR.singleton @A @B `eqv` Naive.singleton @A @B
      it "identity" $
        LTR.identity @A `eqv` Naive.identity @A
      it "full" $
        LTR.full @A @B `eqv` Naive.full @A @B
    context "Algebra" $ do
      it "union" $
        LTR.union @A @B `eqv` Naive.union @A @B
      it "intersection" $
        LTR.intersection @A @B `eqv` Naive.intersection @A @B
      it "difference" $
        LTR.difference @A @B `eqv` Naive.difference @A @B
      it "compose" $
        LTR.compose @A @B @C `eqv` Naive.compose @A @B @C
      it "transpose" $
        LTR.transpose @A @B `eqv` Naive.transpose @A @B
    context "Query" $ do
      it "null" $
        LTR.null @A @B `eqv` Naive.null @A @B
      it "size" $
        LTR.size @A @B `eqv` Naive.size @A @B
      it "lookupL" $
        LTR.lookupL @A @B `eqv` Naive.lookupL @A @B
      it "lookupR" $
        LTR.lookupR @A @B `eqv` Naive.lookupR @A @B
      it "dom" $
        LTR.dom @A @B `eqv` Naive.dom @A @B
      it "cod" $
        LTR.cod @A @B `eqv` Naive.cod @A @B
      it "domSize" $
        LTR.domSize @A @B `eqv` Naive.domSize @A @B
      it "codSize" $
        LTR.codSize @A @B `eqv` Naive.codSize @A @B

-- # Properties of compose

prop_compose_unit :: Rel A B -> Property
prop_compose_unit r =
  let idA = identity (dom r)
      idB = identity (cod r)
  in (idA #.# r === r) .&&. (r #.# idB === r)

prop_compose_empty :: Rel A B -> Property
prop_compose_empty r =
  let z  = empty :: Rel A B
  in (r #.# empty === z) .&&. (empty #.# r === z)

prop_compose_singleton_eq :: A -> B -> C -> Property
prop_compose_singleton_eq a b c =
  let r = singleton a b :: Rel A B
      s = singleton b c :: Rel B C
  in r #.# s === singleton a c

prop_compose_singleton_neq :: A -> B -> B -> C -> Property
prop_compose_singleton_neq a b b' c =
  let r = singleton a b   :: Rel A B
      s = singleton b' c  :: Rel B C
  in (b /= b') ==> (r #.# s === empty)

prop_compose_associative :: Rel A B -> Rel B C -> Rel C D -> Property
prop_compose_associative r s t =
  (r #.# s) #.# t === r #.# (s #.# t)

prop_compose_distributive :: Rel A B -> Rel B C -> Rel B C -> Property
prop_compose_distributive r s t =
  (r #.# (s `union` t) === (r #.# s) `union` (r #.# t))

spec_compose :: Spec
spec_compose =
  describe "compose" $ do
    it "has identity relation." $
      property $ prop_compose_unit
    it "has absorbing element, namely 'empty'." $
      property $ prop_compose_empty
    context "When composing (singleton a b) and (singleton b' c)" $ do
      it "returns (singleton a c) if (b == b')" $
        property $ prop_compose_singleton_eq
      it "returns empty           if (b /= b')" $
        property $ prop_compose_singleton_neq
    it "satisfies associativity law." $
      property $ prop_compose_associative
    it "distributes over union." $
      property $ prop_compose_distributive

-- * Properties of transpose

prop_transpose_involutive :: Rel A B -> Property
prop_transpose_involutive r = r === transpose (transpose r)

prop_transpose_involutive_on_compose :: Rel A B -> Rel B C -> Property
prop_transpose_involutive_on_compose r s =
  transpose (r #.# s) === transpose s #.# transpose r

spec_transpose :: Spec
spec_transpose =
  describe "transpose" $ do
    it "is involution (its inverse is itself)." $
      property $ prop_transpose_involutive
    it "is involution on the monoid formed by composition" $
      property $ prop_transpose_involutive_on_compose

-- * All specs

spec :: Spec
spec = do
  spec_compatibility
  spec_compose
  spec_transpose

