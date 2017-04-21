{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
module Common(
  R(..), A(..), B(..), C(..), D(..),
  Proxy(..),
  spec_compatibility,
  spec_compose,
  spec_transpose,
  spec_all
) where

import           Test.Hspec
import           Test.QuickCheck

import           Data.Proxy

import           Data.Function       (on)
import           Data.Set            (Set)

import           Data.Relation.Class
import           Data.Relation.Naive

{- Wrapper -}
newtype R t a b = R (t a b)
                deriving (Relation)

instance (Relation t, Ord a, Show a, Ord b, Show b) => Show (R t a b) where
  show r = show (toSet r)

instance (Relation t, Ord a, Ord b) => Eq (R t a b) where
  (==) = (==) `on` toSet
instance (Relation t, Ord a, Ord b) => Ord (R t a b) where
  compare = compare `on` toSet

instance (Relation t, Arbitrary a, Ord a, Arbitrary b, Ord b) =>
         Arbitrary (R t a b) where
  arbitrary = fromList <$> relDataOf arbitrary
  shrink r = fromSet <$> shrink (toSet r)

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


fromNaive :: (Relation t, Ord a, Ord b) => R NaiveRel a b -> R t a b
fromNaive = fromSet . toSet

toNaive :: (Relation t, Ord a, Ord b) => R t a b -> R NaiveRel a b
toNaive   = fromSet . toSet

compat1 ::
  forall t a b a' b'.
    (Relation t, Ord a, Ord b, Ord a', Ord b', Show a', Show b') =>
    (forall u. Relation u => u a b -> u a' b') ->
    R t a b -> Property
compat1 op r = toNaive (op r) === op (toNaive r)

compat2 ::
  forall t a b a' b' a'' b''.
    (Relation t, Ord a, Ord b, Ord a', Ord b', Ord a'', Ord b'', Show a'', Show b'') =>
    (forall u. Relation u => u a b -> u a' b' -> u a'' b'') ->
     R t a b -> R t a' b' -> Property
compat2 op r s = toNaive (op r s) === op (toNaive r) (toNaive s)

-- * Equivalence to naive implementation

prop_naive_bijection ::
  forall t. (Relation t) =>
    Proxy t -> R NaiveRel A B -> R t A B -> Property
prop_naive_bijection _ naive r = isBijection fromNaive toNaive naive r

prop_empty ::
  forall t. (Relation t) =>
    Proxy t -> Property
prop_empty _ = toNaive (empty :: R t A B) === empty

prop_singleton ::
  forall t. (Relation t) =>
    Proxy t -> A -> B -> Property
prop_singleton _ a b = toNaive @t (singleton a b) === singleton a b

prop_identity ::
  forall t. (Relation t) =>
    Proxy t -> Set A -> Property
prop_identity _ as = toNaive @t (identity as) === identity as

prop_full ::
  forall t. (Relation t) =>
    Proxy t -> Set A -> Set B -> Property
prop_full _ as bs = toNaive @t (full as bs) === full as bs

prop_union, prop_intersection, prop_difference ::
  forall t. (Relation t) =>
    Proxy t -> R t A B -> R t A B -> Property
prop_union        _ = compat2 @t union
prop_intersection _ = compat2 @t intersection
prop_difference   _ = compat2 @t difference

prop_compose ::
  forall t. (Relation t) =>
    Proxy t -> R t A B -> R t B C -> Property
prop_compose _ = compat2 @t compose

prop_transpose ::
  forall t. (Relation t) =>
    Proxy t -> R t A B -> Property
prop_transpose _ = compat1 @t transpose

spec_compatibility ::
  forall t. (Relation t) => Proxy t -> Spec
spec_compatibility t =
  context "Equivalence to naive implementation NaiveRel" $ do
    it "has bijection onto NaiveRel" $
      property $ prop_naive_bijection t
    it "has equivalent 'empty'" $
      property $ prop_empty t
    it "has equivalent 'singleton'" $
      property $ prop_singleton t
    it "has equivalent 'identity'" $
      property $ prop_identity t
    it "has equivalent 'full'" $
      property $ prop_full t
    it "has equivalent 'union'" $
      property $ prop_union t
    it "has equivalent 'intersection'" $
      property $ prop_intersection t
    it "has equivalent 'difference'" $
      property $ prop_difference t
    it "has equivalent 'compose'" $
      property $ prop_compose t
    it "has equivalent 'transpose'" $
      property $ prop_transpose t

-- * Properties of compose

prop_compose_unit :: (Relation t) =>
  Proxy t -> R t A B -> Property
prop_compose_unit _ r =
  let idA = identity (dom r)
      idB = identity (cod r)
  in (idA #.# r === r) .&&. (r #.# idB === r)

prop_compose_empty ::  forall t. (Relation t) =>
  Proxy t -> R t A B -> Property
prop_compose_empty _ r =
  let z  = empty :: R t A B
  in (r #.# empty === z) .&&. (empty #.# r === z)

prop_compose_singleton_eq :: forall t. (Relation t) =>
  Proxy t -> A -> B -> C -> Property
prop_compose_singleton_eq _ a b c =
  let r = singleton a b :: R t A B
      s = singleton b c :: R t B C
  in r #.# s === singleton a c

prop_compose_singleton_neq :: forall t. (Relation t) =>
  Proxy t -> A -> B -> B -> C -> Property
prop_compose_singleton_neq _ a b b' c =
  let r = singleton a b   :: R t A B
      s = singleton b' c  :: R t B C
  in (b /= b') ==> (r #.# s === empty)

prop_compose_associative :: (Relation t) =>
  Proxy t -> R t A B -> R t B C -> R t C D -> Property
prop_compose_associative _ r s t =
  (r #.# s) #.# t === r #.# (s #.# t)

prop_compose_distributive :: (Relation t) =>
  Proxy t -> R t A B -> R t B C -> R t B C -> Property
prop_compose_distributive _ r s t =
  (r #.# (s `union` t) === (r #.# s) `union` (r #.# t))

spec_compose :: forall t. (Relation t) => Proxy t -> Spec
spec_compose t =
  describe "compose" $ do
    it "has identity relation." $
      property $ prop_compose_unit t
    it "has absorbing element, namely 'empty'." $
      property $ prop_compose_empty t
    context "When composing (singleton a b) and (singleton b' c)" $ do
      it "returns (singleton a c) if (b == b')" $
        property $ prop_compose_singleton_eq t
      it "returns empty           if (b /= b')" $
        property $ prop_compose_singleton_neq t
    it "satisfies associativity law." $
      property $ prop_compose_associative t
    it "distributes over union." $
      property $ prop_compose_distributive t

-- * Properties of transpose

prop_transpose_involutive :: (Relation t) =>
  Proxy t -> R t A B -> Property
prop_transpose_involutive _ r = r === transpose (transpose r)

prop_transpose_involutive_on_compose :: (Relation t) =>
  Proxy t -> R t A B -> R t B C -> Property
prop_transpose_involutive_on_compose _ r s =
  transpose (r #.# s) === transpose s #.# transpose r

spec_transpose :: forall t. (Relation t) => Proxy t -> Spec
spec_transpose t =
  describe "transpose" $ do
    it "is involution (its inverse is itself)." $
      property $ prop_transpose_involutive t
    it "is involution on the monoid formed by composition" $
      property $ prop_transpose_involutive_on_compose t

-- * All specs

spec_all :: forall t. (Relation t) => Proxy t -> Spec
spec_all t = do
  spec_compatibility t
  spec_compose t
  spec_transpose t
