{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Common where

import           Test.QuickCheck

import           Data.Set        (Set)
import qualified Data.Set        as Set

newtype A = A Int
          deriving (Show, Eq, Ord, Enum, Arbitrary)

newtype B = B Int
          deriving (Show, Eq, Ord, Enum, Arbitrary)

newtype C = C Int
          deriving (Show, Eq, Ord, Enum, Arbitrary)

newtype D = D Int
          deriving (Show, Eq, Ord, Enum, Arbitrary)

sqrti :: (Integral a) => a -> a
sqrti = floor . (sqrt :: Double -> Double) . fromIntegral

relDataOf :: Gen (a,b) -> Gen [(a,b)]
relDataOf gen = listOf (scale sqrti gen)

isBijection :: (Eq a, Show a, Eq b, Show b) =>
  (a -> b) -> (b -> a) -> a -> b -> Property
isBijection f g = \a b -> a === g (f a) .&. b === f (g b)

commutes :: (Eq b, Show b) =>
  (a -> b) -> (a -> a) -> (b -> b) -> a -> Property
commutes f opA opB = \a -> f (opA a) === opB (f a)

commutes2 :: (Eq b, Show b) =>
  (a -> b) -> (a -> a -> a) -> (b -> b -> b) -> a -> a -> Property
commutes2 f opA opB = \a1 a2 -> f (opA a1 a2) === opB (f a1) (f a2)

idRel :: Set x -> Set (x,x)
idRel = Set.mapMonotonic (\x -> (x,x))
