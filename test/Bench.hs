{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.DeepSeq
import GHC.Generics(Generic)

import Data.List (sort)
import qualified Data.Set as Set
import qualified Data.Vector as V
import Data.Word

import qualified System.Random.MWC                as R
import           Control.Monad.Primitive

import           Data.Relation.LTR (Rel)
import qualified Data.Relation.LTR as LTR
import           Data.Relation.Naive (NaiveRel)
import qualified Data.Relation.Naive as Naive

import Gauge

seed :: Word32 -> V.Vector Word32
seed w = V.fromList [947359211, 327481, w]

data DataSet r =
  DataSet
    { oneA :: Int
    , oneB :: Int
    , setA :: Set.Set Int
    , setB :: Set.Set Int
    , bare :: [(Int,Int)]
    , drel :: r -- ^ dense  (O(n^2))
    , srel :: r -- ^ sparse (O(n))
    , frel :: r -- ^ functional
    , rect1 :: r -- ^ small a, large b, dense
    , rect2 :: r -- ^ large a, small b, dense
    }
  deriving (Show, Eq, Functor, Generic)

instance NFData r => NFData (DataSet r)

genDataSet :: Int -> IO (DataSet [(Int, Int)])
genDataSet n | n <= 0 = error "size must be positive"
genDataSet n =
  do g <- R.initialize (seed (fromIntegral n))
     oneA <- R.uniformR (0, n-1) g
     oneB <- R.uniformR (0, n-1) g
     setA <- Set.fromList <$> genVec g n n2
     setB <- Set.fromList <$> genVec g n n2
     bare <- genRel g n n nn20
     drel <- genRel g n n nn10
     srel <- genRel g n n (2*n)
     frel <- zip [0..n-1] <$> genVec g n n
     rect1 <- genRel g n3 (3*n) nn20
     rect2 <- genRel g (3*n) n3 nn20
     let di = DataSet{..}
     di `deepseq` return di
  where
    n2 = max 1 (n `div` 2)
    n3 = max 1 (n `div` 3)
    nn10 = max 1 ((n * n) `div` 10)
    nn20 = max 1 ((n * n) `div` 20)
    genRel g a b m = zip <$> genVec g a m <*> genVec g b m

genVec :: (PrimMonad m) => R.Gen (PrimState m) -> Int -> Int -> m [Int]
genVec g range len = loop len []
  where
    loop 0 acc = return acc
    loop n acc = R.uniformR (0, range-1) g >>= \a -> loop (n-1) (a:acc)

benchLTR :: Benchmark
benchLTR =
  bgroup "LTR" $
    map sizedBenchLTR [25,50,100,200,400,800]
  where
    sizedBenchLTR n =
      env (fmap LTR.fromList <$> genDataSet n)
          (bgroup (show n) . benchLTRFor)

benchLTRFor :: DataSet (Rel Int Int) -> [Benchmark]
benchLTRFor ~DataSet{oneA=a,oneB=b,drel=d,srel=s,frel=f,..} =
  [ bench "null d" (whnf LTR.null d)
  , bench "size d" (whnf LTR.size d)
  , bench "member a b d" (whnf (LTR.member a b) d)
  , bench "member a b s" (whnf (LTR.member a b) s)
  , bench "member a b f" (whnf (LTR.member a b) f)
  , bench "a .# d" (whnf (a LTR..#) d)
  , bench "d #. a" (whnf (LTR.#. a) d)
  , bench "dom d" (whnf LTR.dom d)
  , bench "ran d" (whnf LTR.ran d)
  , bench "domSize d" (whnf LTR.domSize d)
  , bench "ranSize d" (whnf LTR.ranSize d)
  
  , bench "singleton a b" (whnf (LTR.singleton a) b)
  , bench "identity setA" (whnf LTR.identity setA)
  , bench "full setA setB" (whnf  (LTR.full setA) setB)
  , bench "fromList" $ whnf LTR.fromList bare
  , bench "fromAscList" $ whnf LTR.fromAscList sortBare
  , bench "fromDistinctAscList" $ whnf LTR.fromDistinctAscList nubSortBare
  , bench "fromSet d" $ whnf LTR.fromSet setBare
  
  , bench "union s s" (whnf (LTR.union s) s)
  , bench "union d s" (whnf (LTR.union d) s)
  , bench "union d d" (whnf (LTR.union d) d)
  , bench "difference s s" (whnf (LTR.difference s) s)
  , bench "difference d s" (whnf (LTR.difference d) s)
  , bench "difference d d" (whnf (LTR.difference d) d)
  , bench "intersection s s" (whnf (LTR.intersection s) s)
  , bench "intersection d s" (whnf (LTR.intersection d) s)
  , bench "intersection d d" (whnf (LTR.intersection d) d)
  , bench "compose s s" (whnf (LTR.compose s) s)
  , bench "compose d s" (whnf (LTR.compose d) s)
  , bench "compose d d" (whnf (LTR.compose d) d)
  , bench "compose f d" (whnf (LTR.compose f) d)
  , bench "compose d f" (whnf (LTR.compose d) f)
  , bench "compose r1 r2" (whnf (LTR.compose rect1) rect2)
  , bench "compose r2 r1" (whnf (LTR.compose rect2) rect1)
  , bench "transpose r1" (whnf LTR.transpose rect1)
  , bench "transpose r2" (whnf LTR.transpose rect2)
  
  , bench "filter p d" (whnf (LTR.filter p) d)
  , bench "partition p d" (whnf (bothNull . LTR.partition p) d)
  
  ]
  where
    p x y = even (x + y)
    bothNull (x, y) = LTR.null x && LTR.null y
    sortBare = sort bare
    nubSortBare = Set.toAscList setBare
    setBare = Set.fromList bare

benchNaive :: Benchmark
benchNaive =
  bgroup "Naive" $
    map sizedBenchNaive [25,50,100,200]
  where
    sizedBenchNaive n =
      env (fmap Naive.fromList <$> genDataSet n)
          (bgroup (show n) . benchNaiveFor)

benchNaiveFor :: DataSet (NaiveRel Int Int) -> [Benchmark]
benchNaiveFor ~DataSet{oneA=a,oneB=b,drel=d,srel=s,frel=f,..} =
  [ bench "null d" (whnf Naive.null d)
  , bench "size d" (whnf Naive.size d)
  , bench "member a b d" (whnf (Naive.member a b) d)
  , bench "member a b s" (whnf (Naive.member a b) s)
  , bench "member a b f" (whnf (Naive.member a b) f)
  , bench "a .# d" (whnf (a Naive..#) d)
  , bench "d #. a" (whnf (Naive.#. a) d)
  , bench "dom d" (whnf Naive.dom d)
  , bench "ran d" (whnf Naive.ran d)
  , bench "domSize d" (whnf Naive.domSize d)
  , bench "ranSize d" (whnf Naive.ranSize d)
  
  , bench "singleton a b" (whnf (Naive.singleton a) b)
  , bench "identity setA" (whnf Naive.identity setA)
  , bench "full setA setB" (whnf  (Naive.full setA) setB)
  , bench "fromList" $ whnf Naive.fromList bare
  , bench "fromAscList" $ whnf Naive.fromAscList sortBare
  , bench "fromDistinctAscList" $ whnf Naive.fromDistinctAscList nubSortBare
  , bench "fromSet d" $ whnf Naive.fromSet setBare
  
  , bench "union s s" (whnf (Naive.union s) s)
  , bench "union d s" (whnf (Naive.union d) s)
  , bench "union d d" (whnf (Naive.union d) d)
  , bench "difference s s" (whnf (Naive.difference s) s)
  , bench "difference d s" (whnf (Naive.difference d) s)
  , bench "difference d d" (whnf (Naive.difference d) d)
  , bench "intersection s s" (whnf (Naive.intersection s) s)
  , bench "intersection d s" (whnf (Naive.intersection d) s)
  , bench "intersection d d" (whnf (Naive.intersection d) d)
  , bench "compose s s" (whnf (Naive.compose s) s)
  , bench "compose d s" (whnf (Naive.compose d) s)
  , bench "compose d d" (whnf (Naive.compose d) d)
  , bench "compose f d" (whnf (Naive.compose f) d)
  , bench "compose d f" (whnf (Naive.compose d) f)
  , bench "compose r1 r2" (whnf (Naive.compose rect1) rect2)
  , bench "compose r2 r1" (whnf (Naive.compose rect2) rect1)
  , bench "transpose r1" (whnf Naive.transpose rect1)
  , bench "transpose r2" (whnf Naive.transpose rect2)
  
  , bench "filter p d" (whnf (Naive.filter p) d)
  , bench "partition p d" (whnf (bothNull . Naive.partition p) d)
  
  ]
  where
    p x y = even (x + y)
    bothNull (x, y) = Naive.null x && Naive.null y
    sortBare = sort bare
    nubSortBare = Set.toAscList setBare
    setBare = Set.fromList bare

main :: IO ()
main = defaultMain [benchLTR, benchNaive]
