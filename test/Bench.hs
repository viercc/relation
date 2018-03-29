{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.DeepSeq
import GHC.Generics(Generic)

import Data.List (sort)
import qualified Data.Set as Set
import qualified Data.Vector as V
import Data.Word

import qualified System.Random.MWC                as R

import           Data.Relation.LTR (Rel)
import qualified Data.Relation.LTR as LTR
import           Data.Relation.Naive (NaiveRel)
import qualified Data.Relation.Naive as Naive

import Gauge

seed :: Word32 -> V.Vector Word32
seed w = V.fromList [947359211, 327481, w]

data DataSet =
  DataSet
    { idxA :: Int
    , idxB :: Int
    , idxSet :: Set.Set Int
    , drel :: [(Int, Int)] -- ^ dense  (O(n^2))
    , srel :: [(Int, Int)] -- ^ sparse (O(n))
    , frel :: [(Int, Int)] -- ^ functional
    , rect1 :: [(Int, Int)] -- ^ small a, large b, dense
    , rect2 :: [(Int, Int)] -- ^ large a, small b, dense
    }
  deriving (Show, Eq, Generic)

instance NFData DataSet

type DataSetFor r = (DataSet, r, r, r, r, r)

genDataSet :: Int -> IO DataSet
genDataSet n | n <= 0 = error "size must be positive"
genDataSet n =
  do g <- R.initialize (seed (fromIntegral n))
     idxA <- R.uniformR (0, n-1) g
     idxB <- R.uniformR (0, n-1) g
     idxSet <- Set.fromList <$> genVec g n n2
     drel <- genRel g n n nn10
     srel <- genRel g n n (3*n)
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
    genVec g a m = map (`mod` a) . V.toList <$> R.uniformVector g m
    genRel g a b m = zip <$> genVec g a m <*> genVec g b m

benchLTR :: Benchmark
benchLTR =
  bgroup "LTR" $
    map sizedBenchLTR [25,50,100,200,400,800]
  where
    sizedBenchLTR n =
      env (datasetForLTR <$> genDataSet n)
          (bgroup (show n) . benchLTRFor)

datasetForLTR ::
  DataSet -> DataSetFor (Rel Int Int)
datasetForLTR dataset@DataSet{..} =
  let d = LTR.fromList drel
      s = LTR.fromList srel
      f = LTR.fromList frel
      r1 = LTR.fromList rect1
      r2 = LTR.fromList rect2
  in (dataset,d,s,f,r1,r2)

benchLTRFor :: DataSetFor (Rel Int Int) -> [Benchmark]
benchLTRFor ~(DataSet{..},d,s,f,r1,r2) =
  [ bench "null d" (whnf LTR.null d)
  , bench "size d" (whnf LTR.size d)
  , bench "member a b d" (whnf (LTR.member idxA idxB) d)
  , bench "member a b s" (whnf (LTR.member idxA idxB) s)
  , bench "member a b f" (whnf (LTR.member idxA idxB) f)
  , bench "lookupL a d" (whnf (LTR.lookupL idxA) d)
  , bench "lookupR a d" (whnf (`LTR.lookupR` idxA) d)
  , bench "dom d" (whnf LTR.dom d)
  , bench "ran d" (whnf LTR.ran d)
  , bench "domSize d" (whnf LTR.domSize d)
  , bench "ranSize d" (whnf LTR.ranSize d)

  , bench "identity aSet" (whnf LTR.identity idxSet)
  , bench "full aSet aSet" (whnf  (LTR.full idxSet) idxSet)

  , bench "union s s" (whnf (LTR.union s) s)
  , bench "union s d" (whnf (LTR.union d) s)
  , bench "union d d" (whnf (LTR.union d) d)
  , bench "difference s s" (whnf (LTR.difference s) s)
  , bench "difference s d" (whnf (LTR.difference d) s)
  , bench "difference d d" (whnf (LTR.difference d) d)
  , bench "intersection s s" (whnf (LTR.intersection s) s)
  , bench "intersection s d" (whnf (LTR.intersection d) s)
  , bench "intersection d d" (whnf (LTR.intersection d) d)
  , bench "compose s s" (whnf (LTR.compose s) s)
  , bench "compose s d" (whnf (LTR.compose d) s)
  , bench "compose d d" (whnf (LTR.compose d) d)
  , bench "compose r1 r2" (whnf (LTR.compose r1) r2)
  , bench "compose r2 r1" (whnf (LTR.compose r2) r1)
  , bench "transpose r1" (whnf LTR.transpose r1)
  , bench "transpose r2" (whnf LTR.transpose r2)

  , bench "filter p d" (whnf (LTR.filter p) d)
  , bench "partition p d" (whnf (LTR.filter p) d)
  
  , env (return (Set.fromList drel)) $
     \dSet -> bench "fromSet d" (whnf LTR.fromSet dSet)
  , env (return (Set.toAscList $ Set.fromList drel)) $
     \ds -> bench "fromDistinctAscList d" $
       whnf LTR.fromDistinctAscList ds
  , env (return (sort drel)) $
     \ds -> bench "fromAscList d" $
       whnf LTR.fromAscList ds
  , bench "fromList d" $ whnf LTR.fromList drel
  ]
  where p a b = even (a + b)

benchNaive :: Benchmark
benchNaive =
  bgroup "Naive" $
    map sizedBenchNaive [25,50,100,200,400]
  where
    sizedBenchNaive n =
      env (datasetForNaive <$> genDataSet n)
          (bgroup (show n) . benchNaiveFor)

datasetForNaive ::
  DataSet -> DataSetFor (NaiveRel Int Int)
datasetForNaive dataset@DataSet{..} =
  let d = Naive.fromList drel
      s = Naive.fromList srel
      f = Naive.fromList frel
      r1 = Naive.fromList rect1
      r2 = Naive.fromList rect2
  in (dataset,d,s,f,r1,r2)

benchNaiveFor :: DataSetFor (NaiveRel Int Int) -> [Benchmark]
benchNaiveFor ~(DataSet{..},d,s,f,r1,r2) =
  [ bench "null d" (whnf Naive.null d)
  , bench "size d" (whnf Naive.size d)
  , bench "member a b d" (whnf (Naive.member idxA idxB) d)
  , bench "member a b s" (whnf (Naive.member idxA idxB) s)
  , bench "member a b f" (whnf (Naive.member idxA idxB) f)
  , bench "lookupL a d" (whnf (Naive.lookupL idxA) d)
  , bench "lookupR a d" (whnf (`Naive.lookupR` idxA) d)
  , bench "dom d" (whnf Naive.dom d)
  , bench "ran d" (whnf Naive.ran d)
  , bench "domSize d" (whnf Naive.domSize d)
  , bench "ranSize d" (whnf Naive.ranSize d)

  , bench "identity aSet" (whnf Naive.identity idxSet)
  , bench "full aSet aSet" (whnf  (Naive.full idxSet) idxSet)

  , bench "union s s" (whnf (Naive.union s) s)
  , bench "union s d" (whnf (Naive.union d) s)
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
  , bench "compose r1 r2" (whnf (Naive.compose r1) r2)
  , bench "compose r2 r1" (whnf (Naive.compose r2) r1)
  , bench "transpose r1" (whnf Naive.transpose r1)
  , bench "transpose r2" (whnf Naive.transpose r2)

  , bench "filter p d" (whnf (Naive.filter p) d)
  , bench "partition p d" (whnf (Naive.filter p) d)
  
  , env (return (Set.fromList drel)) $
     \dSet -> bench "fromSet d" (whnf Naive.fromSet dSet)
  , env (return (Set.toAscList $ Set.fromList drel)) $
     \ds -> bench "fromDistinctAscList d" $
       whnf Naive.fromDistinctAscList ds
  , env (return (sort drel)) $
     \ds -> bench "fromAscList d" $
       whnf Naive.fromAscList ds
  , bench "fromList d" $ whnf Naive.fromList drel
  ]
  where p a b = even (a + b)

main :: IO ()
main = defaultMain [benchLTR, benchNaive]
