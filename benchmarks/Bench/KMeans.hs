module Bench.KMeans
    ( benchKMeans ) where

import           Criterion.Main
import qualified Data.Matrix.Unboxed  as MU
import qualified Data.Vector.Unboxed  as U
import           Data.Word
import           System.IO.Unsafe
import           System.Random.MWC

import           AI.Clustering.KMeans
import           Bench.Utils

gen :: U.Vector Word32
gen = unsafePerformIO $ do
    g <- createSystemRandom
    fmap fromSeed $ save g

matrix_1000_10 :: MU.Matrix Double
matrix_1000_10 = unsafePerformIO $ fmap MU.fromRows $ randVectors 1000 10

matrix_30000_50 :: MU.Matrix Double
matrix_30000_50 = unsafePerformIO $ fmap MU.fromRows $ randVectors 30000 50

benchKMeans :: Benchmark
benchKMeans = bgroup "KMeans clustering"
    [ bgroup "AI.Clustering.KMeans"
        [ bench "k-means++ (size = 1000 X 10, k = 7)" $
            whnf ( \x -> membership $ kmeans 7 x defaultKMeansOpts
                { kmeansMethod = KMeansPP
                , kmeansSeed = gen } ) matrix_1000_10
        , bench "forgy (size = 1000 X 10, k = 7)" $
            whnf ( \x -> membership $ kmeans 7 x defaultKMeansOpts
                { kmeansMethod = Forgy
                , kmeansSeed = gen } ) matrix_1000_10
        , bench "k-means++ (size = 30000 X 50, k = 10)" $
            whnf ( \x -> membership $ kmeans 10 x defaultKMeansOpts
                { kmeansMethod = KMeansPP
                , kmeansSeed = gen } ) matrix_30000_50
        , bench "forgy (size = 30000 X 50, k = 10)" $
            whnf ( \x -> membership $ kmeans 10 x defaultKMeansOpts
                { kmeansMethod = Forgy
                , kmeansSeed = gen } ) matrix_30000_50
        ]
    ]
