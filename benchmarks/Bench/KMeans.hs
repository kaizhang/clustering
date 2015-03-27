module Bench.KMeans
    ( benchKMeans ) where

import Criterion.Main
import qualified Data.Matrix.Unboxed as MU
import qualified Data.Vector.Unboxed as U
import System.Random.MWC
import System.IO.Unsafe

import AI.Clustering.KMeans

import Bench.Utils

gen :: GenIO
gen = unsafePerformIO createSystemRandom

dat :: MU.Matrix Double
dat = unsafePerformIO $ fmap MU.fromRows $ randVectors 10000 10

benchKMeans :: Benchmark
benchKMeans = bgroup "KMeans clustering"
    [ bgroup "AI.Clustering.KMeans"
        [ bench "k-means++ (n = 1000, k = 7)" $
            whnfIO $ kmeans' gen KMeansPP 7 dat
        , bench "forgy (n = 1000, k = 7)" $
            whnfIO $ kmeans' gen Forgy 7 dat
        ]
    ]

kmeans' :: GenIO -> Initialization -> Int -> MU.Matrix Double -> IO (U.Vector Int)
kmeans' g method k = fmap _clusters . kmeans g method k
