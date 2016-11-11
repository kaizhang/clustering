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

dat :: MU.Matrix Double
dat = unsafePerformIO $ fmap MU.fromRows $ randVectors 1000 10

benchKMeans :: Benchmark
benchKMeans = bgroup "KMeans clustering"
    [ bgroup "AI.Clustering.KMeans"
        [ bench "k-means++ (n = 1000, k = 7)" $
            whnf ( \x -> membership $ kmeans 7 x defaultKMeansOpts
                { kmeansMethod = KMeansPP
                , kmeansSeed = gen } ) dat
        , bench "forgy (n = 1000, k = 7)" $
            whnf ( \x -> membership $ kmeans 7 x defaultKMeansOpts
                { kmeansMethod = KMeansPP
                , kmeansSeed = gen } ) dat
        ]
    ]
