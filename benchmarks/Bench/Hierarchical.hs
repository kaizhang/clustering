module Bench.Hierarchical
    ( benchHierarchical ) where

import Criterion.Main
import qualified Data.Clustering.Hierarchical as C
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import System.IO.Unsafe (unsafePerformIO)

import AI.Clustering.Hierarchical
import AI.Clustering.Hierarchical.Types

import Bench.Utils

benchHierarchical :: Benchmark
benchHierarchical =
    let dists = computeDists euclidean xs
        fn i j = dists ! (i,j)
        xs = V.fromList $ unsafePerformIO $ randVectors 1000 5
    in bgroup "Hierarchical clustering"
        [ bgroup "AI.Clustering.Hierarchical"
            [ bench "Average Linkage (n = 10)" $
                  whnf (\x -> hclust Average x fn) $! U.enumFromN 0 10
            , bench "Average Linkage (n = 100)" $
                  whnf (\x -> hclust Average x fn) $! U.enumFromN 0 100
            , bench "Average Linkage (n = 500)" $
                  whnf (\x -> hclust Average x fn) $! U.enumFromN 0 500
            ]
        , bgroup "Data.Clustering.Hierarchical"
            [ bench "Average Linkage (n = 10)" $
                  whnf (\x -> C.dendrogram C.UPGMA x fn) $! [0..9]
            , bench "Average Linkage (n = 100)" $
                  whnf (\x -> C.dendrogram C.UPGMA x fn) $! [0..99]
            , bench "Average Linkage (n = 500)" $
                  whnf (\x -> C.dendrogram C.UPGMA x fn) $! [0..499]
            ]

        , bgroup "Distance matrix"
            [ bench "computeDists" $
                  whnf (\x -> computeDists euclidean x) xs
            , bench "computeDists'" $
                  whnf (\x -> computeDists' euclidean x) xs
            ]
        ]
