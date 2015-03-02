import Control.Monad (replicateM)
import Criterion.Main
import qualified Data.Clustering.Hierarchical as C
import qualified Data.Vector as V
import System.Random.MWC

import AI.Clustering.Hierarchical
import AI.Clustering.Hierarchical.Types ((!))

randSample :: IO [V.Vector Double]
randSample = do
    g <- create
    replicateM 2000 $ uniformVector g 5

main :: IO ()
main = do
    xs <- randSample
    let dists = computeDists euclidean $ V.fromList xs
        fn i j = dists ! (i,j)
    defaultMain
      [ bgroup "AI.Clustering.Hierarchical"
          [ bench "Average Linkage (n = 10)" $
                whnf (\x -> hclust Average x fn) $! V.enumFromN 0 10
          , bench "Average Linkage (n = 100)" $
                whnf (\x -> hclust Average x fn) $! V.enumFromN 0 100
          , bench "Average Linkage (n = 1000)" $
                whnf (\x -> hclust Average x fn) $! V.enumFromN 0 1000
          ]

      , bgroup "Data.Clustering.Hierarchical"
          [ bench "Average Linkage (n = 10)" $
                whnf (\x -> C.dendrogram C.UPGMA x euclidean) $! take 10 xs
          , bench "Average Linkage (n = 100)" $
                whnf (\x -> C.dendrogram C.UPGMA x euclidean) $! take 100 xs
          , bench "Average Linkage (n = 1000)" $
                whnf (\x -> C.dendrogram C.UPGMA x euclidean) $! take 1000 xs
          ]
      ]
