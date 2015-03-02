import Control.Monad (replicateM)
import Criterion.Main
import qualified Data.Clustering.Hierarchical as C
import qualified Data.Vector as V
import System.Random.MWC

import AI.Clustering.Hierarchical

randSample :: IO [V.Vector Double]
randSample = do
    g <- create
    replicateM 10000 $ uniformVector g 5

main :: IO ()
main = do
    xs <- randSample
    defaultMain
      [ bgroup "Hierarchical clustering:"
          [ bench "Average Linkage (n = 10)" $
                whnf (\x -> nnChain x average) $! computeDists euclidean . V.fromList . take 10 $ xs
          , bench "Average Linkage (n = 100)" $
                whnf (\x -> nnChain x average) $! computeDists euclidean . V.fromList . take 100 $ xs
          , bench "Average Linkage (n = 1000)" $
                whnf (\x -> nnChain x average) $! computeDists euclidean . V.fromList . take 1000 $ xs
          ]

{-
      , bgroup "Hierarchical clustering (slow):"
          [ bench "Average Linkage (n = 10)" $
                whnf (\x -> C.dendrogram C.UPGMA x euclidean) $! take 10 xs
          , bench "Average Linkage (n = 100)" $
                whnf (\x -> C.dendrogram C.UPGMA x euclidean) $! take 100 xs
          , bench "Average Linkage (n = 1000)" $
                whnf (\x -> C.dendrogram C.UPGMA x euclidean) $! take 1000 xs
          ]
-}

      ]
