{-# LANGUAGE FlexibleContexts #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  AI.Clustering.Hierarchical
-- Copyright   :  (c) 2015 Kai Zhang
-- License     :  MIT
-- Maintainer  :  kai@kzhang.org
-- Stability   :  experimental
-- Portability :  portable
--
-- High performance agglomerative hierarchical clustering library. Example:
--
-- >>> :set -XOverloadedLists
-- >>> import qualified Data.Vector as V
-- >>> let points = [[2, 3, 4], [2, 1, 2], [2, 1, 6], [2, 4, 6], [5, 1, 2]] :: V.Vector (V.Vector Double)
-- >>> let dendro = hclust Average points euclidean
-- >>> print dendro
-- Branch 5 4.463747440868191 (Branch 3 2.914213562373095 (Leaf (fromList [2.0,1.0,6.0]))
-- (Branch 2 2.23606797749979 (Leaf (fromList [2.0,3.0,4.0])) (Leaf (fromList [2.0,4.0,6.0]))))
-- (Branch 2 3.0 (Leaf (fromList [2.0,1.0,2.0])) (Leaf (fromList [5.0,1.0,2.0])))
-- >>> putStr $ drawDendrogram $ fmap show dendro
-- h: 4.4637
-- |
-- +- h: 2.9142
-- |  |
-- |  +- fromList [2.0,1.0,6.0]
-- |  |
-- |  `- h: 2.2361
-- |     |
-- |     +- fromList [2.0,3.0,4.0]
-- |     |
-- |     `- fromList [2.0,4.0,6.0]
-- |
-- `- h: 3.0000
--    |
--    +- fromList [2.0,1.0,2.0]
--    |
--    `- fromList [5.0,1.0,2.0]
--
--------------------------------------------------------------------------------
module AI.Clustering.Hierarchical
    ( Dendrogram(..)
    , size
    , Metric(..)
    , hclust
    , cutAt
    , flatten
    , drawDendrogram
    , computeDists
    , euclidean

    -- * References
    -- $references
    ) where


import Control.Applicative ((<$>))
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import Text.Printf (printf)

import AI.Clustering.Hierarchical.Internal
import AI.Clustering.Hierarchical.Types

-- | Different hierarchical clustering schemes.
data Metric = Single    -- ^ O(n^2) Single linkage, $d(A,B) = min_{a \in A, b \in B} d(a,b)$.
            | Complete  -- ^ O(n^2) Complete linkage, $d(A,B) = max_{a \in A, b \in B} d(a,b)$.
            | Average   -- ^ O(n^2) Average linkage or UPGMA, $d(A,B) = \frac{\sum_{a \in A}\sum_{b \in B}d(a,b)}{|A||B|}$.
            | Weighted  -- ^ O(n^2) Weighted linkage.
            | Ward      -- ^ O(n^2) Ward's method.
            | Centroid  -- ^ O(n^3) Centroid linkage, not implemented.
            | Median    -- ^ O(n^3) Median linkage, not implemented.

-- | Perform hierarchical clustering.
hclust :: G.Vector v a => Metric -> v a -> DistFn a -> Dendrogram a
hclust method xs f = label <$> nnChain dists fn
  where
    dists = computeDists f xs
    label i = xs G.! i
    fn = case method of
        Single -> single
        Complete -> complete
        Average -> average
        Weighted -> weighted
        Ward -> ward
        _ -> error "Not implemented"

-- | Cut a dendrogram at given height.
cutAt :: Dendrogram a -> Distance -> [Dendrogram a]
cutAt dendro th = go [] dendro
  where
    go acc x@(Leaf _) = x : acc
    go acc x@(Branch _ d l r) | d <= th = x : acc
                              | otherwise = go (go acc r) l

-- | Return the elements of a dendrogram in pre-order.
flatten :: Dendrogram a -> [a]
flatten (Leaf x) = [x]
flatten (Branch _ _ l r) = flatten l ++ flatten r

-- | 2-dimensional drawing of a dendrogram
drawDendrogram :: Dendrogram String -> String
drawDendrogram = unlines . draw
  where
    draw (Branch _ d l r) =
        printf "h: %.4f" d
      : "|"
      : shift "+- " "|  " (draw l) ++ shift "`- " "   " (draw r)
    draw (Leaf x) = [x,""]
    shift first other = zipWith (++) (first : repeat other)

computeDists :: G.Vector v a => DistFn a -> v a -> DistanceMat
computeDists f vec = DistanceMat n . U.fromList . flip concatMap [0..n-1] $ \i ->
    flip map [i+1..n-1] $ \j -> f (vec `G.unsafeIndex` i) (vec `G.unsafeIndex` j)
  where
    n = G.length vec
{-# INLINE computeDists #-}

-- | compute euclidean distance between two points
euclidean :: G.Vector v Double => DistFn (v Double)
euclidean xs ys = sqrt $ G.sum $ G.zipWith (\x y -> (x-y)**2) xs ys
{-# INLINE euclidean #-}

-- $references
--
-- Müllner D (2011). Modern Hierarchical, Agglomerative Clustering Algorithms.
-- ArXiv:1109.2378 [stat.ML]. <http://arxiv.org/abs/1109.2378>
