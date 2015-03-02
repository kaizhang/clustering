{-# LANGUAGE FlexibleContexts #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  $Header$
-- Description :  <optional short text displayed on contents page>
-- Copyright   :  (c) Kai Zhang
-- License     :  MIT

-- Maintainer  :  kai@kzhang.org
-- Stability   :  experimental
-- Portability :  portable

-- <module description starting at first column>
--------------------------------------------------------------------------------

module AI.Clustering.Hierarchical
    ( Dendrogram(..)
    , size
    , cutAt
    , members
    , Metric(..)
    , hclust
    , computeDists
    , euclidean
    , nnChain
    , complete
    , average
    ) where

import Control.Applicative ((<$>))
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

import AI.Clustering.Hierarchical.Internal
import AI.Clustering.Hierarchical.Types

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

computeDists :: G.Vector v a => DistFn a -> v a -> DistanceMat
computeDists f vec = DistanceMat n . U.fromList . flip concatMap [0..n-1] $ \i ->
    flip map [i+1..n-1] $ \j -> f (vec `G.unsafeIndex` i) (vec `G.unsafeIndex` j)
  where
    n = G.length vec
{-# INLINE computeDists #-}

euclidean :: G.Vector v Double => DistFn (v Double)
euclidean xs ys = sqrt $ G.sum $ G.zipWith (\x y -> (x-y)**2) xs ys
{-# INLINE euclidean #-}
