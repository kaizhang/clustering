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
    , Metric(..)
    , dendrogram
    , euclidean
    ) where

import Control.Monad (forM_, when)
import Data.List (foldl')
import qualified Data.Map as M
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

type Distance = Double
type DistFn a = a -> a -> Distance
type Size = Int

-- upper triangular matrix
data DistanceMat = DistanceMat !Int !(U.Vector Double)

(!) :: DistanceMat -> (Int, Int) -> Double
(!) (DistanceMat n v) (i',j') = v U.! idx i' j'
  where
    idx i j | i <= j = i * (2 * n - i - 3) `div` 2 + j - 1
            | otherwise = idx j i

dim :: DistanceMat -> Int
dim (DistanceMat n _) = n
{-# INLINE dim #-}

data Metric = Single
            | Average

data Dendrogram a = Leaf a
                  | Branch !Size !Distance (Dendrogram a) (Dendrogram a)
    deriving (Show)

instance Functor Dendrogram where
    fmap f (Leaf x) = Leaf $ f x
    fmap f (Branch n d a b) = Branch n d (fmap f a) $ fmap f b

size :: Dendrogram a -> Int
size (Leaf _) = 1
size (Branch n _ _ _) = n
{-# INLINE size #-}

dendrogram :: G.Vector v a => Metric -> v a -> DistFn a -> Dendrogram a
dendrogram method xs f = fmap label $ nnChain dists method
  where
    dists = computeDists f xs
    label i = xs G.! i

computeDists :: G.Vector v a => DistFn a -> v a -> DistanceMat
computeDists f vec = DistanceMat n . U.fromList . flip concatMap [0..n-1] $ \i ->
    flip map [i+1..n-1] $ \j -> f (vec `G.unsafeIndex` i) (vec `G.unsafeIndex` j)
  where
    n = G.length vec
{-# INLINE computeDists #-}

nnChain :: DistanceMat -> Metric -> Dendrogram Int
nnChain dist method = go dist initSet []
  where
    go ds activeNodes chain@(b:a:rest)
        | M.size activeNodes == 1 = head . M.elems $ activeNodes
        | c == a = go ds' activeNodes' rest
        | otherwise = go ds activeNodes $ c : chain
      where
        (c,d) = nearestNeighbor ds b a activeNodes
        activeNodes' = M.insert hi (Branch (size1+size2) d c1 c2)
                     . M.delete lo $ activeNodes
        ds' = updateDistMat lo hi size1 size2 method ds
        c1 = M.findWithDefault undefined lo activeNodes
        c2 = M.findWithDefault undefined hi activeNodes
        size1 = size c1
        size2 = size c2
        (lo,hi) = if a <= b then (a,b) else (b,a)
    go ds activeNodes _ = go ds activeNodes [b,a]
      where
        a = fst $ M.elemAt 0 activeNodes
        b = fst $ nearestNeighbor ds a (-1) activeNodes
    initSet = M.fromList . map (\i -> (i, Leaf i)) $ [0..n-1]
    n = dim dist
{-# INLINE nnChain #-}

nearestNeighbor :: DistanceMat -> Int -> Int -> M.Map Int (Dendrogram Int) -> (Int, Double)
nearestNeighbor dist i preference m = foldl' f (-1,1/0) . M.keys $ m
  where
    f (j,d) x | i == x = (j,d)  -- skip
              | d' < d = (x,d')
              | d' == d && x == preference = (x,d')
              | otherwise = (j,d)
      where d' = dist ! (i,x)
{-# INLINE nearestNeighbor #-}

updateDistMat :: Int -> Int -> Size -> Size -> Metric -> DistanceMat -> DistanceMat
updateDistMat lo hi s1 s2 method (DistanceMat n dist) = case method of
    Average -> DistanceMat n $ U.create $ do
        v <- U.unsafeThaw dist
        forM_ [0..n-1] $ \i -> when (i /= lo && i/= hi) $ do
            d_lo_i <- UM.unsafeRead v $ idx i lo
            d_hi_i <- UM.unsafeRead v $ idx i hi
            UM.unsafeWrite v (idx i hi) $ (fromIntegral s1 * d_lo_i + fromIntegral s2 * d_hi_i) / (fromIntegral s1 + fromIntegral s2)
        return v
    _ -> undefined
  where
    idx i j | i <= j = i * (2 * n - i - 3) `div` 2 + j - 1
            | otherwise = idx j i
{-# INLINE updateDistMat #-}

euclidean :: G.Vector v Double => DistFn (v Double)
euclidean xs ys = sqrt $ G.sum $ G.zipWith (\x y -> (x-y)**2) xs ys
{-# INLINE euclidean #-}

