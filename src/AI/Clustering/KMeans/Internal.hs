{-# LANGUAGE BangPatterns #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  AI.Clustering.KMeans.Internal
-- Copyright   :  (c) 2015 Kai Zhang
-- License     :  MIT
--
-- Maintainer  :  kai@kzhang.org
-- Stability   :  experimental
-- Portability :  portable
--
-- <module description starting at first column>
--------------------------------------------------------------------------------
module AI.Clustering.KMeans.Internal
{-# WARNING "To be used by developer only" #-}
    ( forgy
    , kmeansPP
    , sumSquares
    ) where

import Control.Monad (forM_)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.List (nub)
import qualified Data.Matrix.Unboxed as MU
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import System.Random.MWC (uniformR, Gen)

forgy :: (PrimMonad m, G.Vector v a)
      => Gen (PrimState m)
      -> Int                 -- number of clusters
      -> v a                 -- data
      -> (a -> U.Vector Double)
      -> m (MU.Matrix Double)
forgy g k dat fn | k > n = error "k is larger than sample size"
                 | otherwise = iter
  where
    iter = do
        vec <- randN g k . U.enumFromN 0 $ n
        let xs = map (\i -> fn $ dat `G.unsafeIndex` i) . U.toList $ vec
        if length (nub xs) == length xs
           then return . MU.fromRows $ xs
           else iter
    n = G.length dat
{-# INLINE forgy #-}

kmeansPP :: (PrimMonad m, G.Vector v a)
         => Gen (PrimState m)
         -> Int
         -> v a
         -> (a -> U.Vector Double)
         -> m (MU.Matrix Double)
kmeansPP g k dat fn
    | k > n = error "k is larger than sample size"
    | otherwise = do
        c1 <- uniformR (0,n-1) g
        loop [c1] 1
  where
    loop centers !k'
        | k' == k = return $ MU.fromRows $ map (\i -> fn $ dat `G.unsafeIndex` i) centers
        | otherwise = do
            c' <- chooseWithProb g $ U.map (shortestDist centers) rowIndices
            loop (c':centers) (k'+1)

    n = G.length dat
    rowIndices = U.enumFromN 0 n
    shortestDist centers x = minimum $ map (\i ->
        sumSquares (fn $ dat `G.unsafeIndex` x) (fn $ dat `G.unsafeIndex` i)) centers
{-# INLINE kmeansPP #-}

chooseWithProb :: PrimMonad m
               => Gen (PrimState m)
               -> U.Vector Double    -- ^ weights, may not be normalized
               -> m Int              -- ^ result/index
chooseWithProb g ws = do
    x <- uniformR (0,sum') g
    return $ loop x 0 0
  where
    loop v !cdf !i | cdf' >= v = i
                   | otherwise = loop v cdf' (i+1)
      where cdf' = cdf + ws `U.unsafeIndex` i

    sum' = U.sum ws
{-# INLINE chooseWithProb #-}

-- | Random select k samples from a population
randN :: PrimMonad m => Gen (PrimState m) -> Int -> U.Vector Int -> m (U.Vector Int)
randN g k xs = do
    v <- U.thaw xs
    forM_ [0..k-1] $ \i -> do
        j <- uniformR (i, lst) g
        UM.unsafeSwap v i j
    U.unsafeFreeze . UM.take k $ v
  where
    lst = U.length xs - 1
{-# INLINE randN #-}

sumSquares :: U.Vector Double -> U.Vector Double -> Double
sumSquares xs = U.sum . U.zipWith (\x y -> (x - y)**2) xs
{-# INLINE sumSquares #-}
