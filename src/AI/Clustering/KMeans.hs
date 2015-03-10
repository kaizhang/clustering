--------------------------------------------------------------------------------
-- |
-- Module      :  $Header$
-- Copyright   :  (c) 2015 Kai Zhang
-- License     :  MIT
-- Maintainer  :  kai@kzhang.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Kmeans clustering
--------------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts  #-}

module AI.Clustering.KMeans
    ( kmeans
    , kmeansWith
    , forgyMethod
    ) where

import Control.Monad (forM_)
import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Matrix.Generic as M
import qualified Data.Matrix.Generic.Mutable as MM
import Data.Ord (comparing)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import Data.List (minimumBy, nub)
import System.Random.MWC (uniformR, Gen)

-- | Lloyd's algorithm, also known as K-means algorithm
kmeans :: (G.Vector v Double, G.Vector v Int, Eq (v Int), Eq (v Double), PrimMonad m)
       => Gen (PrimState m)
       -> Int                           -- ^ number of clusters
       -> M.Matrix v Double             -- ^ each row represents a point
       -> m (v Int, M.Matrix v Double)  -- ^ membership vector
kmeans g k mat = do
    initial <- forgyMethod g k mat
    return $ kmeansWith mat initial
{-# INLINE kmeans #-}

-- | Lloyd's algorithm, also known as K-means algorithm
kmeansWith :: (G.Vector v Double, G.Vector v Int, Eq (v Int))
           => M.Matrix v Double           -- ^ initial set of k centroids
           -> M.Matrix v Double           -- ^ each row represents a point
           -> (v Int, M.Matrix v Double)  -- ^ membership vector and centroids
kmeansWith initial mat | d /= M.cols initial || k > n = error "check input"
                       | otherwise = loop initial G.empty
  where
    loop means membership
        | membership' == membership = (membership, means)
        | otherwise = loop (update membership') membership'
      where
        membership' = assign means

    -- Assignment step
    assign means = G.generate n $ \i ->
        let x = M.takeRow mat i
        in fst $ minimumBy (comparing snd) $ zip [0..k-1] $ map (dist x) $ M.toRows means

    --  Update step
    update membership = MM.create $ do
        m <- MM.replicate (k,d) 0.0
        count <- UM.replicate k (0 :: Int)
        forM_ [0..n-1] $ \i -> do
            let x = membership G.! i
            GM.unsafeRead count x >>= GM.unsafeWrite count x . (+1)
            forM_ [0..d-1] $ \j ->
                MM.unsafeRead m (x,j) >>= MM.unsafeWrite m (x,j) . (+ mat M.! (i,j))
        -- normalize
        forM_ [0..k-1] $ \i -> do
            c <- GM.unsafeRead count i
            forM_ [0..d-1] $ \j ->
                MM.unsafeRead m (i,j) >>= MM.unsafeWrite m (i,j) . (/fromIntegral c)
        return m

    dist :: G.Vector v Double => v Double -> v Double -> Double
    dist xs = G.sum . G.zipWith (\x y -> (x - y)**2) xs

    n = M.rows mat
    k = M.rows initial
    d = M.cols mat
{-# INLINE kmeansWith #-}

-- * Initialization methods

-- | The Forgy method randomly chooses k unique observations from the data set and uses
-- these as the initial means
forgyMethod :: (PrimMonad m, G.Vector v a, Eq (v a))
            => Gen (PrimState m)
            -> Int                 -- number of clusters
            -> M.Matrix v a        -- data
            -> m (M.Matrix v a)
forgyMethod g k mat | k > n = error "k is larger than sample size"
                    | otherwise = iter
  where
    iter = do
        vec <- sample g k . U.enumFromN 0 $ n
        let xs = map (M.takeRow mat) . G.toList $ vec
        if length (nub xs) == length xs
           then return . M.fromRows $ xs
           else iter
    n = M.rows mat
{-# INLINE forgyMethod #-}

-- random select k samples from a population
sample :: (PrimMonad m, G.Vector v a) => Gen (PrimState m) -> Int -> v a -> m (v a)
sample g k xs = do
    v <- G.thaw xs
    forM_ [0..k-1] $ \i -> do
        j <- uniformR (i, lst) g
        GM.unsafeSwap v i j
    G.unsafeFreeze . GM.take k $ v
  where
    lst = G.length xs - 1
{-# INLINE sample #-}
