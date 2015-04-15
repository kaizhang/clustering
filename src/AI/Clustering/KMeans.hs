{-# LANGUAGE FlexibleContexts #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  AI.Clustering.KMeans
-- Copyright   :  (c) 2015 Kai Zhang
-- License     :  MIT
-- Maintainer  :  kai@kzhang.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Kmeans clustering
--------------------------------------------------------------------------------
module AI.Clustering.KMeans
    ( KMeans(..)
    , kmeans
    , kmeansBy
    , kmeansWith

    -- * Initialization methods
    , Initialization(..)

    -- * Useful functions
    , decode
    , withinSS

    -- * References
    -- $references
    ) where

import Control.Monad (forM_)
import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Matrix.Unboxed as MU
import qualified Data.Matrix.Generic as MG
import qualified Data.Matrix.Unboxed.Mutable as MM
import Data.Ord (comparing)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.List (minimumBy, foldl')
import System.Random.MWC (Gen)

import AI.Clustering.KMeans.Types (KMeans(..), Initialization(..))
import AI.Clustering.KMeans.Internal (sumSquares, forgy, kmeansPP)

kmeans :: (PrimMonad m, MG.Matrix mat U.Vector Double)
       => Gen (PrimState m)
       -> Initialization
       -> Int
       -> mat U.Vector Double
       -> m KMeans
kmeans g method k mat = kmeansBy g method k dat (MG.takeRow mat)
  where
    dat = U.enumFromN 0 $ MG.rows mat
{-# INLINE kmeans #-}

-- | Lloyd's algorithm, also known as K-means algorithm
kmeansBy :: (PrimMonad m, G.Vector v a)
         => Gen (PrimState m)
         -> Initialization
         -> Int                   -- ^ number of clusters
         -> v a                   -- ^ data stores in rows
         -> (a -> U.Vector Double)
         -> m KMeans
kmeansBy g method k dat fn = do
    initial <- case method of
        Forgy -> forgy g k dat fn
        KMeansPP -> kmeansPP g k dat fn
    return $ kmeansWith initial dat fn
{-# INLINE kmeansBy #-}

-- | Lloyd's algorithm, also known as K-means algorithm
kmeansWith :: G.Vector v a
           => MU.Matrix Double   -- ^ initial set of k centroids
           -> v a                -- ^ each row represents a point
           -> (a -> U.Vector Double)
           -> KMeans
kmeansWith initial dat fn | d /= MU.cols initial || k > n = error "check input"
                          | otherwise = KMeans member centers
  where
    (member, centers) = loop initial U.empty
    loop means membership
        | membership' == membership = (membership, means)
        | otherwise = loop (update membership') membership'
      where
        membership' = assign means

    -- Assignment step
    assign means = U.generate n $ \i ->
        let x = fn $ G.unsafeIndex dat i
        in fst $ minimumBy (comparing snd) $ zip [0..k-1] $ map (sumSquares x) $ MU.toRows means

    -- Update step
    update membership = MU.create $ do
        m <- MM.replicate (k,d) 0.0
        count <- UM.replicate k (0 :: Int)
        forM_ [0..n-1] $ \i -> do
            let x = membership U.! i
            UM.unsafeRead count x >>= UM.unsafeWrite count x . (+1)

            let vec = fn $ dat G.! i
            forM_ [0..d-1] $ \j ->
                MM.unsafeRead m (x,j) >>= MM.unsafeWrite m (x,j) . (+ (vec U.! j))
        -- normalize
        forM_ [0..k-1] $ \i -> do
            c <- UM.unsafeRead count i
            forM_ [0..d-1] $ \j ->
                MM.unsafeRead m (i,j) >>= MM.unsafeWrite m (i,j) . (/fromIntegral c)
        return m

    n = G.length dat
    k = MU.rows initial
    d = MU.cols initial
{-# INLINE kmeansWith #-}

-- | Assign data to clusters based on KMeans result
decode :: KMeans -> [a] -> [[a]]
decode result xs = V.toList $ V.create $ do
    v <- VM.replicate n [] 
    forM_ (zip (U.toList membership) xs) $ \(i,x) ->
        VM.unsafeRead v i >>= VM.unsafeWrite v i . (x:)
    return v
  where
    membership = _clusters result
    n = U.maximum membership + 1

-- | Compute within-cluster sum of squares
withinSS :: KMeans -> MU.Matrix Double -> [Double]
withinSS result mat = zipWith f (decode result [0 .. MU.rows mat-1]) .
                          MU.toRows . _centers $ result
  where
    f c center = foldl' (+) 0 $ map (sumSquares center . MU.takeRow mat) c


-- $references
--
-- Arthur, D. and Vassilvitskii, S. (2007). k-means++: the advantages of careful 
-- seeding. Proceedings of the eighteenth annual ACM-SIAM symposium on Discrete 
-- algorithms. Society for Industrial and Applied Mathematics Philadelphia, PA, 
-- USA. pp. 1027–1035.
