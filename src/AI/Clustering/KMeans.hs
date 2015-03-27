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
import qualified Data.Matrix.Unboxed.Mutable as MM
import Data.Ord (comparing)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.List (minimumBy, foldl')
import System.Random.MWC (Gen)

import AI.Clustering.KMeans.Types (KMeans(..), Initialization(..))
import AI.Clustering.KMeans.Internal (sumSquares, forgy, kmeansPP)

-- | Lloyd's algorithm, also known as K-means algorithm
kmeans :: PrimMonad m
       => Gen (PrimState m)
       -> Initialization
       -> Int                   -- ^ number of clusters
       -> MU.Matrix Double      -- ^ data stores in rows
       -> m KMeans
kmeans g method k mat = do
    initial <- case method of
        Forgy -> forgy g k mat
        KMeansPP -> kmeansPP g k mat
    return $ kmeansWith initial mat
{-# INLINE kmeans #-}

-- | Lloyd's algorithm, also known as K-means algorithm
kmeansWith :: MU.Matrix Double   -- ^ initial set of k centroids
           -> MU.Matrix Double   -- ^ each row represents a point
           -> KMeans
kmeansWith initial mat | d /= MU.cols initial || k > n = error "check input"
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
        let x = MU.takeRow mat i
        in fst $ minimumBy (comparing snd) $ zip [0..k-1] $ map (sumSquares x) $ MU.toRows means

    -- Update step
    update membership = MM.create $ do
        m <- MM.replicate (k,d) 0.0
        count <- UM.replicate k (0 :: Int)
        forM_ [0..n-1] $ \i -> do
            let x = membership U.! i
            UM.unsafeRead count x >>= UM.unsafeWrite count x . (+1)
            forM_ [0..d-1] $ \j ->
                MM.unsafeRead m (x,j) >>= MM.unsafeWrite m (x,j) . (+ mat MU.! (i,j))
        -- normalize
        forM_ [0..k-1] $ \i -> do
            c <- UM.unsafeRead count i
            forM_ [0..d-1] $ \j ->
                MM.unsafeRead m (i,j) >>= MM.unsafeWrite m (i,j) . (/fromIntegral c)
        return m

    n = MU.rows mat
    k = MU.rows initial
    d = MU.cols mat
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
