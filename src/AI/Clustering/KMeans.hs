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

    , decode
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
import Data.List (minimumBy, nub)
import System.Random.MWC (uniformR, Gen)

-- | Results from running kmeans
data KMeans = KMeans
    { _clusters :: U.Vector Int     -- ^ A vector of integers (0 ~ k-1)
                                    -- indicating the cluster to which each
                                    -- point is allocated.
    , _centers :: MU.Matrix Double  -- ^ A matrix of cluster centres.
    } deriving (Show)

-- | Lloyd's algorithm, also known as K-means algorithm
kmeans :: PrimMonad m
       => Gen (PrimState m)
       -> Initialization
       -> Int                           -- ^ number of clusters
       -> MU.Matrix Double             -- ^ each row represents a point
       -> m KMeans
kmeans g method k mat = do
    initial <- case method of
        Forgy -> forgy g k mat
        _ -> undefined
    return $ kmeansWith mat initial
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
        in fst $ minimumBy (comparing snd) $ zip [0..k-1] $ map (dist x) $ MU.toRows means

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

    dist xs = U.sum . U.zipWith (\x y -> (x - y)**2) xs

    n = MU.rows mat
    k = MU.rows initial
    d = MU.cols mat
{-# INLINE kmeansWith #-}

-- | Different initialization methods
data Initialization = Forgy    -- ^ The Forgy method randomly chooses k unique
                               -- observations from the data set and uses these
                               -- as the initial means
                    | KMeansPP -- ^ K-means++ algorithm, not implemented.

forgy :: PrimMonad m
      => Gen (PrimState m)
      -> Int                 -- number of clusters
      -> MU.Matrix Double    -- data
      -> m (MU.Matrix Double)
forgy g k mat | k > n = error "k is larger than sample size"
              | otherwise = iter
  where
    iter = do
        vec <- sample g k . U.enumFromN 0 $ n
        let xs = map (MU.takeRow mat) . U.toList $ vec
        if length (nub xs) == length xs
           then return . MU.fromRows $ xs
           else iter
    n = MU.rows mat
{-# INLINE forgy #-}

-- random select k samples from a population
sample :: PrimMonad m => Gen (PrimState m) -> Int -> U.Vector Int -> m (U.Vector Int)
sample g k xs = do
    v <- U.thaw xs
    forM_ [0..k-1] $ \i -> do
        j <- uniformR (i, lst) g
        UM.unsafeSwap v i j
    U.unsafeFreeze . UM.take k $ v
  where
    lst = U.length xs - 1
{-# INLINE sample #-}

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
--withinSS :: Matrix
