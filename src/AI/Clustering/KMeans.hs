{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module AI.Clustering.KMeans
    ( KMeans(..)
    , KMeansOpts(..)
    , defaultKMeansOpts
    , kmeans
    , kmeansBy

    -- * Initialization methods
    , Method(..)

    , decode

    -- * References
    -- $references
    ) where

import Control.Monad (forM_)
import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Matrix.Unboxed as MU
import Data.Matrix.Generic (unsafeTakeRow)
import qualified Data.Matrix.Unboxed.Mutable as MM
import Data.Ord (comparing)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.List (minimumBy, foldl')
import System.Random.MWC (Gen, initialize)
import Control.Monad.ST (runST)

import AI.Clustering.KMeans.Types
import AI.Clustering.KMeans.Internal (sumSquares, forgy, kmeansPP)

-- | Perform K-means clustering
kmeans :: Int                -- ^ The number of clusters
       -> MU.Matrix Double   -- ^ Input data stored as rows in a matrix
       -> KMeansOpts
       -> KMeans (U.Vector Double)
kmeans k mat opts
    | containNaN = error "Input data contains NaN."
    | otherwise = KMeans member cs grps sse'
  where
    containNaN =  U.any isNaN $ MU.flatten mat
    (member, cs, sse') = kmeans' initial (kmeansMaxIter opts) dat fn
    grps = if kmeansClusters opts
        then Just $ decode member $ MU.toRows mat
        else Nothing
    dat = U.enumFromN 0 $ MU.rows mat
    fn = unsafeTakeRow mat
    initial = runST $ do
        gen <- initialize $ kmeansSeed opts
        case kmeansMethod opts of
            Forgy -> forgy gen k dat fn
            KMeansPP -> kmeansPP gen k dat fn
            Centers c -> return c
{-# INLINE kmeans #-}

-- | Perform K-means clustering, using a feature extraction function
kmeansBy :: G.Vector v a
         => Int                   -- ^ The number of clusters
         -> v a                   -- ^ Input data
         -> (a -> U.Vector Double)
         -> KMeansOpts
         -> KMeans a
kmeansBy k dat fn opts
    | containNaN = error "Input data contains NaN."
    | otherwise = KMeans member cs grps sse'
  where
    containNaN = G.foldl (\acc x -> acc || U.any isNaN (fn x)) False dat
    (member, cs, sse') = kmeans' initial (kmeansMaxIter opts) dat fn
    grps = if kmeansClusters opts
        then Just $ decode member $ G.toList dat
        else Nothing
    initial = runST $ do
        gen <- initialize $ kmeansSeed opts
        case kmeansMethod opts of
            Forgy -> forgy gen k dat fn
            KMeansPP -> kmeansPP gen k dat fn
            Centers c -> return c
{-# INLINE kmeansBy #-}

-- | K-means algorithm
kmeans' :: G.Vector v a
        => MU.Matrix Double         -- ^ Initial set of k centroids
        -> Int                      -- ^ Max inter
        -> v a                      -- ^ Input data
        -> (a -> U.Vector Double)   -- ^ Feature extraction function
        -> (U.Vector Int, MU.Matrix Double, Double)
kmeans' initial maxiter dat fn
    | U.length (fn $ G.head dat) /= d = error "Dimension mismatched."
    | otherwise = (member, centers, U.sum $ U.imap ( \i x -> sqrt $ sumSquares
        (fn $ dat G.! i) (centers `MU.takeRow` x) ) member )
  where
    (member, centers) = loop 0 initial U.empty
    loop !iter means membership
        | iter >= maxiter || membership' == membership = (membership, means)
        | otherwise = loop (iter+1) (update membership') membership'
      where
        membership' = assign means

    -- Assignment step
    assign means = U.generate n $ \i ->
        let x = fn $ G.unsafeIndex dat i
            f (!min', !j') j = let d = sumSquares x $ means `unsafeTakeRow` j
                               in if d < min' then (d, j) else (min', j')
        in snd $ foldl' f (1/0, -1) [0..k-1]

    -- Update step
    update membership = MU.create $ do
        m <- MM.replicate (k,d) 0.0
        count <- UM.replicate k (0 :: Int)
        forM_ [0..n-1] $ \i -> do
            let x = membership `U.unsafeIndex` i
                vec = fn $ dat `G.unsafeIndex` i
            UM.unsafeModify count (+1) x
            forM_ [0..d-1] $ \j ->
                MM.unsafeRead m (x,j) >>=
                    MM.unsafeWrite m (x,j) . (+ (vec `U.unsafeIndex` j))
        -- normalize
        forM_ [0..k-1] $ \i -> do
            c <- UM.unsafeRead count i
            forM_ [0..d-1] $ \j ->
                MM.unsafeRead m (i,j) >>= MM.unsafeWrite m (i,j) . (/fromIntegral c)
        return m
    n = G.length dat
    k = MU.rows initial
    d = MU.cols initial
{-# INLINE kmeans' #-}

-- | Assign data to clusters based on KMeans result
decode :: U.Vector Int -> [a] -> [[a]]
decode member xs = V.toList $ V.create $ do
    v <- VM.replicate n []
    forM_ (zip (U.toList member) xs) $ \(i,x) ->
        VM.unsafeRead v i >>= VM.unsafeWrite v i . (x:)
    return v
  where
    n = U.maximum member + 1
{-# INLINE decode #-}

-- $references
--
-- Arthur, D. and Vassilvitskii, S. (2007). k-means++: the advantages of careful
-- seeding. Proceedings of the eighteenth annual ACM-SIAM symposium on Discrete
-- algorithms. Society for Industrial and Applied Mathematics Philadelphia, PA,
-- USA. pp. 1027–1035.
