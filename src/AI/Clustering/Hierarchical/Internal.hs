module AI.Clustering.Hierarchical.Internal
    ( nnChain
    , single
    , complete
    , average
    , weighted
    , ward
    ) where

import Control.Monad (forM_, when)
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import AI.Clustering.Hierarchical.Types

type ActiveNodeSet = M.Map Int (Dendrogram Int)
type DistUpdateFn = Int -> Int -> ActiveNodeSet -> DistanceMat -> DistanceMat

-- | nearest neighbor chain algorithm
nnChain :: DistanceMat -> DistUpdateFn -> Dendrogram Int
nnChain (DistanceMat n dist) fn = go (DistanceMat n $ U.force dist) initSet []
  where
    go ds activeNodes chain@(b:a:rest)
        | M.size activeNodes == 1 = head . M.elems $ activeNodes
        | c == a = go ds' activeNodes' rest
        | otherwise = go ds activeNodes $ c : chain
      where
        (c,d) = nearestNeighbor ds b a activeNodes

        -- We always remove the node with smaller index. The other one will be
        -- used to represent the merged result
        activeNodes' = M.insert hi (Branch (size1+size2) d c1 c2)
                     . M.delete lo $ activeNodes

        ds' = fn lo hi activeNodes ds
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
{-# INLINE nnChain #-}

nearestNeighbor :: DistanceMat                  -- ^ distance matrix
                -> Int                          -- ^ query
                -> Int                          -- ^ this would be selected if
                                                -- it achieves the minimal distance
                -> M.Map Int (Dendrogram Int)
                -> (Int, Double)
nearestNeighbor dist i preference = M.foldlWithKey' f (-1,1/0)
  where
    f (x,d) j _ | i == j = (x,d)  -- skip
                | d' < d = (j,d')
                | d' == d && j == preference = (j,d')
                | otherwise = (x,d)
      where d' = dist ! (i,j)
{-# INLINE nearestNeighbor #-}

-- | all update functions perform destructive updates, and hence should not be
-- called by end users

-- | single linkage update formula
single :: DistUpdateFn
single lo hi nodeset (DistanceMat n dist) = DistanceMat n $ U.create $ do
    v <- U.unsafeThaw dist
    forM_ (M.keys nodeset) $ \i -> when (i/= hi && i/=lo) $ do
        d_lo_i <- UM.unsafeRead v $ idx n i lo
        d_hi_i <- UM.unsafeRead v $ idx n i hi
        UM.unsafeWrite v (idx n i hi) $ min d_lo_i d_hi_i
    return v
{-# INLINE single #-}

-- | complete linkage update formula
complete :: DistUpdateFn
complete lo hi nodeset (DistanceMat n dist) = DistanceMat n $ U.create $ do
    v <- U.unsafeThaw dist
    forM_ (M.keys nodeset) $ \i -> when (i/= hi && i/=lo) $ do
        d_lo_i <- UM.unsafeRead v $ idx n i lo
        d_hi_i <- UM.unsafeRead v $ idx n i hi
        UM.unsafeWrite v (idx n i hi) $ max d_lo_i d_hi_i
    return v
{-# INLINE complete #-}

-- | average linkage update formula
average :: DistUpdateFn
average lo hi nodeset (DistanceMat n dist) = DistanceMat n $ U.create $ do
    v <- U.unsafeThaw dist
    forM_ (M.keys nodeset) $ \i -> when (i/= hi && i/=lo) $ do
        d_lo_i <- UM.unsafeRead v $ idx n i lo
        d_hi_i <- UM.unsafeRead v $ idx n i hi
        UM.unsafeWrite v (idx n i hi) $ f1 * d_lo_i + f2 * d_hi_i
    return v
  where
    s1 = fromIntegral . size . M.findWithDefault undefined lo $ nodeset
    s2 = fromIntegral . size . M.findWithDefault undefined hi $ nodeset
    f1 = s1 / (s1+s2)
    f2 = s2 / (s1+s2)
{-# INLINE average #-}

-- | weighted linkage update formula
weighted :: DistUpdateFn
weighted lo hi nodeset (DistanceMat n dist) = DistanceMat n $ U.create $ do
    v <- U.unsafeThaw dist
    forM_ (M.keys nodeset) $ \i -> when (i/= hi && i/=lo) $ do
        d_lo_i <- UM.unsafeRead v $ idx n i lo
        d_hi_i <- UM.unsafeRead v $ idx n i hi
        UM.unsafeWrite v (idx n i hi) $ (d_lo_i + d_hi_i) / 2
    return v
{-# INLINE weighted #-}

-- | ward linkage update formula
ward :: DistUpdateFn
ward lo hi nodeset (DistanceMat n dist) = DistanceMat n $ U.create $ do
    v <- U.unsafeThaw dist
    d_lo_hi <- UM.unsafeRead v $ idx n lo hi
    forM_ (M.toList nodeset) $ \(i,t) -> when (i/= hi && i/=lo) $ do
        let s3 = fromIntegral . size $ t
        d_lo_i <- UM.unsafeRead v $ idx n i lo
        d_hi_i <- UM.unsafeRead v $ idx n i hi
        UM.unsafeWrite v (idx n i hi) $
            ((s1+s3)*d_lo_i + (s2+s3)*d_hi_i - s3*d_lo_hi) / (s1+s2+s3)
    return v
  where
    s1 = fromIntegral . size . M.findWithDefault undefined lo $ nodeset
    s2 = fromIntegral . size . M.findWithDefault undefined hi $ nodeset
{-# INLINE ward #-}

{-
-- O(n^2) time, O(n) space. Minimum spanning tree algorithm for single linkage
mst :: [a] -> DistFn a -> Dendrogram a
mst xs fn = undefined
-}
