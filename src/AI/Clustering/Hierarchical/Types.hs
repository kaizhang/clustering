module AI.Clustering.Hierarchical.Types
    ( Distance
    , DistFn
    , Size
    , Dendrogram(..)
    , size
    , DistanceMat(..)
    , (!)
    , idx
    , computeDists
    , computeDists'
    ) where

import Control.Monad (liftM, liftM4)
import Control.Parallel.Strategies (rdeepseq, parMap)
import Data.Binary (Binary, put, get, getWord8)
import Data.Bits (shiftR)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic as G
import Data.Word (Word8)

type Distance = Double
type DistFn a = a -> a -> Distance
type Size = Int

data Dendrogram a = Leaf !a
                  | Branch !Size !Distance !(Dendrogram a) !(Dendrogram a)
    deriving (Show, Eq)

instance Binary a => Binary (Dendrogram a) where
    put (Leaf a) = do put (0 :: Word8)
                      put a
    put (Branch s d l r) = do put (1 :: Word8)
                              put s
                              put d
                              put l
                              put r

    get = do tag <- getWord8
             case tag of
                 0 -> liftM Leaf get
                 1 -> liftM4 Branch get get get get
                 _ -> error "fail to decode the dendrogram"

instance Functor Dendrogram where
    fmap f (Leaf x) = Leaf $ f x
    fmap f (Branch n d l r) = Branch n d (fmap f l) $ fmap f r

-- | O(1) Return the size of a dendrogram
size :: Dendrogram a -> Int
size (Leaf _) = 1
size (Branch n _ _ _) = n
{-# INLINE size #-}

-- upper triangular matrix
data DistanceMat = DistanceMat !Int !(U.Vector Double) deriving (Show)

(!) :: DistanceMat -> (Int, Int) -> Double
(!) (DistanceMat n v) (i',j') = v U.! idx n i' j'
{-# INLINE (!) #-}

idx :: Int -> Int -> Int -> Int
idx n i j | i <= j = (i * (2 * n - i - 3)) `shiftR` 1 + j - 1
          | otherwise = (j * (2 * n - j - 3)) `shiftR` 1 + i - 1
{-# INLINE idx #-}

-- | compute distance matrix
computeDists :: G.Vector v a => DistFn a -> v a -> DistanceMat
computeDists f vec = DistanceMat n . U.fromList . flip concatMap [0..n-1] $ \i ->
    flip map [i+1..n-1] $ \j -> f (vec `G.unsafeIndex` i) (vec `G.unsafeIndex` j)
  where
    n = G.length vec
{-# INLINE computeDists #-}

-- | compute distance matrix in parallel
computeDists' :: G.Vector v a => DistFn a -> v a -> DistanceMat
computeDists' f vec = DistanceMat n . U.fromList . concat . flip (parMap rdeepseq) [0..n-1] $ \i ->
    flip map [i+1..n-1] $ \j -> f (vec `G.unsafeIndex` i) (vec `G.unsafeIndex` j)
  where
    n = G.length vec
{-# INLINE computeDists' #-}
