module AI.Clustering.Hierarchical.Types
    ( Distance
    , DistFn
    , Size
    , Dendrogram(..)
    , size
    , DistanceMat(..)
    , (!)
    , idx
    ) where

import Data.Bits (shiftR)
import qualified Data.Vector.Unboxed as U

type Distance = Double
type DistFn a = a -> a -> Distance
type Size = Int

data Dendrogram a = Leaf !a
                  | Branch !Size !Distance !(Dendrogram a) !(Dendrogram a)
    deriving (Show, Read)

instance Functor Dendrogram where
    fmap f (Leaf x) = Leaf $ f x
    fmap f (Branch n d l r) = Branch n d (fmap f l) $ fmap f r

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
