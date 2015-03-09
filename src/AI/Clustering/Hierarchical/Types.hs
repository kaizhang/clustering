module AI.Clustering.Hierarchical.Types
    ( Distance
    , DistFn
    , Size
    , Dendrogram(..)
    , size
    , cutAt
    , members
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

cutAt :: Dendrogram a -> Distance -> [Dendrogram a]
cutAt dendro th = go [] dendro
  where
    go acc x@(Leaf _) = x : acc
    go acc x@(Branch _ d l r) | d <= th = x : acc
                              | otherwise = go (go acc r) l

members :: Dendrogram a -> [a]
members (Leaf x) = [x]
members (Branch _ _ l r) = members l ++ members r

-- upper triangular matrix
data DistanceMat = DistanceMat !Int !(U.Vector Double) deriving (Show)

(!) :: DistanceMat -> (Int, Int) -> Double
(!) (DistanceMat n v) (i',j') = v U.! idx n i' j'
{-# INLINE (!) #-}

idx :: Int -> Int -> Int -> Int
idx n i j | i <= j = (i * (2 * n - i - 3)) `shiftR` 1 + j - 1
          | otherwise = (j * (2 * n - j - 3)) `shiftR` 1 + i - 1
{-# INLINE idx #-}
