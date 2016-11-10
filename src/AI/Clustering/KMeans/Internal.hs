{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}

module AI.Clustering.KMeans.Internal
{-# WARNING "To be used by developer only" #-}
    ( forgy
    , kmeansPP
    , sumSquares
    ) where

import           Control.Monad.Primitive         (PrimMonad, PrimState)
import qualified Data.HashSet                    as S
import           Data.List                       (nub)
import qualified Data.Matrix.Unboxed             as MU
import qualified Data.Vector.Generic             as G
import qualified Data.Vector.Unboxed             as U
import           System.Random.MWC               (Gen, uniformR)
import           System.Random.MWC.Distributions (categorical)


forgy :: (PrimMonad m, G.Vector v a)
      => Gen (PrimState m)
      -> Int                 -- ^ The number of clusters
      -> v a                   -- ^ Input data
      -> (a -> U.Vector Double)  -- ^ Feature extraction function
      -> m (MU.Matrix Double)
forgy g k dat fn | k > n = error "k is larger than sample size"
                 | otherwise = loop
  where
    loop = do
        vec <- uniformRN (0, n-1) k g
        let xs = map (fn . G.unsafeIndex dat) vec
        if length (nub xs) == length xs
           then return $ MU.fromRows xs
           else loop
    n = G.length dat
{-# INLINE forgy #-}

kmeansPP :: (PrimMonad m, G.Vector v a)
         => Gen (PrimState m)
         -> Int                     -- ^ The number of clusters
         -> v a                     -- ^ Input data
         -> (a -> U.Vector Double)  -- ^ Feature extraction function
         -> m (MU.Matrix Double)
kmeansPP g k dat fn
    | k > n = error "k is larger than sample size"
    | otherwise = do
        c1 <- uniformR (0,n-1) g
        loop [c1] 1
  where
    loop centers !k'
        | k' == k = return $ MU.fromRows $ map (fn . G.unsafeIndex dat) centers
        | otherwise = do
            c' <- flip categorical g $ U.generate n $ \i -> minimum $
                map (\c -> sumSquares (fn $ G.unsafeIndex dat i) (fn $ G.unsafeIndex dat c))
                centers
            loop (c':centers) (k'+1)
    n = G.length dat
{-# INLINE kmeansPP #-}

sumSquares :: U.Vector Double -> U.Vector Double -> Double
sumSquares xs = U.sum . U.zipWith (\x y -> (x - y)**2) xs
{-# INLINE sumSquares #-}

-- | Generate N non-duplicated uniformly distributed random variables in a given range.
uniformRN :: PrimMonad m => (Int, Int) -> Int -> Gen (PrimState m) -> m [Int]
uniformRN (lo, hi) n g | hi - lo + 1 < n = error "Range is too narrow!"
                       | otherwise = loop S.empty
  where
    loop m | S.size m >= n = return $ S.toList m
           | otherwise = do
               x <- uniformR (lo,hi) g
               if x `S.member` m
                   then loop m
                   else loop $ S.insert x m
