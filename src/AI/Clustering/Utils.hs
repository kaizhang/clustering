{-# LANGUAGE FlexibleContexts #-}
module AI.Clustering.Utils
    ( orderBy
    ) where

import Control.Monad (forM_)
import qualified Data.Vector.Generic as G
import qualified Data.Matrix.Class as M
import qualified Data.Matrix.Class.Mutable as MM

-- | rearrange the rows of a matrix
orderBy :: (G.Vector v1 Int, M.Matrix m v2 a) => v1 Int -> m v2 a -> m v2 a
orderBy vec mat
    | G.length vec /= r = error "orderBy: n != r"
    | otherwise = M.create $ do
        mat' <- MM.new (r,c)
        forM_ [0..r-1] $ \i -> do
            let i' = vec G.! i
            forM_ [0..c-1] $ \j -> MM.unsafeWrite mat' (i,j) $ mat `M.unsafeIndex` (i',j)
        return mat'
  where
    (r,c) = M.dim mat
{-# INLINE orderBy #-}
