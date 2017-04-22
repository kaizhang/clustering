--------------------------------------------------------------------------------
-- |
-- Module      :  AI.Clustering.KMeans.Types
-- Copyright   :  (c) 2015 Kai Zhang
-- License     :  MIT
--
-- Maintainer  :  kai@kzhang.org
-- Stability   :  experimental
-- Portability :  portable
--
-- <module description starting at first column>
--------------------------------------------------------------------------------
module AI.Clustering.KMeans.Types
    ( KMeansOpts(..)
    , defaultKMeansOpts
    , KMeans(..)
    , Method(..)
    , sumSquares, cosSimilarity
    ) where

import Data.Word (Word32)
import qualified Data.Matrix.Unboxed as MU
import qualified Data.Vector.Unboxed as U

data KMeansOpts = KMeansOpts
    { kmeansMethod :: Method
    , kmeansSeed :: (U.Vector Word32)   -- ^ Seed for random number generation
    , kmeansClusters :: Bool   -- ^ Wether to return clusters, may use a lot memory
    , kmeansMaxIter :: Int     -- ^ Maximum iteration
    }

-- | Default options.
-- > defaultKMeansOpts = KMeansOpts
-- >     { kmeansMethod = KMeansPP sumSquares
-- >     , kmeansSeed = U.fromList [1,2,3,4,5,6,7]
-- >     , kmeansClusters = True
-- >     , kmeansMaxIter = 10
-- >     }
defaultKMeansOpts :: KMeansOpts
defaultKMeansOpts = KMeansOpts
    { kmeansMethod = KMeansPP sumSquares
    , kmeansSeed = U.fromList [1,2,3,4,5,6,7]
    , kmeansClusters = True
    , kmeansMaxIter = 10
    }

sumSquares :: U.Vector Double -> U.Vector Double -> Double
sumSquares xs = U.sum . U.zipWith (\x y -> (x - y) * (x - y)) xs
{-# INLINE sumSquares #-}

cosSimilarity :: U.Vector Double -> U.Vector Double -> Double
cosSimilarity vec1 vec2 =
    let dp = dotProduct vec1 vec2
        mag = (magnitude vec1 * magnitude vec2)
    in dp / mag
{-# INLINE cosSimilarity #-}

dotProduct :: U.Vector Double -> U.Vector Double -> Double
dotProduct = (U.sum .) . U.zipWith (*)

magnitude :: U.Vector Double -> Double
magnitude =
    sqrt . U.foldl acc 0
    where
      acc :: Double -> Double -> Double
      acc cur new = cur + (new ** 2)

-- | Results from running kmeans
data KMeans a = KMeans
    { membership :: U.Vector Int     -- ^ A vector of integers (0 ~ k-1)
                                    -- indicating the cluster to which each
                                    -- point is allocated.
    , centers :: MU.Matrix Double  -- ^ A matrix of cluster centers.
    , clusters :: Maybe [[a]]
    } deriving (Show)

-- | Different initialization methods
data Method =
    Forgy    -- ^ The Forgy method randomly chooses k unique
             -- observations from the data set and uses these
             -- as the initial means.
    | KMeansPP (U.Vector Double -> U.Vector Double -> Double) -- ^ K-means++ algorithm.
    | Centers (MU.Matrix Double)   -- ^ Provide a set of k centroids
