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
    ) where

import qualified Data.Matrix.Unboxed as MU
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)

data KMeansOpts = KMeansOpts
    { kmeansMethod :: Method
    , kmeansSeed :: (U.Vector Word32)   -- ^ Seed for random number generation
    , kmeansClusters :: Bool   -- ^ Wether to return clusters, may use a lot memory
    , kmeansMaxIter :: Int     -- ^ Maximum iteration
    }

-- | Default options.
-- > defaultKMeansOpts = KMeansOpts
-- >     { kmeansMethod = KMeansPP
-- >     , kmeansSeed = U.fromList [1,2,3,4,5,6,7]
-- >     , kmeansClusters = True
-- >     , kmeansMaxIter = 10
-- >     }
defaultKMeansOpts :: KMeansOpts
defaultKMeansOpts = KMeansOpts
    { kmeansMethod = KMeansPP
    , kmeansSeed = U.fromList [2341,2342,3934,425,2345,80006,2343,234491,124,729]
    , kmeansClusters = True
    , kmeansMaxIter = 10000
    }

-- | Results from running kmeans
data KMeans a = KMeans
    { membership :: U.Vector Int     -- ^ A vector of integers (0 ~ k-1)
                                    -- indicating the cluster to which each
                                    -- point is allocated.
    , centers :: MU.Matrix Double  -- ^ A matrix of cluster centers.
    , clusters :: Maybe [[a]]
    , sse :: Double                -- ^ the sum of squared error (SSE)
    } deriving (Show)

-- | Different initialization methods
data Method = Forgy    -- ^ The Forgy method randomly chooses k unique
                       -- observations from the data set and uses these
                       -- as the initial means.
            | KMeansPP -- ^ K-means++ algorithm.
            | Centers (MU.Matrix Double)   -- ^ Provide a set of k centroids
