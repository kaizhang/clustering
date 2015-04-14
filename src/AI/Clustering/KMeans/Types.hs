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
    ( KMeans(..)
    , Initialization(..)
    ) where

import qualified Data.Matrix.Unboxed as MU
import qualified Data.Vector.Unboxed as U

-- | Results from running kmeans
data KMeans = KMeans
    { _clusters :: U.Vector Int     -- ^ A vector of integers (0 ~ k-1)
                                    -- indicating the cluster to which each
                                    -- point is allocated.
    , _centers :: MU.Matrix Double  -- ^ A matrix of cluster centers.
    } deriving (Show)

-- | Different initialization methods
data Initialization = Forgy    -- ^ The Forgy method randomly chooses k unique
                               -- observations from the data set and uses these
                               -- as the initial means.
                    | KMeansPP -- ^ K-means++ algorithm.
