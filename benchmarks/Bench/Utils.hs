module Bench.Utils
    ( randVectors
    ) where

import Control.Monad (replicateM)
import qualified Data.Vector.Unboxed as U
import System.Random.MWC

randVectors :: Int  -- ^ number of samples
            -> Int  -- ^ vector length
            -> IO [U.Vector Double]
randVectors n k = do
    g <- createSystemRandom
    replicateM n $ uniformVector g k
