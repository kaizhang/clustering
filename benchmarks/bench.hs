import Control.Monad (replicateM)
import Criterion.Main
import qualified Data.Clustering.Hierarchical as C
import qualified Data.Vector as V
import System.Random.MWC

import AI.Clustering.Hierarchical
import AI.Clustering.Hierarchical.Types ((!))

import Bench.Hierarchical (benchHierarchical)
import Bench.KMeans (benchKMeans)

main :: IO ()
main = defaultMain
    [ -- benchHierarchical
      benchKMeans
    ]
