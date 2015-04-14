import Test.Tasty

import qualified Test.Hierarchical as Hierarchical
--import qualified Test.KMeans as KMeans

main :: IO ()
main = defaultMain $ testGroup "Main"
    [ Hierarchical.tests
--    , KMeans.tests
    ]
