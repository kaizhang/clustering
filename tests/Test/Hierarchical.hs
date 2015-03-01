module Test.Hierarchical
    ( tests
    ) where

import Control.Monad
import qualified Data.Clustering.Hierarchical as C
import qualified Data.Vector as V
import System.Random.MWC
import Test.Tasty
import Test.Tasty.HUnit

import AI.Clustering.Hierarchical

tests :: TestTree
tests = testGroup "Hierarchical:"
    [testCase "Average Linkage" testHierarchical]

randSample :: IO [V.Vector Double]
randSample = do
    g <- create
    replicateM 100 $ uniformVector g 5

isEqual :: Eq a => Dendrogram a -> C.Dendrogram a -> Bool
isEqual (Leaf x) (C.Leaf x') = x == x'
isEqual (Branch _ d x y) (C.Branch d' x' y') = abs (d - d') < 1e-8 &&
    ((isEqual x x' && isEqual y y') || (isEqual x y' && isEqual y x'))
isEqual _ _ = False

testHierarchical :: Assertion
testHierarchical = do
    xs <- randSample
    let true = C.dendrogram C.UPGMA xs euclidean
        test = dendrogram Average (V.fromList xs) euclidean
    assertBool (unlines ["Expect: ", show true, "But see: ", show test]) $
        isEqual test true
