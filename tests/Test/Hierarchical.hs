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
    [ testCase "Single Linkage" testSingle
    , testCase "Complete Linkage" testComplete
    , testCase "Average Linkage" testAverage
    , testCase "Weighted Linkage" testWeighted
    ]

randSample :: IO [V.Vector Double]
randSample = do
    g <- create
    replicateM 500 $ uniformVector g 5

isEqual :: Eq a => Dendrogram a -> C.Dendrogram a -> Bool
isEqual (Leaf x) (C.Leaf x') = x == x'
isEqual (Branch _ d x y) (C.Branch d' x' y') = abs (d - d') < 1e-8 &&
    ((isEqual x x' && isEqual y y') || (isEqual x y' && isEqual y x'))
isEqual _ _ = False

testSingle :: Assertion
testSingle = do
    xs <- randSample
    let true = C.dendrogram C.SingleLinkage xs euclidean
        test = hclust Single (V.fromList xs) euclidean
    assertBool (unlines ["Expect: ", show true, "But see: ", show test]) $
        isEqual test true

testComplete :: Assertion
testComplete = do
    xs <- randSample
    let true = C.dendrogram C.CompleteLinkage xs euclidean
        test = hclust Complete (V.fromList xs) euclidean
    assertBool (unlines ["Expect: ", show true, "But see: ", show test]) $
        isEqual test true

testAverage :: Assertion
testAverage = do
    xs <- randSample
    let true = C.dendrogram C.UPGMA xs euclidean
        test = hclust Average (V.fromList xs) euclidean
    assertBool (unlines ["Expect: ", show true, "But see: ", show test]) $
        isEqual test true

testWeighted :: Assertion
testWeighted = do
    xs <- randSample
    let true = C.dendrogram C.FakeAverageLinkage xs euclidean
        test = hclust Weighted (V.fromList xs) euclidean
    assertBool (unlines ["Expect: ", show true, "But see: ", show test]) $
        isEqual test true

