module Test.KMeans
    ( tests
    ) where

import Control.Monad
import qualified Data.Matrix.Unboxed as MU
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.List
import qualified Math.KMeans as K
import System.Random.MWC
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import AI.Clustering.KMeans
import AI.Clustering.KMeans.Internal

import Test.Utils

tests :: TestTree
tests = testGroup "KMeans:"
    [ testCase "KMeans" testKMeans
    ]

testKMeans :: Assertion
testKMeans = do
    g <- createSystemRandom
    xs <- randVectors 10 3

    let dat = MU.fromRows xs
    centers <- kmeansPP g 3 dat
    let test = sort $ map sort $ decode (kmeansWith centers dat) xs
    true <- K.kmeansWith (\_ _ -> return $ V.fromList $ map (K.Cluster . return) $ MU.toRows centers) id K.euclidSq 3 xs
    let true' = sort $ map sort $ map K.elements $ V.toList true

    print $ length true'
    print $ length test

    assertBool ("Expect: " ++ show true' ++ "\nBut saw: " ++ show test) $
        test == true'
