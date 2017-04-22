{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
module Test.KMeans
    ( tests
    ) where

import Control.Monad
import Data.List
import RlangQQ
import System.Random.MWC
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Data.Matrix.Unboxed as MU
import qualified Data.Vector.Unboxed as V

import AI.Clustering.KMeans
import AI.Clustering.KMeans.Internal
import AI.Clustering.KMeans.Types

import Test.Utils

tests :: TestTree
tests = testGroup "KMeans:"
    [ testCase "KMeans" testKMeans
    ]

rKmeans :: Int -> [Double] -> [Double] -> IO [Int]
rKmeans n dat center = do
    o <- [r| x <- matrix(hs_dat, ncol=hs_n,byrow=T);
             y <- matrix(hs_center, ncol=hs_n,byrow=T);
             hs_result <- kmeans(x,y,iter.max=1000000,algorithm="Lloyd")$cluster;
         |]
    let x = Label :: Label "result"
    return $ o .!. x

testKMeans :: Assertion
testKMeans = do
    let n = 2000
        d = 15
        k = 10
    g <- createSystemRandom
    xs <- randVectors n d

    let mat = MU.fromRows xs :: MU.Matrix Double
        dat = V.enumFromN 0 $ MU.rows mat
        fn = MU.takeRow mat

    centers <- kmeansPP g k dat fn sumSquares

    r <- rKmeans d (MU.toList mat) (MU.toList centers)
    let test = sort $ map sort $ decode result xs
        result = kmeansWith centers dat fn
        true = sort $ map sort $ decode result{_clusters=V.fromList $ map (subtract 1) r} xs
        show' xs = unlines $ map (show . map (unwords . map show . V.toList)) xs

    assertBool ("Expect: " ++ show' true ++ "\nBut saw: " ++ show' test) $
        test == true
