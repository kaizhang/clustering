{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.KMeans
    ( tests
    ) where

import           Control.Monad
import           Data.Int                      (Int32)
import           Data.List
import qualified Data.Matrix.Unboxed           as MU
import           Data.Maybe
import qualified Data.Vector.SEXP              as S
import qualified Data.Vector.Unboxed           as V
import qualified Foreign.R                     as R
import qualified Foreign.R.Type                as R
import qualified H.Prelude                     as H
import           Language.R.HExp
import           Language.R.QQ
import           System.Random.MWC
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           AI.Clustering.KMeans
import           AI.Clustering.KMeans.Internal

import           Test.Utils

tests :: TestTree
tests = testGroup "KMeans:"
    [ testCase "KMeans" testKMeans
    ]

rKmeans :: Int -> [Double] -> [Double] -> IO [Int]
rKmeans n' dat center = fmap (map (fromIntegral :: Int32 -> Int)) $ H.runRegion $ do
    xxx <- [r| x <- matrix(dat_hs, ncol=n_hs,byrow=T);
             y <- matrix(center_hs, ncol=n_hs,byrow=T);
             kmeans(x,y,iter.max=10000,algorithm="Lloyd")$cluster
    |]
    return $ H.fromSEXP $ H.cast R.SInt xxx
  where
    n = fromIntegral n' :: Double

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

    init_centers <- kmeansPP g k dat fn

    result_r <- rKmeans d (MU.toList mat) (MU.toList init_centers)

    let result = sort $ map sort $ fromJust $ clusters $ kmeans k mat defaultKMeansOpts{kmeansMethod=Centers init_centers}
        true = sort $ map sort $ decode (V.fromList $ map (subtract 1) result_r) xs

    assertBool ("Expect: " ++ show (map length true) ++ "\nBut saw: " ++ show (map length result)) $
        result == true
