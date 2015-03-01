import Test.Tasty

import qualified Test.Hierarchical as Hierarchical

main :: IO ()
main = defaultMain $ testGroup "Main"
    [ Hierarchical.tests ]
