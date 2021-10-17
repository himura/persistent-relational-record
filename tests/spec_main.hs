module Main where

import Test.Tasty
import qualified QueryTest

tests :: TestTree
tests =
    testGroup
        "Tests"
        [ testGroup "Unit Test" [QueryTest.tests]
        ]

main :: IO ()
main = defaultMain tests
