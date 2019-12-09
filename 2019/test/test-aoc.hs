module Main where

import qualified Tests.Intcode
import qualified Tests.Intcode.Chain

import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "AoC 2019 Tests" [Tests.Intcode.tests, Tests.Intcode.Chain.tests]
