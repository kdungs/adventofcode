module Tests.Intcode.Chain
  ( tests
  ) where

import qualified Intcode
import           Intcode.Chain
import           Utils                         (rightOrError)

import qualified Data.List                     as List
import qualified Data.Map.Strict               as Map
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck         as QC
import           Text.ParserCombinators.Parsec

tests :: TestTree
tests = testGroup "Intcode.Chain" [day7]

-- Helpers
programOrFail :: String -> Intcode.Memory
programOrFail s = rightOrError (parse Intcode.programParser "" s)

runChainOrFail :: Intcode.Memory -> [Integer] -> Integer
runChainOrFail mem phases =
  case runChain (initChain phases 0 mem) of
    Nothing     -> error "Program had an error."
    Just signal -> signal

-- Day 7
p1ex1 = programOrFail "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"

p1ex2 =
  programOrFail
    "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"

p1ex3 =
  programOrFail
    "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"

p2ex1 =
  programOrFail
    "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"

p2ex2 =
  programOrFail
    "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"

day7 :: TestTree
day7 =
  testGroup
    "Day 7"
    [ testGroup
        "Part 1"
        [ testCase "Example 1" $ runChainOrFail p1ex1 [4, 3, 2, 1, 0] @?= 43210
        , testCase "Example 2" $ runChainOrFail p1ex2 [0, 1, 2, 3, 4] @?= 54321
        , testCase "Example 3" $ runChainOrFail p1ex3 [1, 0, 4, 3, 2] @?= 65210
        ]
    , testGroup
        "Part 2"
        [ testCase "Example 1" $
          runChainOrFail p2ex1 [9, 8, 7, 6, 5] @?= 139629729
        , testCase "Example 2" $ runChainOrFail p2ex2 [9, 7, 8, 5, 6] @?= 18216
        ]
    ]
