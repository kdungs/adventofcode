module Tests.Intcode
  ( tests
  ) where

import qualified Intcode
import           Utils                         (rightOrError)

import qualified Data.List                     as List
import qualified Data.Map.Strict               as Map
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck         as QC
import           Text.ParserCombinators.Parsec

tests :: TestTree
tests = testGroup "Intcode" [day2, day5, day9]

-- Helpers
programOrFail :: String -> Intcode.Memory
programOrFail s = rightOrError (parse Intcode.programParser "" s)

runOrFail :: Intcode.Memory -> [Integer] -> Intcode.VirtualMachine
runOrFail mem ins =
  case Intcode.run (Intcode.initVm mem ins) of
    Nothing -> error "Program had an error."
    Just vm -> vm

memoryView :: Intcode.VirtualMachine -> [Integer]
memoryView = Map.elems . Intcode.memory

outputView :: Intcode.VirtualMachine -> [Integer]
outputView = List.reverse . Intcode.outputs

becomes :: String -> [Integer] -> TestTree
s `becomes` expected =
  testCase (s ++ " becomes " ++ show expected) $
  memoryView (runOrFail (programOrFail s) []) @?= expected

produces :: (Intcode.Memory, [Integer]) -> [Integer] -> Assertion
(mem, ins) `produces` outs = outputView (runOrFail mem ins) @?= outs

mapsInputToOutput :: Intcode.Memory -> Integer -> Integer -> Bool
mapsInputToOutput mem input output = (List.length o == 1) && (head o == output)
  where
    o = outputView (runOrFail mem [input])

-- Day 2
day2 :: TestTree
day2 =
  testGroup
    "Day 2"
    [ "1,0,0,0,99" `becomes` [2, 0, 0, 0, 99]
    , "2,3,0,3,99" `becomes` [2, 3, 0, 6, 99]
    , "2,4,4,5,99,0" `becomes` [2, 4, 4, 5, 99, 9801]
    , "1,1,1,4,99,5,6,0,99" `becomes` [30, 1, 1, 4, 2, 5, 6, 0, 99]
    ]

-- Day 5
pInOut = programOrFail "3,0,4,0,99"

pCheckEq8P = programOrFail "3,9,8,9,10,9,4,9,99,-1,8"

pCheckLt8P = programOrFail "3,9,7,9,10,9,4,9,99,-1,8"

pCheckEq8I = programOrFail "3,3,1108,-1,8,3,4,3,99"

pCheckLt8I = programOrFail "3,3,1107,-1,8,3,4,3,99"

pCheckEq0P = programOrFail "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9"

pCheckEq0I = programOrFail "3,3,1105,-1,9,1101,0,0,12,4,12,99,1"

pCmp8 =
  programOrFail
    "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"

day5 :: TestTree
day5 =
  testGroup
    "Day 5"
    [ QC.testProperty "pInOut(x) = x" $ \x -> mapsInputToOutput pInOut x x
    , testCase "pCheckEq8P(8) = 1" $ (pCheckEq8P, [8]) `produces` [1]
    , QC.testProperty "pCheckEq8P(x ≠ 8) = 0" $ \x ->
        x /= 8 QC.==> mapsInputToOutput pCheckEq8P x 0
    , QC.testProperty "pCheckLt8P(x < 8) = 1" $ \x ->
        x < 8 QC.==> mapsInputToOutput pCheckLt8P x 1
    , QC.testProperty "pCheckLt8P(x ≥ 8) = 0" $ \x ->
        x >= 8 QC.==> mapsInputToOutput pCheckLt8P x 0
    , testCase "pCheckEq8I(x ≠ 8) = 1" $ (pCheckEq8I, [8]) `produces` [1]
    , QC.testProperty "pCheckEq8I(x ≠ 8) = 0" $ \x ->
        x /= 8 QC.==> mapsInputToOutput pCheckEq8I x 0
    , QC.testProperty "pCheckLt8I(x < 8) = 1" $ \x ->
        x < 8 QC.==> mapsInputToOutput pCheckLt8I x 1
    , QC.testProperty "pCheckLt8I(x ≥ 8) = 0" $ \x ->
        x >= 8 QC.==> mapsInputToOutput pCheckLt8I x 0
    , testCase "pCheckEq0P(0) = 0" $ (pCheckEq0P, [0]) `produces` [0]
    , QC.testProperty "pCheckEq0P(x ≠ 0) = 1" $ \x ->
        x /= 0 QC.==> mapsInputToOutput pCheckEq0P x 1
    , testCase "pCheckEq0I(0) = 0" $ (pCheckEq0I, [0]) `produces` [0]
    , QC.testProperty "pCheckEq0I(x ≠ 0) = 1" $ \x ->
        x /= 0 QC.==> mapsInputToOutput pCheckEq0I x 1
    , QC.testProperty "pCmp8(x < 8) = 999" $ \x ->
        x < 8 QC.==> mapsInputToOutput pCmp8 x 999
    , testCase "pCmp8(8) = 1000" $ (pCmp8, [8]) `produces` [1000]
    , QC.testProperty "pCmp8(x > 8) = 1001" $ \x ->
        x > 8 QC.==> mapsInputToOutput pCmp8 x 1001
    ]

-- Day 9
pQuine =
  programOrFail "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"

p16Digit = programOrFail "1102,34915192,34915192,7,4,7,99,0"

pLargeNumber = programOrFail "104,1125899906842624,99"

day9 :: TestTree
day9 =
  testGroup
    "Day 9"
    [ testCase "Quine outputs itself" $ (pQuine, []) `produces` Map.elems pQuine
    , testCase "16-digit number" $ (p16Digit, []) `produces` [1219070632396864]
    , testCase "Large number" $ (pLargeNumber, []) `produces` [1125899906842624]
    ]
