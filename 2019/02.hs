import           Intcode
import           Utils                         (headM, rightOrError)

import qualified Data.List                     as List
import qualified Data.Map.Strict               as Map
import           Text.ParserCombinators.Parsec (parse)

modify :: Integer -> Integer -> Memory -> Memory
modify x1 x2 = setM 1 x1 . setM 2 x2

initVmWithModification :: Memory -> Integer -> Integer -> VirtualMachine
initVmWithModification mem x1 x2 = initVm (modify x1 x2 mem) []

findSolution :: Memory -> Integer -> Maybe (Integer, Integer)
findSolution prog target = headM (List.dropWhile isNotSolution space)
  where
    space = [(x, y) | x <- [0 .. 99], y <- [0 .. 99]]
    isNotSolution (noun, verb) =
      case runWith noun verb of
        Just v -> v /= target
        _      -> True
    runWith n v = run (initVmWithModification prog n v) >>= getM 0 . memory

-- Main
main :: IO ()
main = do
  contents <- getContents
  let mem = rightOrError (parse programParser "" contents)
  let vm = initVmWithModification mem 12 2
  let res = run vm
  print $ res >>= (getM 0 . memory)
  let target = 19690720
  let solution = findSolution mem target
  print solution
