import           Intcode
import           Intcode.Chain
import           Utils                         (rightOrError)

import qualified Data.List                     as List
import           Data.Maybe                    (catMaybes)
import           Text.ParserCombinators.Parsec

findLargestAmplification :: Memory -> [Integer] -> Integer
findLargestAmplification mem phases =
  List.maximum $
  catMaybes [runChain (initChain perm 0 mem) | perm <- List.permutations phases]

main :: IO ()
main = do
  contents <- getContents
  let prog = rightOrError (parse programParser "" contents)
  print $ findLargestAmplification prog [0 .. 4]
  print $ findLargestAmplification prog [5 .. 9]
