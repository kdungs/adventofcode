import Intcode
import Utils (rightOrError)

import Text.ParserCombinators.Parsec


main :: IO ()
main = do
  contents <- getContents
  let prog = rightOrError (parse programParser "" contents)
  let vm = initVm prog [1]
  print (run vm)
