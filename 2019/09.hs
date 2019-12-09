import Intcode
import Utils (rightOrError)

import Text.ParserCombinators.Parsec


main :: IO ()
main = do
  contents <- getContents
  let prog = rightOrError (parse programParser "" contents)
  let vm = initVm prog [1]
  print $ outputs <$> (run vm)
  let vm2 = initVm prog [2]
  print $ outputs <$> (run vm2)
