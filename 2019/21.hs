import           Intcode

import           Data.Char                     (chr, ord)
import qualified Data.List                     as List
import           Text.ParserCombinators.Parsec (parse)

toAsciiString :: [Integer] -> String
toAsciiString xs = chr . fromIntegral <$> xs

fromAsciiString :: String -> [Integer]
fromAsciiString s = toInteger . ord <$> s

runProgram :: Memory -> String -> Maybe VirtualMachine
runProgram m f = run (initVm m (fromAsciiString f))

main :: IO ()
main = do
  contents <- getContents
  let (Right prog) = parse programParser "" contents
  let f1 = ["NOT C T", "AND D T", "NOT A J", "OR T J", "WALK"]
  let f2 =
        [ "OR H J"
        , "OR E J"
        , "AND D J"
        , "NOT C T"
        , "AND T J"
        , "NOT A T"
        , "OR T J"
        , "NOT E T"
        , "NOT T T"
        , "OR B T"
        , "NOT T T"
        , "OR T J"
        , "RUN"
        ]
  let (Just done) = runProgram prog (List.unlines f2)
  putStrLn (toAsciiString (List.reverse (outputs done)))
