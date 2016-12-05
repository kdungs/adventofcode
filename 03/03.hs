import Data.Char
import Data.List
import Text.ParserCombinators.Parsec

possible :: (Int, Int, Int) -> Bool
possible (x, y, z) = all possible' (permutations [x, y, z]) where
  possible' xs = (xs !! 0) + (xs !! 1) > (xs !! 2)

countPossible :: [(Int, Int, Int)] -> Int
countPossible ts = length (filter possible ts)

-- Parser.
parseSeparator :: Parser ()
parseSeparator = skipMany (space <|> newline)

parseDecimal :: Parser Int
parseDecimal =  convertDecimal <$> many1 digit where
  convertDecimal = foldl' (\a i -> a * 10 + digitToInt i) 0

parseTriple :: Parser (Int, Int, Int)
parseTriple = do
                parseSeparator
                x <- parseDecimal
                parseSeparator
                y <- parseDecimal
                parseSeparator
                z <- parseDecimal
                parseSeparator
                return (x, y, z)

parseFile :: Parser [(Int, Int, Int)]
parseFile = do
              result <- many parseTriple
              eof
              return result

-- Main.
main = do
         input <- getContents
         let program = parse parseFile "" input
         let result = countPossible <$> program
         putStrLn . show $ result
