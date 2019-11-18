import Data.Char
import Data.List
import Text.ParserCombinators.Parsec

possible :: (Int, Int, Int) -> Bool
possible (x, y, z) = all possible' (permutations [x, y, z]) where
  possible' xs = (xs !! 0) + (xs !! 1) > (xs !! 2)

countPossible :: [(Int, Int, Int)] -> Int
countPossible ts = length (filter possible ts)

-- Part 2.
stride :: Int -> [a] -> [a]
stride _ [] = []
stride n (x:xs) = x : stride n (drop (n-1) xs)

groupsOf3 :: [a] -> [(a, a, a)]
groupsOf3 xs = zip3 as bs cs where
  as = stride 3 xs
  bs = stride 3 (drop 1 xs)
  cs = stride 3 (drop 2 xs)

reformatTriples :: [(Int, Int, Int)] -> [(Int, Int, Int)]
reformatTriples ts = groupsOf3 (a ++ b ++ c) where
  (a, b, c) = unzip3 ts

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
         let result1 = countPossible <$> program
         putStrLn . show $ result1
         let result2 = countPossible <$> reformatTriples <$> program
         putStrLn . show $ result2
