import           Text.ParserCombinators.Parsec

fuelRequired :: Int -> Int
fuelRequired mass = mass `div` 3 - 2

fuelRequired_ :: Int -> Int
fuelRequired_ mass =
  if f > 0
    then f + fuelRequired_ f
    else 0
  where
    f = fuelRequired mass

int :: Parser Int
int = read <$> many1 digit

main :: IO ()
main = do
  contents <- getContents
  let ms = parse (sepEndBy int newline) "" contents
  print $ sum . map fuelRequired <$> ms
  print $ sum . map fuelRequired_ <$> ms
