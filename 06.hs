import Text.ParserCombinators.Parsec
import Data.List

file :: Parser [String]
file = many1 $ do
                 line <- many1 lower
                 newline
                 return line

frequencies :: String -> [(Int, Char)]
frequencies s = [(length c, c !! 0) | c <- group (sort s)]

mostFrequentLetter :: String -> Char
mostFrequentLetter = snd . maximum . frequencies

leastFrequentLetter :: String -> Char
leastFrequentLetter = snd . minimum . frequencies

decodeWith :: (String -> Char) -> [String] -> String
decodeWith f = map f . transpose

main = do
         input <- getContents
         let dat = parse file "" input
         putStrLn . show $ (decodeWith mostFrequentLetter) <$> dat
         putStrLn . show $ (decodeWith leastFrequentLetter) <$> dat
