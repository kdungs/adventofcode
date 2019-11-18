import Data.List (sort)
import Data.Set (fromList, size)
import Data.String (lines, words)

-- Part 1.
isGoodPassword :: String -> Bool
isGoodPassword pw = (length ws) == (size (fromList ws)) where ws = words pw

-- Part 2.
isGoodPassword' :: String -> Bool
isGoodPassword' pw = (length ws) == (size (fromList (map sort ws)))
  where ws = words pw

countGoodPasswords :: (String -> Bool) -> [String] -> Int
countGoodPasswords f pws = length (filter f pws)


main :: IO ()
main = do
  input <- readFile "input.txt"
  let pws = lines input
  putStrLn . show $ countGoodPasswords isGoodPassword pws
  putStrLn . show $ countGoodPasswords isGoodPassword' pws
