import qualified Data.List                     as List
import           Text.ParserCombinators.Parsec

digits :: Int -> [Int]
digits i = List.reverse (rdigits i)
  where
    rdigits 0 = []
    rdigits x = x `mod` 10 : rdigits (x `div` 10)

-- Part 1
groupLengths :: [Int] -> [Int]
groupLengths xs = List.map List.length (List.group xs)

hasMatchingAdjacentDigits :: [Int] -> Bool
hasMatchingAdjacentDigits ds = List.any (>= 2) (groupLengths ds)

isMonotonous :: [Int] -> Bool
isMonotonous (x:y:rest) = x <= y && isMonotonous (y : rest)
isMonotonous _          = True

isCandidate :: Int -> Bool
isCandidate i =
  List.length ds == 6 && hasMatchingAdjacentDigits ds && isMonotonous ds
  where
    ds = digits i

countInRange :: (Int -> Bool) -> (Int, Int) -> Int
countInRange pred (left, right) =
  List.sum (List.map (fromEnum . pred) [left .. right])

-- Part 2
hasTwoMatchingAdjacentDigits :: [Int] -> Bool
hasTwoMatchingAdjacentDigits ds = 2 `List.elem` groupLengths ds

isCandidate2 :: Int -> Bool
isCandidate2 i = isCandidate i && hasTwoMatchingAdjacentDigits (digits i)

-- Parser
int :: Parser Int
int = read <$> many1 digit

range :: Parser (Int, Int)
range = do
  left <- int
  char '-'
  right <- int
  pure (left, right)

main :: IO ()
main = do
  contents <- getContents
  let r = parse range "" contents
  print $ countInRange isCandidate <$> r
  print $ countInRange isCandidate2 <$> r
