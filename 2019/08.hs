import qualified Data.List as List
import Text.ParserCombinators.Parsec

type Color = Char
type Layer = [Color]

makeLayers :: (Int, Int) -> [Color] -> [Layer]
makeLayers _ [] = []
makeLayers (w, h) cs = List.take n cs : makeLayers (w, h) (List.drop n cs)
  where n = w * h

countDigits :: Char -> Layer -> Int
countDigits d = List.length . List.filter (== d)

layerWithMinimumZeroes :: [Layer] -> Layer
layerWithMinimumZeroes ls = fst (List.minimumBy (\lhs rhs -> compare (snd lhs) (snd rhs)) zeroCounts)
  where zeroCounts = List.map (\l -> (l, countDigits '0' l)) ls

part1 :: [Layer] -> Int
part1 ls = (countDigits '1' lmin0) * (countDigits '2' lmin0)
  where lmin0 = layerWithMinimumZeroes ls

overlayColors :: Color -> Color -> Color
overlayColors top btn
  | top == '2' = btn
  | otherwise = top

overlay :: Layer -> Layer -> Layer
overlay = List.zipWith overlayColors

part2 :: [Layer] -> Layer
part2 = foldl1 overlay

formatImage :: (Int, Int) -> Layer -> [Char]
formatImage (w, h) l = List.intercalate "\n" ((group w) (List.map formatDigit l))
  where formatDigit d = if d == '0' then '█' else ' '
        group _ [] = []
        group w cs = List.take w cs : group w (List.drop w cs)

-- Parser
color :: Parser Color
color = digit

colors :: Parser [Color]
colors = many1 color

-- Main
main :: IO ()
main = do
  contents <- getContents
  let cs = parse colors "" contents
  let ls = makeLayers (25, 6) <$> cs
  let p1 = part1 <$> ls
  print p1
  let p2 = part2 <$> ls
  case p2 of
    Left err -> print err
    Right res -> putStrLn $ formatImage (25, 6) res
