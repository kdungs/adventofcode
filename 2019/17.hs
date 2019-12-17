import Intcode

import Data.Char (chr)
import qualified Data.List as List
import Text.ParserCombinators.Parsec (parse)

toAsciiString :: [Integer] -> String
toAsciiString xs = chr <$> fromIntegral <$> xs

intersections :: [String] -> [(Int, Int)]
intersections rows = do
    y <- [0 .. h - 1]
    x <- [0 .. List.length (rows !! y) - 1]
    if isSet x y && hasNeighbours x y then pure (x, y)
                                      else []
  where h = List.length rows
        isSet x y = (rows !! y) !! x == '#'
        hasNeighbours x y
          | x < 1 = False
          | y < 1 = False
          | y >= h - 1 = False
          | x >= (List.length (rows !! y)) - 1 = False
          | otherwise = isSet (x - 1) y
                     && isSet (x + 1) y
                     && isSet x (y - 1)
                     && isSet x (y + 1)

alignmentParameter :: (Int, Int) -> Int
alignmentParameter (x, y) = x * y

sumAlignmentParameters :: [(Int, Int)] -> Int
sumAlignmentParameters ps = sum (alignmentParameter <$> ps)


main :: IO ()
main = do
  contents <- getContents
  let (Right prog) = parse programParser "" contents
  let (Just vm) = run (initVm prog [])
  let res = toAsciiString (List.reverse (outputs vm))
  let lines = List.dropWhile (List.null) (List.lines res)
  let saps = sumAlignmentParameters (intersections lines)
  print saps
  -- Part II

example = [ "..#.........."
          , "..#.........."
          , "#######...###"
          , "#.#...#...#.#"
          , "#############"
          , "..#...#...#.."
          , "..#####...^.."
          ]
