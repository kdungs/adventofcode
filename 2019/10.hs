import qualified Data.List as List
import qualified Data.Set as Set

data Point = Point Integer Integer deriving (Eq, Ord, Show)

data UpDown = Up | Down deriving (Eq, Ord, Show)

abs :: Point -> Integer
abs (Point x y) = x ^ 2 + y ^ 2

vec :: Point -> Point -> Point
vec (Point x1 y1) (Point x2 y2) = Point (x2 - x1) (y2 - y1)

orientation :: Point -> Point -> UpDown
orientation p0@(Point x0 y0) p@(Point x y)
  | y < y0 = Up
  | y > y0 = Down
  | x < y0 = Up
  | x > y0 = Down
  | otherwise = error "Points are the same"

lineRepr :: Point -> Point -> (UpDown, Float)
lineRepr p0 p = (orientation p0 p, (fromIntegral vx) / (fromIntegral vy) )
  where (Point vx vy) = vec p0 p

allAsteroids :: Point -> [Char] -> [Point]
allAsteroids _ [] = []
allAsteroids p@(Point x y) ('#':cs) = p : allAsteroids (Point (x+1) y) cs
allAsteroids (Point x y) ('.':cs) = allAsteroids (Point (x+1) y) cs
allAsteroids (Point x y) ('\n':cs) = allAsteroids (Point x (y+1)) cs
allAsteroids _ (c:_)  = error ("Cannot parse " ++ [c])

countVisible :: Point -> [Point] -> Int
countVisible p0 ps = Set.size $ Set.fromList [ lineRepr p0 p
                                             | p <- ps, p /= p0
                                             ]

maxVisible :: [Point] -> Int
maxVisible ps = List.maximum [countVisible p ps | p <- ps]

main :: IO ()
main = do
  contents <- getContents
  let as = allAsteroids (Point 0 0) contents
  print $ maxVisible as
