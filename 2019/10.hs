import           Utils           (rightOrError)

import qualified Data.List       as List
import qualified Data.Map.Strict as Map

data Point =
  Point
    { getX :: Integer
    , getY :: Integer
    }
  deriving (Eq, Ord)

instance Show Point where
  show (Point x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

data Polar =
  Polar
    { getR   :: Float
    , getPhi :: Float
    }
  deriving (Eq, Ord)

instance Show Polar where
  show (Polar r phi) = "(" ++ show r ++ ", " ++ show phi ++ ")"

toPolar :: Point -> Polar
toPolar (Point x y) =
  Polar
    { getR = sqrt . fromIntegral $ x ^ 2 + y ^ 2
    , getPhi = atan2 (fromIntegral y) (fromIntegral x)
    }

toPoint :: Polar -> Point
toPoint (Polar r phi) =
  Point {getX = round (r * cos phi), getY = round (r * sin phi)}

withOrigin :: Point -> Point -> Point
withOrigin p0 p = Point {getX = getX p - getX p0, getY = getY p - getY p0}

fromOrigin :: Point -> Point -> Point
fromOrigin p0 p = Point {getX = getX p + getX p0, getY = getY p + getY p0}

viewFrom :: Point -> [Point] -> [Polar]
viewFrom p0 ps = toPolar . withOrigin p0 <$> filter (/= p0) ps

originalCoordinates :: Point -> [Polar] -> [Point]
originalCoordinates p0 ps = fromOrigin p0 . toPoint <$> ps

groupByAngle :: [Polar] -> [[Polar]]
groupByAngle ps = List.groupBy (\x y -> getPhi x == getPhi y) sorted
  where
    sorted = List.sortOn getPhi (List.sortOn getR ps)

bestPosition :: [Point] -> (Int, Point)
bestPosition ps = List.maximum [(List.length (group p), p) | p <- ps]
  where
    group p = groupByAngle (viewFrom p ps)

rotateIntoStartingPosition :: [[Polar]] -> [[Polar]]
rotateIntoStartingPosition rot@(front:xs)
  | (a front >= (-pi / 2)) && (a (last xs) < (-pi / 2)) = rot
  | otherwise = rotateIntoStartingPosition (xs ++ [front])
  where
    a :: [Polar] -> Float
    a ((Polar _ phi):xs) = phi
    a _                  = error "Empty list!"

vaporizationOrder :: Point -> [Point] -> [Point]
vaporizationOrder p0 ps =
  originalCoordinates
    p0
    (vaporizationOrder_
       (rotateIntoStartingPosition (groupByAngle (viewFrom p0 ps))))

vaporizationOrder_ :: [[Polar]] -> [Polar]
vaporizationOrder_ [] = []
vaporizationOrder_ ([]:rest) = vaporizationOrder_ rest
vaporizationOrder_ (angleGroup:rest) =
  List.head angleGroup : vaporizationOrder_ (rest ++ [List.tail angleGroup])

parseAsteroids :: String -> Either String [Point]
parseAsteroids = parseAsteroids_ (Point 0 0)

parseAsteroids_ :: Point -> String -> Either String [Point]
parseAsteroids_ _ [] = Right []
parseAsteroids_ p@(Point x y) ('#':cs) =
  (p :) <$> parseAsteroids_ (Point (x + 1) y) cs
parseAsteroids_ (Point x y) ('.':cs) = parseAsteroids_ (Point (x + 1) y) cs
parseAsteroids_ (Point x y) ('\n':cs) = parseAsteroids_ (Point 0 (y + 1)) cs
parseAsteroids_ _ (c:_) = Left ("Cannot parse " ++ [c])

main :: IO ()
main = do
  putStrLn "Testing part 1"
  print $ runT <$> tests
  putStrLn "Testing part 2"
  print $ runVOT ex6
  putStrLn "Result"
  contents <- getContents
  let ps = rightOrError (parseAsteroids contents)
  let (count, p0) = bestPosition ps
  print count
  print $ vaporizationOrder p0 ps !! 199

-- Poor man's tests...
type Test = ((Int, Point), String)

runT :: Test -> Either String Bool
runT (expected, input) = do
  ps <- parseAsteroids input
  let res = bestPosition ps
  pure (expected == res)

tests :: [Test]
tests = [ex1, ex2, ex3, ex4, ex5]

ex1 :: Test
ex1 =
  ( (8, Point 3 4)
  , List.intercalate "\n" [".#..#", ".....", "#####", "....#", "...##"])

ex2 :: Test
ex2 =
  ( (33, Point 5 8)
  , List.intercalate
      "\n"
      [ "......#.#."
      , "#..#.#...."
      , "..#######."
      , ".#.#.###.."
      , ".#..#....."
      , "..#....#.#"
      , "#..#....#."
      , ".##.#..###"
      , "##...#..#."
      , ".#....####"
      ])

ex3 :: Test
ex3 =
  ( (35, Point 1 2)
  , List.intercalate
      "\n"
      [ "#.#...#.#."
      , ".###....#."
      , ".#....#..."
      , "##.#.#.#.#"
      , "....#.#.#."
      , ".##..###.#"
      , "..#...##.."
      , "..##....##"
      , "......#..."
      , ".####.###."
      ])

ex4 :: Test
ex4 =
  ( (41, Point 6 3)
  , List.intercalate
      "\n"
      [ ".#..#..###"
      , "####.###.#"
      , "....###.#."
      , "..###.##.#"
      , "##.##.#.#."
      , "....###..#"
      , "..#.#..#.#"
      , "#..#.#.###"
      , ".##...##.#"
      , ".....#.#.."
      ])

ex5str :: String
ex5str =
  List.intercalate
    "\n"
    [ ".#..##.###...#######"
    , "##.############..##."
    , ".#.######.########.#"
    , ".###.#######.####.#."
    , "#####.##.#.##.###.##"
    , "..#####..#.#########"
    , "####################"
    , "#.####....###.#.#.##"
    , "##.#################"
    , "#####.##.###..####.."
    , "..######..##.#######"
    , "####.##.####...##..#"
    , ".#####..#.######.###"
    , "##...#.##########..."
    , "#.##########.#######"
    , ".####.#.###.###.#.##"
    , "....##.##.###..#####"
    , ".#.#.###########.###"
    , "#.#.#.#####.####.###"
    , "###.##.####.##.#..##"
    ]

ex5 :: Test
ex5 = ((210, Point 11 13), ex5str)

type VaporizationOrderTest = (Point, String, [(Int, Point)])

runVOT :: VaporizationOrderTest -> Either String [Bool]
runVOT (p0, input, ts) = do
  ps <- parseAsteroids input
  let vo = vaporizationOrder p0 ps
  pure [vo !! (pos - 1) == p | (pos, p) <- ts]

ex6 :: VaporizationOrderTest
ex6 =
  ( Point 11 13
  , ex5str
  , [ (1, Point 11 12)
    , (2, Point 12 1)
    , (3, Point 12 2)
    , (10, Point 12 8)
    , (20, Point 16 0)
    , (50, Point 16 9)
    , (100, Point 10 16)
    , (199, Point 9 6)
    , (200, Point 8 2)
    , (201, Point 10 9)
    , (299, Point 11 1)
    ])
