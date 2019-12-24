import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data Point = Point { getX :: Int
                   , getY :: Int
                   , getZ :: Int
                   } deriving (Eq, Ord, Show)

neighbours :: Point -> [Point]
neighbours p = [ p { getX = getX p + 1 }
               , p { getX = getX p - 1 }
               , p { getY = getY p + 1 }
               , p { getY = getY p - 1 }
               ]

recursion :: Point -> [Point]
recursion (Point 2 1 z) = [ Point x 0 (z + 1) | x <- [0..4]]
recursion (Point 2 3 z) = [ Point x 4 (z + 1) | x <- [0..4]]
recursion (Point 1 2 z) = [ Point 0 y (z + 1) | y <- [0..4]]
recursion (Point 3 2 z) = [ Point 4 y (z + 1) | y <- [0..4]]
recursion (Point x y z) = (case x of
                             0 -> [Point 1 2 (z - 1)]
                             4 -> [Point 3 2 (z - 1)]
                             _ -> []
                          ) ++ (case y of
                             0 -> [Point 2 1 (z - 1)]
                             4 -> [Point 2 3 (z - 1)]
                             _ -> []
                          )

neighbours' :: Point -> [Point]
neighbours' p
  | is22 p = []
  | otherwise = List.filter not22 (neighbours p) ++ recursion p
  where is22 (Point x y _) = x == 2 && y == 2
        not22 p = not (is22 p)

neighboursAsCounts :: (Point -> [Point]) -> Point -> Map.Map Point Int
neighboursAsCounts nsf p = Map.fromList (zip (nsf p) (List.repeat 1))

isOnGrid :: (Int, Int) -> Point -> Bool
isOnGrid (w, h) p
  | getX p < 0 = False
  | getX p >= w = False
  | getY p < 0 = False
  | getY p >= h = False
  | otherwise = True

limitToGrid :: (Int, Int) -> Set.Set Point -> Set.Set Point
limitToGrid g ps = Set.filter (isOnGrid g) ps

type Board = Set.Set Point

keySet :: Ord k => Map.Map k v -> Set.Set k
keySet = Set.fromList . Map.keys

nextGen :: (Point -> [Point]) -> (Int, Int) -> Board -> Board
nextGen nsf g b = limitToGrid g (oneN `Set.union` twoN)
  where allNeighbours = List.foldl1 (Map.unionWith (+)) (neighboursAsCounts nsf <$> Set.elems b)
        oneN = keySet (Map.filter (==1) allNeighbours)
        twoN = keySet (Map.filter (==2) allNeighbours) Set.\\ b

biodiversity :: (Int, Int) -> Board -> Integer
biodiversity (w, h) b = List.sum [2 ^ (y * w + x) | (Point x y _) <- Set.elems b]

runUntilDuplicateBiodiversity :: Set.Set Integer -> (Int, Int) -> Board -> Integer
runUntilDuplicateBiodiversity bdivs g b = let nextB = nextGen neighbours g b
                                              bdiv = biodiversity g nextB
                                           in if bdiv `Set.member` bdivs then bdiv
                                                                         else runUntilDuplicateBiodiversity (Set.insert bdiv bdivs) g nextB

runRecursiveN :: Int -> (Int, Int) -> Board -> Board
runRecursiveN 0 g b = b
runRecursiveN n g b = runRecursiveN (n - 1) g (nextGen neighbours' g b)

parseBoard :: String -> (Board, (Int, Int))
parseBoard s = (Set.fromList $ (do
    (row, y) <- zip lines [0..]
    (t, x) <- zip row [0..]
    if t == '#' then pure (Point x y 0) else []
  ), (w, h))
  where lines = List.lines s
        w = List.length (List.head lines)
        h = List.length lines

main :: IO ()
main = do
  contents <- getContents
  let (b, g) = parseBoard contents
  print $ biodiversity g b
  let res = runUntilDuplicateBiodiversity Set.empty g b
  print res
  let r2 = runRecursiveN 200 g b
  print (Set.size r2)
