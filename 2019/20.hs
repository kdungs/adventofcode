{-# LANGUAGE TupleSections #-}

import Utils (headM)

import qualified Data.Map.Strict as Map
import qualified Data.List as List


data Point = Point { getX :: Integer
                   , getY :: Integer
                   , getZ :: Integer
                   } deriving (Eq, Ord)

p2d :: Integer -> Integer -> Point
p2d x y = Point x y 0

instance Show Point where
  show p = "(" ++ show (getX p) ++ ", " ++ show (getY p) ++ ", " ++ show (getZ p) ++ ")"

neighbours :: Point -> [Point]
neighbours p = [ p { getX = getX p + 1 }
               , p { getY = getY p + 1 }
               , p { getX = getX p - 1 }
               , p { getY = getY p - 1}
               ]


data Tile = OutsideT | WallT | EmptyT | TeleporterT String deriving (Eq, Ord, Show)

isEmptyT :: Tile -> Bool
isEmptyT EmptyT = True
isEmptyT _ = False

isTeleporterT :: Tile -> Bool
isTeleporterT (TeleporterT _) = True
isTeleporterT _ = False

parseTile :: Char -> Tile
parseTile ' ' = OutsideT
parseTile '#' = WallT
parseTile '.' = EmptyT
parseTile c = TeleporterT [c]


type Maze = Map.Map Point Tile

parseMaze :: String -> Maze
parseMaze s = Map.fromList $ do
  (row, y) <- zip (lines s) [0..]
  (tile, x) <- zip row [0..]
  pure (p2d x y, parseTile tile)

teleporterAt :: Maze -> Point -> Maybe String
teleporterAt m p = do
  t <- Map.lookup p m
  case t of
    (TeleporterT s) -> pure s
    _ -> Nothing

mergeTeleporters :: Maze -> Point -> Point -> String -> Maze
mergeTeleporters m a b s = Map.insert b (TeleporterT s) (Map.insert a (TeleporterT s) m)

combineTeleporters :: Maze -> Maze
combineTeleporters m = combineTeleporters_ m teleporterPositions
  where teleporterPositions = fst <$> (List.filter (\(_, t) -> isTeleporterT t) (Map.assocs m))

combineTeleporters_ :: Maze -> [Point] -> Maze
combineTeleporters_ m [] = m
combineTeleporters_ m (t:rest) = do
  let (TeleporterT c) = m Map.! t
  let right = t { getX = getX t + 1 }
  let below = t { getY = getY t + 1 }
  let nextMaze = case teleporterAt m right of
                   Just s -> mergeTeleporters m t right (c ++ s)
                   Nothing -> case teleporterAt m below of
                                Just s -> mergeTeleporters m t below (c ++ s)
                                Nothing -> m
  let nextKeys = List.filter (\p -> p /= right && p /= below) rest
  combineTeleporters_ nextMaze nextKeys


resolveTeleporters_ :: Maze -> [Point] -> Maze
resolveTeleporters_ m [] = m
resolveTeleporters_ m (p:rest) = do
  let t = m Map.! p
  let n = List.find isEmptyAt (neighbours p)
  let withoutT = Map.insert p OutsideT m
  let nextMaze = case n of
                   Just x -> Map.insert x t withoutT
                   Nothing -> withoutT
  resolveTeleporters_ nextMaze rest
  where isEmptyAt :: Point -> Bool
        isEmptyAt p = case m Map.!? p of
                        Just x -> isEmptyT x
                        Nothing -> False


resolveTeleporters :: Maze -> Maze
resolveTeleporters m = resolveTeleporters_ m (Map.keys (Map.filter isTeleporterT m))

resolveTeleporter :: Maze -> String -> Point -> Maybe Point
resolveTeleporter m t source = headM (List.filter (/= source) (Map.keys (Map.filter (isTeleporter t) m)))

bfsNeighbours :: Maze -> Point -> [Point]
bfsNeighbours m p = do
  let t = m Map.!? p
  let rns = neighbours p
  let ns = rns ++ case t of
                    Just (TeleporterT s) -> case resolveTeleporter m s p of
                                              Just x -> [x]
                                              _ -> []
                    _ -> []
  n <- ns
  case m Map.!? n of
    Just EmptyT -> pure n
    Just (TeleporterT _) -> pure n
    _ -> []


data Bfs = Bfs { getMaze :: Maze
               , getFrontier :: [Point]
               , getStep :: Integer
               , getSeen :: Map.Map Point Integer
               } deriving Show

initBfs :: Maze -> Point -> Bfs
initBfs m p = Bfs { getMaze = m
                  , getFrontier = [p]
                  , getStep = 0
                  , getSeen = Map.singleton p 0
                  }

unseen :: Map.Map Point Integer -> Point -> Bool
unseen s p = not (p `Map.member` s)

runBfs :: Bfs -> Bfs
runBfs bfs
  | List.null (getFrontier bfs) = bfs
  | otherwise = runBfs bfs { getFrontier = nextFrontier
                           , getStep = nextStep
                           , getSeen = nextSeen
                           }
      where nextFrontier = List.filter (unseen (getSeen bfs)) ((getFrontier bfs) >>= (bfsNeighbours (getMaze bfs)))
            nextStep = 1 + getStep bfs
            nextSeen = (getSeen bfs) `Map.union` (Map.fromList ((,nextStep) <$> nextFrontier))

isTeleporter :: String -> Tile -> Bool
isTeleporter s (TeleporterT x) = s == x
isTeleporter _ _ = False

findStart :: Maze -> Point
findStart m = List.head (Map.keys (Map.filter (isTeleporter "AA") m))

findEnd :: Maze -> Point
findEnd m = List.head (Map.keys (Map.filter (isTeleporter "ZZ") m))

-- Part II
isOutside :: Maze -> Point -> Bool
isOutside m p
  | getX p < minx = True
  | getX p > maxx = True
  | getY p < miny = True
  | getY p > maxy = True
  | otherwise = False
  where ps = Map.keys (Map.filter isEmptyT m)
        xs = getX <$> ps
        ys = getY <$> ps
        minx = List.minimum xs
        maxx = List.maximum xs
        miny = List.minimum ys
        maxy = List.maximum ys


resolveRecursiveTeleporter :: Maze -> String -> Point -> Maybe Point
resolveRecursiveTeleporter m s p
  | getZ p == 0 = do
      tel <- resolveTeleporter m s p
      if isOutside m p then Just p
                       else pure tel { getZ = 1 }
  | otherwise = do
      tel <- resolveTeleporter m s p { getZ = 0 }
      let nextZ = if isOutside m p then getZ p - 1
                                   else getZ p + 1
      pure tel { getZ = nextZ }

recursiveBfsNeighbours :: Maze -> Point -> [Point]
recursiveBfsNeighbours m p = do
  let t = m Map.!? p { getZ = 0 }
  let rns = neighbours p
  let ns = rns ++ case t of
                    Just (TeleporterT s) -> case resolveRecursiveTeleporter m s p of
                                              Just x -> [x]
                                              _ -> []
                    _ -> []
  n <- ns
  case m Map.!? n { getZ = 0 } of
    Just EmptyT -> pure n
    Just (TeleporterT _) -> pure n
    _ -> []


runRecursiveBfsUntilFound :: Point -> Bfs -> Bfs
runRecursiveBfsUntilFound end bfs
  | List.null (getFrontier bfs) = bfs
  | end `Map.member` (getSeen bfs) = bfs
  | otherwise = runRecursiveBfsUntilFound end bfs { getFrontier = nextFrontier
                                                  , getStep = nextStep
                                                  , getSeen = nextSeen
                                                  }
      where nextFrontier = List.filter (unseen (getSeen bfs)) ((getFrontier bfs) >>= (recursiveBfsNeighbours (getMaze bfs)))
            nextStep = 1 + getStep bfs
            nextSeen = (getSeen bfs) `Map.union` (Map.fromList ((,nextStep) <$> nextFrontier))


--Main
main :: IO ()
main = do
  contents <- getContents
  let maze = resolveTeleporters (combineTeleporters (parseMaze contents))
  let start = findStart maze
  let bfs = initBfs maze start
  let res = runBfs bfs
  let end = findEnd maze
  print $ (getSeen res Map.! end)
  -- Part II
  let res2 = runRecursiveBfsUntilFound end bfs
  print $ (getSeen res2 Map.! end)
