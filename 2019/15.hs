{-# LANGUAGE TupleSections #-}

import           Intcode
import           Utils                         (headM, rightOrError)

import           Control.Monad                 (sequence)
import qualified Data.List                     as List
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import           Text.ParserCombinators.Parsec (parse)

data Point =
  Point
    { getX :: Integer
    , getY :: Integer
    }
  deriving (Eq, Ord, Show)

data Direction
  = North
  | East
  | South
  | West
  deriving (Eq, Ord, Show)

move :: Direction -> Point -> Point
move North p = p {getY = getY p + 1}
move East p  = p {getX = getX p + 1}
move South p = p {getY = getY p - 1}
move West p  = p {getX = getX p - 1}

oppositeDir :: Direction -> Direction
oppositeDir North = South
oppositeDir East  = West
oppositeDir South = North
oppositeDir West  = East

neighbourhood :: [Point -> Point]
neighbourhood = [move North, move East, move South, move West]

data Droid =
  Droid
    { getVm       :: VirtualMachine
    , getPosition :: Point
    }
  deriving (Show)

directionCommand :: Direction -> Integer
directionCommand North = 1
directionCommand East  = 3
directionCommand South = 2
directionCommand West  = 4

data Response
  = Wall
  | Moved
  | Oxygen
  deriving (Enum, Eq, Ord, Show)

-- Doesn't need to terminate for now
tryMoveD :: Direction -> Droid -> Maybe (Response, Droid)
tryMoveD dir d = do
  let vmWithInput = (getVm d) {inputs = [directionCommand dir]}
  (_, res) <- runUntil isDoneOrOutput vmWithInput
  o <- headM (outputs res)
  let r = toEnum (fromIntegral o) :: Response
  let pos = getPosition d
  let newPos =
        if r /= Wall
          then move dir pos
          else pos
  let newD = d {getVm = res {outputs = []}, getPosition = newPos}
  pure (r, newD)
  where
    isDoneOrOutput :: Operation -> Bool
    isDoneOrOutput Output = True
    isDoneOrOutput Done   = True
    isDoneOrOutput _      = False

data BFS =
  BFS
    { getSteps    :: Integer
    , getFrontier :: [Droid]
    , getSeen     :: Set.Set Point
    }
  deriving (Show)

initBfs :: Droid -> BFS
initBfs d =
  BFS {getSteps = 0, getFrontier = [d], getSeen = Set.singleton (Point 0 0)}

-- This could be expressed as a monad. Maybe refactor.
data Expansion
  = FoundSolution
  | ExpandedFrontier [Droid]

foundSolution :: Expansion -> Bool
foundSolution FoundSolution = True
foundSolution _             = False

mergeExpansions :: [Expansion] -> Expansion
mergeExpansions [] = ExpandedFrontier []
mergeExpansions [e@(ExpandedFrontier _)] = e
mergeExpansions (FoundSolution:_) = FoundSolution
mergeExpansions (_:FoundSolution:_) = FoundSolution
mergeExpansions ((ExpandedFrontier xs):(ExpandedFrontier ys):rest) =
  mergeExpansions (ExpandedFrontier (xs ++ ys) : rest)

tryAllUnseenDirections :: BFS -> Droid -> Maybe Expansion
tryAllUnseenDirections bfs d = do
  let pos = getPosition d
  let unseen dir = not (Set.member (move dir pos) (getSeen bfs))
  exp <-
    sequence
      [tryMoveD dir d | dir <- List.filter unseen [North, East, South, West]]
  let found = List.any (respIs Oxygen) exp
  if found
    then pure FoundSolution
    else pure (ExpandedFrontier (snd <$> List.filter (respIs Moved) exp))
  where
    respIs expected (resp, _) = resp == expected

expandFrontier :: BFS -> Maybe BFS
expandFrontier bfs = do
  let seenUpdate =
        Set.fromList (neighbourhood <*> [getPosition d | d <- getFrontier bfs])
  expansions <- sequence [tryAllUnseenDirections bfs d | d <- getFrontier bfs]
  let merged = mergeExpansions expansions
  let newBfs =
        bfs
          { getSteps = 1 + getSteps bfs
          , getSeen = getSeen bfs `Set.union` seenUpdate
          }
  case merged of
    FoundSolution       -> pure newBfs
    ExpandedFrontier ex -> expandFrontier newBfs {getFrontier = ex}

-- Part 2
data DFS =
  DFS
    { getDroid :: Droid
    , getPath  :: [Direction]
    , getMap   :: Map.Map Point Response
    }
  deriving (Show)

initDfs :: Droid -> DFS
initDfs d =
  DFS {getDroid = d, getPath = [], getMap = Map.singleton (Point 0 0) Moved}

backtrack :: DFS -> Maybe DFS
backtrack dfs
  | List.null (getPath dfs) = Just dfs
  | otherwise = do
    let (lastStep:oldPath) = getPath dfs
    (resp, oldDroid) <- tryMoveD (oppositeDir lastStep) (getDroid dfs)
    case resp of
      Wall -> Nothing -- This should never happen!
      _    -> pure dfs {getDroid = oldDroid, getPath = oldPath}

dfsExplore :: DFS -> Maybe DFS
dfsExplore dfs = do
  let droid = getDroid dfs
  let pos = getPosition droid
  let unseen dir = not (Map.member (move dir pos) (getMap dfs))
  let interestingDirections = List.filter unseen [North, East, South, West]
  let nextDir = headM interestingDirections
  case nextDir of
    Nothing -> do
      old <- backtrack dfs
      if List.null (getPath old)
        then pure old
        else dfsExplore old
    Just d -> do
      let nextPos = move d pos
      let nextPath = d : getPath dfs
      (resp, nextDroid) <- tryMoveD d droid
      let nextMap = Map.insert nextPos resp (getMap dfs)
      let nextDfs = dfs {getDroid = nextDroid, getMap = nextMap}
      case resp of
        Wall -> dfsExplore nextDfs
        _    -> dfsExplore nextDfs {getPath = nextPath}

-- Flood fill is also technically a BFS but much easier in this case since we
-- don't have to make use of the droid anymore and can work directly with the
-- points...
data FloodFill =
  FloodFill
    { getMapFF      :: Map.Map Point Response
    , getFillFF     :: Map.Map Point Integer
    , getFrontierFF :: [Point]
    , getStepFF     :: Integer
    }
  deriving (Show)

initFloodFillFromDFS :: DFS -> Maybe FloodFill
initFloodFillFromDFS dfs = do
  let m = getMap dfs
  (start, _) <- List.find isValueOxygen (Map.assocs m)
  pure
    FloodFill
      { getMapFF = m
      , getFillFF = Map.singleton start 0
      , getFrontierFF = [start]
      , getStepFF = 0
      }
  where
    isValueOxygen :: (Point, Response) -> Bool
    isValueOxygen (_, Oxygen) = True
    isValueOxygen _           = False

floodFill :: FloodFill -> FloodFill
floodFill ff = do
  let ns = neighbourhood <*> getFrontierFF ff
  let nextFrontier = List.filter (\p -> canBeVisited p && notYetFilled p) ns
  let nextStep = 1 + getStepFF ff
  let nextFill = Map.fromList ((, nextStep) <$> nextFrontier)
  if List.null nextFrontier
    then ff
    else floodFill
           ff
             { getFillFF = Map.union (getFillFF ff) nextFill
             , getFrontierFF = nextFrontier
             , getStepFF = nextStep
             }
  where
    canBeVisited p =
      case Map.lookup p (getMapFF ff) of
        Just Moved -> True
        _          -> False
    notYetFilled p = not (Map.member p (getFillFF ff))

drawMap :: (Maybe v -> Char) -> Map.Map Point v -> String
drawMap showV m =
  unlines $ do
    y <- [miny .. maxy]
    let row = [showV (Map.lookup (Point x y) m) | x <- [minx .. maxx]]
    pure row
  where
    positions = Map.keys m
    xs = getX <$> positions
    ys = getY <$> positions
    minx = List.minimum xs
    miny = List.minimum ys
    maxx = List.maximum xs
    maxy = List.maximum ys

showTile :: Maybe Response -> Char
showTile Nothing       = '░'
showTile (Just Wall)   = '█'
showTile (Just Moved)  = ' '
showTile (Just Oxygen) = 'o'

drawDfsMap :: DFS -> String
drawDfsMap = drawMap showTile . getMap

showFill :: Maybe Integer -> Char
showFill Nothing  = '█'
showFill (Just i) = last (show i)

drawFfMap :: FloodFill -> String
drawFfMap = drawMap showFill . getFillFF

-- Main
main :: IO ()
main = do
  contents <- getContents
  let prog = rightOrError (parse programParser "" contents)
  let d = Droid {getVm = initVm prog [], getPosition = Point 0 0}
  let (Just bfs) = expandFrontier (initBfs d)
  print (getSteps bfs)
  let (Just dfs) = dfsExplore (initDfs d)
  print (getMap dfs)
  putStrLn (drawDfsMap dfs)
  let (Just ff) = floodFill <$> initFloodFillFromDFS dfs
  putStrLn (drawFfMap ff)
  print (getStepFF ff)
