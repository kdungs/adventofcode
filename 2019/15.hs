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

-- Main
main :: IO ()
main = do
  contents <- getContents
  let prog = rightOrError (parse programParser "" contents)
  let d = Droid {getVm = initVm prog [], getPosition = Point 0 0}
  let (Just bfs) = expandFrontier (initBfs d)
  print (getSteps bfs)
