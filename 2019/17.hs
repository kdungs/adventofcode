import Intcode

import Data.Char (chr, ord)
import Text.ParserCombinators.Parsec (parse)
import qualified Data.List as List
import qualified Data.Map.Strict as Map

toAsciiString :: [Integer] -> String
toAsciiString xs = chr <$> fromIntegral <$> xs


data Point = Point { getX :: Int
                   , getY :: Int
                   } deriving (Eq, Ord, Show)

data Direction = North | East | South | West deriving (Eq, Ord, Show)

right :: Direction -> Direction
right North = East
right East = South
right South = West
right West = North

left :: Direction -> Direction
left North = West
left East = North
left South = East
left West = South

move :: Direction -> Point -> Point
move North p = p { getY = getY p - 1 }
move East p = p { getX = getX p + 1 }
move South p = p { getY = getY p + 1 }
move West p = p { getX = getX p - 1 }

neighbours :: Point -> [Point]
neighbours p = [ move North
               , move East
               , move West
               , move South
               ] <*> [p]

data Tile = Scaffolding | Robot Direction deriving (Eq, Ord, Show)

type TileMap = Map.Map Point Tile

parseMap :: [String] -> TileMap
parseMap rows = Map.fromList $ do
  (row, y) <- zip rows [0 .. ]
  (c, x) <- zip row [0 .. ]
  case c of
    '#' -> pure (Point x y, Scaffolding)
    '^' -> pure (Point x y, Robot North)
    '>' -> pure (Point x y, Robot East)
    'v' -> pure (Point x y, Robot South)
    '<' -> pure (Point x y, Robot West)
    _ -> []

onMap :: TileMap -> Point -> Bool
onMap m p = p `Map.member` m

hasNeighbours :: TileMap -> Point-> Bool
hasNeighbours m p = List.all (onMap m) (neighbours p)

intersections :: TileMap -> [Point]
intersections m = List.filter (hasNeighbours m) (Map.keys m)

alignmentParameter :: Point -> Int
alignmentParameter p = getX p * getY p

sumAlignmentParameters :: [Point] -> Int
sumAlignmentParameters ps = sum (alignmentParameter <$> ps)

part1 :: TileMap -> Int
part1 = sumAlignmentParameters . intersections


data MoveCommand = TurnL | TurnR | Fwd Int deriving (Eq, Ord)

instance Show MoveCommand where
  show TurnL = "L"
  show TurnR = "R"
  show (Fwd x) = show x

type Path = [MoveCommand]

pushP :: MoveCommand -> Path -> Path
pushP (Fwd x) (Fwd y : cs) = Fwd (x + y) : cs
pushP TurnL (TurnL:TurnL:cs) = TurnR : cs
pushP TurnR (TurnR:TurnR:cs) = TurnL : cs
pushP c cs = c : cs

data Pathfinder = Pathfinder { getPos :: Point
                             , getDir :: Direction
                             , getPath :: Path
                             } deriving Show

initPathfinder :: TileMap -> Maybe Pathfinder
initPathfinder m = do
  (p, (Robot dir)) <- List.find (\(_, v) -> isRobot v) (Map.assocs m)
  pure Pathfinder { getPos = p
                  , getDir = dir
                  , getPath = []
                  }
  where isRobot :: Tile -> Bool
        isRobot (Robot _) = True
        isRobot _ = False

findPath :: TileMap -> Pathfinder -> Pathfinder
findPath m pf@(Pathfinder pos dir path)
  | onMap m nextFwd = findPath m pf { getPos = nextFwd
                                    , getDir = dir
                                    , getPath = pushP (Fwd 1) path
                                    }
  | onMap m nextLft = findPath m pf { getPos = nextLft
                                    , getDir = left dir
                                    , getPath = pushP (Fwd 1) (pushP TurnL path)
                                    }
  | onMap m nextRgt = findPath m pf { getPos = nextRgt
                                    , getDir = right dir
                                    , getPath = pushP (Fwd 1) (pushP TurnR path)
                                    }
  | otherwise = pf  -- end of the journey.
 where nextFwd = move dir pos
       nextLft = move (left dir) pos
       nextRgt = move (right dir) pos

main :: IO ()
main = do
  contents <- getContents
  let (Right prog) = parse programParser "" contents
  let (Just vm) = run (initVm prog [])
  let res = toAsciiString (List.reverse (outputs vm))
  let lines = List.dropWhile (List.null) (List.lines res)
  let m = parseMap lines
  print (part1 m)
  -- Part II
  let (Just pf) = initPathfinder m
  let path = List.reverse (getPath (findPath m pf))
  let p = (List.concat (show <$> path))
  putStrLn p
  -- Some manual work...
  -- Yes, we could do this with backtracking but why if we can just greedily do
  -- it in Vim ¯\_(ツ)_/¯ 
  let functions = [ "A,B,A,C,B,C,B,A,C,B\n"
                  , "L,10,L,6,R,10\n"
                  , "R,6,R,8,R,8,L,6,R,8\n"
                  , "L,10,R,8,R,8,L,10\n"
                  , "n\n"
                  ]
  let inputs = (toInteger . ord) <$> (List.concat functions)
  let vm2 = initVm (setM 0 2 prog) inputs
  let (Just done) = run vm2
  putStrLn (toAsciiString (List.reverse (outputs done)))

example = [ "..#.........."
          , "..#.........."
          , "#######...###"
          , "#.#...#...#.#"
          , "#############"
          , "..#...#...#.."
          , "..#####...^.."
          ]
