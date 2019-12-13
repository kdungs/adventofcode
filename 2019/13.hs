import           Intcode
import           Utils

import qualified Data.List                     as List
import           Data.List.Split               (chunksOf)
import qualified Data.Map.Strict               as Map
import           Text.ParserCombinators.Parsec (parse)

data Coordinate =
  Coordinate
    { getX :: Integer
    , getY :: Integer
    }
  deriving (Eq, Ord, Show)

data Tile
  = EmptyT
  | WallT
  | BlockT
  | HorizontalT
  | BallT
  | ScoreT Integer
  deriving (Eq, Ord)

tile :: Integer -> Tile
tile 0 = EmptyT
tile 1 = WallT
tile 2 = BlockT
tile 3 = HorizontalT
tile 4 = BallT
tile _ = error "Not a simple tile."

instance Show Tile where
  show EmptyT      = " "
  show WallT       = "█"
  show BlockT      = "▣"
  show HorizontalT = "^"
  show BallT       = "●"
  show (ScoreT s)  = "[" ++ show s ++ "]"

newtype Screen =
  Screen (Map.Map Coordinate Tile)

instance Show Screen where
  show scr@(Screen s) = List.intercalate "\n" $ showRow <$> ys
    where
      showRow y = List.concat (show . at <$> row y)
      row y = [Coordinate x y | x <- xs]
      at c = Map.findWithDefault EmptyT c s
      (tl, br) = dims scr
      ys = [getY tl .. getY br]
      xs = [getX tl .. getX br]

updateScreen :: Screen -> Screen -> Screen
updateScreen (Screen orig) (Screen upd) = Screen (upd `Map.union` orig)

dims :: Screen -> (Coordinate, Coordinate)
dims (Screen s) = (topLeft, bottomRight)
  where
    topLeft = Coordinate (List.minimum xs) (List.minimum ys)
    bottomRight = Coordinate (List.maximum xs) (List.maximum ys)
    xs = getX <$> Map.keys s
    ys = getY <$> Map.keys s

countTiles :: Tile -> Screen -> Int
countTiles t (Screen s) = Map.size (Map.filter isT s)
  where
    isT x = x == t

fromTriplet :: [Integer] -> (Coordinate, Tile)
fromTriplet [x, y, t]
  | x == (-1) && y == 0 = (Coordinate 0 (-1), ScoreT t)
  | otherwise = (Coordinate x y, tile t)
fromTriplet _ = error "not 3"

runArcadeProgram :: VirtualMachine -> Maybe Screen
runArcadeProgram vm = do
  doneVm <- run vm
  let os = List.reverse (outputs doneVm)
  let cs = chunksOf 3 os
  let s = Map.fromList (fromTriplet <$> cs)
  pure (Screen s)

data Arcade =
  Arcade
    { getVm      :: VirtualMachine
    , getScreen  :: Screen
    , isGameOver :: Bool
    }

newA :: VirtualMachine -> Arcade
newA vm =
  Arcade
    { getVm = vm {memory = setM 0 2 (memory vm)}
    , getScreen = Screen Map.empty
    , isGameOver = False
    }

instance Show Arcade where
  show a =
    show (getScreen a) ++
    (if isGameOver a
       then "\nGAME OVER\n"
       else "")

inputA :: Integer -> Arcade -> Arcade
inputA i a = a {getVm = (getVm a) {inputs = [i]}}

stepA :: Arcade -> Maybe Arcade
stepA a = do
  let vm = getVm a
  (waiting, nextVm) <- runUntilInputIsRequired vm
  let os = List.reverse (outputs nextVm)
  let updateScr = Screen (Map.fromList (fromTriplet <$> chunksOf 3 os))
  pure
    Arcade
      { getVm = nextVm {outputs = []}
      , getScreen = updateScreen (getScreen a) updateScr
      , isGameOver = not waiting
      }

solveArcade :: Arcade -> Maybe Arcade
solveArcade a@(Arcade _ _ True) = Just a
solveArcade a = do
  nextA <- stepA a
  let (Screen s) = getScreen nextA
  let ps = Map.assocs s
  (b, _) <- List.find ((== BallT) . snd) ps
  (p, _) <- List.find ((== HorizontalT) . snd) ps
  let i = signum (getX b - getX p)
  solveArcade (inputA i nextA)

main :: IO ()
main = do
  contents <- getContents
  let prog = rightOrError (parse programParser "" contents)
  let vm = initVm prog []
  let scr = justOrError (runArcadeProgram vm)
  let cnt = countTiles BlockT scr
  print cnt
  ---
  let arcade = newA vm
  print (solveArcade arcade)
