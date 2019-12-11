import           Intcode
import           Utils

import qualified Data.List                     as List
import qualified Data.Map.Strict               as Map
import           Text.ParserCombinators.Parsec

data Direction
  = North
  | East
  | South
  | West

right :: Direction -> Direction
right North = East
right East  = South
right South = West
right West  = North

left :: Direction -> Direction
left North = West
left West  = South
left South = East
left East  = North

data Point =
  Point
    { getX :: Integer
    , getY :: Integer
    }
  deriving (Eq, Ord)

instance Show Point where
  show (Point x y) = "(" ++ show x ++ "," ++ show y ++ ")"

move :: Direction -> Point -> Point
move North p = p {getY = getY p + 1}
move East p  = p {getX = getX p + 1}
move South p = p {getY = getY p - 1}
move West p  = p {getX = getX p - 1}

data Robot =
  Robot
    { brain     :: VirtualMachine
    , direction :: Direction
    , position  :: Point
    }

data Turn
  = LeftT
  | RightT

toTurn :: Integer -> Maybe Turn
toTurn 0 = Just LeftT
toTurn 1 = Just RightT
toTurn _ = Nothing

turn :: Turn -> Direction -> Direction
turn LeftT  = left
turn RightT = right

turnAndMove :: Turn -> Robot -> Robot
turnAndMove t r =
  r {direction = newDirection, position = move newDirection (position r)}
  where
    newDirection = turn t (direction r)

data Color
  = Black
  | White

instance Show Color where
  show Black = "█"
  show White = " "

toColor :: Integer -> Maybe Color
toColor 0 = Just Black
toColor 1 = Just White
toColor _ = Nothing

colorAsNumber :: Color -> Integer
colorAsNumber Black = 0
colorAsNumber White = 1

type Hull = Map.Map Point Color

showRow :: Integer -> Integer -> Integer -> Hull -> String
showRow y x0 w hull =
  List.concat $ do
    x <- [x0 .. x0 + w]
    let c = Map.findWithDefault Black (Point x y) hull
    pure (show c)

showHull :: Integer -> Integer -> Integer -> Integer -> Hull -> String
showHull x0 y0 w h hull =
  List.intercalate "\n" $ do
    y <- List.reverse [y0 .. y0 + h]
    pure (showRow y x0 w hull)

runRobot :: Integer -> Hull -> Robot -> Maybe (Hull, Robot)
runRobot i h r = do
  let vmWithInput = (brain r) {inputs = [i]}
  (op, newVm) <- runUntil isOutputOrDone vmWithInput
  case op of
    Done -> pure (h, r {brain = newVm})
    Output -> do
      (op2, newVm2) <- runUntil isOutputOrDone newVm
      case op2 of
        Output -> do
          let (tur:col:_) = outputs newVm2
          t <- toTurn tur
          c <- toColor col
          let newHull = Map.insert (position r) c h
          let newRobot = turnAndMove t (r {brain = newVm2})
          let newInput =
                colorAsNumber
                  (Map.findWithDefault Black (position newRobot) newHull)
          runRobot newInput newHull newRobot
        _ -> Nothing -- Error!
    _ -> Nothing -- Should not happen...
  where
    isOutputOrDone :: Operation -> Bool
    isOutputOrDone Done   = True
    isOutputOrDone Output = True
    isOutputOrDone _      = False

main :: IO ()
main = do
  contents <- getContents
  let prog = rightOrError (parse programParser "" contents)
  let robert =
        Robot {brain = initVm prog [], position = Point 0 0, direction = North}
  let res1 = runRobot 0 (Map.singleton (Point 0 0) Black) robert
  print $ Map.size . fst <$> res1
  let res2 = runRobot 1 (Map.singleton (Point 0 0) White) robert
  case res2 of
    Nothing        -> print "ERROR"
    Just (hull, _) -> putStrLn $ showHull 0 (-5) 42 5 hull
