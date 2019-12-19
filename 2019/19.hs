import           Intcode

import qualified Data.Array                    as Array
import           Data.Ix                       (Ix, inRange, range)
import qualified Data.List                     as List
import           Text.ParserCombinators.Parsec (parse)

data Point =
  Point
    { getX :: Integer
    , getY :: Integer
    }
  deriving (Eq, Ord)

addP :: Point -> Point -> Point
addP p1 p2 = Point {getX = getX p1 + getX p2, getY = getY p1 + getY p2}

instance Show Point where
  show p = "(" ++ show (getX p) ++ ", " ++ show (getY p) ++ ")"

instance Ix Point where
  index (tl, br) p = fromIntegral (y * w + x)
    where
      x = getX p - getX tl
      y = getY p - getY tl
      w = getX br - getX tl + 1
  range (tl, br) = do
    y <- [getY tl .. getY br]
    x <- [getX tl .. getX br]
    pure (Point x y)
  inRange (tl, br) p
    | getX p < getX tl = False
    | getX p > getX br = False
    | getY p < getY tl = False
    | getY p > getY br = False
    | otherwise = True

type Beam = Array.Array Point Bool

checkPosition :: VirtualMachine -> Point -> Bool
checkPosition vm p = head (outputs nextVm) == 1
  where
    (Just (_, nextVm)) = runUntilInputIsRequired vm {inputs = [getX p, getY p]}

extractBeam :: VirtualMachine -> Point -> Point -> Beam
extractBeam vm tl br =
  Array.listArray (tl, br) [checkPosition vm p | p <- range (tl, br)]

part1 :: VirtualMachine -> Int
part1 vm =
  List.length
    (List.filter
       (== True)
       (Array.elems (extractBeam vm (Point 0 0) (Point 49 49))))

isSquareContained :: VirtualMachine -> Integer -> Point -> Bool
isSquareContained vm sq p =
  checkPosition vm p &&
  checkPosition vm p {getX = getX p + sq - 1, getY = getY p - sq + 1}

leftEdge :: VirtualMachine -> Integer -> Point
leftEdge vm y =
  List.head
    (List.dropWhile (not . checkPosition vm) [Point x y | x <- [y `div` 2 ..]])

part2 :: VirtualMachine -> Point
part2 vm = found {getY = getY found - 99}
  where
    sq = 100
    found =
      List.head . List.dropWhile (not . isSquareContained vm sq) $
      [leftEdge vm y | y <- [1000 ..]]

checkSquare :: VirtualMachine -> Integer -> Point -> [Bool]
checkSquare vm sq p =
  checkPosition vm <$>
  [ p
  , p {getX = getX p + sq - 1}
  , p {getY = getY p + sq - 1}
  , p {getX = getX p + sq - 1, getY = getY p + sq - 1}
  ]

main :: IO ()
main = do
  contents <- getContents
  let (Right prog) = parse programParser "" contents
  let vm = initVm prog []
  print $ part1 vm
  print $ part2 vm
