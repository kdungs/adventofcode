import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Text.ParserCombinators.Parsec

data Point = Point { x :: Int,
                     y :: Int } deriving (Eq, Ord, Show)

data Direction = UP | RIGHT | DOWN | LEFT deriving (Eq, Ord, Show)

data Instruction = Instruction Direction Int deriving Show

data Step = Step { idx :: Int,
                   ins :: Instruction } deriving Show

movePoint :: Point -> Direction -> Point
movePoint p UP = p { y = y p + 1}
movePoint p RIGHT = p { x = x p + 1}
movePoint p DOWN = p { y = y p - 1}
movePoint p LEFT = p { x = x p - 1}

unrollInstruction :: Instruction -> [Direction]
unrollInstruction (Instruction dir cnt) = replicate cnt dir

unrollInstructions :: [Instruction] -> [Direction]
unrollInstructions = concat . List.map unrollInstruction

wirePoints :: [Instruction] -> [Point]
wirePoints ins = scanl movePoint (Point 0 0) (unrollInstructions ins)

stepMap :: [Point] -> Map.Map Point Int
stepMap ps = Map.fromListWith min (zip ps [1..])

intersectStepMaps :: Map.Map Point Int -> Map.Map Point Int -> Map.Map Point Int
intersectStepMaps m1 m2 = Map.intersectionWith (\a b -> a + b) m1 m2

manhattenDist :: Point -> Int
manhattenDist (Point x y) = (abs x) + (abs y)

prepareMap :: [Instruction] -> [Instruction] -> Map.Map Point Int
prepareMap ins1 ins2 = intersectStepMaps m1 m2
  where m1 = convert ins1
        m2 = convert ins2
        convert = stepMap . tail . wirePoints

-- Parser
int :: Parser Int
int = read <$> many1 digit

instruction :: Parser Instruction
instruction = do
  direction <- char 'U' *> pure UP
           <|> char 'R' *> pure RIGHT
           <|> char 'D' *> pure DOWN
           <|> char 'L' *> pure LEFT
  count <- int
  pure (Instruction direction count)

instructions :: Parser [Instruction]
instructions = sepBy instruction (char ',')


-- Solvers
part1 :: Map.Map Point Int -> Int
part1 m = List.minimum (List.map manhattenDist (Map.keys m))

part2 :: Map.Map Point Int -> Int
part2 m = List.minimum (Map.elems m)

-- Main
main :: IO ()
main = do
  contents <- getContents
  let wireInstructions = parse (sepEndBy instructions newline) "" contents
  let ins1 = head <$> wireInstructions
  let ins2 = head . tail <$> wireInstructions
  let smap = prepareMap <$> ins1 <*> ins2
  putStrLn . show $ part1 <$> smap
  putStrLn . show $ part2 <$> smap
