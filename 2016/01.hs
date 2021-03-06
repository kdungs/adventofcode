import Control.Monad
import Control.Monad.State.Lazy
import Data.Char
import Data.List
import Text.ParserCombinators.Parsec

data Direction = North | East | South | West deriving (Show)
data Rotation = Counterclockwise | Clockwise deriving (Show)

turnLeft :: Direction -> Direction
turnLeft dir = case (dir) of
                 North -> West
                 West -> South
                 South -> East
                 East -> North

turnRight :: Direction -> Direction
turnRight dir = case (dir) of
                  North -> East 
                  East -> South
                  South -> West
                  West -> North

turn :: Rotation -> Direction -> Direction
turn rot = case (rot) of
             Counterclockwise -> turnLeft
             Clockwise -> turnRight

data Position = Position Int Int deriving (Show)
instance Eq Position where
  (Position x1 y1) == (Position x2 y2) = x1 == x2 && y1 == y2

move :: Position -> Direction -> Int -> Position
move (Position x y) dir n = case (dir) of
                              North -> Position x (y + n)
                              East -> Position (x + n) y
                              South -> Position x (y - n)
                              West -> Position (x - n) y

data Step = Step Position Direction deriving (Show)

defaultStep :: Step
defaultStep = Step (Position 0 0) North

data Instruction = Instruction Rotation Int deriving (Show)

applyInstruction :: Step -> Instruction -> Step
applyInstruction (Step pos dir) (Instruction rot steps) = Step (move pos newdir steps) newdir
                                                           where newdir = turn rot dir

applyInstructions :: Step -> [Instruction] -> Step
applyInstructions s0 instructions = foldl applyInstruction s0 instructions

findFinalPosition :: [Instruction] -> Position
findFinalPosition input = pos where (Step pos _) = applyInstructions defaultStep input

totalBlocks :: Position -> Int
totalBlocks (Position x y) = (abs x) + (abs y)

-- Part 2.
data PS = Final Position | Current Position [Position] deriving (Show)

moveOne :: PS -> Direction -> PS
moveOne (Final p) _ = Final p
moveOne (Current p@(Position x y) ps) d = case (elem newp ps) of
                                            True -> Final newp
                                            False -> Current newp (newp:ps)
                                          where newp = move p d 1

move2 :: PS -> Direction -> Int -> PS
move2 p d n = case n of
                0 -> p
                _ -> move2 (moveOne p d) d (n - 1)

app2 :: (PS, Direction) -> Instruction -> (PS, Direction)
app2 (p, d) (Instruction r n) = (move2 p newdir n, newdir) where newdir = turn r d

blocks2 :: (PS, Direction) -> Maybe Int
blocks2 (Current _ _, _) = Nothing
blocks2 (Final p, _) = Just (totalBlocks p)

-- Deal with input.
convertRotation :: Char -> Rotation
convertRotation c = case (c) of
                      'L' -> Counterclockwise
                      'R' -> Clockwise

parseRotation :: Parser Rotation
parseRotation = convertRotation <$> oneOf "LR"

convertDecimal :: String -> Int
convertDecimal = foldl' (\a i -> a * 10 + digitToInt i) 0

parseDecimal :: Parser Int
parseDecimal =  convertDecimal <$> many1 digit

parseInstruction :: Parser Instruction
parseInstruction = do
                     r <- parseRotation;
                     d <- parseDecimal;
                     return (Instruction r d) 

parseSeparator :: Parser ()
parseSeparator = skipMany1 (space <|> char ',')

parseInstructions :: Parser [Instruction]
parseInstructions = sepBy1 parseInstruction parseSeparator

-- Main.
main :: IO ()
main = do
         line <- getLine
         let inss = parse parseInstructions "" line
         putStrLn (show (totalBlocks <$> findFinalPosition <$> inss))
         let result2 = blocks2 <$> foldl app2 (Current (Position 0 0) [], North) <$> inss
         putStrLn . show $ result2
