import qualified Data.Map.Strict               as M
import           Text.ParserCombinators.Parsec

data Instruction
  = Done
  | Add
  | Mul
  deriving (Show)

applyInstruction :: Instruction -> Int -> Int -> Maybe Int
applyInstruction Add x y = Just (x + y)
applyInstruction Mul x y = Just (x * y)
applyInstruction _ _ _   = Nothing

type Position = Int

newtype Memory =
  Memory (M.Map Position Int)
  deriving (Show)

instructionFromNumber :: Int -> Maybe Instruction
instructionFromNumber 1  = Just Add
instructionFromNumber 2  = Just Mul
instructionFromNumber 99 = Just Done
instructionFromNumber _  = Nothing

instructionAt :: Position -> Memory -> Maybe Instruction
instructionAt pos (Memory mem) = do
  num <- M.lookup pos mem
  instructionFromNumber num

valueAt :: Position -> Memory -> Maybe Int
valueAt pos (Memory mem) = M.lookup pos mem

dereferenceAt :: Position -> Memory -> Maybe Int
dereferenceAt pos mem = do
  ptr <- valueAt pos mem
  valueAt ptr mem

setMemory :: Position -> Int -> Memory -> Memory
setMemory pos val (Memory mem) = Memory (M.insert pos val mem)

execute :: Position -> Memory -> Maybe Memory
execute pos mem =
  case instructionAt pos mem of
    Nothing -> Nothing
    Just Done -> Just mem
    Just op -> do
      lhs <- dereferenceAt (pos + 1) mem
      rhs <- dereferenceAt (pos + 2) mem
      dst <- valueAt (pos + 3) mem
      val <- applyInstruction op lhs rhs
      execute (pos + 4) (setMemory dst val mem)

modify :: Int -> Int -> Memory -> Memory
modify noun verb (Memory mem) = Memory (M.insert 1 noun (M.insert 2 verb mem))

checkSolution :: Int -> Int -> Int -> Memory -> Maybe Bool
checkSolution target noun verb memory = do
  res <- execute 0 (modify noun verb memory)
  val <- valueAt 0 res
  pure (target == val)

findSolution :: Int -> Memory -> (Int, Int)
findSolution target mem =
  head (dropWhile isNotSolution [(n, v) | n <- [0 .. 100], v <- [0 .. 100]])
  where
    isNotSolution :: (Int, Int) -> Bool
    isNotSolution (noun, verb) =
      case checkSolution target noun verb mem of
        Nothing  -> True -- meh!
        (Just b) -> not b

-- Parser
int :: Parser Int
int = read <$> many1 digit

memory :: Parser Memory
memory = do
  code <- sepBy int (char ',')
  pure (Memory (M.fromList (zip [0 ..] code)))

-- Main
main :: IO ()
main = do
  contents <- getContents
  let mem = parse memory "" contents
  let res = execute 0 . modify 12 2 <$> mem
  print $ (\mem -> valueAt 0 <$> mem) <$> res
  print $ findSolution 19690720 <$> mem
