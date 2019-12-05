import Control.Monad (sequence)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number (int)
import qualified Data.List as List
import qualified Data.Map.Strict as M


data Operation = Done
               | Add
               | Mul
               | In
               | Out
               | JmpT
               | JmpF
               | Less
               | Eql
               deriving Show
data ParameterMode = PositionMode | ImmediateMode deriving Show

data Instruction = Instruction { op :: Operation,
                                 modes :: [ParameterMode] } deriving Show


applyBinaryOperation :: Operation -> Int -> Int -> Maybe Int
applyBinaryOperation Add x y = Just (x + y)
applyBinaryOperation Mul x y = Just (x * y)
applyBinaryOperation Less x y = Just (if (x < y) then 1 else 0)
applyBinaryOperation Eql x y = Just (if (x == y) then 1 else 0)
applyBinaryOperation _ _ _ = Nothing

operationFromNumber :: Int -> Maybe Operation
operationFromNumber 1 = Just Add
operationFromNumber 2 = Just Mul
operationFromNumber 3 = Just In
operationFromNumber 4 = Just Out
operationFromNumber 5 = Just JmpT
operationFromNumber 6 = Just JmpF
operationFromNumber 7 = Just Less
operationFromNumber 8 = Just Eql 
operationFromNumber 99 = Just Done
operationFromNumber _ = Nothing

parameterModeFromNumber :: Int -> Maybe ParameterMode
parameterModeFromNumber 0 = Just PositionMode
parameterModeFromNumber 1 = Just ImmediateMode
parameterModeFromNumber _ = Nothing

parameterModesFromNumber :: Int -> Maybe [ParameterMode]
parameterModesFromNumber x = sequence (map parameterModeFromNumber (rdigits x))
  where rdigits :: Int -> [Int]
        rdigits 0 = []
        rdigits x = (x `mod` 10):(rdigits (x `div` 10))

instructionFromNumber :: Int -> Maybe Instruction
instructionFromNumber x = do
  op <- operationFromNumber (x `mod` 100)
  mode <- parameterModesFromNumber (x `div` 100)
  pure (Instruction op mode)

type Position = Int
data Memory = Memory (M.Map Position Int) deriving Show

valueAt :: Position -> Memory -> Maybe Int
valueAt pos (Memory mem) = M.lookup pos mem

dereferenceAt :: Position -> Memory -> Maybe Int
dereferenceAt pos mem = do
  ptr <- valueAt pos mem
  valueAt ptr mem

atWithMode :: ParameterMode -> Position -> Memory -> Maybe Int
atWithMode PositionMode = dereferenceAt
atWithMode ImmediateMode = valueAt

instructionAt :: Position -> Memory -> Maybe Instruction
instructionAt pos mem = (valueAt pos mem) >>= instructionFromNumber

setMemory :: Position -> Int -> Memory -> Memory
setMemory pos val (Memory mem) = Memory (M.insert pos val mem)

data Context = Context { memory :: Memory,
                         fptr :: Int,
                         inputs :: [Int],
                         outputs :: [Int] } deriving Show

modeFor :: Int -> [ParameterMode] -> ParameterMode
modeFor x ms
  | x >= List.length ms = PositionMode
  | otherwise = ms !! x

applyInstruction :: Instruction -> Context -> Maybe Context
applyInstruction (Instruction Done _) ctx = Just ctx
applyInstruction (Instruction In _) (Context mem fptr ins outs) = do
  ref <- valueAt (fptr + 1) mem
  let input = head ins  -- Use something like headMay!
  let newIns = tail ins
  let newMem = setMemory ref input mem
  pure (Context newMem (fptr + 2) newIns outs)
applyInstruction (Instruction Out modes) (Context mem fptr ins outs) = do
  ref <- atWithMode (modeFor 0 modes) (fptr + 1) mem
  let newOuts = ref:outs
  pure (Context mem (fptr + 2) ins newOuts)
applyInstruction (Instruction JmpT modes) (Context mem fptr ins outs) = do
  cond <- atWithMode (modeFor 0 modes) (fptr + 1) mem
  val <- atWithMode (modeFor 1 modes) (fptr + 2) mem
  let newFptr = if (cond /= 0) then val else (fptr + 3)
  pure (Context mem newFptr ins outs)
applyInstruction (Instruction JmpF modes) (Context mem fptr ins outs) = do
  cond <- atWithMode (modeFor 0 modes) (fptr + 1) mem
  val <- atWithMode (modeFor 1 modes) (fptr + 2) mem
  let newFptr = if (cond == 0) then val else (fptr + 3)
  pure (Context mem newFptr ins outs)
applyInstruction (Instruction op modes) (Context mem fptr ins outs) = do
  lhs <- atWithMode (modeFor 0 modes) (fptr + 1) mem
  rhs <- atWithMode (modeFor 1 modes) (fptr + 2) mem
  dst <- valueAt (fptr + 3) mem
  val <- applyBinaryOperation op lhs rhs
  let newMem = setMemory dst val mem
  pure (Context newMem (fptr + 4) ins outs)

execute :: Context -> Maybe Context 
execute ctx@(Context mem fptr ins outs) = do
  instruction <- instructionAt fptr mem
  case op instruction of
    Done -> Just ctx
    _ -> (applyInstruction instruction ctx) >>= execute


defaultContextWithInputs :: [Int] -> Memory  -> Context
defaultContextWithInputs ins mem = Context mem 0 ins []

-- Parser
program :: Parser Memory
program = do
  code <- sepBy int (char ',')
  return (Memory (M.fromList (zip [0..] code)))

-- Main
main :: IO ()
main = do
  contents <- getContents
  let mem = parse program "" contents
  let ctx = defaultContextWithInputs [5] <$> mem
  let res = execute <$> ctx
  putStrLn . show $ (\r -> outputs <$> r) <$> res
