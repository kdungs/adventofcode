{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TupleSections      #-}

import           Control.Monad                        (mapM, sequence)
import qualified Data.List                            as List
import qualified Data.Map.Strict                      as M
import           Data.Maybe                           (catMaybes)
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Number (int)

data Operation
  = Done
  | Add
  | Mul
  | In
  | Out
  | JmpT
  | JmpF
  | Less
  | Eql
  deriving (Show)

data ParameterMode
  = PositionMode
  | ImmediateMode
  deriving (Show)

data Instruction =
  Instruction
    { op    :: Operation
    , modes :: [ParameterMode]
    }
  deriving (Show)

applyBinaryOperation :: Operation -> Int -> Int -> Maybe Int
applyBinaryOperation Add x y = Just (x + y)
applyBinaryOperation Mul x y = Just (x * y)
applyBinaryOperation Less x y =
  Just
    (if x < y
       then 1
       else 0)
applyBinaryOperation Eql x y =
  Just
    (if x == y
       then 1
       else 0)
applyBinaryOperation _ _ _ = Nothing

operationFromNumber :: Int -> Maybe Operation
operationFromNumber 1  = Just Add
operationFromNumber 2  = Just Mul
operationFromNumber 3  = Just In
operationFromNumber 4  = Just Out
operationFromNumber 5  = Just JmpT
operationFromNumber 6  = Just JmpF
operationFromNumber 7  = Just Less
operationFromNumber 8  = Just Eql
operationFromNumber 99 = Just Done
operationFromNumber _  = Nothing

parameterModeFromNumber :: Int -> Maybe ParameterMode
parameterModeFromNumber 0 = Just PositionMode
parameterModeFromNumber 1 = Just ImmediateMode
parameterModeFromNumber _ = Nothing

parameterModesFromNumber :: Int -> Maybe [ParameterMode]
parameterModesFromNumber x = mapM parameterModeFromNumber (rdigits x)
  where
    rdigits :: Int -> [Int]
    rdigits 0 = []
    rdigits x = x `mod` 10 : rdigits (x `div` 10)

instructionFromNumber :: Int -> Maybe Instruction
instructionFromNumber x = do
  op <- operationFromNumber (x `mod` 100)
  mode <- parameterModesFromNumber (x `div` 100)
  pure (Instruction op mode)

type Position = Int

newtype Memory =
  Memory (M.Map Position Int)

valueAt :: Position -> Memory -> Maybe Int
valueAt pos (Memory mem) = M.lookup pos mem

dereferenceAt :: Position -> Memory -> Maybe Int
dereferenceAt pos mem = do
  ptr <- valueAt pos mem
  valueAt ptr mem

atWithMode :: ParameterMode -> Position -> Memory -> Maybe Int
atWithMode PositionMode  = dereferenceAt
atWithMode ImmediateMode = valueAt

instructionAt :: Position -> Memory -> Maybe Instruction
instructionAt pos mem = valueAt pos mem >>= instructionFromNumber

setMemory :: Position -> Int -> Memory -> Memory
setMemory pos val (Memory mem) = Memory (M.insert pos val mem)

data Context =
  Context
    { memory  :: Memory
    , fptr    :: Int
    , inputs  :: [Int]
    , outputs :: [Int]
    }
  deriving (Show)

modeFor :: Int -> [ParameterMode] -> ParameterMode
modeFor x ms
  | x >= List.length ms = PositionMode
  | otherwise = ms !! x

applyInstruction :: Instruction -> Context -> Maybe Context
applyInstruction (Instruction Done _) ctx = Just ctx
applyInstruction (Instruction In _) (Context mem fptr ins outs) = do
  ref <- valueAt (fptr + 1) mem
  let input = head ins -- Use something like headMay!
  let newIns = tail ins
  let newMem = setMemory ref input mem
  pure (Context newMem (fptr + 2) newIns outs)
applyInstruction (Instruction Out modes) (Context mem fptr ins outs) = do
  ref <- atWithMode (modeFor 0 modes) (fptr + 1) mem
  let newOuts = ref : outs
  pure (Context mem (fptr + 2) ins newOuts)
applyInstruction (Instruction JmpT modes) (Context mem fptr ins outs) = do
  cond <- atWithMode (modeFor 0 modes) (fptr + 1) mem
  val <- atWithMode (modeFor 1 modes) (fptr + 2) mem
  let newFptr =
        if cond /= 0
          then val
          else fptr + 3
  pure (Context mem newFptr ins outs)
applyInstruction (Instruction JmpF modes) (Context mem fptr ins outs) = do
  cond <- atWithMode (modeFor 0 modes) (fptr + 1) mem
  val <- atWithMode (modeFor 1 modes) (fptr + 2) mem
  let newFptr =
        if cond == 0
          then val
          else fptr + 3
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
    _    -> applyInstruction instruction ctx >>= execute

defaultContextWithInputs :: [Int] -> Memory -> Context
defaultContextWithInputs ins mem = Context mem 0 ins []

-- Day 07
executeInSequence :: [Int] -> Int -> Memory -> Maybe Int
executeInSequence [] input _ = Just input
executeInSequence (phase:phases) input mem = do
  res <- execute (defaultContextWithInputs [phase, input] mem)
  let out = head (outputs res)
  executeInSequence phases out mem

findHighestAmplification :: Memory -> Int
findHighestAmplification mem =
  List.maximum $
  catMaybes [executeInSequence perm 0 mem | perm <- List.permutations [0 .. 4]]

executeUntilOutput :: Context -> Maybe (Operation, Context)
executeUntilOutput ctx@(Context mem fptr ins outs) = do
  instruction <- instructionAt fptr mem
  case op instruction of
    Done -> Just (Done, ctx)
    Out  -> (Out, ) <$> applyInstruction instruction ctx
    _    -> applyInstruction instruction ctx >>= executeUntilOutput

newtype AmplifierChain =
  AmplifierChain [Context]

configureChain :: [Int] -> Int -> Memory -> AmplifierChain
configureChain (phase0:phases) start mem =
  AmplifierChain
    (defaultContextWithInputs [phase0, start] mem :
     [defaultContextWithInputs [phase] mem | phase <- phases])

runChain :: AmplifierChain -> Maybe Int
runChain (AmplifierChain [e]) = do
  newE <- execute e
  pure (head . outputs $ newE)
runChain (AmplifierChain (current:next:rest)) = do
  (op, newCurrent) <- executeUntilOutput current
  case op of
    Done -> runChain (AmplifierChain (next : rest))
    Out ->
      runChain
        (AmplifierChain
           ((next {inputs = inputs next ++ [head . outputs $ newCurrent]}) :
            rest ++ [newCurrent]))
    _ -> Nothing -- Error!

findHighestAmplificationForChains :: Memory -> Int
findHighestAmplificationForChains mem =
  List.maximum $
  catMaybes
    [runChain (configureChain perm 0 mem) | perm <- List.permutations [5 .. 9]]

-- Parser
program :: Parser Memory
program = do
  code <- sepBy int (char ',')
  return (Memory (M.fromList (zip [0 ..] code)))

rightMay :: Either a b -> Maybe b
rightMay (Left _)  = Nothing
rightMay (Right x) = Just x

-- Main
main :: IO ()
main = do
  contents <- getContents
  let mem = rightMay (parse program "" contents)
  let p1 = findHighestAmplification <$> mem
  print p1
  let p2 = findHighestAmplificationForChains <$> mem
  print p2
