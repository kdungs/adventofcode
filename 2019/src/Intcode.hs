{-# LANGUAGE TupleSections #-}

module Intcode
  ( module Intcode
  ) where

import           Utils                                (atWithDefault, headM)

import           Control.Monad                        (mapM)
import qualified Data.List                            as List
import qualified Data.Map.Strict                      as Map
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Number (int)

data Operation
  = Done
  | Input
  | Output
  | JumpTrue
  | JumpFalse
  | Add
  | Multiply
  | Less
  | Equals
  deriving (Show)

operationFromNumber :: Int -> Maybe Operation
operationFromNumber x = Map.lookup x opMap
  where
    opMap :: Map.Map Int Operation
    opMap =
      Map.fromList
        [ (1, Add)
        , (2, Multiply)
        , (3, Input)
        , (4, Output)
        , (5, JumpTrue)
        , (6, JumpFalse)
        , (7, Less)
        , (8, Equals)
        , (99, Done)
        ]

data Mode
  = Reference
  | Value
  deriving (Show)

modeFromNumber :: Int -> Maybe Mode
modeFromNumber 0 = Just Reference
modeFromNumber 1 = Just Value
modeFromNumber _ = Nothing

modesFromNumber :: Int -> Maybe [Mode]
modesFromNumber x = mapM modeFromNumber (rdigits x)
  where
    rdigits :: Int -> [Int]
    rdigits 0 = []
    rdigits x = x `mod` 10 : rdigits (x `div` 10)

data Instruction =
  Instruction
    { op    :: Operation
    , modes :: [Mode]
    }
  deriving (Show)

instructionFromNumber :: Int -> Maybe Instruction
instructionFromNumber x = do
  op <- operationFromNumber (x `mod` 100)
  ms <- modesFromNumber (x `div` 100)
  pure (Instruction op ms)

modeFor :: Int -> [Mode] -> Mode
modeFor = atWithDefault Reference

type Position = Int

type Memory = Map.Map Position Int

value :: Position -> Memory -> Maybe Int
value = Map.lookup

dereference :: Position -> Memory -> Maybe Int
dereference pos mem = value pos mem >>= \ptr -> Map.lookup ptr mem

getM :: Mode -> Position -> Memory -> Maybe Int
getM Reference = dereference
getM Value     = value

setM :: Position -> Int -> Memory -> Memory
setM = Map.insert

data VirtualMachine =
  VirtualMachine
    { memory  :: Memory
    , iptr    :: Position
    , inputs  :: [Int]
    , outputs :: [Int]
    }
  deriving (Show)

initVm :: Memory -> [Int] -> VirtualMachine
initVm mem is =
  VirtualMachine {memory = mem, iptr = 0, inputs = is, outputs = []}

unary ::
     (Int -> VirtualMachine -> Maybe VirtualMachine)
  -> [Mode]
  -> VirtualMachine
  -> Maybe VirtualMachine
unary f ms vm = do
  var <- getM (modeFor 0 ms) (iptr vm + 1) (memory vm)
  newVm <- f var vm
  pure newVm {iptr = iptr newVm + 2}

input :: Int -> VirtualMachine -> Maybe VirtualMachine
input ref vm = do
  input <- headM (inputs vm)
  pure vm {memory = setM ref input (memory vm), inputs = tail (inputs vm)}

output :: Int -> VirtualMachine -> Maybe VirtualMachine
output val vm = Just vm {outputs = val : outputs vm}

jmp :: (Int -> Bool) -> [Mode] -> VirtualMachine -> Maybe VirtualMachine
jmp pred ms vm = do
  val <- getM (modeFor 0 ms) (iptr vm + 1) (memory vm)
  dst <- getM (modeFor 1 ms) (iptr vm + 2) (memory vm)
  pure
    vm
      { iptr =
          if pred val
            then dst
            else iptr vm + 3
      }

evalWrite ::
     (Int -> Int -> Int) -> [Mode] -> VirtualMachine -> Maybe VirtualMachine
evalWrite f ms vm@(VirtualMachine mem i _ _) = do
  lhs <- getM (modeFor 0 ms) (i + 1) mem
  rhs <- getM (modeFor 1 ms) (i + 2) mem
  dst <- value (i + 3) mem
  pure vm {memory = setM dst (f lhs rhs) mem, iptr = i + 4}

execute :: Instruction -> VirtualMachine -> Maybe VirtualMachine
execute (Instruction i ms) = callTable i ms
  where
    callTable :: Operation -> ([Mode] -> VirtualMachine -> Maybe VirtualMachine)
    callTable Done      = \_ vm -> Just vm
    callTable Input     = unary input
    callTable Output    = unary output
    callTable JumpTrue  = jmp (/= 0)
    callTable JumpFalse = jmp (== 0)
    callTable Add       = evalWrite (+)
    callTable Multiply  = evalWrite (*)
    callTable Less      = evalWrite (\x y -> fromEnum (x < y))
    callTable Equals    = evalWrite (\x y -> fromEnum (x == y))

step :: VirtualMachine -> Maybe (Operation, VirtualMachine)
step m@(VirtualMachine mem i is os) = do
  opcode <- value i mem
  ins <- instructionFromNumber opcode
  (op ins, ) <$> execute ins m

runUntil ::
     (Operation -> Bool) -> VirtualMachine -> Maybe (Operation, VirtualMachine)
runUntil pred vm = do
  res@(op, newVm) <- step vm
  if pred op
    then pure res
    else runUntil pred newVm

run :: VirtualMachine -> Maybe VirtualMachine
run vm = snd <$> runUntil isDone vm
  where
    isDone :: Operation -> Bool
    isDone Done = True
    isDone _    = False

-- Parser
programParser :: Parser Memory
programParser = do
  code <- sepBy int (char ',')
  pure (Map.fromList (zip [0 ..] code))
