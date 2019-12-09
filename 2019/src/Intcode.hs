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
  | AdjustRelativeBase
  deriving (Show)

operationFromNumber :: Integer -> Maybe Operation
operationFromNumber x = Map.lookup x opMap
  where
    opMap :: Map.Map Integer Operation
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
        , (9, AdjustRelativeBase)
        , (99, Done)
        ]

data Mode
  = Reference
  | Value
  | Relative
  deriving (Show)

modeFromNumber :: Integer -> Maybe Mode
modeFromNumber 0 = Just Reference
modeFromNumber 1 = Just Value
modeFromNumber 2 = Just Relative
modeFromNumber _ = Nothing

modesFromNumber :: Integer -> Maybe [Mode]
modesFromNumber x = mapM modeFromNumber (rdigits x)
  where
    rdigits :: Integer -> [Integer]
    rdigits 0 = []
    rdigits x = x `mod` 10 : rdigits (x `div` 10)

data Instruction =
  Instruction
    { op    :: Operation
    , modes :: [Mode]
    }
  deriving (Show)

instructionFromNumber :: Integer -> Maybe Instruction
instructionFromNumber x = do
  op <- operationFromNumber (x `mod` 100)
  ms <- modesFromNumber (x `div` 100)
  pure (Instruction op ms)

modeFor :: Int -> [Mode] -> Mode
modeFor = atWithDefault Reference

type Position = Integer

type Memory = Map.Map Position Integer

value :: Position -> Memory -> Maybe Integer
value pos mem
  | pos < 0 = Nothing
  | otherwise = Just (Map.findWithDefault 0 pos mem)

dereference :: Position -> Memory -> Maybe Integer
dereference pos mem = do
  ptr <- value pos mem
  value ptr mem

relative :: Position -> Position -> Memory -> Maybe Integer
relative base pos mem = do
  rptr <- value pos mem
  value (base + rptr) mem

getM :: Mode -> Position -> Position -> Memory -> Maybe Integer
getM Reference _ = dereference
getM Value     _ = value
getM Relative  base = relative base

setM :: Position -> Integer -> Memory -> Memory
setM = Map.insert

data VirtualMachine =
  VirtualMachine
    { memory  :: Memory
    , iptr    :: Position
    , rbase :: Position
    , inputs  :: [Integer]
    , outputs :: [Integer]
    }
  deriving (Show)

getVM :: Mode -> Position -> VirtualMachine -> Maybe Integer
getVM mode pos vm = getM mode (rbase vm) pos (memory vm)

initVm :: Memory -> [Integer] -> VirtualMachine
initVm mem is =
  VirtualMachine {memory = mem, iptr = 0, rbase = 0, inputs = is, outputs = []}

unary ::
     (Integer -> VirtualMachine -> Maybe VirtualMachine)
  -> [Mode]
  -> VirtualMachine
  -> Maybe VirtualMachine
unary f ms vm = do
  var <- getVM (modeFor 0 ms) (iptr vm + 1) vm
  newVm <- f var vm
  pure newVm {iptr = iptr newVm + 2}

input :: [Mode] -> VirtualMachine -> Maybe VirtualMachine
input ms vm = do
  ref <- value (iptr vm + 1) (memory vm)
  input <- headM (inputs vm)
  pure vm {memory = setM ref input (memory vm), iptr = iptr vm + 2, inputs = tail (inputs vm)}

output :: Integer -> VirtualMachine -> Maybe VirtualMachine
output val vm = Just vm {outputs = val : outputs vm}

adjRBase :: Integer -> VirtualMachine -> Maybe VirtualMachine
adjRBase val vm = Just vm { rbase = rbase vm + val }

jmp :: (Integer -> Bool) -> [Mode] -> VirtualMachine -> Maybe VirtualMachine
jmp pred ms vm = do
  val <- getVM (modeFor 0 ms) (iptr vm + 1) vm
  dst <- getVM (modeFor 1 ms) (iptr vm + 2) vm
  pure
    vm
      { iptr =
          if pred val
            then dst
            else iptr vm + 3
      }

evalWrite ::
     (Integer -> Integer -> Integer) -> [Mode] -> VirtualMachine -> Maybe VirtualMachine
evalWrite f ms vm@(VirtualMachine mem i _ _ _) = do
  lhs <- getVM (modeFor 0 ms) (i + 1) vm
  rhs <- getVM (modeFor 1 ms) (i + 2) vm
  dst <- value (i + 3) mem
  pure vm {memory = setM dst (f lhs rhs) mem, iptr = i + 4}

execute :: Instruction -> VirtualMachine -> Maybe VirtualMachine
execute (Instruction i ms) = callTable i ms
  where
    callTable :: Operation -> ([Mode] -> VirtualMachine -> Maybe VirtualMachine)
    callTable Done      = \_ vm -> Just vm
    callTable Input     = input
    callTable Output    = unary output
    callTable AdjustRelativeBase = unary adjRBase
    callTable JumpTrue  = jmp (/= 0)
    callTable JumpFalse = jmp (== 0)
    callTable Add       = evalWrite (+)
    callTable Multiply  = evalWrite (*)
    callTable Less      = evalWrite (\x y -> if (x < y) then 1 else 0)
    callTable Equals    = evalWrite (\x y -> if (x == y) then 1 else 0)

step :: VirtualMachine -> Maybe (Operation, VirtualMachine)
step vm = do
  opcode <- value (iptr vm) (memory vm)
  ins <- instructionFromNumber opcode
  (op ins, ) <$> execute ins vm

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
