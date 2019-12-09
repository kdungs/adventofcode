{-# LANGUAGE TupleSections #-}

module Intcode
  ( module Intcode
  ) where

import           Utils                                (atWithDefault, headM,
                                                       rdigits)

import           Control.Monad                        (mapM)
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
  = PositionMode
  | ImmediateMode
  | RelativeMode
  deriving (Show)

modeFromNumber :: Integer -> Maybe Mode
modeFromNumber 0 = Just PositionMode
modeFromNumber 1 = Just ImmediateMode
modeFromNumber 2 = Just RelativeMode
modeFromNumber _ = Nothing

modesFromNumber :: Integer -> Maybe [Mode]
modesFromNumber x = mapM modeFromNumber (rdigits x)

data Instruction =
  Instruction Operation [Mode]
  deriving (Show)

instructionFromNumber :: Integer -> Maybe Instruction
instructionFromNumber x = do
  op <- operationFromNumber (x `mod` 100)
  ms <- modesFromNumber (x `div` 100)
  pure (Instruction op ms)

modeFor :: Int -> [Mode] -> Mode
modeFor = atWithDefault PositionMode

type Position = Integer

type Memory = Map.Map Position Integer

getM :: Position -> Memory -> Maybe Integer
getM pos mem
  | pos < 0 = Nothing
  | otherwise = Just (Map.findWithDefault 0 pos mem)

setM :: Position -> Integer -> Memory -> Memory
setM = Map.insert

data Variable
  = Value Integer
  | Reference Position

variable :: Mode -> Position -> Position -> Memory -> Maybe Variable
variable mode base pos mem = ctor mode base <$> getM pos mem
  where
    ctor PositionMode _  = Reference
    ctor ImmediateMode _ = Value
    ctor RelativeMode b  = Reference . (b +)

valueOf :: Variable -> Memory -> Maybe Integer
valueOf (Value x) _         = Just x
valueOf (Reference pos) mem = getM pos mem

addressOf :: Variable -> Maybe Position
addressOf (Value _)       = Nothing
addressOf (Reference pos) = Just pos

data VirtualMachine =
  VirtualMachine
    { memory  :: Memory
    , iptr    :: Position
    , rbase   :: Position
    , inputs  :: [Integer]
    , outputs :: [Integer]
    }
  deriving (Show)

getVar :: Mode -> Position -> VirtualMachine -> Maybe Variable
getVar mode pos vm = variable mode (rbase vm) pos (memory vm)

getValue :: Mode -> Position -> VirtualMachine -> Maybe Integer
getValue mode pos vm = do
  var <- getVar mode pos vm
  valueOf var (memory vm)

setVar :: Variable -> Integer -> VirtualMachine -> Maybe VirtualMachine
setVar (Value _) _ _          = Nothing
setVar (Reference pos) val vm = Just vm {memory = setM pos val (memory vm)}

initVm :: Memory -> [Integer] -> VirtualMachine
initVm mem is =
  VirtualMachine {memory = mem, iptr = 0, rbase = 0, inputs = is, outputs = []}

unary ::
     (Variable -> VirtualMachine -> Maybe VirtualMachine)
  -> [Mode]
  -> VirtualMachine
  -> Maybe VirtualMachine
unary f ms vm = do
  var <- getVar (modeFor 0 ms) (iptr vm + 1) vm
  newVm <- f var vm
  pure newVm {iptr = iptr newVm + 2}

input :: Variable -> VirtualMachine -> Maybe VirtualMachine
input var vm = do
  val <- headM (inputs vm)
  newVm <- setVar var val vm
  pure newVm {inputs = tail (inputs vm)}

output :: Variable -> VirtualMachine -> Maybe VirtualMachine
output var vm = do
  val <- valueOf var (memory vm)
  pure vm {outputs = val : outputs vm}

adjRBase :: Variable -> VirtualMachine -> Maybe VirtualMachine
adjRBase var vm = do
  val <- valueOf var (memory vm)
  pure vm {rbase = rbase vm + val}

binary ::
     (Variable -> Variable -> VirtualMachine -> Maybe VirtualMachine)
  -> [Mode]
  -> VirtualMachine
  -> Maybe VirtualMachine
binary f ms vm = do
  lvar <- getVar (modeFor 0 ms) (iptr vm + 1) vm
  rvar <- getVar (modeFor 1 ms) (iptr vm + 2) vm
  newVm <- f lvar rvar vm
  pure newVm {iptr = iptr newVm + 3}

jmp ::
     (Integer -> Bool)
  -> Variable
  -> Variable
  -> VirtualMachine
  -> Maybe VirtualMachine
jmp predicate lvar rvar vm = do
  val <- valueOf lvar (memory vm)
  dst <- valueOf rvar (memory vm)
  let newIptr =
        if predicate val
          then dst - 3
          else iptr vm
  pure vm {iptr = newIptr}

ternary ::
     (Variable -> Variable -> Variable -> VirtualMachine -> Maybe VirtualMachine)
  -> [Mode]
  -> VirtualMachine
  -> Maybe VirtualMachine
ternary f ms vm = do
  var1 <- getVar (modeFor 0 ms) (iptr vm + 1) vm
  var2 <- getVar (modeFor 1 ms) (iptr vm + 2) vm
  var3 <- getVar (modeFor 2 ms) (iptr vm + 3) vm
  newVm <- f var1 var2 var3 vm
  pure newVm {iptr = iptr vm + 4}

evalWrite ::
     (Integer -> Integer -> Integer)
  -> Variable
  -> Variable
  -> Variable
  -> VirtualMachine
  -> Maybe VirtualMachine
evalWrite f var1 var2 var3 vm = do
  lhs <- valueOf var1 (memory vm)
  rhs <- valueOf var2 (memory vm)
  setVar var3 (f lhs rhs) vm

execute :: Instruction -> VirtualMachine -> Maybe VirtualMachine
execute (Instruction i ms) = callTable i ms
  where
    callTable :: Operation -> ([Mode] -> VirtualMachine -> Maybe VirtualMachine)
    callTable Done = \_ vm -> Just vm
    callTable Input = unary input
    callTable Output = unary output
    callTable AdjustRelativeBase = unary adjRBase
    callTable JumpTrue = binary (jmp (/= 0))
    callTable JumpFalse = binary (jmp (== 0))
    callTable Add = ternary (evalWrite (+))
    callTable Multiply = ternary (evalWrite (*))
    callTable Less =
      ternary
        (evalWrite
           (\x y ->
              if x < y
                then 1
                else 0))
    callTable Equals =
      ternary
        (evalWrite
           (\x y ->
              if x == y
                then 1
                else 0))

step :: VirtualMachine -> Maybe (Operation, VirtualMachine)
step vm = do
  opcode <- getM (iptr vm) (memory vm)
  ins@(Instruction op _) <- instructionFromNumber opcode
  (op, ) <$> execute ins vm

runUntil ::
     (Operation -> Bool) -> VirtualMachine -> Maybe (Operation, VirtualMachine)
runUntil predicate vm = do
  res@(op, newVm) <- step vm
  if predicate op
    then pure res
    else runUntil predicate newVm

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
