import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number (int)
import qualified Data.List as List
import qualified Data.Map.Strict as Map

newtype Register = Register Char deriving Show
newtype Value = Value Int deriving Show

addV :: Value -> Int -> Value
addV (Value v) x = Value (v + x)

data ValueOrRegister = V Value | R Register deriving Show

data Instruction = Cpy ValueOrRegister Register
                 | Inc Register
                 | Dec Register
                 | Jnz ValueOrRegister Value
                 deriving Show

data Registers = Registers Value Value Value Value deriving Show

defaultR :: Registers
defaultR = Registers (Value 0) (Value 0) (Value 0) (Value 0)

getR :: Registers -> Register -> Value
getR (Registers a _ _ _) (Register 'a') = a
getR (Registers _ b _ _) (Register 'b') = b
getR (Registers _ _ c _) (Register 'c') = c
getR (Registers _ _ _ d) (Register 'd') = d

setR :: Registers -> Register -> Value -> Registers
setR (Registers _ b c d) (Register 'a') a = Registers a b c d
setR (Registers a _ c d) (Register 'b') b = Registers a b c d
setR (Registers a b _ d) (Register 'c') c = Registers a b c d
setR (Registers a b c _) (Register 'd') d = Registers a b c d

incR :: Registers -> Register -> Registers
incR rs r = setR rs r (addV (getR rs r) 1)

decR :: Registers -> Register -> Registers
decR rs r = setR rs r (addV (getR rs r) (-1))

getValueOrRegister :: Registers -> ValueOrRegister -> Value
getValueOrRegister _ (V val) = val
getValueOrRegister regs (R reg) = getR regs reg

data Context = Context { program :: [Instruction]
                       , registers :: Registers
                       , iptr :: Int
                       } deriving Show

defaultContext :: [Instruction] -> Context
defaultContext is = Context is defaultR 0

execute :: Context -> Context
execute c@(Context is _ iptr) = if iptr < (List.length is) then execute (execute_ c (is !! iptr)) else c

execute_ :: Context -> Instruction -> Context
execute_ (Context is regs iptr) (Cpy vor reg) = Context is (setR regs reg (getValueOrRegister regs vor)) (iptr + 1)
execute_ (Context is regs iptr) (Inc reg) = Context is (incR regs reg) (iptr + 1)
execute_ (Context is regs iptr) (Dec reg) = Context is (decR regs reg) (iptr + 1)
execute_ (Context is regs iptr) (Jnz vor (Value offset)) = Context is regs (case getValueOrRegister regs vor of
                                                                             Value 0 -> iptr + 1
                                                                             _ -> iptr + offset)


-- Parser
register :: Parser Register
register = Register <$> oneOf "abcd"

value :: Parser Value
value = Value <$> int

valueOrRegister :: Parser ValueOrRegister
valueOrRegister = V <$> value <|> R <$> register

cpy :: Parser Instruction
cpy = do
  string "cpy "
  vor <- valueOrRegister
  char ' '
  reg <- register
  pure (Cpy vor reg)

inc :: Parser Instruction
inc = Inc <$> (string "inc " >> register)

dec :: Parser Instruction
dec = Dec <$> (string "dec " >> register)

jnz :: Parser Instruction
jnz = do
  string "jnz "
  vor <- valueOrRegister
  char ' '
  val <- value
  pure (Jnz vor val)

instruction :: Parser Instruction
instruction = choice [cpy, inc, dec, jnz]

instructions :: Parser [Instruction]
instructions = sepEndBy instruction newline

-- Main
main :: IO ()
main = do
  contents <- getContents
  let is = parse instructions "" contents
  print $ execute . defaultContext<$> is
  let regs = setR defaultR (Register 'c') (Value 1)
  print $ execute . (\is -> Context is regs 0) <$> is
