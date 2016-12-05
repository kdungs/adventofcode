import Text.ParserCombinators.Parsec

data Key = Key1 | Key2 | Key3 | Key4 | Key5 | Key6 | Key7 | Key8 | Key9

instance Show Key where
  show Key1 = "1"
  show Key2 = "2"
  show Key3 = "3"
  show Key4 = "4"
  show Key5 = "5"
  show Key6 = "6"
  show Key7 = "7"
  show Key8 = "8"
  show Key9 = "9"

data Dir = U | R | D | L deriving (Show)

move :: Key -> Dir -> Key
move k d = case k of
  Key1 -> case d of
    U -> Key1
    R -> Key2
    D -> Key4
    L -> Key1
  Key2 -> case d of
    U -> Key2
    R -> Key3
    D -> Key5
    L -> Key1
  Key3 -> case d of
    U -> Key3
    R -> Key3
    D -> Key6
    L -> Key2
  Key4 -> case d of
    U -> Key1
    R -> Key5
    D -> Key7
    L -> Key4
  Key5 -> case d of
    U -> Key2
    R -> Key6
    D -> Key8
    L -> Key4
  Key6 -> case d of
    U -> Key3
    R -> Key6
    D -> Key9
    L -> Key5
  Key7 -> case d of
    U -> Key4
    R -> Key8
    D -> Key7
    L -> Key7
  Key8 -> case d of
    U -> Key5
    R -> Key9
    D -> Key8
    L -> Key7
  Key9 -> case d of
    U -> Key6
    R -> Key9
    D -> Key9
    L -> Key8

applyCommand :: Key -> [Dir] -> Key
applyCommand k ds = foldl move k ds

applyCommands :: Key -> [[Dir]] -> [Key]
applyCommands k0 dss = tail $ scanl applyCommand k0 dss

-- Parser
parseDir :: Parser Dir
parseDir = convertDir <$> oneOf "URDL"
             where convertDir c = case (c) of
                                    'U' -> U
                                    'R' -> R
                                    'D' -> D
                                    'L' -> L

parseCommand :: Parser [Dir]
parseCommand = do
                 result <- many1 parseDir
                 many newline
                 return result

parseFile :: Parser [[Dir]]
parseFile = do
              result <- many parseCommand
              eof
              return result

-- Nicer formatting.
newtype Keys = Keys [Key]

instance Show Keys where
  show (Keys ks) = concat $ map show ks

asKeys :: [Key] -> Keys
asKeys ks = Keys ks

-- Main.
main = do
         input <- getContents 
         let program = parse parseFile "" input
         let result = asKeys <$> applyCommands Key5 <$> program
         putStrLn . show $ result
