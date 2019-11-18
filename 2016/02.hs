import Text.ParserCombinators.Parsec

data Dir = U | R | D | L deriving (Show)

class Keypad key where
  move :: key -> Dir -> key

applyCommand :: Keypad key => key -> [Dir] -> key 
applyCommand k ds = foldl move k ds

applyCommands :: Keypad key => key -> [[Dir]] -> [key]
applyCommands k0 dss = tail $ scanl applyCommand k0 dss

-- Part 1.
data Key = Key1 | Key2 | Key3 | Key4 | Key5 | Key6 | Key7 | Key8 | Key9

instance Show Key where
  show k = case k of
    Key1 -> "1"
    Key2 -> "2"
    Key3 -> "3"
    Key4 -> "4"
    Key5 -> "5"
    Key6 -> "6"
    Key7 -> "7"
    Key8 -> "8"
    Key9 -> "9"

instance Keypad Key where
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

-- Part 2.
data Key' = K1 | K2 | K3 | K4 | K5 | K6 | K7 | K8 | K9 | KA | KB | KC | KD

instance Show Key' where
  show k = case k of
    K1 -> "1"
    K2 -> "2"
    K3 -> "3"
    K4 -> "4"
    K5 -> "5"
    K6 -> "6"
    K7 -> "7"
    K8 -> "8"
    K9 -> "9"
    KA -> "A"
    KB -> "B"
    KC -> "C"
    KD -> "D"

instance Keypad Key' where
  move k d = case k of
    K1 -> case d of 
      U -> K1
      R -> K1
      D -> K3
      L -> K1
    K2 -> case d of 
      U -> K2
      R -> K3
      D -> K6
      L -> K2
    K3 -> case d of 
      U -> K1
      R -> K4
      D -> K7
      L -> K2
    K4 -> case d of 
      U -> K4
      R -> K4
      D -> K8
      L -> K3
    K5 -> case d of 
      U -> K5
      R -> K6
      D -> K5
      L -> K5
    K6 -> case d of 
      U -> K2
      R -> K7
      D -> KA
      L -> K5
    K7 -> case d of 
      U -> K3
      R -> K8
      D -> KB
      L -> K6
    K8 -> case d of 
      U -> K4
      R -> K9
      D -> KC
      L -> K7
    K9 -> case d of 
      U -> K9
      R -> K9
      D -> K9
      L -> K8
    KA -> case d of 
      U -> K6
      R -> KB
      D -> KA
      L -> KA
    KB -> case d of 
      U -> K7
      R -> KC
      D -> KD
      L -> KA
    KC -> case d of 
      U -> K8
      R -> KC
      D -> KC
      L -> KB
    KD -> case d of 
      U -> KB
      R -> KD
      D -> KD
      L -> KD

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
showKeys :: (Keypad key, Show key) => [key] -> String
showKeys ks = concat $ map show ks

-- Main.
main = do
         input <- getContents 
         let program = parse parseFile "" input
         let result1 = showKeys <$> applyCommands Key5 <$> program
         putStrLn . show $ result1
         let result2 = showKeys <$> applyCommands K5 <$> program
         putStrLn . show $ result2
