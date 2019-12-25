import           Intcode

import           Data.Char                     (chr, ord)
import qualified Data.List                     as List
import           Text.ParserCombinators.Parsec (parse)

runIO :: VirtualMachine -> IO ()
runIO vm = do
  let (Just (waiting, nvm)) = runUntilInputIsRequired vm
  putStrLn (toAsciiString (outputs nvm))
  input <- getLine
  if waiting
    then runIO nvm {inputs = fromAsciiString (input ++ "\n"), outputs = []}
    else pure ()

toAsciiString :: [Integer] -> String
toAsciiString xs = chr . fromIntegral <$> List.reverse xs

fromAsciiString :: String -> [Integer]
fromAsciiString s = toInteger . ord <$> s

data Command
  = N
  | E
  | S
  | W
  | T String
  | D String

instance Show Command where
  show N     = "north"
  show E     = "east"
  show S     = "south"
  show W     = "west"
  show (T s) = "take " ++ s
  show (D s) = "drop " ++ s

runCommand :: Command -> VirtualMachine -> VirtualMachine
runCommand c vm = nextVm
  where
    (Just (_, nextVm)) =
      runUntilInputIsRequired vm {inputs = fromAsciiString (show c ++ "\n")}

runCommands :: [Command] -> VirtualMachine -> VirtualMachine
runCommands cs vm = foldl (flip runCommand) vm cs

initial :: [Command]
initial =
  [ E
  , T "antenna"
  , E
  , T "ornament"
  , N
  , W
  , T "fixed point"
  , E
  , S
  , W
  , N
  , N
  , T "asterisk"
  , S
  , W
  , S
  , T "hologram"
  , N
  , W
  , T "astronaut ice cream"
  , E
  , E
  , S
  , W
  , S
  , S
  , S
  , T "dark matter"
  , N
  , W
  , N
  , T "monolith"
  , N
  , N
  , D "antenna"
  , D "ornament"
  , D "fixed point"
  , D "asterisk"
  , D "hologram"
  , D "astronaut ice cream"
  , D "dark matter"
  , D "monolith"
  ]

items :: [Command] -> [String]
items []         = []
items ((T s):cs) = s : items cs
items (_:cs)     = items cs

main :: IO ()
main = do
  contents <- readFile "inputs/25"
  let (Right prog) = parse programParser "" contents
  let vm = runCommands initial (initVm prog [])
  runIO vm
