import Text.ParserCombinators.Parsec
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


data Identifier = Value Int
                | Bot Int
                | Output Int
                deriving (Eq, Ord, Show)

data ValueConnector = ValueConnector Identifier deriving Show
data BotConnector = BotConnector { lo :: Identifier, hi :: Identifier } deriving Show

data Node = ValueNode Int ValueConnector
          | EmptyBotNode BotConnector
          | PartialBotNode Int BotConnector
          | BotNode Int Int BotConnector
          | EmptyOutputNode
          | OutputNode Int
          deriving (Show)

type Graph = Map.Map Identifier Node

allValueNodes :: Graph -> [Identifier]
allValueNodes g = List.filter isValue (Map.keys g)
  where isValue (Value _) = True
        isValue _ = False

updateNode :: Int -> Node -> Maybe Node
updateNode in1 (EmptyBotNode bc) = Just (PartialBotNode in1 bc)
updateNode in2 (PartialBotNode in1 bc) = Just (BotNode in1 in2 bc)
updateNode val EmptyOutputNode = Just (OutputNode val)
updateNode _ _ = Nothing

isBotNode :: Node -> Bool
isBotNode (BotNode _ _ _) = True
isBotNode _ = False

performUpdate :: Node -> Graph -> Maybe (Graph, [Identifier])
performUpdate (ValueNode val (ValueConnector ref)) g = do
  node <- Map.lookup ref g
  updated <- updateNode val node
  pure (Map.insert ref updated g, if isBotNode updated then [ref] else [])
performUpdate (BotNode lhs rhs (BotConnector lo hi)) g = do
  loNode <- Map.lookup lo g
  updatedLo <- updateNode (min lhs rhs) loNode
  let ug = Map.insert lo updatedLo g
  hiNode <- Map.lookup hi ug
  updatedHi <- updateNode (max lhs rhs) hiNode
  pure (Map.insert hi updatedHi ug,
        if isBotNode updatedLo then [lo] else [] ++
        if isBotNode updatedHi then [hi] else [])
performUpdate (OutputNode _) g = Just (g, [])
performUpdate _ _ = Nothing

calculate :: [Identifier] -> Graph -> Maybe Graph
calculate [] g = Just g
calculate (x:xs) g = do
  node <- Map.lookup x g
  (ng, ids) <- performUpdate node g
  calculate (xs ++ ids) ng

resolve :: Graph -> Maybe Graph
resolve g = calculate (allValueNodes g) g

findBotNodeWithInputs :: Int -> Int -> Graph -> Graph
findBotNodeWithInputs x y g = Map.filter f g
  where f (BotNode in1 in2 _) = List.sort [x, y] == List.sort [in1, in2]
        f _ = False

-- Parsers
int :: Parser Int
int = read <$> many1 digit

parserForIdentifier :: (Int -> Identifier) -> String -> Parser Identifier
parserForIdentifier cons label = do
  string label
  char ' '
  num <- int
  pure (cons num)

value :: Parser Identifier
value = parserForIdentifier Value "value"

bot :: Parser Identifier
bot = parserForIdentifier Bot "bot"

output :: Parser Identifier
output = parserForIdentifier Output "output"

identifier:: Parser Identifier
identifier = value <|> bot <|> output

sink :: Parser Identifier
sink = bot <|> output

withOutput :: Identifier -> Graph -> Graph
withOutput o@(Output _) g = Map.insert o EmptyOutputNode g
withOutput _ g = g

valueNode :: Parser Graph
valueNode = do
  v <- value
  string " goes to "
  s <- sink
  let (Value x) = v
  pure (withOutput s (Map.singleton v (ValueNode x (ValueConnector s))))

botNode :: Parser Graph
botNode = do
  b <- bot
  string " gives low to "
  lo <- sink
  string " and high to "
  hi <- sink
  pure (withOutput lo
         (withOutput hi
           (Map.singleton b (EmptyBotNode (BotConnector { lo = lo, hi = hi })))))

graph :: Parser Graph
graph = Map.unions <$> sepEndBy (valueNode <|> botNode) newline

-- Main
main :: IO ()
main = do
  contents <- getContents
  let g = parse graph "" contents
  putStrLn . show $ allValueNodes <$> g
  putStrLn . show $ (\x -> findBotNodeWithInputs 17 61 <$> (resolve x)) <$> g
