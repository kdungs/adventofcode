import Text.ParserCombinators.Parsec
import qualified Data.Map.Strict as Map

data Body = Body String deriving (Eq, Ord, Show)
type OrbitMap = Map.Map Body Body
type CountMap = Map.Map Body Int

counts :: OrbitMap -> Maybe CountMap
counts os = foldl (\om b -> om >>= countOrbits os b) (Just Map.empty) (Map.keys os)

countOrbits :: OrbitMap -> Body -> CountMap -> Maybe CountMap
countOrbits _ (Body "COM") cs = Just (Map.insert (Body "COM") 0 cs)
countOrbits os b cs = do
  ob <- Map.lookup b os
  newCs <- countOrbits os ob cs
  cb <- Map.lookup ob newCs
  pure (Map.insert b (cb + 1) newCs)

bfsSteps :: OrbitMap -> Body -> Body -> Maybe Int
bfsSteps os a b
  | a == b = Just 0
  | otherwise = do
    

-- Parsers
body :: Parser Body
body = Body <$> many1 (letter <|> digit)

orbit :: Parser (Body, Body)
orbit = do
    lhs <- body
    char ')'
    rhs <- body
    pure (rhs, lhs)

orbits :: Parser OrbitMap
orbits = Map.fromList <$> sepEndBy orbit newline

-- Main
main :: IO ()
main = do
  contents <- getContents
  let os = parse orbits "" contents
  let cm = counts <$> os
  let res = (\x -> sum . Map.elems <$> x) <$> cm
  putStrLn . show $ res
