import Text.ParserCombinators.Parsec
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

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


pathToCom :: OrbitMap -> Body -> [Body]
pathToCom _ (Body "COM") = []
pathToCom os b = t:(pathToCom os t)
  where t = case Map.lookup b os of
              Just v -> v
              Nothing -> error ("Could not lookup body " ++ show b)

symmetricDifference :: Ord a => Set.Set a -> Set.Set a -> Set.Set a
symmetricDifference lhs rhs = (Set.union lhs rhs) `Set.difference` (Set.intersection lhs rhs)

pathLength :: OrbitMap -> Body -> Body -> Int
pathLength os b1 b2 = Set.size (symmetricDifference lset rset)
  where lpath = pathToCom os b1
        lset = Set.fromList lpath
        rpath = pathToCom os b2
        rset = Set.fromList rpath

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
  putStrLn . show $ (\m -> pathLength m (Body "YOU") (Body "SAN")) <$> os
