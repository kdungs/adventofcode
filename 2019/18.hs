import qualified Data.Char       as Char
import qualified Data.List       as List
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

type Maze = Map.Map (Int, Int) Char

parseMaze :: String -> Maze
parseMaze input =
  Map.fromList $ do
    (row, y) <- zip (List.lines input) [0 ..]
    (c, x) <- zip row [0 ..]
    pure ((x, y), c)

isDoor :: Char -> Bool
isDoor = Char.isUpper

isKey :: Char -> Bool
isKey = Char.isLower

allKeys :: Maze -> Maze
allKeys = Map.filter isKey

data Edge =
  Edge
    { getDist  :: Integer
    , getDoors :: Set.Set Char
    }
  deriving (Show)

type Adj = Map.Map Char Edge

data MazeBfs =
  MazeBfs
    { getMaze     :: Maze
    , getFrontier :: [(Int, Int)]
    , getStep     :: Integer
    , getSeen     :: Set.Set (Int, Int)
    , getAdj      :: Adj
    }
  deriving (Show)

initMazeBfs :: Maze -> (Int, Int) -> MazeBfs
initMazeBfs maze start =
  MazeBfs
    { getMaze = maze
    , getFrontier = [start]
    , getStep = 0
    , getSeen = Set.singleton start
    , getAdj = Map.empty
    }

ns :: (Int, Int) -> [(Int, Int)]
ns (x, y) = [(x - 1, y), (x, y - 1), (x, y + 1), (x + 1, y)]

runMazeBfs :: MazeBfs -> MazeBfs
runMazeBfs bfs
  | List.null (getFrontier bfs) = bfs
  | otherwise = do
    let ms = getFrontier bfs >>= ns
    let us = List.filter (\p -> not (p `Set.member` getSeen bfs)) ms
    let rs =
          List.filter
            (\p ->
               case getMaze bfs Map.!? p of
                 Nothing -> False
                 Just c  -> c /= '#')
            us
    let nextStep = 1 + getStep bfs
    let nextSeen = getSeen bfs `Set.union` Set.fromList rs
    let (paths, vertices) = List.partition (\p -> getMaze bfs Map.! p /= '.') rs
    let nextFrontier = vertices
    let updateAdj =
          Map.fromList $ do
            p <- paths
            let c = getMaze bfs Map.! p
            pure (c, Edge nextStep Set.empty)
    runMazeBfs
      bfs
        { getFrontier = nextFrontier
        , getStep = nextStep
        , getSeen = nextSeen
        , getAdj = getAdj bfs `Map.union` updateAdj
        }

adj :: Maze -> (Int, Int) -> Adj
adj maze start = getAdj (runMazeBfs (initMazeBfs maze start))

type Graph = Map.Map Char Adj

isNode :: Char -> Bool
isNode '@' = True
isNode '#' = False
isNode '.' = False
isNode _   = True

graph :: Maze -> Graph
graph maze =
  Map.fromList $ do
    (p, c) <- Map.assocs (Map.filter isNode maze)
    pure (c, adj maze p)

combineWithDoor :: Char -> Edge -> Edge -> Edge
combineWithDoor d le re =
  Edge
    { getDist = getDist le + getDist re
    , getDoors =
        Set.insert (Char.toLower d) (getDoors le) `Set.union` getDoors re
    }

removeEdgesWithDoor :: Char -> Graph -> Graph
removeEdgesWithDoor d g =
  Map.fromList $ do
    (v, a) <- Map.assocs g
    let aWithout = Map.delete d a
    pure (v, aWithout)

removeDoor :: Char -> Graph -> Graph
removeDoor d g =
  removeEdgesWithDoor d (Map.delete d (Map.unionWith Map.union g updates))
  where
    updates =
      Map.fromList $ do
        let a = g Map.! d
        (lv, le) <- Map.assocs a
        pure
          ( lv
          , Map.fromList $ do
              (rv, re) <- Map.assocs a
              if lv == rv
                then []
                else pure (rv, combineWithDoor d le re))

allDoors :: Graph -> String
allDoors g = List.filter Char.isUpper (Map.keys g)

removeDoors :: Graph -> Graph
removeDoors g = List.foldr removeDoor g (allDoors g)

data Step =
  Step
    { getV  :: Char
    , getKs :: Set.Set Char
    , getD  :: Integer
    }
  deriving (Eq, Ord, Show)

data Dfs =
  Dfs
    { getGraph   :: Graph
    , getAllKeys :: Set.Set Char
    , getStack   :: [Step]
    , getDists   :: Map.Map (Char, Set.Set Char) Integer
    , getBest    :: Integer
    }
  deriving (Show)

initDfs :: Graph -> Dfs
initDfs g =
  Dfs
    { getGraph = g
    , getAllKeys = Set.fromList (Map.keys g)
    , getStack = [Step {getV = '@', getKs = Set.empty, getD = 0}]
    , getDists = Map.singleton ('@', Set.empty) 0
    , getBest = 99999999999999999999
    }

sortByEdgeDist :: [Step] -> [Step]
sortByEdgeDist = List.sortOn getD

candidates ::
     Graph -> Map.Map (Char, Set.Set Char) Integer -> Integer -> Step -> [Step]
candidates g seen best s =
  sortByEdgeDist $ do
    let v1 = getV s
    let ks = Set.insert v1 (getKs s)
    let d1 = seen Map.! (v1, getKs s)
    (v2, e) <- Map.assocs (g Map.! v1)
    let d2 = d1 + getDist e
    let nextStep = Step {getV = v2, getKs = ks, getD = d2}
    if not (getDoors e `Set.isSubsetOf` ks)
      then []
      else case seen Map.!? (v2, ks) of
             Nothing ->
               if d2 < best
                 then pure nextStep
                 else []
             Just d ->
               if d <= d2
                 then []
                 else pure nextStep

bestOrBetter :: Set.Set Char -> Integer -> Step -> Integer
bestOrBetter allKeys best (Step v ks d) =
  if allKeys `Set.isSubsetOf` Set.insert v ks && d < best
    then d
    else best

stepDfs :: Dfs -> Dfs
stepDfs dfs =
  case candidates
         (getGraph dfs)
         (getDists dfs)
         (getBest dfs)
         (head (getStack dfs)) of
    [] -> dfs {getStack = tail (getStack dfs)}
    (s:_) ->
      dfs
        { getStack = s : getStack dfs
        , getDists = Map.insert (getV s, getKs s) (getD s) (getDists dfs)
        , getBest = bestOrBetter (getAllKeys dfs) (getBest dfs) s
        }

runDfs :: Dfs -> Dfs
runDfs dfs
  | List.null (getStack dfs) = dfs
  | otherwise = runDfs (stepDfs dfs)

main :: IO ()
main = do
  contents <- getContents
  let maze = parseMaze contents
  let g = graph maze
  let ng = removeDoors g
  let dfs = initDfs ng
  print $ runDfs dfs
