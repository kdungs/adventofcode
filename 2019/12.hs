import Utils (rightOrError)

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number (int)


data Vec = Vec { getX :: Integer
               , getY :: Integer
               , getZ :: Integer
               } deriving (Eq, Ord)

vzero :: Vec
vzero = Vec 0 0 0

vadd :: Vec -> Vec -> Vec
vadd v1 v2 = Vec { getX = getX v1 + getX v2
                 , getY = getY v1 + getY v2
                 , getZ = getZ v1 + getZ v2
                 }

vabsSum :: Vec -> Integer
vabsSum (Vec x y z) = abs x + abs y + abs z

instance Show Vec where
  show (Vec x y z) = "(" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ ")"


data Body = Body { getQ :: Vec
                 , getP :: Vec
                 } deriving (Eq, Ord)

instance Show Body where
  show (Body q p) = "(q=" ++ show q ++ ", p=" ++ show p ++ ")"

energy :: Body -> Integer
energy (Body q p) = vabsSum q * vabsSum p

bodyAtRest :: Vec -> Body
bodyAtRest q = Body q vzero

gravity :: Body -> Body -> Vec
gravity (Body q1 _) (Body q2 _) = Vec { getX = signum (getX q2 - getX q1)
                                      , getY = signum (getY q2 - getY q1)
                                      , getZ = signum (getZ q2 - getZ q1)
                                      }

type Universe = [Body]

totalEnergy :: Universe -> Integer
totalEnergy = sum . map energy

tick :: Universe -> Universe
tick bs = do
  b <- bs
  let fs = [gravity b b_ | b_ <- bs]
  let newVelocity = foldl vadd (getP b) fs
  let newPosition = (getQ b) `vadd` newVelocity
  pure Body { getQ = newPosition
            , getP = newVelocity
            }


ticks :: Int -> Universe -> Universe
ticks 0 u = u
ticks n u = ticks (n - 1) (tick u)


-- Part 2
data PQ = PQ { p :: Integer
             , q :: Integer
             } deriving (Eq, Ord)

instance Show PQ where
  show (PQ v x) = "(" ++ show v ++ ", " ++ show x ++ ")"

gravityPQ :: [PQ] -> PQ -> PQ
gravityPQ pqs pq = pq { p = delta + p pq }
  where delta = toInteger $ numLarger - numSmaller
        numLarger = List.length [x | x <- qs, x > q pq]
        numSmaller = List.length [x | x <- qs, x < q pq]
        qs = q <$> pqs

movePQ :: PQ -> PQ
movePQ pq = pq { q = q pq + p pq }

tickPQ :: [PQ] -> [PQ]
tickPQ pqs = movePQ <$> gravityPQ pqs <$> pqs

ticksPQ :: Int -> [PQ] -> [PQ]
ticksPQ 0 pqs = pqs
ticksPQ n pqs = ticksPQ (n - 1) (tickPQ pqs)


inPQ :: (Vec -> Integer) -> Universe -> [PQ]
inPQ get bs = [ PQ { p = get (getP b)
                   , q = get (getQ b)
                   } | b <- bs
              ]

totalEnergyPQ :: [PQ] -> [PQ] -> [PQ] -> Integer
totalEnergyPQ xs ys zs = sum (zipWith3 e xs ys zs)
  where e x y z = (abs (p x) + abs (p y) + abs (p z))
                * (abs (q x) + abs (q y) + abs (q z))


freqPQ :: [PQ] -> Int -> Set.Set [PQ] -> Int
freqPQ pqs count seen
  | pqs `Set.member` seen = count
  | otherwise = freqPQ (tickPQ pqs) (count + 1) (Set.insert pqs seen)

-- Parsers
vec :: Parser Vec
vec = do
  char '<'
  string "x="
  x <- int
  string ", y="
  y <- int
  string ", z="
  z <- int
  char '>'
  pure (Vec x y z)

vecs :: Parser [Vec]
vecs = sepEndBy vec newline


-- Main
main :: IO ()
main = do
  contents <- getContents
  let ps = rightOrError (parse vecs "" contents)
  let uni = [bodyAtRest p | p <- ps]
  let uni1000 = ticks 1000 uni
  print (totalEnergy uni1000)
  --
  let uniX = inPQ getX uni
  let uniY = inPQ getY uni
  let uniZ = inPQ getZ uni
  print (totalEnergyPQ (ticksPQ 1000 uniX)
                       (ticksPQ 1000 uniY)
                       (ticksPQ 1000 uniZ))
  --
  let fX = freqPQ uniX 0 Set.empty
  let fY = freqPQ uniY 0 Set.empty
  let fZ = freqPQ uniZ 0 Set.empty
  print fX
  print fY
  print fZ
  print (lcm fX (lcm fY fZ))



-- Tests
ex = [ bodyAtRest (Vec (-1) 0 2)
     , bodyAtRest (Vec 2 (-10) (-7))
     , bodyAtRest (Vec 4 (-8) 8)
     , bodyAtRest (Vec 3 5 (-1))
     ]

exAfterTicks :: Int -> Universe
exAfterTicks n = ticks n ex


