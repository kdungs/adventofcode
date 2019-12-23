import           Data.Functor                         (($>), (<&>))
import qualified Data.List                            as List
import qualified Data.List.NonEmpty                   as NonEmpty
import qualified Data.Map.Strict                      as Map
import           Data.Semigroup
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Number

data Shuffle
  = Stack
  | Cut Integer
  | Deal Integer
  deriving (Show)

data Op =
  Op
    { getModulus :: Integer
    , getA       :: Integer
    , getB       :: Integer
    }
  deriving (Show)

instance Semigroup Op where
  (Op n x y) <> (Op n2 a b)
    | n == n2 = Op {getModulus = n, getA = (a * x) `mod` n, getB = (a * y + b) `mod` n}
    | otherwise = error "deck sizes dont match"

powMod :: Integer -> Integer -> Integer -> Integer
powMod base 1 m = base `mod` m
powMod base pow m =
  if even pow
     then ((powMod base (pow `div` 2) m) ^ 2) `mod` m
     else ((powMod base ((pow - 1) `div` 2) m) ^ 2 * base) `mod` m

inv :: Op -> Op
inv (Op n a b) = Op n a' b'
  where
    a' = powMod a (n - 2) n
    b' = -(a' * b) `mod` n

stack :: Integer -> Op
stack n = Op n (-1) (-1)

cut :: Integer -> Integer -> Op
cut n k = Op n 1 (-k)

icut :: Integer -> Integer -> Op
icut n k = Op n 1 k

deal :: Integer -> Integer -> Op
deal n k = Op n k 0

toOp :: Integer -> Shuffle -> Op
toOp n Stack    = stack n
toOp n (Cut k)  = cut n k
toOp n (Deal k) = deal n k

toInvOp :: Integer -> Shuffle -> Op
toInvOp n Stack    = stack n
toInvOp n (Cut k)  = icut n k
toInvOp n (Deal k) = deal n (-k)

apply :: Op -> Integer -> Integer
apply (Op n a b) i = (a * i + b) `mod` n

-- For some reasons, stimes is slow. Let's try manually.
repeated :: Integer -> Op -> Op
repeated 0 (Op n a b) = Op n 1 0
repeated 1 op = op
repeated n op =
  let (x, y) = n `divMod` 2
      r = repeated x op
      rr = r <> r
   in case y of
        0 -> rr
        1 -> op <> rr

-- Parser
shuffleP :: Parser [Shuffle]
shuffleP =
  sepEndBy
    (choice
       [ string "deal " >>
         choice
           [ string "into new stack" $> Stack
           , string "with increment " >> int <&> Deal
           ]
       , string "cut " >> int <&> Cut
       ])
    newline

-- Main
main :: IO ()
main = do
  putStr "apply Stack: "
  let s = toOp 10 Stack
  print
    [ apply s 9 == 0
    , apply s 8 == 1
    , apply s 7 == 2
    , apply s 6 == 3
    , apply s 5 == 4
    , apply s 4 == 5
    , apply s 3 == 6
    , apply s 2 == 7
    , apply s 1 == 8
    , apply s 0 == 9
    ]
  putStr "apply Cut 3: "
  let c3 = toOp 10 (Cut 3)
  print
    [ apply c3 3 == 0
    , apply c3 4 == 1
    , apply c3 5 == 2
    , apply c3 6 == 3
    , apply c3 7 == 4
    , apply c3 8 == 5
    , apply c3 9 == 6
    , apply c3 0 == 7
    , apply c3 1 == 8
    , apply c3 2 == 9
    ]
  putStr "apply inv Cut 3: "
  let ic3 = toInvOp 10 (Cut 3)
  print
    [ apply ic3 0 == 3
    , apply ic3 1 == 4
    , apply ic3 2 == 5
    , apply ic3 3 == 6
    , apply ic3 4 == 7
    , apply ic3 5 == 8
    , apply ic3 6 == 9
    , apply ic3 7 == 0
    , apply ic3 8 == 1
    , apply ic3 9 == 2
    ]
  putStr "apply Cut (-4): "
  let cm4 = toOp 10 (Cut (-4))
  print
    [ apply cm4 6 == 0
    , apply cm4 7 == 1
    , apply cm4 8 == 2
    , apply cm4 9 == 3
    , apply cm4 0 == 4
    , apply cm4 1 == 5
    , apply cm4 2 == 6
    , apply cm4 3 == 7
    , apply cm4 4 == 8
    , apply cm4 5 == 9
    ]
  putStr "apply inv Cut (-4): "
  let icm4 = toInvOp 10 (Cut (-4))
  print
    [ apply icm4 0 == 6
    , apply icm4 1 == 7
    , apply icm4 2 == 8
    , apply icm4 3 == 9
    , apply icm4 4 == 0
    , apply icm4 5 == 1
    , apply icm4 6 == 2
    , apply icm4 7 == 3
    , apply icm4 8 == 4
    , apply icm4 9 == 5
    ]
  putStr "apply Deal 3: "
  let d3 = toOp 10 (Deal 3)
  print
    [ apply d3 0 == 0
    , apply d3 7 == 1
    , apply d3 4 == 2
    , apply d3 1 == 3
    , apply d3 8 == 4
    , apply d3 5 == 5
    , apply d3 2 == 6
    , apply d3 9 == 7
    , apply d3 6 == 8
    , apply d3 3 == 9
    ]
  putStr "apply deal -3: "
  let id3 = toInvOp 10 (Deal 3)
  print
    [ apply id3 0 == 0
    , apply id3 1 == 7
    , apply id3 2 == 4
    , apply id3 3 == 1
    , apply id3 4 == 8
    , apply id3 5 == 5
    , apply id3 6 == 2
    , apply id3 7 == 9
    , apply id3 8 == 6
    , apply id3 9 == 3
    ]
  contents <- getContents
  let (Right shuffles) = parse shuffleP "" contents
  -- Part 1
  let n1 = 10007
  let ops1 = toOp n1 <$> shuffles
  let o1 = sconcat (NonEmpty.fromList ops1)
  let p1 = apply o1 2019
  print p1
  --
  let n2 = 119315717514047
  let r2 = 101741582076661
  let ops2 = toOp n2 <$> shuffles
  let o2 = sconcat (NonEmpty.fromList ops2)
  print o2
  let or2 = repeated r2 o2
  print or2
  let ior2 = inv or2
  let p2 = apply ior2 2020
  print p2
