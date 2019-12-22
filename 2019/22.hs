import           Data.Functor                         (($>), (<&>))
import qualified Data.List                            as List
import qualified Data.Map.Strict                      as Map
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Number

type Cards = [Int]

stack :: Cards -> Cards
stack = List.reverse

cut :: Int -> Cards -> Cards
cut n cs
  | n < 0 = cut (List.length cs + n) cs
  | otherwise = right ++ left
  where
    (left, right) = List.splitAt n cs

deal :: Int -> Cards -> Cards
deal n cs = Map.elems (Map.fromList (zip indices cs))
  where
    num = List.length cs
    indices = [(i * n) `mod` num | i <- [0 .. (num - 1)]]


applyShuffle :: [Cards -> Cards] -> Cards -> Cards
applyShuffle [] cs = cs
applyShuffle (s:rest) cs = (applyShuffle rest (s cs))


-- Parser
shuffleP :: Parser [(Cards -> Cards)]
shuffleP =
  sepEndBy
    (choice
       [ string "deal " >>
         choice
           [ string "into new stack" $> stack
           , string "with increment " >> int <&> deal
           ]
       , string "cut " >> int <&> cut
       ])
    newline

-- Main
main :: IO ()
main = do
  contents <- getContents
  let (Right shuffle) = parse shuffleP "" contents
  let shuffled = applyShuffle shuffle [0..10006]
  print (List.elemIndex 2019 shuffled)
