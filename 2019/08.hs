import           Data.Functor                  (($>))
import qualified Data.List                     as List
import           Text.ParserCombinators.Parsec

-- List helpers
groups :: Int -> [a] -> [[a]]
groups _ []    = []
groups size xs = List.take size xs : groups size (List.drop size xs)

countElem :: Eq a => a -> [a] -> Int
countElem x xs = List.length (List.filter (== x) xs)

-- Solution
data Color
  = Black
  | White
  | Transparent
  deriving (Eq)

instance Show Color where
  show Black       = "█"
  show White       = " "
  show Transparent = "X"

overlayC :: Color -> Color -> Color
overlayC Transparent c = c
overlayC c _           = c

data Image =
  Image
    { width  :: Int
    , height :: Int
    , layers :: [[Color]]
    }

instance Show Image where
  show (Image w _ ls) = List.intercalate "\n" (fmtRow <$> rows)
    where
      reduced = foldl1 (zipWith overlayC) ls
      rows = groups w reduced
      fmtRow cs = List.intercalate "" (show <$> cs)

fromList :: Int -> Int -> [Color] -> Image
fromList w h cs = Image w h layers
  where
    layers = groups (w * h) cs

layerWithMinimumBlack :: Image -> [Color]
layerWithMinimumBlack (Image _ _ ls) = fst (List.minimumBy cmpSnd blackCounts)
  where
    blackCounts = (\l -> (l, countElem Black l)) <$> ls
    cmpSnd lhs rhs = compare (snd lhs) (snd rhs)

-- Parser
color :: Parser Color
color = choice [char '0' $> Black, char '1' $> White, char '2' $> Transparent]

colors :: Parser [Color]
colors = many1 color

-- Main
rightOrFail :: Show a => Either a b -> b
rightOrFail (Left err) = error (show err)
rightOrFail (Right x)  = x

main :: IO ()
main = do
  contents <- getContents
  let cs = rightOrFail (parse colors "" contents)
  let img = fromList 25 6 cs
  print img
  let lmb = layerWithMinimumBlack img
  let cWhite = countElem White lmb
  let cTransparent = countElem Transparent lmb
  print (cWhite * cTransparent)
