import Data.Char (digitToInt)
import Data.List
import Text.ParserCombinators.Parsec

nrows :: Int
nrows = 6

ncols :: Int
ncols = 50

data Led = On | Off
instance Show Led where
  show On = "█"
  show Off = "░"

data Row = Row [Led]
instance Show Row where
  show (Row leds) = concat (map show leds)

makeRow :: Led -> Int -> Row
makeRow led n = Row [led | _ <- [1..n]]

onRow = makeRow On
offRow = makeRow Off

turnOnFront :: Int -> Row -> Row
turnOnFront n (Row leds) = Row (onLeds ++ rest)
                             where (Row onLeds) = onRow n
                                   rest = drop n leds

rotateSingleRow :: Int -> Row -> Row
rotateSingleRow n (Row leds) = Row (back ++ front)
                                 where (front, back) = splitAt n' leds
                                       n' = (length leds) - n

data Display = Display [Row]
instance Show Display where
  show (Display rows) = intercalate "\n" (map show rows)

emptyDisplay :: Display
emptyDisplay = Display [offRow ncols | _ <- [1..nrows]]

rect :: Int -> Int -> Display -> Display
rect a b (Display rows) = Display (map (turnOnFront a) upper ++ lower)
                            where (upper, lower) = splitAt b rows

select :: Int -> [a] -> ([a], a, [a])
select n xs = (front, elem, back)
                               where (front, (elem:back)) = splitAt n xs

rotateRow :: Int -> Int -> Display -> Display
rotateRow y n (Display rows) = Display (front ++ (rotateSingleRow n elem):back)
                                 where (front, elem, back) = select y rows

transposeDisplay :: Display -> Display
transposeDisplay (Display rows) = Display (map Row (transpose leds))
                                    where leds = [leds | (Row leds) <- rows]

rotateCol :: Int -> Int -> Display -> Display
rotateCol x n d = transposeDisplay (rotateRow x n d')
                    where d' = transposeDisplay d

data Instruction = Rect Int Int | RotCol Int Int | RotRow Int Int

applyInstruction :: Display -> Instruction -> Display
applyInstruction d ins = case (ins) of
                           (Rect a b) -> rect a b d
                           (RotCol x n) -> rotateCol x n d
                           (RotRow y n) -> rotateRow y n d

applyInstructions :: Display -> [Instruction] -> Display
applyInstructions d0 = foldl applyInstruction d0

countOnLeds :: Display -> Int
countOnLeds (Display rows) = sum (map count' rows)
  where count' (Row leds) = length $ filter isOn leds
        isOn led = case (led) of
                     On -> True
                     Off -> False

-- Parser.
parseDecimal :: Parser Int
parseDecimal = read <$> many1 digit

parseRect :: Parser Instruction 
parseRect = do
              string "rect "
              a <- parseDecimal
              char 'x'
              b <- parseDecimal
              return (Rect a b)

parseRotCol :: Parser Instruction
parseRotCol = do
                string "rotate column x="
                x <- parseDecimal
                string " by "
                n <- parseDecimal
                return (RotCol x n)

parseRotRow :: Parser Instruction
parseRotRow = do
                string "rotate row y="
                y <- parseDecimal
                string " by "
                n <- parseDecimal
                return (RotRow y n)

instruction :: Parser Instruction
instruction = try parseRect <|> try parseRotCol <|> parseRotRow

instructions :: Parser [Instruction]
instructions = many $ do
                        ins <- instruction
                        newline
                        return ins

-- Main.
main = do
         input <- getContents
         let inss = parse instructions "" input
         let result = applyInstructions emptyDisplay <$> inss
         putStrLn . show $ countOnLeds <$> result
         putStrLn $ case (result) of
          (Right x) -> show x
          (Left e) -> show e
