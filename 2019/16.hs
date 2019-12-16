import           Utils                                (rdigits)

import qualified Data.List                            as List
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Number (int)

digits :: Integer -> [Integer]
digits = List.reverse . rdigits

undigits :: [Integer] -> Integer
undigits = foldl (\acc x -> acc * 10 + x) 0

lastDigit :: Integer -> Integer
lastDigit x = abs (x `rem` 10)

firstNDigits :: Int -> Integer -> Integer
firstNDigits n = undigits . take n . digits

nDigitsAtPosition :: Int -> Int -> Integer -> Integer
nDigitsAtPosition start n = undigits . take n . drop (start - 1) . digits

pattern_ :: Int -> [Integer]
pattern_ n =
  List.drop 1 $
  List.cycle (List.concat [List.replicate n x | x <- [0, 1, 0, -1]])

applyPattern :: Int -> [Integer] -> Integer
applyPattern n xs = lastDigit (sum withPattern)
  where
    withPattern = zipWith (*) xs (pattern_ n)

fft_ :: [Integer] -> [Integer]
fft_ ds = [applyPattern i ds | i <- [1 .. (List.length ds)]]

fftN_ :: Integer -> [Integer] -> [Integer]
fftN_ 0 ds = ds
fftN_ n ds = fftN_ (n - 1) (fft_ ds)

fft :: Integer -> Integer
fft x = undigits (fft_ (digits x))

fftN :: Integer -> Integer -> Integer
fftN n x = undigits (fftN_ n (digits x))

part1 :: Integer -> Integer
part1 = firstNDigits 8 . fftN 100

fftWithUpperTriangle :: [Integer] -> [Integer]
fftWithUpperTriangle ds = List.map lastDigit (scanr1 (+) ds)

fftNwithUpperTriangle :: Int -> [Integer] -> [Integer]
fftNwithUpperTriangle 0 ds = ds
fftNwithUpperTriangle n ds =
  fftNwithUpperTriangle (n - 1) (fftWithUpperTriangle ds)

part2 :: Integer -> Integer
part2 x = undigits (take 8 (fftNwithUpperTriangle 100 xs))
  where
    offset = fromIntegral (firstNDigits 7 x) :: Int
    ds = digits x
    ys = List.concat (List.replicate 10000 ds)
    xs = List.drop offset ys

main :: IO ()
main
  -- Tests
 = do
  print tests
  -- Puzzle
  contents <- getContents
  let (Right start) = parse int "" contents
  -- Part 1
  print (part1 start)
  -- Part 2
  print (part2 start)

tests :: [Bool]
tests =
  [ test1 ex1
  , test1 ex2
  , test1 ex3
  , test1 ex4
  , test11 ex5
  , test11 ex6
  , test11 ex7
  ]

test1 :: Example1 -> Bool
test1 ex = getEx1Expected ex == fftN (getEx1Phases ex) (getEx1Input ex)

test11 :: Example1 -> Bool
test11 ex =
  getEx1Expected ex == firstNDigits 8 (fftN (getEx1Phases ex) (getEx1Input ex))

data Example1 =
  Example1
    { getEx1Input    :: Integer
    , getEx1Phases   :: Integer
    , getEx1Expected :: Integer
    }

ex1 = Example1 12345678 1 48226158

ex2 = Example1 12345678 2 34040438

ex3 = Example1 12345678 3 03415518

ex4 = Example1 12345678 4 01029498

ex5 = Example1 80871224585914546619083218645595 100 24176176

ex6 = Example1 19617804207202209144916044189917 100 73745418

ex7 = Example1 69317163492948606335995924319873 100 52432133
