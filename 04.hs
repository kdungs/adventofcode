import Data.Char (digitToInt)
import Data.List
import Text.ParserCombinators.Parsec
import qualified Data.Map.Strict as M

data Room = Room { name :: String
                 , sector :: Int
                 , checksum :: String } deriving (Show)

data CountedChar = CountedChar Int Char deriving (Eq, Show)
instance Ord CountedChar where
  CountedChar n c `compare` CountedChar n' c' = if (n == n')
                                                  then c `compare` c'
                                                  else n' `compare` n

legalRoom :: Room -> Bool
legalRoom r = all id (zipWith matches cs (checksum r))
                where matches (CountedChar _ c) c' = c == c'
                      cs = sort [CountedChar (length g) (g !! 0)
                                 | g <- group (sort (name r))]

onlyLegalRooms :: [Room] -> [Room]
onlyLegalRooms = filter legalRoom

sumSectorIds :: [Room] -> Int
sumSectorIds = foldl (\acc r -> acc + (sector r)) 0

-- Part 2.
rotatedAlphabet :: Int -> M.Map Char Char
rotatedAlphabet n = M.fromList (zip alphabet (rot n alphabet))
                      where alphabet = ['a'..'z']
                            rot n = take 26 . drop n . cycle

data DecryptedRoom = DecryptedRoom { dName :: String
                                   , dSector :: Int }
instance Show DecryptedRoom where
  show (DecryptedRoom n s) = n ++ ": " ++ (show s)

decrypt :: Room -> DecryptedRoom
decrypt r = DecryptedRoom (map decode (name r)) (sector r)
              where rA = rotatedAlphabet (sector r)
                    decode c = rA M.! c

-- Parser.
parseRoomName :: Parser String
parseRoomName = concat <$> (many1 $ do
                                      part <- many1 lower
                                      char '-'
                                      return part)

parseDecimal :: Parser Int
parseDecimal = convertDecimal <$> many1 digit where
  convertDecimal = foldl' (\a i -> a * 10 + digitToInt i) 0

parseRoom :: Parser Room
parseRoom = do
              name <- parseRoomName
              sector <- parseDecimal
              char '['
              checksum <- many1 lower
              char ']'
              return (Room name sector checksum)

parseRooms :: Parser [Room]
parseRooms = many1 $ do
                       room <- parseRoom
                       newline
                       return room

-- Main.
main = do
         input <- getContents
         let rooms = parse parseRooms "" input
         let legalRooms = onlyLegalRooms <$> rooms
         let result = sumSectorIds <$> legalRooms
         putStrLn . show $ result
         let decryptedRooms = (map decrypt) <$> legalRooms
         putStrLn . show $ (map show) <$> decryptedRooms
