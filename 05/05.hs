import Crypto.Hash
import Crypto.Hash.Algorithms (MD5)
import Data.ByteString.Builder
import Data.Char (digitToInt)
import Data.List
import qualified Data.ByteString.Lazy as L
import qualified Data.Map.Strict as M

toStrictByteString = L.toStrict . toLazyByteString

md5 :: Builder -> String
md5 b = show (hash (toStrictByteString b) :: Digest MD5)

hashIdAndIndex :: Builder -> Int -> String
hashIdAndIndex b i = md5 (b `mappend` intDec i)

startsWithFiveZeros :: String -> Bool
startsWithFiveZeros s = "00000" `isPrefixOf` s

allHashes :: Builder -> [String]
allHashes b = [hashIdAndIndex b i | i <- [1..]]

findPassword :: Builder -> String
findPassword b = take 8 (map
                           (\s -> s !! 5)
                           (filter
                              startsWithFiveZeros
                              (allHashes b)))
-- Part 2.
insertIfNotPresent :: Ord k => k -> a -> M.Map k a -> M.Map k a
insertIfNotPresent k v m = if (M.member k m)
                             then m
                             else (M.insert k v m)

legalPos :: Char -> Maybe Int
legalPos p = if (p `elem` ['0' .. '7'])
               then Just (digitToInt p)
               else Nothing

insertPart :: String -> M.Map Int Char -> M.Map Int Char
insertPart s m = case (legalPos k) of
                   Just x -> insertIfNotPresent x v m
                   Nothing -> m
                 where k = s !! 5
                       v = s !! 6

buildMap :: M.Map Int Char -> [String] -> M.Map Int Char
buildMap m (h:hs) = case (M.size m) of
                     8 -> m
                     _ -> if (startsWithFiveZeros h)
                            then buildMap (insertPart h m) hs
                            else buildMap m hs

mapToString :: M.Map Int Char -> String
mapToString m = foldl (\acc x -> acc ++ [x]) "" m

findPassword' :: Builder -> String
findPassword' b = mapToString $ buildMap M.empty (allHashes b)

-- Main.
main = do
         input <- getLine
         let builder = (stringUtf8 input)
         let pw1 = findPassword builder
         putStrLn . show $ pw1
         let pw2 = findPassword' builder
         putStrLn . show $ pw2
