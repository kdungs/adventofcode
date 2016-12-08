import Data.List
import Text.ParserCombinators.Parsec

data Ip = Ip { sseqs :: [String]
             , hseqs :: [String] }

hasAbba :: String -> Bool
hasAbba (a:b:c:d:xs) = if and [a == d, b == c, a /= b]
                         then True
                         else hasAbba (b:c:d:xs)
hasAbba _ = False

hasTls :: Ip -> Bool
hasTls ip = (any hasAbba (sseqs ip)) && (all (not . hasAbba) (hseqs ip))

-- Part 2.
allAba :: String -> [String]
allAba (a:b:c:xs) = if a == c && a /= b
                      then (a:b:a:""):rest
                      else rest
                    where rest = allAba (b:c:xs)
allAba _ = []

toBab :: String -> String
toBab (a:b:c) = b:a:b:""
toBab _ = error "toBab has to be called with length 3"

hasSsl :: Ip -> Bool
hasSsl ip = any hasBab (hseqs ip)
              where abas = concat (map allAba (sseqs ip))
                    babs = map toBab abas
                    hasBab s = any (\b -> b `isInfixOf` s) babs

-- Parser.
file :: Parser [Ip]
file = many1 $ do
                 base <- many1 lower
                 rest <- many $ do
                                  char '['
                                  hseq <- many1 lower
                                  char ']'
                                  sseq <- many1 lower
                                  return (sseq, hseq)
                 newline
                 let sseqs = [base] ++ [sseq | (sseq, _) <- rest]
                 let hseqs = [hseq | (_, hseq) <- rest]
                 return (Ip sseqs hseqs)

-- Main.
main = do
         input <- getContents
         let dat = parse file "" input
         let result1 = length <$> (filter hasTls) <$> dat
         putStrLn . show $ result1
         let result2 = length <$> (filter hasSsl) <$> dat
         putStrLn . show $ result2
