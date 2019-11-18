data CubeCoord = CubeCoord Int Int Int deriving (Show)

origin :: CubeCoord
origin = CubeCoord 0 0 0

add :: CubeCoord -> CubeCoord -> CubeCoord
add (CubeCoord x1 y1 z1) (CubeCoord x2 y2 z2) = CubeCoord (x1 + x2) (y1 + y2) (z1 + z2)

cubeDist :: CubeCoord -> CubeCoord -> Int
cubeDist (CubeCoord x1 y1 z1) (CubeCoord x2 y2 z2) = sum (map abs [x1 - x2, y1 - y2, z1 - z2]) `div` 2

destinations :: [CubeCoord] -> [CubeCoord]
destinations path = scanl add origin path

parsePath :: String -> [CubeCoord]
parsePath [] = []
parsePath ('n':'e':rest) = CubeCoord 1 0 (-1) : (parsePath rest)
parsePath ('n':'w':rest) = CubeCoord (-1) 1 0 : (parsePath rest)
parsePath ('n':rest)     = CubeCoord 0 1 (-1) : (parsePath rest)
parsePath ('s':'e':rest) = CubeCoord 1 (-1) 0 : (parsePath rest)
parsePath ('s':'w':rest) = CubeCoord (-1) 0 1 : (parsePath rest)
parsePath ('s':rest)     = CubeCoord 0 (-1) 1 : (parsePath rest)
parsePath (_:rest) = parsePath rest


main :: IO ()
main = do
  input <- readFile "input.txt"
  let path = parsePath input
  let distances = map (cubeDist origin) (destinations path)
  putStrLn . show $ last distances
  putStrLn . show $ maximum distances
