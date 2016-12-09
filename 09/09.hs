import Text.ParserCombinators.Parsec

data Exp = Txt String | Rep Int Int [Exp] deriving Show

expLength1 :: Exp -> Int
expLength1 e = case e of
  (Txt s) -> length s
  (Rep n r _) -> n * r
  
expLength2 :: Exp -> Int
expLength2 e = case e of
  (Txt s) -> length s
  (Rep _ r se) -> r * (sum . map expLength2 $ se)

-- Parser.
exps :: Parser [Exp]
exps = many (txt <|> rep)

txt :: Parser Exp
txt = Txt <$> many1 upper

rep :: Parser Exp
rep = do
  char '('
  n <- int
  char 'x'
  r <- int
  char ')'
  s <- count n anyChar
  let es = parse exps "" s
  case es of
    (Right es) -> return (Rep n r es)
    (Left _) -> fail "Could not parse subexpr."
  
int :: Parser Int
int = read <$> many1 digit

-- Main.
main = do
  input <- getLine
  let es = parse exps "" input
  putStrLn . show $ sum . map expLength1 <$> es
  putStrLn . show $ sum . map expLength2 <$> es
