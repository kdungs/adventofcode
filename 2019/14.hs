import           Utils                                (rightOrError)

import           Control.Monad                        (sequence)
import qualified Data.List                            as List
import qualified Data.Map                             as Map
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Number (int)

trillion :: Integer
trillion = 1000000000000

newtype Chemical =
  Chemical String
  deriving (Eq, Ord, Show)

ore :: Chemical
ore = Chemical "ORE"

data Quantity =
  Quantity
    { getChemical :: Chemical
    , getAmount   :: Integer
    }
  deriving (Eq, Ord, Show)

data Recipe =
  Recipe
    { getIngredients :: [Quantity]
    , getResult      :: Quantity
    }
  deriving (Eq, Ord, Show)

newtype RecipeBook =
  RecipeBook (Map.Map Chemical Recipe)
  deriving (Eq, Ord, Show)

fromRecipes :: [Recipe] -> RecipeBook
fromRecipes rs =
  RecipeBook (Map.fromList [(getChemical (getResult r), r) | r <- rs])

data Requirements =
  Requirements
    { getOre   :: Integer
    , getTable :: Map.Map Chemical Integer
    }
  deriving (Eq, Ord, Show)

fuelRequirement :: Integer -> Requirements
fuelRequirement i =
  Requirements {getOre = 0, getTable = Map.singleton (Chemical "FUEL") i}

plusR :: Requirements -> Requirements -> Requirements
plusR r1 r2 =
  Requirements
    { getOre = getOre r1 + getOre r2
    , getTable = Map.unionWith (+) (getTable r1) (getTable r2)
    }

fromRawTable :: Map.Map Chemical Integer -> Requirements
fromRawTable t =
  case Map.lookup ore t of
    Nothing -> Requirements {getOre = 0, getTable = t}
    Just n  -> Requirements {getOre = n, getTable = Map.insert ore 0 t}

ingredients :: Integer -> Recipe -> Requirements
ingredients n r =
  fromRawTable $
  Map.fromList
    ((c0, -f * a0) : [(getChemical i, f * getAmount i) | i <- getIngredients r])
  where
    (Quantity c0 a0) = getResult r
    (f_, remainder) = n `quotRem` a0
    f =
      if remainder == 0
        then f_
        else f_ + 1

ingredientsFromBook :: Integer -> Chemical -> RecipeBook -> Maybe Requirements
ingredientsFromBook n c (RecipeBook rm) = ingredients n <$> Map.lookup c rm

resolve :: RecipeBook -> Requirements -> Maybe Requirements
resolve rb rs =
  if Map.null posReq
    then Just rs
    else do
      let (c, n) = List.head (Map.assocs posReq)
      ins <- ingredientsFromBook n c rb
      resolve rb (rs `plusR` ins)
  where
    posReq = Map.filter (> 0) (getTable rs)

oreRequiredForFuel :: Integer -> RecipeBook -> Maybe Integer
oreRequiredForFuel i rb = getOre <$> resolve rb (fuelRequirement i)

findMaxFuelForOre_ ::
     Integer -> RecipeBook -> Integer -> Integer -> Maybe Integer
findMaxFuelForOre_ limit rb left right
  | left == right = Just left
  | left == (right - 1) = Just left
  | otherwise = do
    let center = left + (right - left) `quot` 2
    oreq <- oreRequiredForFuel center rb
    if oreq > limit
      then findMaxFuelForOre_ limit rb left center
      else findMaxFuelForOre_ limit rb center right

findMaxFuelForOre :: Integer -> RecipeBook -> Maybe Integer
findMaxFuelForOre limit rb = do
  n <- oreRequiredForFuel 1 rb
  let left = limit `quot` n
  findMaxFuelForOre_ limit rb left (10 * limit)

-- Parsers
quantity :: Parser Quantity
quantity = do
  a <- int
  char ' '
  c <- many1 upper
  pure Quantity {getChemical = Chemical c, getAmount = a}

recipe :: Parser Recipe
recipe = do
  is <- sepBy quantity (string ", ")
  string " => "
  r <- quantity
  pure Recipe {getIngredients = is, getResult = r}

recipes :: Parser [Recipe]
recipes = sepEndBy recipe newline

-- Main
main :: IO ()
main
  -- Tests
 = do
  print [test1 e | e <- examples]
  print [test2 e | e <- examples2]
  -- Solution
  contents <- getContents
  let rs = rightOrError (parse recipes "" contents)
  let rb = fromRecipes rs
  let (Just oreReq1) = oreRequiredForFuel 1 rb
  print oreReq1
  --
  print (findMaxFuelForOre trillion rb)

-- Tests
test1 :: Example -> String
test1 (Example expected input) =
  case parse recipes "" input of
    Left err -> show err
    Right rs ->
      case oreRequiredForFuel 1 (fromRecipes rs) of
        Nothing -> "No result!"
        Just x ->
          if x == expected
            then "True"
            else show x

data Example =
  Example Integer String

examples = [ex1, ex2, ex3, ex4, ex5]

ex1 :: Example
ex1 =
  Example
    31
    (unlines
       [ "10 ORE => 10 A"
       , "1 ORE => 1 B"
       , "7 A, 1 B => 1 C"
       , "7 A, 1 C => 1 D"
       , "7 A, 1 D => 1 E"
       , "7 A, 1 E => 1 FUEL"
       ])

ex2 =
  Example
    165
    (unlines
       [ "9 ORE => 2 A"
       , "8 ORE => 3 B"
       , "7 ORE => 5 C"
       , "3 A, 4 B => 1 AB"
       , "5 B, 7 C => 1 BC"
       , "4 C, 1 A => 1 CA"
       , "2 AB, 3 BC, 4 CA => 1 FUEL"
       ])

ex3 =
  Example
    13312
    (unlines
       [ "157 ORE => 5 NZVS"
       , "165 ORE => 6 DCFZ"
       , "44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL"
       , "12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ"
       , "179 ORE => 7 PSHF"
       , "177 ORE => 5 HKGWZ"
       , "7 DCFZ, 7 PSHF => 2 XJWVT"
       , "165 ORE => 2 GPVTF"
       , "3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"
       ])

ex4 =
  Example
    180697
    (unlines
       [ "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG"
       , "17 NVRVD, 3 JNWZP => 8 VPVL"
       , "53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL"
       , "22 VJHF, 37 MNCFX => 5 FWMGM"
       , "139 ORE => 4 NVRVD"
       , "144 ORE => 7 JNWZP"
       , "5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC"
       , "5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV"
       , "145 ORE => 6 MNCFX"
       , "1 NVRVD => 8 CXFTF"
       , "1 VJHF, 6 MNCFX => 4 RFSQX"
       , "176 ORE => 6 VJHF"
       ])

ex5 =
  Example
    2210736
    (unlines
       [ "171 ORE => 8 CNZTR"
       , "7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL"
       , "114 ORE => 4 BHXH"
       , "14 VRPVC => 6 BMBT"
       , "6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL"
       , "6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT"
       , "15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW"
       , "13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW"
       , "5 BMBT => 4 WPTQ"
       , "189 ORE => 9 KTJDG"
       , "1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP"
       , "12 VRPVC, 27 CNZTR => 2 XDBXC"
       , "15 KTJDG, 12 BHXH => 5 XCVML"
       , "3 BHXH, 2 VRPVC => 7 MZWV"
       , "121 ORE => 7 VRPVC"
       , "7 XCVML => 6 RJRHP"
       , "5 BHXH, 4 VRPVC => 5 LTCX"
       ])

test2 :: Example2 -> String
test2 (Example2 expected (Example opf input)) =
  case parse recipes "" input of
    Left err -> show err
    Right rs ->
      case findMaxFuelForOre trillion (fromRecipes rs) of
        Nothing -> "No result!"
        Just x ->
          if x == expected
            then "True"
            else show x

examples2 = [p2ex3, p2ex4, p2ex5]

data Example2 =
  Example2 Integer Example

p2ex3 = Example2 82892753 ex3

p2ex4 = Example2 5586022 ex4

p2ex5 = Example2 460664 ex5
