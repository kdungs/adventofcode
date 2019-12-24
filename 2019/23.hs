import           Intcode

import qualified Data.List                     as List
import qualified Data.Map.Strict               as Map
import           Data.Maybe                    (catMaybes)
import qualified Data.Set                      as Set
import           Text.ParserCombinators.Parsec (parse)

data Packet =
  Packet Integer Integer
  deriving (Eq, Show)

data M =
  M
    { getVm   :: VirtualMachine
    , getAdr  :: Integer
    , getIdle :: Bool
    , getQ    :: [Packet]
    }
  deriving (Show)

initM :: Integer -> Memory -> M
initM adr prog =
  M {getVm = initVm prog [adr], getAdr = adr, getIdle = False, getQ = []}

toInputs :: [Packet] -> [Integer]
toInputs ps = List.concat [[x, y] | (Packet x y) <- ps]

stepM :: M -> (M, Maybe (Integer, Packet))
stepM m =
  case stepOrWaitForInput (getVm m) of
    Nothing -> error "This should not happen!"
    Just (False, Output, nextVm) ->
      case outputs nextVm of
        [y, x, adr] ->
          (m {getVm = nextVm {outputs = []}}, Just (adr, Packet x y))
        _ -> (m {getVm = nextVm}, Nothing)
    Just (False, _, nextVm) -> (m {getVm = nextVm}, Nothing)
    Just (True, _, _) ->
      if List.null (getQ m)
        then stepM m {getVm = (getVm m) {inputs = [-1]}, getIdle = True}
        else stepM
               m
                 { getVm = (getVm m) {inputs = toInputs (getQ m)}
                 , getQ = []
                 , getIdle = False
                 }

data NW =
  NW
    { getMs  :: Map.Map Integer M
    , getNAT :: Packet
    }
  deriving (Show)

initNW :: Integer -> Memory -> NW
initNW n prog =
  NW
    { getMs = Map.fromList [(adr, initM adr prog) | adr <- [0 .. n - 1]]
    , getNAT = Packet 0 0
    }

appendQ :: Packet -> M -> M
appendQ p m = m {getQ = getQ m ++ [p], getIdle = False}

appendQs :: Map.Map Integer M -> [(Integer, Packet)] -> Map.Map Integer M
appendQs ms [] = ms
appendQs ms ((adr, p):rest) =
  let nextMs = Map.adjust (appendQ p) adr ms
   in appendQs nextMs rest

stepNW :: NW -> NW
stepNW nw = do
  let steps = stepM <$> Map.elems (getMs nw)
  let ms = Map.fromList [(getAdr m, m) | m <- fst <$> steps]
  let qs = catMaybes (snd <$> steps)
  let nextMs = appendQs ms qs
  let nextNAT =
        case List.find (\(adr, _) -> adr == 255) qs of
          Nothing     -> getNAT nw
          Just (_, p) -> p
  nw {getMs = nextMs, getNAT = nextNAT}

runUntilNat :: NW -> NW
runUntilNat nw =
  let nextNw = stepNW nw
   in if getNAT nw /= getNAT nextNw
        then nextNw
        else runUntilNat nextNw

allIdle :: NW -> Bool
allIdle nw = List.all getIdle (Map.elems (getMs nw))

runUntilAllIdle :: NW -> NW
runUntilAllIdle nw =
  let nextNw = stepNW nw
   in if allIdle nextNw
        then nextNw
        else runUntilAllIdle nextNw

provideNatToFirstM :: NW -> NW
provideNatToFirstM nw = nw {getMs = Map.adjust withNat 0 (getMs nw)}
  where
    withNat = appendQ (getNAT nw)

runUntilValueSentTwiceByNat :: Integer -> NW -> Integer
runUntilValueSentTwiceByNat lastY nw =
  let nextNw = runUntilAllIdle nw
      (Packet _ natY) = getNAT nextNw
   in if natY == lastY
        then natY
        else runUntilValueSentTwiceByNat natY (provideNatToFirstM nextNw)

-- Main
main :: IO ()
main = do
  contents <- getContents
  let (Right prog) = parse programParser "" contents
  let nw = initNW 50 prog
  let r1 = runUntilNat nw
  print $ getNAT r1
  --
  let r2 = runUntilValueSentTwiceByNat (-1) nw
  print r2
