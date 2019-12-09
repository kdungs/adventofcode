module Intcode.Chain where

import           Intcode

import qualified Data.List as List

type AmplifierChain = [VirtualMachine]

initChain :: [Integer] -> Integer -> Memory -> AmplifierChain
initChain (phase0:phases) start mem =
  initVm mem [phase0, start] : [initVm mem [phase] | phase <- phases]
initChain _ _ _ = []

runChain :: AmplifierChain -> Maybe Integer
runChain [] = Nothing
runChain [lastVm] = do
  res <- run lastVm
  pure (head . outputs $ res)
runChain (current:next:rest) = do
  (op, newCurrent) <- runUntil isOutputOrDone current
  case op of
    Done -> runChain (next : rest)
    Output ->
      runChain
        (next {inputs = inputs next ++ [List.head (outputs newCurrent)]} :
         rest ++ [newCurrent])
    _ -> Nothing -- Error!
  where
    isOutputOrDone :: Operation -> Bool
    isOutputOrDone Done   = True
    isOutputOrDone Output = True
    isOutputOrDone _      = False
