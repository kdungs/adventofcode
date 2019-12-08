module Utils
  ( module Utils
  ) where

import qualified Data.List as List

atWithDefault :: a -> Int -> [a] -> a
atWithDefault def pos xs
  | pos < List.length xs = xs !! pos
  | otherwise = def

headM :: [a] -> Maybe a
headM []    = Nothing
headM (x:_) = Just x

rightOrError :: Show a => Either a b -> b
rightOrError (Left err) = error (show err)
rightOrError (Right x)  = x
