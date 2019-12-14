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

rdigits :: Integral a => a -> [a]
rdigits 0 = []
rdigits x = x `mod` 10 : rdigits (x `div` 10)

rightOrError :: Show a => Either a b -> b
rightOrError (Left err) = error (show err)
rightOrError (Right x)  = x

justOrError :: Maybe a -> a
justOrError Nothing  = error "Nothing"
justOrError (Just x) = x
