{-# OPTIONS -Wall #-}
module Euler.Problem043
    ( problem043
    ) where

import Data.List (permutations)

-- problem043
problem043 :: Integer
problem043 = sum $ map read $ filter condp43 pandigitList

pandigitList :: [String]
pandigitList = filter (\s-> '0' /= head s) $ permutations "0123456789"

partd :: String -> Int -> Int -> Int -> Integer
partd s i j k = read [s!!(i-1),s!!(j-1),s!!(k-1)] :: Integer

condp43 :: String -> Bool
condp43 s = 
  partd s 2 3 4 `mod` 2 == 0 &&
  partd s 3 4 5 `mod` 3 == 0 &&
  partd s 4 5 6 `mod` 5 == 0 &&
  partd s 5 6 7 `mod` 7 == 0 &&
  partd s 6 7 8 `mod` 11 == 0 &&
  partd s 7 8 9 `mod` 13 == 0 &&
  partd s 8 9 10 `mod` 17 == 0


