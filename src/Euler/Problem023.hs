{-# OPTIONS -Wall #-}
module Euler.Problem023
    ( problem023
    ) where

import EulerUtil

-- problem023
problem023 :: Integer
problem023 = sum $ filter (not . isSumOfAbundant) [(1::Integer)..28123]

sumOfDivs :: Integer -> Integer
sumOfDivs 1 = 0
sumOfDivs n = product [calc f i | (f,i) <- factorize n] - n
  where calc f i = div (f^(i+1)-1) (f-1)

isAbundant :: Integer -> Bool
isAbundant 1 = False
isAbundant n = n < sumOfDivs n

{-
candAbundant :: [Integer]
candAbundant = filter isAbundant [12..28123]
-}

isSumOfAbundant :: Integer -> Bool
isSumOfAbundant n = isSumOfAbundant' n candAbundant
  where
    isSumOfAbundant' _ [] = False
    isSumOfAbundant' n' (a:as)
      | n' <= a = False
      | isAbundant (n' - a) = True
      | otherwise = isSumOfAbundant' n' as
    candAbundant = filter isAbundant [(12::Integer)..28123]


