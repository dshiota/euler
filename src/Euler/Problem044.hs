{-# OPTIONS -Wall #-}
module Euler.Problem044
    ( problem044
    ) where

-- problem044
problem044 :: Integer
problem044 = head [a - b | a <- pentaList , b <- takeWhile (<a) pentaList, isPenta' (a-b), isPenta' (a+b)]

pentaList :: [Integer]
pentaList = [n * (3 * n - 1) `div` 2 | n <- [(1::Integer)..5000]]

isPenta' :: Integer -> Bool
isPenta' n = isPentaInner' n $ filter (<=n) pentaList
  where isPentaInner' _ [] = False
        isPentaInner' k (x:xs)
          | k == x = True
          | k < x  = False
          | otherwise  = isPentaInner' k xs


