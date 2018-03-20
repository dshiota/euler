{-# OPTIONS -Wall #-}
module Euler.Problem030
    ( problem030
    ) where

-- problem030
{-
 -       1 = 1 ->  59049 = 9^5
 -      11 = 2 -> 118098 = 9^5+9^5
 -     111 = 3 -> 177147 = 9^5+9^5+9^5
 -    1111 = 4 -> 236196 = 9^5+9^5+9^5+9^5
 -   11111 = 5 -> 295245 = 9^5+9^5+9^5+9^5+9^5
 -  111111 = 6 -> 354294 = 9^5+9^5+9^5+9^5+9^5+9^5
 - 1111111 = 7 -> 413343 = 9^5+9^5+9^5+9^5+9^5+9^5+9^5
 -}
problem030 :: Int
problem030 = sum $ filter (\x -> x == sumOfDigitsNth x (5 :: Int)) [2..354295]

sumOfDigitsNth :: (Show a, Integral b) => a -> b -> Int
sumOfDigitsNth d n = sum (map (\c -> (read [c] :: Int)^n) (show d))


