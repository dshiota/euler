{-# OPTIONS -Wall #-}
module Euler.Problem021
    ( problem021
    ) where

-- problem021
problem021 :: Integer
problem021 = sum $ filter isAmicable [(2::Integer)..9999]

isAmicable :: Integral a => a -> Bool
isAmicable n = n == sumOfDiv m && n /= m
  where m = sumOfDiv n

sumOfDiv :: Integral a => a -> a
sumOfDiv n = sum (divisors n)

divisors :: Integral t => t -> [t]
divisors n = [x | x <- [1..n `div` 2], n `mod` x == 0]

