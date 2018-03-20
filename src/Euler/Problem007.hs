{-# OPTIONS -Wall #-}
module Euler.Problem007
    ( -- problem007
    ) where

-- problem7 10001
{-
-- very slow
problem7 :: Int -> Integer
problem7 n = last $ take n allPrimes

allPrimes :: [Integer]
allPrimes = [x | x <- [2..], isPrime x]
-}


