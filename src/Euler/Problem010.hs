{-# OPTIONS -Wall #-}
module Euler.Problem010
    ( problem010
    ) where

-- problem10 2000000
problem010 :: Integer -> Integer
-- problem10 n = sum (takeWhile (< n) allPrimes)
problem010 n = sum $ eratosthenes n

-- eratosthenes :: Integer -> [Integer]
eratosthenes :: (Integral a) => a -> [a]
eratosthenes n
  | n < 2     = []
  | otherwise = reverse $ sieve n [] [2..n]

-- sieve :: Integer -> [Integer] -> [Integer] -> [Integer]
sieve :: (Integral a) => a -> [a] -> [a] -> [a]
sieve _ eprimes [] = eprimes
sieve n eprimes (x:xs)
  | x^(2::Integer) < n = sieve n (x:eprimes) (filter (\y -> y `mod` x /= 0) xs)
  | otherwise          = sieve n (x:eprimes) xs 

