module Euler.Problem077
    ( problem077
    ) where

import Primality as P

problem077 :: Integer
problem077 = fst $ head $ dropWhile ((<=5000) . snd)
  [(n, primePartition n) | n <- [1..]]

-- https://zach.se/project-euler-solutions/77/

primes' :: [Integer]
primes' = P.primes

primePartition :: Integer -> Integer
primePartition  = f primes'
  where
  f _ 0 = 1
  f ks'@(k:ks) m = if m < k then 0 else f ks' (m-k) + f ks m

