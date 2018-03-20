{-# OPTIONS -Wall #-}
module Euler.Problem027
    ( problem027
    ) where

-- problem027
problem027 :: Integer
problem027 = (1-2*p)*(p*p-p+41)
  where n = 1000
        m = head $ filter (\x-> x*x-x+41>n) [1..]
        p = m-1
{-
isPrimeNum :: Integral t => t -> Bool
isPrimeNum n = isPrime' n primes

isPrime' :: Ord t => t -> [t] -> Bool
isPrime' n prms@(p:ps)
  | n > p = isPrime' n ps
  | n == p = True
  | otherwise = False
-}


