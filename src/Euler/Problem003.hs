{-# OPTIONS -Wall #-}
module Euler.Problem003
    ( problem003
    ) where

problem003 :: Integral a => a -> a
problem003 _ = undefined

-- problem003 600851475143
-- problem003 :: Integral a => a -> a
{-
problem3 :: Integer -> Integer
problem3 n = last (primeFactors n)
-}
-- problem3 n = maximum ( factorize n)

{-
primes :: [Integer]
primes = 2 : filter isPrime [3,5..]

isPrime :: Integer -> Bool
isPrime = null . tail . primeFactors
-}
{-
primes :: Integral a => [a]
primes = map fromIntegral ([2,3] ++ primes' :: [Int])
  where
    isPrime'' _ [] = False
    isPrime'' m (x:xs)
      | x * x > m = True
      | rem m x == 0 = False
      | otherwise = isPrime'' m xs
    primes' = 5 : sieve' 1
    sieve' n = filter (`isPrime''` primes') ns ++ sieve' (n + 1000)
      where ns = [6*x+y | x <- [n..n+999], y <- [1,5]]
-}
{-
primeFactors :: Integer -> [Integer]
primeFactors n = factor n primes
  where
    factor _n (p:ps)
     | p*p > _n        = [_n]
     | _n `mod` p == 0 = p : factor (_n `div` p) (p:ps)
     | otherwise      =     factor _n ps
-}
-- https://gist.github.com/liveinwood/6121076
-- factor :: Integral a => a -> a -> Bool
-- factor x y = x `mod` y == 0

{-
prime :: Integral a => a -> Bool
--prime n = length [x | x <- [1..n], factor n x] == 2
prime n
  | n == 1  = False
  | n < 4   = True
  | n `mod` 2 == 0 = False
  | n < 9 = False
  | n `mod` 3 == 0 = False
  | otherwise = null $ filter (\x -> ((fromIntegral x) < (isqrt n)) && (n `mod` x /= 0) && (n `mod` (x+2) /= 0)) [5, 11..]

isqrt :: (Floating a, Integral a1) => a1 -> a
isqrt n = sqrt $ fromIntegral n
-}
-- primes :: Integral a => a -> [a]
-- primes = eratosthenes
-- primes n = [x | x <- [1..n], prime x]

-- factorize :: Integral a => a -> [a]
-- factorize 1 = []
-- factorize n = {-# SCC factorize #-}
--  let x = head [x2 | x2 <- primes n, factor n x2]
--    in x:(factorize(n `div` x))

