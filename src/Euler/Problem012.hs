{-# OPTIONS -Wall #-}
module Euler.Problem012
    ( problem012
    ) where

problem012 :: a
problem012 = undefined

{-
-- problem12
problem12 :: Integer
problem12 = head (filter (\n -> (numOfDivisors n) > (500::Integer)) triangleNumber)
-- very slow
-- problem12 = head (filter (\n -> length (divisors n) > 500) triangleNumber)

triangleNumber :: [Integer]
triangleNumber = map (\n -> sum [1..n]) [1..]

-- divisors :: Integral t => t -> [t]
-- divisors n = [x | x <- [1..n], n `mod` x == 0]

numOfDivisors :: (Num b, Integral k) => k -> b
numOfDivisors n = foldr (\v -> (*) (v+1)) 1 (primeList n)

primeList :: (Num a, Integral k) => k -> Map.Map k a
primeList n = Map.fromListWith (+) (map (\p -> (p,1)) (factorize n))
-}

