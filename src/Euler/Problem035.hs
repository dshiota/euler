{-# OPTIONS -Wall #-}
module Euler.Problem035
    ( problem035
    ) where

import Primality as P
import EulerUtil (factorize)

-- problem035
problem035 :: Int
problem035 = length $ circularPrimes $ filter (notElem '0'. show) $ takeWhile (<1000000) P.primes

circularPrimes :: [Integer] -> [Integer]
circularPrimes [] = []
circularPrimes (x:xs)
  | all P.isPrime' p = x : circularPrimes xs
  | otherwise      = circularPrimes xs
  where
    p = rotList x

rotList :: Integer -> [Integer]
rotList n = rotListInner n (length (show n)) [] where
  rotListInner x l xs
    | l == 0 = xs
    | otherwise = rotListInner (rotateNum x) (l-1) (x:xs)

rotateNum :: Integer -> Integer
rotateNum n = read (tail $ show n ++ [head $ show n]) :: Integer


