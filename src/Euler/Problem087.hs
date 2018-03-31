module Euler.Problem087
    ( problem087
    ) where

import Data.Array.Unboxed (UArray, accumArray, elems)
import Primality as P (primes)
-- https://wiki.haskell.org/Euler_problems/81_to_90

problem087 :: Int
problem087 = length $ filter id $ elems expressible

fm :: Integer
fm = 50000000
-- fm = 50

takeMapPrimes :: Ord a => a -> (Integer -> a) -> [a]
takeMapPrimes u f = takeWhile (<u) . map f $ P.primes

squares :: [Integer]
squares = takeMapPrimes fm (^(2::Int))

cubes :: [Integer]
cubes   = takeMapPrimes fm (^(3::Int))

fourths :: [Integer]
fourths = takeMapPrimes fm (^(4::Int))

expressible :: UArray Integer Bool
expressible = accumArray (||) False (1, fm) 
  [(t, True) | 
  a <- squares,
  b <- takeWhile (<(fm-a)) cubes,
  c <- takeWhile (<(fm-a-b)) fourths,
  let t = a + b + c]

