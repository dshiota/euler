{-# OPTIONS -Wall #-}
module EulerUtil
    ( isPalindrome
    , factorize
    , factorial
    ) where

import Data.List (group)
import Primality as P

isPalindrome :: Show a => a -> Bool
isPalindrome n = show n == reverse (show n)

factorize :: Integer -> [(Integer,Integer)]
factorize 1 = [(1,0::Integer)] -- 1^0
factorize n = format $ factorize' n P.primes
  where
    factorize' _ [] = undefined
    factorize' n' ps@(p:ps')
      | p * p > n' = [n']
      | rem n' p == 0 = p : factorize' (div n' p) ps
      | otherwise = factorize' n' ps'
    format ps = [(x, toInteger $ length xs) | xs@(x:_) <- group ps]

factorial :: (Num t, Ord t) => t -> t
factorial n
  | n <= 1 = 1
  | otherwise = n * factorial (n-1)
 
