{-# OPTIONS -Wall #-}
module Euler.Problem069
    ( problem069
    ) where

import Data.List (inits)

{-
  Let n be the product of pk^ek where p is a prime number.
  phi(n) = n * (1-1/p1) * (1-1/p2) * ...
  n/phi(n) = (1/(1-1/p1)) * (1/(1-1/p2)) * ...
  1-1/pk is less than 1, so 1/(1-1/pk) is greater than 1.
  Smaller p is smaller 1-1/pk, and larger 1/(1-1/pk).

  Maximul n/phi(n) is the product of primes from the smallest until <10^6.
 -}

problem069 :: Integer
problem069 = maximum  
  [c | b <- tail $ inits primes, 
       let c = product b,
       c < 10^(6::Integer)]

primes :: [Integer]
primes = [2,3,5,7,11,13,17,19,23,29]
