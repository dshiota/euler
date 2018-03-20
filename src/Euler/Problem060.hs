{-# OPTIONS -Wall #-}
module Euler.Problem060
    ( problem060
    ) where

import Primality as P

-- problem 60
-- https://wiki.haskell.org/Euler_problems/51_to_60
problem60 :: IO ()
problem60 = print $ sum $ head solve
primesTo10000 :: [Integer]
primesTo10000 = 2:filter P.isPrime [3,5..9999]

solve :: [[Integer]]
solve = do
  a <- primesTo10000
  let m = f a $ dropWhile (<= a) primesTo10000
  b <- m
  let n = f b $ dropWhile (<= b) m
  c <- n
  let o = f c $ dropWhile (<= c) n
  d <- o
  let p = f d $ dropWhile (<= d) o
  e <- p
  return [a,b,c,d,e]
  where
    f x = filter (\y -> P.isPrime (read $ shows x $ show y)
                     && P.isPrime (read $ shows y $ show x)) 


