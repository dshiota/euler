{-# OPTIONS -Wall #-}
module Euler.Problem050
    ( problem050
    ) where

import Primality as P

-- problem050
-- http://tsumuji.cocolog-nifty.com/tsumuji/2010/10/project-euler-2.html
problem050 :: Integer -> Integer
problem050 n = head $ filter P.isPrime $ map sum ps1
  where
    ps1 = concatMap (parts ps2) $ reverse [1..(length ps2)]
    ps2 = take count P.primes
    count = length $ takeWhile (<n) primeSums
      where primeSums = tail $ scanl (+) 0 P.primes

parts :: [a] -> Int -> [[a]]
parts xs n = take (length xs - n + 1) $ map (take n) $ iterate tail xs


