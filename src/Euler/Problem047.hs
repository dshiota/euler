{-# OPTIONS -Wall #-}
module Euler.Problem047
    ( problem047
    ) where

import Primality as P

-- problem 47
problem47 :: Integer -> Integer
problem47 n = fst $ head $ filter (\(_,xs) -> all (==n) xs)
                         $ map (head Control.Arrow.&&& map (toInteger . length . fstfac) )
                         [[m..(m+n-1)] | m <- [1..]]

-- primeFactors :: Integral t => t -> [t]
{-
primeFactors n = factor n P.primes
  where factor _ [] = []
        factor m (p:ps)
          | p*p > m        = [m]
--          | m `mod` p == 0 = [p, m `div` p]
          | m `mod` p == 0 = p: factor (m `div` p) (p:ps)
          | otherwise      = factor m ps
-}
{-
fac :: (Integral b, Num t) => [(t,b)] -> [t]
fac [(x,y)] = [x^a | a <- [0..y]]
fac (x:xs) = [a*b | a <- fac [x], b <- fac xs]
-}

fstfac :: Integer -> [(Integer, Int)]
fstfac x = [(head a, length a) | a <- group $ P.primeFactors x]


