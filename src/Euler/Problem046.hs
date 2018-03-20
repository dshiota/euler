{-# OPTIONS -Wall #-}
module Euler.Problem046
    ( problem046
    ) where

import Primality as P

-- problem046
problem046 :: Integer
problem046 = head $ filter (not . verifConj) compOdds

-- isPrime :: Integral a => a -> Bool
{-
isPrime 1 = False
isPrime n = all (\k -> n `mod` k /= 0) $ takeWhile (\p -> p*p <= n) P.primes
-}

compOdds :: [Integer]
compOdds = filter (not . P.isPrime) [3,5..]

verifConj :: Integer -> Bool
verifConj n = any P.isPrime (takeWhile (>0) $ map (\i -> n - 2*i*i) [1..])


