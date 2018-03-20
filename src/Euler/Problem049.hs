{-# OPTIONS -Wall #-}
module Euler.Problem049
    ( problem049
    ) where

import Primality as P

-- problem049
problem049 :: String
problem049 = concatMap show $ last 
              [[n,n+3330,n+6660] | 
                n <- [1001::Integer,1003..9999],
                P.isPrime n,
                P.isPrime (n+3330),
                isPerm n (n+3330),
                P.isPrime (n+6660),
                isPerm n (n+6660)]

isPerm :: (Show a, Show a1) => a1 -> a -> Bool
isPerm n m = sort (show n) == sort (show m)


