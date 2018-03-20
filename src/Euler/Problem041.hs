{-# OPTIONS -Wall #-}
module Euler.Problem041
    ( problem041
    ) where

import Primality as P

-- problem041
problem041 :: Integer
problem041 = maximum $ filter isPandigitalN $ takeWhile (<9999999) P.primes

isPandigitalN :: Show a => a -> Bool
isPandigitalN n = sort (show n) == concatMap show [1..(length (show n))]


