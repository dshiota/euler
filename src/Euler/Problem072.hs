module Euler.Problem072
    ( problem072
    ) where

import Primality as P

problem072 :: Integer
problem072 = sum $ map totPhi [2..(10::Integer)^(6::Integer)]

totPhi :: Integer -> Integer
totPhi n = product $ map (\(p,k) -> p^(k-1) * (p - 1)) $ P.factorize n
