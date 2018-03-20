{-# OPTIONS -Wall #-}
module Euler.Problem006
    ( problem006
    ) where

-- problem006 100
problem006 :: (Enum a, Num a) => a -> a
problem006 n = sum [1..n] ^ (2 :: Integer) - sum (map (^ (2 :: Integer)) [1..n])


