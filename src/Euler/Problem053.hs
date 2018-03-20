{-# OPTIONS -Wall #-}
module Euler.Problem053
    ( problem053
    ) where

import EulerUtil (factorial)

-- problem053
problem053 :: Int
problem053 = length $ filter (>(1000000.0::Double)) $ concat [ combList n | n <- [1..100]]

combList :: (Fractional t, Ord t, Enum t) => t -> [t]
combList n = [comb n x | x <- [0..n]]

comb :: (Ord t, Fractional t) => t -> t -> t
comb n r = factorial n / (factorial r * factorial (n - r))


