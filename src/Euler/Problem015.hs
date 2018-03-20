{-# OPTIONS -Wall #-}
module Euler.Problem015
    ( problem015
    ) where

-- problem015 20
problem015 :: Integral a => a -> a
problem015 n = product [(n+1)..(2*n)] `div` product [1..n]

