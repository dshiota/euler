{-# OPTIONS -Wall #-}
module Euler.Problem020
    ( problem020
    ) where

-- problem020
problem020 :: Int
problem020 = sum (map (\c -> read [c] :: Int) (show (product [(1::Integer)..100])))


