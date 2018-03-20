{-# OPTIONS -Wall #-}
module Euler.Problem063
    ( problem063
    ) where

problem063 :: Int
problem063 = length [a^b | a <- [(1::Integer)..10], b <- [1..100], b == length (show $ a^b)]
