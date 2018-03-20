{-# OPTIONS -Wall #-}
module Euler.Problem029
    ( problem029
    ) where

import Data.List (nub, sort)

-- problem029
problem029 :: Int
problem029 = length $ nub $ sort [a^b | a <- [(2::Integer)..100], b <- [(2::Integer)..100]]


