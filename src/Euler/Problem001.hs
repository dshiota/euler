{-# OPTIONS -Wall #-}
module Euler.Problem001
    ( problem001
    ) where

-- problem001 1000
problem001 :: Integral a => a -> a
problem001 n = sum [x | x <- [1..(n-1)], mod x 3 == 0 || mod x 5 == 0]

