{-# OPTIONS -Wall #-}
module Euler.Problem028
    ( problem028
    ) where

-- problem 28
problem028 :: Integer
problem028 = 1 + sum (map f [3,5..1001])
  where f n = 4*(n-2)^(2::Integer)+10*(n-1)


