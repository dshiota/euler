{-# OPTIONS -Wall #-}
module Euler.Problem025
    ( problem025
    ) where

-- problem025
problem025 :: Int
problem025 = length w
  where
    w = takeWhile (< t) fibs

fibs :: [Integer]
fibs = 0:1:zipWith (+) fibs (tail fibs)
t :: Integer
t = 10^(999 :: Integer)

{-
fibLength n = (length . show $ fst $ fib' n, show $ fst $ fib' n, n)

fib' 1 = (1,1)
fib' 2 = (1,2)
fib' n = ((fst $ fib' (n-1)) + (fst $ fib' (n-2)), n)
-}


