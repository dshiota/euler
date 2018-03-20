{-# OPTIONS -Wall #-}
module Euler.Problem002
    ( problem002
    ) where

-- problem002 4000000
problem002 :: Integral a => a -> a
problem002 n = sum (filter even (takeWhile (< n) (map fib [1..])))

fib :: Integral a => a -> a
fib n 
  | n == 1 = 1
  | n == 2 = 2
  | otherwise = fib (n-1) + fib (n-2)


