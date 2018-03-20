{-# OPTIONS -Wall #-}
module Euler.Problem005
    ( problem005
    ) where

-- problem005 20
problem005 :: Integral a => a -> a
-- problem005 n = head ([x | x <- [n,(n*2)..], isDivisible x [2..n] ])
problem005 = productFactor

-- isDivisible :: (Integral a, Foldable t) => a -> t a -> Bool
-- isDivisible num xs = all (\x -> num `mod` x == 0) xs

productFactor :: Integral a => a -> a
productFactor n
  | n == 1 = 1
  | otherwise = n `div` gcd (productFactor (n-1)) n * productFactor (n-1)

