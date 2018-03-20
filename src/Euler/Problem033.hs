{-# OPTIONS -Wall #-}
module Euler.Problem033
    ( problem033
    ) where

import Data.Ratio ((%), denominator)
-- problem033
problem033 :: Integer
problem033 = denominator $ product
  [ a%c | a <- [1..9], b <- [1..9], c <- [1..9], isCurious a b c, a /= c]

isCurious :: Integral a => a -> a -> a -> Bool
isCurious a b c = (10*a+b)%(10*b+c) == a%c


