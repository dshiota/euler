module Euler.Problem076
    ( problem076
    ) where

import Data.Function.Memoize (Memoizable, memoFix2)

problem076 :: (Memoizable a0, Num a0, Ord a0) => a0 -> Integer
-- problem076 100
problem076 n = pfun n - 1

pfun :: (Ord a0, Num a0, Memoizable a0) => a0 -> Integer
pfun = ppfunMem 1

ppfun :: (Num a, Num t, Ord a) => (a -> a -> t) -> a -> a -> t
ppfun f k n
  | k > n  = 0
  | k == n = 1
  | otherwise = f (k+1) n + f k (n-k)

ppfunMem :: (Memoizable a0, Num a0, Ord a0) => a0 -> a0 -> Integer
ppfunMem = memoFix2 ppfun

