{-# OPTIONS -Wall #-}
module Euler.Problem071
    ( problem071
    ) where

import Data.Ratio (Ratio, (%), numerator, denominator)
{-
 - x / 1000000 < 3 / 7 => x < 3 * 1000000 / 7 = 428571.428571...
 - and 428571 / 1000000 < 3 / 7 = 428571 / 999999
 - 428567 % 999990 < 
 -}

problem071 :: Int
problem071 = fareySeq (2%5) (3%7) ((10::Int)^(6::Int))

fareySeq :: Integral a => Ratio a -> Ratio a -> a -> a
fareySeq p q n
  | dp' <= n = fareySeq p' q n
  | otherwise = np
  where
  np = numerator p
  nq = numerator q
  dp = denominator p
  dq = denominator q
  p' = (np + nq) % (dp + dq)
  dp' = denominator p'

