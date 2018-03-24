module Euler.Problem073
    ( problem073
    ) where

import Data.Ratio (Ratio, (%), numerator, denominator)

problem073 :: Int
problem073 = length $ fareySeqNum (1%3) (1%2) (12000::Integer)

fareySeqNum :: Integral a => Ratio a -> Ratio a -> a -> [Ratio a]
fareySeqNum p q n
  | dp' <= n = fareySeqNum p p' n ++ [p'] ++ fareySeqNum p' q n
  | otherwise = []
  where
  np = numerator p
  nq = numerator q
  dp = denominator p
  dq = denominator q
  p' = (np + nq) % (dp + dq)
  dp' = denominator p'


