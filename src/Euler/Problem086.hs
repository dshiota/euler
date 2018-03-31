module Euler.Problem086
    ( problem086
    ) where

import Data.List (findIndex)
-- https://wiki.haskell.org/Euler_problems/81_to_90

problem086 :: Maybe Int
problem086 = findIndex (>(1000000::Integer)) (scanl (+) 0 (map cube [1..]))

isSquare :: Integral a => a -> Bool
isSquare x = (truncate $ sqrt $ fromIntegral x)^(2::Integer) == x

cube :: Integral a => a -> a
cube m = 
  sum [ (a`div`2) - if a > m then (a - m - 1) else 0|
      a <- [1..2*m],
      isSquare (a*a+m2)
      ]
      where
      m2 = m * m

