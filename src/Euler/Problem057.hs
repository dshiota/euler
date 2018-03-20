{-# OPTIONS -Wall #-}
module Euler.Problem057
    ( problem057
    ) where

-- problem057
problem057 :: Int
problem057 = length $ filter (\(n, m) -> length (show n) > length (show m)) $ 
  take 1000 $ drop 1 $ iterate nextExp (1::Integer,1::Integer)

nextExp :: Num t => (t, t) -> (t, t)
nextExp (a, b) = (a + 2 * b, a + b)


