{-# OPTIONS -Wall #-}
module Euler.Problem031
    ( problem031
    ) where

-- problem031
problem031 :: Int
problem031 = length $ g (200::Int) [200,100,50,20,10,5,2,1]
  where 
    g 0 _ = [[]]
    g _ [] = []
    g n coins@(c:rest)
      | c <= n = map (c:) (g (n-c) coins)
                  ++ g n rest
      | otherwise = g n rest


