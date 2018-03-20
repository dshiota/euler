{-# OPTIONS -Wall #-}
module Euler.Problem048
    ( problem048
    ) where

-- problem048
problem048 :: String
problem048 = last10 $ show $ powSum (1000::Integer)

powSum :: Integral b => b -> b
powSum n = sum [k^k | k <- [1..n]]

last10 :: [a] -> [a]
last10 s = reverse $ take 10 $ reverse s


