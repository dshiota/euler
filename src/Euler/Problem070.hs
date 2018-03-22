{-# OPTIONS -Wall #-}
module Euler.Problem070
    ( problem070
    ) where

import Data.List (nub, sort, minimumBy)
import Data.Function (on)
import Data.Ratio ((%))
import Primality as P

problem070 :: Integer
problem070 = fst $ minimumBy (compare `on` snd) 
  [(n,nOverPhi n)| 
    n <- [2..10^(7::Integer)],
    isPermute (show n) (show (totPhi n))
    ]

nOverPhi :: (Integral a1, Fractional a) => a1 -> a
nOverPhi n = ((fromIntegral n) / (fromIntegral (totPhi n))) 

totPhi :: (Integral t, Integral a) => a -> t
totPhi 1 = 1
totPhi n = round ((product $ map (\p -> 1 - (1 % p)) $ nub $ P.factors n) * (fromIntegral n))

isPermute :: Ord a => [a] -> [a] -> Bool
isPermute n m = (sort n) == (sort m)
