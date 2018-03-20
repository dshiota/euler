{-# OPTIONS -Wall #-}
module Euler.Problem026
    ( problem026
    ) where

import Data.List (maximumBy, elemIndex)
import Data.Function (on)

-- problem026
problem026::Integer
problem026 = fst $ maximumBy (compare `on` snd) [(n, recurringCycle n) | n <- [1..999]]

recurringCycle :: Integral a => a -> Int
recurringCycle d = remainders d 10 []

remainders :: Integral a => a -> a -> [a] -> Int
remainders _ 0 _ = 0
remainders d r rs = let r' = r `mod` d
                    in case elemIndex r' rs of
                      Just i -> i + 1
                      Nothing -> remainders d (10*r') (r':rs)


