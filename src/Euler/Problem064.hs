{-# OPTIONS -Wall #-}
module Euler.Problem064
    ( problem064
    ) where

import Data.List ((\\))

problem064 :: Int
problem064 = length $ filter isOddList tgtList

isOddList :: Integral a => a -> Bool
isOddList n = even $ length $ cntList n 0 1

tgtList :: [Integer]
tgtList = [2..9999] \\ map (^(2::Integer)) [2..100]

cntList :: Integral t => t -> t -> t -> [t]
cntList r n d = m : rest
  where
  m = (truncate (sqrt (fromIntegral r)::Double) + n) `div` d
  a = n - d * m
  rest | d == 1 && n /= 0 = []
       | otherwise = cntList r (-a) ((r - a ^ (2::Integer)) `div` d)

