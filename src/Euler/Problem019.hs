{-# OPTIONS -Wall #-}
module Euler.Problem019
    ( problem019
    ) where

-- problem019
problem019 :: Int
problem019 = length $ filter (==0) $
             drop 11 $ scanl (\x y -> (x + y) `mod` 7) 1 (day1900 ++ day21c)

day21c :: [Int]
day21c = concatMap dayYear [1901..2000]

day1900 :: [Int]
day1900 = dayYear 1900

dayYear :: Int -> [Int]
dayYear y = if isLeap y then dayMonthLeap else dayMonth

isLeap :: Integral a => a -> Bool
isLeap y
  | y `mod` 400 == 0 = False
  | y `mod` 4   == 0 = True
  | otherwise        = False

dayMonth :: [Int]
dayMonth = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

dayMonthLeap :: [Int]
dayMonthLeap = 31 : 29 : drop 2 dayMonth


