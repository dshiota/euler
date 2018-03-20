{-# OPTIONS -Wall #-}
module Euler.Problem055
    ( problem055
    ) where

import EulerUtil (isPalindrome)

-- problem 55
problem55 :: Int
problem55 = length $ filter lychrel [1..10000]

lychrel :: Integer -> Bool
lychrel = not . any isPalindrome . take 50 . tail . iterate next
  where next x = x + revNum x

revNum :: Show a => a -> Integer
revNum n = read $ reverse $ show n :: Integer


