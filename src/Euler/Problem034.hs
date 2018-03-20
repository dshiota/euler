{-# OPTIONS -Wall #-}
module Euler.Problem034
    ( problem034
    ) where

import EulerUtil (factorial)

-- problem034
problem034 :: Int
-- problem34 = sum $ filter isCurNum [3..9999999]
problem034 = sum $ filter isCurNum [3..99999]

isCurNum :: Int -> Bool
isCurNum n = n == sumOfFac n

sumOfFac :: Show a => a -> Int
sumOfFac n = sum $ map (\c -> factorial $ read [c] :: Int) $ show n


