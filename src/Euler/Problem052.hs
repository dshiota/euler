{-# OPTIONS -Wall #-}
module Euler.Problem052
    ( problem052
    ) where

import Data.List (sort)

-- problem052
problem052 :: Integer
problem052 = head [x | x <- [100000..999999],
                      isSameDigit x (x*2),
                      isSameDigit x (x*3),
                      isSameDigit x (x*4),
                      isSameDigit x (x*5),
                      isSameDigit x (x*6)
                      ]

isSameDigit :: (Show a, Show a1) => a1 -> a -> Bool
isSameDigit n m = sort (show n) == sort (show m)


