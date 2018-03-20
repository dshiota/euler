{-# OPTIONS -Wall #-}
module Euler.Problem004
    ( problem004
    ) where

import EulerUtil

-- problem004
problem004 :: Integer
problem004 = maximum [x*y | x <- [100..999], y <- [x..999], x `mod` 11 == 0 || y `mod` 11 == 0, isPalindrome (x*y)]

