{-# OPTIONS -Wall #-}
module Euler.Problem036
    ( problem036
    ) where

-- problem036
problem036 :: Integer
problem036 = sum $ filter bothPalindromic [1..999999]

bothPalindromic :: (Integral a, Show a) => a -> Bool
bothPalindromic n = isPalindromic (show n) && isPalindromic (dec2binstr n)

dec2binstr :: (Integral a, Show a) => a -> String
dec2binstr n = dec2binstr' n ""
  where
    dec2binstr' 0 xs = '0':xs
    dec2binstr' 1 xs = '1':xs
    dec2binstr' m xs = dec2binstr' (m `div` 2) (show (m `mod` 2) ++ xs)

isPalindromic :: Eq a => [a] -> Bool
isPalindromic s = s == reverse s


