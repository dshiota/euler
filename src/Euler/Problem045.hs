{-# OPTIONS -Wall #-}
module Euler.Problem045
    ( problem045
    ) where

-- problem045
problem045 :: Integer
problem045 = head [x | x <- scanl (+) 1 [5,9..], x > 40755, isPenta x]

isPenta :: Integral t => t -> Bool
isPenta n = (abs af < (1e-9 :: Double)) && ai `mod` (6::Int) == 5
  where (ai, af) = properFraction . sqrt $ 1 + 24 * fromIntegral n

-- triNum = [n*(n+1) `div` 2 | n <- [1..]]
{-
pentaNum :: [Integer]
pentaNum = [n*(3*n-1) `div` 2 | n <- [1..]]
hexaNum :: [Integer]
hexaNum = [n*(2*n-1) | n <- [1..]]

isTri :: Integer -> Bool
isTri n = isTriInner n $ filter (<=n) triNum
  where isTriInner _ [] = False
        isTriInner k (x:xs)
          | k == x = True
          | k < x = False
          | otherwise = isTriInner k xs

isPenta :: Integer -> Bool
isPenta n = isPentaInner n $ filter (<=n) pentaNum
  where isPentaInner _ [] = False
        isPentaInner k (x:xs)
          | k == x = True
          | k < x = False
          | otherwise = isPentaInner k xs

isHexa :: Integer -> Bool
isHexa n = isHexaInner n $ filter (<=n) hexaNum
  where isHexaInner _ [] = False
        isHexaInner k (x:xs)
          | k == x = True
          | k < x  = False
          | otherwise = isHexaInner k xs
-}


