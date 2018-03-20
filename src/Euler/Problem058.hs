{-# OPTIONS -Wall #-}
module Euler.Problem058
    ( problem058
    ) where

import Primality as P

--problem 58
problem58 :: Integer
problem58 = 
  result $ dropWhile tooBig $ drop 2 $ scanl primeRatio (0, 0) diag
    where
      primeRatio (n, d) num = (if d ` mod` 4 /= 0 && isPrimeP58 num then n+1 else n,d+1)
      tooBig (n,d) = n * 10 > d
      result ((_,d):_) = (d+2) `div` 4 * 2 + 1

isPrimeP58 :: Integer -> Bool
isPrimeP58 x
  | x == 3 = True
  | otherwise = and [P.millerRabinPrimality x n | n <- [2,3]]

diag :: [Integer]
diag = 1:3:5:7:zipWith (+) diag [8,10..]

{-
problem58 :: Int
problem58 = fst . head $ filter (\(_, r) -> r < 0.1) $ map primeRatio [1..]

primeRatio :: Int -> (Int, Ratio Int)
primeRatio n = (2*n+1, length (filter P.isPrime $ takeDiag n) % length (takeDiag n))

takeDiag :: Int -> [Integer]
takeDiag n = concat [[1], ru, lu, ld, rd]
  where
    ru = take n ruList
    lu = take n luList
    ld = take n ldList
    rd = take n rdList

ruList :: [Integer]
ruList = [(2*n-1)*2*n+1 | n <- [1..]]
luList :: [Integer]
luList = [(2*n)^(2::Integer)+1 | n <- [1..]]
ldList :: [Integer]
ldList = [2*n*(2*n+1)+1 | n <- [1..]]
rdList :: [Integer]
rdList = [(2*n+1)^(2::Integer) | n <- [1..]]
-}


