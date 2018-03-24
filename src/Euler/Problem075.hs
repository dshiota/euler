module Euler.Problem075
    ( problem075
    ) where

import Data.List (group, sort)

{-
 - Let lengths of a right triangle be m^2-n^2, 2mn and ,m^2+n^2.
 - In this case, L is 2*m^2 + 2mn = 2(m^2+n).
 -}

problem075 :: Int
problem075 = length $ filter (\l -> length l == 1) $ group $ sort allLs

maxL :: Integer
maxL = 1500000

triLs :: [Integer]
triLs = [l| n <- [2..1000], m <- [1..(n-1)],
            odd (m+n), m `gcd` n == 1,
            let l = 2 * (n^(2::Int) + m * n),
            l <= maxL]

allLs :: [Integer]
allLs = concatMap (\m -> takeWhile (<= maxL) [m, 2*m..]) triLs

