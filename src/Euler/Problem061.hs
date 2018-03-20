{-# OPTIONS -Wall #-}
module Euler.Problem061
    ( problem061
    ) where

import Data.List (delete)

-- problem061
problem061 :: Integer
problem061 = sum $ head solveP61

solveP61 :: [[Integer]]
solveP61 = filter isCyclicP61 $ concatMap permute listP61

listP61 :: [[Integer]]
listP61 = [[a,b,c,d,e,f] | 
  a <- triP61,
  b <- squP61,
  c <- penP61,
  d <- hexP61,
  e <- hepP61,
  f <- octP61]

isCyclicP61 :: Show a => [a] -> Bool
isCyclicP61 [a,b,c,d,e,f] = 
  isSameTwoDigit a b &&
  isSameTwoDigit b c &&
  isSameTwoDigit c d &&
  isSameTwoDigit d e &&
  isSameTwoDigit e f &&
  isSameTwoDigit f a
   
permute :: Eq t => [t] -> [[t]]
permute [] = [[]]
permute xs = concatMap (\x -> map (x:) $ permute $ delete x xs) xs

triP61 :: [Integer]
triP61 = takeWhile(<10000) $ dropWhile (<1010) [div (n*(n+1)) 2 | n <- [1..]]

squP61 :: [Integer]
squP61 = takeWhile(<10000) $ dropWhile (<1010) [n*n | n <- [1..]]

penP61 :: [Integer]
penP61 = takeWhile(<10000) $ dropWhile (<1010) [div (n*(3*n-1)) 2 | n <- [1..]]

hexP61 :: [Integer]
hexP61 = takeWhile(<10000) $ dropWhile (<1010) [n*(2*n-1) | n <- [1..]]

hepP61 :: [Integer]
hepP61 = takeWhile(<10000) $ dropWhile (<1010) [div (n*(5*n-1)) 2 | n <- [1..]]

octP61 :: [Integer]
octP61 = takeWhile(<10000) $ dropWhile (<1010) [n*(3*n-2) | n <- [1..]]

isSameTwoDigit :: (Show a) => a -> a -> Bool
isSameTwoDigit x y = drop 2 (show x) == take 2 (show y)

