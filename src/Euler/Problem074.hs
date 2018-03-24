module Euler.Problem074
    ( problem074
    ) where

problem074 :: Int
problem074 = length $ filter (\(_,l) -> l == 60) $ map sumFacLoopLen [1..999999]

sumFacLoopLen :: Int -> (Int, Int)
sumFacLoopLen n = (n, length $ sumFacLoop n)

sumFacLoop :: Int -> [Int]
sumFacLoop n = sumFacInner n []

sumFacInner :: Int -> [Int] -> [Int]
sumFacInner n xs
  | n `elem` xs = xs
  | otherwise = sumFacInner (sumFac n) (n:xs)

sumFac :: Int -> Int
sumFac n = sum $ map (facts !!) $ splitNum n

splitNum :: Integral t => t -> [t]
splitNum 0 = []
splitNum n = (n `mod` 10) : splitNum (n `quot` 10)

facts :: [Int]
facts = scanl (*) 1 [1..9]

