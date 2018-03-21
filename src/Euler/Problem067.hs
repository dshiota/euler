{-# OPTIONS -Wall #-}
module Euler.Problem067
    ( problem067
    ) where

problem067 :: IO ()
problem067 = 
  do input <- readFile "Euler/p067_triangle.txt"
     let triList = map (map (read :: String -> Int) . words) (lines input)
     print . head $ foldr1 zipTri triList


-- from Problem018
plusLarger :: (Ord a, Num a) => a -> a -> a -> a
plusLarger x y z = x + max y z

zipTri :: (Num a, Ord a) => [a] -> [a] -> [a]
zipTri xs ys = zipWith3 plusLarger xs ys $ tail ys

