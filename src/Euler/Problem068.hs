{-# OPTIONS -Wall #-}
module Euler.Problem068
    ( problem068
    ) where

import Data.List (unfoldr, nub)
-- https://wiki.haskell.org/Euler_problems/61_to_70
problem068 :: String
problem068 = 
  maximum $ map (concatMap show) poel
  where
    gon68 = [(1::Int)..10]
    knip = length gon68 `div` 2
    (is,e:es) = splitAt knip gon68
    extnodes = map (e:) $ permute es
    intnodes = map (\(p:ps) -> zipWith (\ x y -> [x, y])
      (p:ps) (ps++[p])) $ permute is
    poel = [ concat hs | 
            uitsteeksels <- extnodes,
            organen <- intnodes,
            let hs = zipWith (:) uitsteeksels organen,
            let subsom = map sum hs,
            length (nub subsom) == 1 ]

permute :: [t] -> [[t]]
permute [] = [[]]
permute list = 
  concatMap (\(x:xs) -> map (x:) (permute xs))
  (take (length list)
  (unfoldr (\l@(x:xs) -> Just (l, xs ++ [x])) list))

