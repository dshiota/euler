{-# OPTIONS -Wall #-}
module Euler.Problem054
    ( problem054
    ) where

import Data.Maybe (fromJust)
import Data.List (elemIndex, group, sortBy, nub)

-- problem054
-- https://wiki.haskell.org/Euler_problems/51_to_60
problem054 :: IO()
problem054 = do
  f <- readFile "../p054_poker.txt"
  let games = map gameLineToHands $ lines f
      wins = filter p1won games
  print $ length wins

readCard :: String -> (Int, Int)
readCard [] = (0, 0)
readCard [_] = (0, 0)
readCard (_:_:_:_) = (0, 0)
readCard [r, s] = (parseRank r, parseSuit s)
  where parseSuit = translate "SHDC"
        parseRank = translate "23456789TJQKA"
        translate from x = fromJust $ elemIndex x from

solveHand :: (Num a, Eq b, Ord a) => [(a, b)] -> (Integer, [a])
solveHand hand = (handRank, tiebreak)
  where
    handRank
      | flush && straight   = 9::Integer
      | hasKinds 4          = 8
      | all hasKinds [2,3]  = 7
      | flush               = 6
      | straight            = 5
      | hasKinds 3          = 4
      | 1 < length (kind 2) = 3
      | hasKinds 2          = 2
      | otherwise           = 1
    tiebreak = kind =<< [4,3,2,1]
    hasKinds = not . null . kind
    kind n = map head $ filter ((n==).length) $ group ranks
    ranks = sortBy (flip compare) $ map fst hand
    flush = 1 == length (nub (map snd hand))
    straight = length (kind 1) == 5 && 4 == head ranks - last ranks

gameLineToHands :: String -> ([(Int, Int)], [(Int, Int)])
gameLineToHands = splitAt 5 . map readCard . words

p1won :: (Eq b, Eq b1, Num a, Ord a) => ([(a, b1)], [(a, b)]) -> Bool
p1won (a,b) = solveHand a > solveHand b


