module Euler.Problem079
    ( problem079
    ) where

import Data.Char (digitToInt, intToDigit)
import Data.List (intersect)
import Data.Graph (topSort, buildG)

-- https://wiki.haskell.org/Euler_problems/71_to_80
problem079 :: IO ()
problem079 = do
  f <- readFile "Euler/p079_keylog.txt"
  print $ p79 f

p79 :: (Read a, Num a) => String -> a
p79 file = 
  (+0) $ read . intersect graphWalk $ usedDigits
  where
  usedDigits = intersect "0123456789" $ file
  edges = concatMap (edgePair . map digitToInt) . words $ file
  graphWalk = map intToDigit . topSort . buildG (0, 9) $ edges
  edgePair [x, y, z] = [(x, y), (y, z)]
  edgePair _         = undefined


