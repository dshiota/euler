{-# OPTIONS -Wall #-}
module Euler.Problem014
    ( problem014
    ) where

import Data.List (maximumBy)
import Data.Function (on)

-- problem014
problem014 :: Integral a => a -> a
problem014 n = fst $ maximumBy (compare `on` snd) (clenList n)
clenList :: (Integral t) => t -> [(t, Int)]
clenList n = map (\c -> (c, collatzLen 1 c)) [1..n]

collatzLen :: (Integral a, Num t) => t -> a -> t
collatzLen c 1 = c
collatzLen c n = collatzLen (c+1) $ if even n then n `div` 2 else 3 * n + 1

{-
collats :: (Integral a) => a -> [a]
collats 1      = [1]
collats n
  | even n = n : collats (n `div` 2)
  | otherwise = n : collats (3 * n + 1)
-}


