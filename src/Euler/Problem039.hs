{-# OPTIONS -Wall #-}
module Euler.Problem039
    ( problem039
    ) where

import Data.List (maximumBy)
import Data.Function (on)

-- problem039
problem039 :: Integer
problem039 = fst $ maximumBy (compare `on` (length . snd)) triList

triList :: [(Integer, [(Integer, Integer, Integer)])]
triList = map (\n -> (n, triangle n)) [12..1000]

triangle :: Integral t => t -> [(t, t, t)]
triangle p = [(a,b,c) | a <- [1..(p-1)], b <- [p*(p-2*a) `div` (2*(p-a))], a < b, c <- [p-(a+b)], a*a + b*b == c*c]


